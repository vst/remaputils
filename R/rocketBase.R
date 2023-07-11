##' Pocketbase dependent function to gather records from the db.
##'
##' This is the description
##'
##' @param rb The rocketbase authorizaion credentials. Defined externally.
##' @param path The path of the pocketbase API endpoint, e.g. /api/collections/cfraEquities/records
##' @param ext the ext to the path - where applicable, "?page=1"
##' @param item The actual endpoint element to return, e.g. 'items' or 'pages'

##' @return a data frame or vector of the item(s).
##' @export
getRecs <- function(rb,path,ext,item) {

  response <- rb$bare$get(paste0(path,ext))
  content <- response$parse(encoding = "UTF-8")
  recs <- jsonlite::fromJSON(content)[[item]]

  return(recs)

}

##' Pocketbase dependent function to query a rocketbase endpoint and stack the results X pages at a time.
##'
##' This is the description
##'
##' @param pages The number of pages corresponding to the endpoint. We can only query one page at a time.
##' @param rb The rocketbase authorizaion credentials. Defined externally.
##' @param path The path of the pocketbase API endpoint, e.g. /api/collections/cfraEquities/records

##' @return a data frame of the stacked records.
##' @export
stackData <- function(pages,rb,path) {
    stack <- data.frame() %>%
    dplyr::bind_rows(
        lapply(1:pages, function(x) {
        getRecs(rb,path,paste0("?page=",x),"items")
        }
        )
    )
    return(stack)
}

##' Pocketbase dependent function to push data in the DB. Ancillary to the incrIt fn below. 
##'
##' This is the description
##'
##' @param rb The rocketbase authorizaion credentials. Defined externally.
##' @param path The path of the pocketbase API endpoint, e.g. /api/collections/cfraEquities/records
##' @param lst list of the data to be pused to the endpoint above.

##' @return NULL.
##' @export
postIt <- function(rb,path,lst) {

        response <- rb$bare$post(
        dest,
        encode = "json",
        body = lst
    )
    if(response$status_code!=200) {return(NULL)}

}

##' Pocketbase dependent function to push data efficiently without duplicating already existing data in the DB.
##'
##' This is the description
##'
##' @param rb The rocketbase authorizaion credentials. Defined externally.
##' @param path The path of the pocketbase API endpoint, e.g. /api/collections/cfraEquities/records
##' @param increment The data to add or append to the endpoint.
##' @param join The vector of the keys to join by to exclude already existing data in the DB.
##' @param push The TRUE/FALSE flag dictating whether to push the data or just return the records.

##' @return NULL or the records depending on the outcome.
##' @export
incrIt <- function(rb,path,increment,join,push=TRUE) {

  pages <- getRecs(rb,path,"","totalPages")

  if(pages>0) {

  existing <- data.frame() %>%
    dplyr::bind_rows(
        lapply(1:pages, function(pg) {
        getRecs(rb,path,paste0("?page=",pg),"items")
        }
        )
    )

  records <- existing %>%
    dplyr::mutate(across(contains("date"),as.Date))

  existing <- records %>%
    dplyr::select(all_of(join))


  increment <- increment %>%
    dplyr::anti_join(existing %>% dplyr::select(all_of(join)),by=join)


  }

  if(NROW(increment)==0) {
    return(NULL)
  }

  if(!push) {

  return(records)

  }

  upList <- sapply(1:NROW(increment), function(x) list(increment[x,]))

  lapply(upList, function(p) {
  postIt(rb,path,as.list(p))
  }
  )

  return(NULL)

}

##' Pocketbase dependent function to flatten all the endpoints into a master data frame.
##'
##' This is the description
##'
##' @param rb The rocketbase authorizaion credentials. Defined externally.
##' @param paths The paths of the relevant endpoints, e.g. /api/collections/cfraEquities/records
##' @param eDate date filter floor to apply. Defaults to present.

##' @return a master flattened data frame.
##' @export
flattenUni <- function(rb,paths,eDate=Sys.Date()) {

      for (i in 1:length(paths)) {
        p <- paths[[i]]
        assign(names(paths)[i],p)
      }

  ## Static Universe
      pgs <- getRecs(rb,universePath,"","totalPages")
      uni <- stackData(pgs,rb,universePath) %>% select(-contains("collection"),-c(id,created,updated))

      pgs <- getRecs(rb,closePricePath,"","totalPages")
      cp <- stackData(pgs,rb,closePricePath) %>% select(-contains("collection"),-c(id,created,updated))
      cp <- cp %>%
        dplyr::group_by(symbol) %>%
        dplyr::mutate(close_date=as.Date(date)) %>%
        dplyr::filter(close_date==max(close_date)) %>%
        dplyr::select(-date) %>%
        ungroup()
  ## Close Prices
      pgs <- getRecs(rb,targetPricePath,"","totalPages")
      tp <- stackData(pgs,rb,targetPricePath) %>% select(-contains("collection"),-c(id,created,updated))
      tp <- tp %>%
        dplyr::group_by(symbol) %>%
        dplyr::mutate(date=as.Date(date)) %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::slice(1:2)

      tpMap <- getRecs(rb,"/api/collections/cfraStarsRecommendation/records","","items") %>%
        dplyr::select(stars_rank,stars_recommendation)

      tpA <- tp %>%
        dplyr::inner_join(tpMap,by=c("rank"="stars_rank")) %>%
        dplyr::select(symbol,target_price,stars_recommendation,date)

      tpN <- tpA %>%
        dplyr::filter(row_number()==1)
      colnames(tpN) <- c("symbol","target_price","recommendation","target_date")

      tpO <- tpA %>%
        dplyr::filter(row_number()==2)
      colnames(tpO) <- c("symbol","target_price_prev","recommendation_prev","target_date_prev")
  ## Target Prices
      tpA <- tpN %>%
        dplyr::left_join(tpO,by=c("symbol")) %>%
        ungroup()

      pgs <- getRecs(rb,highlightsPath,"","totalPages")
      hl <- stackData(pgs,rb,highlightsPath) %>% select(-contains("collection"),-c(id,created,updated))
      hl <- hl %>%
        dplyr::group_by(symbol) %>%
        dplyr::mutate(date=as.Date(if_else(date=="",as.character(NA),date))) %>%
        dplyr::filter(date==max(date)) %>%
        dplyr::rename(highlights_date=date) %>%
        dplyr::ungroup()

      pgs <- getRecs(rb,estimatesPath,"","totalPages")
      es <- stackData(pgs,rb,estimatesPath) %>% select(-contains("collection"),-c(id,created,updated))
      est <- es %>%
        dplyr::filter(year>=lubridate::year(eDate))

      estQ <- est %>%
        dplyr::filter(str_detect(period,"q")) %>%
        dplyr::filter(as.numeric(str_remove(period,"q"))>=lubridate::quarter(eDate))

      estY <- est %>%
        dplyr::filter(period=="year",year>lubridate::year(eDate)) %>%
        dplyr::group_by(symbol,type) %>%
        dplyr::arrange(year) %>%
        dplyr::filter(row_number()==1)

      chg3m <- estQ %>%
        dplyr::group_by(symbol,type) %>%
        dplyr::arrange(year,period) %>%
        dplyr::mutate(curr=if_else(row_number()==1,est,as.numeric(NA)),
        nxt=if_else(row_number()==2,est,as.numeric(NA))) %>%
        dplyr::summarise(curr=max(curr,na.rm=TRUE),nxt=max(nxt,na.rm=TRUE)) %>%
        dplyr::mutate(delta=nxt/curr-1) %>%
        dplyr::filter(is.finite(delta)) %>%
        ungroup()

      delta <- chg3m %>%
        dplyr::select(-curr,-nxt) %>%
        tidyr::pivot_wider(names_from=type,values_from=delta,names_prefix="delta3m")

      estNY <- estY %>%
        dplyr::select(-period,-year) %>%
        dplyr::group_by(symbol,type) %>%
        dplyr::inner_join(cp %>% dplyr::select(close_price,symbol),by="symbol") %>%
        dplyr::mutate(estimate=close_price/est) %>%
        dplyr::select(-close_price,-est) %>%
        tidyr::pivot_wider(names_from=type,values_from=estimate,names_prefix="priceRatioNxtYrEst") %>%
        ungroup()
## derived
    research <- hl %>%
      dplyr::mutate(projEps3yrCagr=projEps3yrCagr/100) %>%
      dplyr::left_join(delta,by=c("symbol")) %>%
      dplyr::left_join(estNY,by=c("symbol"))

    flattened <- uni %>%
      dplyr::left_join(tpA,by=c("symbol")) %>%
      dplyr::left_join(cp,by=c("symbol")) %>%
      dplyr::mutate(target_upside=target_price/close_price-1) %>%
      dplyr::left_join(research,by=c("symbol")) %>%
      dplyr::mutate(across(where(is.numeric),~ if_else(is.finite(.x),.x,as.numeric(NA))))

   return(flattened)
}
