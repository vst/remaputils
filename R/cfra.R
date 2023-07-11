##' CFRA-pocketbase dependent function to query the CFRA endpoint, given client authorization config.
##'
##' This is the description
##'
##' @param client The authorization config as defined outside. Should be an R6Class object.
##' @param endpoint The respective CFRA endpoint, e.g. 'equities'. See documentation here https://developers.cfraresearch.com/.
##' @return list of the endpoint contents.
##' @export
queryCfra <- function(client,endpoint) {

    response <- client$bare$get(paste0("/",endpoint))

    if(response$status_code != 200) {
        print(paste("Check Authorization/Issue:",response$status_code))
    return(NULL)
    }
    ##stopifnot(response$status_code == 200)

    content <- response$parse(encoding = "UTF-8")

    result <- jsonlite::fromJSON(content)

    return(result)

}


##' CFRA-pocketbase dependent function to unlist the estimates CFRA endpoint. Relevant for cfraDetails function ONLY.
##'
##' This is the description
##'
##' @param lst list to flatten. Should be the income or earnings from CFRA queryCfra(cc,path)[["result"]], path e.g. equity/research/stars-full/ticker/XXX/exchange/YYY
##' @return data frame of the flattened list.
##' @export
aggEstimates <- function(lst) {
    lapply(seq_along(lst),function(x) {
        if(all(sapply(lst[[x]], is.null))) {return(NULL)}
        ulst <- safeNull(unlist(lst[[x]]))
        df <- data.frame(
        est=numerize(ulst),
        period=as.character(safeNull(names(ulst))),
        year=as.numeric(safeNull(names(lst))[x]),
        stringsAsFactors=FALSE
        )
        if(all(is.na(df$est))) {return(NULL)}
        return(df)
    })
}

##' CFRA-pocketbase dependent function to gather the CFRA details/appendix info.
##'
##' This is the description
##'
##' @param tick The ticker that identifies the instrument in CFRA DB, e.g. 'AAPL'
##' @param exch The exchange that corresponds to the ticker in CFRA DB, e.g. 'NasdaqGS'
##' @param lastStarDate The date floor to supply for the star rank elements, for efficiency, e.g. 2023-01-01
##' @param lastHgltDate The date floor to supply for the star highlights elements, for efficiency, e.g. 2023-01-01
##' @param path2Config The path to the file containing the cfra configurations

##' @return a list containing 3 data frame elements corresponding to stars ranking, research, and estimates data.
##' @export
cfraDetails <- function(tick,exch,lastStarDate=NA,lastHgltDate=NA,path2Config="cfra/cfraAuth.R") {

    stars <- NULL

    ## get the snapshot history
    path <- paste0("equity/data/stars/ticker/",tick,"/exchange/",exch)
    if(!is.na(lastStarDate)) {
    path <- paste0(path,"?date_from=",lastStarDate+1)
    }
    starsHist <- queryCfra(cc,path)[["result"]]$stars_data

    ##check connection
    if(is.null(starsHist)) {
    source(path2Config) ##assumes running from mapper
    Sys.sleep(1)
    starsHist <- queryCfra(cc,path)[["result"]]$stars_data
    }

    if(length(starsHist)>0) {
    if(nrow(starsHist)>0) {
    stars<- starsHist %>%
        dplyr::mutate(ticker=as.character(tick),exchange=as.character(exch),date=as.Date(actual_from),target_price=as.numeric(target_price),rank=as.numeric(stars_rank)) %>%
        dplyr::select(ticker,exchange,date,target_price,rank)
    }
    }
    ## get the full research history
    hghlts <- NULL
    estimates <- NULL

    path <- paste0("equity/research/stars-full/ticker/",tick,"/exchange/",exch)
    if(!is.na(lastHgltDate)) {
    path <- paste0(path,"?date_from=",lastHgltDate+1)
    }

    rsrch <- queryCfra(cc,path)[["result"]]

    if(length(rsrch)>0) {

    hghlts <- data.frame(
    ticker=as.character(tick),
    exchange=as.character(exch),
    date=as.Date(safeNull(rsrch$highlights_date)),
    projEps3yrCagr=as.numeric(safeNull(rsrch$`3_yr_proj_eps_cagr_prcntg`)),
    peers=as.character(paste(safeNull(unlist(rsrch$related_companies)), collapse=";")),
    divYield12mTrail=as.numeric(safeNull(as.numeric(rsrch$trailing_12_mo_dividend)/100)),
    volatility=as.character(safeNull(rsrch$volatility)),
    reportingFreq=as.character(safeNull(rsrch$reporting_frequency)),
    stringsAsFactors=FALSE
    )

    ## gather estimates
    eps <- NULL
    estimates <- rsrch$income_estimates$eps
    if(length(estimates)>0) {
    epsLs <- aggEstimates(estimates)
    epsDf <- data.frame() %>%
        dplyr::bind_rows(lapply(epsLs, function(x) {
        if(length(x)==0) {return(NULL)}
        x
    }))
    if(nrow(epsDf)!=0) { 
    eps <- data.frame(ticker=as.character(tick),exchange=as.character(exch),epsDf,type="EPS",stringsAsFactors=FALSE)  
    }
    }

    rev <- NULL
    estimates <- rsrch$revenue_estimates
    if(length(estimates)>0) {
    revLs <- aggEstimates(estimates)
    revDf <- data.frame(stringsAsFactors=FALSE) %>%
        dplyr::bind_rows(lapply(revLs, function(x) {
        if(length(x)==0) {return(NULL)}
        x
    }))
    if(nrow(revDf)!=0) {
    rev <- data.frame(ticker=as.character(tick),exchange=as.character(exch),revDf,type="REV",stringsAsFactors=FALSE)  
    }
    }

    estimates <- data.frame() %>%
        dplyr::bind_rows(eps) %>%
        dplyr::bind_rows(rev) 
        

    }

    return(list(
    "stars"=stars,
    "research"=hghlts,
    "estimates"=estimates
    ))

}
