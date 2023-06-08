##' Provides the internal NAV/Share data frame
##'
##' This is the description
##'
##' @param portfolio The portfolio id.
##' @param ccy The portfolio currency.
##' @param date The report date.
##' @param session The rdecaf session
##' @return A data frame with the internal nav/share price.
##' @export
getInternalNAVShare <- function(portfolio, ccy, date, session) {

    ## Get the persisted consolidation:
    pConsolidation <- getResource("pconsolidations", params=list("portfolio"=portfolio, "page_size"=-1, "date__gte"=dateOfPeriod("Y-0", date), "price__gt"=0), session=session)

    if (length(pConsolidation) == 0) {
        return(data.frame("date"=seq(dateOfPeriod("Y-0"), date, 1), "price"=100, "ccy"=ccy))
    }

    ## Get the internal price and return:
    do.call(rbind, lapply(1:length(pConsolidation), function(i) {

        ## Get the current list element:
        pCons <- pConsolidation[[i]]

        ## If date of consolidation is passed report date, return NULL:
        if (pCons[["date"]] > date) {
            return(NULL)
        }

        ## If ccy of consolidation is not portfolio ccy, return NULL:
        if (pCons[["ccy"]] != ccy) {
            return(NULL)
        }

        ## If price is null , return NULL:
        if (is.null(pCons[["price"]])) {
            return(NULL)
        }

        ## Construct the data frame and return:
        return(data.frame("date"=pCons[["date"]], "price"=pCons[["price"]], "ccy"=pCons[["ccy"]], "shrcls"=safeNull(pCons[["shareclass"]])))
    }))

}

##' Given a NAV or return time series df, flags extreme outliers before other treatment, given scale
##'
##' This is the description
##'
##' @param series the time series data frame
##' @param scale the scale applied to IQR calculation to define outlier
##' @return A data frame with the extreme outliers flagged.
##' @export
prefaceSeries <- function(series,scale=5) {

## calculate summary statistics of the return series
  smry <- summary(series$return,na.rm=TRUE)

  med <- smry["Median"] 
  q1 <- smry["1st Qu."]
  q3 <- smry["3rd Qu."]
  iqr <- q3 - q1

  ## upper limit max of: min of outlier defined (e.g. q3 + 1.5*IQR) vs. max - haircut (e.g. 5%),q3 of IQR, average of the series
  upper <- max(min(q3 + scale * iqr,smry["Max."]*(1-scale/100)),q3,smry["Mean"])
  ## lower limit same as above but do not allow for negative values
  lower <- max(q1 - scale * iqr,smry["Min."]+(abs(smry["Min."])*scale/100),0)

  series <- series %>%
    dplyr::mutate(absurd=!between(return,lower,upper)) ## anything outside winder defined as 'absurd' 

  series$skewness <- 0

  seriesClean <- series[!series$absurd,]

  if(NROW(seriesClean)==0) {return(series)} ## if all the data is absurd, return the original series

  smry <- summary(seriesClean$return,na.rm=TRUE)
  mean <- smry["Mean"]
  median <- smry["Median"]
  sd <- sd(seriesClean$return,na.rm=TRUE)
  skew <- 3 * (mean-median) / sd ##calculate skewness of cleaned series to be used to derive the factor used in the outlier function

  if(!is.na(skew)) {
  series$skewness <- skew
  }

  print(paste("Skew:",unique(series$skewness)))

  return(series)

}


##' Given a NAV or return time series df, derives the outliers statistically
##'
##' This is the description
##'
##' @param series the time series data frame
##' @param session The rdecaf session
##' @param fctr numeric ceiling factor used in standard deviation calculation to define outlier
##' @param flor numeric floor factor used in standard deviation calculation to limit outlier derivation
##' @param xVal true/false to dictate whether external valuation overrides should ignore outlier derivation
##' @param portfolio The portfolio id. Not required, defaults to NULL
##' @return A data frame with the extreme outliers flagged.
##' @export
findOutlier <- function(series,
                        session,
                        fctr=8,
                        flor=3.5,
                        xVal=FALSE,
                        portfolio=NULL) {

series <- series[order(series[,"date"]),] %>%
  dplyr::mutate(outlierFactor=as.numeric(NA)) ##var to record which factor flagged the outlier, ~3.5 - 8

series$diff <- c(NA,diff(as.numeric(unlist((series[,"return"])))))
series$diffAbs <- abs(series$diff) ## outliers calculated based on difference in return series

extVal <- NULL
## get the external valuations if we want to ignore outliers from manual overrides
if(!is.null(portfolio) & xVal) {
    extVal <- getDBObject("externalvaluations",addParams=list("portfolio"=portfolio),session=session)
}

while(fctr>flor) {

outlier <- series %>%
  dplyr::filter(diffAbs>sd(if_else(is.na(outlierFactor),diffAbs,as.numeric(NA)),na.rm=TRUE)*fctr)
## outlier defn, e.g. delta > sd(all deltas - ignoring already flagged) * factor of 8 to 3.5 
series <- series %>%
  dplyr::mutate(outlierFactor=if_else(is.na(outlierFactor)&NROW(outlier)>0&date %in% outlier$date,fctr,outlierFactor)) %>%
  dplyr::mutate(outlierFactor=if_else(xVal & !is.na(outlierFactor) & date %in% extVal$date,0,outlierFactor))
## override the outlier factor for the identified dates, ignoring those defined in previous iteration and/or external valuation
fctr <- fctr - log10(fctr)
## negatively increment the factor for the next iteration
}

series$portfolio <- as.numeric(safeNull(portfolio)) ## add the portfolio ID to avoid requerires

return(series)

}

##' Given an outliers data frame, attempts to explain them using DECAF transaction data
##'
##' This is the description
##'
##' @param outliers the outliers data frame
##' @param session The rdecaf session
##' @param portfolio The portfolio id. Not required, defaults to NULL, taken from outliers if not found
##' @param date The outlier date. Not required, defaults to NULL, taken from outliers if not found
##' @param currency The outlier currency. Not required, defaults to NULL, taken as reference currency if not found
##' @param excluded vector defining resources (from quants endpoint) to exclude in analysis, e.g. fx future. Defaults to NULL
##' @param inferred vector defining resources (from quants endpoint) to switch the sign of, e.g. loan. Defaults to NULL
##' @param correct magnitude factor applied in calculation used to derive relevancy of outlier residual.
##' @param thresh magnitude factor applied in calculation used to derive relevancy of outlier residual.
##' @param toler buffer factor applied in calculation used to derive relevancy of outlier residual.
##' @return A data frame with the relevant associated transaction returned for the outliers inout.
##' @export
cleanOutlier <- function(outliers,
                         session,
                         portfolio=NULL,
                         date=NULL,
                         currency=NULL,
                         excluded=NULL,
                         inferred=NULL,
                         correct=.5,
                         thresh=.02,
                         toler=.05) {

    if(is.null(portfolio)) {  ##1) make sure we have a portfolio ID to query with
        portfolio <- unique(outliers$portfolio)
        if(is.na(portfolio)|length(portfolio)!=1) {
            print("Incorrect Outlier Data Input - no portfolio ID")
            return(NULL)
        }
    }

    if(is.null(currency)) {  ##2) get the reference currency
    currency <- getDBObject("portfolios",session=session) %>%
      dplyr::filter(id==portfolio) %>%
      dplyr::select(rccy) %>%
      as.character()
    }

    if(is.null(date)) {  ##
        date <- unique(outliers$date)
        if(is.na(date)|length(date)!=1) {
            print("Incorrect Outlier Data Input - Invalid dates")
            return(NULL)
        }
    }

## 3) Pull relevant trades (transactions) for the outlier date - if any, given above information complete
    trans <- getDBObject("quants",session, addParams=list("account__portfolio"=portfolio, "refccy"=currency,"commitment__gte"=date,"commitment__lte"=date, nojournal=TRUE))
    if(NROW(trans)==0) { return(NULL) }

    trans <- trans %>%
      dplyr::select(id, trade, refamt, commitment, type, symbol, valamt, quantity, resource, ctype) %>%
      dplyr::mutate(across(c(contains("amt"),quantity),~as.numeric(.x))) %>%
      dplyr::mutate(sign=if_else(resource %in% inferred,-1*sign(quantity),sign(quantity))) %>% ## if its a counter position, e.g. loan, flip sign
      dplyr::mutate(amount=if_else(is.na(refamt),as.numeric(valamt),as.numeric(refamt))*sign) %>%
      dplyr::mutate(residual=round(sum(amount,na.rm=TRUE))) %>% ## compute the residual quant level, resolving NAs to 0
      dplyr::select(-contains("amt"),-quantity) %>%
      dplyr::filter(abs(amount)>0,!resource %in% excluded) ## stop the analysis is there is no residual or the instrument makes it not relevant
    if(NROW(trans)==0) { return(NULL) }


## only continue if we have records
    residual <- sum(trans$amount,na.rm=TRUE)
    residual != 0 || return(trans %>% dplyr::mutate(trade=as.character(NA),rationale="Zero Residual"))

## check if the transfers explain the residual 
    transfer <- sum(trans[trans$ctype==30,]$amount,na.rm=TRUE)
    if(abs(transfer/residual)>(1-toler)) {
      residual <- 0
    }

## check if the magnitude is great enought
    delta <- outliers$diffAbs / outliers$return

## decide whether to continue - if not, explain the rationale
    residual!=0                                                   || return(trans %>% dplyr::mutate(trade=as.character(NA), residual=0,rationale="Transfered Residual"))
    sign(residual)==sign(outliers$diff)                           || return(trans %>% dplyr::mutate(trade=as.character(NA), residual=0,rationale="Inverse Residual"))
    round(abs(residual))<=round(1.1*abs(outliers$diff))           || return(trans %>% dplyr::mutate(trade=as.character(NA), residual=0,rationale="Oversized Residual"))
    !(abs(delta)<thresh & abs(residual)<correct*outliers$diffAbs) || return(trans %>% dplyr::mutate(trade=as.character(NA), residual=0,rationale="Unimpactful Residual"))

    t <- trans %>%
      dplyr::mutate(rationale="Not Applicable",trade=as.character(trade)) ## Rationale should be 'Not Applicable' for the valid residual trades that remain unexplained
    sumFlag <- FALSE
    n <- 1

    NROW(t)<=10 || return(t %>% dplyr::mutate(residual=as.numeric(NA),trade=as.character(NA)))

    while(!sumFlag & n <= NROW(t)) { ## if more than 10 trades, combo iteration below is too computationally heavy, return all trades

        combos <- combn(NROW(t),n,simplify=FALSE)
        oddDfs <- lapply(combos, function(c) {
            v <- t[-c,]
            return(v)
        })
        oddDf <- data.frame() %>%
          bind_rows(
        oddDfs[lapply(oddDfs, function(df)
          {round(sum(df$amount,na.rm=TRUE))}
        )  == residual##0
        ] %>% as.data.frame()
          )

        sumFlag <- NROW(oddDf)>0
        n <- n + 1

    }

    oddObs <- t %>%
      dplyr::filter(id %in% oddDf$id)
    if(NROW(oddObs)==0) {
      oddObs <- t
    }

    return(oddObs) ##4) Return the relevant trades/transactions that were not explained away

}

##' Compiler function that runs find and clean outlier functions above for a given data frame of returns
##'
##' This is the description
##'
##' @param factor vector of numeric factors c(upper,lower) used in standard deviation calculation to limit outlier derivation iteration start to end
##' @param df the returns (NAV) data frame
##' @param session The rdecaf session
##' @param resources the rdecaf resources endpoint. Not required, defaults to NULL
##' @param correct magnitude factor applied in calculation used to derive relevancy of outlier residual.
##' @param thresh magnitude factor applied in calculation used to derive relevancy of outlier residual.
##' @param toler buffer factor applied in calculation used to derive relevancy of outlier residual.
##' @param portfolio The portfolio id. Not required, defaults to NULL, taken from outliers if not found
##' @param date The outlier date. Not required, defaults to NULL, taken from outliers if not found
##' @param xVal true/false to dictate whether external valuation overrides should ignore outlier derivation
##' @param currency The outlier currency. Not required, defaults to NULL, taken as reference currency if not found
##' @param excluded vector defining resources (from quants endpoint) to exclude in analysis, e.g. fx future. Defaults to NULL
##' @param inferred vector defining resources (from quants endpoint) to switch the sign of, e.g. loan. Defaults to NULL
##' @param tradeLvl true/false to dictate whether residual should be calculated in aggregate or trade level. Aggregate by default

##' @return A data frame with the outlier dates and details.
##' @export
treatSeries <- function(factor=c(8,3.5),
                        df,
                        session,
                        resources,
                        correct=.5,
                        thresh=.02,
                        toler=.2,
                        portfolio=NULL,
                        date=NULL,
                        xVal=FALSE,
                        currency=NULL,
                        excluded=c("FUT","FXFWD"),
                        inferred=c("LOAN"),
                        tradeLvl=FALSE) {

    ##:
    df <- prefaceSeries(df)

    skewness <- unique(df$skewness) 

## if the data series is severely skewed in either direction, increased the floor by a factor of 2 points, reducing false positives 
    floor <- factor[2]

    !is.na(floor) || {
        print("Issue with floor parameter in outlier derivation")
        return(NULL)
    }

    if(abs(skewness)>1) {
        floor <- floor + 2
    }

    df <- df %>%
        dplyr::select(-skewness)

## the skeleton series to be returned if error | NULL
    returnSeries <- data.frame(
        df,
        "diff"=as.numeric(NA),
        "diffAbs"=as.numeric(NA),
        "outlierFactor"=as.numeric(NA),
        "portfolio"=safeNull(portfolio),
        "amount"=as.numeric(NA),
        "corrected"=as.numeric(NA),
        "trades"=as.character(NA),
        "rationale"=as.character(NA),
        stringsAsFactors=FALSE
    ) %>%
        dplyr::arrange(date)

if(NROW(df[!df$absurd,])==0) {
  print("Return series too absurd to analyze further")
  return(returnSeries)
}

## Run the first pass outlier statistical function
seriesOutliers <- findOutlier(series=df
  ,session=session,fctr=factor[1],flor=floor,xVal=xVal,portfolio=portfolio)

outliers <- seriesOutliers[!is.na(seriesOutliers$outlierFactor),]

if(NROW(outliers)==0) {
    print("No Outliers Detected")
    return(returnSeries)
}


outliersTrades <- lapply(1:NROW(outliers)
    , function(i) {
    print(outliers[i,]$date) ## for each date, determine if trades/transactions explain away the outlier via residuals
    cleanOutlier(outliers[i,],session=session,portfolio=portfolio,date=date,currency=currency,excluded=resources[resources$ctype %in% excluded,]$id,inferred=resources[resources$ctype %in% inferred,]$id,correct=correct,thresh=thresh,toler=toler)
    })

outlierAmounts <- data.frame() %>%
  dplyr::bind_rows(
    lapply(outliersTrades,function(x) x )
  )


if(NROW(outlierAmounts)==0) {
    print("No Corrections Determined")
    seriesOutliers <- data.frame(seriesOutliers,
        amount=0,
        corrected=seriesOutliers$return,
        trades=as.character(NA),
        rationale=as.character(NA),
        stringsAsFactors=FALSE)
    return(seriesOutliers)
}

##medianNav <- median(df$return,na.rm=TRUE)

if(!tradeLvl) { ## Summarize the data at date level if not trade level
  outlierAmounts <- outlierAmounts %>%
    dplyr::select(-amount) %>%
    dplyr::rename(amount=residual) %>%
    dplyr::group_by(commitment) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup()
}

returnSeries <- seriesOutliers %>%
  dplyr::left_join(outlierAmounts %>% dplyr::select(commitment,amount,trade,rationale), by=c("date"="commitment")) %>% ## join the residual data
  dplyr::group_by(across(-c(amount,trade))) %>%
  dplyr::summarise(
     amount=sum(amount)
    ,trades=paste0(na.omit(trade),collapse=";") ## concatenated value of the identified trade IDs
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(corrected=return+(-1*amount)) %>% ## Suggest a correction to return value (e.g. NAV) by the unexplained residual
  dplyr::arrange(date)

return(returnSeries)

}

##' Wrapper function that runs find and clean outlier functions above for a given portfolio or set of portfolios
##'
##' This is the description
##'
##' @param date the end date for the analysis. Defaults to present.
##' @param resources the rdecaf resources endpoint.
##' @param excluded the instrument types to exclude from the statistical outlier determination.
##' @param inferred the instrument types to flip the sign on in the the clean outlier function.
##' @param external T/F whether to apply the external override logic (see functions above).
##' @param tradeLvl T/F whether to explain the outliers via residual aggregate or trade level (see functions above).
##' @param params the remaining statistical outlier parameters (see functions above).
##' @param portfolios the rdecaf portfolios endpoint. Can be a subset.
##' @param session The rdecaf session
##' @param lookBack the backward time horizon from the date to be analyzed. Defaults to one year.

##' @return A list with a detailed data frame and a summarized/aggregate data frame.
##' @export
outlierWrapper <- function(date=Sys.Date(),
                           resources,
                           excluded=c("FUT","FXFWD"),
                           inferred=c("LOAN"),
                           external=FALSE,
                           tradeLvl=FALSE,
                           params=c("correct"=.5,"thresh"=.02,"toler"=.2,"ceiling"=8,"floor"=3.5),
                           portfolios,
                           session,
                           lookBack=365) {

if(is.null(resources)) {
    resources <- getDBObject("resources",session=session)
}

correct <- params[names(params)=="correct"]
thresh  <- params[names(params)=="thresh"]
toler   <- params[names(params)=="toler"]
ceiling <- params[names(params)=="ceiling"]
floor   <- params[names(params)=="floor"]
factor  <- c(ceiling,floor)

start <- as.Date(date-lookBack)

portfolioOutliers <- lapply(1:NROW(portfolios), function(o) {

  date_gte <- max(portfolios[o,]$inception,start,na.rm=TRUE)

  print(paste(o,portfolios[o,]$id,sep=") "))

  cons <- getResource("pconsolidations", params=list("portfolio"=portfolios[o,]$id, "page_size"=-1, "date__gte"=date_gte, "price__gt"=0), session=session)
  navs <- data.frame() %>%
    dplyr::bind_rows(
      lapply(cons, function(x) {
        data.frame(date=as.Date(x$date),return=as.numeric(x$nav),stringsAsFactors=FALSE)
      })
    )

  NROW(navs)>= 30 || { ## Only bother if we have at least 30 data points for statistics sake
    print("Insufficient History")
    return(NULL)
    }

  navs <- navs[abs(diff(navs$date)) < 7,] ## We should have continuous data points as much as possible

  NROW(navs)>0 || {
    print("History too scattered")
    return(NULL)
    }

  seriesT <- treatSeries(factor=factor,
                        resources=resources,
                        df=navs,
                        correct=correct,
                        thresh=thresh,
                        toler=toler,
                        xVal=external,
                        tradeLvl=tradeLvl,
                        portfolio=portfolios[o,]$id,
                        currency=portfolios[o,]$rccy,
                        session=session) %>%
    dplyr::filter(!is.na(outlierFactor)|absurd)
  if(NROW(seriesT)==0) {return(NULL)}
  seriesT <- seriesT %>%
    dplyr::mutate(portfolioName=as.character(portfolios[o,]$name),portfolioCurrency=as.character(portfolios[o,]$rccy))
  return(seriesT)
}
)

outliersDf <- data.frame() %>%
  dplyr::bind_rows(
    lapply(portfolioOutliers, function(x) x)
  )

NROW(outliersDf) > 0 || {
    print("No Relevant Outliers Detected")
    return(NULL)
}

details <- outliersDf %>%
  dplyr::mutate(
    firstPass=!absurd,
    secondPass=!is.na(rationale)&rationale=="Not Applicable",
    isExtVal=!is.na(outlierFactor)&outlierFactor==0,
    secondPass=if_else(isExtVal,FALSE,secondPass),
    deepDiveReq=absurd
    ) %>%
  dplyr::mutate(positCorrection=if_else(secondPass,-1*amount,0),positNAV=if_else(secondPass,corrected,return)) %>%
  dplyr::rename(NAV=return,deltaNAV=diff) %>%
  dplyr::mutate(deltaNAVPct=deltaNAV/NAV) %>%
  dplyr::select(portfolio,portfolioName,portfolioCurrency,date,contains("NAV"),contains("posit"),contains("pass"),isExtVal,deepDiveReq,trades,rationale,outlierFactor)

summary <- details %>% ## default summary
  dplyr::group_by(portfolioName) %>%
  dplyr::summarise(firstPass=sum(as.numeric(firstPass)),secondPass=sum(as.numeric(secondPass)),deepDiveReq=sum(as.numeric(deepDiveReq))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(totalOutliers=firstPass+secondPass+deepDiveReq)

summary <- details %>%
  dplyr::filter(secondPass) %>%
  dplyr::select(portfolio,portfolioName,date,deltaNAV,deltaNAVPct,positCorrection)

  return(list("details"=details,"summary"=summary))

}


