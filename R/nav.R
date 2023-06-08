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

