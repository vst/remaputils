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

