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


