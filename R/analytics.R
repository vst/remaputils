##' Provides for a time series smoothing, date ascension and limit expansion
##'
##' This is the description
##'
##' @param df A data frame with 'value' and 'date' columns.
##' @param smooth The smoothing parameter.
##' @param limitFactor The factor with which to expand the limits.
##' @return A list with the date-ascended and smoothed values and the expanded lower and upper limits.
##' @export
timeSeriesTransform <- function(df, smooth=0.3, limitFactor=0.1) {

    ## Transform df into time series:
    ts <- timeSeries::as.timeSeries(df[, "value"], df[, "date"])

    ## Smoothen the values:
    lw <- timeSeries::smoothLowess(ts, smooth)[, "lowess"]

    ## Convert back to basic data types:
    value <- as.numeric(lw)
    date <- rownames(lw)

    ## Order values based on date:
    value <- value[order(as.Date(date))]
    date  <- as.character(date[order(as.Date(date))])

    ## Expand the limits:
    limits <- expandLimits(value, factor=limitFactor)

    ## Done, return:
    list("value"=value,
         "date"=date,
         "limits"=limits)
}


##' A function to produce the option deltas.
##'
##' This is the description
##'
##' @param holdings The holdings.
##' @param resources The resources.
##' @return The option deltas.
##' @export
optionDelta <- function(holdings, resources) {

    bscallimpvol(s=2649, k=2800, r=0.08, tt=as.numeric(as.Date("2018-12-21") - Sys.Date()) / 365, d=0, price=5.5)


}

##' A function to detect outliers in xts return series:
##'
##' This is the description
##'
##' @param xtsReturns Return series of xts.
##' @param factor The factor of stdev to apply
##' @return A vector with TRUE/FALSE values
##' @export
returnOutliers <- function(xtsReturns, factor) {

    ## Initialise the outlier vector:
    outliers <- rep(FALSE, NROW(xtsReturns))

    ## Iterate:
    for (i in 1:10) {

        ## Current outliers:
        cOutliers <- abs(xtsReturns[,1]) > sd(xtsReturns[,1]) * factor & abs(xtsReturns[,1]) > 0.0040

        if (all(!cOutliers)) {
            break
        }

        ## Get absolute outlier:
        absOutliers <- cOutliers * abs(xtsReturns)

        ## Get maximum outlier index:
        maxOutlierIdx <- which(absOutliers == max(absOutliers))

        ## Correct outlier index:
        xtsReturns[maxOutlierIdx, "returns"] <- 0

        ## Store the captured outliers:
        outliers[maxOutlierIdx] <- TRUE

    }

    ## Return:
    outliers
}
