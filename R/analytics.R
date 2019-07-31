##' Provides the asset returns MTD and YTD including FX returns
##'
##' This is the description
##'
##' @param portfolio The portfolio id.
##' @param date The date of consideration.
##' @param ccy The portfolio currency.
##' @param resources The resource data frame.
##' @param session The rdecaf session.
##' @return A data frame with the asset returns.
##' @export
getAssetReturns <- function(portfolio, date, ccy, resources, session) {

    ## Construct the params:
    params <- list("p"=portfolio,
                   "start"=dateOfPeriod("Y-2"),
                   "end"=date)

    ## Get the asset evolves:
    assetReturns <- rdecaf::getResource("returnsgrid", params=params, session=session)

    if (length(assetReturns) == 0) {
        return(data.frame("symbol"=NA,
                          "mtd"=NA,
                          "ytd"=NA,
                          "ccy"=NA,
                          "ctype"=NA,
                          "pair"=NA,
                          "ytdFXRet"=NA,
                          "mtdFXRet"=NA,
                          "ytdTotal"=NA,
                          "mtdTotal"=NA))

    }

    ## Prepare the asset returns data frame:
    assetReturns <- do.call(rbind, lapply(assetReturns, function(x) {

        ## Get the symbol start index:
        symbolSttIdx <- head(which(strsplit(x[[1]], "")[[1]] == "["), 1) + 1

        ## Get the symbol end index:
        symbolEndIdx <- tail(which(strsplit(x[[1]], "")[[1]] == "]"), 1) - 1

        ## Construct the data frame and return:
        data.frame("symbol"=substr(x[[1]], symbolSttIdx, symbolEndIdx),
                   "mtd"=x[[2]][[2]][["value"]],
                   "ytd"=x[[2]][[3]][["value"]])
    }))


    ## Append the resource's ccymain:
    assetReturns[, "ccy"] <-  resources[match(assetReturns[, "symbol"], resources[, "symbol"]), "ccymain"]

    ## Append the resource's ctype:
    assetReturns[, "ctype"] <-  resources[match(assetReturns[, "symbol"], resources[, "symbol"]), "ctype"]

    ## Prepare the FX pairs:
    assetReturns[, "pair"] <- paste0(assetReturns[, "ccy"], ccy)

    ## Get the unique FX pairs:
    uniquePairs <-unique(assetReturns[, "pair"])

    ## Get the ytd FX series:
    ytdFX <- lapply(uniquePairs, function(x) getOhlcObsForSymbol(session, x, date, lookBack=date - dateOfPeriod("Y-0")))

    ## Get the mtd FX series:
    mtdFX <- lapply(uniquePairs, function(x) getOhlcObsForSymbol(session, x, date, lookBack=date - dateOfPeriod("M-0")))

    ## Prepare the YTD FX returns:
    ytdFXRet <- sapply(ytdFX, function(x) ifelse(NROW(x) == 0, 0, (head(x[, "close"], 1) / tail(x[, "close"], 1)) - 1))
    names(ytdFXRet) <- uniquePairs

    ## Prepare the MTD FX returns:
    mtdFXRet <- sapply(mtdFX, function(x) ifelse(NROW(x) == 0, 0, (head(x[, "close"], 1) / tail(x[, "close"], 1)) - 1))
    names(mtdFXRet) <- uniquePairs

    ## Append the YTD FX return to asset returns:
    assetReturns[, "ytdFXRet"] <- ytdFXRet[match(assetReturns[, "pair"], names(ytdFXRet))]

    ## Append the YTD FX return to asset returns:
    assetReturns[, "mtdFXRet"] <- mtdFXRet[match(assetReturns[, "pair"], names(mtdFXRet))]

    ## Compute Total YTD returns:
    assetReturns[, "ytdTotal"] <- assetReturns[, "ytd"] + assetReturns[, "ytdFXRet"]

    ## Compute Total MTD returns:
    assetReturns[, "mtdTotal"] <- assetReturns[, "mtd"] + assetReturns[, "mtdFXRet"]

    ## Done, return
    assetReturns

}


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
