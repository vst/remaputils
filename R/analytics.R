##' A function to prepare a data frame with all positions by ctype and holding period
##'
##' This is the description
##'
##' @param ctype The ctype.
##' @param resources The rdecaf resources data frame.
##' @param portfolios The rdecaf portfolios data frame.
##' @param accounts The rdecaf accounts data frame.
##' @param session The rdecaf session.
##' @return A data frame with all holding periods.
##' @export
getOpenPositionByCtype <- function(ctype, resources, portfolios, accounts, session) {

    ## Get the future resources:
    ctypeResources <- resources[resources[, "ctype"] == ctype, ]

    ## Get the trades by futures:
    ctypeTrades <- lapply(ctypeResources[, "id"], function(id) as.data.frame(getResource("trades", params=list(format="csv", page_size=-1, resmain=id), session=.SESSION)))

    ## Extract each future trade by account:
    openPositionData <- do.call(rbind, lapply(ctypeTrades, function(f) {

        ## If no trade by account, return NULL:
        if (NROW(f) == 0){
            return(NULL)
        }

        ## For each future, extract trades by account and prepare open future position data:
        do.call(rbind, lapply(extractToList(f, "accmain"), function(x) {

            ## Order trades data frame by commitment:
            x <- x[order(x[, "commitment"]), ]

            ## Compute the positions:
            x[, "position"] <- cumsum(x[, "qtymain"])

            ## Append the accmain:
            x[, "account"] <- paste(x[, "accmain"], resources[match(x[, "resmain"], resources[, "id"]), "isin"], "PnL Acc")

            ## Append the security ccy:
            x[, "secccy"] <- resources[match(x[, "resmain"], resources[, "id"]), "ccymain"]

            ## Append the closing trades:
            x[x[, "position"] == 0, "closes"] <- as.character(x[x[, "position"] == 0, "commitment"])

            ## Compute the open, close index values:
            indices <- c(-1, diff(abs(x[, "position"]) == 0))

            ## Get the open indices:
            startIdx <- which(indices == -1)

            ## Get the close indices:
            endIdx <- which(indices == 1)

            ## If no close, set to NA:
            if (length(endIdx) ==0) {
                endIdx <- NA
            }

            ## Flatten the open position period and data:
            retval <- do.call(rbind, lapply(1:length(startIdx), function(i) data.frame("open"=x[startIdx[i], "commitment"],
                                                                                       "close"=x[endIdx[i], "commitment"],
                                                                                       "pnlAcct"=x[startIdx[i], "account"],
                                                                                       "secccy"=x[startIdx[i], "secccy"],
                                                                                       "portfolio"=accounts[match(x[, "accmain"][1], accounts[, "id"]), "portfolio"],
                                                                                       "accmain"=x[startIdx[i], "accmain"],
                                                                                       "resmain"=x[startIdx[i], "resmain"])))

            ## Append the reference currency:
            retval <- data.frame(retval, "rccy"=portfolios[match(retval[, "portfolio"], portfolios[, "id"]), "rccy"])

            ## Done, return:
            retval

        }))
    }))

    ## Done, return:
    return(openPositionData)
}


##' A function to retrieve and slice OHLC observations as per given period.
##'
##' This is the description
##'
##' @param symbols A vector of ohlcs symbols.
##' @param session The rdecaf session.
##' @param date The report date.
##' @param periods A vector with desired period slices: 'DTD', 'WTD', 'MTD', 'QTD', 'YTD'
##' @param excludeWeekends Should the weekends be excluded? Default to TRUE.
##' @return A list with the slices per period.
##' @export
getSlicedOhlcs <- function(symbols, session, date, periods, excludeWeekends=TRUE) {

    ## Get the dateOfPeriod memnonic:
    periodMemnonic <- sapply(periods, function(x) switch(x,
                                                        "YTD"="Y-0",
                                                        "QTD"="Q-0",
                                                        "MTD"="M-0",
                                                        "WTD"="W-1",
                                                        "DTD"="D-0"))

    ## Order the periods:
    periodMemnonic <- orderByKey(periodMemnonic, c("Y", "Q", "M", "W", "D"))

    ## Compute the look back:
    lookBack <- as.numeric(date - dateOfPeriod(periodMemnonic[1], date)) + 27

    ## Get the ohlcs:
    ohlcs <- lapply(symbols, function(s) getOhlcObsForSymbol(session, s, lte=date, lookBack=lookBack, excludeWeekends))

    ## Construct the functions:
    myFun <- paste0("get", periods, "Slice")

    ## Get the period ohlcs:
    periodOhlcs <- lapply(myFun, function(fun) lapply(ohlcs, function(y) do.call(fun, list(y, date))))

    ## Assign the symbols as names to each period list:
    for (i in 1:length(periods)) {
        names(periodOhlcs[[i]]) <- symbols
    }

    ## Assign the names:
    names(periodOhlcs) <- periods

    ## Done, return:
    periodOhlcs

}


##' Provides the return statistics for a data frame with price and date column
##'
##' This is the description
##'
##' @param df The data frame.
##' @param pxCol The name of the price column.
##' @param dtCol The name of the date column.
##' @param method The return calcuation method:'discrete', 'log'
##' @param returnOnly Should only the Total Return be calculated?
##' @return A data frame with the return statistics.
##' @export
computeReturnStats <- function(df, pxCol, dtCol, method="discrete", returnOnly=FALSE) {

    ## Remove zero prices:
    df <- df[df[, pxCol] != 0 | df[, pxCol] != "0", ]

    ## If empty df, return NA's:
    if (is.null(df) | NROW(df) == 0) {
        return(data.frame("Return"=NA,
                          "Return Annualized"=NA,
                          "Volalitiy"=NA,
                          "Vol Annualized"=NA,
                          "Value-At-Risk"=NA,
                          "Expected Shortfall"=NA,
                          "Sharpe Ratio (STDEV)"=NA,
                          "Sharpe Ratio (VaR)"=NA,
                          "Sharpe Ratio (ES)"=NA,
                          "Calmar Ratio"=NA,
                          "Sortino Ratio"=NA,
                          "Sterling Ratio"=NA,
                          "Avg. Drawdown"=NA,
                          "Max. Drawdown"=NA,
                          "Avg. Recovery"=NA,
                          "Skewness"=NA,
                          "Kurtosis"=NA,
                          "Quantile Ratio"=NA,
                          check.names=FALSE,
                          stringsAsFactors=FALSE))
    }

    ## XTSify:
    ts <- xts::as.xts(as.numeric(df[, pxCol]), order.by=as.Date(df[, dtCol]))

    ## Compute returns:
    rets <- diff(log(ts))

    ## If there are a few observations, return only Total Return:
    if (NROW(df) < 7 | returnOnly) {
        return(data.frame(##"Return"=sum(na.omit(rets)),
                          "Return"=tail(as.numeric(ts), 1) / head(as.numeric(ts), 1) - 1,
                          "Return Annualized"=NA,
                          "Volalitiy"=NA,
                          "Vol Annualized"=NA,
                          "Value-At-Risk"=NA,
                          "Expected Shortfall"=NA,
                          "Sharpe Ratio (STDEV)"=NA,
                          "Sharpe Ratio (VaR)"=NA,
                          "Sharpe Ratio (ES)"=NA,
                          "Calmar Ratio"=NA,
                          "Sortino Ratio"=NA,
                          "Sterling Ratio"=NA,
                          "Avg. Drawdown"=NA,
                          "Max. Drawdown"=NA,
                          "Avg. Recovery"=NA,
                          "Skewness"=NA,
                          "Kurtosis"=NA,
                          "Quantile Ratio"=NA,
                          check.names=FALSE,
                          stringsAsFactors=FALSE))
    }

    ## Compute standard deviation:
    stdev <- PerformanceAnalytics::StdDev(rets)

    ## Compute annualized return:
    retsAnnual <- PerformanceAnalytics::Return.annualized(rets)

    ## Compute annualized standard deviation:
    stdevAnnual <- PerformanceAnalytics::sd.annualized(rets)

    ## Compute the sharpe:
    sharpe <- PerformanceAnalytics::SharpeRatio(rets)

    ## Compute the calmar:
    calmar <- PerformanceAnalytics::CalmarRatio(rets)

    ## Compute the sortino:
    sortino <- PerformanceAnalytics::SortinoRatio(rets)

    ## Compute the sterling:
    sterling <- PerformanceAnalytics::SterlingRatio(rets)

    ## Compute the average recovery:
    avgRecovery <- PerformanceAnalytics::AverageRecovery(rets)

    ## Compute the average drawdown:
    avgDrawdown <- PerformanceAnalytics::AverageDrawdown(rets)

    ## Compute the maximum drawdown:
    maxDrawdown <- PerformanceAnalytics::maxDrawdown(rets)

    ## Compute the VaR:
    var <- PerformanceAnalytics::VaR(rets)

    ## Comput the Expected Shortfall
    es <- PerformanceAnalytics::ES(rets)

    ## Compute the skewness:
    skew <- PerformanceAnalytics::skewness(rets)

    ## Compute the skewness:
    kurt <- PerformanceAnalytics::kurtosis(rets)

    ## Quantile Ratio:
    quantileRatio <- as.numeric(abs(quantile(as.numeric(na.omit(rets)))[2]) / abs(quantile(as.numeric(na.omit(rets)))[4]))

    ## Construct data frame and return:
    data.frame(##"Return"=sum(na.omit(rets)),
               "Return"=tail(as.numeric(ts), 1) / head(as.numeric(ts), 1) - 1,
               "Return Annualized"=as.numeric(retsAnnual),
               "Volalitiy"=as.numeric(stdev),
               "Vol Annualized"=as.numeric(stdevAnnual),
               "Value-At-Risk"=as.numeric(var),
               "Expected Shortfall"=as.numeric(es),
               "Sharpe Ratio (STDEV)"=as.numeric(sharpe[1,1]),
               "Sharpe Ratio (VaR)"=as.numeric(sharpe[2,1]),
               "Sharpe Ratio (ES)"=as.numeric(sharpe[3,1]),
               "Calmar Ratio"=as.numeric(calmar),
               "Sortino Ratio"=as.numeric(sortino),
               "Sterling Ratio"=as.numeric(sterling),
               "Avg. Drawdown"=as.numeric(avgDrawdown),
               "Max. Drawdown"=as.numeric(maxDrawdown),
               "Avg. Recovery"=as.numeric(avgRecovery),
               "Skewness"=as.numeric(skew),
               "Kurtosis"=as.numeric(kurt),
               "Quantile Ratio"=as.numeric(quantileRatio),
               check.names=FALSE,
               stringsAsFactors=FALSE)

}


##' Provides the asset returns MTD and YTD including FX returns
##'
##' This is the description
##'
##' @param series The portfolio id.
##' @param anchors The anchors
##' @return A data frame with the asset returns.
##' @export
exfoliateSeries <- function(series, anchors) {




}



##' Provides the asset returns MTD and YTD including FX returns
##'
##' This is the description
##'
##' @param date The date of consideration.
##' @param ccy The portfolio currency.
##' @param resources The resource data frame.
##' @param periods A vector with periods: c('DTD', 'WTD', 'MTD', 'QTD', 'YTD').
##' @param returnOnly Consider only Total Return?
##' @param excludeWeekends Should the weekends be excluded? Default to TRUE.
##' @param session The rdecaf session.
##' @return A data frame with the asset returns.
##' @export
getAssetReturns <- function(date, ccy, resources, periods, returnOnly, excludeWeekends, session) {

    ## Get the slices ohlcs:
    slicedOhlcs <- getSlicedOhlcs(resources[, "ohlcID"], session, date, periods, excludeWeekends)

    ## Get the returns:
    returnStats <- lapply(slicedOhlcs, function(s) do.call(rbind, lapply(s, function(y) computeReturnStats(y, "close", "date", method="discrete", returnOnly=returnOnly))))

    ## Append the FX pair:
    returnStats <- lapply(returnStats, function(x) data.frame(x, "pair"=as.character(paste0(resources[, "ccymain"], ccy)), check.names=FALSE))

    ## Get the unique FX pairs:
    uniquePairs <-as.character(unique(returnStats[[1]][, "pair"]))

    ## Get the slices ohlcs:
    slicedFX <- getSlicedOhlcs(uniquePairs, session, date, periods)

    ## Get the returns:
    returnFXStats <- lapply(slicedFX, function(s) do.call(rbind, lapply(s, function(y) computeReturnStats(y, "close", "date", method="discrete", returnOnly=returnOnly))))

    ## Append the FX and total returns:
    returnStats <- lapply(1:length(returnStats), function(i) {
        retval <- data.frame(returnStats[[i]],
                             "fxRet"=returnFXStats[[i]][match(returnStats[[i]][, "pair"], rownames(returnFXStats[[i]])), "Return"],
                             check.names=FALSE)
        retval[is.na(retval[, "fxRet"]), "fxRet"] <- 0
        retval[, "Total Return"] <- retval[, "Return"] + retval[, "fxRet"]
        retval
    })

    ## Name the list:
    names(returnStats) <- periods

    ## Done, return
    returnStats

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
