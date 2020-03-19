##' A helper function to align 2 results the performance endpoint.
##'
##' This is the description
##'
##' @param primary The primary performance result.
##' @param secondary The secondary performance result.
##' @return The secondary performance result aligne to the primary.
##' @export
performance.AlignData <- function(primary, secondary) {

    ## Get the index, i.e date data from secondary:
    bIdx <- unlist(secondary[["indexed"]][["index"]])

    ## Get the data, i.e price data from secondary:
    bVal <- unlist(secondary[["indexed"]][["data"]])

    ## If length of bVal is less than bIdx, add:
    if (length(bVal) < length(bIdx)) {
            bVal <- c(bVal[1], bVal)
    }

    ## Transform secondary data to xts:
    xtsBVal <- xts::as.xts(bVal, order.by=as.Date(bIdx))

    ## Combine the xts data:
    combXTS <- cbind(primary[["xts"]], xtsBVal)

    ## Forward and backward fill the secondary column:
    bXTS <- zoo::na.locf(zoo::na.locf(combXTS), fromLast=TRUE)[, 2]

    ## Reassign the index values:
    secondary[["indexed"]][["index"]] <- as.list(as.character(zoo::index(bXTS)))

    ## Reassign the data values:
    secondary[["indexed"]][["data"]]  <- as.list(as.numeric(bXTS))

    ## Done, return:
    return(secondary)

}


##' A function to compute the relative performance between 2 getPerformanceV2 results.
##'
##' This is the description
##'
##' @param primary The primary getPerformanceV2 result.
##' @param secondary The secondary getPerformanceV2 result.
##' @return The relative performance result.
##' @export
performance.ComputeRelative <- function(primary, secondary) {

    ## Initialize the rPerformance list:
    rPerformance <- list()

    ## Combine the xts:
    combinedXts  <- zoo::na.locf(cbind("container"=primary[["xts"]], "benchmark"=secondary[["xts"]]))

    ## Compute the difference of the price series:
    rPerformance[["xts"]] <- combinedXts[, "container"] - combinedXts[, "benchmark"]

    ## Compue thte difference of the stats:
    rStats <- primary[["stats"]][, -1] - secondary[["stats"]][, -1]

    ## Append the stats data frame:
    rPerformance[["stats"]] <- as.data.frame(cbind("label"=primary[["stats"]][, "label"], rStats), stringsAsFactors=FALSE)

    auxFun <- function(primary, secondary, element) {
        retval <- do.call(cbind, lapply(1:NCOL(primary[["periodStats"]][[element]]), function(i) {
            round(as.numeric(primary[["periodStats"]][[element]][-1, i]) - as.numeric(secondary[["periodStats"]][[element]][-1, i]), 5)
        }))

        retval <- as.data.frame(rbind(as.character(primary[["periodStats"]][[element]][1, ]), retval), stringsAsFactors=FALSE)
        colnames(retval) <- colnames(primary[["periodStats"]][[element]])
        rownames(retval) <- rownames(primary[["periodStats"]][[element]])

        return(retval)

    }

    ## Compute the difference in periodic stats:
    rPeriodStats <- auxFun(primary, secondary, "periodStats")

    ## Append the difference in periodic stats to the relative list:
    rPerformance[["periodStats"]][["periodStats"]] <- rPeriodStats

    ## Compute the difference in current window stats:
    rCurrentWindowStats <- auxFun(primary, secondary, "currentWindowStats")

    ## Append the difference in current window stats to the relative list:
    rPerformance[["periodStats"]][["currentWindowStats"]] <- rCurrentWindowStats

    ## If no historical window, append NULL to historical window stats and return:
    if (any(dim(primary[["periodStats"]][["historicalWindowStats"]]) == 0)) {
        rPerformance[["periodStats"]][["historicalWindowStats"]] <- NULL
        return(rPerformance)
    }

    ## Compute the difference in historical window stats:
    rHistoricalWindowStats <- auxFun(primary, secondary, "historicalWindowStats")

    ## Append the difference in historical window stats to the relative list:
    rPerformance[["periodStats"]][["historicalWindowStats"]] <- rHistoricalWindowStats

    ## Done, return:
    return(rPerformance)

}


##' A function to generate a rich performance data for a container including
##' benchmark and relative performance.
##'
##' This is the description
##'
##' @param containerType The endpoint to query, i.e "portfolios", "shareclasses".
##' @param id The container id.
##' @param start The start date of the desired performance.
##' @param end The end date of the desired performance.
##' @param freq The frequency of the observations. Default is 'daily'.
##' @param benchmark The benchmark id. Default is NULL.
##' @param periodicity The desired alternative periodicity of returns. 'M' for monthly, 'Q' for quarterly.
##' @param window The time window of analysis.
##' @param session The rdecaf session.
##' @return A list with container, benchmark and relative performance data.
##' @export
getPerformanceV2 <- function(containerType,
                             id,
                             start,
                             end,
                             freq="daily",
                             benchmark=NULL,
                             periodicity="M",
                             window="Y",
                             session) {


    ## Initialize the query parameters:
    params <- list("start"=start,
                   "end"=end,
                   "frequency"=freq)

    ## Append the container type and id:
    params[[containerType]] <- id

    ## Get the performance and flatten:
    cPerformance <- flattenPerformance("x"=getResource("performance", params=params, session=session),
                                       "containerType"=containerType,
                                       "currentDate"=end,
                                       "startDate"=start,
                                       "periodicity"=periodicity,
                                       "window"=window)

    ## If no benchmark is given, return:
    if (is.null(benchmark)) {
        return(list("container"=cPerformance,
                    "benchmark"=NULL,
                    "relative"=NULL))
    }

    ## Set endpoint's container type to NULL:
    params[[containerType]] <- NULL

    ## Add the benchmark container type:
    params[["benchmarks"]] <- benchmark

    ## Get the benchmark's performance:
    bPerformance <- getResource("performance", params=params, session=session)

    ## Align the benchmark's data to the container's data:
    ## bPerformance <- performance.AlignData(cPerformance, bPerformance)

    ## Flatten the benchmark data:
    bPerformance <- flattenPerformance(bPerformance, "benchmarks", end, start, periodicity="M", window="Y")

    ## Ensure that the xts series of benchmark has the same length as the container:
    bPerformance[["xts"]] <- bPerformance[["xts"]][zoo::index(bPerformance[["xts"]]) >= min(zoo::index(cPerformance[["xts"]])), ]

    ## Compute the relative performance:
    rPerformance <- performance.ComputeRelative(cPerformance, bPerformance)

    ## Done, return:
    return(list("container"=cPerformance,
                "benchmark"=bPerformance,
                "relative"=rPerformance))
}


##' A function to generate periodic return data for a given window.
##'
##' This is the description
##'
##' @param period The period of analysis, i.e 'W', 'M', Q', 'Y'.
##' @param returns a xts vector with returns.
##' @param window The window for analysis, i.e 'W', 'M', Q', 'Y'.
##' @return A list with periodic returns, current window returns and historical window returns.
##' @export
performance.GetPeriodicTable <- function(period, returns, window) {

    ## The statistics to be applied:
    stats <- c("sum", "mean", "median", "sd", "min", "max")

    ## The metadata for each period:
    pFun <- list("W"=list("apply"=function(x){do.call(cbind, lapply(stats, function(z) xts::apply.weekly(x, z)))},
                          "format"="%d %h %y",
                          "add"=7,
                          "by"="week"),
                 "M"=list("apply"=function(x){do.call(cbind, lapply(stats, function(z) xts::apply.monthly(x, z)))},
                          "format"="%h %y",
                          "add"=31,
                          "by"="month"),
                 "Q"=list("apply"=function(x){do.call(cbind, lapply(stats, function(z) xts::apply.quarterly(x, z)))},
                          "format"="%m %y",
                          "add"=90,
                          "by"="quarter"),
                 "Y"=list("apply"=function(x){do.call(cbind, lapply(stats, function(z) xts::apply.yearly(x, z)))},
                          "format"="%Y",
                          "add"=365,
                          "by"="year"))

    ## Get the periodic returns:
    periodReturns <- round(t(do.call(pFun[[period]][["apply"]], list(returns))), 5)

    ## XTS is broken!!!!!
    colnames(periodReturns)[colnames(periodReturns) == "2020-02-28"] <- "2020-02-29"

    ## Add the dates as separate row:
    periodReturns <- as.data.frame(rbind(as.character(colnames(periodReturns)),
                                         periodReturns),
                                   stringsAsFactors=FALSE)

    ## Append row names:
    rownames(periodReturns) <- c("date", stats)

    ## Get the window Memnonic:
    windowMemnonic <- paste0(window, "-0")

    ## Get the window start:
    windowStart <- dateOfPeriod(windowMemnonic, date=tail(zoo::index(returns), 1))

    ## Get the window end:
    windowEnd <- dateOfPeriod(windowMemnonic, date=tail(zoo::index(returns), 1) + pFun[[window]][["add"]])

    ## Get the current window index:
    currentWindowIdx <- as.Date(as.character(periodReturns["date", ])) > windowStart

    ## Get the current window stats:
    currentWindowReturns <- as.data.frame(periodReturns[, currentWindowIdx], stringsAsFactors=FALSE)

    ## Get the historical window stats:
    histWindowReturns <- as.data.frame(periodReturns[, !currentWindowIdx], stringsAsFactors=FALSE)

    ## Get the forward dates for current window:
    fwdDates <- seq.Date(windowStart, windowEnd, by="day")[-1]

    ## Construct xts for forward dates:
    fwdXts <- xts::as.xts(rep(0, NROW(fwdDates)), order.by=fwdDates)

    ## Initilize a data frame with forward window dates:
    df <- initDF(zoo::index(do.call(pFun[[period]][["apply"]], list(fwdXts))), NROW(currentWindowReturns))

    matchIdx <- match(currentWindowReturns["date", ], colnames(df))

    if (any(is.na(matchIdx))) {
        matchIdx[is.na(matchIdx)] <- NCOL(currentWindowReturns)
    }

    ##
    df[, matchIdx] <- currentWindowReturns

    ## Assign the row names"
    rownames(df) <- c("date", stats)

    rownames(histWindowReturns) <- c("date", stats)

    return(list("periodStats"=periodReturns,
                "currentWindowStats"=df,
                "historicalWindowStats"=histWindowReturns))

}


##' A function to flatten the results from the performance endpoint.
##'
##' This is the description
##'
##' @param x The list form the performance endpoint.
##' @param containerType The type of container, i.e "portfolios". "shareclasses" etc.
##' @param currentDate The date of report.
##' @param startDate The start date.
##' @param periodicity The desired alternative periodicity of returns. 'M' for monthly, 'Q' for quarterly.
##' @param window The time window of analysis.
##' @return A list with xtsPrice, stats, periodStats and returns.
##' @export
flattenPerformance <- function(x, containerType, currentDate, startDate, periodicity="M", window="Y") {

    ## Extract the dates:
    date <- unlist(x[["indexed"]][["index"]])

    ## Extract the series:
    price <- unlist(x[["indexed"]][["data"]])

    ## TODO: This should be handeled in API:
    if (length(price) < length(date)) {
        price <- c(1, price)
    }

    if (min(as.Date(date)) > as.Date(startDate)) {
        addDate <- seq(as.Date(startDate), as.Date(min(date))-1, 1)
        price <- c(rep(1, length(addDate)), price)
        date <- c(addDate, as.Date(date))
    }

    ## Transform data to xts:
    xtsPrice <- xts::as.xts(price, order.by=as.Date(date))

    ## Compute and append the periodic returns:
    returns <- computePeriodicReturns(xtsPrice)

    ## Extract the statistics:
    stats <- x[["statistics"]][["univariate"]][[containerType]][[1]][["stats"]]

    ## Construct data frame for stats:
    stats <- data.frame("label"=sapply(stats, function(x) x$label),
                        do.call(rbind, lapply(stats, function(x) do.call(cbind, lapply(x$stats, function(y) safeNull(y))))),
                        stringsAsFactors=FALSE)

    ## Get the periodic performance stats:
    periodStats <- performance.GetPeriodicTable(periodicity, returns[, "raw"], window)

    ## Return the series as xts and the stats as data frame:
    return(list("xts"=xtsPrice,
                "stats"=stats,
                "periodStats"=periodStats,
                "returns"=returns))

}


##' A wrapper function for calculating the position wise pnls.
##'
##' This is the description
##'
##' @param holdings The holdings list from getPrintableHoldings.
##' @param holdingsParams The list of paramters used for holdings.
##' @param resources The resources data frame.
##' @param ccy The currency of the container.
##' @param date The report date.
##' @param session The rdecaf session.
##' @param portName The container name.
##' @param portfolio The portfolio id.
##' @param begPeriod The beginning period.
##' @param fxobs The FX observatios for the same period.
##' @return The period to date pnls.
##' @export
positionPnLWrapper <- function(holdings, holdingsParams, resources, ccy, date, session, portName, portfolio, begPeriod, fxobs) {

    ## Get the YTD beginning date:
    begDate <- dateOfPeriod(begPeriod, as.Date(date))

    ## TODO:
    holdingsParams[["date"]] <- begDate

    ## Get consolidated holdings:
    holdingsYTD <- do.call("getPrintableHoldings", holdingsParams)

    ## Get the end date:
    dateEnd <- as.Date(date)

    quantParams <- list("account__portfolio"=portfolio,
                        "format"="csv",
                        "page_size"=-1)

    ## Get the quents:
    quants <- as.data.frame(getResource("quants", params=quantParams, session=session))
    quants <- quants[quants[, "commitment"] >= begDate, ]

    ## Get the trades list:
    trdParams <- list("accmain__portfolio"=portfolio,
                      "page_size"=-1,
                      "format"="csv")

    trades <- as.data.frame(getResource("trades", params=trdParams, session=session))
    trades <- trades[trades[, "commitment"] >= begDate, ]

    ## The column selections for the consolidations:
    colSelect <- c("Name", "ID", "QTY", "Value", "Type", "Symbol")

    holdingsYTDX <- holdingsYTD[["rawHoldings"]]

    if (is.null(holdingsYTDX)) {
        holdingsYTDX <- initDF(colSelect, 1)
    } else {
        holdingsYTDX <- holdingsYTD[["rawHoldings"]][, colSelect]
    }

    ## Get the pnl preemble:
    ytdPreemble <- getPnlPreemble("posBeg"=holdingsYTDX,
                                  "posEnd"=holdings[["rawHoldings"]][, colSelect],
                                  "resources"=resources,
                                  "quants"=quants[quants[, "commitment"] > begDate & quants[, "commitment"] <= date, ],
                                  "trades"=trades[trades[, "commitment"] > begDate & trades[, "commitment"] <= date, ],
                                  "portfolio"=portfolio,
                                  "fxobs"=fxobs,
                                  "ccy"=ccy,
                                  "session"=session)

    ## Get the quant contexts':
    ytdContext <- contextualizeQuants(ytdPreemble, begDate, date)

    ## Construct composite ids:
    ytdContext <- lapply(ytdContext, function(x) cbind(x, "comp"=paste0(x[, "date"], x[, "symbol"], abs(as.numeric(x[, "qQty"])), x[, "type"]!="End")))

    ## IMPORTANT: We need to eliminate REVERSALS:
    ## ###########################################
    ## Extract each composit id to list:
    idWiseContext <- lapply(ytdContext, function(x) extractToList(x, "comp"))
    ## Get the YTD context:
    ytdContext <- lapply(idWiseContext, function(x) do.call(rbind, x[sapply(x, function(z) sum(as.numeric(z[, "qQty"])) != 0 |
                                                                                           tail(z[, "type"],1) == "End" |
                                                                                           head(z[, "type"],1) == "Start")]))
    ## Compute PnLs:
    ytdPnL <- computePnL(ytdContext)

    ytdPnL <- ytdPnL[!sapply(ytdPnL, function(x) is.null(x$PnLs))]

    ytdPnL <- lapply(1:length(ytdPnL), function(i) {
        totalPnL <- ytdPnL[[i]][["Totals"]][, "Total"]
        nav <-  holdings[["consolidation"]][["nav"]]
        ytdPnL[[i]][["Totals"]][, "Contr"] <- totalPnL / nav

        ytdPnL[[i]]

    })

    ## Done, return:
    return(ytdPnL)
}


##' A wrapper function for calculating the position wise performance and pnls.
##'
##' This is the description
##'
##' @param portfolio The portfolio id.
##' @param date The date of the report.
##' @param priceField The name of the price id column.
##' @param periods The desired periods.
##' @param session The rdecaf session.
##' @param includeStats Should advanced stats be included?
##' @param excludeWeekends Should weekends be excluded from consideration?
##' @param holdings If pnls are desired, holdings mus be supplied. Default is NULL.
##' @param hParams If pnls are desired, holdings params must be supplied. Default is NULL.
##' @param resources The resources data frame. If NULL (Default), function will get.
##' @return A list with asset returns, asset statistics and asset pnls.
##' @export
assetPerformanceWrapper <- function(portfolio,
                                    date,
                                    priceField,
                                    periods,
                                    session,
                                    includeStats=FALSE,
                                    excludeWeekends=TRUE,
                                    holdings=NULL,
                                    hParams=NULL,
                                    resources=NULL) {

    pnlsYTD <- NULL
    pnlsMTD <- NULL
    returnStats <- NULL

    ## Get the portfolio details:
    portfolio <- as.data.frame(getResource("portfolios", params=list("page_size"=-1, "format"="csv", "id"=portfolio), session=session))

    if (is.null(resources)) {
        ## Get resources:
        resources <- getResourcesByStock(getStocks(portfolio[, "id"], session, zero=1, date=date, c="portfolio"), session, getUnderlying=FALSE)
    }

    ## Get the asset returns:
    assetReturns <- getAssetReturns("date"=date,
                                    "ccy"=portfolio[, "rccy"],
                                    "resources"=resources,
                                    "priceField"=priceField,
                                    "periods"=periods,
                                    "returnOnly"=!includeStats,
                                    "excludeWeekends"=excludeWeekends,
                                    "session"=session)

    ## Flatten the asset returns stats:
    rStats <- data.frame("mtd"=assetReturns[["returnStats"]][["MTD"]][, "Return"],
                         "ytd"=assetReturns[["returnStats"]][["YTD"]][, "Return"],
                         "mtdfx"=assetReturns[["returnStats"]][["MTD"]][, "Total Return"],
                         "ytdfx"=assetReturns[["returnStats"]][["YTD"]][, "Total Return"],
                         "symbol"=rownames(assetReturns[["returnStats"]][["YTD"]]),
                         "ctype"=resources[match(rownames(assetReturns[["returnStats"]][["YTD"]]), resources[, "symbol"]), "ctype"],
                         "lastPxDate"=assetReturns[["returnStats"]][["YTD"]][, "lastPxDate"])

    if (!is.null(holdings)) {
        ## Contributeration:
        pnlsYTD <- positionPnLWrapper(holdings, hParams, resources, portfolio[, "rccy"], date, session, portfolio[, "name"], portfolio[, "id"], "Y-0", assetReturns[["fxObs"]][["YTD"]])
        pnlsMTD <- positionPnLWrapper(holdings, hParams, resources, portfolio[, "rccy"], date, session, portfolio[, "name"], portfolio[, "id"], "M-0", assetReturns[["fxObs"]][["MTD"]])
    }

    if (includeStats) {
        returnStats <- assetReturns[["returnStats"]]
    }

    return(list("assetReturns"=rStats,
                "assetRetStats"=returnStats,
                "assetPnLs"=list("ytd"=pnlsYTD, "mtd"=pnlsMTD)))


}
