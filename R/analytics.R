##' A function to compute the aggregate exposure of holdings data frame.
##'
##' @param holdings A data frame with column names.
##' @param keys The keys to be aggregated.
##' @return A list with the exposures
##' @export
##'
getAggregateExposure <- function(holdings, keys) {
    exposures <- lapply(keys, function(x) {
        aggs <- aggregate(holdings[, "Exposure"], list(holdings[, x]), sum)
        aggs <- aggs[order(aggs[, 2], decreasing=TRUE), ]
        colnames(aggs) <- c(x, "Exposure")
        aggs
    })

    names(exposures) <- keys

    ## Done, return:
    exposures
}


##' A function to compute the periodic returns of xts price.
##'
##' @param price An xts price vector.
##' @return A xts returns data frame.
##' @export
##'
computePeriodicReturns <- function(price) {

    ## Compute the discrete returns:
    returns <- PerformanceAnalytics::CalculateReturns(price, method = c("discrete"))

    ## Set NA returns to 0:
    returns[is.na(returns)] <- 0

    ## Append monthly and yearly columns:
    returns <- cbind("raw"=returns, "monthly"=0, "yearly"=0)

    ## Compute the monthly returns:
    monthlyReturns <- xts::apply.monthly(returns[, "raw"], sum)

    ## Compute the yearly returns:
    yearlyReturns  <- xts::apply.yearly(returns[, "raw"], sum)

    ## Assign the monthly returns to the daily return data frame:
    returns[!is.na(match(zoo::index(returns), zoo::index(monthlyReturns))), "monthly"] <- as.numeric(monthlyReturns)

    ## Assign the yearly returns to the daily return data frame:
    returns[!is.na(match(zoo::index(returns), zoo::index(yearlyReturns))), "yearly"] <- as.numeric(yearlyReturns)

    ## Compute the cumulative returns:
    returns <- cbind(returns, "cumrets"=cumprod(1 + as.numeric(returns[, "raw"]))-1)

    ## Done, return
    return(returns)

}


##' This function to get the usage metrics.
##'
##' @param userActions The user actions result.
##' @param tokenSession The token session data frame.
##' @return A xts usage metrics data frame.
##' @export
##'
getUsageMetrics <- function(userActions, tokenSession) {

    usages[, "Created"] <- as.POSIXct(sapply(strsplit(as.character(usages[, "Created"]), "\\."), function(x) gsub("T", " ", x[1])))

    xtsElapsed <- do.call(cbind, lapply(extractToList(tokenSession, "Username"), function(x) {
        aggs <- aggregate(x[, "Elapsed"], list(as.Date(x[, "Created"])), sum)
        xts::as.xts(aggs[, 2], order.by=aggs[, 1])
    }))

    colnames(xtsElapsed) <- unique(tokenSession[, "Username"])

    xtsMetrics <- lapply(names(userActions), function(user) {

        if (user == "AUTO") {
            return(NULL)
        }

        df <- cbind(userActions[[user]],
                    userActions[[user]][, "Created"] + userActions[[user]][, "Updated"],
                    xtsElapsed[, user])

        colnames(df) <- c("created", "updated", "total", "elapsed")

        df[, "elapsed"] <- round(df[, "elapsed"] / 3600, 2) * 0.5

        df[is.na(df)] <- 0

        metric <- df[, "elapsed"] / df[, "total"]

        metric[is.infinite(metric)] <- 0

        metric[is.na(metric)] <- 0

        metric <- round(zoo::rollmean(metric, 20), 2)

        list("metric"=metric,
             "elapsed"=round(zoo::rollmean(df[, "elapsed"], 20), 2),
             "created"=round(zoo::rollmean(df[, "created"], 20), 2),
             "updated"=round(zoo::rollmean(df[, "updated"], 20), 2),
             "total"=round(zoo::rollmean(df[, "total"], 20), 2))
    })

    names(xtsMetrics) <- names(userActions)

    ## Done, return
    xtsMetrics

}


##' This function gets the historical user actions in a deployment.
##'
##' @param session The rdecaf session.
##' @param endpoint The endpoint to investigate, i.e "trades", "quants".
##' @param exclCols The columns to exclude.
##' @param exclKeys The keys to exclude from exclCols.
##' @param actionQueryParams Addtional query parameters to be passed to params.
##' @return A table with the automation rate.
##' @export
##'
getHistoricalUserActions <- function(session, endpoint, exclCols=NULL, exclKeys=NULL, actionQueryParams=NULL) {

    ## Action params:
    params <- c(list("page_size"=-1, "format"="csv"), actionQueryParams)

    ## Get actions:
    actions <- as.data.frame(getResource(endpoint, params=params, session=session))

    ## Get users:
    users <- as.data.frame(getResource("users", params=list("page_size"=-1, "format"="csv"), session=session))

    ## Replace creator id with creator username:
    actions[, "creator"] <- users[match(actions[, "creator"], users[, "id"]), "username"]

    ## Replace NA creators with "AUTO"
    actions[, "creator"] <- ifelse(is.na(actions[, "creator"]), "AUTO", actions[, "creator"])

    ## Replace updater id with updater username:
    actions[, "updater"] <- users[match(actions[, "updater"], users[, "id"]), "username"]

    ## Replace NA updaters with "AUTO"
    actions[, "updater"] <- ifelse(is.na(actions[, "updater"]), "AUTO", actions[, "updater"])

    ## Parse the created date:
    actions[, "created"] <- as.POSIXct(sapply(strsplit(as.character(actions[, "created"]), "\\."), function(x) gsub("T", " ", x[1])))

    ## Parse the uupdated date:
    actions[, "updated"] <- as.POSIXct(sapply(strsplit(as.character(actions[, "updated"]), "\\."), function(x) gsub("T", " ", x[1])))

    ## Append the is updated column:
    actions[, "isUpdated"] <- actions[, "created"] != actions[, "updated"]

    ## Exclude keys in columns:
    if (!is.null(exclCols)) {
        actions <- actions[!mCondition(actions, exclCols, exclKeys), ]
    }

    ## Get the unique users:
    uniqueUsers <- unique(as.character(unlist(actions[, c("creator", "updater")])))

    ## Compute the actions for each user:
    actionHistory <- lapply(uniqueUsers, function(x) {
        df <- actions
        df[, "createdByUser"] <- (df[, "creator"] == x)
        df[, "updatedByUser"] <- (df[, "updater"] == x & df[, "isUpdated"])

        tsC <- aggregate(df[, "createdByUser"], list(as.Date(df[, "created"])), sum)
        tsU <- aggregate(df[, "updatedByUser"], list(as.Date(df[, "updated"])), sum)

        tsC <- xts::as.xts(tsC[, 2], order.by=as.Date(tsC[, 1]))
        tsU <- xts::as.xts(tsU[, 2], order.by=as.Date(tsU[, 1]))

        ts <- cbind("Created"=tsC, "Updated"=tsU)
        ts[is.na(ts)] <- 0

        ts

    })

    ## Empty users are AUTO:
    uniqueUsers[isNAorEmpty(uniqueUsers)] <- "AUTO"

    ## Add the names:
    names(actionHistory) <- uniqueUsers

    ## Done, return:
    actionHistory
}


##' This function computes the automation rate for users.
##'
##' @param userActions The data frame with the use actions.
##' @return A table with the automation rate.
##' @export
##'
computeAutomationRate <- function(userActions) {

    ## Get the user names:
    users <- names(userActions)

    ## Compute the action counts:
    aTable <- as.data.frame(do.call(rbind, lapply(userActions, colSums)))

    ## Sum the rows of the action counts:
    aTable[, "Total"] <- as.numeric(rowSums(aTable))

    ## Compute the percentages:
    percentages <-  apply(aTable, MARGIN=2, function(x) x / sum(x))

    ## Parse the percentages:
    percentages <- trimws(apply(percentages, MARGIN=2, percentify))

    ## Add column names:
    colnames(percentages) <- paste0(colnames(aTable), " (%)")

    ## Prepare return table:
    aTable <- cbind("User"=users, aTable, percentages)

    ## Get rid of row names:
    rownames(aTable) <- NULL

    ## Done, return:
    aTable
}



##' This function computes the ytd monthly returns using the pconsolidation's internal navs.
##'
##' @param holdings The holdings data frame from getFlatHoldings.
##' @param ccy The portfolio currency.
##' @param resources The resources data frame.
##' @return The fx forward hedge adjusted currency exposure
##' @export
##'
computeCurrencyExposure <- function(holdings, ccy, resources) {

    ## Get the FX Forwards holdings:
    fxfwd <- holdings[holdings[, "Type"] == "FX Forward", ]

    ## Get the main and alternative currency of the forwards:
    resCCYs <- resources[match(fxfwd[, "ID"], resources[, "id"]), c("ccymain", "ccyaltn")]

    ## Get the non FX Forward holdings:
    holds <- holdings[holdings[, "Type"] != "FX Forward", ]

    ## Aggregate exposure by currency:
    aggs <- aggregate(as.numeric(holds[, "Exposure"]), list(holds[, "CCY"]), sum)

    ## Compute the percentages:
    aggs[,2] <- aggs[,2] / sum(holds[, "Exposure"])

    ## If no FX Forward holding, create a ficitios data frame:
    if (NROW(fxfwd) == 0) {
        ccyExp <- data.frame("Currency"=aggs[,1], "Exposure"=percentify(aggs[, 2]))
        ccyExp <- ccyExp[ccyExp[,2] != "0 %", ]
        ccyExp <- ccyExp[ccyExp[,1] != "XAU", ]
        ccyExp <- ccyExp[ccyExp[,1] != "XAG", ]
        ccyExp <- ccyExp[ccyExp[,1] != "XPT", ]
        ## Set the column to NULL:
        colnames(ccyExp) <- NULL
        ## Set the rows to NULL:
        rownames(ccyExp) <- NULL

        return(ccyExp)

    }

    ## Infer the non portfolio currency and append:
    riskCCY <- lapply(1:NROW(resCCYs), function(i) resCCYs[i, is.na(match(resCCYs[i, ], ccy))])
    fxfwd[, "XCCY"] <- sapply(riskCCY, function(x) ifelse(length(x) > 1, NA, x))

    ## If FX Forward holdings exists, compute it's exposure:
    fxfwd[, "expPer"] <- (fxfwd[, "Exposure"] - fxfwd[, "Value"]) / sum(holds[, "Exposure"])

    ## Aggregate fx forward exposure by currency:
    fxAgg <- aggregate(fxfwd[, "expPer"], list(fxfwd[, "XCCY"]), sum)

    ## Compute the addjustment based on fx fowards aggregates:
    aggs[, "adj"] <- fxAgg[match(aggs[,1], fxAgg[,1]), 2]

    ## Set NA's to 0:
    aggs[, "adj"] <- ifelse(is.na(aggs[, "adj"]), 0, aggs[, "adj"])

    ## Match the currencies with the portfolio currency:
    xMatch <- match(ccy, aggs[,1])

    ## If no matching currency, assume no hedge of FX Forward:
    if (all(is.na(xMatch))) {
        aggs[, "adj"] <- 0
    } else {
        ## If match, compute the adjustment to exposure to the FX Forward hedge:
        aggs[xMatch, "adj"] <- sum(fxAgg[, 2]) * -1
    }

    ## Compute the final exposure:
    aggs[, "final"] <- aggs[,2] + aggs[, 3]

    ## Prepare and parse the table:
    ccyExp <- data.frame("Currency"=aggs[,1], "Exposure"=percentify(aggs[, "final"]))
    ccyExp <- ccyExp[ccyExp[,2] != "0 %", ]
    ccyExp <- ccyExp[ccyExp[,1] != "XAU", ]
    ccyExp <- ccyExp[ccyExp[,1] != "XAG", ]
    ccyExp <- ccyExp[ccyExp[,1] != "XPT", ]

    ## Set the column to NULL:
    colnames(ccyExp) <- NULL
    ## Set the rows to NULL:
    rownames(ccyExp) <- NULL

    ## Done, return:
    ccyExp

}


##' This function computes the ytd monthly returns using the pconsolidation's internal navs.
##'
##' @param intPrice The internal price series.
##' @param date The date of the report.
##' @param session The rdecaf session.
##' @return A list with the return objects.
##' @export
##'
getInternalMonthlyReturns <- function(intPrice, date, session) {

    ## The start date:
    startDate <- dateOfPeriod("Y-0", date)

    ## Get the month period dates:
    monthlyDates <- periodDates("M", date=date)

    ## Filter out dates earlier than the start date:
    monthlyDates <- monthlyDates[monthlyDates >= startDate]

    ## XTS the internal price:
    intPrice <- xts::as.xts(intPrice[, "price"], order.by=as.Date(intPrice[, "date"]))

    ## Filter in relevant dates:
    intPrice <- intPrice[zoo::index(intPrice) >= startDate, ]

    intPrice[, 1] <- 100 * cumprod(c(1, head(c(as.numeric(tail(intPrice, -1)), 1) / as.numeric(intPrice), -1)))

    ## If the internal price start date is later than the period start date,
    ## create sequence till internal start date:
    if (zoo::index(intPrice)[1] > dateOfPeriod("Y-0")) {

        ## Create the date series to be appended:
        new <- seq(dateOfPeriod("Y-0"), zoo::index(intPrice)[1], 1)

        ## Remove last date:
        new <- new[-length(new)]

        ## Append the additional dates and prices:
        intPrice <- rbind(xts::as.xts(rep(100, length(new)), order.by=new), intPrice)

    }

    ## Get the internal returns:
    ## returns <- diff(log(intPrice))
    returns <- PerformanceAnalytics::CalculateReturns(intPrice, method = c("discrete"))

    ## Set NA returns to 0:
    returns[is.na(returns)] <- 0

    ## Append periodic column:
    returns <- cbind("internal"=returns, "periodic"=0)

    ## Get the dates to be handled:
    months <- unique(c(monthlyDates, date))

    ## Fill the missing ohlc returns with internal monthly returns:
    for (i in 1:(length(months)-1)) {
        mIdx <- which(zoo::index(returns) == months[i+1])
        nIdx <- which(zoo::index(returns) == months[i])
        returns[mIdx, "periodic"] <- as.numeric(tail(cumprod(1+returns[(mIdx):(nIdx+1), "internal"]) - 1, 1))
    }

    ## Compute the cumulative returns:
    returns <- cbind(returns, "cumrets"=cumprod(1 + as.numeric(returns[, "internal"]))-1)

    ## Initialse the Monthly table:
    df <- initDF(c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

    ## Compute the monthly returns:
    ## mrets <- as.numeric(xts::apply.monthly(returns[, "periodic"], sum))[-1]
    if (all(returns[, "periodic"] == 0)) {
        mrets <- rep(NA, length(months))
    } else {
        mrets <- as.numeric(returns[returns[, "periodic"] != 0, "periodic"])
    }

    ## Assign monthly returns to the monthly table:
    df[1:length(mrets)] <- mrets

    ## Parse the monthly returns:
    monthlyrets <- t(data.frame(paste0(sprintf("%.2f", df*100), " %")))

    ## Remove strings:
    monthlyrets[monthlyrets=="NA %"] <- "   "

    ## Rename columns:
    colnames(monthlyrets) <- colnames(df)
    rownames(monthlyrets) <- NULL

    ## Get the YTD
    ytd <- (as.numeric(tail(intPrice, 1)) - 100) / 100

    ## Done, return:
    list("mretsRaw"=df,
         "mretsTable"=monthlyrets,
         "ytdRaw"=ytd,
         "ytdPrint"=percentify(ytd, 2),
         "returns"=returns)
}


##' This function removes NA's and optionally detects and remove outliers.
##'
##' @param df The data frame with the price and date columns.
##' @param dCol The column name of the date.
##' @param pCol The column name of the price.
##' @param quantile The quantile for the outlier detection
##' @param surpressPlot TODO
##' @return Cleaned price series
##' @import tseries
##' @export
##'
treatPriceSeries <- function(df, dCol, pCol, quantile=0.998, surpressPlot=FALSE){

    ## Return NULL if df is NULL:
    if (is.null(df)) {
        return(df)
    }

    ## Return NULL if not enought observations:
    if (NROW(df) < 5) {
        return(df)
    }

    ## Ensure price column is numeric:
    df[, pCol] <- as.numeric(df[, pCol])

    ## Ensure no zeros:
    df[df[, pCol] == 0, pCol] <- 0.00001

    ## Remove NA's and replace with last observable value:
    price <- xts::as.xts(df[, pCol], order.by=as.Date(df[, dCol]))

    ## Store the orignal NA removed series:
    originalPrice <- price

    ## Compute the original returns using the simple method:
    originalRets <- as.numeric(timeSeries::returns(price, "simple"))

    ## Replace NA for first observations with the mean return:
    originalRets[is.na(originalRets)] <- 0

    ## Compute the returns for our analysis using compoung method:
    rets  <- as.numeric(timeSeries::returns(price, "compound"))

    ## Replace NA for first observations with the mean return:
    ## rets[is.na(rets)]  <- as.numeric(mean(na.omit(rets)))
    rets[is.na(rets)]  <- 0

    ## Create the GARCH(1,1) object using fixed a0, a1, b1 parameters:
    garchObj <- list("order"=c("p"=1, "q"=1),
                     "residuals"=rets*100,
                     "call"=call("garch", x=rets, order = c(1, 1)),
                     "coef"=c("a0"=0, "a1"=0.08, "b1"=0.90),
                     "series"=as.character("rets"))

    ## Assign the GARCH class:
    class(garchObj) <- "garch"

    ## library(tseries)

    ## Fit the GARCH object to the returns and get the estimated conditional volatility:
    fittedGarch <- predict(garchObj, rets)[,1]

    ## First estimate will be our square-root of squared return:
    fittedGarch[1] <- sqrt(rets[1]^2)

    ## Compute the residuals, squared returns less the estimated variance:
    residuals <- rets^2 - fittedGarch^2

    ## Compute the density of the residuals:
    resDensity <- density(residuals)

    ## Sample the residuals using the density bandwidth parameter:
    resDraw <- rnorm(100000, sample(as.numeric(residuals), size = 100000, replace = TRUE), resDensity$bw)

    ## Calculate the cut-off residuals using quantile parameter:
    cutOff <- quantile(resDraw, quantile)

    ## Get the index of outliers:
    outlierIndex  <- which(residuals^2 > cutOff)

    ## Set original return outlier index to zero:
    originalRets[outlierIndex] <- 0

    ## Reconstruct the price series:
    price <- xts::as.xts(cumprod(1 + originalRets) * as.numeric(originalPrice[1]), order.by=zoo::index(originalPrice))

    if (!surpressPlot) {
        ## Run the outlier plot:
        outlierPlot(originalPrice, price, outlierIndex)
        readline(prompt = "Pause. Press <Enter> to continue...")
        dev.off()
    }

    ## Reassign the treated price series:
    df[, pCol] <- as.numeric(price)[match(as.Date(df[, dCol]), zoo::index(price))]

    ## Done, return:
    df
}


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
    openPositionData <- do.call(rbind, lapply(ctypeTrades, function(ctypeTrade) {

        ## If no trade by account, return NULL:
        if (NROW(ctypeTrade) == 0){
            return(NULL)
        }

        ## For each future, extract trades by account and prepare open future position data:
        do.call(rbind, lapply(extractToList(ctypeTrade, "accmain"), function(x) {

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
        return(data.frame("Return"=tail(as.numeric(ts), 1) / head(as.numeric(ts), 1) - 1,
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
    retsAnnual <- PerformanceAnalytics::Return.annualized(rets, geometric=FALSE)

    ## Compute annualized standard deviation:
    stdevAnnual <- PerformanceAnalytics::sd.annualized(rets)

    ## Compute the sharpe:
    sharpe <- as.numeric(retsAnnual) / as.numeric(stdevAnnual)

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
    var <- abs(PerformanceAnalytics::VaR(rets)) * sqrt(250)

    ## Comput the Expected Shortfall
    es <- abs(PerformanceAnalytics::ES(rets)) * sqrt(250)

    ## Compute the skewness:
    skew <- PerformanceAnalytics::skewness(rets)

    ## Compute the skewness:
    kurt <- PerformanceAnalytics::kurtosis(rets)

    ## Quantile Ratio:
    quantileRatio <- as.numeric(abs(quantile(as.numeric(na.omit(rets)))[2]) / abs(quantile(as.numeric(na.omit(rets)))[4]))

    ## Construct data frame and return:
    data.frame("Return"=tail(as.numeric(ts), 1) / head(as.numeric(ts), 1) - 1,
               "Return Annualized"=as.numeric(retsAnnual),
               "Volalitiy"=as.numeric(stdev),
               "Vol Annualized"=as.numeric(stdevAnnual),
               "Value-At-Risk"=as.numeric(var),
               "Expected Shortfall"=as.numeric(es),
               "Sharpe Ratio (STDEV)"=as.numeric(retsAnnual) / as.numeric(stdevAnnual),
               "Sharpe Ratio (VaR)"=as.numeric(retsAnnual) / as.numeric(abs(var)),
               "Sharpe Ratio (ES)"=as.numeric(retsAnnual) / as.numeric(abs(es)),
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
##' @param priceField The name of the price field in resources.
##' @param periods A vector with periods: c('DTD', 'WTD', 'MTD', 'QTD', 'YTD').
##' @param returnOnly Consider only Total Return?
##' @param excludeWeekends Should the weekends be excluded? Default to TRUE.
##' @param session The rdecaf session.
##' @return A data frame with the asset returns.
##' @export
getAssetReturns <- function(date, ccy, resources, priceField="ohlcID", periods, returnOnly, excludeWeekends, session) {

    ## Get the slices ohlcs:
    slicedOhlcs <- getSlicedOhlcs(resources[, priceField], session, date, periods, excludeWeekends)

    slicedOhlcs <- lapply(slicedOhlcs, function(x) lapply(x, function(z) treatPriceSeries(z, "date", "close", quantile=0.998, surpressPlot=TRUE)))

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

    ## Get the last available price dates:
    lastPxDate <- do.call(rbind, lapply(slicedOhlcs, function(x) do.call(rbind, lapply(1:length(x), function(i) {

        ## If Null, return very early date:
        if (is.null(x[[i]])) {
            return(data.frame("Last PX Dt"=as.character("1990-01-01"),
                              "Symbol"=names(x)[i],
                              check.names=FALSE))
        }

        ## Append:
        data.frame("Last PX Dt"=as.character(max(x[[i]][, "date"][x[[i]][, "date"] <= date])),
                   "Symbol"=x[[i]][1, "symbol"],
                   check.names=FALSE)
    }))))

    ## Aggregate the dates by 'max':
    lastPxDate <- aggregate(as.Date(lastPxDate[, "Last PX Dt"]), list(lastPxDate[, "Symbol"]), max)

    ## Append the column names:
    colnames(lastPxDate) <- c("symbol", "date")

    ## Append last available price dates to the main return stats
    returnStats <- lapply(returnStats, function(x) data.frame(x,
                                                              "lastPxDate"=lastPxDate[match(rownames(x), lastPxDate[, "symbol"]), "date"],
                                                              check.names=FALSE,
                                                              stringsAsFactors=FALSE))

    ## Done, return
    return(list("returnStats"=returnStats,
                "posObs"=slicedOhlcs,
                "fxObs"=slicedFX,
                "lastPxDate"=lastPxDate))


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
