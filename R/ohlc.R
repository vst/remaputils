##' This function create a new index price series from returns in the ohlc table.
##'
##' This is a description
##'
##' @param session The rdecaf session.
##' @param sourceKeyword The keyword to look for in the ohlcs denoting returns
##' @param targetReplacement The replacement of the sourceKeyword to be used for the new ohlc series
##' @return NULL
##' @export
createOhlcIndexSeriesFromReturns <- function(session, sourceKeyword="\\|Return", targetReplacement="|PXLAST") {

    ## Get all ohlcs:
    ohlcs <- getResource("ohlcs", params=list("page_size"=-1), session=session)

    ## Get all ohlc symbols:
    symOhlcs <- sapply(ohlcs, function(x) x$symbol)

    ## Get the ohlc's with the Return keyword:
    retOhlcs <- ohlcs[safeGrep(symOhlcs, sourceKeyword) == "1"]

    ## If none, exit:
    length(retOhlcs) > 0 || return(NULL)

    ## Construct the index price series and push the new series using the new symbol:
    retOhlcObs <- lapply(retOhlcs, function(x) {

        ## Get the symbol:
        symbol <- x$symbol

        ##  Get the observations of returns
        ohlcObs <- getOhlcObsForSymbol(session, symbol, lte=Sys.Date(), lookBack=NULL)

        ## If none, exit:
        NROW(ohlcObs) != 0 || return(NULL)

        ## Crdate the indexed series:
        indexed <- rev(100*(1+cumsum(rev(ohlcObs[, "close"]))))

        ## Set OHL to NULL
        ohlcObs[, c("open", "high", "low", "id")] <- NULL

        ## Construct the new symbol:
        ohlcObs[, "symbol"] <- trimws(paste0(unlist(strsplit(symbol, sourceKeyword)), targetReplacement))

        ## Assign indexed to close:
        ohlcObs[, "close"] <- indexed

        ## Push OHLC:
        pushOhlc(ohlcObs[, "symbol"], ohlcObs[, "close"], ohlcObs[, "date"], session)
    })

    ## Done, return:
    return(NULL)
}


##' This function create a new custom ohlc series and observations based on existing ohlc oberservations
##' by stichting them based on the order/rank.
##'
##' This is a description
##'
##' @param resources The resources data frame.
##' @param session The rdecaf session.
##' @param lookBack The lookBak period.
##' @param constituents A list with the existing ohlc symbols and the desired target symbol such as: list("ohlc1"="XYZ|1", "ohlc2"=XYZ|2, target=XYZ|Custom)
##' @return NULL
##' @export
customOhlcRanked <- function(resources, session, lookBack=60, constituents) {

    ## Which resources to consider:
    resources <- resources[!mCondition(resources, c("ctype"), c("CCY", "DEPO", "LOAN", "FXFWD")), ]

    ## Get the target custom ohlc symbols:
    target <- as.character(sapply(constituents, function(x) x$target))

    ## Remove the target from the list:
    constituents <- lapply(constituents, function(x) x[names(x) != "target"])

    ## Iterate over constituents, retrieve ohlc obs and parse:
    ohlcobs <- lapply(constituents, function(x) {

        ## Get the ohlc obs:
        aa <- lapply(x, function(z) getOhlcObsForSymbol(session, symbol=z, lookBack=lookBack, addFields="source"))

        ## Name the list:
        names(aa) <- as.character(unlist(x))

        ## Remove empty lists:
        aa[sapply(aa, NROW)==0] <- NULL

        ## Round the close values:
        aa <- lapply(aa, function(x) {
            x[, "close"] <- round(as.numeric(x[, "close"]), 8)
            return(x)
        })

        ## Done return:
        return(aa)
    })

    ## Name the ohlc obs list:
    names(ohlcobs) <- target

    ## Iterative over ohlc observations and get the combined
    ## xts time series data frame and corresondong source information:
    combinedOhlc <- lapply(ohlcobs, function(x) {

        ## If no data, exit:
        length(x) != 0 || return(NULL)

        ## Combine the closes:
        closes <- list("close"=
                           ## Get the closes as xts:
                           do.call(cbind,
                                   c(lapply(x, function(z) xts::as.xts(z[, "close"], order.by=as.Date(z[, "date"]), source=z[, "source"])),
                                     check.names=FALSE)))

        ## Get the source of the closes:
        ## xts does not accept characters so we use the list indices:
        sources <- do.call(cbind,
                lapply(1:length(x), function(i) {
                    xts::as.xts(rep(i, NROW(x[[i]])), order.by=as.Date(x[[i]][, "date"]))
                }))

        ## Reassign the character values to the xts series:
        for (i in 1:length(x)) {
            sources[!is.na(sources[, i]), i] <- x[[i]][, "source"]
        }

        ## Name the columns of the ohlc source data frame:
        colnames(sources) <- paste0("source", 1:length(x))

        ## Done, return:
        return(c(closes, "source"=list(sources)))

    })

    ohlcobs <- NULL
    gc()

    ## Iterative over combined ohlcs and construct the new series:
    combinedOhlc <- lapply(combinedOhlc, function(x) {

        ## Exit if NULL:
        !is.null(x) || return(NULL)

        ## Get the first non-na close index in data frame (by row):
        clsIdx <- as.numeric(apply(x[["close"]], MARGIN=1, function(z) which(!is.na(z))[1]))

        ## Conscturct the data frame:
        cls <- data.frame(x[["close"]],
                          "x"=apply(x[["close"]], MARGIN=1, function(z) z[which(!is.na(z))[1]]),
                          check.names=FALSE,
                          stringsAsFactors=FALSE)

        ## Determine the original source fo the new custom observation:
        sourcePx <- as.character(sapply(1:NROW(x[["source"]]), function(row) as.character(x[["source"]][row, clsIdx[row]])))

        ## Done, return:
        return(data.frame(cls,
                          sourcePx,
                          check.names=FALSE,
                          stringsAsFactors=FALSE))

    })

    ## Iterate over combinedOHlc and prepare the ohlc push:
    syntheticSeries <- do.call(rbind, lapply(1:length(combinedOhlc), function(i) {

        !is.null(combinedOhlc[[i]]) || return(NULL)

        data.frame("date"=rownames(combinedOhlc[[i]]),
                   "close"=combinedOhlc[[i]][, "x"],
                   "symbol"=names(combinedOhlc)[[i]],
                   "source"=combinedOhlc[[i]][, "sourcePx"])
    }))

    ## Remove ilegal data:
    syntheticSeries <- syntheticSeries[!syntheticSeries[, "close"] == 0, ]
    syntheticSeries <- syntheticSeries[!syntheticSeries[, "date"] == 1, ]

    ## Push:
    pushOhlc("symbol"=syntheticSeries[, "symbol"],
              "close"=syntheticSeries[, "close"],
              "date"=syntheticSeries[, "date"],
              "source"=syntheticSeries[, "source"],
              "session"=session)

    ## Done, exit:
    return(NULL)

}




##' This function retrievs the ohlc observations for a give symbol.
##'
##' This is a description
##'
##' @param symbols A vector with symbols.
##' @param ltes A vector with dates. Less than or equal to this date.
##' @param lookBack The number of days to look back from 'lte'. Default is 30 days.
##' @param excludeWeekends Should the weekends be excluded? Default is TRUE.
##' @param session The rdecaf session.
##' @return A list with result.
##' @export
getOhlcObsWrapper <- function(symbols, ltes, lookBack, excludeWeekends, session) {

    ## Run for each symbol:
    lapply(1:length(symbols), function(i) {

        ## Get the symbol:
        symbol <- symbols[i]

        ## Get the lte date:
        lte <- ltes[i]

        ## If NA lte, return NA:
        if (is.na(lte)) {
            return(NA)
        }

        ## Get the price:
        px <- head(getOhlcObsForSymbol(session, symbol, lte, lookBack=1, FALSE)[, "close"], 1)

        ## If length 0, return NA:
        if (length(px) == 0) {
            return(NA)
        }

        ## Done, return px:
        px

    })
}


##' This function retrievs the ohlc observations for a give symbol.
##'
##' This is a description
##'
##' @param session The session info for the instance to retrieve the ohlc observation from.
##' @param symbol The symbol to for which the ohlc observation is to be retrieved.
##' @param lte Less than or equal to this date. Default is Sys.Date(), meaning today.
##' @param lookBack The number of days to look back from 'lte'. Default is 30 days.
##' @param excludeWeekends Should the weekends be excluded? Default is TRUE.
##' @param addFields Additional fields to be queried.
##' @return A data frame with the symbosl ohlc observation.
##' @import rdecaf
##' @export
getOhlcObsForSymbol <- function(session, symbol, lte=Sys.Date(), lookBack=30, excludeWeekends=TRUE, addFields=NULL) {

    ## Print some stuff:
    print(sprintf("Retrieving ohlc observations for: %s", as.character(symbol)))

    if (!is.null(addFields)) {
        addFields <- paste0(",", paste(addFields, collapse=","))
    }

    ## Append additional query fields:
    fields <- paste0("id,symbol,date,open,high,low,close", addFields)

    ## Safety:
    !isNAorEmpty(symbol) || return(NULL)
    nchar(symbol) > 0 || return(NULL)

    ## Build the parameters:
    params=list("format"="csv",
                "_fields"=fields,
                "page_size"=-1,
                "series__symbol"=symbol,
                "date__gte"=lte - lookBack,
                "date__lte"=lte)

    ## Get the ohlc observation:
    ohlc <- as.data.frame(getResource("ohlcobservations", params=params, session=session))

    ## If no ohlc, return NULL:
    if (NROW(ohlc) == 0) {
        return(ohlc)
    }

    if (excludeWeekends) {
        ## Filter our weekends and return:
        ohlc <- ohlc[weekdays(ohlc[, "date"]) != "Saturday" & weekdays(ohlc[, "date"]) != "Sunday", ]
    }

    ## Done, return
    ohlc

}


##' This function retrieves all OHLC series for stocks in give portfolios.
##'
##' This is a description
##'
##' @param container The container data-frame from rdecaf (either portfolio or account data-frame).
##' @param containerType A string with container type (either "portfolio" or "account").
##' @param asof The date of the stocks.
##' @param resources The resource data-frame from rdecaf.
##' @param session The rdecaf session.
##' @param lookBack The lookback period for the ohlc observations.
##' @param exclType The instrument types to be excluded from stocks. Default is c("CCY", "DEPO", "LOAN")
##' @return A list with the ohlc observations.
##' @export
getOHLCSeriesForStocks <- function(container, containerType, asof, resources, session, lookBack=30, exclType=c("CCY", "DEPO", "LOAN")) {

    ## Get the stocks for portfolios:
    stocks <- getStocks(container=container,
                        session=session,
                        zero=0,
                        date=asof,
                        c=containerType)

    ## Get the enriched stocks:
    stocks <- getEnrichedStocks(stocks, container, resources)

    ## Exclude types from stocks:
    for (excl in exclType) {
        stocks <- stocks[stocks[, "type"] != excl, ]
    }

    ## Append ohlccode to stocks:
    stocks[, "ohlccode"] <- resources[match(stocks[, "symbol"], resources[, "symbol"]), "ohlccode"]

    ## Use ohlccode as symbol if exists:
    symbols <- ifelse(is.na(stocks[, "ohlccode"]), stocks[, "symbol"], stocks[, "ohlccode"])

    ## Get the unique symbols:
    uniqueSymbols <- unique(symbols)

    ## Retrieve the ohlc observations for symbols:
    ohlcs <- lapply(uniqueSymbols, function(sym) getOhlcObsForSymbol(session=session, symbol=sym, lookBack=lookBack))

    ## Append the symbols as names:
    names(ohlcs) <- uniqueSymbols

    ## Done, return:
    ohlcs

}
