##' This function retrievs the ohlc observations for a give symbol.
##'
##' This is a description
##'
##' @param session The session info for the instance to retrieve the ohlc observation from.
##' @param symbol The symbol to for which the ohlc observation is to be retrieved.
##' @param lte Less than or equal to this date. Default is Sys.Date(), meaning today.
##' @param lookBack The number of days to look back from 'lte'. Default is 30 days.
##' @return A data frame with the symbosl ohlc observation.
##' @import rdecaf
##' @export
getOhlcObsForSymbol <- function(session, symbol, lte=Sys.Date(), lookBack=30) {

    ## Print some stuff:
    print(sprintf("Retrieving ohlc observations for: %s", as.character(symbol)))

    ## Build the parameters:
    params=list("format"="csv",
                "_fields"="id,symbol,date,open,high,low,close",
                "page_size"=-1,
                "series__symbol"=symbol,
                "date__gte"=lte - lookBack,
                "date__lte"=lte)

    ## Get the ohlc observation:
    ohlc <- as.data.frame(getResource("ohlcobservations", params=params, session=session))

    ## If no ohlc, return NULL:
    if (NROW(ohlc) == 0) {
        return(NULL)
    }

    ## Filter our weekends and return:
    ohlc[weekdays(ohlc[, "date"]) != "Saturday" & weekdays(ohlc[, "date"]) != "Sunday", ]

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
