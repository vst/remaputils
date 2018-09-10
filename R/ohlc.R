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
    sprintf("Retrieving ohlc observations for: %s", symbol)

    ## Build the parameters:
    params=list("format"="csv",
                "_fields"="id,symbol,date,open,high,low,close",
                "page_size"=-1,
                "series__symbol"=symbol,
                "date__gte"=lte - lookBack,
                "date__lte"=lte)

    ## Get the ohlc observation and return data-frame:
    as.data.frame(getResource("ohlcobservations", params=params, session=session))
}
