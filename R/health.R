##' This function checks the health of the OHLC observations.
##'
##' This is a description
##'
##' @param ohlc A data-frame with the ohlc observations.
##' @param asof The date to compare ohlc observations to.
##' @return A data-frame with state of ohlc series.
##' @export
ohlcHealth <- function(ohlc, asof=Sys.Date()) {
    data.frame("Symbol"=ohlc[1, "symbol"],
               "No PX in 5 days"=ifelse(NROW(ohlc) == 0, TRUE, all(asof - ohlc[, "date"] > 5)),
               "No PX at all"=NROW(ohlc[[1]]) == 0,
               "No PX change"=ifelse(NROW(ohlc) == 0, TRUE, all(diff(ohlc[, "close"]) == 0)),
               check.names=FALSE,
               stringsAsFactors=FALSE)
}


##' This function checks whether symbols are dubious w.r.t Bloomberg symbology.
##'
##' This is a description
##'
##' @param symbol A vector with symbols
##' @return A vector of length symbols with TRUE or FALSE.
##' @export
dubiousSymbolBBG <- function(symbol) {

    ## Split each symbol by space.
    splitSymbol <- strsplit(symbol, " ")

    ## Check each split symbol for inconsistency and return:
    sapply(splitSymbol, function(x) length(x) == 1 | all(is.na(match(tail(x), c("Equity", "Corp", "Curncy", "Index", "Comdty")))))
}
