##' This function checks whether ohlccodes are dubious.
##'
##' This is a description
##'
##' @param resources A data-frame with the resources from rdecaf.
##' @param jaccardCoeff The threshold for the jaccard string distance coefficient.
##' @return The resource data-frame with dubious ohlccodes.
##' @export
dubiousOHLCcodes <- function(resources, jaccardCoeff) {

    ## Get the resources which have OHLC codes:
    resourceWithOHLCcode <- resources[!isNAorEmpty(resources[, "ohlccode"]), ]

    ## If none has ohlccodes, return NULL:
    if (NROW(resourceWithOHLCcode) == 0) {
        return(NULL)
    }

    ## Which ones fail to grep the ohlccode in symbol:
    grepMethod <- apply(resourceWithOHLCcode, MARGIN=1, function(row) safeGrep(row["symbol"], row["ohlccode"]) == "0")

    ## Which ones have a very dissimilar ohlccode compared to symbol:
    jaccardMethod <- as.numeric(apply(resourceWithOHLCcode, MARGIN=1, function(x) stringdist(as.character(x["symbol"]), as.character(x["ohlccode"]), method="jaccard"))) > jaccardCoeff

    ## Get the dubious ohlc codes:
    dubiousOHLCcodes <- resourceWithOHLCcode[grepMethod | jaccardMethod,]

    ## If none dubious, return NULL:
    if (NROW(dubiousOHLCcodes) == 0) {
        return(NULL)
    }

    ## Done, return:
    dubiousOHLCcodes

}


##' This function checks whether trade dates are dubious.
##'
##' This is a description
##'
##' @param trades A data-frame with the trades from rdecaf.
##' @param backdatedness The number of days after which a trade should have been created.
##' @return The trades data-frame with dubious trade dates.
##' @export
dubiousTradeDates <- function(trades, backdatedness) {

    ## Trades with creation dates over N days of commitment date:
    aPriorCreation <- as.Date(trades[, "created"]) - as.Date(trades[, "commitment"]) > backdatedness

    ## Dates with commitment date later than creation date:
    exAnteCreation <- as.Date(trades[, "created"]) < as.Date(trades[, "commitment"])

    ## Filter and return:
    trades <- trades[aPriorCreation | exAnteCreation, ]

}


##' This function checks the health of the OHLC observations.
##'
##' This is a description
##'
##' @param ohlc A data-frame with the ohlc observations.
##' @param asof The date to compare ohlc observations to.
##' @param lookBack The number of days to look back for the check.
##' @return A data-frame with state of ohlc series.
##' @export
ohlcHealth <- function(ohlc, asof=Sys.Date(), lookBack=5) {

    ohlc[, "close"] <- as.numeric(ohlc[, "close"])

    data.frame("Symbol"=ohlc[1, "symbol"],
               "No PX in N days"=ifelse(NROW(ohlc) == 0, TRUE, all(asof - ohlc[, "date"] > lookBack)),
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
