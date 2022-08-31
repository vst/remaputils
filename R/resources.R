##' A function the country to region mapping
##'
##' This is a description.
##'
##' @return Returns a list with the country to region mapping.
##' @export
countryRegionMap <- function() {
    list(
        "Europe"=c("Switzerland", "Germany", "Denmark", "Sweden",
                   "Norway", "France", "UK", "Britain", "Spain",
                   "Belgium", "Luxembourg", "Netherlands", "Ireland", "Italy"),
        "North America"=c("United States", "Canada", "Bahamas"),
        "Americas"=c("Brazil", "Mexico", "Argentina"),
        "EMEA"=c("Poland", "Hungary", "Turkey", "South Africa", "Israel"),
        "Asia"=c("China", "Japan", "Korea", "Hong Kong", "Thailand",
                 "Malaysia", "Australia")
    )
}


##' A function to provide the future month memnonic mapper:
##'
##' This is a description.
##'
##' @return Returns a matrix with the future month mapping.
##' @export
futureMonthMap <- function() {
    cbind("F"="01",
          "G"="02",
          "H"="03",
          "J"="04",
          "K"="05",
          "M"="06",
          "N"="07",
          "Q"="08",
          "U"="09",
          "V"="10",
          "X"="11",
          "Z"="12")
}


##' A function to provide the future contract size:
##'
##' This is a description.
##'
##' @return Returns a matrix with the future contract size mapping.
##' @export
futureContractSizes <- function() {
    cbind("NG" = 10000,
          "C " = 5000,
          "TY" = 1000,
          "RTA"= 50,
          "RTY"= 50,
          "DX" = 1000,
          "ES" = 50,
          "IK" = 1000,
          "CL" = 1000,
          "NQ" = 20,
          "SI" = 50,
          "HI" = 50,
          "GC" = 100,
          "G " = 1000,
          "HG" = 250,
          "PL" = 50,
          "PT" = 200,
          "KC" = 37500,
          "VG" = 10,
          "GX" = 25,
          "NX" = 5,
          "XBT"= 1,
          "JO" = 15000,
          "IB" = 10,
          "ST" = 5,
          "NO" = 20000,
          "SM" = 10,
          "FV" = 1000,
          "NG" = 10000,
          "RX" = 100,
          "DFW"= 5,
          "JY" = 12500,
          "OA" = 100000,
          "OAT"= 100,
          "TY" = 1000,
          "PL" = 50,
          "GC" = 100,
          "DED"= 100,
          "EC" = 125000,
          "SF" = 125000,
          "BR" = 100000,
          "BP" = 625,
          "RF" = 125000,
          "RP" = 125000,
          "RY" = 125000,
          "ER" = 2500,
          "PE" = 500000,
          "CA" = 50,
          "CD" = 1000,
          "S " = 10,
          "US" = 1000,
          "AD" = 100000,
          "NV" = 100000,
          "Z " = 10,
          "CF" = 10)
}


##' A function to infer the rounded factor between two values.
##'
##' This is a description.
##'
##' @param sourceValue The value to compute the factor for.
##' @param targetValue The value to compute the factor from.
##' @return Returns a factor.
##' @export
quantityFinder <- function(sourceValue, targetValue) {

    ## Compute the actual factor:
    factor <- sourceValue / targetValue

    ## Is the factor 0.01:
    factor <- ifelse(factor < 0.00013 & factor > 0.00007, 0.0001, factor)

    ## Is the factor 0.01:
    factor <- ifelse(factor < 0.0013 & factor > 0.0007, 0.001, factor)

    ## Is the factor 0.01:
    factor <- ifelse(factor < 0.0130 & factor > 0.0070, 0.01, factor)

    ## Is the factor 0.10:
    factor <- ifelse(factor < 0.1300 & factor > 0.0700, 0.10, factor)

    ## Is the factor 1:
    factor <- ifelse(factor < 1.3000 & factor > 0.7000, 1.00, factor)

    ## Is the factor 10:
    factor <- ifelse(factor < 13.000 & factor > 7.0000, 10.0, factor)

    ## Is the factor 100:
    factor <- ifelse(factor < 130.00 & factor > 70.000, 100 , factor)

    ## Is the factor 100:
    factor <- ifelse(factor < 1300.0 & factor > 700.00, 1000, factor)

    ## Is the factor 100:
    factor <- ifelse(factor < 13000 & factor > 7000.00, 10000, factor)

    ## Replace Inf values with 1:
    factor[which(abs(factor) == Inf)] <- 1

    ## Replace NA values with 1:
    factor[which(is.na(factor))] <- 1

    ## Done, return:
    factor
}


##' A function to infer expiry of future bloomberg tickers:
##'
##' When the bloomberg ticker of a future is provided,
##' we can infer the date of the expiry.
##'
##' @param bbgticker The bloomberg ticker.
##' @param weekday The day of the week convention.
##' @param nthWeekday The nth weekday's date to be used. If weekday is 'Friday' and nthWeekday is '3', it returns the 3rd Friday of the month.
##' @return Returns a date object.
##' @export
getFutureExpiry <- function(bbgticker, weekday="Friday", nthWeekday=3){

    ## If empty or NA, return NA:
    if (isNAorEmpty(bbgticker) | bbgticker == "NA") {
        return(NA)
    }

    ## Infere the location of the year number in ticker for single line:
    if (NROW(bbgticker) == 1){

        ## Ticker for active instruments follow the convention of single digit year, therefore
        ## the year is indicated as an integer from 1 to 9 in ticker.
        pos  <- unlist(sapply(0:9, function(n) gregexpr(as.character(n), bbgticker)))

        ## Identify the location of the matching integer:
        nLoc <- pos[which(pos != -1)]
    }

    ## Infere the location of the year number in ticker for multiple lines:
    if (NROW(bbgticker) > 1) {

        ## Ticker for active instruments follow the convention of single digit year, therefore
        ## the year is indicated as an integer from 1 to 9 in ticker.
        nLoc <- sapply(0:9, function(n) unlist(lapply(gregexpr(as.character(n), bbgticker), function(x) x[1])))

        ## Set unmatched characters to Inf:
        nLoc[nLoc == -1] <- Inf

        ## TODO:
        ow <- t(apply(nLoc, MARGIN=1, function(row) !row - min(row) == 0))
        nLoc[ow] <- Inf
        nLoc <- apply(nLoc, MARGIN=1, function(x) x[which(x !=Inf)][[1]])
    }

    ## Map the month. The month memnonic in ticker is indicated with a single letter
    ## right before the year integer, therefore the position is nLoc - 1.
    month <- futureMonthMap()[match(substr(bbgticker, nLoc-1, nLoc-1), colnames(futureMonthMap()))]

    ## Compute the year. The convention of single digit year indication requires an
    ## inference of the start of the current decade.
    decade <- as.numeric(paste0(substr(Sys.Date(), 1, 3), "0"))

    ## Get the tickers year in decade:
    tickerYear <- as.numeric(substr(bbgticker, nLoc, nLoc))

    ## Add a decade of the ticker's decade year is smaller than current decades year:
    if (tickerYear < as.numeric(substr(Sys.Date(), 4, 4))) {
        decade <- decade + 10
    }

    ## Compute the year in full digits:
    year <- decade + as.numeric(substr(bbgticker, nLoc, nLoc))

    ## Construct the year month %Y%m:
    yearMonth <- paste0(year, month)

    ## Generate 1 to 31 day sequence:
    seqs <- seq(1, 31, 1)

    ## Make day sequence 01, 02, instead of 1, 2:
    days <- paste0(ifelse(nchar(seqs) == 1, "0", ""), seqs)

    ## Construct the dates:
    datesInMonth <- as.Date(paste0(yearMonth, days), format="%Y%m%d")

    ## Get the week day as name:
    wDays <- weekdays(datesInMonth)

    ## Get the 3rd Friday and return date:
    datesInMonth[which(wDays == weekday)[nthWeekday]]
}


##' A function to create share resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createZCPNResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "id"=NA,
                      "isin"=safeColumn(df ,"isin"),
                      "ctype"="ZCPN",
                      "launch"=safeColumn(df, "launch"),
                      "name"=df[,"name"],
                      "ccymain"=df[,"ccymain"],
                      "ticker"=safeColumn(df, "ticker"),
                      "reference"=safeColumn(df, "reference"),
                      "expiry"=df[,"maturity"],
                      "quantity"=safeColumn(df, "pxfactor"),
                      "frequency"=df[,"frequency"],
                      "convday"=df[,"convday"])

    ## Create the payload:
    payload <- toJSON(apply(dfx, MARGIN=1, as.list), auto_unbox=TRUE, digits=10)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to create share resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createShareResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "id"=NA,
                      "ctype"="SHRE",
                      "quantity"=safeColumn(df, "pxfactor"),
                      "isin"=safeColumn(df, "isin"),
                      "ticker"=safeColumn(df, "ticker"),
                      "reference"=safeColumn(df, "reference"),
                      "figi"=safeColumn(df, "figi"),
                      "stype"=safeColumn(df, "stype"),
                      "name"=df[,"name"],
                      "ccymain"=df[,"ccymain"])

    ## Create the payload:
    payload <- toJSON(apply(dfx, MARGIN=1, as.list), auto_unbox=TRUE)
    if(NROW(df)==1) {
      payload <- toJSON(dfx, auto_unbox=TRUE)
    }

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to create cash resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createCashResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "id"=NA,
                      "ctype"="CCY")

    ## Create the payload:
    payload <- toJSON(apply(dfx, MARGIN=1, as.list), auto_unbox=TRUE)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to create loan/deposit resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createLoanDepoResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=safeColumn(df, "symbol"),
                      "id"=NA,
                      "ctype"=df[,"ctype"],
                      "pxmain"=df[,"pxmain"],
                      "launch"=df[,"launch"],
                      "reference"=safeColumn(df, "reference"),
                      "expiry"=df[,"expiry"],
                      "convday"=df[,"convday"],
                      "ccymain"=df[,"ccymain"])

    ## Create the payload:
    payload <- toJSON(apply(dfx, MARGIN=1, as.list), auto_unbox=TRUE, digits=10)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to create other resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createOtherResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "id"=NA,
                      "ctype"="OTHER",
                      "isin"=safeColumn(df, "isin"),
                      "ticker"=safeColumn(df, "ticker"),
                      "reference"=safeColumn(df, "reference"),
                      "name"=df[,"name"],
                      "quantity"=safeColumn(df, "pxfactor"),
                      "ccymain"=df[,"ccymain"])

    ## Create the payload:
    payload <- toJSON(apply(dfx, MARGIN=1, as.list), auto_unbox=TRUE)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to create fx forward resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createFXFwdResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("ccymain"=df[,"ccymain"],
                      "symbol"=safeTry(try(df[, "symbol"], silent=TRUE)),
                      "launch"=safeTry(try(df[, "launch"], silent=TRUE)),
                      "ccyaltn"=df[,"ccyaltn"],
                      "expiry"=df[,"settlement"],
                      "id"=NA,
                      "guid"=safeColumn(df, "guid"),
                      "reference"=safeColumn(df, "reference"),
                      "ctype"="FXFWD",
                      "pxflip"=as.character(df[,"isFlip"]),
                      "pxmain"=df[,"pxmain"])

    ## Create the payload:
    payload <- toJSON(dfx, auto_unbox=TRUE, digits=10)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to create fx forward resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createFXFutureResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[, "symbol"],
                      "ccymain"=df[,"ccymain"],
                      "ccyaltn"=df[,"ccyaltn"],
                      "expiry"=safeColumn(df,"expiry"),
                      "ohlccode"=safeColumn(df ,"ohlccode"),
                      "ticker"=safeColumn(df, "ticker"),
                      "reference"=safeColumn(df, "reference"),
                      "id"=NA,
                      "ctype"="FXFUT",
                      "stype"=safeColumn(df, "stype"),
                      "quantity"=safeColumn(df, "contractsize"),
                      "pxflip"=as.character(df[,"isFlip"]),
                      stringsAsFactors=FALSE)

    ## Create the payload:
    payload <- toJSON(apply(dfx, MARGIN=1, as.list), auto_unbox=TRUE, digits=10)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to create structured product resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createSpResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "id"=NA,
                      "ctype"="SP",
                      "isin"=safeColumn(df, "isin"),
                      "quantity"=safeColumn(df, "pxfactor"),
                      "reference"=safeColumn(df, "reference"),
                      "expiry"=safeColumn(df,"expiry"),
                      "name"=df[,"name"],
                      "ccymain"=df[,"ccymain"])

    ## Create the payload:
    payload <- toJSON(apply(dfx, MARGIN=1, as.list), auto_unbox=TRUE)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to infer the product symbol from ticker:
##'
##' This is a description.
##'
##' @param ticker The bloomberg ticker.
##' @return Returns the inferred product code of a bloomber ticker.
##' @export
productCodeFromTicker <- function(ticker){

    ## Get the
    firstElement <- lapply(strsplit(as.character(ticker), " "), function(x) x[1])
    strLength <- lapply(firstElement, function(x) switch(as.character(nchar(x)), "5"=3, "4"=2, "1"=2))

    productCode <- do.call(c, lapply(1:length(strLength), function(i) {

        if (is.null(strLength[[i]])) {
            return(NA)
        }

        substr(ticker[i], 1, as.numeric(strLength[[i]]))
    }))

    ## Done, return:
    productCode
}


##' A function to create future resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createFutureResource <- function(df, session){

    ## Check if expiry column exists:
    expiry <- safeColumn(df, "expiry")

    ## If expiry doesn't exist, try infering from bbgcode:
    if (any(is.na(expiry))) {
        ## Get the expiry of the future:
        expiry <- safeTry(try(do.call(c, lapply(df[,"bbgcode"], getFutureExpiry)), silent=TRUE))
    }

    ## Check if contract size exists:
    contractsize <- safeColumn(df, "contractsize")

    ## For NA contract sizes, infer:
    if (any(is.na(contractsize))) {

        ## NA contract sizes:
        naContractSize <- which(is.na(contractsize))

        ## Get the product code:
        productCode <- sapply(df[naContractSize,"symbol"], function(s) productCodeFromTicker(s))

        ## Get the contract size of the future:
        df[naContractSize,"contractsize"] <- futureContractSizes()[match(productCode, colnames(futureContractSizes()))]
    }

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "ohlccode"=safeColumn(df ,"ohlccode"),
                      "id"=NA,
                      "type"="Future Contract",
                      "ctype"="FUT",
                      "stype"=safeColumn(df, "stype"),
                      "figi"=trimws(safeColumn(df, "figi")),
                      "ticker"=safeColumn(df, "ticker"),
                      "reference"=safeColumn(df, "reference"),
                      "isin"=trimws(safeColumn(df, "isin")),
                      "name"=df[,"name"],
                      "ccymain"=df[,"ccymain"],
                      "expiry"=expiry,
                      "quantity"=df[,"contractsize"])

    ## Create the payload:
    payload <- toJSON(dfx, auto_unbox=TRUE, na="null", digits=10)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to create CFD resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createCFDResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "ohlccode"=safeColumn(df, "ohlccode"),
                      "id"=NA,
                      "ctype"="CFD",
                      "reference"=safeColumn(df, "reference"),
                      "stype"=safeColumn(df, "stype"),
                      "name"=df[,"name"],
                      "ccymain"=df[,"ccymain"],
                      "quantity"=safeTry(try(df[, "contractsize"], silent=TRUE)))

    ## Create the payload:
    payload <- toJSON(dfx, auto_unbox=TRUE, na="null", digits=10)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}



##' A function to create commodity resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createCommodityResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "ohlccode"=safeColumn(df, "ohlccode"),
                      "id"=NA,
                      "ctype"="COMM",
                      "ticker"=safeColumn(df, "ticker"),
                      "reference"=safeColumn(df, "reference"),
                      "name"=df[,"name"],
                      "ccymain"=df[,"ccymain"])

    ## Create the payload:
    payload <- toJSON(dfx, auto_unbox=TRUE, na="null")

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}



##' A function to create bond resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createBondResource <- function(df, session) {

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "id"=NA,
                      "isin"=safeColumn(df ,"isin"),
                      "ctype"="BOND",
                      "stype"=safeColumn(df, "stype"),
                      "launch"=safeColumn(df, "launch"),
                      "name"=df[,"name"],
                      "pxmain"=df[,"cpn"],
                      "assetclass"=safeColumn(df, "assetclass"),
                      "ccymain"=df[,"ccymain"],
                      "ticker"=safeColumn(df, "ticker"),
                      "eom"=safeColumn(df, "eom"),
                      "reference"=safeColumn(df, "reference"),
                      "expiry"=df[,"maturity"],
                      "quantity"=safeColumn(df, "pxfactor"),
                      "frequency"=safeColumn(df,"frequency"),
                      "convday"=safeColumn(df, "convday"))

    ## Create the payload:
    payload <- toJSON(apply(dfx, MARGIN=1, as.list), auto_unbox=TRUE, digits=10)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to create option resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createOptionResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "id"=NA,
                      "ohlccode"=safeColumn(df, "bbgcode"),
                      "ctype"="OPT",
                      "name"=df[,"name"],
                      "callput"=df[,"callput"],
                      "isin"=safeColumn(df, "isin"),
                      "ticker"=safeColumn(df, "ticker"),
                      "reference"=safeColumn(df, "reference"),
                      "pxmain"=as.character(df[,"strike"]),
                      "underlying"=safeColumn(df, "underlying"),
                      "ccymain"=df[,"ccymain"],
                      "expiry"=df[,"expiry"],
                      "quantity"=as.character(df[,"contractsize"]))

    ## Create the payload:
    payload <- toJSON(dfx, auto_unbox=TRUE, na="null", digits=10)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to create fx option resources:
##'
##' This is a description.
##'
##' @param df The data-frame with the resource information
##' @param session The rdecaf session.
##' @return Returns the http post result.
##' @import rdecaf
##' @import jsonlite
##' @export
createFXOptionResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "id"=NA,
                      "ohlccode"=safeColumn(df, "bbgcode"),
                      "ctype"="FXOPT",
                      "ccymain"=df[,"ccymain"],
                      "ccyaltn"=df[,"ccyaltn"],
                      "pxflip"=df[,"pxflip"],
                      "ticker"=safeColumn(df, "ticker"),
                      "reference"=safeColumn(df, "reference"),
                      "name"=df[,"name"],
                      "callput"=df[,"callput"],
                      "pxmain"=as.character(df[,"strike"]),
                      "expiry"=df[,"expiry"],
                      "quantity"=as.character(df[,"contractsize"]))

    ## Create the payload:
    payload <- toJSON(dfx, auto_unbox=TRUE, na="null", digits=10)

    ## Post the resource:
    postResource("resources", "imports", payload=payload, session=session)
}


##' A function to infer the maturity of bond from bloomberg ticker:
##'
##' This is a description.
##'
##' @param ticker The bloomberg ticker.
##' @return Returns the inferred maturity
##' @export
bondMaturityFromTicker <- function(ticker){

    ## Split the ticker:
    splitTicker <- lapply(ticker, function(n) strsplit(as.character(n), " ")[[1]])

    ## Get the maturities:
    maturities <- lapply(1:length(splitTicker), function(i) {

        ## If perpetual, return afar date:
        isPerp <- grep("PERP", splitTicker[[i]])
        if (length(isPerp) > 0){
            return("01/01/49")
        }

        ## Get and return the date string:
        splitTicker[[i]][grep("\\/", splitTicker[[i]])]
    })

    ## hebele
    do.call(c, lapply(maturities, function(x) ifelse(length(x) == 0, NA, as.character(as.Date(x, format="%m/%d/%y")))))
}


##' A function to retrieve the resources by stock for a rdecaf session:
##'
##' This is a description.
##'
##' @param stocks The stocks data-frame
##' @param session The rdecaf session
##' @param getUnderlying TODO:
##' @return Returns the inferred maturity
##' @import rdecaf
##' @export
getResourcesByStock <- function(stocks, session, getUnderlying=TRUE){

    ## Get the unique stocks:
    uniqueArtifacts <- unique(stocks[, "artifact"])

    ## Create batches:
    batches <- createBatches(length(uniqueArtifacts), 1000)

    ## Initialize resources:
    resources <- NULL

    ## Iterate over batches and get the resources:
    for (i in 1:length(batches[[1]])) {

        cArtifacts <- uniqueArtifacts[batches$startingIdx[[i]]:batches$endingIdx[[i]]]

        ## Construct the resource params:
        params <- list(page_size = -1, id__in = paste(cArtifacts, collapse = ","))

        ## Get vision resource. NOTE: We are binding them safely ourselfs.
        resources <- c(resources, getResource("resources", params = params, session = session))

    }

    ## Get the external data:
    extData1Pass <- lapply(resources,function(x) x[["extdata"]])

    ## Get the tags:
    tags <- lapply(resources,function(x) x[["tags"]])

    ## Excluding tag information, safely combine and return:
    resources <- safeRbind(resources)

    ## Reassign the extdata as list:
    resources$extdata <- extData1Pass

    ## Reassign the extdata as list:
    resources$tags <- tags

    ## Indicate if it is a underlying:
    resources[, "is_underlying"] <- FALSE

    ## Get the underlying resources:
    resources[, "underlying"] <- safeColumn(resources, "underlying")

    if (!getUnderlying) {
        return(resources)
    }

    ## Retrieve the underlying resources, if any:
    if (any(!is.na(resources[, "underlying"]))) {

        ## Which resources have underlyings:
        undrl <- !is.na(resources[, "underlying"])

        ## Construct the underlying resource params:
        params <- list("page_size"=-1,
                       "id__in"=paste(resources[undrl, "underlying"], collapse=","))

        ## Get and bind the undelrying resources:
        undrlResources <- getResource("resources", params=params, session=session)

        ## Get the external data as list:
        extData2Pass <- lapply(undrlResources,function(x) x[["extdata"]])

        tags2 <- lapply(undrlResources,function(x) x[["tags"]])

        ## Remove the tags:
        ## undrlResources <- safeRbind(lapply(undrlResources, function(res) do.call(cbind, res[!names(res) == "tags"])))

        undrlResources <- safeRbind(undrlResources)

        ## Reassign the extdata as list:
        undrlResources$extdata <- extData2Pass

        ## Reassign the extdata as list:
        undrlResources$tags <- tags2

        ## These are underlying resources:
        undrlResources[, "is_underlying"] <- TRUE

        ## Bind to the main resources:
        resources <- safeRbind(list(resources, undrlResources))

    }

    ## Done, return:
    resources
}


##' A function to retrieve the stocks for set of accounts:
##'
##' This is a description.
##'
##' @param container The container data-frame from rdecaf
##' @param session The rdecaf session
##' @param zero Either 1 or 0 to indicate whether closed positions should be included.
##' @param date The as of date for the stocks. Default is today.
##' @param c The type. Either "account" or "portfolio".
##' @return Returns a data-frame with the stocks for the accounts.
##' @import rdecaf
##' @export
getStocks <- function(container, session, zero=1, date=Sys.Date(), c="account") {

    ## If not a data-frame, assume container is the id:
    if (NCOL(container) == 1) {
        container <- data.frame("id"=container,
                                "name"=NA,
                                stringsAsFactors=FALSE)
    }

    ## Constrct the stocks params:
    params <- list("page_size"=-1,
                   "c"=c,
                   "format"="csv",
                   "date"=date,
                   "zero"=zero)

    ## Iterate over accounts and append account id to params:
    for (row in 1:NROW(container)) {
        params <- c(params, container[row,"id"])
    }

    ## Append the names to stocks params:
    names(params) <- c("page_size", "c", "format", "date", "zero", rep("i", NROW(container)))

    ## Return stocks:
    as.data.frame(getResource("stocks", params=params, session=session))

}


##' A function to get stocks from container type and names.
##'
##' This is a description.
##'
##' @param session The rdecaf session:
##' @param containerType The type of container, either 'portfolios' or 'accounts'.
##' @param containerNames The names of the container.
##' @param zero If 1, closed and open stocks are considered. If 0, only open stocks.
##' @param date The date of the stocks.
##' @return Returns the data frame with the stocks.
##' @import rdecaf
##' @export
getStocksFromContainerNames <- function(session, containerType, containerNames, zero=1, date) {

    if (containerType == "portfolios") {

        ## Construct the portfolio params:
        params <- list("page_size"=-1,
                       "format"="csv",
                       "name__in"=paste(containerNames, collapse=","))

        ## Get the portfolios:
        container <- as.data.frame(getResource(containerType, params=params, session=session))

    }

    if (containerType == "accounts") {

        ## Construct the account params:
        params <- list("page_size"=-1,
                       "name__in"=paste(containerNames, collapse=","))

        ## Get the portfolios:
        container <- do.call(rbind, getResource(containerType, params=params, session=session))

    }

    ## Get the stocks and return:
    getStocks(container, session, zero=zero, date=date, c=substr(containerType, 1, nchar(containerType) -1))

}


##' A function to enrich the stocks data-frame
##'
##' This is a description.
##'
##' @param stocks The stocks data-frame.
##' @param accounts The accounts data-frame.
##' @param resources The resources data-frame.
##' @return Returns a enriched stocks data-frame.
##' @export
getEnrichedStocks <- function(stocks, accounts, resources){

    ## Append account name:
    stocks[,"account_name"] <- accounts[match(stocks[,"account"], accounts[,"id"]), "name"]

    ## Append the sub type:
    stocks[,"name"] <- resources[match(stocks[,"artifact"], resources[,"id"]), "name"]

    ## Append symbol:
    stocks[,"symbol"] <- resources[match(stocks[,"artifact"], resources[,"id"]), "symbol"]

    ## Append the sub type:
    stocks[,"currency"] <- resources[match(stocks[,"artifact"], resources[,"id"]), "ccymain"]

    ## Append ctype:
    stocks[,"type"] <- resources[match(stocks[,"artifact"], resources[,"id"]), "ctype"]

    ## Append the sub type:
    stocks[,"subtype"] <- resources[match(stocks[,"artifact"], resources[,"id"]), "stype"]

    ## Append the sub type:
    stocks[,"expiry"] <- resources[match(stocks[,"artifact"], resources[,"id"]), "expiry"]

    ## Append the sub type:
    stocks[,"country"] <- resources[match(stocks[,"artifact"], resources[,"id"]), "country"]

    ## Append the sub type:
    stocks[,"isin"] <- resources[match(stocks[,"artifact"], resources[,"id"]), "isin"]

    ## Return:
    stocks

}

##' A function to match instrument in a data frame with resources in the system.
##'
##' This is a description.
##'
##' @param data A data-frame with 'isin' and 'ccymain' columns
##' @param resources The resources data-frame from rdecaf
##' @return The data-frame with the resmain column
##' @export
isinCCYMatch <- function(data, resources) {

    ## Exclude the currency instruments from resources:
    resources <- resources[isIsin(resources[, "isin"]), ]

    ## If no resources left, return:
    if (NROW(resources) == 0) {
        data[, "resmain"] <- NA
        data[, "symbol"] <- NA
        return(data)
    }

    ## ISIN and CCY composite in data:
    compositeD <- paste0(data[, "isin"], data[, "ccymain"])

    ## ISIN and CCY composite in resources:
    compositeR <- paste0(resources[, "isin"], resources[, "ccymain"])

    ## Append first pass of resmain matches:
    data <- data.frame(data, "resmain"=resources[match(compositeD, compositeR), "id"])

    ## Get the na resmains:
    naResmains <- is.na(data[, "resmain"])

    ## ISIN of na resmains in data:
    naResD <- as.character(data[naResmains, "isin"])

    ## Match isins with no ccy:
    data[naResmains, "resmain"] <- resources[match(naResD, resources[, "isin"]), "id"]

    ## Get the symbol:
    data[, "symbol"] <- resources[match(data[, "resmain"], resources[, "id"]), "symbol"]

    ## Done, return:
    data

}


##' A function to match instrument in a data frame with resources in the system.
##'
##' This is a description.
##'
##' @param data A data-frame with the resource information.
##' @param session The rdecaf session.
##' @return The data-frame with the resmain column
##' @export
createResourcesWrapper <- function(data, session) {

    ## Get the na resmains:
    naResmain <- is.na(data[, "resmain"])

    ## Any SHRE to be created?
    createShare <- naResmain & data[, "ctype"] == "SHRE"

    ## Create SHRE if needed:
    if (any(createShare)) {

        ## Create SHRE and get response:
        data[createShare, "pxfactor"] <- data[createShare, "quantity"]

        shareResourceResponse <- createShareResource(data[createShare,], session)

        ## Assign the newly created resmain:
        data[createShare,"resmain"] <- as.character(sapply(shareResourceResponse, function(e) e$id))
    }

    ## Any BOND to be created?
    createBond <- naResmain & data[, "ctype"] == "BOND"

    ## Create BOND if needed:
    if (any(createBond)) {

        ## Get the cpn:
        data[createBond, "cpn"] <- data[createBond, "pxmain"]

        ## Get the maturity:
        data[createBond, "maturity"] <- data[createBond, "expiry"]

        ## Get the pxfactor:
        data[createBond, "pxfactor"] <- data[createBond, "quantity"]

        ## Create BOND and get response:
        bondResourceResponse <- createBondResource(data[createBond,], session)

        ## Assign the newly created resmain:
        data[createBond,"resmain"] <- as.character(sapply(bondResourceResponse, function(e) e$id))
    }

    ## Any BOND to be created?
    createZCPN <- naResmain & data[, "ctype"] == "ZCPN"

    ## Create BOND if needed:
    if (any(createZCPN)) {

        ## Get the maturity:
        data[createZCPN, "maturity"] <- data[createZCPN, "expiry"]

        ## Get the pxfactor:
        data[createZCPN, "pxfactor"] <- data[createZCPN, "quantity"]

        ## Create BOND and get response:
        zcpnResourceResponse <- createZCPNResource(data[createZCPN,], session)

        ## Assign the newly created resmain:
        data[createZCPN,"resmain"] <- as.character(sapply(zcpnResourceResponse, function(e) e$id))
    }

    ## Any SP to be created?
    createSP <- naResmain & data[, "ctype"] == "SP"

    ## Create SP if needed:
    if (any(createSP)) {

        ## Set the pxfactor:
        data[createSP, "pxfactor"] <- data[createSP, "quantity"]

        ## Create SP and get response:
        spResourceResponse <- createSpResource(data[createSP,], session)

        ## Assign the newly created resmain:
        data[createSP,"resmain"] <- as.character(sapply(spResourceResponse, function(e) e$id))
    }

    ## Any FUT to be created?
    createFut <- naResmain & data[, "ctype"] == "FUT"

    ## Create FUT if needed:
    if (any(createFut)) {

        ## Get the contract size:
        contractsize <- records[match(data[, "isin"], newResources[, "ISIN"]), "CONTRACTSIZE"]

        ## Set the contract size:
        data[createFut, "contractsize"] <- contractsize[createFut]

        ## Create the Fut and get response:
        futResourceResponse <- createFutureResource(data[createFut,], session)

        ## Assign the newly create resmain:
        data[createFut,"resmain"] <- as.character(sapply(futResourceResponse, function(e) e$id))
    }

    ## Any OPT to be created?
    createOpt <- naResmain & data[, "ctype"] == "OPT"

    ## Create OPT if needed:
    if (any(createOpt)) {

        ## Get the contract size:
        contractsize <- records[match(data[, "isin"], newResources[, "ISIN"]), "CONTRACTSIZE"]

        ## Set the contract size:
        data[createOpt, "contractsize"] <- contractsize[createOpt]

        ## Set the strike:
        data[createOpt, "strike"] <- data[createOpt, "pxmain"]

        ## Create the OPT and get response:
        optResourceResponse <- createOptionResource(data[createOpt,], session)

        ## Assign the newly created resmain:
        data[createOpt,"resmain"] <- as.character(sapply(optResourceResponse, function(e) e$id))
    }

    ## Any COMM to be created?
    createComm <- naResmain & data[, "ctype"] == "COMM"

    ## Create COMM if needed:
    if (any(createComm)) {

        ## Create COMM and get response:
        commResourceResponse <- createCommodityResource(data[createComm,], session)

        ## Assign the newly created resmain:
        data[createComm,"resmain"] <- as.character(sapply(commResourceResponse, function(e) e$id))
    }

    ## Any OTHER to be created?
    createOther <- naResmain & data[, "ctype"] == "OTHER"

    ## Create OTHER if needed:
    if (any(createOther)) {

        data[createOther, "pxfactor"] <- data[createOther, "quantity"]

        ## Create OTHER and get response:
        otherResourceResponse <- createOtherResource(data[createOther,], session)

        ## Assign the newly created resmain:
        data[createOther,"resmain"] <- as.character(sapply(otherResourceResponse, function(e) e$id))
    }

    ## Any OTHER to be created?
    createLoan <- naResmain & data[, "ctype"] == "LOAN"

    if (any(createLoan)) {

        ## Create OTHER and get response:
        loanResourceResponse <- createOtherResource(data[createLoan,], session)

        ## Assign the newly created resmain:
        data[createLoan,"resmain"] <- as.character(sapply(loanResourceResponse, function(e) e$id))

    }

    ## Any OTHER to be created?
    createDepo <- naResmain & data[, "ctype"] == "DEPO"

    if (any(createDepo)) {

        ## Create OTHER and get response:
        depoResourceResponse <- createLoanDepoResource(data[createDepo,], session)

        ## Assign the newly created resmain:
        data[createDepo,"resmain"] <- as.character(sapply(depoResourceResponse, function(e) e$id))

    }


    ## Done, return resources:
    data
}
