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
    factor <- ifelse(factor < 0.011 & factor > 0.009, 0.01, factor)

    ## Is the factor 0.10:
    factor <- ifelse(factor < 0.110 & factor > 0.090, 0.10, factor)

    ## Is the factor 1:
    factor <- ifelse(factor < 1.100 & factor > 0.900, 1.00, factor)

    ## Is the factor 10:
    factor <- ifelse(factor < 11.00 & factor > 9.000, 10.0, factor)

    ## Is the factor 100:
    factor <- ifelse(factor < 110.0 & factor > 90.00, 100 , factor)

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

    ## Infere the location of the year number in ticker for single line:
    if (NROW(bbgticker) == 1){

        ## Ticker for active instruments follow the convention of single digit year, therefore
        ## the year is indicated as an integer from 1 to 9 in ticker.
        pos  <- unlist(sapply(seq(1, 9, 1), function(n) gregexpr(as.character(n), bbgticker)))

        ## Identify the location of the matching integer:
        nLoc <- pos[which(pos != -1)]
    }

    ## Infere the location of the year number in ticker for multiple lines:
    if (NROW(bbgticker) > 1) {

        ## Ticker for active instruments follow the convention of single digit year, therefore
        ## the year is indicated as an integer from 1 to 9 in ticker.
        nLoc <- sapply(seq(1, 9, 1), function(n) unlist(lapply(gregexpr(as.character(n), bbgticker), function(x) x[1])))

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
createShareResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "id"=NA,
                      "ctype"="SHRE",
                      "isin"=safeColumn(df, "isin"),
                      "figi"=safeColumn(df, "figi"),
                      "stype"=safeColumn(df, "stype"),
                      "name"=df[,"name"],
                      "ccymain"=df[,"ccymain"])

    ## Create the payload:
    payload <- toJSON(apply(dfx, MARGIN=1, as.list), auto_unbox=TRUE)

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
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "id"=NA,
                      "ctype"=df[,"ctype"],
                      "pxmain"=df[,"rate"],
                      "launch"=df[,"commitment"],
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
                      "name"=df[,"name"],
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
                      "ccyaltn"=df[,"ccyaltn"],
                      "expiry"=df[,"settlement"],
                      "id"=NA,
                      "ctype"="FXFWD",
                      "pxflip"=as.character(df[,"isFlip"]),
                      "pxmain"=df[,"pxmain"])

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
createBondResource <- function(df, session){

    ## Create the data frame:
    dfx <- data.frame("symbol"=df[,"symbol"],
                      "id"=NA,
                      "isin"=safeColumn(df ,"isin"),
                      "ctype"="BOND",
                      "name"=df[,"name"],
                      "pxmain"=df[,"cpn"],
                      "ccymain"=df[,"ccymain"],
                      "expiry"=df[,"maturity"],
                      "quantity"=df[,"pxfactor"],
                      "frequency"=df[,"frequency"],
                      "convday"=df[,"convday"])

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
    maturities <- lapply(splitTicker, function(x) {

        ## If perpetual, return afar date:
        isPerp <- grep("PERP", x)
        if (length(isPerp) > 0){
            return("01/01/49")
        }

        ## Get and return the date string:
        x[grep("\\/", x)]
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
##' @return Returns the inferred maturity
##' @import rdecaf
##' @xexport
getResourcesByStock <- function(stocks, session){

    ## Construct the resource params:
    params <- list("page_size"=-1,
                   "id__in"=paste(unique(stocks[,"artifact"]), collapse=","))

    ## Get vision resource. NOTE: We are binding them safely ourselfs.
    resources <- getResource("resources", params=params, session=session)

    ## Excluding tag information, safely combine and return:
    resources <- safeRbind(lapply(resources, function(res) do.call(cbind, res[!names(res) == "tags"])))

    ## Indicate if it is a underlying:
    resources[, "is_underlying"] <- FALSE

    ## Get the underlying resources:
    resources[, "underlying"] <- safeColumn(resources, "underlying")

    ## Retrieve the underlying resources, if any:
    if (any(!is.na(resources[, "underlying"]))) {

        ## Which resources have underlyings:
        undrl <- !is.na(resources[, "underlying"])

        ## Construct the underlying resource params:
        params <- list("page_size"=-1,
                       "id__in"=paste(resources[undrl, "underlying"], collapse=","))

        ## Get and bind the undelrying resources:
        undrlResources <- safeRbind(lapply(getResource("resources", params=params, session=session), function(res) do.call(cbind, res[!names(res) == "tags"])))

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
##' @param accounts The accounts data-frame from rdecaf
##' @param session The rdecaf session
##' @param zero Either 1 or 0 to indicate whether closed positions should be included.
##' @return Returns a data-frame with the stocks for the accounts.
##' @import rdecaf
##' @export
getStocks <- function(accounts, session, zero=1){

    ## Constrct the stocks params:
    params <- list("page_size"=-1,
                   "c"="account",
                   "format"="csv",
                   "zero"=zero)

    ## Iterate over accounts and append account id to params:
    for (row in 1:NROW(accounts)) {
        params <- c(params, accounts[row,"id"])
    }

    ## Append the names to stocks params:
    names(params) <- c("page_size", "c", "format", "zero", rep("i", NROW(accounts)))

    ## Return stocks:
    as.data.frame(getResource("stocks", params=params, session=session))

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
