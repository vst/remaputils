##' A function to get the remap content on strapi.
##'
##' This is a description.
##'
##' @param baseUrl  The base url of the remap db. Default: "https://db.remap.decafhub.com/".
##' @param contentName The name of the content.
##' @return The httr GET response.
##' @export
getRemapContent <- function(baseUrl="https://db.remap.decafhub.com/", contentName) {

    ## Parse base url:
    url <- httr::parse_url(paste0(baseUrl, contentName))

    ## Add the limit param:
    url$query <- list("_limit"=999999)

    ## Build the url:
    url <- httr::build_url(url)

    ## Get the response:
    response <- httr::GET(url)

    ## Get the status of response:
    status <- response$status_code

    ## Print and stop if exception:
    if (status != 200) {
        stop(sprintf("%s returned a status code of '%d'.\n\n  Details provided by the API are:\n\n%s",
                     url, status, httr::content(response, as = "text")))
    }

    ## Return the content:
    suppressMessages(httr::content(response))

}


##' This function converts letter code or arbitrary country names to full names:
##'
##' This is a description.
##'
##' @param countries A string vector with country names & codes.
##' @param countrymaps The name of the countryMap content.
##' @return Returns a vector with the correct country names where possible.
##' @export
dbRemapCountryTreater <- function(countries, countrymaps) {

    ## Get the country map:
    countryMap <- as.data.frame(do.call(rbind, getRemapContent(contentName=countrymaps)))

    ## Which columns to check:
    checkCols <- c("iso-3-letter", "iso-2-letter", "arbitrary1")

    ## Try to match:
    matches <- lapply(checkCols, function(col) match(trimConcatenate(countries), trimConcatenate(countryMap[, col])))

    ## Flatten the match:
    flatMatch <- apply(do.call(cbind, matches), MARGIN=1, function(x) ifelse(all(is.na(x)), NA, x[!is.na(x)]))

    ## Write the correct country name:
    countries[!is.na(flatMatch)] <- unlist(countryMap[flatMatch[!is.na(flatMatch)], "name"])

    ## Capitalise country names:
    capitalised <- capitalise(countries)

    ## Assign capitalised country names to non-NA's:
    countries[!is.na(countries)] <- capitalised[!is.na(countries)]

    ## Notavailable field to NA:
    countries[countries == "Notavailable"] <- NA

    ## Done, return:
    return(countries)

}


##' This function tries to identify if string is a bloomberg ticker.
##'
##' This is a description.
##'
##' @param tickers A vector with strings.
##' @return Returns a TRUE | FALSE vector.
##' @export
isBBGTicker <- function(tickers) {

    ## The correct bloomberg ticker suffices:
    suffices <-  c("Corp", "Equity", "Govt", "Curncy", "Index", "Comdty")

    ## Check if any suffix is ticker and return:
    apply(do.call(cbind, lapply(suffices, function(x) safeGrep(tickers, x) == "1")), MARGIN=1, any)
}


##' This function cleans possible errors in bloomberg ticker symbols.
##'
##' This is a description.
##'
##' @param tickers A vector with bloomberg ticker symbols.
##' @return Returns a vector with cleaned bloomberg ticker symbols.
##' @export
dbRemapBBGTickerCleaner <- function(tickers) {

    ## The correct bloomberg ticker suffices:
    suffices <-  c("Corp", "Equity", "Govt", "Curncy", "Index", "Comdty")

    ## Get the possible errorenous suffixes and get the error matrix:
    errMatrix <- mgrep(tickers, c(tolower(suffices), toupper(suffices)))

    ## Replace the errorenous suffix where necessary:
    tickers <- sapply(1:NROW(errMatrix), function(i) {
            errSuffix <- errMatrix[i, ][errMatrix[i,] != "0"]
            if (length(errSuffix) == 0) {
                return(rownames(errMatrix)[i])
            }
            gsub(errSuffix, capitalise(errSuffix), rownames(errMatrix)[i])
    })

    ## Done, return:
    tickers

}


##' A function to prepare DECAF resources for dbRemap action.
##'
##' This is a description.
##'
##' @param resources The data-frame with the DECAF resources.
##' @param exCtypes The ctypes to be excluded from dbRemap consideration.
##' @param rplKeysInField A list with fields and grep/replace keys.
##' @param tickerField A string indicating with resource field corresponds to ticker.
##' @param tickerProvider A string indicating which market data provider ticker to be used. Currently only "BBG". Default is "BBG".
##' @return A data-frame with the prepared DECAF resources.
##' @export
dbRemapPrepareResources <- function(resources,
                                    exCtypes=c("FXFWD", "CCY", "DEPO", "LOAN", "FWD", "OTHER"),
                                    rplKeysInField=list("symbol"=c(" @"="@"),
                                                        "symbol"=c(" - LONG"=""),
                                                        "symbol"=c(" - SHORT"="")),
                                    tickerField="symbol",
                                    tickerProvider="BBG") {

    ## In this function, we are going to prepare the source decaf resources object
    ## for any action related to dbRemap data base.

    ## First, we will omit NA or empty symbols:
    resources <- resources[!isNAorEmpty(resources[, tickerField]), ]

    ## Second, we will get rid of unwanted ctypes:
    resources <- excludeRowsWithKeys(resources, "ctype", keys=exCtypes)

    ## Iterate over keys to be replaced in target field:
    for (i in 1:length(rplKeysInField)) {

        ## Get the field name:
        fldName <- names(rplKeysInField)[i]

        ## Get the keys to be excluded:
        rplKeys <- rplKeysInField[[i]]

        ## Exclude keys:
            resources[, fldName] <- mgsub(resources[, fldName], patterns=names(rplKeys), replace=rplKeys)
    }

    ## Third, we will treat the tickers:
    resources[, tickerField] <- do.call(switch(provider, "BBG"="remapBBGTickerCleaner"), list(resources[, tickerField]))

    ## Do we have the right ticker formats:
    isTicker <- do.call(switch(tickerProvider, "BBG"=isBBGTicker), list(resources[, tickerField]))

    ## Assign to ticker field:
    resources[isTicker, "ticker"] <- resources[isTicker, tickerField]

    ## Are the isin fields really isin?
    isIsin <- isIsin(resources[, "isin"])

    ##
    resources[!isIsin, "isin"] <- NA

    ## Do we have any avaloq id's:
    isAvaloq <- mxgrep(resources[, c("reference", "symbol", "ohlccode")], "~DEBSM")

    ## For an avaloq id, add to resources as field:
    resources[!is.na(isAvaloq), "avaloq"] <- isAvaloq[!is.na(isAvaloq)]

    ## Treat the countries:
    resources[, "country"] <- dbRemapCountryTreater(resources[, "country"], "countrymaps")

    ## Done, return:
    resources

}
