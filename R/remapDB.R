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

##' This function tries to identify if string is a telekurs id.
##'
##' This is a description.
##'
##' @param str A vector with strings.
##' @return Returns a TRUE | FALSE vector.
##' @export
isTelekursID <- function(str) {

    ## Is the id an integer?
    isInteger <- isWholenumber(str)

    ## Is the number of characters of the str between 5 and 10?
    isLength <- nchar(str) > 5 & nchar(str) < 10

    ## Done, return:
    isInteger & isLength

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
    resources[, tickerField] <- do.call(switch(tickerProvider, "BBG"="dbRemapBBGTickerCleaner"), list(resources[, tickerField]))

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

    ## SHRE ctype cannot have expiry:
    resources[resources[, "ctype"] == "SHRE", "expiry"] <- NA

    ## Get rid of all NOTAVAILABLE strings:
    ## !!TODO
    for (i in 1:NCOL(resources)) {
        isNA <- toupper(resources[, i]) == "NOTAVAILABLE"
        isNA <- ifelse(is.na(isNA), FALSE, isNA)
        resources[isNA, i] <- NA
    }

    ## Done, return:
    resources

}


##' This function checks if the incoming resource identifer is new and valid
##'
##' This is a description.
##'
##' @param remapDB The remap database as data-frame.
##' @param sourceDB The source database as data-frame.
##' @param isinFlds The isin field mapper for source database.
##' @param idFlds The field mapper for identifier in source database.
##' @return Returns list of list. 1: Are identifiers new? 2: Are identifiers valid?
##' @export
dbRemapNewResource <- function(remapDB, sourceDB, isinFlds=NULL, idFlds=NULL) {

    ## Initialise the lists:
    isNewID <- list()
    isValidID <- list()

    if (!is.null(idFlds)) {

        ## Get the id fields:
        idFlds <- idFlds[!sapply(idFlds, is.na)]

        ## Determine if fields are new:
        isNewID <- lapply(1:length(idFlds), function(i) {
            is.na(match(sourceDB[, idFlds[[i]]], remapDB[, names(idFlds)[i]], incomparables=NA))
        })

        ## Get the validation functions corresponding to the id fields:
        validationFun <- sapply(names(idFlds), function(x) switch(x,
                                                                  "ticker"="isBBGTicker",
                                                                  "telekurs"="isTelekursID"))
        ## Validate the ID's:
        isValidID <- lapply(1:length(idFlds), function(i) {
            do.call(validationFun[i], list(sourceDB[, idFlds[[i]]]))
        })

        ## Assign the names:
        names(isValidID) <- names(idFlds)
        names(isNewID) <- names(idFlds)
    }

    if (!is.null(isinFlds)) {

        ## Get the incomparable isins:
        incomparables <- paste0(NA, unique(na.omit(sourceDB[, isinFlds[["ccymain"]]])))

        ## Generate composite isin+ccy for remap db:
        isinRemap <- as.character(apply(remapDB[, c("isin", "ccymain")], MARGIN=1, function(x) paste0(x, collapse="")))

        ## Generate composite isin+ccy for resources:
        isinSource <- as.character(apply(sourceDB[, c(isinFlds[["isin"]], isinFlds[["ccymain"]])], MARGIN=1, function(x) paste0(x, collapse="")))

        ## Match the symbols:
        isNewID <- c(list(is.na(match(isinSource, isinRemap, incomparables=incomparables))), isNewID)

        isValidID <- c(list(isIsin(sourceDB[, isinFlds[["isin"]]])), isValidID)
        names(isValidID) <- c("isin", tail(names(isValidID), -1))

        ## Reassign names:
        names(isNewID) <- c("isin", tail(names(isNewID), -1))

    }

    return(list("newIDs"=isNewID,
                "validIDs"=isValidID))
}


patchDataframe <- function(targetDF,
                           sourceDF,
                           mField,
                           putMap,
                           patchMap) {

    ## Match the 2 data-frames:
    matchIdx <- match(targetDF[, names(mField)], sourceDF[, mField[[1]]], incomparables=NA)

    ## Rearrange the 2 data-frames:
    newTargetDF <- targetDF[!is.na(matchIdx), ]
    newSourceDF <- sourceDF[matchIdx[!is.na(matchIdx)], ]

    ## Iterate over the patch map:
    for (i in 1:length(patchMap)) {

        ## Get the target field:
        targetField <- names(patchMap)[i]

        ## Get the source field:
        sourceField <- patchMap[[i]]

        ## Which rows should be overwritten?
        overwrite <- is.na(newTargetDF[, targetField]) & !is.na(newSourceDF[, sourceField])

        ## Overwrite the field:
        newTargetDF[overwrite, targetField] <- as.character(newSourceDF[overwrite, sourceField])
    }

    ## Iterate over the put map:
    for (i in 1:length(putMap)) {

        ## Get the target field:
        targetField <- names(putMap)[i]

        ## Get the source field:
        sourceField <- putMap[[i]]

        ## Which rows should be overwritten?
        overwrite <- !is.na(newSourceDF[, sourceField])

        ## Overwrite the field:
        newTargetDF[overwrite, targetField] <- as.character(newSourceDF[overwrite, sourceField])

    }

    ## Reassing the treated data to the original data-frame:
    targetDF[!is.na(matchIdx), ] <- newTargetDF

    ## Done, return:
    targetDF

}


##' This TODO
##'
##' This is a description.
##'
##' @param resources TODO
##' @param mktProvider TODO
##' @param tickerField TODO
##' @param dbRemapName TODO
##' @param isinFlds TODO
##' @param idFlds TODO
##' @param figiApiKey TODO
##' @param figiMField TODO
##' @param figiPutMap TODO
##' @param figiPatchMap TODO
##' @return TODO
##' @export
dbRemapCheckUpdate <- function(resources,
                               mktProvider,
                               tickerField,
                               dbRemapName,
                               isinFlds,
                               idFlds,
                               figiApiKey,
                               figiMField,
                               figiPutMap,
                               figiPatchMap) {

    ## 1. Get the dbRemap
    ## 2. Check which identifiers are new and valid
    ## 3. Check new isins on Figi
    ## 4. Post new identifiers
    ## 5. Check old identifiers for changes???

    ## Get the remap fields:
    remapFields <- as.character(unlist(getRemapContent(contentName="resourcemetas")[[1]][["fields"]]))

    ## Get the Remap db:
    remapDB <- getRemapContent(contentName=dbRemapName)

    ## If remapDB is empty, create ficticious row:
    if (NROW(remapDB) == 0) {
        remapDB <- as.data.frame(matrix(NA, 1, length(remapFields)))
        colnames(remapDB) <- remapFields
    } else {
            remapDB <- safeRbind(remapDB)
    }

    ## Prepare the source resources:
    resources <- dbRemapPrepareResources(resources,
                                         tickerField=tickerField,
                                         tickerProvider=mktProvider)

    ## Identify new and valid identifiers:
    newRes <- dbRemapNewResource(remapDB,
                                 resources,
                                 isinFlds=isinFlds,
                                 idFlds=idFlds)

    ## Hebele:
    if (any(names(newRes[["newIDs"]]) == "isin")) {

        isNewIsin <- newRes[["newIDs"]][["isin"]] & newRes[["validIDs"]][["isin"]]

        checkFigi <- isNewIsin & apply(do.call(cbind, lapply(c("SHRE", "BOND", "FUT", "OPT"), function(x) safeGrep(resources[, "ctype"], x) == "1")), MARGIN=1, any)

        if (any(checkFigi)) {
            ## Run Figi:
            figiResult <- figi(resources[checkFigi, ], figiApi=figiApiKey)

            ## Patch the data-frame:
            resources <- patchDataframe(targetDF=resources,
                                        sourceDF=figiResult,
                                        mField=figiMField,
                                        putMap=figiPutMap,
                                        patchMap=figiPatchMap)
        }
    }

    newIDs   <- newRes[["newIDs"]]
    validIDs <- newRes[["validIDs"]]

    ## Which ones to post:
    posts <- apply(do.call(cbind, lapply(1:length(newIDs), function(i) {

        idx <- 1:length(newIDs)

        isNew <- newIDs[[i]] & validIDs[[i]]
        for (num in idx[-i]) {
            isNew <- isNew & (newIDs[[num]] | !validIDs[[num]])
        }

        isNew
    })), MARGIN=1, any)


    if (any(posts)) {
        postRecords <- as.data.frame(lapply(remapFields, function(x) safeColumn(resources[posts, ], x)), stringsAsFactors=FALSE)
        colnames(postRecords) <- remapFields
    } else {
        postRecords <- NULL
    }

    return(list("resources"=resources,
                "isNewID"=newRes[["newIDs"]],
                "isValidID"=newRes[["validIDs"]],
                "remapFields"=remapFields,
                "posts"=postRecords))

    ## exResources <- resources[!posts, ]
    ## exTickerMatch <- match(exResources[, "ticker"], remapDB[, "ticker"], incomparables=NA)
    ## exIsinMatch <- match(compIsinResrc[!posts], compIsinRemap, incomparables=incomparables)
    ## exMatches <- ifelse(is.na(exTickerMatch), exIsinMatch, exTickerMatch)
    ## exResources <- exResources[!is.na(exMatches), ]
    ## exMatches <- exMatches[!is.na(exMatches)]
    ## remapDB <- remapDB[exMatches,]
    ## remapFields <- as.character(unlist(getRemapContent(contentName="resourcemetas")[[1]][["fields"]]))
    ## exResources <- as.data.frame(lapply(remapFields, function(x) safeColumn(exResources, x)), stringsAsFactors=FALSE)
    ## colnames(exResources) <- remapFields
    ## remapIDs <- remapDB[, "id"]
    ## remapDB <- as.data.frame(lapply(remapFields, function(x) safeColumn(remapDB, x)), stringsAsFactors=FALSE)
    ## colnames(remapDB) <- remapFields
    ## ## naFieldsRemapDB <- t(apply(remapDB, MARGIN=1, function(x) is.na(x) | x == "NA"))
    ## ## naFieldsResrcDB <- t(apply(exResources, MARGIN=1, function(x) is.na(x) | x == "NA"))
    ## ## aa <- naFieldsResrcDB - naFieldsRemapDB

}


dbRemapPost <- function(records, baseUrl, content) {

    for (row in 1:NROW(records)) {

        print(row)

        payload <- as.list(records[row,])

        response <- httr::POST(paste0(baseUrl, content), body=payload, encode="json")

    }

}
