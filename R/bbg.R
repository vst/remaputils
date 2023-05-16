##' R wrapper around bdlhs.
##'
##' @param host Remote SFTP server hostname.
##' @param port Remote SFTP server port.
##' @param user Remote SFTP username.
##' @param pass Remote SFTP password.
##' @param flds The fields.
##' @param tcks The tickers.
##' @param exe Path to or name of `bdlhs` executable.
##' @return A data frame
##' @export
bdlhs <- function (host, port, user, pass, flds, tcks, exe="bdlhs") {
    ## Check if the bdlhs executable is installed:
    if (Sys.which(exe) == "") {
        stop("bdlhs executable is not found.")
    }

    ## Define the arguments:
    args <- list("--host"=host, "--port"=port, "--user"=user, "--pass"=pass, "--flds"=paste(flds, collapse=" "))

    ## Prepare arguments:
    args <- as.character(sapply(names(args), function (x) paste0(x, "=\"", args[[x]], "\"")))

    ## Run the application:
    resdata <- system2(exe, args=args, stdout=TRUE, input=paste(tcks, collapse="\n"))

    ## Read the output and return:
    read.csv(text=resdata, sep="|")
}


##' A function to provide the transformations for bbg results.
##'
##' @return A list with the memnonic as the name of the list and the function as the element.
##' @export
bbgTransformFuns <- function() {
    list("LAST_TRADEABLE_DT"=function(x) {as.Date(x, format="%Y%m%d")},
         "FUT_NOTICE_FIRST"=function(x) {as.Date(x, format="%Y%m%d")},
         "MATURITY"=function(x) {as.Date(x, format="%Y%m%d")},
         "ID_ISIN"=function(x) {as.character(x)},
         "NAME"=function(x) {as.character(x)},
         "SECURITY_TYP"=function(x) {as.character(x)},
         "SECURITY_TYP2"=function(x) {as.character(x)},
         "FUND_ASSET_CLASS_FOCUS"=function(x) {as.character(x)},
         "MIFID_UNDERLYING_ASSET_CLASS"=function(x) {as.character(x)},
         "BPIPE_REFERENCE_SECURITY_CLASS"=function(x) {as.character(x)},
         "CFI_CODE"=function(x) {as.character(x)},
         "COUNTRY"=function(x) {as.character(x)},
         "EXCH_CODE"=function(x) {as.character(x)},
         "ID_MIC_PRIM_EXCH"=function(x) {as.character(x)},
         "SECURITY_DES"=function(x) {as.character(x)},
         "COUNTRY_FULL_NAME"=function(x){as.character(x)},
         "ISSUER"=function(x){as.character(x)},
         "INDUSTRY_SECTOR"=function(x){as.character(x)},
         "RTG_SP_LT_LC_ISSUER_CREDIT"=function(x){as.character(x)},
         "RTG_SP_OUTLOOK"=function(x){as.character(x)},
         "PRIMARY_EXCHANGE_NAME"=function(x){as.character(x)},
         "CPN"=function(x){as.numeric(x)},
         "FUTURES_CATEGORY"=function(x){as.character(x)},
         "FIRST_CPN_DT"=function(x) {as.Date(x, format="%Y%m%d")},
         "CPN_FREQ"=function(x) {as.integer(x)},
         "OPT_STRIKE_PX"=function(x) {as.numeric(x)})
}


##' A function to build the reference data request object.
##'
##' @param resources The resources data frame.
##' @param bbgFields The list of bbg field mappings.
##' @param forceAux Are the aux fields to be forced?
##' @return Either a list with the reference data object or NULL.
##' @export
referenceFieldRequestBuilder <- function(resources, bbgFields, forceAux=FALSE) {

    ## Is there a base field mapping?
    base  <- !is.null(bbgFields[["BASE"]])

    ## Is there a ctype field mapping?
    ctype <- !is.null(bbgFields[["CTYPE"]])

    ## Are there aux type request?
    aux   <- !is.null(bbgFields[["AUXS"]])

    ## If nothing to be requested, return NULL:
    if (!base & !aux & !ctype) {
        return(NULL)
    }

    ## Initialise the return value:
    referenceFlds <- list()

    ## Check if all instruments have base field information:
    referenceFlds[["id"]] <- resources[, c("symbol", "priceIds", "ctype")]

    ## If base information is to be retrieved, add fields to be called.
    if (base) {

        ## Determine which fields are missing in the system resources:
        referenceFlds[["fld"]] <- cbind(referenceFlds[["fld"]], apply(resources[, bbgFields[["BASE"]]], MARGIN=2, isNAorEmpty))

        ## Append the additional bbg memnonics:
        referenceFlds[["bbg"]] <- c(referenceFlds[["bbg"]], names(bbgFields[["BASE"]]))
    }

    ## If ctype-specific fields are to be called, add fields:
    if (ctype) {
        ## Hebele:
        for (i in 1:length(bbgFields[["CTYPE"]])) {

            ## Get the current ctype bbg fields:
            currentType <- bbgFields[["CTYPE"]][i]

             ## Resources which are current ctype:
            isCtype <- resources[, "ctype"] == names(currentType)

            ## If no such ctype, next:
            if (!any(isCtype)) {
                next
            }

            ## Are fields for current ctype empty?
            fld <- apply(resources[isCtype, currentType[[1]]], MARGIN=2, isNAorEmpty)

            ## Initialise the appendable data-frame with all FALSE:
            appendable <- data.frame(matrix(FALSE, NROW(referenceFlds[["id"]]), NCOL(fld)))

            ## Add column name to appendable:
            colnames(appendable) <- colnames(fld)

            ## Set fields for current ctype in appendable:
            appendable[isCtype, ] <- fld

            ## Append to reference fields:
            referenceFlds[["fld"]] <- as.data.frame(cbind(referenceFlds[["fld"]], as.matrix(appendable)), check.names=FALSE)

            ## Append the bbg memnonics:
            referenceFlds[["bbg"]] <- c(referenceFlds[["bbg"]], names(currentType[[1]]))
        }
    }

    ## Either base or ctype-specific fields are to be called, reduce to empty fields:
    if (!is.null(referenceFlds[["fld"]])) {

        ## For which instruments to we need to request data?
        requestableResrcs <- apply(referenceFlds[["fld"]], MARGIN=1, any)

        ## For which fields to we need to request data?
        requestableFields <- apply(referenceFlds[["fld"]], MARGIN=2, any)

        ## Filter in resources id's for which we need to request data:
        referenceFlds[["id"]] <- referenceFlds[["id"]][requestableResrcs, ]

        ## Filter in resources for which we need to request data:
        if (sum(requestableResrcs) == 1) {
            referenceFlds[["fld"]] <- data.frame(t(referenceFlds[["fld"]][requestableResrcs, ]), check.names = FALSE)
        } else {
            referenceFlds[["fld"]] <- data.frame(referenceFlds[["fld"]][requestableResrcs, ], check.names = FALSE)
        }

        ## Filter in fields for which we need to request data:
        referenceFlds[["fld"]] <- data.frame(referenceFlds[["fld"]][, requestableFields], check.names=FALSE)

        ## Filter in bbg memnonic for which we need to request data:
        referenceFlds[["bbg"]] <- referenceFlds[["bbg"]][requestableFields]

    }

    ## If aux is TRUE, append aux:
    if (aux & (NROW(referenceFlds[["fld"]]) != 0 | forceAux)) {

        ## Initialise the appendable matrix:
        appendable <- data.frame(matrix(FALSE, NROW(referenceFlds[["id"]]), length(names(bbgFields[["AUXS"]]))))
        colnames(appendable) <- names(bbgFields[["AUXS"]])

        ## For each aux memomnic (column), append to fields to be called.
        for (col in 1:NCOL(appendable)) {
            mgrepRetval <- mgrep(referenceFlds[["id"]][, "ctype"], bbgFields[["AUXS"]][[col]])
            appendable[, col] <- apply(mgrepRetval, MARGIN=1, function(x) any(x != "0"))
        }

        ## Prepare the appendable data frame:
        appendable <- appendable[, apply(appendable, MARGIN=2, function(x) any(x))]

        ## Extend the bbg memnonics to be called:
        referenceFlds[["bbg"]] <- c(referenceFlds[["bbg"]], colnames(appendable))

        ## Prepare the column names:
        colnames(appendable) <- paste0("_AUXS_", colnames(appendable))

        ## Append aux data frame to reference fields:
        referenceFlds[["fld"]] <- as.data.frame(cbind(referenceFlds[["fld"]], as.matrix(appendable)), check.names=FALSE)

    }

    ## If no fields are to be called, return NULL:
    if (is.null(referenceFlds[["fld"]])) {
        return(NULL)
    }

    ## Parse the column names:
    colnames(referenceFlds[["fld"]]) <- mgsub(colnames(referenceFlds[["fld"]]), c("\\.1", "\\.2", "\\.3"))

    ## Done, return
    referenceFlds

}


##' A function to build the requestable currency pairs and the entire combinations of currencies
##'
##' @param mainFX A vector with the main currencies
##' @param minorFX A vector with the minor currencies.
##' @return A list with the requestable currency pairs and all combinations.
##' @export
getExchangeRatePairs <- function(mainFX, minorFX) {

    ## Get the grid of the main currency pairs:
    mainPairs <- expand.grid(mainFX, mainFX)

    ## Exclude the diagonals:
    mainPairs <- mainPairs[apply(mainPairs, MARGIN=1, function(x) x[1] != x[2]), ]

    ## Prepare the fx request data frame
    ## NOTE: Append the minor currency vs 'USD' to the main currency pairs:
    reqPairs <- data.frame("main"=c(as.character(mainPairs[, 1]), rep("USD", length(minorFX)), minorFX),
                           "altn"=c(as.character(mainPairs[, 2]), minorFX, rep("USD", length(minorFX))),
                           stringsAsFactors=FALSE)

    ## Extend the request data frame with symbol and ticker:
    reqPairs <- data.frame(reqPairs,
                           "symbol"=apply(reqPairs, MARGIN=1, function(x) paste0(x, collapse="")),
                           "ticker"=paste0(apply(reqPairs, MARGIN=1, function(x) paste0(x, collapse="")), " Curncy"),
                           stringsAsFactors=FALSE)

    ## Get the grid for all currencies:
    allPairs <- expand.grid(c(mainFX, minorFX), c(mainFX, minorFX))

    ## Exclude the diagonals:
    allPairs <- allPairs[apply(allPairs, MARGIN=1, function(x) x[1] != x[2]), ]

    ## Prepare the data frame for all currency pairs:
    allPairs <- data.frame("main"=as.character(allPairs[, 1]),
                           "altn"=as.character(allPairs[, 2]),
                           stringsAsFactors=FALSE)

    ## Extend the data frame with the symbol:
    allPairs <- data.frame(allPairs,
                           "symbol"=apply(allPairs, MARGIN=1, function(x) paste0(x, collapse="")))


    ## Done, return:
    return(list("requestPairs"=reqPairs,
                "allPairs"=allPairs))

}


##' A function to build the requestable currency pairs and the entire combinations of currencies
##'
##' @param session The rdecaf session.
##' @param date The date.
##' @param zero The zero parameter for stocks.
##' @param underlying Shall underlying be included if stocks are to be considered?
##' @param bbgFields The list of bbg field mappings.
##' @param resources The entire resource data frame.
##' @param byStock Should the request data be based on stocks only?
##' @param forceAux Are the aux fields to be forced?
##' @param exclCtypes Which ctypes should be excluded from consideration?
##' @return A list with the reference request data objest and the resources.
##' @export
getRequestDataReference <- function(session,
                                    date,
                                    zero,
                                    underlying,
                                    bbgFields,
                                    resources,
                                    byStock=TRUE,
                                    forceAux=FALSE,
                                    exclCtypes=c("CCY", "LOAN", "FXFWD", "DEPO")) {

    ## If by stock, get resources by stock:
    if (byStock) {

        ## Prepare the params for the stock request:
        params <- list(page_size=-1, format="csv", date=date, zero=zero)

        ## Get the stocks:
        stocks <- as.data.frame(rdecaf::getResource("stocks", params=params, session=session))

        if (NROW(stocks) == 0) {
            ## Return ohlc id's with missing information:
            return(list("reference"=NULL,
                        "resources"=resources))
        }

        ## Get the resources by stocks:
        resources <- remaputils::getResourcesByStock(stocks, session, getUnderlying=TRUE)
    }

    ## Get the price ID's:
    priceIDs <- ifelse(!isNAorEmpty(resources[, "ohlccode"]), as.character(resources[, "ohlccode"]), as.character(resources[, "symbol"]))

    ## Append the price ID's
    resources <- data.frame(resources, "priceIds"=priceIDs, stringsAsFactors=FALSE)

    ## Check if identifer is bbg ticker:
    resources <- resources[isBBGTicker(as.character(resources[, "priceIds"])), ]

    ## Exclude ctypes from resources:
    resources <- resources[!apply(mgrep(resources[, "ctype"], exclCtypes), MARGIN=1, function(x) any(x != "0")), ]

    ## Get the reference Flds
    reference <- referenceFieldRequestBuilder(resources, bbgFields, forceAux=forceAux)

    ## Done, return
    return(list("reference"=reference,
                "resources"=resources))

}


##' A function to build the requestable currency pairs and the entire combinations of currencies
##'
##' @param session The rdecaf session.
##' @param date The date.
##' @param zero The zero parameter for stocks.
##' @param underlying Shall underlying be included if stocks are to be considered?
##' @param fx If exchange rate prices are to be called, provide a vector with main currencies, otherwise NULL.
##' @param resources The entire resource data frame.
##' @param priceTag Force resources which this tag to be considered.
##' @param byStock Should the request data be based on stocks only?
##' @return A list with the reference request data objest and the resources.
##' @export
getRequestDataPrice <- function(session, date, zero, underlying, fx=NULL, resources, priceTag, byStock) {

    ## Benchmark Stuff!
    tagsColumns <- data.frame(resources[, safeGrep(colnames(resources), "tags") == "1"])

    ## Append taged resources:
    tagsPriceIds <- resources[apply(tagsColumns, MARGIN=1, function(x) any(x == priceTag)), "symbol"]

    ## If by stock, get resources by stock:
    if (byStock) {

        ## Prepare the params for the stock request:
        params <- list(page_size=-1, format="csv", date=date, zero=zero)

        ## Get the stocks:
        stocks <- as.data.frame(rdecaf::getResource("stocks", params=params, session=session))

        if (NROW(stocks) == 0) {
            ## Return ohlc id's with missing information:
            return(NULL)
        }

        ## Get the resources by stocks:
        resources <- remaputils::getResourcesByStock(stocks, session, getUnderlying=TRUE)
    }

    ## Get the price ID's:
    priceIDs <- as.character(ifelse(!isNAorEmpty(resources[, "ohlccode"]), as.character(resources[, "ohlccode"]), as.character(resources[, "symbol"])))

    ## Append the price ID's
    resources <- data.frame(resources, "priceIds"=priceIDs, stringsAsFactors=FALSE)

    ## Check if identifer is bbg ticker:
    resources <- resources[isBBGTicker(as.character(resources[, "priceIds"])), ]

    ## Determine which resources have not yet expired:
    isAlive <-  as.Date(as.character(resources[, "horizon"])) > Sys.Date() -1
    isAlive <- ifelse(is.na(isAlive), TRUE, isAlive)

    ## Filter resources which have not expired yet:
    resources <- resources[isAlive, ]

    ## Combine the resource price ids and the price ids for resources with tags:
    priceIds <- c(resources[, "priceIds"], tagsPriceIds)

    ## If fx is not NULL, append exchange rate symbols to price ids:
    if (!is.null(fx)) {

        ## Assign the vector with main currencies to the ccymain:
        ccymains <- fx

        ## Get the ccymain's from the resource data frame:
        ccyminor <- unique(resources[, "ccymain"])

        ## Get the minor currencies:
        ccyminor <- ccyminor[is.na(match(ccyminor, ccymains))]

        ## Get to exchange rate pairs object:
        exchangePairs <- getExchangeRatePairs(ccymains, ccyminor)

        ## Append the requestable exchange rate pairs to price id's:
        priceIds <- c(priceIds, exchangePairs[["requestPairs"]][, "ticker"])

        ## Assign the exchange rate pairs object to fx:
        fx <- exchangePairs
    }

    ## Return ohlc id's with missing information:
    list("prices"=as.character(priceIds),
         "fx"=fx,
         "resources"=resources)
}


##' A function to treat the results of a bbgdl price request.
##'
##' @param result The result data frame as obtained from bdhls.
##' @param reqData The original request data from getRequestDataPrice.
##' @param noCents If noCents, prices in cents will be divided by 100. Default is TRUE.
##' @param addBid Should Bid price be added separately. Default=FALSE.
##' @return A list with the ohlc observations and invalid symbols.
##' @export
treatBBGResultsPrice <- function(result, reqData, noCents=TRUE, addBid=FALSE) {

    if (is.null(result)) {
        return(NULL)
    }

    ## The price fields to be requested:
    priceFlds <- c("PX_LAST",
                   "LAST_UPDATE",
                   "QUOTED_CRNCY",
                   "PX_YEST_CLOSE",
                   "PX_YEST_DT")

    ##:
    if (addBid) {
        priceFlds <- c(priceFlds, "PX_BID", "PX_YEST_BID")
    }

    ## Set columns characters:
    for (col in c("ID", priceFlds)) {
        result[, col] <- as.character(result[, col])
    }

    ## Prepare the PX_LAST observations:
    obs <- data.frame("symbol"=result[, "ID"],
                      "date"=Sys.Date(),
                      "close"=result[, "PX_LAST"],
                      "last_update"=result[, "LAST_UPDATE"],
                      "quoted_ccy"=result[, "QUOTED_CRNCY"],
                      stringsAsFactors=FALSE)

    ## Append the PX_YEST observations to PX_LAST:
    obs <- rbind(obs, data.frame("symbol"=result[, "ID"],
                                 "date"=Sys.Date(),
                                 "close"=result[, "PX_YEST_CLOSE"],
                                 "last_update"=result[, "PX_YEST_DT"],
                                 "quoted_ccy"=result[, "QUOTED_CRNCY"],
                                 stringsAsFactors=FALSE))

    ##:
    if (addBid) {

        obs <- rbind(obs , data.frame("symbol"=paste0(result[, "ID"], "|BID"),
                                      "date"=Sys.Date(),
                                      "close"=result[, "PX_BID"],
                                      "last_update"=result[, "LAST_UPDATE"],
                                      "quoted_ccy"=result[, "QUOTED_CRNCY"],
                                      stringsAsFactors=FALSE))

        obs <- rbind(obs, data.frame("symbol"=paste0(result[, "ID"], "|BID"),
                                     "date"=Sys.Date(),
                                     "close"=result[, "PX_YEST_BID"],
                                     "last_update"=result[, "PX_YEST_DT"],
                                     "quoted_ccy"=result[, "QUOTED_CRNCY"],
                                     stringsAsFactors=FALSE))
    }

    ## Invalid symbols:
    invalids <- obs[, "close"] == "N.A." | isNAorEmpty(trimws(obs[, "close"]))

    ## Extract symbol from obs to list:
    obsBySymbol <- extractToList(obs, "symbol")

    ## Determine the invalid symbols in list:
    ## invalidSymbols <- do.call(rbind, lapply(obsBySymbol, function(x) x[all(x[, "close"] == "N.A." | isNAorEmpty(x[, "close"])), ]))
    invalidSymbols <- result[result[, "X_1"] != 0, "ID"]

    ## Valid observations:
    obs <- obs[!invalids,]

    ## When last update date is missing, replace with system date:
    obs[is.na(obs[, "last_update"]), "last_update"] <- as.character(gsub("-", "", Sys.Date()))

    ## Parse the date info from LAST_UPDATE:
    dates <- as.Date(obs[, "last_update"], format="%Y%m%d")

    ## Parse the date for ohlc observation:
    obs[, "date"] <- ifelse(is.na(dates), as.character(Sys.Date()), as.character(dates))

    ##  If noCents, then divide any close for which the quoted currency has lower case by 100:
    if (noCents) {
        ## When lower case in quoted currency exists, divide close by 100:
        isCents <- grepl("[a-z]", obs[, "quoted_ccy"])
        obs[isCents, "close"] <- as.numeric(obs[isCents, "close"]) / 100
    }

    ## Remove column:
    obs[, "last_update"] <- NULL
    obs[, "quoted_ccy"] <- NULL

    ## If fx object in request data is not null, treat the FX:
    if (!is.null(reqData[["fx"]])) {

        ## First, we need to transform the bbg ticker to a valid decaf fx symbol:
        ## ############################################################################

        ## Match the symbol in the result (i.e bbg ticker) with the request data ticker:
        matchIdx <- match(obs[, "symbol"], reqData[["fx"]][["requestPairs"]][, "ticker"])

        ## Use the matching index to get the symbol:
        obs[!is.na(matchIdx), "symbol"] <- reqData[["fx"]][["requestPairs"]][matchIdx[!is.na(matchIdx)], "symbol"]

        ## Second, we need to compute the fx crosses:
        ## ############################################################################

        ## Get all pairs:
        allPairs <- reqData[["fx"]][["allPairs"]]
        allPairs <- allPairs[!apply(allPairs, 1, function(x) any(is.na(x))), ]

        ## Extract obs to list by date:
        obsByDate <- extractToList(obs, "date")

        ## For each obs by date, run compute FX crosses:
        obs <- rbind(obs, do.call(rbind, lapply(obsByDate, function(x) computeFXCrosses(allPairs, x))))

    }

    ## Done, return:
    return(list("ohlc"=obs,
                "invalidSymbols"=invalidSymbols))

}


##' A function to compute FX crosses. It assume 'USD' as anchor currency and that inverses of rate for anchor vs minor is provided.
##'
##' @param allPairs A data frame with all the currency pairs. Requires "main", "altn" and "symbol" columns.
##' @param pricedPairs A data frame with the priced currency pairs. Requires "symbol", "close" and "date" columns.
##' @return A list with the ohlc observations and invalid symbols.
##' @export
computeFXCrosses <- function(allPairs, pricedPairs) {

    ## Initialise the date and close columns in allPairs:
    allPairs[, c("date", "close")] <- NA

    ## Match the symbols between all pairs and priced pairs.
    matchIdx <- match(allPairs[, "symbol"], pricedPairs[, "symbol"])

    ## If no matches, return NULL:
    if (all(is.na(matchIdx))) {
        return(NULL)
    }

    ## Add the closes of the priced exchange rates to the matching all pairs closes:
    allPairs[!is.na(matchIdx), "close"] <- pricedPairs[matchIdx[!is.na(matchIdx)], "close"]

    ## Add the dates of the priced exchange rates to the matching all pairs dates:
    allPairs[!is.na(matchIdx), "date"] <- pricedPairs[matchIdx[!is.na(matchIdx)], "date"]

    ## NA closes in all pairs are, hence, crosses:
    allPairs[, "cross"] <- ifelse(is.na(allPairs[, "close"]), TRUE, FALSE)

    ## Get the crosses:
    crosses <- allPairs[allPairs[, "cross"], ]

    ## Get the mains:
    mains <- allPairs[!allPairs[, "cross"], ]

    ## Iterate over the crosses rows:
    for (row in 1:NROW(crosses)) {

        ## Get the current cross:
        cross <- crosses[row, ]

        ## Get the first leg of the cross rate:
        cross1 <- mains[mains[, "altn"] == cross[, "main"] & mains[, "main"] == "USD", "close"]

        ## Get the second leg of the cross rate:
        cross2 <- mains[mains[, "altn"] == cross[, "altn"] & mains[, "main"] == "USD", "close"]

        ## Get the corresponding date:
        date <- mains[mains[, "altn"] == cross[, "altn"] & mains[, "main"] == "USD", "date"]

        ## If no cross, skip computation:
        length(cross1) > 1 || next

        ## If no cross, skip computation:
        length(cross2) > 1 || next

        ## Compute the cross close and append to the crosses rate:
        crosses[row, "close"] <- round(as.numeric(cross2) / as.numeric(cross1), 8)

        ## Append the date:
        crosses[row, "date"] <- date
    }

    ## Done, return
    return(crosses[, c("symbol", "date", "close")])
}


##' A function to email the BBGDL price update report.
##'
##' @param ohlc The list object coming from treatBBGResultsPrice.
##' @param emailContent A list with the email contents. Expects "greeting", "deployment", "url".
##' @param emailParams A list with the email parameters.
##' @param reportText An optional report text to override the default text. Default is NULL.
##' @param session The rdecaf session.
##' @return NULL
##' @export
emailBBGDLPxReport <- function(ohlc, emailContent, emailParams, reportText=NULL, session) {

    if (is.null(reportText)) {
        baseText <- paste0("We ran BBG DL price request just now. ",
                           "%s instruments were successfully updated. ",
                           "%s instruments were not. Below is a list",
                           " with unpriced symbols. Please check the symbols!")

        reportText <- sprintf(baseText,
                              length(unique(ohlc[["ohlc"]][, "symbol"])),
                              length(unique(ohlc[["invalidSymbols"]])))

    }

    ## Construct the content of the alert email:
    .UPDATETEXT <- list("GREETINGPLACEHOLDER"=emailContent[["greeting"]],
                        "EMAILBODYPLACEHOLDER"="This is a autogenerated alert for USS BBG DL Price Requests.",
                        "CALLTOACTIONPLACEHOLDER"="Go to System",
                        "DEPLOYMENT"=emailContent[["deployment"]],
                        "URLPLACEHOLDER"=emailContent[["url"]],
                        "FINALPARAGRAPHPLACEHOLDER"=reportText,
                        "ADDRESSPLACEHOLDER"="",
                        "GOODBYEPLACEHOLDER"="Best Regards,<br>DECAF TEAM",
                        "ADDENDUMPLACEHOLDER"=emailHTMLTable(data.frame(unique(ohlc[["invalidSymbols"]])),
                                                             provider="BBG",
                                                             caption="No prices!",
                                                             sourceType="API"))

    ## Run sync email report:
    syncUpdateEmail(template=readLines("../assets/update_email.html"),
                    updateText=.UPDATETEXT,
                    emailParams=emailParams,
                    subject=" BBG DL Alert: ")
}
