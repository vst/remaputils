##' TODO:
##'
##' This is the description
##'
##' @param session The rdecaf session
##' @param containerType The container type
##' @param containerId The container id
##' @param ccy The currency of the consolidation
##' @param date The date of the consolidation
##' @param charLimit The limit of characters of holdings name
##' @param resources The rdecaf resources data frame.
##' @return A data-frame with the holdings
##' @import rdecaf
##' @export
getConsolidationHoldings <- function(session, containerType, containerId, ccy, date, charLimit, resources) {

    print(paste0("From ", session[["location"]], ": Getting consolidation for ", containerType, " ", containerId))

    ## Construct the params:
    params <- list("c"=substr(containerType, 1, nchar(containerType) - 1), "i"=trimws(containerId), "ccy"=ccy, "date"=date)

    ## Get the consolidation:
    consolidation <- rdecaf::getResource("consolidation", params=params, session=session)

    ## Get and return the flat holdings:
    flatHoldings <- getFlatHoldings(consolidation[["holdings"]], charLimit)

    ## Add container name:
    flatHoldings[, "CName"] <- as.character(unlist(consolidation[["containers"]][["containers"]])["name"])

    ## Match the position with the resources:
    matchIdx <- match(flatHoldings[, "ID"], resources[, "id"])

    ## Append ISIN:
    flatHoldings[, "ISIN"] <- resources[matchIdx, "isin"]

    ## Append Reference:
    flatHoldings[, "Reference"] <- resources[matchIdx, "reference"]

    ## Append the Telekurs:
    flatHoldings[, "Telekurs"] <- resources[matchIdx, "telekurs"]

    ## Append the OHLC:
    flatHoldings[, "OHLC"] <- resources[matchIdx, "ohlccode"]

    ## Append the PX Factor:
    flatHoldings[, "PXFactor"] <- resources[matchIdx, "quantity"]

    ## Append the PX Factor:
    flatHoldings[, "Maturity"] <- resources[matchIdx, "expiry"]

    ## Done, return:
    flatHoldings

}


##' Gets the consolidation based on container type and name
##'
##' This is the description
##'
##' @param containerNames A vector with container names
##' @param containerType The container type
##' @param ccy The currency of the consolidation
##' @param date The date of the consolidation
##' @param session The rdecaf session
##' @param charLimit The character limit for holding name
##' @param resources The rdecaf resources data frame.
##' @return A list with the holdings data-frames.
##' @import rdecaf
##' @export
getConsolidationFromContainerName <- function(containerNames, containerType, ccy, date, session, charLimit=Inf, resources) {

    ## Get the container by name:
    container <- getContainer(containerNames, containerType, session)

    ## Get the consolidations:
    apply(container, MARGIN=1, function(x) getConsolidationHoldings(session, containerType, x["id"], ccy, date, charLimit, resources))

}


##' A function to get the enriched holdings data-frame.
##'
##' This is the description
##'
##' @param holdings The holdings data-frame as returned by getFlatHoldings
##' @param nav The nav as returned by getResource("consolidation").
##' @param gav The gav as returned by getResource("consolidation").
##' @param regions The data-frame with the country to region mapping.
##' @param resources The data-frame as returned by getResource("resources")
##' @param addTagsBy TODO.
##' @param childDefaults TODO.
##' @return A data-frame with the enriched holdings.
##' @import rdecaf
##' @export
getEnrichedHoldings <- function(holdings, nav, gav, regions, resources, addTagsBy=NULL, childDefaults=TRUE){

    ## Treat the country names:
    ## holdings[, "Country"] <- dbRemapCountryTreater(holdings[, "Country"], "countrymaps")

    ## If regions mapper is supplied, append to the data-frame:
    if (!is.null(regions)) {
        regions <- toupper(unlist(regions))
        regions <- data.frame("country"=regions, "region"=toupper(gsub("[[:digit:]]", "", names(regions))), row.names=NULL)
        regions <- regions[match(toupper(holdings[,"Country"]), regions[,"country"]), "region"]
    }

    ## Get the match index:
    matchIdx <- match(holdings[,"ID"], resources[,"id"])

    ## TODO:
    if (childDefaults) {
        ## Replace na Subtypes with Type:
        holdings[, "Subtype"] <- as.character(holdings[, "Subtype"])
        holdings[is.na(holdings[,"Subtype"]), "Subtype"] <- holdings[is.na(holdings[,"Subtype"]), "Type"]

        ## Replace NA Asset Class 2 with Type:
        holdings[, "Asset Class 2"] <- as.character(holdings[, "Asset Class 2"])
        holdings[is.na(holdings[,"Asset Class 2"]), "Asset Class 2"] <- holdings[is.na(holdings[,"Asset Class 2"]), "Type"]

        ## Replace NA Asset Class 3 with Subtype:
        holdings[, "Asset Class 3"] <- as.character(holdings[, "Asset Class 3"])
        holdings[is.na(holdings[,"Asset Class 3"]), "Asset Class 3"] <- holdings[is.na(holdings[,"Asset Class 3"]), "Subtype"]

    }

    if (any(safeCondition(holdings, "Subtype", "Money"))) {
        ## All 'Money' subtypes to 'Cash'.
        holdings[safeCondition(holdings, "Subtype", "Money"), "Subtype"] <- "Cash"
    }

    ## Enrich the holdings data-frame and return:
    retval <- data.frame(holdings,
                         "PnL (Contr)"=safeTry(try(as.character(round(holdings[, "PnL (%Inv)"] * holdings[,"Value"]/nav, 4)), silent=TRUE)),
                         "Region"=safeTry(try(as.character(regions))),
                         "Value (%)"=safeTry(try(holdings[,"Value"] / nav, silent=TRUE)),
                         "Exp (%)"=safeTry(try(holdings[,"Exposure"] / nav, silent=TRUE)),
                         "Expiry"=as.character(resources[matchIdx, "expiry"]),
                         "Call/Put"=safeCondition(resources[matchIdx, ], "callput", "True"),
                         "Rate"= as.character(ifelse(sapply(resources[matchIdx, "pxmain"], is.null), NA, resources[matchIdx, "pxmain"])),
                         "Underlying"=as.character(ifelse(sapply(resources[matchIdx, "underlying"], is.null), NA, resources[matchIdx, "underlying"])),
                         "ISIN"=as.character(resources[matchIdx, "isin"]),
                         "OHLC"=as.character(resources[matchIdx, "ohlccode"]),
                         check.names=FALSE,
                         stringsAsFactors=FALSE)

    ## Define addCols as NA:
    addCols <- NA

    ## If additional columns are to added, do so:
    if (!is.null(addTagsBy)) {
        addTagRetval <- addTagsAsColumns(retval, resources, addTagsBy)
        addCols <- addTagRetval[["addCols"]]
        retval <- addTagRetval[["holdings"]]
    }

    ## Done, return:
    return(list("holdings"=retval,
                "addCols"=addCols))

}


##' A wrapper function to get the enriched holdings based on consolidation parameters.
##'
##' This is the description
##'
##' @param params The parameters for the consolidation.
##' @param resources The data-frame as returned by getResource("resources").
##' @param session The rdecaf session.
##' @param charLimit The character cutoff for the name of the holdings. Default is 50.
##' @param regions The data-frame with the country to region mapping.
##' @return The enriched holdings data-frame.
##' @import rdecaf
##' @export
getHoldingsWrapper <- function(params, resources, session, charLimit=50, regions=NULL){

    ## Retrieve the consolidation:
    consolidation <- getResource("consolidation", params=params, session=session)

    ## Flatten the holdings:
    holdings <- getFlatHoldings(consolidation[["holdings"]], charLimit=charLimit)

    ## Enrich the holdings:
    getEnrichedHoldings(holdings, consolidation[["nav"]], consolidation[["gav"]], regions, resources)[["holdings"]]
}


##' A function to flatten the consolidation object.
##'
##' This is the description
##'
##' @param x The holdings element from getResource("consolidation").
##' @param charLimit The character cutoff for the name of the holdings. Default is 50.
##' @return Returns the flat holdings data-frame.
##' @import rdecaf
##' @export
getFlatHoldings <- function(x, charLimit=30){

    ## The column names:
    colNames <- c("Name",
                  "Account",
                  "Symbol",
                  "ID",
                  "CCY",
                  "Type",
                  "Subtype",
                  "Country",
                  "Sector",
                  "QTY",
                  "PX Cost",
                  "PX Last",
                  "Value",
                  "Accrd",
                  "Exposure",
                  "PnL (Unrl)",
                  "PnL (%Inv)",
                  "Asset Class",
                  "AClass Order")

    ## Initialse the holdings data-frame:
    holdings <- initDF(colNames)

    ## If no holdings, return empty holdings:
    if (NROW(x) == 0) {
        return(holdings)
    }

    ## Construct the data frame:
    holdings <- lapply(x, function(h) data.frame("Name"=.emptyToNA(as.character(ellipsify(h[["artifact"]][["name"]], charLimit=500))),
                                                 "Account"=.emptyToNA(h[["accounts"]][[1]][["id"]]),
                                                 "Symbol"=.emptyToNA(as.character(h[["artifact"]][["symbol"]])),
                                                 "ID"=.emptyToNA(as.numeric(h[["artifact"]][["id"]])),
                                                 "CCY"=.emptyToNA(as.character(h[["artifact"]][["ccy"]])),
                                                 "Type"=.emptyToNA(as.character(trimws(gsub("Contract", "", h[["artifact"]][["type"]][["name"]])))),
                                                 "Subtype"=.emptyToNA(as.character(ifelse(isNAorEmpty(h[["artifact"]][["stype"]]), NA,h[["artifact"]][["stype"]]))),
                                                 "Country"=.emptyToNA(capitalise(as.character(ifelse(isNAorEmpty(h[["artifact"]][["country"]]), NA, h[["artifact"]][["country"]])))),
                                                 "Sector"=.emptyToNA(capitalise(as.character(ifelse(isNAorEmpty(h[["artifact"]][["sector"]]), NA, h[["artifact"]][["sector"]])))),
                                                 "QTY"=.emptyToNA(as.numeric(h[["quantity"]])),
                                                 "PX Cost"=.emptyToNA(as.numeric(h[["investment"]][["px"]][["org"]])),
                                                 "PX Last"=.emptyToNA(as.numeric(h[["valuation"]][["px"]][["org"]])),
                                                 "Value"=.emptyToNA(safeNull(as.numeric(h[["valuation"]][["value"]][["net"]][["ref"]]))),
                                                 "Accrd"=.emptyToNA(as.numeric(h[["valuation"]][["accrued"]][["org"]])),
                                                 "Exposure"=.emptyToNA(safeNull(as.numeric(h[["valuation"]][["exposure"]][["net"]][["ref"]]))),
                                                 "PnL (Unrl)"=.emptyToNA(safeNull(as.numeric(h[["pnl"]]))),
                                                 ##"PnL (%Inv)"=.emptyToNA(safeNull(as.numeric(h[["pnl_to_investment"]]))),
                                                 "PnL (%Inv)"=.emptyToNA(safeNull(as.numeric(h[["pnl"]])) / safeNull(abs(as.numeric(h[["investment"]][["value"]][["ref"]])))),
                                                 "PX Val (Ref)"=.emptyToNA(safeNull(as.numeric(h[["valuation" ]][["px"]][["ref"]]))),
                                                 "PX Inv (Ref)"=.emptyToNA(safeNull(as.numeric(h[["investment"]][["px"]][["ref"]]))),
                                                 "Asset Class 1"=safeTry(try(.emptyToNA(as.character(h[["tags"]][["classification"]][[1]][["name"]])), silent=TRUE)),
                                                 "AClass 1 Order"=safeTry(try(.emptyToNA(as.character(h[["tags"]][["classification"]][[1]][["order"]])), silent=TRUE)),
                                                 "Asset Class 2"=safeTry(try(.emptyToNA(as.character(h[["tags"]][["classification"]][[2]][["name"]])), silent=TRUE)),
                                                 "AClass 2 Order"=safeTry(try(.emptyToNA(as.character(h[["tags"]][["classification"]][[2]][["order"]])), silent=TRUE)),
                                                 "Asset Class 3"=safeTry(try(.emptyToNA(as.character(h[["tags"]][["classification"]][[3]][["name"]])), silent=TRUE)),
                                                 "AClass 3 Order"=safeTry(try(.emptyToNA(as.character(h[["tags"]][["classification"]][[3]][["order"]])), silent=TRUE)),
                                                 "Asset Class 4"=safeTry(try(.emptyToNA(as.character(h[["tags"]][["classification"]][[4]][["name"]])), silent=TRUE)),
                                                 "AClass 4 Order"=safeTry(try(.emptyToNA(as.character(h[["tags"]][["classification"]][[4]][["order"]])), silent=TRUE)),
                                                 "Asset Class 5"=safeTry(try(.emptyToNA(as.character(h[["tags"]][["classification"]][[5]][["name"]])), silent=TRUE)),
                                                 "AClass 5 Order"=safeTry(try(.emptyToNA(as.character(h[["tags"]][["classification"]][[5]][["order"]])), silent=TRUE)),
                                                 check.names=FALSE))

    ## Alternative computation for PnL % if Inf:
    holdings <- lapply(holdings, function(h) {
        h[, "PnL (%Inv)"] <- ifelse(is.infinite(h[, "PnL (%Inv)"]),
                             ifelse(h[, "QTY"] > 0, 1, -1) * (h[, "PX Val (Ref)"] / h[, "PX Inv (Ref)"] - 1),
                             h[, "PnL (%Inv)"])

        h[, "PnL (%Inv)"] <- ifelse(h[, "Type"] == "FX Forward",
                                    h[, "PnL (Unrl)"] / abs(h[, "Exposure"]),
                                    h[, "PnL (%Inv)"])

        ## Return:
        return(h)

    })

    holdings <- as.data.frame(do.call(rbind, holdings), check.names=FALSE)

    ## Treat the FX Forward names:
    holdings[, "Name"] <- gsub("C/FXFWD", "FWD", holdings[, "Name"])
    holdings[, "Name"] <- gsub("CX/DEPO", "DEPO", holdings[, "Name"])
    holdings[, "Name"] <- ellipsify(holdings[, "Name"], charLimit=charLimit)

    ## Get the holdings:
    classify(holdings)
}


##' A function to generate a ordered holdings data-frame
##'
##' This is the description
##'
##' @param holdings The ordered holdings.
##' @param toplevel TODO
##' @param sublevels TODO
##' @param customTop TODO
##' @param ... Additional parameters
##' @return Returns the nested holdings data-frame.
##' @export
getOrderedHoldings <- function(holdings, toplevel="Subtype", sublevels=c("CCY", "Country"), customTop=NULL, ...) {

    ## Get the extra arguments:
    args <- list(...)[[1]]

    if (is.null(args[["seperateCash"]])) {
        sepCash <- TRUE
    } else {
        sepCash <- args[["seperateCash"]]
    }

    ## Run the custom top functions, if any:
    if (!is.null(customTop)){

        ##
        exclIdx <- 0

        ## Set the custom top levels:
        for (cTop in customTop) {

            ## Run the custom top function:
            retval <- do.call(cTop, list(holdings, args))

            if (!is.null(retval[["grpIdx"]])) {
                retval[["index"]] <- as.numeric(retval[["index"]][apply(retval[["grpIdx"]], MARGIN=1, function(x) all(is.na(match(x, exclIdx))))])
            } else {
                ## Exclude indices which were tagged exclusive prior:
                retval[["index"]] <- retval[["index"]][is.na(match(as.numeric(retval[["index"]]), exclIdx))]
            }

            ## Append the exclusive list if required:
            if (!is.null(retval[["excl"]])) {
                exclIdx <- c(exclIdx, as.numeric(retval[["index"]]))
            }

            ## Add the indices from custom top to holdings:
            holdings[retval[["index"]], toplevel] <- as.character(retval[["name"]])

        }
    }


    if (sepCash) {
        ## Get the cash:
        isCash <- holdings[,"Type"] == "Cash"
        cashHoldings <- holdings[isCash, ]

        ## Exclude the cash holdings:
        holdings <- holdings[!isCash,]
    } else {
        cashHoldings <- NULL
    }

    ## Combine the levels:
    levels <- c(toplevel, sublevels)

    ##:
    aClassOrderColIdx <- which(!is.na(match(colnames(holdings), levels))) + 1

    ##
    if (length(levels) == 1) {
        holdings <- holdings[do.call(order, list(holdings[, aClassOrderColIdx])), ]
    } else {
        ## Order the data frame:
        holdings <- holdings[do.call(order, lapply(apply(holdings[, aClassOrderColIdx], MARGIN=2, function(x) list(x)), unlist)), ]
        ##holdings <- holdings[do.call(order, lapply(apply(holdings[,levels], MARGIN=2, function(x) list(x)), unlist)), ]
    }

    for (i in 1:length(levels)) {
        colName <- paste0(levels[i], "-Key")

        if (i == 1) {
            holdings[, colName] <- holdings[, levels[1]]
            next
        }

        holdings[, colName] <- apply(holdings[, levels[1:i]], MARGIN=1, function(x) (paste(x, collapse=", ")))
    }

    topIdx <- grep("-Key", colnames(holdings))[1]

    ## topLevelWise <- lapply(unique(holdings[, topIdx]), function(z) holdings[holdings[, topIdx] == z, ])
    ## holdings <- do.call(rbind, topLevelWise[order(sapply(topLevelWise, function(x) sum(as.numeric(x[, "Value"]))), decreasing=TRUE)])

    ## Done, return:
    list("holdings"=holdings,
         "cash"=cashHoldings)

}


##' A function to generate a nested holdings data-frame
##'
##' This is the description
##'
##' @param holdings The ordered holdings.
##' @param levels TODO
##' @param toplevel TODO
##' @param sublevels TODO
##' @param orderByKeys Based on which columns should the final grouping be ordered?Defaults: Expiry, CCY, Exposure, Name and decreasing TRUE?
##' @return Returns the nested holdings data-frame.
##' @export
getNestedHoldings <- function(holdings, levels, toplevel="Subtype", sublevels=c("CCY", "Country"), orderByKeys=c("Expiry", "CCY", "Exposure", "Name")) {

    ## If no security holdings, return NULL:
    if (is.null(holdings)) {
        return(NULL)
    }

    if (NROW(holdings) == 0) {
        return(NULL)
    }

    ## Combine all keys:
    levels <- c(toplevel, sublevels)

    ## Initialise the header/footer indication columns:
    holdings[, "isHeader"] <- FALSE

    ## Iterate over the levels and nest:
    for (i in 0:(length(levels)-1)) {

        ## Get the lowest level key:
        level <- levels[length(levels)-i]
        keys <- holdings[, paste0(level, "-Key")]

        ## hebele <- !is.na(holdings[, level])
        ## keys <- keys[hebele]

        ## Get the unique keys:
        uniqueIdx <- which(!duplicated(keys))

        ## Initialise a new data frame (extend the holdings dimensionality with additional rows):
        newM <- as.data.frame(matrix(data=NA, nrow=length(uniqueIdx) + NROW(holdings), ncol=NCOL(holdings)))
        colnames(newM) <- colnames(holdings)

        ## Compute the indices of holdings in the new data frame:
        newMIdx <- 1:NROW(holdings) + cumsum((!duplicated(keys)) * 1)

        ## Fill the Holdings:
        newM[newMIdx, ] <- as.data.frame(holdings, stringsAsFactors=FALSE)

        ## Compute the header indices:
        headerIdx <- newMIdx[!duplicated(keys)] - 1

        ## Fill the Headers:
        newM[headerIdx, ] <- do.call(rbind, lapply(unique(keys), function(key) c(key, rep(NA, NCOL(newM) - 1))))

        ## Fill the level's key column:
        newM[headerIdx, paste0(level, "-Key")] <- unique(keys)

        ## Fill the next levels key column:
        if (length(levels) -i != 1) {

            ## Get the prior level's key:
            priorKey <- paste0(levels[length(levels)-i -1], "-Key")

            ## Get the key replacement (basically fill NA's)
            replaceK <- lapply(strsplit(newM[is.na(newM[headerIdx, priorKey]), paste0(level, "-Key")], ","), function(x) paste(head(x, -1), collapse=","))
            replaceK <- sapply(replaceK, function(x) ifelse(isNAorEmpty(x), NA, x))

            ## Replace the NA with the key:
            newM[is.na(newM[headerIdx, priorKey]), priorKey] <- replaceK
        }

        ## Fill the isHeader and order columns:
        newM[headerIdx, "isHeader"] <- TRUE
        newM[headerIdx, "order"] <- length(levels) - i

        ## Clean NA's in the isHeader and isFooter columns:
        newM[,"isHeader"] <- ifelse(is.na(newM[,"isHeader"]), FALSE, newM[,"isHeader"])

        ## Assign the new data frame back to holdings:
        holdings <- newM
    }

    ## Here, we order the final level of grouping alphabetically:

    ## First, get the last colum index with the word "-Key", ie our final level of grouping key.
    finalKeyIdx <- tail(grep("-Key", colnames(holdings)), 1)

    ## Iterate over the unique final grouping keys:
    for (key in unique(holdings[, finalKeyIdx])) {

        ## Identify the indices for the final grouping keys' positions (excluding header and footer)
        holdingsIdx <- which(holdings[, finalKeyIdx] == key & holdings[, "isHeader"] == "FALSE") ##& holdings[, "isFooter"] == "FALSE")

        orderList <- list()

        for (orderBy in orderByKeys) {

            if (orderBy == "Exposure") {
                orderList[[orderBy]] <- as.numeric(holdings[holdingsIdx, orderBy])
            } else {
                orderList[[orderBy]] <- holdings[holdingsIdx, orderBy]
            }
        }

        ## Shall the ordering be decreasing?
        decreasing <- as.logical(sapply(orderByKeys, function(x) switch(x, "Expiry"=FALSE, "CCY"=FALSE, "Exposure"=TRUE, "Name"=FALSE)))

        if (length(orderByKeys) > 1) {
            decreasing = TRUE
        }

        ## Reorder the positions and assign back:
        holdings[holdingsIdx, ] <- holdings[holdingsIdx, ][do.call(order, c(orderList, "decreasing"=decreasing)), ]

        ## ## Reorder the positions and assign back:
        ## holdings[holdingsIdx, ] <- holdings[holdingsIdx,][order(holdings[holdingsIdx, "Expiry"],
        ##                                                         holdings[holdingsIdx, "CCY"],
        ##                                                         as.numeric(holdings[holdingsIdx, "Exposure"]),
        ##                                                         holdings[holdingsIdx, "Name"],
        ##                                                         decreasing=TRUE), ]

        ## holdings[holdingsIdx, ] <- holdings[holdingsIdx, ][do.call(order, lapply(apply(holdings[holdingsIdx, c("Expiry", "CCY", "Exposure", "Name")], MARGIN=2, function(x) list(x)), unlist)), ]

    }

    ## Return:
    holdings
}


##' A function to generate the formatted holdings data-frame
##'
##' This is the description
##'
##' @param holdings The nested holdings.
##' @return Returns the formated holdings data-frame.
##' @export
getFormattedHoldings <- function(holdings){

    ## Convert colum format to numeric:
    for (col in 1:NCOL(holdings)) {

        if (toupper(colnames(holdings)[col]) == "EXPIRY") {
            holdings[, col]
        }

        if (toupper(colnames(holdings)[col]) == "SYMBOL") {
            next
        }


        val <- suppressWarnings(try(as.numeric(holdings[, col]), silent=TRUE))

        if (class(val) == "try-error" | all(is.na(val))) {
            next
        }

        holdings[, col] <- .replaceNA(val)
    }

    holdings
}


##' A function to generate the printable holdings data-frame
##'
##' This is a description.
##'
##' @param portfolio The portfolio id
##' @param account The account id
##' @param ccy The currency of reporting
##' @param date The date of reporting
##' @param dtype The date type of reporting
##' @param toplevel TODO
##' @param sublevel TODO
##' @param customTop TODO
##' @param colselect TODO
##' @param regions The list with the country to region mapping
##' @param summaryaddon TODO
##' @param addTagsBy TODO
##' @param childDefaults TODO
##' @param charLimit TODO
##' @param orderByKeys TODO
##' @param session The rdecaf session
##' @param ... Additional parameters
##' @return A data-frame with the printable format of holdings
##' @import tidyverse
##' @export
getPrintableHoldings <- function(portfolio=NA,
                                 account=NA,
                                 ccy,
                                 date,
                                 dtype,
                                 toplevel,
                                 sublevel,
                                 customTop,
                                 colselect,
                                 regions,
                                 summaryaddon,
                                 addTagsBy=NULL,
                                 childDefaults=TRUE,
                                 charLimit=30,
                                 orderByKeys=c("Expiry", "CCY", "Exposure", "Name"),
                                 session, ...){

    ## Get additional arguments:
    args <- list(...)

    ## Get the container type:
    containerType <- ifelse(is.na(portfolio), "accounts", "portfolios")

    ## Get the container id:
    containerID <- ifelse(is.na(portfolio), account, portfolio)

    ## Get the portfolio details:
    ## portfolioDetails <- as.data.frame(rdecaf::getResource("portfolios", params=list("id"=portfolio, "format"="csv", "page_size"=-1), session=session))
    containerDetails <- as.data.frame(rdecaf::getResource(containerType, params=list("id"=containerID, "format"="csv", "page_size"=-1), session=session))

    ## Grep the columns with the name shareclass in it:
    shareclassCol <- grep("shareclass", colnames(containerDetails))

    ## If such column exists, do following:
    if (length(shareclassCol) > 0) {

        ## Initialise the isin variable:
        isin <- NULL

        ## Iterate over the shareclass columns:
        for (shcl in shareclassCol) {

            ## Get the in-loop shareclass:
            shclses <- as.data.frame(rdecaf::getResource("shareclasses", params=list("id"=containerDetails[, shcl], "format"="csv", "page_size"=-1), session=session))

            ## Append the isin of the in-loop shareclass to the isin variable:
            isin <- paste(isin, ifelse(is.na(shclses[, "isin"]), "", shclses[, "isin"]), sep=" ")
        }

    ## If not shareclass exists, try to get the isin from portfolio details:
    } else {
        isin <- safeTry(try(containerDetails[,"isin"], silent=TRUE))
    }

    isin <- gsub(" ", ", ", trimws(isin))

    ## Get the inception:
    inception <- safeTry(try(containerDetails[,"inception"], silent=TRUE))

    ## Get the consolidation:
    if (containerType == "portfolios") {
        consolidation <- rdecaf::getResource("fundreport", params=list("fund"=containerID, ccy=ccy, date=date, type=dtype), session=session)
    } else {
        consolidation <- rdecaf::getResource("consolidation", params=list("c"="account","i"=containerID, "ccy"=ccy, "date"=date, "type"=dtype, "page_size"=-1), session=session)
    }

    ## Get the flat holdings:
    holdings <- getFlatHoldings(consolidation[["holdings"]], charLimit=charLimit) %>%
      mutate_at(.vars=c("Value","Exposure"),numerize)

    ## Check if we have any holdings:
    noHoldings <- all(apply(holdings, MARGIN=1, function(x) all(is.na(x))))

    ## If no holdings, return empty holdings:
    if (noHoldings) {

    holdingsDetails <- list("holdingsDetails"=initDF(colselect),
                            "subtlIdx"=1,
                            "subtl2Idx"=1,
                            "totalIdx"=1,
                            "holdsIdx"=1)

        return(list("holdings"=holdingsDetails,
                    "consolidation"=consolidation,
                    "rawHoldings"=NULL,
                    "colselect"=colselect,
                    "resources"=NULL,
                    "formattedHoldings"= NULL))
    }

    ## Get the stocks:
    stocks <- getStocksFromContainerNames(session, containerType, containerDetails[, "name"], zero=0, date)

    ## Get the resources:
    resources <- getResourcesByStock(stocks, session)

    ## Get the enriched holdings:
    enrichedHoldings <- getEnrichedHoldings(holdings, consolidation[["nav"]], consolidation[["gav"]], regions, resources, addTagsBy, childDefaults)

    addCols <- enrichedHoldings[["addCols"]]

    enrichedHoldings <- enrichedHoldings[["holdings"]] %>%
      dplyr::mutate(Exposure=if_else(is.na(Exposure),0,Exposure))

    ## Get the ordered holdings:
    orderedHoldings <- getOrderedHoldings(enrichedHoldings, toplevel=toplevel, sublevels=sublevel, customTop=customTop, args)

    ## Get the nested holdings:
    nestedHoldings <- getNestedHoldings(orderedHoldings[["holdings"]], toplevel=toplevel, sublevels=sublevel, orderByKeys=orderByKeys)

    ## Append the cash holdings:
    nestedHoldings <- appendCash(orderedHoldings, nestedHoldings)

    ## Get the subtotalled holdings:
    totalledHoldings <- getSubtotalledHoldings(nestedHoldings)

    ## Get formatted holdings:
    formattedHoldings <- getFormattedHoldings(totalledHoldings)

    ## Append the isin & inception:
    consolidation[["isin"]] <- isin
    consolidation[["inception"]] <- inception

    if (any(!is.na(addCols))) {
        colselect <- c(colselect, addCols)
    }

    ## Clean the header name
    formattedHoldings <- formattedHoldings[, colselect]  %>%
      mutate(isHeader=if_else(is.na(isHeader),FALSE,as.logical(isHeader)))

    holdingsDetails <- getHoldingsDetails(formattedHoldings,colselect,consolidation[["nav"]])

    ## Return:
    list("holdings"=holdingsDetails,
         "consolidation"=consolidation,
         "rawHoldings"=enrichedHoldings,
         "colselect"=colselect,
         "resources"=resources,
         "formattedHoldings"= formattedHoldings
         )
}

##' TODO:
##'
##' This is the description
##'
##' @param holdings TODO
##' @param resources TODO
##' @param addTagsBy TODO
##' @return A data-frame with the holdings
##' @import rdecaf
##' @export
addTagsAsColumns <- function(holdings, resources, addTagsBy) {

    ## Get the match index:
    matchIdx <- match(holdings[,"ID"], resources[,"id"])

    ## Get the relevant, aligned resources:
    resources <- resources[matchIdx, ]

    tagsTemp <- unlist(lapply(lapply(resources[, "tags"], function(x) unlist(x)), function(x) x[safeGrep(x, addTagsBy) == "1"]))

    ## If not tag qualifies with the addTagsBy seperator, return retval:
    if (is.null(tagsTemp)) {
        return(list("holdings"=holdings,
                    "addCols"=NA))
    }

    ## Get the tags by the seperator addTagsBy:
    tags <- unlist(strsplit(tagsTemp, addTagsBy))

    ## Get the non-empty tags:
    tags <- tags[nchar(tags) > 0]

    ## If not tag qualifies with the addTagsBy seperator, return retval:
    if (length(tags) == 0) {
        return(list("holdings"=holdings,
                    "addCols"=NA))
    }

    ## Get the additional column names from tag:
    addCols <- unique(sapply(strsplit(tags, ":"), function(x) x[1]))

    ## If no tag qualifies with the seperator, return retval:
    if (length(addCols) == 0) {
        return(list("holdings"=holdings,
                    "addCols"=NA))
    }

    ## Add/append the tags as column:
    holdings[, addCols] <- NA

    ## Split each tag by the addTagsBy key:
    splitKey <- lapply(resources[, "tags"], function(x) unlist(strsplit(as.character(x), addTagsBy)))

    ## Get rid of empty splits:
    splitKey <- lapply(splitKey, function(x) x[nchar(x) > 0])

    ## Split by column and value separator:
    columnValue<- lapply(splitKey, function(x) strsplit(as.character(x), ":"))

    ## Iterate over each column and value keys:
    for (i in 1:length(columnValue)) {

        ## Match the column value with the appended columns:
        matchIdx <- sapply(columnValue[[i]], function(x) match(x[1], colnames(holdings)))

        ## If no match, next:
        if (length(matchIdx) == 0) {
            next
        }

        ## Assign the column values to the appended columsn:
        holdings[i, na.omit(matchIdx)] <- sapply(columnValue[[i]], function(x) x[2])[!is.na(matchIdx)]
    }

    ## Done, return:
    return(list("holdings"=holdings,
                "addCols"=addCols))
}


##' This function prepares the holding details and summary tables for html/pdf.
##' It requires the holdings list from getPrintableHoldings.
##'
##' This is the description
##'
##' @param holdings The holdings list from getPrintableHoldings.
##' @param colSelect Which columns from the getPrintableHoldings should be selected?
##' @param summaryCol The column of holdings by which to summarize the holdings by.
##' @param summaryKey Optionally, you may select sublevels for the summary for a key in summaryCol.
##' @return A list with the holdingsDetails, holdingsSummary data frames, and the original holdings list.
##' @export
prepareHoldingsTables <- function(holdings,
                                  colSelect=c("Name", "Value", "Value (%)", "Exposure", "Exp (%)", "PnL (Unrl)"),
                                  summaryCol="Subtype",
                                  summaryKey="HEBELE1234") {

    ## Get the holdings details:
    holdingsDetails <- holdings[["holdings"]][["holdingsDetails"]]

    ## Parse the Type if it exists:
    if (any(colnames(holdingsDetails) == "Type")) {
        holdingsDetails[, "Type"] <- capitalise(ellipsify(holdingsDetails[, "Type"], 15))
    }

    hDetailsTable <- holdingsDetails
    hDetailsTable$Value <- ifelse(hDetailsTable$Value != "",
                           ifelse(hDetailsTable$Accrd != "",
                                  sprintf("%s<div class='text-muted'>%s</div>", hDetailsTable$Value, hDetailsTable$Accrd),
                                  hDetailsTable$Value),
                                      "")
    hDetailsTable$Accrd <- NULL

    holdings[["holdings"]][["holdingsDetails"]] <- hDetailsTable

    ## Get the holdings summary:
    holdingsSummary <- getHoldingsSummary(holdings[["formattedHoldings"]], col=summaryCol, key=summaryKey)

    ## Now, we create the asset class details by deducing from holdings details:
    assetClassDetails <- holdings[["holdings"]]

    ## Get all the indices of the totals and subtype indices:
    subIndices <- c(assetClassDetails[["subtlIdx"]], assetClassDetails[["totalIdx"]])

    ## Rank the sub indices:
    rankedIndices <- cbind(subIndices, "rank"=rank(subIndices))

    ## Re-index subtypes:
    assetClassDetails[["subtlIdx"]] <- rankedIndices[match(assetClassDetails[["subtlIdx"]], rankedIndices[, "subIndices"]), "rank"]

    ## Re-index totals:
    assetClassDetails[["totalIdx"]] <- rankedIndices[match(assetClassDetails[["totalIdx"]], rankedIndices[, "subIndices"]), "rank"]

    ## Exclude holdings:
    assetClassDetails[["holdingsDetails"]] <- assetClassDetails[["holdingsDetails"]][-assetClassDetails[["holdsIdx"]], ]

    ## Exclude holdings-relevant columns:
    assetClassDetails[["holdingsDetails"]] <- assetClassDetails[["holdingsDetails"]][, colSelect]

    ## Set row names to NULL:
    rownames(assetClassDetails[["holdingsDetails"]]) <- NULL

    ## Set holdings indices to NA:
    assetClassDetails[["holdsIdx"]] <- NA

    ## Done, combine and return:
    return(list("holdingsDetails"=holdings[["holdings"]],
                "holdingsSummary"=holdingsSummary,
                "assetClassDetails"=assetClassDetails,
                "holdings"=holdings))

}
