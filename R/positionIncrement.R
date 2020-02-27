##' This function pushes the positions difference between decaf and provider
##'
##' This is the description
##'
##' @param stocks The position comparisons data frame.
##' @param provider The provider as string.
##' @param session The rdecaf session.
##' @param ctype The ctype. Default is 20.
##' @return NULL
##' @export
pushPosDifferences <- function(stocks, provider, session, ctype=20) {

    ## Set NA QTY's in Decaf to 0:
    stocks[is.na(stocks[, "QTY"]), "QTY"] <- 0

    ## Set NA QTY's in provider to 0:
    stocks[is.na(stocks[, "qtyext"]), "qtyext"] <- 0

    ## Set NA provider prices to decaf PX Last:
    stocks[is.na(stocks[, "pxext"]), "pxext"] <- stocks[is.na(stocks[, "pxext"]), "PX Last"]

    ## Inverse provider price and qty if provider price is negative:
    stocks[stocks[, "pxext"] < 0, "qtyext"] <- stocks[stocks[, "pxext"] < 0, "qtyext"] * -1
    stocks[stocks[, "pxext"] < 0, "pxext" ] <- stocks[stocks[, "pxext"] < 0, "pxext" ] * -1

    ## Compute the differences:
    stocks[, "diff"] <- as.numeric(stocks[, "qtyext"]) - as.numeric(stocks[, "QTY"])

    ## Get the differences:
    increments <- stocks[stocks[, "diff"] != 0,]

    ## If no differences, return 0:
    if (NROW(increments) == 0) {
        return("No new increments")
    }

    ## Remove NA rows:
    increments <- increments[apply(increments, MARGIN=1, function(x) !all(is.na(x))), ]

    ## If no differences, return 0:
    if (NROW(increments) == 0) {
        return("No new increments")
    }

    ## Get the NA prices:
    naPX <- is.na(increments[, "pxext"])

    ## Use the PX Last for missing prices:
    increments[naPX, "pxext"] <- increments[naPX, "PX Last"]

    ## Construct the raw keys for the trade guid:
    guid <- paste0(toupper(provider), ":", apply(increments[, c("ID", "date", "diff", "Account", "pxext")], MARGIN=1, function(x) paste0(x, collapse="")))

    ## Construct the hased keys:
    guid <- paste0("~XID.trade.", sapply(guid, digest))

    ## Create the batches:
    batches <- createBatches(NROW(increments), 500)

    ## Interate over batches and push:
    for (i in 1:length(batches[[1]])) {

        ## Get the start index:
        start <- batches$startingIdx[[i]]

        ## Get the end index
        end <- batches$endingIdx[[i]]

        ## Get the payload:
        payload <- toJSON(list("actions"=data.frame("resmain"=increments[start:end, "ID"],
                                                    "commitment"=increments[start:end, "date"],
                                                    "qtymain"=round(increments[start:end, "diff"], 12),
                                                    "accmain"=increments[start:end, "Account"],
                                                    "pxmain"=round(as.numeric(increments[start:end, "pxext"]), 10),
                                                    "ctype"=ctype,
                                                    "guid"=guid[start:end],
                                                    stringsAsFactors=FALSE)), auto_unbox=TRUE, na="null", digits=10)

        print(paste0("Posting trades ", start, ":", end, " of ", NROW(increments)))

        ## Push and get response:
        response <- postResource("imports/inbulk", params=list(sync="True"), payload=payload, session = session)

        ## Done, return:
        return("Successfully pushed new increments")

    }
}


##' This function masks decaf positions based on provider positions:
##'
##' This is the description
##'
##' @param ePos The excess position from provider.
##' @param stocks The stocks on Decaf.
##' @param pDate The position date.
##' @param resources The resources data frame
##' @return Return the stocks with the excess provider position appended.
##' @export
appendStocks <- function(ePos, stocks, pDate, resources) {

    ## Initialize the appendix:
    appendix <- initDF(colnames(stocks), NROW(ePos))

    ## Assign the Account:
    appendix[, "Account"] <- stocks[1, "Account"]

    ## Assign the account name:
    appendix[, "accountName"] <- stocks[1, "accountName"]

    ## Assign the resource id:
    appendix[, "ID"] <- ePos[, "resmain"]

    ## Assign the provider QTY
    appendix[, "qtyext"] <- ePos[, "QTY"]

    ## Assign the decaf QTY:
    appendix[, "QTY"] <- 0

    ## Assign the Type:
    appendix[, "Type"] <- resources[match(ePos[, "resmain"], resources[, "id"]), "type"]

    ## Assign the date:
    appendix[, "date"] <- pDate

    ## Assign the provider px:
    appendix[, "pxext"] <- ePos[, "PXLAST"]

    ## Rbind and return:
    rbind(stocks, appendix)

}


##' This function masks decaf positions based on provider positions:
##'
##' This is the description
##'
##' @param pPos The positions data frame from the provider.
##' @param resources The resources data frame
##' @param pDate The position date.
##' @return Returns a masked decaf positions data frame.
##' @export
maskPositions <- function(pPos, resources, pDate) {

    ## Define the column names:
    colNames <- c("Name", "Account", "Symbol", "ID", "CCY", "Type", "QTY", "PX Last", "accountName", "qtyext", "pxext", "date", "symbol", "ctype")

    ## Initialize a data frame:
    retval <- initDF(colNames, nRow=NROW(pPos))

    ## Set the decaf QTY:
    retval[, "QTY"] <- 0

    ## Set the external QTY:
    retval[, "qtyext"] <- pPos[, "QTY"]

    ## Set the account:
    retval[, "Account"] <- pPos[, "accmain"]

    ## Set the external price:
    retval[, "pxext"] <- pPos[, "PXLAST"]

    ## Set the date:
    retval[, "date"] <- pDate

    ## Set the ctype:
    retval[, "ctype"] <- resources[match(pPos[, "resmain"], resources[, "id"]), "ctype"]

    ## Set the symbol:
    retval[, "symbol"] <- resources[match(pPos[, "resmain"], resources[, "id"]), "symbol"]

    ## Set the Symbol:
    retval[, "Symbol"] <- resources[match(pPos[, "resmain"], resources[, "id"]), "symbol"]

    ## Set the resource id:
    retval[, "ID"] <- pPos[, "resmain"]

    ## Done, return:
    retval

}


##' This function masks decaf positions based on provider positions:
##'
##' This is the description
##'
##' @param pPos The positions data frame from the provider.
##' @param resources The resources data frame
##' @param pDate The position date.
##' @param type The positions type. Either 'Security' or 'Cash' or 'FX Forward'.
##' @param session The rdecaf session.
##' @return Returns the side by side comparison of decaf position and provider position.
##' @export
getPositionComparison <- function(pPos, resources, pDate, type, session) {

    ## Remove NA rows:
    pPos <- pPos[apply(pPos, MARGIN=1, function(x) !all(is.na(x))),]

    ## Prepare consolidation params:
    consolParams <- list("i"=pPos[1, "accmain"], "c"="account", "date"=pDate)

    ## Get consolidation:
    stocks <- getFlatHoldings(getResource("consolidation", params=consolParams, "session"=session)[["holdings"]])

    ## Handle FX Forwards:
    if (type != "FX Forward") {
        ## Exclude FXFWD positions:
        stocks <- stocks[stocks[, "Type"] != "FX Forward", ]
    } else {
        stocks <- stocks[stocks[, "Type"] == "FX Forward", ]
    }

    ## If type of positions is cash, set PX Last to 1:
    if (type == "Cash") {

        ## Ensure we only take the cash positions:
        stocks <- stocks[stocks[, "Type"] == "Cash",]

        ## Set PX Last to 1:
        pPos[, "PXLAST"] <- 1
    }

    ## If type of positions is depo, set PX Last to 1:
    if (type == "Time Deposit") {

        ## Ensure we only take the cash positions:
        stocks <- stocks[stocks[, "Type"] == "Time Deposit Contract",]

        ## Set PX Last to 1:
        pPos[, "PXLAST"] <- 1
    }


    ## If position type is security, exclude cash positions:
    if (type == "Security") {
        ## Ensure we only take the non-cash positions:
        stocks <- stocks[stocks[, "Type"] != "Cash",]
    }

    ## If there are no stocks, mask empty positions:
    if (NROW(stocks) == 0) {
        ## Initialize stocks data frame with NA row:
        stocks <- initDF(colnames(stocks))
    }

    ## If no stocks, mask positions:
    if (all(is.na(stocks[1,])) & NROW(stocks) == 1) {

        ## If type is Security, use PXCOST as PXLAST:
        if (type == "Security" | type == "FX Forward") {
            print("USING COST PRICES")
            pPos[, "PXLAST"] <- ifelse(isNAorEmpty(safeColumn(pPos, "PXCOST")), pPos[, "PXLAST"], pPos[, "PXCOST"])
        }

        ## If no stocks, mask positions:
        return(maskPositions(pPos, resources, pDate))
    }

    ## Assign the account name:
    stocks[, "accountName"] <- pPos[1, "ACCOUNT"]

    ## Assign the external QTY:
    stocks[, "qtyext"] <- pPos[match(stocks[, "ID"], pPos[, "resmain"]), "QTY"]

    ## Assign the external PX:
    stocks[, "pxext"] <- pPos[match(stocks[, "ID"], pPos[, "resmain"]), "PXLAST"]

    ## Assign the position date:
    stocks[, "date"] <- pDate

    ## Are there any excess resources?
    excessPos <- pPos[is.na(match(pPos[, "resmain"], stocks[, "ID"])),]

    ## If any excess resources, append to stocks:
    if (NROW(excessPos) > 0) {
            stocks <- appendStocks(excessPos, stocks, pDate, resources)
    }

    ## Assign the symbol to stocks:
    stocks[, "symbol"] <- resources[match(stocks[, "ID"],  resources[, "id"]), "symbol"]

    ## Assign the ctype to stocks:
    stocks[, "ctype"] <- resources[match(stocks[, "ID"],  resources[, "id"]), "ctype"]

    ## Set the provider price for cash positions to 1:
    stocks[stocks[, "Type"] == "Cash", "pxext"] <- 1

    ## Select the columns and return:
    stocks[, c("Name", "Account", "Symbol", "ID", "CCY", "Type", "QTY", "PX Last", "accountName", "qtyext", "pxext", "date", "symbol", "ctype")]


}
