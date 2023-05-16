##' A function to email execution & booking report.
##'
##' This is the description
##'
##' @param trdOrdExecutions The trade order executions data-frame.
##' @param data The incoming external execution data.
##' @param fldMap A list with the field mappings between trade order execution and incoming execution data.
##' @param emailContent The content parameters of the email.
##' @param session The rdecaf session.
##' @return NULL
##' @export
emailExecutionReport <- function(trdOrdExecutions, data, fldMap, emailContent, session) {

    ## Append the order id:
    trdOrdExecutions[, "order"] <- data[match(trdOrdExecutions[, "allocation"], data[, fldMap[["allocation"]]]), fldMap[["order"]]]

    ## Append the symbol:
    trdOrdExecutions[, "symbol"] <- data[match(trdOrdExecutions[, "allocation"], data[, fldMap[["allocation"]]]), fldMap[["symbol"]]]

    ## Append the direction:
    trdOrdExecutions[, "direction"] <- data[match(trdOrdExecutions[, "allocation"], data[, fldMap[["allocation"]]]), fldMap[["direction"]]]

    ## Append the trade date:
    trdOrdExecutions[, "tradedate"] <- data[match(trdOrdExecutions[, "allocation"], data[, fldMap[["allocation"]]]), fldMap[["date"]]]

    ## Construct the trade links:
    tradeLink <- paste0(gsub("api", "", session[["location"]]), "trade/details/", trdOrdExecutions[, "booking"])
    orderLink <- paste0(gsub("api", "", session[["location"]]), "trading/tradeorder/details/", trdOrdExecutions[, "order"])

    ## Construct the report email data-frame:
    result <- data.frame("Trade"=tradeLink,
                         "Order"=orderLink,
                         "Symbol"=trdOrdExecutions[, "symbol"],
                         "Direction"=trdOrdExecutions[, "direction"],
                         "Trade Date"=trdOrdExecutions[, "tradedate"],
                         "QTY"=beautify(as.character(round(as.numeric(trdOrdExecutions[, "quantity"]), 2)), nsmall=2),
                         "PX"=beautify(as.character(round(as.numeric(trdOrdExecutions[, "pricenet"]), 2)), nsmall=2),
                         check.names=FALSE,
                         stringsAsFactors=FALSE)

    ## HTMLize the links:
    result[, "Trade"] <- paste0("<a href='", result[, "Trade"], "'>LINK</a>")
    result[, "Trade"] <- ifelse(is.na(trdOrdExecutions[, "booking"]), NA, result[, "Trade"])
    result[, "Order"] <- paste0("<a href='", result[, "Order"], "'>LINK</a>")

    ## Generate the outlier table as HTML and convert to string:
    result <- as.character(emailHTMLTable(result,
                                          provider="DECAF",
                                          caption="Executed & Booked Trades",
                                          sourceType="API"))

    ## Clean the HTML text:
    result <- gsub("&#62;LINK&#60;/a&#62;", ">LINK<aya/a>", result)
    result <- gsub("&#60;a href", "<a href", result)

    .UPDATETEXT <- list("GREETINGPLACEHOLDER"=emailContent[["greeting"]],
                        "EMAILBODYPLACEHOLDER"="We have executed & booked trades in the system. ",
                        "CALLTOACTIONPLACEHOLDER"="Go to System",
                        "DEPLOYMENT"=emailContent[["deployment"]],
                        "URLPLACEHOLDER"=emailContent[["url"]],
                        "FINALPARAGRAPHPLACEHOLDER"="Please contact us if you experience any issues or have questions/feedback.",
                        "ADDRESSPLACEHOLDER"="",
                        "GOODBYEPLACEHOLDER"="Best Regards,<br>DECAF TEAM",
                        "ADDENDUMPLACEHOLDER"=result)

    emailParams[["emailList"]] <- emailContent[["emaillist"]]

    syncUpdateEmail(template=readLines("../assets/update_email.html"),
                    updateText=.UPDATETEXT,
                    emailParams=emailParams,
                    subject=" DECAF Trade Execution Update: ")

}


##' A function to retrieve trade order data objects.
##'
##' This is the description
##'
##' @param session The rdecaf session.
##' @return A list with trader oder, trade order allocation and trade order execution data frames.
##' @export
getTradingObjects <- function(session) {
    ## Get the trade orders:
    return(list("trdOrds"=as.data.frame(getResource("tradeorders", params=list("page_size"=-1, "format"="csv"), session=session)),
                "trdOrdAllocations"=as.data.frame(getResource("tradeorderallocations", params=list("page_size"=-1, "format"="csv"), session=session)),
                "trdOrdExecutions"=as.data.frame(getResource("tradeorderexecutions", params=list("page_size"=-1, "format"="csv"), session=session))))
}


##' A function to patch trade order executions with the trade id.
##'
##' This is the description
##'
##' @param trdOrdExecutions A data-frame with the 'booking' column populated.
##' @param session The rdecaf session.
##' @return NULL
##' @export
patchTrdOrdExecutions <- function(trdOrdExecutions, session) {

    ## Set booked to NULL:
    trdOrdExecutions[, "booked"] <- NULL

    ## For each trade order execution with booking, patch the trade id:
    for (row in 1:NROW(trdOrdExecutions)) {

        ## Get current trade order execution:
        cc <- trdOrdExecutions[row, ]

        ## If there is no booking, skip:
        if (is.na(cc[, "booking"])) {
            next
        }

        ## Prepare payload of the patch:
        payload <- toJSON(list("booking"=cc[, "booking"]), auto_unbox=TRUE, na="null")

        ## Patch:
        patchResource(paste0("tradeorderexecutions/", cc[, "id"]), payload=payload, session=session)
    }

}

##' A function to book executions from incoming external execution data.
##'
##' This is the description
##'
##' @param resources The rdecaf resources data-frame.
##' @param trdOrdExecutions The trade order executions data-frame.
##' @param trdOrds The trade order data-frame.
##' @param allocationField The name of the allocation id column in the incoming execution data.
##' @param trdOrdAllocations The trade order allocation data-frame.
##' @param data The incoming external execution data.
##' @param bookingFun The booking function for the incoming execution data.
##' @param session The rdecaf session.
##' @return NULL
##' @export
bookExecutions <- function(resources, trdOrdExecutions, trdOrds, allocationField, trdOrdAllocations, data, bookingFun, session) {

    ## Get exectutions which are not booked yet:
    ## bookableExecutions <- trdOrdExecutions[!trdOrdExecutions[, "booked"] , ]
    bookableExecutions <- trdOrdExecutions

    ## If nothing to book, return NULL:
    if (NROW(bookableExecutions) == 0) {
        return(NULL)
    }

    ## Get those executions to which the incoming data refers to:
    bookableExecutions <- bookableExecutions[!is.na(match(data[, allocationField], bookableExecutions[, "allocation"])), ]

    ## If nothing to book, return NULL:
    if (NROW(bookableExecutions) == 0) {
        return(NULL)
    }

    ## Call the booking function:
    bookableExecutions <- do.call(bookingFun, list(resources, trdOrds, bookableExecutions, trdOrdAllocations, allocationField, data, session))

    ## Append the booking ID to trade order executions:
    trdOrdExecutions[, "booking"] <- bookableExecutions[match(trdOrdExecutions[, "allocation"], bookableExecutions[, "allocation"]), "tradeid"]
    trdOrdExecutions <- trdOrdExecutions[match(bookableExecutions[, "allocation"], trdOrdExecutions[, "allocation"]), ]

    ## Done, return:
    return(trdOrdExecutions)

}


##' A function to book executions from incoming external execution data.
##'
##' This is the description
##'
##' @param trdOrdExecutions The trade order executions data-frame.
##' @param data The incoming external execution data.
##' @param fldMap A list with the field mappings between trade order execution and incoming execution data.
##' @param session The rdecaf session.
##' @return NULL
##' @export
executeAllocations <- function(trdOrdExecutions, data, fldMap, session) {

    ## If no trade order executions, mask:
    if (NROW(trdOrdExecutions) == 0) {
        trdOrdExecutions <- data.frame("allocation"=NA)
    }

    ## Get the allocation match idx:
    matchIdx <- match(data[, fldMap[["allocation"]]], trdOrdExecutions[, "allocation"], incomparables=NA)

    ## Get the new executions:
    newExecutions <- data[is.na(matchIdx), ]

    ## Make new executions: !!! Note: Additional partial executions are not possible at the moment.
    if (NROW(newExecutions) > 0) {

        ## Construct the data frame:
        df <- data.frame("quantity"=newExecutions[, fldMap[["quantity"]]],
                         "id"=NA,
                         "date"=parseDate(newExecutions[, fldMap[["date"]]]),
                         "allocation"=newExecutions[, fldMap[["allocation"]]],
                         "orderID"=newExecutions[, fldMap[["order"]]],
                         "pricenet"=newExecutions[, fldMap[["pricenet"]]],
                         "pricegrs"=newExecutions[, fldMap[["pricegrs"]]])

        ## Execute trades:
        for (row in 1:NROW(df)) {
            exe <- df[row, ]
            payload <- toJSON(as.list(exe), auto_unbox=TRUE)
            result <- postResource("tradeorderexecutions", payload=payload, session=session)
        }

        ## Update the trade order executions:
        trdOrdExecutions <- as.data.frame(getResource("tradeorderexecutions", params=list("page_size"=-1, "format"="csv"), session=session))
    }

    ## Reduce trade order executions to the incoming data:
    trdOrdExecutions <- trdOrdExecutions[match(data[, fldMap[["allocation"]]], trdOrdExecutions[, "allocation"]), ]

    ## Done, return:
    return(trdOrdExecutions)
}
