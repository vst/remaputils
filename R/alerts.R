##' A function to push a payload to a decaf instance.
##'
##' This is the description
##'
##' @param accounts The data-frame with the rdecaf accounts. If NULL, all accounts of session are considered. Default is NULL.
##' @param resources The data-frame of the resources in the decaf instance.
##' @param session The rdecaf session.
##' @return A data-frame of positions which are duplicated in terms of isin.
##' @import rdecaf
##' @export
duplicatedIsinInPortfolios <- function(accounts=NULL, resources, session) {

    ## If no accounts given, retriev all accounts:
    if (is.null(accounts)) {
        ## Get system accounts:
        accounts <- as.data.frame(getResource("accounts", params=list("page_size"=-1, "format"="csv"), session=session))
    }

    ## Get the account wise positions:
    stocks <- getStocks(accounts, session, zero=0)

    ## Get the enriched stocks:
    stocks <- getEnrichedStocks(stocks, accounts, resources)

    ## Append the portfolio id:
    stocks[, "portfolio"] <- accounts[match(stocks[, "account"], accounts[, "id"]), "portfolio"]

    ## Append the portfolio id:
    stocks[, "portfolio_name"] <- accounts[match(stocks[, "account"], accounts[, "id"]), "portfolio_name"]

    ## Get the portfolio wise stocks:
    pWiseStocks <- lapply(unique(stocks[, "portfolio"]), function(x) stocks[stocks[, "portfolio"] == x, ])

    ## Iterate over list and return duplicate positions with duplicate isin:
    pWiseStocks <- safeRbind(lapply(1:length(pWiseStocks), function(i) {

        ## Get the current list element:
        pWS <- pWiseStocks[[i]]

        ## Which stocks are duplicated in terms of isin:
        pWS[, "duplIsin"] <- duplicated(pWS[, "isin"])

        ## Filter out rows of which the isin field is not an isin:
        pWS <- pWS[isIsin(pWS[,"isin"]), ]

        ## If there are any rows, return such:
        if (any(pWS[, "duplIsin"])) {
            return(pWS[pWS[, "duplIsin"], ])
        } else {
            ## Else return NULL:
            return(NULL)
        }
    }))

    ## If there are now duplicates, return NULL:
    if (NROW(pWiseStocks) == 0){
        return(NULL)
    }

    ## Return the data-frame with duplicate isins in portfolio:
    data.frame("Portfolio"=pWiseStocks[, "portfolio_name"],
               "ISIN"=pWiseStocks[,"isin"],
               "Symbol"=pWiseStocks[, "symbol"],
               "Name"=pWiseStocks[, "name"])
}


##' A function prepares the performance outliers data-frame
##'
##' This is the description
##'
##' @param portfolio The portfolio id,
##' @param start The start date of the performance. Default is start of the current year.
##' @param factor The factor to sd()
##' @param session The rdecaf session.
##' @return A data-frame with outliers.
##' @import rdecaf
##' @export
performanceOutliers <- function(portfolio, start=NULL, factor=8, session) {

    if (is.null(start)) {
        ## Get the year start:
        start <- paste0(substr(Sys.Date(), 1, 4), "-01-01")

    }

    print(portfolio)

    ## Construct the parameters:
    params <- list("portfolios"=portfolio,
                   "start"=start)

    ## Get the performance data:
    performance <- try(getResource("performance", params=params, session=session), silent=TRUE)

    link <- paste0(gsub("api", "", session[["location"]]),
                   "dashboard/performance?portfolios=", portfolio,
                   "&start=", as.character(start))

    if (class(performance) == "try-error") {
        return(data.frame("Portfolio"=portfolio,
                          "Date"="ERROR",
                          "Return"="ERROR",
                          "Link to Performance"=link,
                          check.names=FALSE,
                          stringsAsFactors=FALSE))
    }

    ## Get the returns:
    returns <- unlist(performance[["returns"]][["data"]])

    ## Get the date index:
    index <- unlist(performance[["returns"]][["index"]])

    ## If no returns, return NULL:
    if (is.null(returns)) {
        return(NULL)
    }

    ## Make zoo:
    xtsReturns <- data.frame("returns"=xts::as.xts(returns, order.by=as.Date(index)))

    ## Get the outliersxs:
    outliers <- returnOutliers(xtsReturns, factor)

    ## Return outlier observations:
    if (any(outliers)) {
        return(data.frame("Portfolio"=portfolio,
                          "Date"=as.character(rownames(xtsReturns)[outliers]),
                          "Return"=round(as.numeric(xtsReturns[outliers,]), 4),
                          "Link to Performance"=link,
                          check.names=FALSE,
                          stringsAsFactors=FALSE))
    }

    ## Return:
    NULL
}


##' A function to send latest trades alert.
##'
##' This is the description
##'
##' @param session The rdecaf session.
##' @param accounts The data-frame with the rdecaf accounts.
##' @param resources The data-frame of the resources in the decaf instance.
##' @param emailParams The parameters for the email dispatch.
##' @param hours The number of hours to look back.
##' @param greeting The greetings string.
##' @param deployment The name of the deployment / client.
##' @param url The url of the deployment.
##' @param gte Greater than or equal to this time (HH:MM:SS) to run this alert.
##' @param lte Less than or equal to this time (HH:MM:SS) to run this alert.
##' @param tz The time-zone for gte and lte.
##' @return NULL. Email with the alert will be sent.
##' @export
alertLatestTrades <- function(session,
                              accounts,
                              resources,
                              emailParams,
                              hours=24,
                              greeting="",
                              deployment="",
                              url="",
                              gte="10:01:00",
                              lte="10:19:00",
                              tz="UTC") {

    ## Is it alert time?
    itsAlertTime <- itsTime(tz=tz, gte=gte, lte=lte)

    ## If it is not alert time, return NULL:
    if (!itsAlertTime) {
        return(NULL)
    }

    ## Prepare the params for the trades:
    params <- list("page_size"=-1, "format"="csv", "commitment__gte"=Sys.Date() - 90)

    ## Get the trades:
    trades <- as.data.frame(getResource("trades", params=params, session=session))

    ## Filter out the cash trades:
    trades <- trades[trades[, "resmain_ctype"] != "CCY", ]

    ## Filter the latest trades:
    trades <- trades[difftime(as.POSIXct(format(Sys.time(), tz="UTC")), as.POSIXct(trades[, "created"]), units="hours") < hours, ]

    ## If no trades, mask trade data frame:
    if (NROW(trades) == 0) {
        trades <- initDF(colnames(trades), 1)
    }

    ## Construct the trade links:
    tradeLink <- paste0(gsub("api", "", session[["location"]]), "trade/details/", trades[, "id"])

    ## Construct the report email data-frame:
    result <- data.frame("Trade Link"=tradeLink,
                         "Account"=trades[, "accmain_name"],
                         "Type"=resources[match(trades[, "resmain"], resources[, "id"]), "ctype"],
                         "Symbol"=ellipsify(trades[, "resmain_symbol"], 16),
                         "Name"=ellipsify(resources[match(trades[, "resmain"], resources[, "id"]), "name"], 25),
                         "Trade Date"=trades[, "commitment"],
                         "QTY"=beautify(as.character(round(as.numeric(trades[, "qtymain"]), 2)), nsmall=2),
                         "PX"=beautify(as.character(round(as.numeric(trades[, "pxmain"]), 2)), nsmall=2),
                         check.names=FALSE,
                         stringsAsFactors=FALSE)

    ## HTMLize the links:
    result[, "Trade Link"] <- paste0("<a href='", result[, "Trade Link"], "'>LINK</a>")

    ## Generate the outlier table as HTML and convert to string:
    result <- as.character(emailHTMLTable(result,
                                          provider="DECAF",
                                          caption=paste0("Trades Created In The Last ", hours, " Hours"),
                                          sourceType="API"))

    ## Clean the HTML text:
    result <- gsub("&#62;LINK&#60;/a&#62;", ">LINK<aya/a>", result)
    result <- gsub("&#60;a href", "<a href", result)

    .UPDATETEXT <- list("GREETINGPLACEHOLDER"=greeting,
                        "EMAILBODYPLACEHOLDER"="Your DECAF system has been updated with the latest data snapshots available ",
                        "CALLTOACTIONPLACEHOLDER"="Go to System",
                        "DEPLOYMENT"=deployment,
                        "URLPLACEHOLDER"=url,
                        "FINALPARAGRAPHPLACEHOLDER"="Please contact us if you experience any issues or have questions/feedback.",
                        "ADDRESSPLACEHOLDER"="",
                        "GOODBYEPLACEHOLDER"="Best Regards,<br>DECAF TEAM",
                        "ADDENDUMPLACEHOLDER"=result)

    syncUpdateEmail(template=readLines("../assets/update_email.html"),
                     updateText=.UPDATETEXT,
                     emailParams=emailParams,
                     subject=" DECAF Data Update: ")

}
