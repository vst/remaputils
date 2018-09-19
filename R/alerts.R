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
