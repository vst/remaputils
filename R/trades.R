##' A function to get trades from session using portfolio names:
##'
##' This is the description
##'
##' @param portfolioNames A vector with portfolio names:
##' @param session The DECAF session info.
##' @return A data-frame with DECAF trades for portfolio.
##' @import rdecaf
##' @export
getTradesFromPortfolioNames <- function(portfolioNames, session) {

    ## Construct the portfolio params:
    params <- list("page_size"=-1,
                   "name__in"=paste(portfolioNames, collapse=","),
                   "format"="csv")

    ## Get the portfolios:
    portfolios <- as.data.frame(getResource("portfolios", params=params, session=session))

    ## Get the account id:
    accounts <- as.numeric(na.omit(unique(unlist(portfolios[, grep("accounts.", colnames(portfolios))]))))

    ## Get the account wise trades and return:
    list("trades"=getAccountWiseTrades(accounts, session),
         "portfolios"=portfolios)

}


##' A function to get account-wise trades from a DECAF instance.
##'
##' This is the description
##'
##' @param accounts A vector with account id's.
##' @param session The DECAF session info.
##' @param gte The date after which trades should be considered.
##' @return A data-frame with DECAF trades.
##' @import rdecaf
##' @export
getAccountWiseTrades <- function(accounts, session, gte=NULL) {

    ## Initialise the trade list:
    trades <- list()

    ## Retrieve account-wise trades
    for (i in 1:length(accounts)) {

        ## Get the trades list:
        params <- list("accmain"=accounts[i],
                       "page_size"=-1,
                       "format"="csv",
                       "commitment__gte"=gte)

        trds  <- as.data.frame(getResource("trades", params=params, session=session))

        ## If no trades, next:
        if (NROW(trds) == 0) {
            next
        }

        ## Do the nested ordering:
        trds <- nestedOrdering(trds, c("commitment", "executedat", "pseudorder", "created"))

        ## Change date formats to character:
        for (fld in c("commitment", "settlement", "created", "updated")) {
            ## Get the trades:
            trds[, fld] <- as.character(trds[, fld])
        }

        ## Append the system trades:
        trades <- c(trades, list(trds))
    }

    ## Safely bind and return:
    safeRbind(trades)
}


##' A function to detect outliers in xts return series:
##'
##' This is the description
##'
##' @param portfolio A vector of portfolio id's
##' @param session The rdecaf session
##' @param gte A date after which the trades should be considered
##' @param lte A date before which the trades should be considered. Default is set to gte.
##' @param nojournal Either "True" or "False" indicating whether journal entries shall be omitted.
##' @return A vector with string representing the decaf url
##' @export
getLinkToTradesByDate <- function(portfolio, session, gte, lte=NULL, nojournal="False") {

    ## If lte is null, set to gte:
    if (is.null(lte)) {
        lte <- as.character(gte)
    }

    ## Construct the link:
    paste0(gsub("api", "", session[["location"]]),
           "trade?accmain__portfolio=", portfolio,
           "&commitment__gte=", as.character(gte),
           "&commitment__lte=", as.character(lte),
           "&nojournal=", nojournal)

}
