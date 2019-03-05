##' A function to get trades from session using container names:
##'
##' This is the description
##'
##' @param containerNames A vector with container names
##' @param session The DECAF session info
##' @param type The container type
##' @param gte A date object to express 'Greater Than' for commitment date of trades. Default is NULL.
##' @return A data-frame with DECAF trades for container.
##' @import rdecaf
##' @export
getTradesFromContainerNames <- function(containerNames, session, type, gte=NULL) {

    if (type == "accounts") {

        ## Construct the account params:
        params <- list("page_size"=-1,
                       "name__in"=paste(containerNames, collapse=","))

        ## Get the portfolios:
        container <- do.call(rbind, getResource(type, params=params, session=session))

        accounts <- container[, "id"]
    }

    if (type == "portfolios") {

        ## Construct the portfolio params:
        params <- list("page_size"=-1,
                       "format"="csv",
                       "name__in"=paste(containerNames, collapse=","))

        ## Get the portfolios:
        container <- as.data.frame(getResource(type, params=params, session=session))

        ## Get the account id:
        accounts <- as.numeric(na.omit(unique(unlist(container[, grep("accounts.", colnames(container))]))))
    }

    ## Get the account wise trades and return:
    list("trades"=getAccountWiseTrades(accounts, session, gte),
         "container"=container)

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


##' A function to get account-wise trades from a DECAF instance.
##'
##' This is the description
##'
##' @param ids A vector with container id's.
##' @param containerType The container type.
##' @param session The DECAF session info.
##' @param gte The date after which trades should be considered.
##' @return A data-frame with DECAF trades.
##' @import rdecaf
##' @export
getContainerWiseTrades <- function(ids, containerType="accounts", session, gte=NULL) {

    ## Initialise the trade list:
    trades <- list()

    if (containerType == "accounts") {
        params <- list("accmain"=NA,
                       "page_size"=-1,
                       "format"="csv",
                       "commitment__gte"=gte)}

    if (containerType == "portfolios") {
        params <- list("portfolio"=NA,
                       "page_size"=-1,
                       "format"="csv",
                       "commitment__gte"=gte)}

    ## Retrieve account-wise trades
    for (i in 1:length(ids)) {

        ## Get the trades list:
        params[[1]] <- ids[i]

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
