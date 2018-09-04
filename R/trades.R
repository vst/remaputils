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
