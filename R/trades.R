getAccountWiseTrades <- function(accounts, session) {

    trades <- list()

    ## Retrieve account-wise trades
    for (i in 1:length(accounts)) {

        ## Get the trades list:
        params <- list("accmain"=accounts[i], "page_size"=-1, "format"="csv")
        trds  <- as.data.frame(getResource("trades", params=params, session=session))

        ## If no trades, next:
        if (NROW(trds) == 0) {
            next
        }

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
