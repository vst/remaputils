getAccountWiseTrades <- function(accounts, session) {

    trades <- list()

    ## Retrieve account-wise trades
    for (i in 1:length(accounts)) {

        ## Get the trades list:
        params <- list("accmain"=accounts[i], "page_size"=-1, "format"="csv")
        trds  <- as.data.frame(getResource("trades", params=params, session=session))

        ## If no trades, next:
        if (NROW(trds) == 0){
            next
        }

        ## Get the trades:
        trds[,c("commitment", "settlement")] <- as.character(trds[, c("commitment", "settlement")])

        ## Append the system trades:
        trades <- c(trades, list(trds))
    }

    ## Safely bind and return:
    safeRbind(trades)
}
