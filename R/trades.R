##' A function to sync trades in batches
##'
##' This is the description
##'
##' @param ctype The trade ctype.
##' @param portfolio The portfolio id.
##' @param gte Great than date.
##' @param session The rdecaf session.
##' @return A data frame with the trades for desired ctype.
##' @export
getTradeByCtypeForPortfolio <- function(ctype, portfolio, gte=NULL, session) {

    ## Construct the params:
    params <- list("format"="csv", "page_size"=-1, "ctype"=ctype, "accmain__portfolio"=portfolio, "commitment__gte"=gte)

    ## Get and return:
    as.data.frame(getResource("trades", params=params, session=session))
}


##' A function to sync trades in batches
##'
##' This is the description
##'
##' @param trades The trades data frame.
##' @param ccy The currency to convert valamt's to.
##' @param session The rdecaf session.
##' @return A data frame with the trades for desired ctype.
##' @export
getTXNsOfTrades <- function(trades, ccy=NULL, session) {

    ## If trades is empty, return NULL:
    if (NROW(trades) == 0) {
        return(NULL)
    }

    ## Get the transaction id's:
    txnIds <- na.omit(as.character(unlist(trades[,safeGrep(colnames(trades), "quant.") == "1"])))

    ## Construct the params:
    params <- list("page_size"=-1,
                   "format"="csv",
                   "id__in"=paste(txnIds, collapse=","))

    ## Get and return:
    quants <- as.data.frame(getResource("quants", params=params, session=session))

    ## If ccy is null, return quants:
    if (is.null(ccy)) {
        return(quants)
    }

    ## Append the fx pair for the conversion:
    quants[, "fxpair"] <- paste0(quants[, "valccy"], ccy)

    ## Get the unique fx pairs and the min and max dates:
    fxratesQ <- lapply(extractToList(quants, "fxpair"), function(x) data.frame("pair"=x[1, "fxpair"],
                                                                               "gte"=min(x[, "commitment"]),
                                                                               "lte"=max(x[, "commitment"]),
                                                                               stringsAsFactors=FALSE))


    ## Get the fx rates:
    fxrates <- lapply(fxratesQ, function(x) getOhlcObsForSymbol("session"=session,
                                                                "symbol"=x[, "pair"],
                                                                "lte"=x[, "lte"],
                                                                "lookBack"=x[, "lte"]-x[, "gte"]))


    ## Name the fx rates:
    names(fxrates) <- unique(quants[, "fxpair"])

    ## Iterate over quant rows and apppend converted valamt's:
    quants <- do.call(rbind, lapply(1:NROW(quants), function(i) {

        ## Get the current fx pair for row:
        cPair <- quants[i, "fxpair"]

        ## Get the corresponding fx rate for row:
        cRates <- fxrates[[cPair]]

        ## Initialise the value fo the nearesth date variable:
        vond <- list()

        ## If no fx rates for row, set vond value to 1, else get the value for closest date:
        if (NROW(cRates) == 0) {
            vond[["value"]] <- 1
        } else {
            ## Get the close of the nearest date:
            vond <- valueOfNearestDate(quants[i, "commitment"], cRates, tolerance=10, dateField="date", valueField="close", nameField=NULL)
        }

        ## Compute the net value amount in reference currency:
        quants[i, "valamt_refccy"] <- as.numeric(quants[i, "valamt"]) * vond[["value"]] * ifelse(as.numeric(quants[i, "quantity"]) < 0, -1, 1)

        ## Return the transaction/quant for row:
        quants[i, ]

    }))

    ## Done, return:
    quants

}


##' A function to sync trades in batches
##'
##' This is the description
##'
##' @param trades The trades data frame.
##' @param session The DECAF session info
##' @param batchSize The desired batch sizes. Default=150.
##' @return NULL
##' @import rdecaf
##' @export
syncTradesByBatch <- function(trades, session, batchSize=500) {

    ## Create the batches:
    batches <- createBatches(NROW(trades), batchSize)

    ## Interate over batches and push:
    for (i in 1:length(batches[[1]])) {

        ## Get the start index:
        start <- batches$startingIdx[[i]]

        ## Get the end index
        end <- batches$endingIdx[[i]]

        ## Get the payload:
        payload <- toJSON(list("actions"=trades[start:end,]), auto_unbox=TRUE, na="null", digits=10)

        print(paste0("Posting trades ", start, ":", end, " of ", NROW(trades)))

        ## Push and get response:
        response <- postResource("imports/inbulk", params=list(sync="True"), payload=payload, session = session)
    }

    ## Done, return:
    return(NULL)

}


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


##' TODO:
##' '
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


##' A function to get the non-performance related actions for a portfolio.
##'
##' This is the description
##'
##' @param portfolio A vector of portfolio id's
##' @param pxinfo The data frame with the pxinfo
##' @param date The asof date
##' @param ccy The desired ccy.
##' @param session The rdecaf session
##' @return A data frame
##' @export
getNPRTxns <- function(portfolio, pxinfo, date, ccy, session) {

    ## Get the trade ctype for the fund vs mandates:
    ctype <- ifelse(any(do.call(rbind, pxinfo)[, "isFund"]), "35", "30")

    ## Get the investment/transfer trades:
    invstm <- getTradeByCtypeForPortfolio(ctype, portfolio, gte=dateOfPeriod("Y-0", date), session)

    ## The the investments' quants:
    invstm <- getTXNsOfTrades(invstm, ccy, session)

    ## Filter by date:
    invstm[invstm[, "commitment"] <= date, "valamt_refccy"]

    ## Filter by type:
    invstm <- invstm[invstm[, "ctype"] == "45" | invstm[, "ctype"] == "30", ]

    ## Mask if non:
    if (is.null(invstm)) {
        invstm <- data.frame("valamt_refccy"=0)
    }

    ## Done, return:
    return(invstm)

}
