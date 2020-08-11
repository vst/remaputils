##' This function gets the portfolio information give a portfolio id.
##'
##' This is the description
##'
##' @param portfolio The portfolio id.
##' @param session The rdecaf session.
##' @return A data frame with the portfolio details.
##' @export
getPortfolio <- function(portfolio, session) {
    ## Set the parameters:
    params <- list("id"=portfolio,
                   "format"="csv",
                   "page_size"=-1)

    as.data.frame(getResource("portfolios", params=params, session=session))
}


##' This function gets the investments for a portfolio.
##'
##' This is the description
##'
##' @param portfolio The portfolio id.
##' @param session The rdecaf session.
##' @return A data frame with the portfolio's investment trades.
##' @export
getInvestments <- function(portfolio, session) {
    ## Set the parameters:
    params <- list("accmain__portfolio"=portfolio,
                   "ctype"="35",
                   "format"="csv",
                   "page_size"=-1)

    as.data.frame(getResource("trades", params=params, session=session))

}


##' This function gets the external valuations for a portfolio.
##'
##' This is the description
##'
##' @param portfolio The portfolio id.
##' @param session The rdecaf session.
##' @return A data frame with the portfolio's external valuations.
##' @export
getExternalValuation <- function(portfolio, session) {
    params <- list("page_size"=-1, "format"="csv", "portfolio"=portfolio)
    data.frame(getResource("externalvaluations", params=params, session=session))
}


##' This function gets the pconsolidations for a portfolio.
##'
##' This is the description
##'
##' @param portfolio The portfolio id.
##' @param session The rdecaf session.
##' @return A list with the pconsolidations for the portfolio.
##' @export
getPconsolidation <- function(portfolio, session) {
    ## Set the parameters:
    params <- list("portfolio"=portfolio,
                   "shareclass__isnull"="False",
                   "page_size"=-1)

    ## Get the report:
    report <- getResource("pconsolidations", params=params, session=session)

}

##' This function gets the partial journal entries for a specific account id.
##'
##' This is the description
##'
##' @param id The account id.
##' @param session The rdecaf session.
##' @return A data frame with the partial journal entries.
##' @export
getPJournalsByAccount <- function(id, session) {
    ## Set the parameters:
    params <- list("accmain"=id,
                   "ctype"="300",
                   "format"="csv",
                   "page_size"=-1)

    as.data.frame(getResource("trades", params=params, session=session))

}


##' This function executes a job give a portfolio id and pools the result until finished.
##'
##' This is the description
##'
##' @param id The portfolio id.
##' @param session The rdecaf session.
##' @param maxWait The maximum waiting time in seconds.
##' @return NULL
##' @export
executeValuationAndPool <- function(id, session, maxWait=900) {

    ## Post the horses job for portfolio:
    job <- getResource(paste0("jobs/valuations/portfolios/", id), session=session)

    ## Assign the state of the job:
    state <- job[["state"]]

    ## Get the job id:
    jobID <- job[["id"]]

    ## Total time waited for horses:
    timeWaited <- 0

    ## If state still Pending, wait another 3 seconds:
    while (state == "PENDING" & timeWaited < maxWait) {

        Sys.sleep(3)

        job <- getResource(paste0("jobs/", jobID), session=session)

        state <- job[["state"]]
        timeWaited <- timeWaited + 3

        print(paste0("Elapsed time: ", timeWaited, " secs. Still Waiting ..."))

    }
}


##' This function creates a ficticious PnL account and settles the Future pnl T-1
##'
##' This is the description
##'
##' @param futurePositions The openPositions data frame coming from getOpenpositionbyctype.
##' @param accounts The accounts data frame from rdecaf.
##' @param resources The resources data frame.
##' @param session The rdecaf session.
##' @return NULL
##' @export
settleFuturePnL <- function(futurePositions, accounts, resources, session) {

    ## Apply:
    apply(futurePositions, MARGIN=1, function(row) {

        ## Is this closed?
        close <- !is.na(row["close"])

        ## if not closed, set close to Sys.Date()
        if (is.na(row["close"])) {
            row["close"] <- Sys.Date()
        }

        ## Get the dates for which we need to know the PnL:
        pnlDates <- seq(as.Date(row["open"]), as.Date(row["close"]) -1, 1)

        ## Get the dates to which we need to post the pnls:
        conDates <- seq(as.Date(row["open"]) + 1, as.Date(row["close"]), 1)

        ## Get the closing date of the
        clsDates <- row["close"]

        ## Has account:
        row["pnlAccId"] <- accounts[match(row["pnlAcct"], accounts[, "name"]), "id"]

        if (is.na(row["pnlAccId"])) {
            ## Create the accounts:
            payload <- toJSON(list("name"=row["pnlAcct"],
                                   "rccy"=row["secccy"],
                                   "portfolio"=trimws(row["portfolio"]),
                                   "custodian"="2",
                                   "atype"=NA), auto_unbox=TRUE)

            row["pnlAccId"] <- postResource("accounts", payload=payload, session=session)[["id"]]
            accounts <- as.data.frame(getResource("accounts", params=list("page_size"=-1, "format"="csv"), session=session))
        }


        for (i in 1:length(pnlDates)) {

            params <- list(c="portfolio", i=trimws(row["portfolio"]), ccy=row["rccy"], date=as.character(pnlDates[i]))
            consolidation <- getResource("consolidation", params=params, session=session)
            holdings <- consolidation[["holdings"]]
            pnls <- data.frame("pnl"=holdings[[match(row["resmain"], sapply(holdings, function(x) x[["artifact"]][["id"]]))]][["valuation"]][["value"]][["net"]][["org"]],
                               "pnlDate"=pnlDates[i])

            params <- list(c="account", i=trimws(row["pnlAccId"]), ccy=row["secccy"], date=as.character(conDates[i]))
            consolidation <- getResource("consolidation", params=params, session=session)
            cons <- data.frame("balance"=consolidation[["nav"]],
                               "balanceDate"=conDates[i])

            difference <- (-pnls[, "pnl"]) - cons[, "balance"]

            if (abs(difference) > 0.1) {

                if (close & row["close"] == conDates[i]) {
                    next
                }

                payload <- toJSON(list("accmain"=row["pnlAccId"],
                                       "commitment"=conDates[i],
                                       "id"=NA,
                                       "resmain"=resources[match(row["secccy"], resources[, "symbol"]), "id"],
                                       "ctype"="20",
                                       "notes"="Daily Future PnL Settlement",
                                       "qtymain"=-pnls[,"pnl"] - cons[,"balance"],
                                       "pxmain"=1), auto_unbox=TRUE)
                response <- postResource("trades", payload=payload, session=session)
            }
        }

        if (close & abs(cons[, "balance"]) > 0.1) {
            params <- list(c="account", i=trimws(row["pnlAccId"]), ccy=row["secccy"], date=as.character(row["close"]))
            consolidation <- getResource("consolidation", params=params, session=session)
            cons <- data.frame("balance"=consolidation[["nav"]],
                               "balanceDate"=conDates[i])

            payload <- toJSON(list("accmain"=row["pnlAccId"],
                                   "commitment"=row["close"],
                                   "id"=NA,
                                   "resmain"=resources[match(row["secccy"], resources[, "symbol"]), "id"],
                                   "ctype"="20",
                                   "notes"="Closing of Future Position: PnL Account Reset",
                                   "qtymain"=-cons[, "balance"],
                                   "pxmain"=1), auto_unbox=TRUE)
            response <- postResource("trades", payload=payload, session=session)
        }

    })
}


##' Delete resources by identifier, either symbol, isin or id.
##'
##' This is the description
##'
##' @param identifier A vector with identifier
##' @param idType The type of identifier, either symbol, isin or id.
##' @param resources The resources data frame.
##' @param excludeCols A vector with resource columns to apply a filter
##' @param excludeVals A vector with values to filter out from resource column.
##' @param session The rdecaf session.
##' @return NULL
##' @export
deleteResourcesByIdentifier <- function(identifier, idType, resources, excludeCols=NULL, excludeVals=NULL, session) {

    ## First, get the resources to be deleted:
    deletable <- resources[match(identifier, resources[, idType]), ]

    ## Iterate over resource columns to be excluded from consideration:
    if (!is.null(excludeCols)) {
        for (i in 1:length(excludeCols)) {
            ## Exclude
            deletable <- deletable[deletable[, excludeCols[i]] != excludeVals[i], ]
        }
    }

    ## Iterate and delete:
    for (id in deletable[, "id"]) {
        print(paste0("Deleting resource: ", id))
        response <- deleteResource("resources", payload=list(id), session=session)
    }
}


##' For trade identity candidates, infer matches
##'
##' This is the description
##'
##' @param tradeIdentities A list with the trade identity candidates.
##' @return A list with matched trade identities.
##' @export
matchTradeIdentity <- function(tradeIdentities) {

    ## Initialise the return value:
    retval <- list()

    ## Iterate over trade identities:
    for (row in 1:length(tradeIdentities)) {

        ## Get the trade identity:
        trdIdty <- tradeIdentities[[row]]

        ## If trdIdty does not have the corresponding dTrade, return:
        if (is.null(trdIdty[["dTrade"]])) {
            retval[[row]] <- trdIdty
            next
        }

        ## Match the signs:
        sgnMatch <- sign(as.numeric(trdIdty[["iTrade"]][, "qtymain"])) == sign(as.numeric(trdIdty[["dTrade"]][, "qtymain"]))

        ## Match the numbers:
        qtyMatch <- sapply(as.numeric(trdIdty[["dTrade"]][, "qtymain"]), function(q) numberEquivalency(as.numeric(trdIdty[["iTrade"]][, "qtymain"]), q))

        ## If no match, return NULL
        if (!(sgnMatch & qtyMatch)) {
            retval[[row]] <- list("iTrade"=trdIdty[["iTrade"]], "dTrade"=NULL)
            next
        }

        ## If match, persist:
        retval[[row]] <- trdIdty

    }

    ## Done, return:
    retval

}


##' For a incoming trade record, this function identifies corresponding candidate trades in decaf.
##'
##' This is the description
##'
##' @param records A data frame with incoming trade records. Requires columns 'resmain', 'commitment' and 'settlement'
##' @param accmain The account id in decaf
##' @param session The rdecaf session info
##' @return A list of length NROW(records).
##' @export
getTradeIdentity <- function (records, accmain, session) {

    ## Define the account params:
    params <- list("id"=accmain, "page_size"=-1, "format"="csv")

    ## Get the account:
    account <- as.data.frame(getResource("accounts", params=params, session=session))

    ## Get the container trades:
    trades <- getTradesFromContainerNames(account[, "name"], session, "accounts")[["trades"]]

    ## Initialise the return value:
    retval <- list()

    ## Iterate over the incoming trade records:
    for (row in 1:NROW(records)) {

        ## Get the incoming trade record:
        iTrade <- records[row, ]

        ## Filter the system trades by the incoming trade record dates:
        dTrade <- trades[iTrade[, "commitment"] <= trades[, "commitment"] &
                         iTrade[, "settlement"] >= trades[, "commitment"], ]

        ## Match the resmains and filter:
        dTrade <- dTrade[match(iTrade[, "resmain"], dTrade[, "resmain"], incomparables=NA), ]

        ## If there is no match, append empty match and next:
        if (NROW(dTrade) == 1 & all(is.na(dTrade[1, ]))) {
            retval[[row]] <- list("iTrade"=iTrade, "dTrade"=NULL)
            next
        }

        ## If there are candidates, append to incoming trade record:
        retval[[row]] <- list("iTrade"=iTrade, "dTrade"=dTrade)

    }

    ## Done, return:
    retval

}


##' This function syncs portfolios from a data frame and appends the name and id's to the data frame
##'
##' This is the description
##'
##' @param data The data frame. Required columns: "portfolio" ("name of portoflio"),
##' @param session The rdecaf session
##' @param prefix Optional string to be used as prefix for portfolio name:
##' @return Returns the appended data frame.
##' @export
appendPortfolioIDs <- function(data, session, prefix="") {

    ## Get the portfolios:
    portfolios <- as.data.frame(getResource("portfolios", params=list("format"="csv", "page_size"=-1), session=session))

    ## If not portfolios yet, mask:
    if (NROW(portfolios) == 0) {
        portfolios <- as.data.frame(matrix(NA, 1, 2))
        colnames(portfolios) <- c("name", "id")
    }

    ## Grep the portoflios name:
    pNameMatch <- mgrep(portfolios[, "name"], data[, "portfolio"])

    ## Check if portfolio exists:
    hasIt <- do.call(c, lapply(1:NCOL(pNameMatch), function(i) {
        hasIt <- which(pNameMatch[, i] != "0")
        if (length(hasIt) == 0) {
            return(NA)
        }
        portfolios[hasIt, "guid"]

    }))

    ## Assign the portfolio names to the existence boolean:
    names(hasIt) <- colnames(pNameMatch)

    ## For the exsiting portoflios, assign the guid:
    data[, "portfolio_guid"] <- hasIt[match(data[, "portfolio"], names(hasIt))]

    ## For the exsiting portoflios, assign the name:
    data[, "portfolio_name"] <- portfolios[match(data[, "portfolio_guid"], portfolios[, "guid"]), "name"]

    ## For the exsiting portoflios, assign the id:
    data[, "portfolio_id"] <- portfolios[match(data[, "portfolio_guid"], portfolios[, "guid"]), "id"]

    ## Create if portfolio doesn't exsit:
    if (any(is.na(hasIt))) {

        ## Get the NA portfolios:
        naHasIt <- is.na(hasIt)

        ## Inbulk the NA portfolios:
        resPort <- inbulkPortfolio(name=paste0(prefix, data[naHasIt, "portfolio"]),
                                   rccy=data[naHasIt,"refCCY"],
                                   team="1",
                                   guidPrefix=prefix,
                                   xguid=data[naHasIt, "portfolio"],
                                   session=session)

        ## For the new portoflios, assign the name:
        data[naHasIt, "portfolio_name"] <- resPort[["name"]]

        ## For the new portoflios, assign the id:
        data[naHasIt, "portfolio_id"] <- resPort[["id"]]

        ## For the new portoflios, assign the guid:
        data[naHasIt, "portfolio_guid"] <- resPort[["guid"]]

    }

    ## Done, return:
    return(data)

}


##' This function creates new accounts if missing and appends masked ficticious records
##'
##' This is the description
##'
##' @param data The data frame. Required columns: "account" ("name of account"), "portfolio_id",
##' @param guidInst The guid for the institution.
##' @param session The rdecaf session
##' @param prefix Optional string to be used as prefix for account name:
##' @return Returns the appended data frame.
##' @export
appendAccountIDs <- function(data, guidInst, session, prefix) {

    ## Inbulk the accounts:
    result <- inbulkAccount(name=paste0(prefix, data[, "account"]),
                            portfolio=paste0("dcf:portfolio?guid=", data[["portfolio_guid"]]),
                            custodian=paste0("dcf:institution?guid=", guidInst),
                            guidPrefix=prefix,
                            xguid=data[, "account"],
                            session=session)

    ## Assign the account name:
    data[, "account_name"] <- result[["name"]]

    ## Assign the account id:
    data[, "account_id"] <- result[["id"]]

    ## Assign the accmain:
    data[, "accmain"] <- paste0("dcf:account?guid=", result[["guid"]])

    ## Get the portfolio-wise data:
    pWiseData <- lapply(unique(data[, "portfolio_id"]), function(p) data[data[, "portfolio_id"] == p,])

    ## Get the system accounts:
    systemAccounts <- as.data.frame(getResource("accounts", params=list("format"="csv", "page_size"=-1), session=session))

    ## Initialise new data:
    newData <- NULL

    ## Iterate over portfolios:
    for (i in 1:length(pWiseData)) {

        ## Get the portfolio data:
        pWise <- pWiseData[[i]]

        ## Get the account id's of matching accounts:
        pAccs <- systemAccounts[systemAccounts[, "portfolio"] == pWise[1, "portfolio_id"], "id"]

        ## Get the match the account ids:
        matchIdx <- match(pAccs, pWise[, "account_id"])

        ## If no match, create account:
        if (any(is.na(matchIdx))) {

            ## Get the number of missing accounts:
            noOfMissingAccs <- sum(is.na(matchIdx))

            ## Create dataframe with as many rows as missing accounts:
            append <- do.call(rbind, lapply(1:noOfMissingAccs, function(x) rep(NA, NCOL(pWise))))

            ## Name the columns of the missing accounts:
            colnames(append) <- colnames(pWise)

            ## Append the rows to the portfolio data:
            pWise <- rbind(pWise, append)

            ## Compute the sequence of new row numbers:
            newRows <- (NROW(pWise) - noOfMissingAccs + 1):(NROW(pWise))

            ## Mask ficitcious values:
            pWise[newRows, "account_id"] <- pAccs[is.na(matchIdx)]
            pWise[newRows, "portfolio"]  <- pWise[1, "portfolio"]
            pWise[newRows, "portfolio_name"]  <- pWise[1, "portfolio_name"]
            pWise[newRows, "portfolio_guid"]  <- pWise[1, "portfolio_guid"]
            pWise[newRows, "portfolio_id"]  <- pWise[1, "portfolio_id"]
            pWise[newRows, "posamnt"]  <- 0
            pWise[newRows, "ccymain"]  <- "EUR"
            pWise[newRows, "id"]  <- "EUR"
            pWise[newRows, "type"]  <- "Cash"
            pWise[newRows, "positionDate"]  <- pWise[1, "positionDate"]
            pWise[newRows, "accmain"] <- paste0("dcf:account?guid=", systemAccounts[match(pAccs[is.na(matchIdx)], systemAccounts[, "id"]), "guid"])
            pWise[newRows, "account_name"] <- systemAccounts[match(pAccs[is.na(matchIdx)], systemAccounts[, "id"]), "name"]
        }

        ## Append to new data:
        newData <- rbind(newData, pWise)
    }

    ## Done, return:
    return(newData)

}


##' This function mimicks the PX Last methodology of DECAF:
##'
##' This is the description
##'
##' @param resmain The resmain id in DECAF:
##' @param resources The rdecaf resources:
##' @param session The rdecaf session
##' @param date The date of interest.
##' @return Returns the last available price for the resmains
##' @export
getPXLast <- function(resmain, resources, session, date) {

    ## Get the symbol of resmain:
    symbols <- resources[match(resmain, resources[, "id"]), "symbol"]

    ## Iterate over symbols:
    ohlc <- do.call(rbind, lapply(1:length(symbols), function(i) {

        ## Try to get the ohlc observations:
        result <- head(getOhlcObsForSymbol(session, symbols[i], lte=date, lookBack=2000), 1)

        ## If ohlc successful, return result:
        if (NROW(result) > 0) {
            return(result)
        }

        ## Try the trade of resmain:
        params <- list("resmain"=resmain[i],
                       "page_size"=-1,
                       "format"="csv")

        ## Get the trades of resmain:
        trds  <- as.data.frame(getResource("trades", params=params, session=session))

        ## Get trades which are equal or less than date:
        trds <- trds[trds[, "commitment"] <= date, ]

        ## Map and return:
        return(data.frame("id"=NA,
                          "symbol"=symbols[i],
                          "date"=trds[which(trds[, "commitment"] == min(trds[, "commitment"]))[1], "commitment"],
                          "open"=NA,
                          "high"=NA,
                          "low"=NA,
                          "close"=trds[which(trds[, "commitment"] == min(trds[, "commitment"]))[1], "pxmain"],
                          stringsAsFactors=FALSE))
    }))

    ## Done, return:
    return(ohlc)

}


##' This function prepares the position quantities of external vis-a-vis decaf stocks:
##'
##' This is the description
##'
##' @param data The data with the external positions. Expected columns: resmain, qtymain, pxmain, accmain
##' @param session the rdecaf session
##' @return Returns a data frame with the respective quantities:
##' @export
positionDifferential <- function(data, session) {

    ## Get the stocks:
    stocks <- as.data.frame(getResource("stocks", params=list("page_size"=-1,
                                                              "format"="csv",
                                                              "date"=data[1, "pDate"],
                                                              "zero"=0), session=session))

    ## If no stocks, mask:
    if (NROW(stocks) == 0) {
        stocks <- data.frame("account"=NA, "artifact"=NA, "quantity"=NA)
    }

    ## Get the account wise positions:
    aWisePositions <- lapply(unique(data[, "accmain"]), function(acc) data[data[, "accmain"] == acc,])

    ## Iterate over account positions:
    stockdiff <- do.call(rbind, lapply(1:length(aWisePositions), function(i) {

        ## Get the account's positions:
        aPos <- aWisePositions[[i]]

        ## If NA rows, omit:
        aPos <- aPos[apply(aPos, MARGIN=1, function(x) !all(is.na(x))),]

        ## Get the stocks for such account:
        aStocks <- stocks[!is.na(match(stocks[, "account"], aPos[, "accmain"])), ]

        ## If there is no stock, return:
        if (NROW(aStocks) == 0) {
            return(data.frame("resmain"=aPos[, "resmain"],
                              "qtyint"=0,
                              "qtyext"=aPos[, "qtymain"],
                              "pxmain"=aPos[, "pxmain"],
                              "accmain"=aPos[, "accmain"],
                              stringsAsFactors=FALSE))
        }

        ## Initialise the retval:
        retval <- as.data.frame(matrix(NA, length(unique(c(aStocks[, "artifact"], aPos[, "resmain"]))), 5), stringsAsFactors=FALSE)
        colnames(retval) <- c("resmain", "qtyint", "qtyext", "pxmain", "accmain")

        ## Plug the unique resmains:
        retval[, "resmain"] <- unique(c(aStocks[, "artifact"], aPos[, "resmain"]))

        ## Match and assign the internal quantity:
        retval[, "qtyint"] <- aStocks[match(retval[, "resmain"], aStocks[, "artifact"]), "quantity"]

        ## Match and assign the external quantity:
        retval[, "qtyext"] <- aPos[match(retval[, "resmain"], aPos[, "resmain"]), "qtymain"]

        ## Assing the pxmain:
        retval[, "pxmain"] <- aPos[match(retval[, "resmain"], aPos[, "resmain"]), "pxmain"]

        retval[, "accmain"] <- aPos[1, "accmain"]

        ## NA qtyint to 0:
        retval[is.na(retval[, "qtyint"]), "qtyint"] <- 0

        ## NA qtyext to 0:
        retval[is.na(retval[, "qtyext"]), "qtyext"] <- 0

        ## Done, return:
        return(retval)

    }))

    ## Done, return:
    return(stockdiff)
}


##' A function to push ohlc observations
##'
##' This is the description
##'
##' @param symbol The vector of symbols.
##' @param close The vector of close values.
##' @param date The vector of dates.
##' @param session The rdecaf session.
##' @return Returns NULL
##' @export
pushOhlc <- function(symbol, close, date, session) {

    ## Construct the data-frame:
    ohlcObs <- data.frame("symbol"=symbol,
                          "close"=close,
                          "date"=date,
                          stringsAsFactors=FALSE)

    ## Remove NA symbols:
    ohlcObs <- ohlcObs[!is.na(ohlcObs[, "symbol"]), ]

    ## Create batches:
    batches <- createBatches(NROW(ohlcObs), 500)

    ## Iterate over batches and push:
    for (i in 1:length(batches[[1]])) {

        ## The starting index:
        strt <- batches[["startingIdx"]][i]

        ## The ending index:
        ends <- batches[["endingIdx"]][i]

        ## Get the payload:
        payload <- toJSON(ohlcObs[strt:ends,], auto_unbox=TRUE, na = c("null"))

        print(paste0("Posting prices ", strt, ":", ends, " of ", NROW(ohlcObs)))

        ## Push:
        result <- httr::POST(paste0(session[["location"]], "/ohlcobservations/updatebulk/"),
                             httr::authenticate(session[["username"]], session[["password"]]),
                             body=payload,
                             httr::add_headers(.headers = c("Content-Type"="application/json")))
    }

}


##' A function to inbulk trades.
##'
##' This is the description
##'
##' @param data The data frame with trade records.
##' @param guid The vector with guids. If NULL (default), creates using guidPrefix and xguid.
##' @param guidPrefix A string to use as prefix (e.g Custodian Name)
##' @param xguid A vector with the id to be transformed to guid.
##' @param session The rdecaf session
##' @return Returns list with inbulk results.
##' @export
inbulkTrades <- function (data, guid = NULL, guidPrefix = NULL, xguid = NULL, session) {

    ## If guid is null, construct one:
    if (is.null(guid)) {
        data[, "guid"] <- as.character(sapply(paste0(guidPrefix, xguid), function(id) paste0("~XID.trade.", digest::digest(id))))
    } else {
        data[, "guid"] <- guid
    }

    ## Create the payload:x
    payload <- toJSON(list(actions = data), auto_unbox = TRUE, na = "null")

    ## Sync trades:
    response <- postResource("imports/inbulk", params = list(sync = "True"),payload = payload, session = session)

    ## Done, return:
    list(response=response,
         guid=data[,"guid"],
         name=NULL,
         id = sapply(response[[1]][["actions"]], function(x) x[[1]]))
}


##' A function to inbulk portfolios.
##'
##' This is the description
##'
##' @param guid The vector with guids. If NULL (default), creates using guidPrefix and xguid.
##' @param name The vector with names.
##' @param rccy The vector of currencies.
##' @param team The vector of teams id's.
##' @param guidPrefix A string to use as prefix (e.g Custodian Name)
##' @param xguid A vector with the id to be transformed to guid.
##' @param session The rdecaf session
##' @return Returns NULL
##' @export
inbulkPortfolio <- function (guid=NULL, name, rccy, team, guidPrefix=NULL, xguid=NULL, session) {

    ## If guid is null, construct one:
    if (is.null(guid)) {
        guid <- as.character(sapply(paste0(guidPrefix, xguid), function(id) paste0("~XID.portfolio.", digest::digest(id))))
    }

    ## Create the payload:
    payload <- jsonlite::toJSON(list("portfolios"=data.frame("guid"=guid,
                                                             "name"=name,
                                                             "rccy"=rccy,
                                                             "team"=team)), auto_unbox=TRUE, na="null")
    ## Sync portfolios:
    response <- rdecaf::postResource("imports/inbulk", params=list(sync="True"), payload=payload, session = session)

    ## Done, return:
    list("response"=response,
         "guid"=guid,
         "name"=name,
         "id"=sapply(response[[1]][["portfolios"]], function(x) x[[1]]))

}


##' A function to inbulk accounts.
##'
##' This is the description
##'
##' @param guid The vector with guids. If NULL (default), creates using guidPrefix and xguid.
##' @param name The vector with names.
##' @param portfolio The vector with portfolio id's.
##' @param custodian The vector with custodian id's.
##' @param guidPrefix A string to use as prefix (e.g Custodian Name)
##' @param xguid A vector with the id to be transformed to guid.
##' @param session The rdecaf session
##' @return Returns NULL
##' @export
inbulkAccount <- function (guid=NULL, name, portfolio, custodian, guidPrefix=NULL, xguid=NULL, session) {

    ## If guid is null, construct one:
    if (is.null(guid)) {
        guid <- as.character(sapply(paste0(guidPrefix, xguid), function(id) paste0("~XID.account.", digest::digest(id))))
    }

    ## Create the payload:
    payload <- toJSON(list("accounts"=data.frame("guid"=guid,
                                                 "name"=name,
                                                 "portfolio"=portfolio,
                                                 "custodian"=custodian)), auto_unbox=TRUE, na="null")
    ## Sync portfolios:
    response <- postResource("imports/inbulk", params=list(sync="True"), payload=payload, session = session)

    ## Done, return:
    list("response"=response,
         "guid"=guid,
         "name"=name,
         "id"=sapply(response[[1]][["accounts"]], function(x) x[[1]]))

}


##' A function to inbulk accounts.
##'
##' This is the description
##'
##' @param guid The vector with guids. If NULL (default), created inside.
##' @param resmain TODO.
##' @param accmain TODO.
##' @param agent TODO.
##' @param commitment TODO.
##' @param pxnavs TODO.
##' @param shrcnt TODO.
##' @param qtymain TODO.
##' @param shrcls TODO.
##' @param notes TODO.
##' @param session The rdecaf session
##' @return A list with results.
##' @export
inbulkInvestment <- function (guid=NULL,
                              resmain,
                              accmain,
                              agent,
                              commitment,
                              pxnavs,
                              shrcnt,
                              qtymain,
                              shrcls,
                              notes=NA,
                              session) {

    ## Get the data frame:
    invst <- data.frame("id"=NA,
                        "ctype"=35,
                        "resmain"=resmain,
                        "accmain"=accmain,
                        "agent"=agent,
                        "shrcnt"=shrcnt,
                        "commitment"=commitment,
                        "pxnavs"=pxnavs,
                        "qtymain"=qtymain,
                        "notes"=notes,
                        "shrcls"=shrcls)

    ## If no guid provided, create one:
    if (is.null(guid)) {
        invst[, "guid"] <- apply(invst, MARGIN=1, function(x) digest::digest(paste0(x, collapse="")))
    } else {
        invst[, "guid"] <- guid
    }

    invst <- data.frame(invst, "shrcnt"=shrcnt)

    ## Create the payload:
    payload <- toJSON(list("actions"=invst), auto_unbox=TRUE, na="null")

    ## Sync investments:
    response <- postResource("imports/inbulk", params=list(sync="True"), payload=payload, session = session)

    ## Done, return:
    list("response"=response,
         "guid"=invst["guid"],
         "name"=shrcls,
         "commitment"=commitment,
         "id"=sapply(response[[1]][["actions"]], function(x) x[[1]]))

}


##' A function returns the full journal entry in the system which matches the record most.
##'
##' This is the description
##'
##' @param qtymain The quantity of the record.
##' @param commitment The date of the record.
##' @param portfolio The portfolio to be considered.
##' @param keyword Optional. The keyword in FJE notes to be considered.
##' @param session The rdecaf session
##' @return A list with results.
##' @export
matchFJE <- function(qtymain, commitment, portfolio, keyword=NA, session) {

    ## Prepare FJE params:
    params <- list("format"="csv", "page_size"=-1, "accmain__portfolio"=portfolio, "ctype"=301)

    ## Get the full journal entries for the portfolio:
    fje <- as.data.frame(getResource("trades", params=params, session=session))

    ## Filter out keywords from notes if required:
    fje <- fje[grep(keyword, fje[, "notes"]), ]

    ## Short circuit:
    if (NROW(fje) == 0) {
        return(NA)
    }

    ## Filter out by commitment:
    fje <- fje[as.Date(commitment) - as.Date(fje[, "commitment"]) < 14, ]

    ## Short circuit:
    if (NROW(fje) == 0) {
        return(NA)
    }

    ## Get the quantity distance:
    dist <- abs(fje[, "qtymain"] - as.numeric(qtymain))

    ## Done, return:
    return("fje"=fje[min(dist) == dist, ])

}


##' A function returns the full journal entry in the system which matches the record most.
##'
##' This is the description
##'
##' @param fje The full journal entry to be offset.
##' @param commitment The date of the record.
##' @param notes The text for the notes. Default: AUTO-GENERATED FJE OFFSET
##' @param reference The text for the reference. Default: AUTO-GENERATED FJE OFFSET
##' @param session The rdecaf session
##' @return A list with results.
##' @export
offsetFJE <- function(fje, commitment, notes="AUTO-GENERATED FJE OFFSET", reference="AUTO-GENERATED FJE OFFSET", session) {

    ## If fje NA, return NA:
    if (is.na(fje)) {
        return(NA)
    }

    ## Prepare FJE data frame:
    offset <- data.frame("id"=NA,
                         "ctype"=301,
                         "atype"=NA,
                         "accmain"=fje[, "accmain"],
                         "pxmain"=1,
                         "pxcost"=1,
                         "resmain"=fje[, "resmain"],
                         "qtymain"=round(fje[, "qtymain"], 8) * -1,
                         "accaltn"=fje[, "accaltn"],
                         "pxaltn"=1,
                         "reference"=reference,
                         "resaltn"=fje[, "resaltn"],
                         "qtyaltn"=round(fje[, "qtyaltn"], 8) * -1,
                         "notes"=notes,
                         "commitment"=commitment)

    offset[, "guid"] <- apply(offset, MARGIN=1, function(x) digest::digest(paste0(x, collapse="")))

    ## Create the payload:
    payload <- toJSON(list("actions"=offset), auto_unbox=TRUE, na="null")

    ## Sync investments:
    response <- postResource("imports/inbulk", params=list(sync="True"), payload=payload, session = session)

    ## Done, return:
    list("response"=response,
         "guid"=offset["guid"],
         "commitment"=commitment,
         "id"=sapply(response[[1]][["actions"]], function(x) x[[1]]))
}


##' A function to post resources in batches.
##'
##' This is the description
##'
##' @param resources The resource data frame.
##' @param batchSize The size of the batches.
##' @param session The DECAF session info.
##' @return Returns NULL
##' @export
postResourcesByBatch <- function(resources, batchSize=1000, session) {

    ## Initialise the ending index:
    endingIdx <- seq(0, NROW(resources), batchSize)

    ## Create the starting index:
    startingIdx <- endingIdx + 1

    ## Update the ending index:
    endingIdx <- c(tail(endingIdx, -1), NROW(resources) - tail(endingIdx, 1) + tail(endingIdx, 1))

    ## Push the resources:
    for (i in 1:length(endingIdx)) {

        ## Batch:
        batch <- resources[startingIdx[i]:endingIdx[i], ]

        print(paste0("Posting batch: ", startingIdx[i], ":", endingIdx[i]))

        ## Push the resources:
        result <- rdecaf::postResource("resources", "imports", payload=jsonlite::toJSON(batch, auto_unbox=TRUE), session=session)

    }

    return(NULL)

}


##' A function to get trades from session using container names:
##'
##' This is the description
##'
##' @param containerNames A vector with container names:
##' @param containerType The container type
##' @param session The DECAF session info.
##' @return A data-frame with DECAF trades for portfolio.
##' @import rdecaf
##' @export
getContainer <- function(containerNames, containerType, session) {

    ## If no container names, return NULL:
    if (NROW(containerNames) == 0) {
        return(NULL)
    }

    ## If container type is accounts, run:
    if (containerType == "accounts") {

        ## Construct the account params:
        params <- list("page_size"=-1,
                       "name__in"=paste(containerNames, collapse=","))

        ## Get the portfolios:
        container <- safeRbind(getResource(containerType, params=params, session=session))

    }

    ## If container type is portfolios, run:
    if (containerType == "portfolios") {

        ## Construct the portfolio params:
        params <- list("page_size"=-1,
                       "format"="csv",
                       "name__in"=paste(containerNames, collapse=","))

        ## Get the portfolios:
        container <- as.data.frame(getResource(containerType, params=params, session=session))

    }

    ## Done, return:
    container

}


##' A function to provide the enriched trade data-frame from source session.
##'
##' This is the description
##'
##' @param sourceSession The rdecaf session for source instance.
##' @param targetSession The rdecaf session for target instance.
##' @param containerMap TODO:
##' @param containerType TODO:
##' @param gte The commitment date after which the trades should be considered. Default is NULL.
##' @return A trade data-frame with mapped/created accmains and auxiliary resource information.
##' @export
xDecafPreemble <- function(sourceSession, targetSession, containerMap, containerType, gte=NULL) {

    ## Print message:
    print(paste0("Retrieving trades by container names from source for ", containerType, " ..."))

    ## Get the trades by portfolio name:
    tradesAndContainer <- getTradesFromContainerNames(names(containerMap),
                                                      sourceSession,
                                                      type=containerType,
                                                      gte=gte)

    ## Get the vision portfolios:
    sourceContainer <- tradesAndContainer[["container"]]

    ## Get the vision trades by portfolio:
    sourceTrades <- tradesAndContainer[["trades"]]

    ## Get the portfolio account map:
    portAccMap <- as.data.frame(getAccountPortfolioMap(sourceContainer, containerType))

    ## Match and add the ucpbh names to the portAccMap
    portAccMap[, "target"] <- sapply(containerMap[match(portAccMap[, "container_name"], names(containerMap))], function(x) x[["tcontainername"]])

    ## Add the container type information:
    portAccMap[, "containerType"] <- rep(containerType, NROW(portAccMap))

    taccountname <- lapply(containerMap, function(x) x[["taccountname"]])
    taccountname <- sapply(taccountname, function(x) ifelse(is.null(x), NA, x))
    portAccMap[, "taccountname"] <- taccountname

    ## Print message:
    print(paste0("Retrieving stocks and corresponding resources from source for ", containerType, " ..."))

    ## Get stocks:
    sourceStocks <- getStocks(sourceContainer, sourceSession, zero = 1, date = Sys.Date(), c = substr(containerType, 1, nchar(containerType) -1))

    ## Get the vision resources by stock:
    sourceResources <- getResourcesByStock(sourceStocks, sourceSession)

    ## Return, if no trades:
    if (NROW(sourceTrades) == 0) {
        return(list("trades"=NULL,
                    "resources"=sourceResources,
                    "portAccMap"=portAccMap))
    }

    ## Append the ucapbh portfolioNames to the vision trades:
    sourceTrades <- data.frame(sourceTrades, "port_name_target"=portAccMap[match(sourceTrades[, "accmain"], portAccMap[, "account"]), "target"],
                               stringsAsFactors=FALSE)

    sourceTrades <- data.frame(sourceTrades, "acc_name_target"=portAccMap[match(sourceTrades[, "accmain"], portAccMap[, "account"]), "taccountname"],
                               stringsAsFactors=FALSE)

    ## Append the instrument currencies to the vision trades:
    sourceTrades <- data.frame(sourceTrades, "ccymain"=as.character(sourceResources[match(sourceTrades[, "resmain"], sourceResources[, "id"]), "ccymain"]),
                               stringsAsFactors=FALSE)

    ## Print message:
    print("Mapping / Creating accounts ...")

    ## Map and overwrite the vision accmain id's with ucapbh accmain id's:
    sourceTrades[, "accmain"] <- accountMapper(portfolio=trimConcatenate(sourceTrades[, "port_name_target"]),
                                               currency=as.character(sourceTrades[,"ccymain"]),
                                               session=targetSession,
                                               accountName=sourceTrades[, "acc_name_target"])

    ## Map and append the resource quantity to vision trades:
    sourceTrades <- data.frame(sourceTrades,
                               "resmain_quantity_source"=as.character(sourceResources[match(sourceTrades[, "resmain"], sourceResources[, "id"]), "quantity"]),
                               stringsAsFactors=FALSE)

    ## Map and append the resource isin to vision trades:
    sourceTrades <- data.frame(sourceTrades, "isin"=sourceResources[match(sourceTrades[, "resmain"], sourceResources[, "id"]), "isin"],
                               stringsAsFactors=FALSE)

    ## Map and append the resource name to vision trades:
    sourceTrades <- data.frame(sourceTrades, "name"=sourceResources[match(sourceTrades[, "resmain"], sourceResources[, "id"]), "name"],
                               stringsAsFactors=FALSE)

    ## Map and append the resource name to vision trades:
    sourceTrades <- data.frame(sourceTrades, "resmain_ccymain"=sourceResources[match(sourceTrades[, "resmain"], sourceResources[, "id"]), "ccymain"],
                               stringsAsFactors=FALSE)

    ## Done, return:
    list("trades"=sourceTrades,
         "resources"=sourceResources,
         "portAccMap"=portAccMap)

}



##' A function to return a flat account to portfolio mapping using the portfolio data-frame
##'
##' This is the description
##'
##' @param container The rdecaf portfolio data-frame
##' @param containerType TODO:
##' @return A data-frame with the flat account to portfolio id's
##' @export
getAccountPortfolioMap <- function(container, containerType) {

    ## If accounts, map according to accounts and return:
    if (containerType == "accounts") {

        ## Get the account/portfolio information
        portAccMap <- cbind(container[, "id"], container[, "portfolio"], container[, "name"])

        ## Name the column:
        colnames(portAccMap) <- c("account", "portfolio", "container_name")

        ## Done, return:
        return(portAccMap)
    }

    ## Initialse the portAccMap
    portAccMap <- data.frame()

    ## Get the account indices in portfolio:
    accountIdx <- grep("accounts.", colnames(container))

    ## Iterate over the accIdx:
    for (accIdx in accountIdx) {

        portAccMap <- rbind(portAccMap, cbind(container[, accIdx], container[, "id"], container[, "name"]))
    }

    ## Get rid of NA accounts:
    portAccMap <- portAccMap[!is.na(portAccMap[,1]),]

    ## Name the column:
    colnames(portAccMap) <- c("account", "portfolio", "container_name")

    ## Done, return:
    portAccMap

}


##' A function to push a payload to a decaf instance.
##'
##' This is the description
##'
##' @param payload The payload in json.
##' @param endpoint The endpoint to be pushed to.
##' @param session The rdecaf session.
##' @param import Boolean to indicate whether to use the import extension. Default is TRUE.
##' @param inbulk Boolean to indicate whether to use the inbulk extension. Default is FALSE.
##' @param params Additional parameters.
##' @return A list with the httr response and a message.
##' @import rdecaf
##' @export
pushPayload <- function(payload, endpoint, session, import=TRUE, inbulk=FALSE, params=NULL) {

    ## If import, then extend the url with "imports" and push payload:
    if (import & !inbulk) {

        response <- try(postResource(endpoint, "imports", payload=payload, session=session), silent=TRUE)
        response <- response[[1]]
    }

    ## If inbulk, extend the url and push payload:
    if (inbulk) {

        response <- try(postResource("imports/inbulk", payload=payload, params=params, session=session), silent=TRUE)
        response <- list("id"=response)
    }

    ## If not import, use the endpoint itself and push payload:
    if (!import & !inbulk) {
        response <- try(postResource(endpoint, payload=payload, session=session), silent=TRUE)
    }

    ## Handle the return value and message:
    if (class(response) == "try-error"){
        val <- NA
        msg <- "ERROR"
    } else {
        val <- response$id
        msg <- "SUCCESS"
    }

    ## Done, return:
    list("id"=val,
         "msg"=msg)

}


##' A function to create a trade reference.
##'
##' This is the description
##'
##' @param date The trade date.
##' @param account The account id.
##' @param resource The resource id.
##' @param qty The quantity of the trade.
##' @param px The price of the trade.
##' @param ext A custom extension. Default is NULL.
##' @return A reference string.
##' @export
createReferenceTrade <- function(date, account, resource, qty, px, ext=NULL){

    ## Construct the record reference.
    ## 1. Date as numeric
    ## 2. Account id
    ## 3. Resource id (max 6 characters)
    ## 4. Number of characters in qtymain (max 12 characters)
    ## 5. First 4 characters of abs(qtymain) * 100000
    ## 6. Buy or Sell indicator ("B" or "S")
    paste0(as.numeric(as.Date(date)),
           account,
           substr(resource, 1, 6),
           ifelse(nchar(qty) > 12, 12, nchar(qty)),
           substr(as.numeric(abs(qty)*100000), 1, 4),
           ifelse(qty > 0, "B", "S"),
           substr(as.numeric(abs(px)*100000), 1, 4),
           ext)
}


##' A function to create the file pipeline for decaf-imports.
##'
##' This is the description
##'
##' @param isLocal Boolean to indicate whether we are in a test environment. Default is FALSE.
##' @param providers A vector with file provider names. If NULL, all existing providers will be considers. Default is NULL.
##' @param append A vector with non-file data providers (i.e API). Any non-file data provider will be appended to the pipeline. Default is NULL.
##' @param processedFiles Boolean to indicate whether processed files should be included. Default is FALSE.
##' @param ignores A vector with key words to ignore for the pipeline.
##' @return A reference string.
##' @export
getPipeline <- function(isLocal,
                        providers=NULL,
                        append=NULL,
                        processedFiles=FALSE,
                        ignores=c(".zip", ".xml", ".txt", "provider_resource_id.csv")){

    ## List all the folders under files:
    folders <- list.files("files/")

    ## Exclude the archive folder:
    folders <- folders[-grep("archive", folders)]

    ## Exclude the archive folder:
    folders <- folders[-grep("stashed", folders)]

    ## Exclude the incoming folder:
    folders <- folders[-grep("incoming", folders)]

    ## List all the files under the folders:
    files <- do.call(c, lapply(folders, function(f) list.files(paste0("files/", f), full.names=TRUE, recursive=TRUE)))

    ## Ignore zips:
    ignoreIdx <- unique(do.call(c, lapply(ignores, function(x) grep(x, files))))

    if (length(ignoreIdx) > 0) {
        files <- files[-ignoreIdx]
    }

    ## Grep temporary files:
    tempFiles <- grep("\\~", files)

    ## Exclude temporary files, if any:
    if (length(tempFiles) > 0) {
        files <- files[-grep("\\~", files)]
    }

    ## Read processed files:
    processed <- as.character(read.csv("files/processed.csv", header=FALSE)[,1])

    ## If processed files should be returned as well overwrite processed file names:
    if (processedFiles) {
        processed <- getRandString(12)
    }

    ## Get the list of file names:
    fileNames <- do.call(c, lapply(strsplit(files, "/"), function(f) f[length(f)]))

    ## Match processed files:
    matchedFiles <- match(fileNames, processed)

    ## List all the files under the folders:
    files <- files[is.na(matchedFiles)]

    ## If only single provider should be returned, filter:
    if (!isNAorEmpty(providers)) {
        files <- files[sapply(strsplit(files, "/"), function(x) !all(is.na(match(providers, x[2]))))]
    }

    ## Check if any file is zip:
    isZip <- safeGrep(files, ".zip") == "1"

    if (any(isZip)) {

        zipFiles <- files[isZip]

        extDirs <- sapply(strsplit(zipFiles, "/"), function(x) paste(x[-length(x)], collapse="/"))

        unzip(zipFiles, exdir=extDirs)

        file.remove(zipFiles)

        unzippedFiles <- list.files(extDirs, recursive=TRUE)

        fileNames <- do.call(c, lapply(strsplit(unzippedFiles, "/"), function(f) f[length(f)]))

        matchedFiles <- match(fileNames, processed)

        unzippedFiles <- unzippedFiles[is.na(matchedFiles)]

        files <- c(files, unzippedFiles)

    }

    ## Append the api providers:
    files <- c(files, append)

    ## Finally, return:
    return(files)

}


##' A function to queue incoming files to be considered for file pipeline.
##'
##' This is the description
##'
##' @param mapper The income file mapper.
##' @return Produces a log of the queueing process and writes to the log of particular session.
##' @export
queueIncoming <- function(mapper){

    ## Get the incoming files:
    incoming <- list.files("files/_incoming", recursive=TRUE)

    ## Get the incoming files full path:
    incomingFull <- list.files("files/_incoming", full.names=TRUE, recursive=TRUE)

    ## Assign files to the folders in list:
    folderAssignment <- lapply(names(mapper), function(m) incoming[grep(m, trimConcatenate(incoming))])

    ## Folder assignment full:
    folderAssignmentFull <- lapply(names(mapper), function(m) incomingFull[grep(m, trimConcatenate(incoming))])

    ## Assign the folder names:
    names(folderAssignment) <- mapper

    ## Iterate over the assigned folders:
    for (i in 1:length(folderAssignment)){

        ## Get current folder:
        files <- folderAssignment[[i]]

        ## Current folder full:
        filesFull <- folderAssignmentFull[[i]]

        ## Ingore, if not file:
        if (length(files) == 0){
            next
        }

        ## Is folder?
        isFolder <- safeGrep(files, "/") == "1"

        ## If any is folder, reassign file name:
        if (any(isFolder)) {
            files[isFolder] <- sapply(strsplit(files[isFolder], "/"), function(x) tail(x, 1))
        }

        ## Gauge if it already has file name extension:
        hasFileExt <- nchar(files) > 30

        ## If yes, then keep same name. Else create new name:
        if (all(hasFileExt)) {
            newFiles <- files
        } else {
            ## Set new file name(s):
            newFiles <- do.call(c, lapply(files, function(f) extendFileName(f, randomStrN=4)))
        }

        ## Set the new path(s):
        newPath <- paste0("files/", mapper[i], "/", newFiles)

        ## Move files:
        file.rename(filesFull, newPath)

        ## Message:
        messageHandler(infotext=paste0("Queued ", newPath))
    }

    ## Get the incoming files:
    incoming <- list.files("files/_incoming")
    incomingFull <- list.files("files/_incoming", full.names=TRUE)

    ## Iterate over the incoming files, if any:
    if (length(incoming) > 0){

        ## Iterate over incoming files:
        for (i in 1:length(incoming)){

            ## Incoming file:
            rem <- incoming[i]

            ## Memorise the original name:
            remOrg <- rem

            ## Memorise the full path:
            remFull <- incomingFull[i]

            ## Get the file extension:
            ext <- sapply(strsplit(rem, "\\."), function(x) tail(x, 1))

            ## Upper case the incoming file name without the extension:
            rem <- toupper(gsub("\\.", "", gsub(ext, "", rem)))

            ## Get rid of unnecessary strings:
            rem <- trimws(gsub("\\(1)", "", rem))

            ## Compute a coefficient to be applied to the jaccard string distance.
            ## The less characters the string has, the lower the coefficient, hence
            ## more permissable in the matching process.
            coeff <- 1 - (1 / nchar(rem))
            strDist <- stringdist(rem, as.character(mapper), method="jaccard") * coeff

            ## The fuzzy matching has to comform with minimum requirements:
            ## 1. Jaccard coefficient has to be below 0.2.
            ## 2. The string distance coefficient has to be the minimum.
            ## 3. The string distance coefficient may not be -Inf.
            remAssignment <- strDist < 0.2 & strDist == min(strDist) & abs(strDist) < Inf

            ## If matching criteria are met, place file in the right folder:
            if (any(remAssignment)) {
                ## Exten the file name with a random string:
                newFile <- extendFileName(remOrg, randomStrN=4)
                ## Set the new path(s):
                newPath <- paste0("files/", mapper[remAssignment], "/", newFile)
                ## Move files:
                file.rename(remFull, newPath)
            }
        }
    }

    ## Check the incoming files:
    incoming <- list.files("files/_incoming")

    if (length(incoming) > 0){
        ## Message:
        messageHandler(warntext=paste0(". File could NOT be queued:", incoming))
    }
}


##' A function runs the java demail.
##'
##' This is the description
##'
##' @param address The address of the email server.
##' @param jarPath The path of the demail jar file.
##' @param host The name of the email host.
##' @param port The port of the email server.
##' @param folder The name of the folder on the email server.
##' @param directory The local directory to sync to.
##' @param archive The name of the archive folder on server.
##' @param exclude TODO
##' @param since TODO
##' @param until TODO
##' @return A message to show the log of the demail process.
##' @export
runDemail <- function(address, jarPath, host, port, folder, directory, archive, exclude=NULL, since=Sys.Date(), until=Sys.Date()){

    ## Get user name of email:
    user <- strsplit(address, "@")[[1]][1]

    ## Get the email pw:
    demailpw <- jsonlite::fromJSON("~/.decaf.json")[["settings"]][["demail"]][[user]]

    ## Comman-line friendly parse the email password
    demailpw <- as.factor(paste0("'", as.factor(demailpw), "'"))

    ## Construct the java jar command argument:
    jarsArg <- sprintf("java -jar %s", jarPath)
    ## jarsArg <- sprintf("java -Dmail.imap.fetchsize=1048576 -jar %s", jarPath)
    jarsArg <- "demail"

    ## Construct the host command argument:
    hostArg <- sprintf("--host %s", host)

    ## Construct the port command argument:
    portArg <- sprintf("--port %s", port)

    ## Construct the user command argument:
    userArg <- sprintf("--user %s", address)

    ## Construct the user command argument:
    passArg <- sprintf("--pass %s", demailpw)

    ## Construct folder command argument:
    fldrArg <- sprintf("--folder %s", folder)

    ## Construct the directory command argument:
    dirsArg <- sprintf("--directory %s", directory)

    ## Construct the archive command argument:
    archArg <- sprintf("--archive %s", archive)

    ##sincArg <- sprintf("--since %s", "2018-11-08")
    sincArg <- sprintf("--since %s", since)

    ##untlArg <- sprintf("--until %s", "2018-11-08")
    untlArg <- sprintf("--until %s", until)

    ## Construct the command:
    runCmd <- paste(jarsArg,
                    "download-attachments",
                    hostArg,
                    portArg,
                    "--ssl",
                    userArg,
                    passArg,
                    fldrArg,
                    dirsArg,
                    archArg,
                    sincArg,
                    untlArg)

    print(runCmd)

    ## Run demail:
    result <- try(system(runCmd), silent=TRUE)

    ## Handle message:
    messageHandler(result, 1, errortext=" Couldn't run demail command.")
 }


##' A function to call the system command rsync
##'
##' This is the description
##'
##' @param server The server address
##' @param location The location on the server.
##' @param target The target directory locally.
##' @param folderNames The names of the local folders.
##' @param exclude A vector with keys to exclude from syncin.
##' @return A message to show the log of the rsync call.
##' @export
rSync <- function(server, location, target, folderNames, exclude=""){

    ## If we have to exclude keywords, overwrite exclude:
    if (any(!isNAorEmpty(exclude))) {
        exclude <- paste("--exclude", exclude, collapse=" ")
    }

    ## If servers is local, omit server string:
    server <- ifelse(server=="local", "", paste0(server, ":"))

    ## Run the rsync commands:
    for (f in 1:length(folderNames)) {
        cmd <- trimDws(sprintf("rsync -auzvP %s %s%s%s/ %s%s/", exclude, server, location, names(folderNames)[f], target, folderNames[f]))
        result <- try(system(cmd), silent = TRUE)
    }

    ## Message:
    messageHandler(result, 0, condition="greater", errortext=paste0(" error code: ", result))
}


##' This function applies the account preemble method 1.
##'
##' Accounts are created with the name as provided if such
##' name is missing.If target portfolio name does not exist
##' yet, function creates. If such portfolio assignment is missing,
##' this functions creates a portfolio with the same name
##' as account.
##'
##' @param records A data frame with columns ACCNAME, targetPortName and REFCCY.
##' @param sysAccs A data frame with the system accounts.
##' @param custodian The custodian id.
##' @param session The rdecaf session.
##' @return Returns the side by side comparison of decaf position and provider position.
##' @export
accountPreembleMethod1 <- function(records, sysAccs, custodian, session) {

    ## Get the accmains:
    records[, "accmain"] <- sysAccs[match(records[, "ACCNAME"], sysAccs[, "name"], incomparables=NA), "id"]

    ## If all accounts exist, return:
    if (all(!is.na(records[, "accmain"]))) {
        return(records)
    }

    ## Get the system portfolios:
    sysPorts <- as.data.frame(getResource("portfolios", params=list("format"="csv", "page_size"=-1), session=session))

    ## Get the records with missing accmain:
    recs <- records[is.na(records[, "accmain"]), ]

    ## Get the portfolio id:
    recs[, "portfolio"] <- sysPorts[match(trimConcatenate(recs[, "targetPortName"]), trimConcatenate(sysPorts[, "name"])), "id"]

    ## If portfolios are missing, create:
    if (any(is.na(recs[, "portfolio"]))) {

        ## Get the NA portfolios:
        naPorts <- is.na(recs[, "portfolio"])

        guids <- apply(recs[naPorts, c("targetPortName", "REFCCY")], MARGIN=1, function(x) digest::digest(paste0(x, custodian, collapse="")))

        ## Get the portfolio name:
        port <- data.frame("name"=recs[naPorts, "targetPortName"],
                           "rccy"=recs[naPorts, "REFCCY"],
                           "refccy"=recs[naPorts, "REFCCY"],
                           "guid"=guids,
                           "team"=1)

        ## Get the portfolio payload:
        payload <- toJSON(list(portfolios=port), auto_unbox=TRUE, na="null", digits=10)

        ## Inbulk portfolios:
        response <- pushPayload(payload=payload, session=session, import=FALSE, inbulk=TRUE, params=list(sync="True"))

        ## Assign the portfolio id:
        recs[naPorts, "portfolio"] <- sapply(response[[1]][[1]][[1]], function(x) x[[1]])

    }

    ## Get the account guids:
    guids <- apply(recs[, c("targetPortName", "ACCNAME", "REFCCY")], MARGIN=1, function(x) digest::digest(paste0(x, custodian, collapse="")))

    ## Get the account data frame:
    accs <- data.frame("name"=recs[, "ACCNAME"],
                       "rccy"=as.character(recs[, "REFCCY"]),
                       "portfolio"=recs[, "portfolio"],
                       "custodian"=as.character(custodian),
                       "guid"=guids,
                       "atype"=NA)

    ## Get  the account payload:
    payload <- toJSON(list(accounts=accs), auto_unbox=TRUE, na="null", digits=10)

    ## Inbulk accounts:
    response <- pushPayload(payload=payload, session=session, import=FALSE, inbulk=TRUE, params=list(sync="True"))

    ## Assign the portfolio id:
    recs[, "accmain"] <- sapply(response[[1]][[1]][[1]], function(x) x[[1]])

    ## Append previously existing accmain records and new accmain records:
    records <- rbind(records[!is.na(records[, "accmain"]), ], recs)

    ## Done, return
    records

}
