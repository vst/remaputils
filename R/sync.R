##' A function to sync portfolios between 2 DECAF instances.
##'
##' This is the description
##'
##' @param sourceSession The session info for the DECAF instance to sync from.
##' @param targetSession The session info for the DECAF instance to sync to.
##' @param accounts The account/portfolio mapper between the 2 instances.
##' @param teamGUID The team GUID of the target instance.
##' @param exclude A vector with key words to exclude unwanted columns in source portfolios.
##' @param search A key word to search for. Default is NULL.
##' @param customName A vector of strings to replace the names of source portfolios with.
##' @param sourcePortfolios The rdecaf portfolio data frame from source.
##' @return The function creates the portfolios in the target instance and return the source portfolio data-frame.
##' @import rdecaf
##' @export
syncPortfolios <- function(sourceSession, targetSession, accounts, teamGUID, exclude=NULL, search=NULL, customName=NULL, sourcePortfolios=NULL) {

    if (is.null(sourcePortfolios)) {

        ## Construct the account params:
        params <- list("page_size"=-1,
                       "format"="csv",
                       "search"=as.character(search))

        ## Get the entire accounts:
        sourcePortfolios <- try(as.data.frame(getResource("portfolios", params=params, session=sourceSession)), silent=TRUE)

        ## Match, filter target portfolios:
        sourcePortfolios <- sourcePortfolios[!is.na(stringMatch(trimConcatenate(sourcePortfolios[,"name"]), trimConcatenate(accounts))), ]

    }

    ## Retain the orginal source portfolio:
    sourcePortfoliosOriginal <- sourcePortfolios

    ## Overwrite the team field:
    sourcePortfolios[, "team"] <- paste0("dcf:team?guid=", teamGUID)

    ## Exclude columns from source portfolio frame:
    if (!is.null(exclude)) {

        ## Iterate over exclude keys:
        for (excl in exclude) {
            sourcePortfolios <- sourcePortfolios[, -grep(excl, colnames(sourcePortfolios))]
        }
    }

    ## Overwrite resource fields:
    for (fld in c("id", "created", "creator", "updated", "updater", "team_name")) {
        sourcePortfolios[, fld] <- NULL
    }

    if (!is.null(customName)) {
        sourcePortfolios[, "name"] <- customName
    }

    ## Create the payload:
    payload <- toJSON(list("portfolios"=sourcePortfolios), auto_unbox=TRUE, na="null", digits=10)

    ## Push the payload:
    response <- pushPayload(payload=payload, endpoint=NULL, session=targetSession, import=FALSE, inbulk=TRUE, params=list(sync="True"))

    ## Done, return:
    sourcePortfoliosOriginal

}


##' A function to sync share classes between 2 DECAF instances.
##'
##' This is the description
##'
##' @param sourceSession The session info for the DECAF instance to sync from.
##' @param targetSession The session info for the DECAF instance to sync to.
##' @param sourcePortfolios The data-frame with the source portfolios.
##' @return The function creates the share classes in the target instance and returns a share class data-frame.
##' @import rdecaf
##' @export
syncShareclass <- function(sourceSession, targetSession, sourcePortfolios){

    ## Get the params:
    params <- list("format"="csv", "page_size"=-1)

    ## Get the system portfolios:
    sourceShreclasses <- as.data.frame(getResource("shareclasses", params=params, session=sourceSession))

    ## Append the source id's:
    sourceShreclasses[, "source_id"] <- sourceShreclasses[, "id"]

    ## Get the system portfolios:
    targetShreclasses <- as.data.frame(getResource("shareclasses", params=params, session=targetSession))

    ## Get the target portfolios:
    targetPortfolios <- as.data.frame(getResource("portfolios", params=params, session=targetSession))

    ##  Get rid of unnecessary fields:
    for (fld in c("created", "creator", "updated", "updater", "subscriptions", "outstanding", "schedule")) {
        sourceShreclasses <- sourceShreclasses[, -grep(fld, colnames(sourceShreclasses))]
    }

    ## Match the portfolio:
    sourcePortfolioID <- sourcePortfolios[match(sourceShreclasses[, "portfolio"], sourcePortfolios[, "id"]), "id"]

    ## Get the share-class information for portfolios in question:
    sourceShreclasses <- sourceShreclasses[!is.na(sourcePortfolioID), ]

    ## Iterate over source share classes and create where necessary:
    for (row in 1:NROW(sourceShreclasses)) {

        ## Get the nth share-class:
        shrcls <- sourceShreclasses[row,]

        ## Tyr to match the individual source share-class and the target share-classes:
        matchShrcls <- match(trimConcatenate(shrcls[, "name"]), trimConcatenate(safeTry(try(safeColumn(targetShreclasses, "name"), silent=TRUE))))

        ## If such share-class exists in target, overwrite fields and return:
            if (any(!is.na(matchShrcls))) {
                sourceShreclasses[row, "portfolio"] <- targetShreclasses[matchShrcls, "portfolio"]
                sourceShreclasses[row, "id"] <- targetShreclasses[matchShrcls, "id"]
                sourceShreclasses[row, "guid"] <- targetShreclasses[matchShrcls, "guid"]
                next
            }

        ## If share-class in target doesn't exist, overwrite the necessary fields to create such share-class:
        sourcePortfolioGUID   <-   sourcePortfolios[match(shrcls[, "portfolio"], sourcePortfolios[, "id"]), "guid"]
        shrcls[, "portfolio"] <- targetPortfolios[match(sourcePortfolioGUID, targetPortfolios[, "guid"]), "id"]
        shrcls[, "id"] <- NULL

        ## Create the payload:
        payload <- toJSON(as.list(shrcls),  auto_unbox=TRUE, na="null")

        ## Post the payload:
        response <- postResource("shareclasses", payload=payload, session=targetSession)

        ## Overwrite the fields:
        sourceShreclasses[row, "portfolio"] <- shrcls[, "portfolio"]
        sourceShreclasses[row, "id"] <- response[["id"]]
        sourceShreclasses[row, "guid"] <- response[["guid"]]
    }

    ## Done, return:
    sourceShreclasses

}


##' A function to sync agents between 2 DECAF instances:
##'
##' This is the description
##'
##' @param sourceSession The session info for the DECAF instance to sync from.
##' @param targetSession The session info for the DECAF instance to sync to.
##' @param tradeAgents The agent id's coming from the source trades.
##' @return The function creates the agents in the target instance and returns a agent class data-frame.
##' @import rdecaf
##' @export
syncAgent <- function(sourceSession, targetSession, tradeAgents){

    ## Get the params:
    params <- list("format"="csv", "page_size"=-1)

    ## Get the system portfolios:
    sourceAgents <- as.data.frame(getResource("agents", params=params, session=sourceSession))

    ## Filter the agents by the trade's agents:
    sourceAgents <- sourceAgents[match(unique(na.omit(tradeAgents)), sourceAgents[, "id"]), ]

    ## Append the source id's:
    sourceAgents[, "source_id"] <- sourceAgents[, "id"]

    ## The return value:
    retval <- sourceAgents

    ## Get the system portfolios:
    targetAgents <- as.data.frame(getResource("agents", params=params, session=targetSession))

    ##  Get rid of unnecessary fields:
    for (fld in c("created", "creator", "updated", "updater")) {
        sourceAgents <- sourceAgents[, -grep(fld, colnames(sourceAgents))]
    }

    ## If we have any agents in target, filter source agents which do not exist in target:
    if (!NROW(targetAgents) == 0) {
        sourceAgents <- sourceAgents[is.na(match(targetAgents[, "guid"], sourceAgents[, "guid"])), ]
    }

    ## If all source agents exist in target, exit and return retval:
    if (NROW(sourceAgents) == 0) {
        return(retval)
    }

    ## Iterate over agents and create:
    for (row in 1:NROW(sourceAgents)) {

        ## Set id to NULL:
        sourceAgents[, "id"] <- NULL

        ## Create payload:
        payload <- toJSON(as.list(sourceAgents), auto_unbox=TRUE)

        ## Push payload:
        response <- pushPayload(payload, "agents", session=targetSession, import=FALSE, inbulk=FALSE, params=NULL)

    }

    ## Done, return:
    retval

}


##' A function to sync resources between 2 DECAF instances:
##'
##' This is the description
##'
##' @param sourceSession The session info for the DECAF instance to sync from.
##' @param targetSession The session info for the DECAF instance to sync to.
##' @param sourceAccounts The accounts data-frame coming from the source instance.
##' @param noPush The flag to indiciate whether source resources should not be pushed.
##' @return The function creates the resources in the target instance and returns a resource data-frame.
##' @import rdecaf
##' @export
syncResources <- function(targetSession, sourceSession, sourceAccounts, noPush=FALSE) {

    ## Get the vision stocks:
    stocks <- getStocks(sourceAccounts, sourceSession)

    ## Get the resources:
    resources <- getResourcesByStock(stocks, sourceSession)

    ## Overwrite resource fields:
    for (fld in c("id", "created", "creator", "updated", "updater", "incomplete", "tags")) {
        resources[, fld] <- NULL
    }

    ## Import the underlyings:
    if (any(resources[, "is_underlying"])) {

        ## Get the resources which are underlyings:
        underlyings <- list("artifacts"=resources[resources[, "is_underlying"], ])

        ## Create the payload:
        payload <- toJSON(underlyings, auto_unbox=TRUE, na="null", digits=10)

        ## Push the payload:
        response <- pushPayload(payload=payload, endpoint=NULL, session=targetSession, import=FALSE, inbulk=TRUE, params=list(sync="True"))
    }

    ## Exit before pushing data to target:
    if (noPush) {
        return(resources)
    }

    ## resources[, "description"] <- ellipsify(resources[, "description"])
    resources[is.na(resources[, "quantity"]), "quantity"] <- 1

    ## Create the payload:
    payload <- toJSON(list("artifacts"=resources), auto_unbox=TRUE, na="null", digits=10)

    ## Push the payload:
    response <- pushPayload(payload=payload, endpoint=NULL, session=targetSession, import=FALSE, inbulk=TRUE, params=list(sync="True"))

    ## Done, return:
    resources <- getResource("resources", params=list("page_size"=-1), session=targetSession)
    resources <- lapply(resources, function(res) do.call(cbind, res[!names(res) == "tags"]))
    safeRbind(lapply(1:length(resources), function(i) {
        data.frame(t(resources[[i]][, safeGrep(colnames(resources[[i]]), "extdata") == "0"]),
                   stringsAsFactors=FALSE)
    }))

}


##' A function to sync analytical types between 2 DECAF instances:
##'
##' This is the description
##'
##' @param sourceSession The session info for the DECAF instance to sync from.
##' @param targetSession The session info for the DECAF instance to sync to.
##' @param sourceAccounts The accounts data-frame coming from the source instance.
##' @return The function creates the analytical types in the target instance and returns a analytical types data-frame.
##' @import rdecaf
##' @export
syncAtypes <- function(sourceSession, targetSession, sourceAccounts) {

    ## Get the source atypes:
    sourceAtypes <- as.data.frame(getResource("analyticaltypes", params=list("format"="csv", "page_size"=-1), session=sourceSession))

    ## Filter the atypes:
    sourceAtypes <- sourceAtypes[na.omit(match(sourceAccounts[, "atype"], sourceAtypes[, "id"])), ]

    sourceAtypes[, "source_id"] <- sourceAtypes[, "id"]

    ## Overwrite resource fields:
    for (fld in c("id", "created", "creator", "updated", "updater")) {
        sourceAtypes[, fld] <- NULL
    }

    ## Create the payload:
    payload <- toJSON(list("analyticaltypes"=sourceAtypes), auto_unbox=TRUE, na="null", digits=10)

    ## Push the payload:
    response <- try(pushPayload(payload=payload, endpoint=NULL, session=targetSession, import=FALSE, inbulk=TRUE, params=list(sync="True")), silent=TRUE)

    ## Return:
    sourceAtypes

}


##' A function to sync institutions between 2 DECAF instances:
##'
##' This is the description
##'
##' @param sourceSession The session info for the DECAF instance to sync from.
##' @param targetSession The session info for the DECAF instance to sync to.
##' @param sourceAccounts The accounts data-frame coming from the source instance.
##' @param trades If trades is given, additional institutions based on feeagt will be created. Default is NULL.
##' @return The function creates the institutions in the target instance and returns a institutions data-frame.
##' @import rdecaf
##' @export
syncInstitutions <- function(sourceSession, targetSession, sourceAccounts, trades=NULL) {

    ## Get the source atypes:
    sourceInstitutions <- as.data.frame(getResource("institutions", params=list("format"="csv", "page_size"=-1), session=sourceSession))

    ## Append the fee agent institutions:
    if (!is.null(trades)) {
        ## Map the fee agent institutions:
        feeagtInstitutions <- sourceInstitutions[match(unique(trades[, "feeagt"]), sourceInstitutions[, "id"]), ]
    } else {
        feeagtInstitutions <- NULL
    }

    ## Filter the atypes:
    sourceInstitutions <- rbind(sourceInstitutions[na.omit(match(sourceAccounts[, "custodian"], sourceInstitutions[, "id"])), ], feeagtInstitutions)

    ## Remove duplicate institutions and NA's:
    sourceInstitutions <- cleanNARowsCols(sourceInstitutions[!duplicated(sourceInstitutions[, "id"]), ])

    ## Retain the source id:
    sourceInstitutions[, "source_id"] <- sourceInstitutions[, "id"]

    ## Overwrite resource fields:
    for (fld in c("id", "created", "creator", "updated", "updater")) {
        sourceInstitutions[, fld] <- NULL
    }

    ## Create the payload:
    payload <- toJSON(list("institutions"=sourceInstitutions), auto_unbox=TRUE, na="null", digits=10)

    ## Push the payload:
    response <- pushPayload(payload=payload, endpoint=NULL, session=targetSession, import=FALSE, inbulk=TRUE, params=list(sync="True"))

    ## Return:
    sourceInstitutions

}


##' A function to sync accounts between 2 DECAF instances:
##'
##' This is the description
##'
##' @param sourceSession The session info for the DECAF instance to sync from.
##' @param targetSession The session info for the DECAF instance to sync to.
##' @param atypes The data-frame with analtyical types.
##' @param institutions The data-frame with institutions.
##' @param portfolios The data-frame with portfolios.
##' @param accounts The data-frame with the source accounts.
##' @return The function creates the accounts in the target instance and returns a accounts data-frame.
##' @import rdecaf
##' @export
syncAccounts <- function(sourceSession, targetSession, atypes, institutions, portfolios, accounts) {

    ## Overwrite resource fields:
    for (fld in c("id", "creator", "updated", "updater", "team_name")) {
        accounts[, fld] <- NULL
    }

    ## Force atype to be character:
    accounts[, "atype"] <- as.character(accounts[, "atype"])

    ## Overwrite atype with guid:
    accounts[!is.na(accounts[, "atype"]), "atype"] <- paste0("dcf:analyticaltype?guid=", atypes[, "guid"])

    ## Overwrite custodian with guid:
    accounts[, "custodian"] <- paste0("dcf:institution?guid=", institutions[, "guid"])

    ## Overwrite portfolio with guid:
    accounts[, "portfolio"] <- paste0("dcf:portfolio?guid=",
                                      portfolios[match(accounts[, "portfolio_name"], portfolios[, "name"]), "guid"])

    ## Create the payload:
    payload <- toJSON(list("accounts"=accounts), auto_unbox=TRUE, na="null", digits=10)

    ## Push the payload:
    response <- pushPayload(payload=payload, endpoint=NULL, session=targetSession, import=FALSE, inbulk=TRUE, params=list(sync="True"))

    ## Return:
    accounts

}


##' A function to sync external valuations between 2 DECAF instances:
##'
##' This is the description
##'
##' @param sourceSession The session info for the DECAF instance to sync from.
##' @param targetSession The session info for the DECAF instance to sync to.
##' @param targetPortfolios The data-frame with target portfolios data frame.
##' @param targetShareclasses The data-frame with target share classes data frame.
##' @param accounts The data-frame with the source accounts.
##' @return The function creates the accounts in the target instance and returns a accounts data-frame.
##' @import rdecaf
##' @export
syncExtValuation <- function(sourceSession, targetSession, targetPortfolios, targetShareclasses, accounts) {

    ## Get the source portfolio id's:
    sourcePortIds <- unique(accounts[, "portfolio"])

    ## Initialise the variable:
    extValuations <- NULL

    ## Iterate over source  portfolio id's and get external valuations:
    for (id in sourcePortIds) {
        ## Get the valuation and combine:
        extValuations <- rbind(extValuations, as.data.frame(getResource("externalvaluations", params=list("format"="csv", "page_size"=-1, "portfolio"=id), session=sourceSession)))
    }

    for (fld in c("id", "creator", "updated", "updater")) {
        extValuations[, fld] <- NULL
    }

    ## Overwrite the target share class id:
    extValuations[, "shareclass"] <- targetShareclasses[match(extValuations[, "shareclass"], targetShareclasses[, "source_id"]), "id"]

    ## Overwrite the portfolio id:
    extValuations[, "portfolio"] <- paste0("dcf:portfolio?guid=", targetPortfolios[match(extValuations[, "portfolio"], targetPortfolios[, "id"]), "guid"])

    ## Prepare the payload:
    payload <- toJSON(list(externalvaluations = extValuations), auto_unbox = TRUE, na = "null", digits = 10)

    ## Push and get the response:
    response <- pushPayload(payload = payload, endpoint = NULL, session = targetSession, import = FALSE, inbulk = TRUE, params = list(sync = "True"))

    ## Done, return the external valuations:
    extValuations

}



##' A function to sync trades between 2 DECAF instances:
##'
##' This is the description
##'
##' @param targetSession The session info for the DECAF instance to sync to.
##' @param trades The trades from the target instance.
##' @param atypes The data-frame with analtyical types.
##' @param shrclss The data-frame with the share classes.
##' @param institutions The data-frame with institutions.
##' @param agents The data-frame with agents.
##' @return The function creates the trades in the target instance and returns a trades data-frame.
##' @import rdecaf
##' @export
syncTrades <- function(targetSession, trades, atypes, shrclss, institutions, agents) {

    ## Overwrite accmain with guid:
    trades[, "accmain"] <- ifelse(is.na(trades[, "accmain"]), NA, paste0("dcf:account?guid=", trades[, "accmain_guid"]))

    ## Overwrite accaltn with guid:
    trades[, "accaltn"] <- ifelse(is.na(trades[, "accaltn"]), NA, paste0("dcf:account?guid=", trades[, "accaltn_guid"]))

    ## Overwrite acccomp with guid:
    trades[, "acccomp"] <- ifelse(is.na(trades[, "acccomp"]), NA, paste0("dcf:account?guid=", trades[, "acccomp_guid"]))

    ## Overwrite accundr with guid:
    trades[, "accundr"] <- ifelse(is.na(trades[, "accundr"]), NA, paste0("dcf:account?guid=", trades[, "accundr_guid"]))

    ## Overwrite acccntr with guid:
    trades[, "acccntr"] <- ifelse(is.na(trades[, "acccntr"]), NA, paste0("dcf:account?guid=", trades[, "acccntr_guid"]))

    ## Overwrite accprin with guid:
    trades[, "accprin"] <- ifelse(is.na(trades[, "accprin"]), NA, paste0("dcf:account?guid=", trades[, "accprin_guid"]))

    ## Overwrite accintr with guid:
    trades[, "accintr"] <- ifelse(is.na(trades[, "accintr"]), NA, paste0("dcf:account?guid=", trades[, "accintr_guid"]))

    ## Overwrite resmain with guid:
    trades[, "resmain"] <- ifelse(is.na(trades[, "resmain"]), NA, paste0("dcf:artifact?guid=", trades[, "resmain_guid"]))

    ## Overwrite resaltn with guid:
    trades[, "resaltn"] <- ifelse(is.na(trades[, "resaltn"]), NA, paste0("dcf:artifact?guid=", trades[, "resaltn_guid"]))

    ## Overwrite rescomp with guid:
    trades[, "rescomp"] <- ifelse(is.na(trades[, "rescomp"]), NA, paste0("dcf:artifact?guid=", trades[, "rescomp_guid"]))

    ## Overwrite resundr with guid:
    trades[, "resundr"] <- ifelse(is.na(trades[, "resundr"]), NA, paste0("dcf:artifact?guid=", trades[, "resundr_guid"]))

    ## Overwrite rescntr with guid:
    trades[, "rescntr"] <- ifelse(is.na(trades[, "rescntr"]), NA, paste0("dcf:artifact?guid=", trades[, "rescntr_guid"]))

    ## Overwrite resprin with guid:
    trades[, "resprin"] <- ifelse(is.na(trades[, "resprin"]), NA, paste0("dcf:artifact?guid=", trades[, "resprin_guid"]))

    ## Overwrite resintr with guid:
    trades[, "resintr"] <- ifelse(is.na(trades[, "resintr"]), NA, paste0("dcf:artifact?guid=", trades[, "resintr_guid"]))

    ## Overwrite atype with guid:
    trades[, "atype"]   <- ifelse(is.na(trades[, "atype"]), NA, paste0("dcf:analyticaltype?guid=", atypes[match(trades[, "atype"], atypes[, "source_id"]), "guid"]))

    ## Overwrite agent with guid:
    trades[, "agent"]   <- ifelse(is.na(trades[, "agent"]), NA, paste0("dcf:agent?guid=", agents[match(trades[, "agent"], agents[, "source_id"]), "guid"]))

    ## Overwrite shrcls" with guid:
    trades[, "shrcls"]  <- ifelse(is.na(trades[, "shrcls"]), NA, paste0("dcf:shareclass?guid=", shrclss[match(trades[, "shrcls"], shrclss[, "source_id"]), "guid"]))

    ## Overwrite feeagt" with guid:
    trades[, "feeagt"]  <- ifelse(is.na(trades[, "feeagt"]), NA, paste0("dcf:institution?guid=", institutions[match(trades[, "feeagt"], institutions[, "source_id"]), "guid"]))

    ## Get rid of columns:
    for (fld in c("id")) {
        trades[, fld] <- NULL
    }

    ## Create the trade batches:
    batches <- createBatches(NROW(trades), 1000)

    ## Iterate over batches:
    for (i in 1:length(batches[[1]])) {

        ## What's the start index of the current batch:
        start <- batches$startingIdx[[i]]

        ## What's the end index of the current batch:
        end <- batches$endingIdx[[i]]

        ## Create the payload:
        payload <- toJSON(list("actions"=trades[start:end,]), auto_unbox=TRUE, na="null", digits=10)

        ## Push the payload:
        response <- pushPayload(payload=payload, endpoint=NULL, session=targetSession, import=FALSE, inbulk=TRUE, params=list(sync="True"))
    }

    ## Done, return:
    trades
}


##' A function to sync ohlc observations between 2 DECAF instances.
##'
##' This is the description
##'
##' @param sourceSession The session info for the DECAF instance to sync from.
##' @param targetSession The session info for the DECAF instance to sync to.
##' @param resources The resource data frame.
##' @param lte TODO:
##' @param lookBack TODO:
##' @param targetPrefix TODO:
##' @return TODO:
##' @import rdecaf
##' @export
syncOHLC <- function(sourceSession, targetSession, resources, lte=NULL, lookBack=NULL, targetPrefix=NULL) {

    ## Store the original symbols:
    originalSymbol <- resources[, "symbol"]

    ## If there is no special prefix, mask the prefix variable:
    if (is.null(targetPrefix)) {
        prefix <- ""
    } else {
        prefix <- targetPrefix
    }

    ## Remove the special prefix:
    resources[, "symbol"] <- gsub(prefix, "", resources[, "symbol"])

    ## Get the unique symbols including ohlccodes:
    uSymbols <- as.character(na.omit(unique(c(resources[, "symbol"], safeColumn(resources, "ohlccode")))))

    ## Get the currency symbols:
    currency <- resources[resources[, "type"] == "Cash", "symbol"]

    ## Compute the possible fx pairs:
    pairs <- expand.grid(currency, currency)

    ## Get rid of identities and combine the legs:
    pairs <- as.character(apply(pairs[apply(pairs, MARGIN=1, function(x) as.character(x[1]) != as.character(x[2])), ], MARGIN=1, function(x) paste0(x, collapse="")))

    ## Append pairs to the symbols:
    uSymbols <- c(uSymbols, pairs)

    ## Remove empty or NA symbols:
    uSymbols <- uSymbols[!isNAorEmpty(uSymbols)]

    ## Get the ohlc observations for each symbol:
    ohlcObsList <- lapply(uSymbols, function(sym) getOhlcObsForSymbol(sourceSession, sym, lte, lookBack))

    ## Discard empty lists:
    ohlcObsList <- ohlcObsList[sapply(ohlcObsList, function(x) dim(x)[1] != 0)]

    ## Retain the dates as character:
    dates <- do.call(c, lapply(ohlcObsList, function(x) as.character(x[, "date"])))

    ## Safely combine the ohlc list:
    ohlcObs <- safeRbind(ohlcObsList)

    ## Overwrite dates:
    ohlcObs[, "date"] <- dates

        ## Get rid of id:
    ohlcObs[, "id"] <- NULL

    ## In which chunks shall we push?
    chunk <- 20000

    ## Initialize the starting index:
    start <- 1

    ## Compute the number of interations required:
    iterations <- ceiling(NROW(ohlcObs) / chunk)

    ## Iterate over chunks:
    for (i in 1:iterations) {

        ## Compute the minimum chunk:
        chunk <- min(NROW(ohlcObs) - start, chunk)

            ## Compute the end index:
        end <- start + chunk

        ## Print stuff:
        print(paste0("Pushing chunk: ", end, " of ", NROW(ohlcObs)))

        ## Get the chunk of observations to be pushed this iteration:
        ohlcObsN <- ohlcObs[start:end,]

        ## Get the replace symbol:
        replaceSymbol <- originalSymbol[match(paste0(prefix, ohlcObsN[, "symbol"]), originalSymbol)]

        ## Replace the symbol with the originals:
        ohlcObsN[, "symbol"] <- ifelse(is.na(replaceSymbol), ohlcObsN[, "symbol"], replaceSymbol)

        ## Construct the payload:
        payload <- toJSON(ohlcObsN, auto_unbox=TRUE, na = c("null"))

        ## Push:
        result <- httr::POST(paste0(targetSession[["location"]], "/ohlcobservations/updatebulk/"),
                             authenticate(targetSession[["username"]], targetSession[["password"]]),
                             body=payload,
                             add_headers(.headers = c("Content-Type"="application/json")))

            ## Update the start index:
        start <- start + chunk + 1
    }

    ## Done, return
    ohlcObs
}
