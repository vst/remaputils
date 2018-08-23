syncPortfolios <- function(sourceSession, targetSession, accounts, teamGUID, exclude=NULL) {

    ## Construct the vision account params:
    params <- list("page_size"=-1,
                   "format"="csv")

    ## Get the entire ucapbh accounts:
    sourcePortfolios <- as.data.frame(getResource("portfolios", params=params, session=sourceSession))

    ## Match, filter target portfolios:
    sourcePortfolios <- sourcePortfolios[!is.na(stringMatch(trimConcatenate(sourcePortfolios[,"name"]), trimConcatenate(accounts))), ]

    ## Retain the orginal source portfolio:
    sourcePortfoliosOriginal <- sourcePortfolios

    ## Overwrite the team field:
    sourcePortfolios[, "team"] <- "dcf:team?guid=f401ee72-60a3-496a-9504-da8dd1d67b3e"

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

    ## Create the payload:
    payload <- toJSON(list("portfolios"=sourcePortfolios), auto_unbox=TRUE, na="null", digits=10)

    ## Push the payload:
    response <- pushPayload(payload=payload, endpoint=NULL, session=targetSession, import=FALSE, inbulk=TRUE, params=list(sync="True"))

    ## Done, return:
    list("response"=response,
         "portfolios"=sourcePortfoliosOriginal)

}


syncShareclass <- function(sourceSession, targetSession, sourcePortfolios){

    ## Get the params:
    params <- list("format"="csv", "page_size"=-1)

    ## Get the system portfolios:
    sourceShreclasses <- as.data.frame(getResource("shareclasses", params=params, session=sourceSession))

    ## Get the system portfolios:
    targetShreclasses <- as.data.frame(getResource("shareclasses", params=params, session=targetSession))

    ## Get the target portfolios:
    targetPortfolios <- as.data.frame(getResource("portfolios", params=params, session=targetSession))

    ##  Get rid of unnecessary fields:
    for (fld in c("created", "creator", "updated", "updater", "subscriptions", "outstanding", "schedule")) {
        sourceShreclasses <- sourceShreclasses[, -grep(fld, colnames(sourceShreclasses))]
    }

    sourcePortfolioID <- sourcePortfolios[match(sourceShreclasses[, "portfolio"], sourcePortfolios[, "id"]), "id"]
    sourceShreclasses <- sourceShreclasses[!is.na(sourcePortfolioID), ]

    for (row in 1:NROW(sourceShreclasses)) {

        shrcls <- sourceShreclasses[row,]

        matchShrcls <- match(trimConcatenate(shrcls[, "name"]), trimConcatenate(safeTry(try(safeColumn(targetShreclasses, "name"), silent=TRUE))))

        ## Hebele:
        if (any(!is.na(matchShrcls))) {
            sourceShreclasses[row, "portfolio"] <- targetShreclasses[matchShrcls, "portfolio"]
            sourceShreclasses[row, "id"] <- targetShreclasses[matchShrcls, "id"]
            sourceShreclasses[row, "guid"] <- targetShreclasses[matchShrcls, "guid"]
            next
        }

        sourcePortfolioGUID   <-   sourcePortfolios[match(shrcls[, "portfolio"], sourcePortfolios[, "id"]), "guid"]
        shrcls[, "portfolio"] <- targetPortfolios[match(sourcePortfolioGUID, targetPortfolios[, "guid"]), "id"]
        shrcls[, "id"] <- NULL
        shrcls[, "guid"] <- NULL

        payload <- toJSON(as.list(shrcls),  auto_unbox=TRUE)

        response <- postResource("shareclasses", payload=payload, session=targetSession)

        sourceShreclasses[row, "portfolio"] <- shrcls[, "portfolio"]
        sourceShreclasses[row, "id"] <- response[["id"]]
        sourceShreclasses[row, "guid"] <- response[["guid"]]
    }

    ## Done, return:
    list("shareclasses"=sourceShreclasses)

}


syncResources <- function(targetSession, sourceSession, sourceAccounts) {

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

        underlyings <- list("artifacts"=resources[resources[, "is_underlying"], ])

        ## Create the payload:
        payload <- toJSON(underlyings, auto_unbox=TRUE, na="null", digits=10)

        ## Push the payload:
        response <- pushPayload(payload=payload, endpoint=NULL, session=targetSession, import=FALSE, inbulk=TRUE, params=list(sync="True"))
    }

    ## Create the payload:
    payload <- toJSON(list("artifacts"=resources), auto_unbox=TRUE, na="null", digits=10)

    ## Push the payload:
    response <- pushPayload(payload=payload, endpoint=NULL, session=session, import=FALSE, inbulk=TRUE, params=list(sync="True"))

    ## Done, return:
    safeRbind(lapply(getResource("resources", params=list("page_size"=-1), session=targetSession), function(res) do.call(cbind, res[!names(res) == "tags"])))

}


syncAtypes <- function(sourceSession, targetSession, sourceAccounts) {

    ## Get the source atypes:
    sourceAtypes <- as.data.frame(getResource("analyticaltypes", params=list("format"="csv", "page_size"=-1), session=sourceSession))

    ## Filter the atypes:
    sourceAtypes <- sourceAtypes[na.omit(match(sourceAccounts[, "atype"], sourceAtypes[, "id"])), ]

    ## Overwrite resource fields:
    for (fld in c("id", "created", "creator", "updated", "updater")) {
        sourceAtypes[, fld] <- NULL
    }

    ## Create the payload:
    payload <- toJSON(list("analyticaltypes"=sourceAtypes), auto_unbox=TRUE, na="null", digits=10)

    ## Push the payload:
    response <- pushPayload(payload=payload, endpoint=NULL, session=targetSession, import=FALSE, inbulk=TRUE, params=list(sync="True"))

    ## Return:
    sourceAtypes

}


syncInstitutions <- function(sourceSession, targetSession, sourceAccounts) {

    ## Get the source atypes:
    sourceInstitutions <- as.data.frame(getResource("institutions", params=list("format"="csv", "page_size"=-1), session=sourceSession))

    ## Filter the atypes:
    sourceInstitutions <- sourceInstitutions[na.omit(match(sourceAccounts[, "custodian"], sourceInstitutions[, "id"])), ]

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
    accounts[, "custodian"] <- paste0("dcf:custodian?guid=", institutions[, "guid"])

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
