##' A function to compare NAV's between 2 decaf systems.
##'
##' @param accounts The account mapping list.
##' @param type The type of container. Either 'accounts' or 'portfolios'.
##' @param ccy The currency to be valued.
##' @param date The date of the consolidations.
##' @param sSession The source session.
##' @param tSession The target session.
##' @param tDeployment The name of the target deployment.
##' @param sDeployment The name of the source deployment.
##' @param charLimit The character limit for names.
##' @return A data frame with the NAv comparisons.
##' @export
##'
decafNAVComparison <- function(accounts,
                               type,
                               ccy,
                               date,
                               sSession,
                               tSession,
                               tDeployment,
                               sDeployment,
                               charLimit=100) {

    ## Get the source resources:
    sResources <- getSystemResources(sSession)

    ## Get the target resources:
    tResources <- getSystemResources(tSession)

    ## Assing main function to shorter variable name:
    .gcfcn <-getConsolidationFromContainerName
    .trcon <- trimConcatenate

    ## Get the account consolidations for source:
    sCons <- .gcfcn("containerNames"=names(accounts),
                    "containerType"=type,
                    "ccy"=ccy,
                    "date"=date,
                    "session"=sSession,
                    "resources"=sResources,
                    "charLimit"=charLimit)

    ## Get the portfolio consolidations for target:
    tCons <- .gcfcn("containerNames"=sapply(accounts, function(x) x[[substr(type, 1, nchar(type)-1)]]),
                    "containerType"=type,
                    "ccy"=ccy,
                    "date"=date,
                    "session"=tSession,
                    "resources"=tResources,
                    "charLimit"=charLimit)

    ## Get the target container names:
    tConsNames <- sapply(tCons, function(x) x[1, "CName"])

    ## Get the source container names:
    sConsNames <- sapply(sCons, function(x) x[1, "CName"])

    ## Align the source and target container consolidations:
    sConsNames <- sapply(accounts[match(sConsNames, names(accounts))], function(x) x[[substr(type, 1, nchar(type)-1)]])
    sCons <- sCons[match(tConsNames, sConsNames)]

    ## Get the reconciliation data-frame:
    reconciliation <- do.call(rbind, lapply(1:length(sCons), function(i) {
        data.frame("target NAV"=safeTry(try(sum(tCons[[i]][, "Value"]), silent=TRUE)),
                   "source NAV"=safeTry(try(sum(sCons[[i]][, "Value"]), silent=TRUE)),
                   "target Name"=safeNull(tCons[[i]][1, "CName"]),
                   "source Name"=safeNull(sCons[[i]][1, "CName"]),
                   "target ID"=tCons[[i]][1, capitalise(substr(type, 1, nchar(type)-1))],
                   "diff"=safeTry(try(sum(tCons[[i]][, "Value"]) / sum(sCons[[i]][, "Value"]) - 1, silent=TRUE)),
                   check.names=FALSE)
    }))

    ## Generate a presentable table:
    reconciliation <- data.frame("NAVt"=beautify(reconciliation[, "target NAV"]),
                                 "NAVs"=beautify(reconciliation[, "source NAV"]),
                                 "Name"=reconciliation[, "target Name"],
                                 "Diff (%)"=percentify(reconciliation[, "diff"], 3),
                                 "ID"=reconciliation[, "target ID"],
                                 check.names=FALSE)

    ## Name the columns:
    colnames(reconciliation) <- c(sprintf("NAV (%s)", c(tDeployment)),
                                  sprintf("NAV (%s)", c(sDeployment)),
                                  sprintf("Name (%s)", c(tDeployment)),
                                  "Diff (%)",
                                  "ID")

    ## Done, return:
    return(reconciliation)

}


##' A function to infer dubious user login behaviour.
##'
##' @param data The data frame.
##' @param configJSON The json config file
##' @return A user wise list with the user login analysis.
##' @export
##'
userMultipleCountryLogin <- function(data, configJSON) {

    ## Get the config list
    config <- fromJSON(configJSON)

    ## Exclude countries if any:
    data <- data[is.na(match(as.character(data[, "country_name"]), config[["excl_countries"]])), ]

    ## Get the date:
    data[, "date"] <- substr(as.character(data[, "datetime"]), 1, 10)

    ## Get user-wise list:
    userWise <- extractToList(data, "username")

    ## For each user, aggregate country name by date:
    result <- lapply(userWise, function(user) aggregate(user[, "country_name"], list(user[, "date"]), function(x) {unique(as.character(x))}))

    ## Empty data:
    data <- NULL

    ## Clean:
    gc()

    result <- lapply(1:length(result), function(i) {

        ## Assign column names:
        colnames(result[[i]]) <- c("date", "countries")

        value <- sapply(result[[i]][, "countries"], function(x) paste(x, collapse=", "))
        date <- result[[i]][, "date"]

        ## Get the user specific exemptions:
        exempt <- safeNull(config[["exemptions"]][[names(result)[i]]], getRandString())

        ## Get the country count:
        countryCount <- table(unlist(result[[i]][, 2]))

        ## Infer the base country:
        baseCountry <- names(countryCount)[countryCount == max(countryCount)[1]]

        ## Get the dubious index:
        isDubious <- sapply(result[[i]][, 2], function(x) length(x) > 1)

        if (all(!isDubious)) {
            return(list("is_dubious"=FALSE, "was_exempted"=FALSE, "dubious_data"=NULL,
                        "exemptions"=config[["exemptions"]][[names(result)[i]]]))
        }

        ## Get the dubious countries:
        result[[i]][isDubious, "dubiousCountry"] <- sapply(result[[i]][isDubious, 2], function(x) paste0(x[x != baseCountry], collapse=", "))

        ## Get the dubious data frame:
        dubious <- result[[i]][isDubious, ]

        ## Exclude exemptions:
        isExempted <- !apply(mgrep(dubious[, "dubiousCountry"], exempt), MARGIN=1, function(x) all(x == "0"))

        ## Return if all are exempted:
        if (all(isExempted)) {
            return(list("is_dubious"=FALSE, "was_exempted"=TRUE, "dubious_data"=NULL,
                        "exemptions"=config[["exemptions"]][[names(result)[i]]]))

        }

        ## Get the dubious lines:
        dubious <- dubious[!isExempted, ]

        ## Name the columns:
        colnames(dubious) <- c("date", "countries", "dubiousCountry")

        ## Done, return:
        list("is_dubious"=TRUE, "was_exempted"=any(isExempted), "dubious_data"=dubious,
             "exemptions"=config[["exemptions"]][[names(result)[i]]])

    })

    ## Assign names:
    names(result) <- names(userWise)

    return(result)
}


##' A function to inspect the breaches in the latest compliance checks.
##'
##' @param resources The resources data frame.
##' @param session The rdecaf session.
##' @param occurThreshold For common instrument culprits, what is the occurance threshold? Default = 2.
##' @return A list with the inspected data.
##' @export
##'
complianceBreachInspector <- function(resources, session, occurThreshold=2) {

    ## Define the column names for the result:
    resultColnames <- c("Common ID",
                        "Link",
                        "Name",
                        "Symbol",
                        "ISIN",
                        "Asset Class",
                        "Portfolio")

    ## Get the compliance checks:
    complChecks <- getResource("compliancechecks", params=list("page_size"=-1), session=session)[[1]]

    ## Get the compliance check items:
    complCheckitems <- getResource("compliancecheckitems", params=list("page_size"=-1, "parent"=complChecks[["id"]]), session=session)

    ## Get the failed compliance checks:
    complCheckitems <- complCheckitems[!sapply(complCheckitems, function(x) x[["passed"]])]

    ## Get the compliance checks which failed and flatten:
    complCheckitems <- do.call(rbind, lapply(1:length(complCheckitems), function(i) {

        compl <- complCheckitems[[i]][["auxdata"]]
        portf <- complCheckitems[[i]][["portfolio"]]

        do.call(rbind, lapply(compl[["groups"]], function(x) {

            !length(x[["items"]]) == 0 || return(NULL)

            data.frame(do.call(rbind, x[["items"]]),
                       "passed"=x[["passed"]],
                       "portfolio"=safeNull(portf))
        }))

    }))

    ## Get the portfolios:
    portfolios <- as.data.frame(getResource("portfolios", params=list("format"="csv", "page_size"=-1), session=session))

    ## Replace portfolio id with portfolio name:
    complCheckitems[, "portfolio"] <- portfolios[match(complCheckitems[, "portfolio"], portfolios[, "id"]), "name"]

    ## Get the failed groups:
    failedGroups <- complCheckitems[!complCheckitems[, "passed"], ]

    ## If no group failed, return:
    if (NROW(failedGroups) == 0) {
        return(list("complianceCheckitems"=complCheckitems,
                    "failedGroups"=NULL,
                    "culpritsWithoutAClass"=initDF(resultColnames),
                    "culpritsWithAClass"=initDF(resultColnames)))
    }

    ## Get the asset class:
    failedGroups[, "assetclass"] <- resources[match(unlist(failedGroups[, "symbol"]), resources[, "symbol"]), "assetclass"]

    ## Get the isin:
    failedGroups[, "isin"] <- resources[match(unlist(failedGroups[, "symbol"]), resources[, "symbol"]), "isin"]

    ## Compose the custom ID:
    failedGroups[, "customID"] <- as.character(unlist(ifelse(isNAorEmpty(failedGroups[, "isin"]), failedGroups[, "symbol"], failedGroups[, "isin"])))

    ## Compute the occurances:
    occurances <- table(unlist(failedGroups[, "symbol"]))

    ## Assign the occurances:
    failedGroups[, "occurances"] <- as.numeric(occurances[match(unlist(failedGroups[, "symbol"]), names(occurances))])

    ## Extract by custom id:
    idWise <- extractToList(failedGroups, "customID")

    ## Order list:
    idWise <- idWise[order(as.numeric(lapply(idWise, NROW)), decreasing=TRUE)]

    ## Exclude rows with less than N occurances:
    idWise <- idWise[sapply(idWise, function(x) NROW(x) >= occurThreshold)]

    if (length(idWise) == 0) {
        return(list("complianceCheckitems"=complCheckitems,
                    "failedGroups"=failedGroups,
                    "culpritsWithoutAClass"=initDF(resultColnames),
                    "culpritsWithAClass"=initDF(resultColnames)))
    }

    ## Remove duplidates:
    idWise <- lapply(idWise, function(x) x[!duplicated(x[, "symbol"]), ])

    ## Prepare the data frame in the list:
    idWise <- lapply(idWise, function(x) data.frame("Common ID"=ellipsify(as.character(c(x[1, "customID"], rep("", NROW(x)-1)))),
                                                    "Link"=paste0(gsub("api", "", session[["location"]]), "resource/details/", as.character(x[, "id"])),
                                                    "Name"=ellipsify(as.character(x[, "name"])),
                                                    "Symbol"=ellipsify(as.character(x[, "symbol"])),
                                                    "ISIN"=as.character(x[, "isin"]),
                                                    "Asset Class"=as.character(x[, "assetclass"]),
                                                    "Portfolio"=as.character(x[, "portfolio"]),
                                                    check.names=FALSE,
                                                    stringsAsFactors=FALSE))

    ## Get the list elements which have NA asset class:
    naAclss <- unlist(sapply(idWise, function(x) any(is.na(x[, "Asset Class"]))))

    ## Get the data frame without asset classes:
    nonAClss <- do.call(rbind, idWise[naAclss])
    rownames(nonAClss) <- NULL


    ## Get the data frame with asset classes:
    hasAClss <- do.call(rbind, idWise[!naAclss])
    rownames(hasAClss) <- NULL

    ## Done, return list:
    list("complianceCheckitems"=complCheckitems,
         "failedGroups"=failedGroups,
         "culpritsWithoutAClass"=nonAClss,
         "culpritsWithAClass"=hasAClss)

}



##' A function to compute the aggregate exposure of holdings data frame.
##'
##' @param holdings A data frame with column names.
##' @param keys The keys to be aggregated.
##' @return A list with the exposures
##' @export
##'
getAggregateExposure <- function(holdings, keys) {

    exposures <- lapply(keys, function(x) {
        holdings[, x] <- ifelse(is.na(holdings[, x]), "NOTAVAILABLE", holdings[, x])
        aggs <- aggregate(numerize(if_else(is.na(holdings[, "Exposure"]),0,holdings[, "Exposure"])), list(holdings[, x]), sum)
        aggs <- aggs[order(aggs[, 2], decreasing=TRUE), ]
        colnames(aggs) <- c(x, "Exposure")
        aggs
    })

    names(exposures) <- keys

    ## Done, return:
    exposures
}


##' A function to compute the periodic returns of xts price.
##'
##' @param price An xts price vector.
##' @return A xts returns data frame.
##' @export
##'
computePeriodicReturns <- function(price) {

    ## normalized
    ## Compute the discrete returns:
    returns <- PerformanceAnalytics::CalculateReturns(price, method = c("discrete"))

    ## Set NA returns to 0:
    returns[is.na(returns)] <- 0

    ## Append monthly and yearly columns:
    returns <- cbind("raw"=returns, "monthly"=0, "yearly"=0)

    auxFun <- function(returns, lengths) {

        rets <- NULL

        i <- 1
        add <- 0

        for (len in lengths) {
            rets <- c(rets, tail(cumprod(1 + returns[i:(len + add), "raw"]) -1, 1))
            i <- i + len
            add <- len
        }

        return(rets)
    }

    ## Compute the yearly returns:
    yearlyIdx <- xts::apply.yearly(returns[, "raw"], function(x) length(x))
    yearlyReturns <- auxFun(returns[, "raw"], as.numeric(yearlyIdx))

    ## Compute the monthly returns:
    monthlyIdx <- xts::apply.monthly(returns[, "raw"], function(x) length(x))
    monthlyReturns <- auxFun(returns[, "raw"], as.numeric(monthlyIdx))

    ## Assign the monthly returns to the daily return data frame:
    returns[!is.na(match(zoo::index(returns), as.Date(zoo::index(monthlyIdx)))), "monthly"] <- as.numeric(monthlyReturns)

    ## Assign the yearly returns to the daily return data frame:
    returns[!is.na(match(zoo::index(returns), as.Date(zoo::index(yearlyIdx)))), "yearly"] <- as.numeric(yearlyReturns)

    ## Compute the cumulative returns:
    returns <- cbind(returns, "cumrets"=cumprod(1 + as.numeric(returns[, "raw"]))-1)

    ## Done, return
    return(returns)

}


##' This function to get the usage metrics.
##'
##' @param userActions The user actions result.
##' @param tokenSession The token session data frame.
##' @return A xts usage metrics data frame.
##' @export
##'
getUsageMetrics <- function(userActions, tokenSession) {

    usages[, "Created"] <- as.POSIXct(sapply(strsplit(as.character(usages[, "Created"]), "\\."), function(x) gsub("T", " ", x[1])))

    xtsElapsed <- do.call(cbind, lapply(extractToList(tokenSession, "Username"), function(x) {
        aggs <- aggregate(x[, "Elapsed"], list(as.Date(x[, "Created"])), sum)
        xts::as.xts(aggs[, 2], order.by=aggs[, 1])
    }))

    colnames(xtsElapsed) <- unique(tokenSession[, "Username"])

    xtsMetrics <- lapply(names(userActions), function(user) {

        if (user == "AUTO") {
            return(NULL)
        }

        df <- cbind(userActions[[user]],
                    userActions[[user]][, "Created"] + userActions[[user]][, "Updated"],
                    xtsElapsed[, user])

        colnames(df) <- c("created", "updated", "total", "elapsed")

        df[, "elapsed"] <- round(df[, "elapsed"] / 3600, 2) * 0.5

        df[is.na(df)] <- 0

        metric <- df[, "elapsed"] / df[, "total"]

        metric[is.infinite(metric)] <- 0

        metric[is.na(metric)] <- 0

        metric <- round(zoo::rollmean(metric, 20), 2)

        list("metric"=metric,
             "elapsed"=round(zoo::rollmean(df[, "elapsed"], 20), 2),
             "created"=round(zoo::rollmean(df[, "created"], 20), 2),
             "updated"=round(zoo::rollmean(df[, "updated"], 20), 2),
             "total"=round(zoo::rollmean(df[, "total"], 20), 2))
    })

    names(xtsMetrics) <- names(userActions)

    ## Done, return
    xtsMetrics

}


##' This function gets the historical user actions in a deployment.
##'
##' @param session The rdecaf session.
##' @param endpoint The endpoint to investigate, i.e "trades", "quants".
##' @param exclCols The columns to exclude.
##' @param exclKeys The keys to exclude from exclCols.
##' @param asof Addtional query parameters to be passed to params.
##' @return A table with the automation rate.
##' @export
##'
getHistoricalUserActions <- function(session, endpoint, sample=1000, exclCols=NULL, exclKeys=NULL, asof=NULL) {

    ## Action params:
    params <- c(list("page_size"=sample, "format"="csv", "created__lte"=asof))

    ## Get actions:
    actions <- as.data.frame(getResource(endpoint, params=params, session=session))

    if (NROW(actions) == 0) {
        return(NULL)
    }

    ## Get users:
    users <- as.data.frame(getResource("users", params=list("page_size"=-1, "format"="csv"), session=session))

    ## Replace creator id with creator username:
    actions[, "creator"] <- users[match(actions[, "creator"], users[, "id"]), "username"]

    ## Replace NA creators with "AUTO"
    actions[, "creator"] <- ifelse(is.na(actions[, "creator"]), "AUTO", actions[, "creator"])

    ## Replace updater id with updater username:
    actions[, "updater"] <- users[match(actions[, "updater"], users[, "id"]), "username"]

    ## Replace NA updaters with "AUTO"
    actions[, "updater"] <- ifelse(is.na(actions[, "updater"]), "AUTO", actions[, "updater"])

    ## Parse the created date:
    actions[, "created"] <- as.POSIXct(sapply(strsplit(as.character(actions[, "created"]), "\\."), function(x) gsub("T", " ", x[1])))

    ## Parse the uupdated date:
    actions[, "updated"] <- as.POSIXct(sapply(strsplit(as.character(actions[, "updated"]), "\\."), function(x) gsub("T", " ", x[1])))

    ## Append the is updated column:
    actions[, "isUpdated"] <- actions[, "created"] != actions[, "updated"]

    ## Exclude keys in columns:
    if (!is.null(exclCols)) {
        actions <- actions[!mCondition(actions, exclCols, exclKeys), ]
    }

    ## Get the unique users:
    uniqueUsers <- unique(as.character(unlist(actions[, c("creator", "updater")])))

    ## Compute the actions for each user:
    actionHistory <- lapply(uniqueUsers, function(x) {
        df <- actions
        df[, "createdByUser"] <- (df[, "creator"] == x)
        df[, "updatedByUser"] <- (df[, "updater"] == x & df[, "isUpdated"])

        tsC <- aggregate(df[, "createdByUser"], list(as.Date(df[, "created"])), sum)
        tsU <- aggregate(df[, "updatedByUser"], list(as.Date(df[, "updated"])), sum)

        tsC <- xts::as.xts(tsC[, 2], order.by=as.Date(tsC[, 1]))
        tsU <- xts::as.xts(tsU[, 2], order.by=as.Date(tsU[, 1]))

        ts <- cbind("Created"=tsC, "Updated"=tsU)
        ts[is.na(ts)] <- 0

        ts

    })

    ## Empty users are AUTO:
    uniqueUsers[isNAorEmpty(uniqueUsers)] <- "AUTO"

    ## Add the names:
    names(actionHistory) <- uniqueUsers

    ## Done, return:
    actionHistory
}


##' This function computes the automation rate for users.
##'
##' @param userActions The data frame with the use actions.
##' @return A table with the automation rate.
##' @export
##'
computeAutomationRate <- function(userActions, includeAUTO=TRUE) {

    if (is.null(userActions)) {
        return(NULL)
    }

    ## Get the user names:
    users <- names(userActions)

    ## Compute the action counts:
    aTable <- as.data.frame(do.call(rbind, lapply(userActions, colSums)))

    if (!includeAUTO) {
        aTable <- aTable[rownames(aTable) != "AUTO", ]
        users <- users[!users == "AUTO"]
    }

    ## Sum the rows of the action counts:
    aTable[, "Total"] <- as.numeric(rowSums(aTable))

    ## Compute the percentages:
    percentages <-  apply(aTable, MARGIN=2, function(x) x / sum(x))

    if (class(percentages) == "numeric") {
        percentages <- as.data.frame(t(percentages))
    }

    ## Add column names:
    colnames(percentages) <- paste0(colnames(aTable), " (%)")

    ## Prepare return table:
    aTable <- cbind("User"=users, aTable, percentages)

    ## Get rid of row names:
    rownames(aTable) <- NULL

    ## Done, return:
    aTable
}



##' This function computes the ytd monthly returns using the pconsolidation's internal navs.
##'
##' @param holdings The holdings data frame from getFlatHoldings.
##' @param ccy The portfolio currency.
##' @param resources The resources data frame.
##' @return The fx forward hedge adjusted currency exposure
##' @export
##'
computeCurrencyExposure <- function(holdings, ccy, resources) {

    ## Get the FX Forwards holdings:
    fxfwd <- holdings[holdings[, "Type"] == "FX Forward", ]

    ## Get the main and alternative currency of the forwards:
    resCCYs <- resources[match(fxfwd[, "ID"], resources[, "id"]), c("ccymain", "ccyaltn")]

    ## Get the non FX Forward holdings:
    holds <- holdings[holdings[, "Type"] != "FX Forward", ]

    ## Aggregate exposure by currency:
    aggs <- aggregate(as.numeric(holds[, "Exposure"]), list(holds[, "CCY"]), sum)

    ## Compute the percentages:
    aggs[,2] <- aggs[,2] / sum(holds[, "Exposure"])

    ## If no FX Forward holding, create a ficitios data frame:
    if (NROW(fxfwd) == 0) {
        ccyExp <- data.frame("Currency"=aggs[,1], "Exposure"=percentify(aggs[, 2]))
        ccyExp <- ccyExp[ccyExp[,2] != "0 %", ]
        ccyExp <- ccyExp[ccyExp[,1] != "XAU", ]
        ccyExp <- ccyExp[ccyExp[,1] != "XAG", ]
        ccyExp <- ccyExp[ccyExp[,1] != "XPT", ]
        ## Set the column to NULL:
        colnames(ccyExp) <- NULL
        ## Set the rows to NULL:
        rownames(ccyExp) <- NULL

        return(ccyExp)

    }

    ## Infer the non portfolio currency and append:
    riskCCY <- lapply(1:NROW(resCCYs), function(i) resCCYs[i, is.na(match(resCCYs[i, ], ccy))])
    fxfwd[, "XCCY"] <- sapply(riskCCY, function(x) ifelse(length(x) > 1, NA, x))

    ## If FX Forward holdings exists, compute it's exposure:
    fxfwd[, "expPer"] <- (fxfwd[, "Exposure"] - fxfwd[, "Value"]) / sum(holds[, "Exposure"])

    ## Aggregate fx forward exposure by currency:
    fxAgg <- aggregate(fxfwd[, "expPer"], list(fxfwd[, "XCCY"]), sum)

    ## Compute the addjustment based on fx fowards aggregates:
    aggs[, "adj"] <- fxAgg[match(aggs[,1], fxAgg[,1]), 2]

    ## Set NA's to 0:
    aggs[, "adj"] <- ifelse(is.na(aggs[, "adj"]), 0, aggs[, "adj"])

    ## Match the currencies with the portfolio currency:
    xMatch <- match(ccy, aggs[,1])

    ## If no matching currency, assume no hedge of FX Forward:
    if (all(is.na(xMatch))) {
        aggs[, "adj"] <- 0
    } else {
        ## If match, compute the adjustment to exposure to the FX Forward hedge:
        aggs[xMatch, "adj"] <- sum(fxAgg[, 2]) * -1
    }

    ## Compute the final exposure:
    aggs[, "final"] <- aggs[,2] + aggs[, 3]

    ## Prepare and parse the table:
    ccyExp <- data.frame("Currency"=aggs[,1], "Exposure"=percentify(aggs[, "final"]))
    ccyExp <- ccyExp[ccyExp[,2] != "0 %", ]
    ccyExp <- ccyExp[ccyExp[,1] != "XAU", ]
    ccyExp <- ccyExp[ccyExp[,1] != "XAG", ]
    ccyExp <- ccyExp[ccyExp[,1] != "XPT", ]

    ## Set the column to NULL:
    colnames(ccyExp) <- NULL
    ## Set the rows to NULL:
    rownames(ccyExp) <- NULL

    ## Done, return:
    ccyExp

}


##' This function computes the ytd monthly returns using the pconsolidation's internal navs.
##'
##' @param intPrice The internal price series.
##' @param date The date of the report.
##' @param session The rdecaf session.
##' @return A list with the return objects.
##' @export
##'
getInternalMonthlyReturns <- function(intPrice, date, session) {

    ## The start date:
    startDate <- dateOfPeriod("Y-0", date)

    ## Get the month period dates:
    monthlyDates <- periodDates("M", date=date)

    ## Filter out dates earlier than the start date:
    monthlyDates <- monthlyDates[monthlyDates >= startDate]

    ## XTS the internal price:
    intPrice <- xts::as.xts(intPrice[, "price"], order.by=as.Date(intPrice[, "date"]))

    ## Filter in relevant dates:
    intPrice <- intPrice[zoo::index(intPrice) >= startDate, ]

    ##:
    intPrice[, 1] <- 100 * cumprod(c(1, head(c(as.numeric(tail(intPrice, -1)), 1) / as.numeric(intPrice), -1)))

    ## If the internal price start date is later than the period start date,
    ## create sequence till internal start date:
    if (zoo::index(intPrice)[1] > startDate) {

        ## Create the date series to be appended:
        new <- seq(startDate, zoo::index(intPrice)[1], 1)

        ## Remove last date:
        new <- new[-length(new)]

        ## Append the additional dates and prices:
        intPrice <- rbind(xts::as.xts(rep(100, length(new)), order.by=new), intPrice)

    }

    ## Get the internal returns:
    ## returns <- diff(log(intPrice))
    returns <- PerformanceAnalytics::CalculateReturns(intPrice, method = c("discrete"))

    ## Set NA returns to 0:
    returns[is.na(returns)] <- 0

    ## Append periodic column:
    returns <- cbind("internal"=returns, "periodic"=0)

    ## Get the dates to be handled:
    months <- unique(c(monthlyDates, date))

    ## Fill the missing ohlc returns with internal monthly returns:
    for (i in 1:(length(months)-1)) {

        mIdx <- which(zoo::index(returns) == months[i+1])

        if (length(mIdx) == 0) {
            mIdx <- NROW(returns)
        }

        nIdx <- which(zoo::index(returns) == months[i])
        returns[mIdx, "periodic"] <- as.numeric(tail(cumprod(1+returns[(mIdx):(nIdx+1), "internal"]) - 1, 1))

    }

    ## Compute the cumulative returns:
    returns <- cbind(returns, "cumrets"=cumprod(1 + as.numeric(returns[, "internal"]))-1)

    ## Initialse the Monthly table:
    df <- initDF(c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

    ## Compute the monthly returns:
    ## mrets <- as.numeric(xts::apply.monthly(returns[, "periodic"], sum))[-1]
    if (all(returns[, "periodic"] == 0)) {
        mrets <- rep(NA, length(months))
    } else {
        mrets <- returns[returns[, "periodic"] != 0, "periodic"]
        monthVerb <- toupper(format(zoo::index(mrets), "%b"))
        mrets <- as.numeric(mrets)
    }

    ## Assign monthly returns to the monthly table:
    df[!is.na(match(colnames(df), monthVerb))] <- mrets

    ## df[1:length(mrets)] <- mrets

    ## Parse the monthly returns:
    monthlyrets <- t(data.frame(paste0(sprintf("%.2f", df*100), " %")))

    ## Remove strings:
    monthlyrets[monthlyrets=="NA %"] <- "   "

    ## Rename columns:
    colnames(monthlyrets) <- colnames(df)
    rownames(monthlyrets) <- NULL

    ## Get the YTD
    ytd <- (as.numeric(tail(intPrice, 1)) - 100) / 100

    ## Done, return:
    list("mretsRaw"=df,
         "mretsTable"=monthlyrets,
         "ytdRaw"=ytd,
         "ytdPrint"=percentify(ytd, 2),
         "returns"=returns)
}


##' This function removes NA's and optionally detects and remove outliers.
##'
##' @param df The data frame with the price and date columns.
##' @param dCol The column name of the date.
##' @param pCol The column name of the price.
##' @param quantile The quantile for the outlier detection
##' @param surpressPlot TODO
##' @return Cleaned price series
##' @import tseries
##' @export
##'
treatPriceSeries <- function(df, dCol, pCol, quantile=0.998, surpressPlot=FALSE){

    ## Return NULL if df is NULL:
    if (is.null(df)) {
        return(df)
    }

    ## Return NULL if not enought observations:
    if (NROW(df) < 5) {
        return(df)
    }

    ## Ensure price column is numeric:
    df[, pCol] <- as.numeric(df[, pCol])

    ## Ensure no zeros:
    df[df[, pCol] == 0, pCol] <- 0.00001

    ## Remove NA's and replace with last observable value:
    price <- xts::as.xts(df[, pCol], order.by=as.Date(df[, dCol]))

    ## Store the orignal NA removed series:
    originalPrice <- price

    ## Compute the original returns using the simple method:
    originalRets <- as.numeric(timeSeries::returns(price, "simple"))

    ## Replace NA for first observations with the mean return:
    originalRets[is.na(originalRets)] <- 0

    ## Compute the returns for our analysis using compoung method:
    rets  <- as.numeric(timeSeries::returns(price, "compound"))

    ## Replace NA for first observations with the mean return:
    ## rets[is.na(rets)]  <- as.numeric(mean(na.omit(rets)))
    rets[is.na(rets)]  <- 0

    ## Create the GARCH(1,1) object using fixed a0, a1, b1 parameters:
    garchObj <- list("order"=c("p"=1, "q"=1),
                     "residuals"=rets*100,
                     "call"=call("garch", x=rets, order = c(1, 1)),
                     "coef"=c("a0"=0, "a1"=0.08, "b1"=0.90),
                     "series"=as.character("rets"))

    ## Assign the GARCH class:
    class(garchObj) <- "garch"

    ## library(tseries)

    ## Fit the GARCH object to the returns and get the estimated conditional volatility:
    fittedGarch <- predict(garchObj, rets)[,1]

    ## First estimate will be our square-root of squared return:
    fittedGarch[1] <- sqrt(rets[1]^2)

    ## Compute the residuals, squared returns less the estimated variance:
    residuals <- rets^2 - fittedGarch^2

    ## Compute the density of the residuals:
    resDensity <- density(residuals)

    ## Sample the residuals using the density bandwidth parameter:
    resDraw <- rnorm(100000, sample(as.numeric(residuals), size = 100000, replace = TRUE), resDensity$bw)

    ## Calculate the cut-off residuals using quantile parameter:
    cutOff <- quantile(resDraw, quantile)

    ## Get the index of outliers:
    outlierIndex  <- which(residuals^2 > cutOff)

    ## Set original return outlier index to zero:
    originalRets[outlierIndex] <- 0

    ## Reconstruct the price series:
    price <- xts::as.xts(cumprod(1 + originalRets) * as.numeric(originalPrice[1]), order.by=zoo::index(originalPrice))

    if (!surpressPlot) {
        ## Run the outlier plot:
        outlierPlot(originalPrice, price, outlierIndex)
        readline(prompt = "Pause. Press <Enter> to continue...")
        dev.off()
    }

    ## Reassign the treated price series:
    df[, pCol] <- as.numeric(price)[match(as.Date(df[, dCol]), zoo::index(price))]

    ## Done, return:
    df
}


##' A function to prepare a data frame with all positions by ctype and holding period
##'
##' This is the description
##'
##' @param ctype The ctype.
##' @param resources The rdecaf resources data frame.
##' @param portfolios The rdecaf portfolios data frame.
##' @param accounts The rdecaf accounts data frame.
##' @param session The rdecaf session.
##' @return A data frame with all holding periods.
##' @export
getOpenPositionByCtype <- function(ctype, resources, portfolios, accounts, session) {

    ## Get the future resources:
    ctypeResources <- resources[resources[, "ctype"] == ctype, ]

    ## Get the trades by futures:
    ctypeTrades <- lapply(ctypeResources[, "id"], function(id) as.data.frame(getResource("trades", params=list(format="csv", page_size=-1, resmain=id), session=session)))

    ## Extract each future trade by account:
    openPositionData <- do.call(rbind, lapply(ctypeTrades, function(ctypeTrade) {

        ## If no trade by account, return NULL:
        if (NROW(ctypeTrade) == 0){
            return(NULL)
        }

        ## For each future, extract trades by account and prepare open future position data:
        do.call(rbind, lapply(extractToList(ctypeTrade, "accmain"), function(x) {

            ## Order trades data frame by commitment:
            x <- x[order(x[, "commitment"]), ]

            ## Compute the positions:
            x[, "position"] <- cumsum(x[, "qtymain"])

            ## Append the accmain:
            x[, "account"] <- paste(x[, "accmain"], resources[match(x[, "resmain"], resources[, "id"]), "isin"], "PnL Acc")

            ## Append the security ccy:
            x[, "secccy"] <- resources[match(x[, "resmain"], resources[, "id"]), "ccymain"]

            ## Append the closing trades:
            x[x[, "position"] == 0, "closes"] <- as.character(x[x[, "position"] == 0, "commitment"])

            ## Compute the open, close index values:
            indices <- c(-1, diff(abs(x[, "position"]) == 0))

            ## Get the open indices:
            startIdx <- which(indices == -1)

            ## Get the close indices:
            endIdx <- which(indices == 1)

            ## If no close, set to NA:
            if (length(endIdx) ==0) {
                endIdx <- NA
            }

            ## Flatten the open position period and data:
            retval <- do.call(rbind, lapply(1:length(startIdx), function(i) data.frame("open"=x[startIdx[i], "commitment"],
                                                                                       "close"=x[endIdx[i], "commitment"],
                                                                                       "pnlAcct"=x[startIdx[i], "account"],
                                                                                       "secccy"=x[startIdx[i], "secccy"],
                                                                                       "portfolio"=accounts[match(x[, "accmain"][1], accounts[, "id"]), "portfolio"],
                                                                                       "accmain"=x[startIdx[i], "accmain"],
                                                                                       "resmain"=x[startIdx[i], "resmain"])))

            ## Append the reference currency:
            retval <- data.frame(retval, "rccy"=portfolios[match(retval[, "portfolio"], portfolios[, "id"]), "rccy"])

            ## Done, return:
            retval

        }))
    }))

    ## Done, return:
    return(openPositionData)
}


##' A function to retrieve and slice OHLC observations as per given period.
##'
##' This is the description
##'
##' @param symbols A vector of ohlcs symbols.
##' @param session The rdecaf session.
##' @param date The report date.
##' @param periods A vector with desired period slices: 'DTD', 'WTD', 'MTD', 'QTD', 'YTD'
##' @param excludeWeekends Should the weekends be excluded? Default to TRUE.
##' @return A list with the slices per period.
##' @export
getSlicedOhlcs <- function(symbols, session, date, periods, excludeWeekends=TRUE) {

    ## Get the dateOfPeriod memnonic:
    periodMemnonic <- sapply(periods, function(x) switch(x,
                                                        "YTD"="Y-0",
                                                        "QTD"="Q-0",
                                                        "MTD"="M-0",
                                                        "WTD"="W-1",
                                                        "DTD"="D-0"))

    ## Order the periods:
    periodMemnonic <- orderByKey(periodMemnonic, c("Y", "Q", "M", "W", "D"))

    ## Compute the look back:
    lookBack <- as.numeric(date - dateOfPeriod(periodMemnonic[1], date)) + 27

    ## Get the ohlcs:
    ohlcs <- lapply(symbols, function(s) getOhlcObsForSymbol(session, s, lte=date, lookBack=lookBack, excludeWeekends))

    ## Construct the functions:
    myFun <- paste0("get", periods, "Slice")

    ## Get the period ohlcs:
    periodOhlcs <- lapply(myFun, function(fun) lapply(ohlcs, function(y) do.call(fun, list(y, date))))

    ## Assign the symbols as names to each period list:
    for (i in 1:length(periods)) {
        names(periodOhlcs[[i]]) <- symbols
    }

    ## Assign the names:
    names(periodOhlcs) <- periods

    ## Done, return:
    periodOhlcs

}

##' A function used in computeReturnStats for benchmark metrics, e.g. treynor.
##'
##' This is the description
##'
##' @param data a df with ts columns (benchmark and container column names)
##' @param minobs min required obs to run the advanced statitistics. defaults to 30
##' @return A list with key metrics.
##' @export
benchmarkAnalysis <- function(data, minobs=30) {

        retval <- list("correlation"=NA,
                       "relativeReturn"=NA,
                       "modelAlpha"=NA,
                       "modelAlphaSign"=NA,
                       "modelBeta"=NA,
                       "modelBetaSign"=NA,
                       "lm"=NA,
                       "treynor"=NA,
                       "return"=NA,
                       "volatility"=NA)
                       
        data1 <- data[!is.na(data[, "container"]), ]  
        data1 <- data1[!is.na(data1[, "benchmark"]), ]             

        if (is.null(data1)||NCOL(data1) == 1||NROW(data1)<minobs) {
            return(retval)
        }
        
        retPf <- as.numeric(tail(as.numeric(data[,"container"]), 1) / head(as.numeric(data[,"container"]), 1) - 1)
        bRet <-  as.numeric(tail(as.numeric(data[!is.na(data$benchmark),]$benchmark), 1) / head(as.numeric(data[!is.na(data$benchmark),]$benchmark), 1) - 1)

        data <- treatPriceSeries(data[!is.na(data$benchmark),], "date", "benchmark", quantile=.998, surpressPlot=TRUE) 

        bRets <- data %>%
          mutate_if(is.numeric, function(x) c(NA,{diff(log(x))})) %>%
          slice(2:NROW(.)) 
        
        #bRets <- bRets[abs(bRets[, 1]) < sd(bRets[, 1]) * 4, ] #need to add control for nrow == 0 
        #bRets <- bRets[abs(bRets[, 2]) < sd(bRets[, 2]) * 4, ]
        
        if(sum(bRets$container)==0||sum(bRets$benchmark)==0) {
           return(retval)
        }
        
        bRets <- cbind(
          container=xts::as.xts(bRets$container, order.by=bRets$date),
          benchmark=xts::as.xts(bRets$benchmark, order.by=bRets$date)
          )
          
        bVol <-  as.numeric(PerformanceAnalytics::StdDev(bRets$benchmark)) * sqrt(length(bRets$benchmark))
        
        period <- cbind(xts::apply.weekly(bRets[, "container"], sum), xts::apply.weekly(bRets[, "benchmark"], sum))

        bCorl <- as.numeric(cor(period[, "container"], period[, "benchmark"]))
        bModel <- summary(lm(period[, "container"] ~ period[, "benchmark"]))
        coeffs <- bModel$coefficients
        
        alpha <- NA
        alphaPT <- NA
        beta  <- NA
        betaPT <- NA
        
        if(length(coeffs[, "Estimate"])>1) {

        alpha <- round(coeffs[1, "Estimate"], 4)#paste0(gsub(" ", "", round(coeffs[1, "Estimate"], 4)), " : P(t)=", round(coeffs[1, "Pr(>|t|)"], 2))
        alphaPT <- round(coeffs[1, "Pr(>|t|)"], 2)
        beta  <- round(coeffs[2, "Estimate"], 4)#paste0(gsub(" ", "", round(coeffs[2, "Estimate"], 4)), " : P(t)=", round(coeffs[2, "Pr(>|t|)"], 2))
        betaPT <- round(coeffs[2, "Pr(>|t|)"], 2)
        }
        
        treynor <- NA
        
        if(!is.na(beta)) {
        retRf <- max(as.numeric(tail(as.numeric(data[,"riskfree"]), 1) / head(as.numeric(data[,"riskfree"]), 1) - 1),0,na.rm=TRUE)
        treynor <- (retPf-retRf) / coeffs[2, "Estimate"]
        }
        
        retval[["correlation"]] <- bCorl
        retval[["relativeReturn"]] <- safeNull(retPf-bRet)##-diff(colSums(period))
        retval[["modelAlpha"]] <- alpha
        retval[["modelAlphaSign"]] <- alphaPT
        retval[["modelBeta"]] <- beta
        retval[["modelBetaSign"]] <- betaPT
        retval[["lm"]] <- bModel
        retval[["treynor"]] <- treynor
        retval[["return"]] <- bRet
        retval[["volatility"]] <- bVol

        return(retval)

    }



##' Provides the return statistics for a data frame with price and date column
##'
##' This is the description
##'
##' @param df The data frame.
##' @param pxCol The name of the price column.
##' @param dtCol The name of the date column.
##' @param method The return calcuation method:'discrete', 'log'
##' @param returnOnly Should only the Total Return be calculated?
##' @param benchmark The benchmark symbol. Defaul NULL.
##' @param rfts The the risk free time series. Defaults NA.
##' @param smoothQ outlier trimming via treatpriceseries fn quantile. Defaults 1 (no trimming).
##' @return A data frame with the return statistics.
##' @export
computeReturnStats <- function(df, pxCol, dtCol, method="discrete", returnOnly=FALSE, benchmark=NULL, rfts=NULL, smoothQ=1) {

    retval <- data.frame("Period: Return"=NA,
                         "Period: Volatility"=NA,
                         "Period: Downside Deviation"=NA,
                         "Period: Sharpe (STDEV)"=NA,
                         "Period: Sharpe (VaR)"=NA,
                         "Period: Sharpe (ES)"=NA,
                         "Period: Calmar Ratio"=NA,
                         "Period: Sortino Ratio"=NA,
                         "Period: Sterling Ratio"=NA,                       
                         "Period: Value-At-Risk"=NA,
                         "Period: Expected Shortfall"=NA,
                         "Annual: Return"=NA,
                         "Annual: Volatility"=NA,
                         "Annual: Downside Deviation"=NA,
                         "Annual: Sharpe (STDEV)"=NA,
                         "Annual: Sharpe (VaR)"=NA,
                         "Annual: Sharpe (ES)"=NA,
                         "Annual: Calmar Ratio"=NA,
                         "Annual: Sortino Ratio"=NA,
                         "Annual: Sterling Ratio"=NA,
                         "Annual: Value-At-Risk"=NA,
                         "Annual: Expected Shortfall"=NA,
                         "Daily: Return"=NA,
                         "Daily: Volatility"=NA,
                         "Daily: Downside Deviation"=NA,
                         "Daily: Sharpe (STDEV)"=NA,
                         "Daily: Sharpe (VaR)"=NA,
                         "Daily: Sharpe (ES)"=NA,
                         "Daily: Calmar Ratio"=NA,
                         "Daily: Sortino Ratio"=NA,
                         "Daily: Sterling Ratio"=NA,
                         "Daily: Value-At-Risk"=NA,
                         "Daily: Expected Shortfall"=NA,
                         "Avg. Drawdown"=NA,
                         "Max. Drawdown"=NA,
                         "Avg. Recovery"=NA,
                         "Skewness"=NA,
                         "Kurtosis"=NA,
                         "Quantile Ratio"=NA,
                         "Avg. Losing Month"=NA,
                         "Benchmark Return"=NA,
                         "Benchmark Volatility"=NA,
                         "Benchmark Relative Return"=NA,
                         "Benchmark Correlation"=NA,
                         "Benchmark Model Alpha"=NA,
                         "Benchmark Model Alpha (PT)"=NA,
                         "Benchmark Model Beta"=NA,
                         "Benchmark Model Beta (PT)"=NA,
                         "Benchmark Treynor Ratio"=NA,
                         check.names=FALSE,
                         stringsAsFactors=FALSE,
                         row.names=NULL) 


    ## If empty df, return NA's:
    if (is.null(df) | NROW(df) == 0) {
        return(retval)
    }
    
    ## Remove zero prices:
    ##df <- df[df[, pxCol] != 0 | df[, pxCol] != "0", ]
    ##df <- treatPriceSeries(df, dtCol, pxCol, quantile=smoothQ, surpressPlot=TRUE) 

    ## XTSify:
    ts <- xts::as.xts(as.numeric(df[, pxCol]), order.by=as.Date(df[, dtCol]))
    
    if(is.null(rfts)|all(is.na(rfts))) {
    rfts <- xts::as.xts(as.numeric(rep(1,length(ts))), order.by=as.Date(df[, dtCol]))
    }
    
    retsRf <- diff(log(rfts))
        
    bData <- data.frame(container=ts,date=zoo::index(ts)) %>%
      left_join(data.frame(benchmark=as.numeric(safeNull(benchmark)),date=as.Date(safeNull(zoo::index(benchmark)),origin="1970-01-01")),by="date") %>%
      left_join(data.frame(riskfree=as.numeric(safeNull(rfts)),date=as.Date(safeNull(zoo::index(rfts)),origin="1970-01-01")),by="date") 
      
    bAnalysis <- benchmarkAnalysis(bData)
    
    ## Compute returns:
    if(!is.null(df$ret)) {
    rets <- xts::as.xts(df$ret, order.by=as.Date(df[, dtCol]))
    }
    else {
    rets <- diff(log(ts))
    }

    ## Compute annualized return:
    retsAnnual <- as.numeric(PerformanceAnalytics::Return.annualized(rets, geometric=FALSE))
    retsRfAnnual <- as.numeric(PerformanceAnalytics::Return.annualized(retsRf, geometric=FALSE))

    ## If there are a few observations, return only Total Return:
    if (NROW(df) < 7 | returnOnly) {
        retval[, "Period: Return"] <- tail(as.numeric(ts), 1) / head(as.numeric(ts), 1) - 1
        retval[, "Annual: Return"] <- retsAnnual
        retval[, "Daily: Return"] <- as.numeric(mean(na.omit(rets)))
        return(retval)
    }
    
    ## Compute standard deviation:
    stdev <- as.numeric(PerformanceAnalytics::StdDev(rets))

    ## Compute annualized standard deviation:
    stdevAnnual <- as.numeric(PerformanceAnalytics::sd.annualized(rets))

    ## Downside Deviation:
    downsideDev <- as.numeric(PerformanceAnalytics::DownsideDeviation(rets, MAR = 0))

    ## Compute the average recovery:
    avgRecovery <- as.numeric(PerformanceAnalytics::AverageRecovery(rets))

    ## Compute the average drawdown:
    avgDrawdown <- as.numeric(PerformanceAnalytics::AverageDrawdown(rets))

    ## Compute the maximum drawdown:
    maxDrawdown <- as.numeric(PerformanceAnalytics::maxDrawdown(rets))

    ## Compute the skewness:
    skew <- as.numeric(PerformanceAnalytics::skewness(rets))

    ## Compute the skewness:
    kurt <- as.numeric(PerformanceAnalytics::kurtosis(rets))

    ## Quantile Ratio:
    quantileRatio <- as.numeric(abs(quantile(as.numeric(na.omit(rets)))[2]) / abs(quantile(as.numeric(na.omit(rets)))[4]))

    ## Compute Average Losing Month:
    monthlyRets <- xts::apply.monthly(rets, sum)
    avgLosingMonth <- mean(as.numeric(monthlyRets[monthlyRets < 0]))
    avgLosingMonth <- if_else(is.finite(avgLosingMonth),avgLosingMonth,as.numeric(NA))

    ## Get the period return:
    retPeriod <- as.numeric(tail(as.numeric(ts), 1) / head(as.numeric(ts), 1) - 1)
    retRfPeriod <- as.numeric(tail(as.numeric(rfts), 1) / head(as.numeric(rfts), 1) - 1)

    ## Get the period standard deviation:
    stdevPeriod <- as.numeric(stdev) * sqrt(NROW(rets))

    ## Get the daily return:
    retsDaily <- as.numeric(mean(na.omit(rets)))
    retsRfDaily <- as.numeric(mean(na.omit(retsRf)))

    df <- treatPriceSeries(df, dtCol, pxCol, quantile=smoothQ, surpressPlot=TRUE) 
    ts <- xts::as.xts(as.numeric(df[, pxCol]), order.by=as.Date(df[, dtCol]))
    rets <- diff(log(ts))
    ##if(abs(sum(rets,na.rm=TRUE))>0) {
    rets <- rets[abs(rets)<=abs(median(rets,na.rm=TRUE))+sd(rets,na.rm=TRUE)*4]
    ##}
    ##if(!is.null(df$symbol) && df$symbol=="XD0253306019") {browser()}
    ##if(length(rets[rets==-0.00224928387813375])>0) {browser()}

    ## Compute the VaR:
    var <- as.numeric(PerformanceAnalytics::VaR(rets))
    var <- if_else(is.finite(var),var,as.numeric(NA))

    ## Comput the Expected Shortfall
    es <- as.numeric(PerformanceAnalytics::ES(rets))
    es <- if_else(is.finite(es),es,as.numeric(NA))
    
    ## Construct data frame and return:
    retval <- data.frame("Period: Return"=retPeriod,
                         "Period: Volatility"=stdevPeriod,
                         "Period: Downside Deviation"=downsideDev * sqrt(NROW(rets)),
                         "Period: Sharpe (STDEV)"= (retPeriod-retRfPeriod) / stdevPeriod,
                         "Period: Sharpe (VaR)"=(retPeriod-retRfPeriod) / (abs(var)*sqrt(NROW(rets))),
                         "Period: Sharpe (ES)"=(retPeriod-retRfPeriod) / (abs(es)*sqrt(NROW(rets))),
                         "Period: Calmar Ratio"=retPeriod / maxDrawdown,
                         "Period: Sortino Ratio"=(retPeriod-retRfPeriod) / (downsideDev*sqrt(NROW(rets))),
                         "Period: Sterling Ratio"=retPeriod / avgDrawdown,
                         "Period: Value-At-Risk"=var*sqrt(NROW(rets)),
                         "Period: Expected Shortfall"=es*sqrt(NROW(rets)),
                         "Annual: Return"=retsAnnual,
                         "Annual: Volatility"=stdevAnnual,
                         "Annual: Downside Deviation"=downsideDev * sqrt(252),
                         "Annual: Sharpe (STDEV)"=(retsAnnual-retsRfAnnual) / stdevAnnual,
                         "Annual: Sharpe (VaR)"=(retsAnnual-retsRfAnnual) / (abs(var)*sqrt(252)),
                         "Annual: Sharpe (ES)"=(retsAnnual-retsRfAnnual) / (abs(es)*sqrt(252)),
                         "Annual: Calmar Ratio"=retsAnnual / maxDrawdown,
                         "Annual: Sortino Ratio"=(retsAnnual-retsRfAnnual) / (downsideDev*sqrt(252)),
                         "Annual: Sterling Ratio"=retsAnnual / avgDrawdown,
                         "Annual: Value-At-Risk"=var*sqrt(252),
                         "Annual: Expected Shortfall"=es*sqrt(252),
                         "Daily: Return"=retsDaily,
                         "Daily: Volatility"=stdev,
                         "Daily: Downside Deviation"=downsideDev,
                         "Daily: Sharpe (STDEV)"=(retsDaily-retsRfDaily) / stdev,
                         "Daily: Sharpe (VaR)"=(retsDaily-retsRfDaily) / abs(var),
                         "Daily: Sharpe (ES)"=(retsDaily-retsRfDaily) / abs(es),
                         "Daily: Calmar Ratio"=retsDaily / maxDrawdown,
                         "Daily: Sortino Ratio"=(retsDaily-retsRfDaily) / downsideDev,
                         "Daily: Sterling Ratio"=retsDaily / avgDrawdown,
                         "Daily: Value-At-Risk"=var,
                         "Daily: Expected Shortfall"=es,
                         "Avg. Drawdown"=avgDrawdown,
                         "Max. Drawdown"=maxDrawdown,
                         "Avg. Recovery"=avgRecovery,
                         "Skewness"=skew,
                         "Kurtosis"=kurt,
                         "Quantile Ratio"=quantileRatio,
                         "Avg. Losing Month"=-abs(avgLosingMonth),
                         "Benchmark Return"=bAnalysis[["return"]],
                         "Benchmark Volatility"=bAnalysis[["volatility"]],
                         "Benchmark Relative Return"=bAnalysis[["relativeReturn"]],
                         "Benchmark Correlation"=bAnalysis[["correlation"]],
                         "Benchmark Model Alpha"=bAnalysis[["modelAlpha"]],
                         "Benchmark Model Alpha (PT)"=bAnalysis[["modelAlphaSign"]],
                         "Benchmark Model Beta"=bAnalysis[["modelBeta"]],
                         "Benchmark Model Beta (PT)"=bAnalysis[["modelBetaSign"]],
                         "Benchmark Treynor Ratio"=bAnalysis[["treynor"]],
                         check.names=FALSE,
                         stringsAsFactors=FALSE,
                         row.names=NULL) 

    return(retval)

}


##' Wrappper that runs getPerformance historically for portfolio and benchmark when available 
##'
##' This is the description
##'
##' @param portfolio the portfolio ID.
##' @param date the asofdate.
##' @param session The rdecaf session.
##' @param rfs the symbol indicating what to use as risk free rate.
##' @return A list with return statistics.
##' @export
advancedStatsHistory <- function(portfolio, date, session, rfs=NULL) {


    rawPerformance <- rdecaf::getResource("performance", params=list("portfolios"=portfolio, "start"=dateOfPeriod("Y-3"), "end"=dateOfPeriod("Y-0",date)), session=session)


    yearsWithPerformance <- unique(substr(as.Date(safeNull(unlist(rawPerformance[["indexed"]][["index"]]))), 1,4))
    yearsWithPerformance <- yearsWithPerformance[yearsWithPerformance != substr(date, 1,4)]

    if (length(yearsWithPerformance) == 0|is.na(yearsWithPerformance)) {


        return(NULL)


    }

    ends <- paste0(yearsWithPerformance, "-12-31")
    starts <- paste0(as.numeric(yearsWithPerformance)-1, "-12-31")
    
    ## Get the performance:

    performanceHist <- lapply(1:length(ends), function(i) {

            
    rf <- getRf(rfSymbol=rfs,start= as.Date(starts[i]),end=as.Date(ends[i]),session=session)
    
    benchmark <- getBenchmark(portfolio, as.Date(starts[i]), as.Date(ends[i]), session=session)$benchmarkFlat[["xts"]]

        getPerformance(portfolio, as.Date(starts[i]), as.Date(ends[i]), "daily", session, benchMark=benchmark, rF=rf)
    })


    
    names(performanceHist) <- ends

    return(performanceHist)

}

##' Provides the keystatistics given the input of the getPerformanceV2 output and an as-of-date
##'
##' This is the description
##'
##' @param performance list of output from function referenced above.
##' @param date the asofdate.
##' @return A list with return statistics.
##' @export
keyStats <- function(performance, date) {
    

    statRows <- c("Abs Return", "Return Alpha", "Sharpe Ratio", "Std Deviation", "Max Drawdown")

    if (is.null(performance[["container"]])) {

    return(
      list(
        "keyStats"=data.frame(label=as.character(lubridate::year(date)),
          return=NA,
          stddev=NA,
          sharpe=NA,
          maxddown=NA
          )
        ,
        "daily"=NULL,
        "monthly"=NULL
      )
    )


    }

    ## Get the inception year of the container:
    startYear   <- lubridate::year(head(zoo::index(performance[["container"]][["xts"]]), 1))

    ## Get the current year of the report:
    currentYear <- lubridate::year(date)

    ## If the relative performance object is missing, i.e no benchmark, use container itself as relative:
    if (is.null(performance[["relative" ]][["periodStats"]][["currentWindowStats"]]["sum", ])) {
        relative <- performance[["container"]][["periodStats"]][["currentWindowStats"]]["sum", ]
    } else {
        relative <- performance[["relative"]][["periodStats"]][["currentWindowStats"]]["sum", ]
    }

    ## Initialise the monthly table:
    monthly <- performance[["container"]][["periodStats"]][["currentWindowStats"]]["sum", ]  %>% 
                 pivot_longer(1:NCOL(.),names_to="month",values_to=c("container"))  %>% 
                 inner_join(relative %>% pivot_longer(1:NCOL(relative),names_to="month",values_to=c("relative")),by="month")  %>% 
                 mutate(month=as.Date(month))  %>% 
                 mutate_at(-1,numerize)
    ## add max drawdown
    DDs <- as.data.frame(performance[["container"]][["returns"]])  %>% 
      mutate(month=sapply(as.Date(row.names(.)), function(x) lubridate::ceiling_date(x,"month")-1)  %>% as.Date("1970-01-01"))  %>% 
      group_by(month)  %>% 
      summarise(maxDD=PerformanceAnalytics::maxDrawdown(raw))

    monthly <- monthly  %>% 
      inner_join(DDs,by='month')

    ## Get the container stats:
    containerStats <- performance[["container"]][["stats"]]

    ## Exclude non-numeric date labels:
    containerStats <- containerStats[!is.na(suppressWarnings(as.numeric(as.character(containerStats[, "label"])))), ]

    ## Order the date labels:
    containerStats <- containerStats[order(as.numeric(containerStats[, "label"])), ]

    ## Exclude date labels
    containerStats <- containerStats[as.numeric(containerStats[, "label"]) >= startYear & as.numeric(containerStats[, "label"]) < currentYear, ]

    ## If container stats nrow is 0, initialise data frame:
    if (NROW(containerStats) == 0) {
        containerStats <- initDF(colnames(containerStats))
    }

    ## Get the relative stats:
    retsC <- as.data.frame(performance[["container"]][["returns"]]) %>% mutate(date=as.Date(row.names(.)))  %>% select(date,raw)
    retsB <- NULL

    if(any(names(performance)=="benchmark")) {
    if(!is.null(performance[["benchmark"]])) {
    retsB <- as.data.frame(performance[["benchmark"]][["returns"]]) %>% mutate(date=as.Date(row.names(.)))  %>% select(date,raw)
    }
    }
    retsAll <- retsC  %>% mutate(Returns="Portfolio")  %>% rename(Portfolio=raw) 
    #Get full set of daily returns
    if(!(is.null(retsB))) {
    rets <- retsAll  %>% 
      left_join(retsB  %>% mutate(Returns="Benchmark"), by="date")
    if(NROW(rets)>0) {
    colnames(rets) <- c("date","Portfolio","Returns","Benchmark","ReturnsBenchmark") 
    retsAll <- rets %>% dplyr::filter(Returns=="Portfolio")  %>%  select(c(1:3))  %>% 
      bind_rows(rets %>% dplyr::filter(ReturnsBenchmark=="Benchmark")  %>% select(date,Benchmark,ReturnsBenchmark)  %>% rename(Returns=ReturnsBenchmark, Portfolio=Benchmark))  %>% 
      group_by(Returns)
    }
    }
    ## Done, return:
    return(list(
        "keyStats"=containerStats,
        "daily"=retsAll,
        "monthly"=monthly
    )
        )

}


##' Simplifies the compute returns stats advanced stats into the key metrics for the period in simple df format
##'
##' This is the description
##'
##' @param df the complex computereturnstats data frame input.
##' @param suffix the suffix string to provide the row names, e.g. benchmark.
##' @return A simplified data frame containing return, volatility and sharpe metrics.
##' @export
sumStats <- function(df,suffix="") {
    dfSs <- df   %>% 
      dplyr::filter(str_detect(rownames(.),"Period"))  %>% 
      dplyr::filter(rownames(.) %in% paste("Period:",c("Return","Volatility","Sharpe (STDEV)"))) %>% 
      mutate_all(numerize)
    row.names(dfSs) <- c("Return","Volatility","Sharpe")
    row.names(dfSs) <- paste0(row.names(dfSs),suffix)
    return(dfSs)
}


##' Provides the asset returns MTD and YTD including FX returns
##'
##' This is the description
##'
##' @param series The portfolio id.
##' @param anchors The anchors
##' @return A data frame with the asset returns.
##' @export
exfoliateSeries <- function(series, anchors) {




}


##' Provides the asset returns MTD and YTD including FX returns
##'
##' This is the description
##'
##' @param date The date of consideration.
##' @param ccy The portfolio currency.
##' @param resources The resource data frame.
##' @param priceField The name of the price field in resources.
##' @param periods A vector with periods: c('DTD', 'WTD', 'MTD', 'QTD', 'YTD').
##' @param returnOnly Consider only Total Return?
##' @param excludeWeekends Should the weekends be excluded? Default to TRUE.
##' @param session The rdecaf session.
##' @param treat Should the time series be treated? Default=TRUE.
##' @param exclude Which ctypes should be excluded? Default=c("FXFWD", "DEPO", "LOAN").
##' @return A data frame with the asset returns.
##' @export
getAssetReturns <- function(date,
                            ccy,
                            resources,
                            priceField="ohlcID",
                            periods,
                            returnOnly,
                            excludeWeekends,
                            session,
                            treat=TRUE,
                            exclude=c("FXFWD", "DEPO", "LOAN")) {

    ## Exclude ctypes:
    resources <- resources[!mCondition(resources, "ctype", exclude), ]

    ## Get the slices ohlcs:
    slicedOhlcs <- getSlicedOhlcs(resources[, priceField], session, date, periods, excludeWeekends)

    slicedOhlcs[["MTD"]] <- lapply(slicedOhlcs[["MTD"]], function(x) x[x[, "close"] > 0, ])
    slicedOhlcs[["YTD"]] <- lapply(slicedOhlcs[["YTD"]], function(x) x[x[, "close"] > 0, ])

    if (treat) {
        slicedOhlcs <- lapply(slicedOhlcs, function(x) lapply(x, function(z) treatPriceSeries(z, "date", "close", quantile=0.998, surpressPlot=TRUE)))
    }

    ## Get the returns:
    returnStats <- lapply(slicedOhlcs, function(s) do.call(rbind, lapply(s, function(y)  computeReturnStats(y, "close", "date", method="discrete", returnOnly=returnOnly))))

    ## ## Get the unique FX pairs:
    pairs <- as.character(paste0(resources[, "ccymain"], ccy))

    ## Get the slices ohlcs:
    slicedFX <- getSlicedOhlcs(unique(pairs), session, date, periods)

    ## Get the returns:
    returnFXStats <- lapply(slicedFX, function(s) do.call(rbind, lapply(s, function(y) computeReturnStats(y, "close", "date", method="discrete", returnOnly=returnOnly))))

    ## Append the FX and total returns:
    returnStats <- lapply(1:length(returnStats), function(i) {

        rStats <- returnStats[[i]]
        rFXStats <- returnFXStats[[i]]
        pairs <- paste0(resources[match(rownames(rStats), resources[, "symbol"]), "ccymain"], ccy)

        retCols <- safeGrep(colnames(rStats), "Return") == "1"

        fxRet <- rStats[, retCols]
        fxRet[is.na(fxRet)] <- 0
        colnames(fxRet) <- paste0(gsub("Return", "Rets", colnames(fxRet)), "FX")

        retval <- data.frame(rStats,
                             fxRet,
                             check.names=FALSE)

        totalReturns <- retval[, safeGrep(colnames(retval), "Return") == "1"] + retval[, safeGrep(colnames(retval), "RetsFX") == "1"]
        colnames(totalReturns) <- paste0(colnames(totalReturns), ":Total")

        data.frame(retval,
                   totalReturns,
                   check.names=FALSE)

    })

    ## Name the list:
    names(returnStats) <- periods

    ## Get the last available price dates:
    lastPxDate <- do.call(rbind, lapply(slicedOhlcs, function(x) do.call(rbind, lapply(1:length(x), function(i) {

        ## If Null, return very early date:
        if (is.null(x[[i]])) {
            return(data.frame("Last PX Dt"=as.character("1990-01-01"),
                              "Symbol"=names(x)[i],
                              check.names=FALSE))
        }

        ## Append:
        data.frame("Last PX Dt"=as.character(max(x[[i]][, "date"][x[[i]][, "date"] <= date])),
                   "Symbol"=x[[i]][1, "symbol"],
                   check.names=FALSE)
    }))))

    ## Aggregate the dates by 'max':
    lastPxDate <- aggregate(as.Date(lastPxDate[, "Last PX Dt"]), list(lastPxDate[, "Symbol"]), max)

    ## Append the column names:
    colnames(lastPxDate) <- c("symbol", "date")

    ## Append last available price dates to the main return stats
    returnStats <- lapply(returnStats, function(x) data.frame(x,
                                                              "lastPxDate"=lastPxDate[match(rownames(x), lastPxDate[, "symbol"]), "date"],
                                                              check.names=FALSE,
                                                              stringsAsFactors=FALSE))

    ## Done, return
    return(list("returnStats"=returnStats,
                "posObs"=slicedOhlcs,
                "fxObs"=slicedFX,
                "lastPxDate"=lastPxDate))


}


##' Provides for a time series smoothing, date ascension and limit expansion
##'
##' This is the description
##'
##' @param df A data frame with 'value' and 'date' columns.
##' @param smooth The smoothing parameter.
##' @param limitFactor The factor with which to expand the limits.
##' @return A list with the date-ascended and smoothed values and the expanded lower and upper limits.
##' @export
timeSeriesTransform <- function(df, smooth=0.3, limitFactor=0.1) {

    ## Transform df into time series:
    ts <- timeSeries::as.timeSeries(df[, "value"], df[, "date"])

    ## Smoothen the values:
    lw <- timeSeries::smoothLowess(ts, smooth)[, "lowess"]

    ## Convert back to basic data types:
    value <- as.numeric(lw)
    date <- rownames(lw)

    ## Order values based on date:
    value <- value[order(as.Date(date))]
    date  <- as.character(date[order(as.Date(date))])

    ## Expand the limits:
    limits <- expandLimits(value, factor=limitFactor)

    ## Done, return:
    list("value"=value,
         "date"=date,
         "limits"=limits)
}


##' A function to produce the option deltas.
##'
##' This is the description
##'
##' @param holdings The holdings.
##' @param resources The resources.
##' @return The option deltas.
##' @export
optionDelta <- function(holdings, resources) {
    bscallimpvol(s=2649, k=2800, r=0.08, tt=as.numeric(as.Date("2018-12-21") - Sys.Date()) / 365, d=0, price=5.5)


}

##' A function to detect outliers in xts return series:
##'
##' This is the description
##'
##' @param xtsReturns Return series of xts.
##' @param factor The factor of stdev to apply
##' @return A vector with TRUE/FALSE values
##' @export
returnOutliers <- function(xtsReturns, factor) {

    ## Initialise the outlier vector:
    outliers <- rep(FALSE, NROW(xtsReturns))

    ## Iterate:
    for (i in 1:10) {

        ## Current outliers:
        cOutliers <- abs(xtsReturns[,1]) > sd(xtsReturns[,1]) * factor & abs(xtsReturns[,1]) > 0.0040

        if (all(!cOutliers)) {
            break
        }

        ## Get absolute outlier:
        absOutliers <- cOutliers * abs(xtsReturns)

        ## Get maximum outlier index:
        maxOutlierIdx <- which(absOutliers == max(absOutliers))

        ## Correct outlier index:
        xtsReturns[maxOutlierIdx, "returns"] <- 0

        ## Store the captured outliers:
        outliers[maxOutlierIdx] <- TRUE

    }

    ## Return:
    outliers
}

##' Returns list of regression df and associated ts df for one factor lms only
##'
##' This is a description.
##'
##' @param y the df containing the lhs time series to be regressed
##' @param x the df containing the rhs times series
##' @param rgColname the column name of the time series value, defaults to close
##' @param tsColname the column name of the time series date value, defaults to date
##' @param minobs the min #obs to run a regression, defaults to 30
##' @param roll the rolling avg lag to apply, defaults to 5 days
##' @param stdev the # of st.devs to use when trimming outliers, defaults to 3
##' @return A list with model alpha/beta info and associated filtered time series dfs.
##' @export
regressions <- function(y,x,tsColname="date",rgColname="close",minobs=30,roll=5,stdev=3) {

    y <- y %>%
      rename("close"=rgColname, "date"=tsColname)
      
    x <- x %>% 
       rename("closed"=rgColname, "date"=tsColname)

    dat <- y %>%
      left_join(x,by="date") %>%
      fill(symbol.y,.direction="downup")  %>% 
      arrange(date)  %>% 
      mutate(returns_f=c(NA,diff(log(close))),returns_b=c(NA,diff(log(closed)))) %>%  
      mutate(returns_f=ifelse(abs(returns_f)>sd(returns_f,na.rm=TRUE) * stdev,0,returns_f), returns_b=ifelse(abs(returns_b)>sd(returns_b,na.rm=TRUE) * stdev,0,returns_b)) %>% #trimming
      mutate(sma_f=zoo::rollapply(returns_f,roll,mean,align='right',fill=NA), sma_b=zoo::rollapply(returns_b,roll,mean,align='right',fill=NA))  %>%  #smoothing
      slice(roll:nrow(.)) 
    
    #return(dat)
    if(NROW(dat)>=minobs) {
    dat <- dat  %>% 
      mutate(sma_b=if_else(row_number()==1,0,sma_b))  %>% 
      fill(sma_b,.direction="down")  %>% 
      mutate(sma_f_indx=if_else(row_number()==1,1,sma_f),sma_b_indx=if_else(row_number()==1,1,sma_b))  %>% 
      mutate(sma_f_indx=cumsum(sma_f_indx),sma_b_indx=cumsum(sma_b_indx)) #indexing

    coeffs <- summary(lm(dat$sma_f_indx ~ dat$sma_b_indx))$coefficients
    if(length(coeffs[,"Estimate"])==2) {
    
    alpha <- coeffs[1, "Estimate"]
    alpha_p <- coeffs[1, "Pr(>|t|)"]
    beta <- coeffs[2, "Estimate"]
    beta_p <- coeffs[2, "Pr(>|t|)"]
    
    ## return std err and r2 next time

    df <- data.frame(alpha=alpha,alpha_p=alpha_p,beta=beta,beta_p=beta_p)  %>%  
          mutate_all(~round(.,4))

    return(
      list("regressions"=df,
           "timeseries"=dat 
      )
    )
    }

    }

  }
