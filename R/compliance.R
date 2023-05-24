##' Returns a data-frame with the concentration of a resource field in a portfolio.
##'
##' This is a description.
##'
##' @param portfolio The portfolio id.
##' @param date The date of the report.
##' @param resources The resources data frame. If NULL, function will query it. Default NULL.
##' @param session The session info.
##' @param field The resources field. i.e issuer for issuer concentration or id for single concentration.
##' @param orderBy The ordering of the report. Default is valuation(pc).
##' @param divField The consolidation list name to be used as the divisor. Default is nav.
##' @return A data frame
##' @export
getConcentrationByResourceField <- function(portfolio,
                                            date,
                                            resources=NULL,
                                            session,
                                            field,
                                            orderBy="valuation(%)",
                                            divField="nav") {

    ## Construct the field syntax:
    fieldSyntax <- paste0("concentration::", field)

    ## Get the portfolio and basic info:
    portObjc <- getDBObject("portfolios", session, addParams=list("id"=portfolio))
    portName <- portObjc[, "name"]
    portRccy <- portObjc[, "rccy"]

    ## Get the account id's:
    accounts <- getDBObject("accounts", session, addParams=list("portfolio"=portfolio))[, "id"]

    ## Initialise the holdings list:
    holdings <- list()

    ## Iterate over accounts and get holdings:
    for (acc in 1:length(accounts)) {
        consolParams <- list(c="account", i=accounts[acc], ccy=portRccy, date=date)
        consolidation <- rdecaf::getResource("consolidation", params=consolParams, session=session)
        holdings[[acc]] <- getFlatHoldings(consolidation[["holdings"]], charLimit=30)
    }

    ## Combine and clean holdings:
    holdings <- safeRbind(holdings, convertToString=TRUE)
    holdings <- holdings[!apply(holdings, MARGIN=1, function(x) all(is.na(x))), ]

    ## If only one line or less, return NULL:
    NROW(holdings) > 1 || return(NULL)

    ## Get the portfolio consolidation:
    consolParams <- list(c="portfolio", i=portfolio, ccy=portRccy, date=date)
    consolidation <- rdecaf::getResource("consolidation", params=consolParams, session=session)

    ## Define the divisor:
    divisor <- consolidation[[divField]]

    ## Filter in holdings columns:
    holdings <- holdings[, c("ID", "Account", "Name", "Symbol", "CCY", "Type", "Value", "Exposure")]

    ## Parse the exposure column:
    holdings[, "Exposure"] <- as.numeric(as.character(holdings[, "Exposure"]))

    ## Parse the value column:
    holdings[, "Value"] <- as.numeric(as.character(holdings[, "Value"]))

    ## Get resources if NULL
    if (is.null(resources)) {
        ## Get the stocks for portfolio:
        stocks <- getStocks(portfolio, session, date=date, c="portfolio")
        ## Append resources to stocks:
        resources <- data.frame(stocks, getResourcesByStock(stocks, session))
    }

    ##  Match holdings account-wise resource id with system's:
    matchIdx <- match(paste0(holdings[, "ID"], holdings[, "Account"]), paste0(resources[, "id"], resources[, "account"]))

    ## Add the concentration field:
    holdings[, fieldSyntax] <- resources[matchIdx, field]

    ## Compute exposure aggregates by field:
    aggByExp <- aggregate(as.numeric(as.character(holdings[, "Exposure"])), list(holdings[, fieldSyntax]), sum)

    ## Append quotient:
    aggByExp[, "pcExp"] <- aggByExp[, 2] / divisor

    ## Add column names:
    colnames(aggByExp) <- c(field, "exposure", "exposure(%)")

    ## Compute value aggregates by field:
    aggByVal <- aggregate(as.numeric(as.character(holdings[, "Value"])), list(holdings[, fieldSyntax]), sum)

    ## Compute quotient:
    aggByVal[, "pcVal"] <- aggByVal[, 2] / divisor

    ## Add column names:
    colnames(aggByVal) <- c(field, "valuation", "valuation(%)")

    ## Create data frame of exposure aggregates:
    aggByExp <- data.frame(aggByExp,
                           aggByVal[match(aggByExp[, field], aggByVal[, field]), c("valuation", "valuation(%)")],
                           check.names=FALSE)

    ## Create the final aggregates data frame:
    aggregates <- data.frame("type"="aggregate",
                             "portfolio"=portName,
                             "refCCY"=portRccy,
                             "symbol"=NA,
                             "instrument"=NA,
                             "NAME_CONCENTRATION"=aggByExp[,1],
                             "NAME_DIVISOR"=beautify(as.numeric(divisor), 2),
                             "exposure"=trimws(trimDws(beautify(aggByExp[, "exposure"], 2))),
                             "exposure(%)"=trimws(trimDws(percentify(aggByExp[, "exposure(%)"]))),
                             "valuation"=trimws(trimDws(beautify(aggByExp[, "valuation"], 2))),
                             "valuation(%)"=trimws(trimDws(percentify(aggByExp[, "valuation(%)"]))),
                             check.names=FALSE,
                             stringsAsFactors=FALSE)

    ## Rename concentration and divisor column names:
    colnames(aggregates)[which(colnames(aggregates) == "NAME_CONCENTRATION")] <- field
    colnames(aggregates)[which(colnames(aggregates) == "NAME_DIVISOR")] <- divField
    aggregates <- aggregates[order(numerize(aggregates[, orderBy]), decreasing=TRUE), ]

    ## Create the final stand-alone data frame:
    standalone <- data.frame("type"="standalone",
                             "portfolio"=portName,
                             "refCCY"=portRccy,
                             "symbol"=holdings[, "Symbol"],
                             "instrument"=holdings[, "Type"],
                             "NAME_CONCENTRATION"=holdings[, fieldSyntax],
                             "NAME_DIVISOR"=beautify(as.numeric(divisor), 2),
                             "exposure"=trimws(trimDws(beautify(holdings[, "Exposure"], 2))),
                             "exposure(%)"=trimws(trimDws(percentify(holdings[, "Exposure"] / divisor))),
                             "valuation"=trimws(trimDws(beautify(holdings[, "Value"], 2))),
                             "valuation(%)"=trimws(trimDws(percentify(holdings[, "Value"] / divisor))),
                             check.names=FALSE,
                             stringsAsFactors=FALSE)

    ## Rename the concentration and divisor column names:
    colnames(standalone)[which(colnames(standalone) == "NAME_CONCENTRATION")] <- field
    colnames(standalone)[which(colnames(standalone) == "NAME_DIVISOR")] <- divField
    standalone <- standalone[order(numerize(standalone[, orderBy]), decreasing=TRUE), ]

    ## Done, return:
    rbind(aggregates, standalone)

}
