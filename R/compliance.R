##' Returns a data-frame with the concentration of a resource field in a portfolio.
##'
##' This is a description.
##'
##' @param portfolio The portfolio id.
##' @param rDate The date of the report.
##' @param resources The resources data frame. If NULL, function will query it. Default NULL.
##' @param session The session info.
##' @param field The resources field. i.e issuer for issuer concentration or id for single concentration.
##' @param orderBy The ordering of the report. Default is valuation(pc).
##' @param divField The consolidation list name to be used as the divisor. Default is nav.
##' @return A data frame
##' @export
getConcentrationByResourceField <- function(portfolio,
                                            rDate,
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
        consolParams <- list(c="account", i=accounts[acc], ccy=portRccy, date=rDate)
        consolidation <- rdecaf::getResource("consolidation", params=consolParams, session=session)
        holdings[[acc]] <- getFlatHoldings(consolidation[["holdings"]], charLimit=30)
    }

    ## Combine and clean holdings:
    holdings <- safeRbind(holdings, convertToString=TRUE)
    holdings <- holdings[!apply(holdings, MARGIN=1, function(x) all(is.na(x))), ]

    ## If only one line or less, return NULL:
    NROW(holdings) > 1 || return(NULL)

    ## Get the portfolio consolidation:
    consolParams <- list(c="portfolio", i=portfolio, ccy=portRccy, date=rDate)
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
        stocks <- getStocks(portfolio, session, date=rDate, c="portfolio")
        ## Append resources to stocks:
        resources <- data.frame(stocks, getResourcesByStock(stocks, session))
    }

    ##  Match holdings account-wise resource id with system's:
    matchIdx <- match(paste0(holdings[, "ID"], holdings[, "Account"]), paste0(resources[, "id"], resources[, "account"]))

    ## Add the concentration field:
    holdings[, fieldSyntax] <- resources[matchIdx, field]

    ## Exist if all field values are NA:
    holdings[is.na(holdings[, fieldSyntax]), fieldSyntax] <- "missing values"

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
    retval <- rbind(aggregates, standalone)
    colNames <- unique(colnames(retval))

    ## Get rid of duplicated columns by checking NA's:
    retval <- do.call(cbind, lapply(colNames, function(x) {
        colVal <- data.frame(retval[, colnames(retval) == x])
        NCOL(colVal) > 1 || return(colVal)
        noOfNAs <- apply(colVal, MARGIN=2, function(x) sum(is.na(x)))
        return(data.frame(colVal[, noOfNAs == min(noOfNAs)]))
    }))

    ## Assign back column names:
    colnames(retval) <- colNames

    ## Done return:
    return(retval)

}


##' Returns the breach report as a list with the aggregate, standalone and report elements.
##'
##' This is a description.
##'
##' @param portfolios The portfolio id's.
##' @param session The session info.
##' @param divisor The consolidation list name to be used as the divisor. Default is nav.
##' @param field The resources field. i.e issuer for issuer concentration or id for single concentration.
##' @param minLim The lower limit.
##' @param maxLim The upper limit.
##' @param rDate The date of the report. Default is Sys.Date.
##' @param rName The name of the report. Default is Breach Report.
##' @param excludeCtypes A vector with strings, i.e CCY or NULL. Default is NULL.
##' @return A list.
##' @export
complianceBreachReport <- function(portfolios, session, divisor="nav", field, minLim, maxLim, rDate=Sys.Date(), rName="Breach Report", excludeCtypes=NULL){

    ## Iterate over unique portfolios in stocks and compute breaches:
    lapply(portfolios, function(p) {

        ## Going for portfolio id:
        print(sprintf("... going for portfolio id: %s", p))

        ## Get the stocks for portfolio:
        stocks <- getStocks(p, session, date=rDate, c="portfolio", zero=0)

        ## Exit if no stocks:
        NROW(stocks) > 1 || return(NULL)

        ## Get resources by stock:
        resources <- getResourcesByStock(stocks, session)

        ## Append resources to stock:
        resources <- data.frame(stocks,
                                resources[match(stocks[, "artifact"], resources[, "id"]), ],
                                stringsAsFactors=FALSE)

        ## Exclude ctypes if needed:
        if (!is.null(excludeCtypes)) {
            resources <- resources[!resources[, "ctype"] %in% excludeCtypes, ]
        }

        ## Exit if no stocks:
        NROW(resources) > 1 || return(NULL)

        ## Get the accounts of the portfolio:
        accounts <- getDBObject("accounts", session=session, addParams=list("portfolio"=p))

        ## Get the custodian names for accounts:
        custodian <- accounts[match(resources[, "account"], accounts[, "id"]), "custodian_name"]

        ## Append the issuer for cash stocks:
        resources[resources[, "ctype"] == "CCY", "issuer"] <- custodian[resources[, "ctype"] == "CCY"]

        ## Get the concentration for portfolio's stocks:
        concentration <- getConcentrationByResourceField("portfolio"=p,
                                                         "date"=rDate,
                                                         "resources"=resources,
                                                         "session"=session,
                                                         "field"=field,
                                                         "orderBy"="valuation(%)",
                                                         "divField"=divisor)

        ## Get the aggregate concentration:
        aggregates <- concentration[concentration[, "type"] == "aggregate", ]

        ## Get the standalone concentration:
        standalone <- concentration[concentration[, "type"] == "standalone", ]

        ## Compute max breaches:
        inMaxLim <- (numerize(aggregates[, "valuation(%)"]) / 100) > as.numeric(maxLim)

        ## Compute min breaches:
        inMinLim <- (numerize(aggregates[, "valuation(%)"]) / 100) < as.numeric(minLim)

        ## Filter in breaches and clean the data frame from NA's:
        breaches <- cleanNARowsCols(aggregates[inMaxLim | inMinLim, ], onlyRows=TRUE)

        ## Initialse report value:
        report <- NULL

        ## If there are breaches, populate the report value:
        if (NROW(breaches) > 0) {

            ## Generate report data frame:
            report <- lapply(breaches[, field], function(z) {

                list("container"=list("name"=accounts[1, "portfolio_name"],
                                      "id"=p,
                                      "type"="portfolio"),
                     "compliance"=rName,
                     "group"=z,
                     "value"=breaches[safeCondition(breaches, field, z), "valuation(%)"],
                     "breachStandalone"=data.frame("compliance"=field,
                                                   standalone[safeCondition(standalone, field, z), ],
                                                   check.names=FALSE),
                     "breachAggregates"=data.frame("compliance"=field,
                                                   aggregates[safeCondition(aggregates, field, z), ],
                                                   check.names=FALSE))})
        }

        ## Initialize empty list:
        retval <- list()

        ## Append report data frame to list:
        retval[["breaches"]] <- report

        ## Append aggregates concentration to list:
        retval[["aggregates"]] <- data.frame("compliance"=rName, safeNull(aggregates), check.names=FALSE)

        ## Append standalone cocentration to list:
        retval[["standalone"]] <- data.frame("compliance"=rName, safeNull(standalone), check.names=FALSE)

        ## Return:
        return(retval)

    })

}


##' The wrapper function for the breach report which emails the report.
##'
##' This is a description.
##'
##' @param portfolios The portfolio id's. Default is NULL which runs all portfolio id's.
##' @param tz The time zone.
##' @param gte The lower time limit for the report to run.
##' @param lte The upper time limit for the report to run.
##' @param weekdays The weekdays when the report should be run.
##' @param session The session info.
##' @param emailParams The email parameters.
##' @param rDate The date of the report. Default is Sys.Date.
##' @param field The resources field. i.e issuer for issuer concentration or id for single concentration.
##' @param divisor The consolidation list name to be used as the divisor. Default is nav.
##' @param minLim The lower limit.
##' @param maxLim The upper limit.
##' @param excludeCtypes A vector with strings, i.e CCY or NULL. Default is NULL.
##' @return NULL.
##' @export
breachReportWrapper <- function(portfolios=NULL,
                                tz="UTC",
                                gte="00:00:00",
                                lte="23:59:59",
                                weekdays=c("MON", "TUE", "WED", "THU", "FRI"),
                                session,
                                emailParams,
                                rDate=Sys.Date(),
                                field="issuer",
                                divisor="nav",
                                minLim=0.0,
                                maxLim=0.3,
                                excludeCtypes=NULL) {

    ## Is it time?:
    itsBreachTime <- itsTime(tz=tz, gte=gte, lte=lte, inWeekdays=weekdays)

    ## Return null, if it's not time:
    itsBreachTime || return(NULL)

    rName <- sprintf("%s %s", capitalise(field), percentify(maxLim))

    ##:
    print(sprintf("Going for compliance breach report: %s", rName))

    ## Get the portfolios if missing:
    if (is.null(portfolios)) {
        portfolios <- getDBObject("portfolios", session=session)[, "id"]
    }

    ## Run the breach report:
    reports <- complianceBreachReport("portfolios"=portfolios,
                                      "session"=session,
                                      "divisor"=divisor,
                                      "field"=field,
                                      "minLim"=minLim,
                                      "maxLim"=maxLim,
                                      "rDate"=rDate,
                                      "rName"=rName,
                                      "excludeCtypes"=excludeCtypes)

    ## Combine the breach reports:
    breaches <- do.call(c, lapply(reports, function(x) x[["breaches"]]))

    ## Get the constituents fo breaches:
    constituents <- do.call(rbind, lapply(breaches, function(x) rbind(x[["breachStandalone"]],
                                                                      x[["breachAggregates"]])))

    ## Order by report type (aggregage, standalone:
    constituents <- extractToList(constituents, "type")
    constituents <- do.call(rbind, constituents[match(c("aggregate", "standalone"), sapply(constituents, function(x) x[1, "type"]))])

    ## Define the report columns:
    reportColumns <- c("compliance", "type", "portfolio", "symbol", "instrument", field, divisor, "valuation(%)")

    ## Filter in report columns:
    constituentsEmail <- constituents[, reportColumns]

    ## Combine the full reports
    full <- do.call(rbind, lapply(1:length(reports), function(i) {
        rbind(reports[[i]]$aggregates, reports[[i]]$standalone)
    }))

    ## If no breaches, create pseude table:
    if (NROW(constituentsEmail) == 0) {
        constituentsEmail <- initDF("NO BREACHES", 1)
    }

    ## Define the email html table:
    .emailHTMLTable <- function(df, provider, caption, sourceType="API", align=rep("left", NCOL(df))) {
        tableHTML::tableHTML(df,
                             border=0,
                             spacing="4px",
                             footer=paste0("Source Reference: ", toupper(provider), " " , sourceType),
                             caption=caption,
                             rownames=FALSE) %>%
            tableHTML::add_css_caption(css = list(c('color', 'font-size', 'text-align', 'margin-bottom'), c("#666666", '18px', 'center', '10px'))) %>%
            tableHTML::add_css_footer(css = list(c('color', 'font-size', 'text-align', 'margin-top'), c("#999999", '12px', 'center', '10px'))) %>%
            tableHTML::add_css_column(css = list(rep("text-align", NCOL(df)), align), columns=colnames(df))
    }


    ## Infer the deployment:
    deployment <- strsplit(strsplit(session$location, "//")[[1]][2], "[.]")[[1]][1]

    ## Construct the content of the alert email:
    .UPDATETEXT <- list("GREETINGPLACEHOLDER"="Dear Team",
                        "EMAILBODYPLACEHOLDER"="This is an autogenerated compliance report for issuer concentration",
                        "CALLTOACTIONPLACEHOLDER"="Go to System",
                        "DEPLOYMENT"=toupper(deployment),
                        "URLPLACEHOLDER"=gsub("/api", "", session$location),
                        "FINALPARAGRAPHPLACEHOLDER"="Please see below the constituents of issuers which cause a breach",
                        "ADDRESSPLACEHOLDER"="",
                        "GOODBYEPLACEHOLDER"="Best Regards,<br>DECAF TEAM",
                        "ADDENDUMPLACEHOLDER"=.emailHTMLTable(constituentsEmail,
                                                              provider=toupper(deployment),
                                                              caption="Breaches",
                                                              sourceType="API",
                                                              align=c(rep("left", NCOL(constituentsEmail)-1), "right")))

    ## Construct the file path for the breach report:
    breachesPath <- gsub(":", "" ,gsub(" ", "_", sprintf("/tmp/%s_breaches_%s_%s.csv", deployment, rName, Sys.time())))

    ## Construct the file path for the full concentration report:
    fullPath <- gsub(":", "" ,gsub(" ", "_", sprintf("/tmp/%s_full_%s_%s.csv", deployment, rName, Sys.time())))

    ## Write the files:
    write.csv(constituents, breachesPath, row.names=FALSE)
    write.csv(full, fullPath, row.names=FALSE)

    ## Prepare the email subject flag:
    subjectFlag <- ifelse(NROW(constituents) == 0, "NO BREACHES ", "BREACHES DETECTED ")

    ## Construct the email subject:
    subject <- sprintf(" Compliance Report: %s as of %s : %s", rName, Sys.Date(), subjectFlag)

    ## Send email:
    syncUpdateEmail(template=readLines("../assets/update_email.html"),
                    updateText=.UPDATETEXT,
                    emailParams=emailParams,
                    subject=subject,
                    attachments=c(breachesPath, fullPath))

    ## Done, return:
    return(NULL)

}
