##' Arranges holdings based on given columns.
##'
##' This is a description.
##'
##' @param holdings The holdings data frame.
##' @param col The columns to be arranged by.
##' @return A list.
##' @export
arrangeHoldings <- function(holdings, col) {

    ## Format the holdings data frame:
    holdings <- data.frame(holdings, row.names=NULL, check.names=FALSE)

    ## Get the top level index:
    tlevelIdx <- which(holdings[, "order"] == "1")

    ## Get the start and endpoint of top levels:
    tlevelIndices <- cbind(tlevelIdx[seq(1, length(tlevelIdx), 2)], tlevelIdx[seq(2, length(tlevelIdx), 2)])

    ## Get the holdings by top level:
    hTopLevel <- apply(tlevelIndices, MARGIN=1, function(x) holdings[x[1]:x[2], ])

    ## Rearrange the holdings:
    holdings <- do.call(rbind, hTopLevel[order(sapply(hTopLevel, function(h) median(na.omit(as.numeric(h[, "AClass Order"])))))])

    ## Get the top level index:
    tlevelIdx <- which(holdings[, "order"] == "1")

    ## Get the start and endpoint of top levels:
    tlevelIndices <- cbind(tlevelIdx[seq(1, length(tlevelIdx), 2)], tlevelIdx[seq(2, length(tlevelIdx), 2)])

    ##
    grpSum <- getPositionsByGrp(holdings, col)

    ## Done, return list:
    list("holdings" = holdings,
         "subtotals"= holdings[tlevelIndices[,2], ],
         "asstLbts" = grpSum[["asstLbts"]],
         "asstLbtsSum"=grpSum[["asstLbtsSummary"]],
         "positions"=grpSum[["positions"]],
         "total"=sum(na.omit(holdings[is.na(holdings[, "order"]), "Value"])))
}


##' Provides the positions by groupings.
##'
##' This is a description.
##'
##' @param holdings The holdings data frame.
##' @param col The columns to be arranged by.
##' @return A list.
##' @export
getPositionsByGrp <- function(holdings, col) {

    ## Get the asset class indices:
    grpIdx <- lapply(unique(na.omit(holdings[, col])), function(x) holdings[, col] == x)

    ## Positions by asset class:
    grpPos <- lapply(grpIdx, function(x) holdings[ifelse(is.na(x), FALSE, x), ])
    names(grpPos) <- unique(na.omit(holdings[, col]))

    ## Get asset and liabilities:
    grpAsstLbts <- lapply(grpPos, function(x) list("asset"=cleanNARowsCols(x[x[, "Value"] > 0,]), "liability"=cleanNARowsCols(x[x[, "Value"] < 0, ])))

    ## Assign the names:
    names(grpAsstLbts) <- unique(na.omit(holdings[, col]))

    ## Get the asset liability summary:
    grpLbtsSummary <- do.call(rbind, lapply(grpAsstLbts, function(x) c("totalAsset"=sum(safeColumn(x[["asset"]], "Value")),
                                                                       "totalLiability"=sum(safeColumn(x[["liability"]], "Value")))))
    return(list("positions"=grpPos,
                "asstLbts"=grpAsstLbts,
                "asstLbtsSummary"=grpLbtsSummary))
}


##' Prepares a data frame with scaled hex color codes for values centerd around zero.
##'
##' This is a description.
##'
##' @param values A vector with values centered around zero.
##' @param positiveRange A vector with 2 strings with the range of colors for positive values.
##' @param negativeRange A vector with 2 strings with the range of colors for negative values.
##' @param stepsno The number of steps for the color range.
##' @return A data frame with the values and the scaled hex color codes.
##' @export
centeredColorScale <- function(values, positiveRange=c("lightgreen", "darkgreen"), negativeRange=c("orangered3", "coral"), stepsno=15) {

    ## Get the color range for positive values:
    posColors <- colorRampPalette(positiveRange)(stepsno)

    ## Get the color range for negative values:
    negColors <- colorRampPalette(negativeRange)(stepsno)

    ## Get the positive value indices:
    posIdx <- which(values >  0)

    ## Get the negative value indices:
    negIdx <- which(values <= 0)

    ## Store the original positive, negative indices:
    origNeg <- negIdx
    origPos <- posIdx

    ## If no positive index, mask values:
    if (length(posIdx) == 0) {
        posIdx <- c(1,2)
    }

    ## If no negatve index, mask values:
    if (length(negIdx) == 0) {
        negIdx <- c(1,2)
    }

    ## Get the positive values:
    posVal <- data.frame("value"=values[posIdx], "idx"=posIdx)

    ## Get the negative values:
    negVal <- data.frame("value"=values[negIdx], "idx"=negIdx)

    ## Construct a vector with the equally spaced range of length(posColors) for positive values:
    posScale <- seq(min(posVal[, "value"]), max(posVal[, "value"]), length.out=length(posColors))

    ## Compute the difference of the actual values wrt the scale:
    posDists <- lapply(posVal[, "value"], function(x) x - posScale)

    ## Map the actual value to the scaled value:
    posVal[, "class"] <- sapply(posDists, function(x) which(abs(x) == min(abs(x)))[1])

    ## Assign the corresponding color:
    posVal[, "col"] <- posColors[posVal[,"class"]]

    ## Construct a vector with the equally spaced range of length(posColors) for negative values:
    negScale <- seq(min(negVal[, "value"]), max(negVal[, "value"]), length.out=length(negColors))

    ## Compute the difference of the actual values wrt the scale:
    negDists <- lapply(negVal[, "value"], function(x) x - negScale)

    ## Map the actual value to the scaled value:
    negVal[, "class"] <- sapply(negDists, function(x) which(abs(x) == min(abs(x)))[1])

    ## Assign the corresponding color:
    negVal[, "col"] <- negColors[negVal[,"class"]]

    ## Combine the positive and negative data frames:
    if (length(origPos) == 0) {
        df <- negVal
    } else if (length(origNeg) == 0) {
        df <- posVal
    } else {
        df <- rbind(posVal, negVal)
    }

    ## Done, return
    df[order(df[, "idx"]), ]

}



##' Renders an html template.
##'
##' This is a description.
##'
##' @param tmplPath The path to the template.
##' @param ... Additional parameters to be passed to tmpl.
##' @return An html.
##' @export
renderTemplate <- function (tmplPath, ...) {
    templates::tmpl(paste(readLines(tmplPath), collapse="\n"), ...)
}


##' Creates an image tag.
##'
##' This is a description.
##'
##' @param path The path to the image.
##' @return An image tag.
##' @export
makeImgTag <- function (path) {
    sprintf("data:image/png;base64,%s", base64enc::base64encode(path))
}


##' Creates an image tag for SVG.
##'
##' This is a description.
##'
##' @param path The path to the SVG image.
##' @return An image tag.
##' @export
makeImgTagSVG <- function (path) {
    sprintf("data:image/svg+xml;base64,%s", base64enc::base64encode(path))
}


##' A function to style multiple rows in data frame
##'
##' This is a description.
##'
##' @param x The data frame.
##' @param rows The row indices.
##' @param ... Arguments for kablExtra.
##' @return A data frame with style rows.
##' @export
multirow_spec <- function(x, rows, ...) {
    for (row in rows)
        x <- kableExtra::row_spec(x, row, ...)
    x
}



##' Prepares the NAV data frame.
##'
##' This is a description.
##'
##' @param x A list with "portfolio", "ccy", "currentNAV", "previous NAV".
##' @param inception The launch date.
##' @param pxinfo The YTD Performance.
##' @return A data frame.
##' @export
beanbagNAVTable <- function (x, inception, pxinfo) {

    ## Prepare the name column:
    names <- c("Name",
               "Currency",
               "Inception",
               paste0("NAV (", x[["date"]], ")"),
               paste0("NAV (", x[["previousDate"]], ")"),
               "Change",
               "ISIN",
               "NAV/Share",
               trimws(paste0("Perf (YTD) ", ifelse(!is.na(pxinfo[, "isin"]), as.character(pxinfo[, "shareclass"]), ""))))

    ## Prepare the value column:
    value <- c(x[["portfolio"]],
               x[["ccy"]],
               as.character(inception),
               paste0(beautify(x[["currentNAV"]]), " ", x[["ccy"]]),
               paste0(beautify(x[["previousNAV"]]), " ", x[["ccy"]]),
               paste0(beautify(x[["currentNAV"]] - x[["previousNAV"]]), " ", x[["ccy"]]),
               paste(pxinfo[, "isin"], collapse=", "),
               x[["navshare"]],
               sapply(pxinfo[,"ytdext"], function(x) ifelse(is.na(x), NA, percentify(x))))

    ## Make the data frame:
    df1 <- data.frame("Name"=names, "Value"=value, check.rows=FALSE, check.names=FALSE, stringsAsFactors=FALSE)

    ## For non funds:
    if (!any(pxinfo[, "isFund"])) {
        df1 <- df1[!df1[, "Name"] == "ISIN", ]
        df1 <- df1[!df1[, "Name"] == "NAV/Share", ]
    }

    ## Set the column to NULL:
    colnames(df1) <- NULL

    ## Set the rows to NULL:
    rownames(df1) <- NULL

    ## Done, return:
    return(df1)

}


##' Gets and prepares the performance information for a portfolio.
##'
##' This is a description.
##'
##' @param portfolio The portfolio id.
##' @param start The desired start date.
##' @param end  The desired end date.
##' @param freq The desired frequency.
##' @param session The rdecaf session.
##' @return A list with the indexed cumulative return series and performance stats.
##' @export
getPerformance <- function(portfolio, start, end, freq="daily", session) {

    ## Construct the params:
    params <- list("portfolios"=portfolio,
                   "start"=start,
                   "end"=end,
                   "frequency"=freq)

    ## Get the asset evolves:
    performance <- rdecaf::getResource("performance", params=params, session=session)

    ## Get the performance index series:
    series <- xts::as.xts(unlist(performance[["indexed"]][["data"]]),
                          order.by=as.Date(unlist(performance[["indexed"]][["index"]])))

    ## Get the stats:
    stats <- t(safeRbind(lapply(performance[["statistics"]][["univariate"]][["portfolios"]][[1]][["stats"]], function(x) do.call(cbind, x[["stats"]]))))

    ## Add the colnames:
    colnames(stats) <- sapply(performance[["statistics"]][["univariate"]][["portfolios"]][[1]][["stats"]], function(x) x[["label"]])

    ## Return:
    list("series"=series,
         "stats"=stats[,-1])
}


##' Prepares the holdings detail data frame.
##'
##' This is a description.
##'
##' @param holdings The holdings data frame.
##' @param colSelect Which columns to select.
##' @return A list with holdings details information.
##' @export
getHoldingsDetails <- function(holdings, colSelect) {

    ## Replace the NA order with the highest available order + 1:
    holdings[is.na(holdings[, "order"]), "order"] <- max(na.omit(holdings[, "order"])) + 1

    ## Grep title names:
    grepTitles <- paste0(holdings[holdings[, "order"] == 1 & as.logical(holdings[, "isHeader"]), "Name"], ", ")

    ## Remove title names from subtitles:
    holdings[, "Name"] <- mgsub(holdings[, "Name"], grepTitles, "")

    ## Get the row indices which are totals:
    totalRowIdx <- which(holdings[, "order"] == "1")

    ## Get the row indices which are subtotals:
    subtlRowIdx <- which(holdings[, "order"] == "2")

    ## Get the row indices which are holdings:
    holdsRowIdx <- which(holdings[, "order"] == max(na.omit(holdings[, "order"])))

    ## Remove the order column:
    holdings <- holdings[, !(colnames(holdings) == "order")]

    ## Remove the header column:
    holdings <- holdings[, !(colnames(holdings) == "isHeader")]

    ## Remove the footer column:
    holdings <- holdings[, !(colnames(holdings) == "isFooter")]

    ## Remove the rownames:
    rownames(holdings) <- NULL

    ## Select the columns:
    holdings <- holdings[, colSelect]

    ## Replace long country strings:
    if (any(colnames(holdings) == "Country")) {
        holdings[, "Country"] <- gsub("United States Of America", "United States", holdings[, "Country"])
    }

    priceKeys <- c("QTY", "PX Cost", "PX Last")
    priceIdx <- lapply(priceKeys, function(x) colnames(holdings) == x)
    priceIdx <- apply(do.call(cbind, priceIdx), MARGIN=1, any)
    ## Beautify percentages:
    suppressWarnings(holdings[, priceIdx] <- apply(holdings[, priceIdx], MARGIN=2, function(x) round(x, 4)))

    monetaryKeys <- c("QTY", "Value", "Exposure", "Accrd")
    monetaryIdx <- apply(mgrep(colnames(holdings), monetaryKeys), MARGIN=1, function(x) any(x != "0"))

    ## Beautify monetary amounts:
    holdings[, monetaryIdx] <- apply(holdings[, monetaryIdx], MARGIN=2, function(x) trimws(beautify(x)))

    percentageKeys <- c("Value (%)", "Exp (%)")
    percentageIdx <- lapply(percentageKeys, function(x) colnames(holdings) == x)
    percentageIdx <- apply(do.call(cbind, percentageIdx), MARGIN=1, any)
    ## Beautify percentages:
    suppressWarnings(holdings[, percentageIdx] <- apply(holdings[, percentageIdx], MARGIN=2, function(x) trimws(percentify(x))))

    ## Replace unwanted strings with empty:
    holdings[is.na(holdings)] <- ""
    holdings[holdings == "NA"] <- ""
    holdings[holdings == "NA %"] <- ""

    ## Done, return:
    list("holdingsDetails"=holdings,
         "subtlIdx"=subtlRowIdx,
         "totalIdx"=totalRowIdx,
         "holdsIdx"=holdsRowIdx)
}


##' Prepares the holdings summary data frame.
##'
##' This is a description.
##'
##' @param holdings The holdings data frame
##' @return A list with holdings summary information.
##' @export
getHoldingsSummary <- function(holdings) {

    ## Get the cash holdings:
    cashHoldings <-  holdings[safeCondition(holdings, "Subtype", "Cash"), ]

    if (NROW(cashHoldings) != 0) {
        ## For empty cash names, replace by currency:
        cashHoldings[isNAorEmpty(cashHoldings[, "Name"]), "Name"] <- cashHoldings[isNAorEmpty(cashHoldings[, "Name"]), "CCY"]
    }

    ## Exclude the holdings:
    holdings <- holdings[!is.na(holdings[, "order"]), ]

    ## Get the footers, i.e subtotals:
    holdings <- holdings[!(as.logical(holdings[, "isHeader"]) & holdings[, "order"] > 1),]

    ## Replace the 'Subtotal' string with 'Total' from the name of footers:
    holdings[holdings[, "order"] == 1, "Name"] <- gsub("Subtotal", "Total", holdings[holdings[, "order"] == 1, "Name"])

    ## Append the cash holdings to subtotals:
    holdings <- rbind(holdings[1, ], cashHoldings, holdings[2:NROW(holdings), ])

    ## For NA order, assign 2:
    holdings[is.na(holdings[, "order"]), "order"] <- 2

    ## Get the index of first level footers:
    myFooter <- holdings[, "order"] == 1 & as.logical(holdings[, "isFooter"])

    ## Get the index of first level headers:
    holdings[holdings[, "order"] == 1 & as.logical(holdings[, "isHeader"]), ] <- holdings[myFooter,]

    ## Get the subtotals which are not first level footers:
    holdings <- holdings[!myFooter, ]

    ## Get the index of rows of order 1, i.e the totals:
    totalRowIdx <- which(holdings[, "order"] == "1")

    ## Get the index of rows of order !=1, i.e the subtotals:
    subtlRowIdx <- which(holdings[, "order"] != "1")

    ## Get rid of unwanted columns:
    holdings <- holdings[, !(colnames(holdings) == "order")]

    ## Get rid of unwanted columns:
    holdings <- holdings[, !(colnames(holdings) == "isHeader")]

    ## Get rid of unwanted columns:
    holdings <- holdings[, !(colnames(holdings) == "isFooter")]

    ## Remove rownames:
    rownames(holdings) <- NULL

    ## Beautify monetary amounts:
    holdings[, c("Value", "Exposure", "PnL (Unrl)")] <- apply(holdings[, c("Value", "Exposure", "PnL (Unrl)")], MARGIN=2, function(x) trimws(beautify(x)))

    ## Beautify percentages:
    holdings[, c("Value (%)", "Exp (%)", "PnL (%Inv)")] <- apply(holdings[, c("Value (%)", "Exp (%)", "PnL (%Inv)")], MARGIN=2, function(x) trimws(percentify(x)))

    ## Replace unwanted strings with empty:
    holdings[is.na(holdings)] <- ""
    holdings[holdings == "NA"] <- ""
    holdings[holdings == "NA %"] <- ""

    ## Select the columns
    holdings <- holdings[, c("Name", "Value", "Value (%)", "Exposure", "Exp (%)", "PnL (Unrl)")]

    ## Done, return list:
    list("holdingsSummary"=holdings,
         "subtlIdx"=subtlRowIdx,
         "totalIdx"=totalRowIdx)
}


##' Gets the asset evolution for a portfolio from pconsolidation
##'
##' This is a description.
##'
##' @param portfolio The portfolio id.
##' @param date The date__lte.
##' @param years The number of years to look back.
##' @param session The rdecaf session,
##' @return An data frame with NAV and AUM.
##' @export
getAssetEvolution <- function(portfolio, date, years="2", session) {
    ## Define the params:
    params <- list("portfolio"=portfolio,
                   "account__isnull"="True",
                   "shareclass__isnull"="True",
                   "date__gte"=dateOfPeriod(paste0("Y-", years)),
                   "date__lte"=date,
                   "page_size"=-1)

    ## Get the pconsolidaton:
    pCons <- rdecaf::getResource("pconsolidations", params=params, session=session)

    ## Build the asset evolution data frame:
    as.data.frame(do.call(rbind, lapply(pCons, function(x) data.frame("nav"=x[["nav"]], "aum"=x[["aum"]], "date"=as.Date(x[["date"]]), stringsAsFactors=FALSE))))

}


##' Highlights each word in a string with a background color
##'
##' This is a description.
##'
##' @param string A vector with strings.
##' @param textColor The text color.
##' @param bgColor The background color.
##' @param fontSize The font size.
##' @return An html object.
##' @export
beanbagStringHighlight <- function(string, textColor="white", bgColor="darkblue", fontSize=8.5) {
    ## Prepare and return a styled string:
    paste(kableExtra::text_spec(string,
                                color=textColor,
                                bold=T,
                                background=bgColor,
                                font_size=fontSize), collapse = " ")
}


##' Prepares the performance table as styled html.
##'
##' This is a description.
##'
##' @param stats The statistics data frame from the endpoint.
##' @return A styled html table.
##' @export
beanbagPerformanceTable <- function(stats) {

    ## If no data-frame, Return NULL:
    if (is.null(dim(stats))) {
        return(NULL)
    }

    ## Percentify rows:
    stats[c(1, 2, 3), ] <- t(apply(stats[c(1,2,3), ], MARGIN=1, percentify))

    ## Round row(s):
    stats[4, ] <- round(as.numeric(stats[4,]), 2)

    ## Get rid of unwanted characters:
    stats <- as.data.frame(mgsub(stats, c("0 %", "NA %"), NA))

    ## Capitalise the rownames:
    rownames(stats) <- capitalise(rownames(stats))

    ## Format and return:
    stats %>%
        knitr::kable("html", escape=FALSE) %>%
        kableExtra::kable_styling(bootstrap_options="striped")

}



##' A function to define a fund report composition
##'
##' This is a description.
##'
##' @param portfolio The portfolio id
##' @param ccy The currency of reporting
##' @param date The date of reporting
##' @param dtype The date type of reporting
##' @param regions The list with the country to region mapping
##' @param session The rdecaf session
##' @return A data-frame with the printable format of holdings
##' @export
COMPOSITION1 <- function(portfolio, ccy, date, dtype, regions, session){

    ## The toplevel for this composition:
    toplevel <- "Subtype"

    ## The sublevel for this compositions:
    sublevel <- NULL

    ## The custom top levels for this composition:
    customtop <- c("subTypeParser", "ctypeByRegionAndDirection")

    ## The columsn to be selected for this composition:
    colselect <- c(
        "Name", "QTY", "CCY", "PX Cost", "PX Last", "Value", "Accrd", "Value (%)", "Exposure", "Exp (%)", "PnL (Unrl)", "PnL (%Inv)", "order"
    )

    ## The summary addons for this composition:
    summaryaddon <- c("currency", "country", "region", "sector")

    ## Get the indata:
    getPrintableHoldings(portfolio, ccy, date, dtype, toplevel, sublevel, customtop, colselect, regions, summaryaddon, session, ctbrInclude=c("Share"))

}


##' A function to define a fund report composition
##'
##' This is a description.
##'
##' @param portfolio The portfolio id
##' @param ccy The currency of reporting
##' @param date The date of reporting
##' @param dtype The date type of reporting
##' @param regions The list with the country to region mapping
##' @param session The rdecaf session
##' @return A data-frame with the printable format of holdings
##' @export
COMPOSITION2 <- function(portfolio, ccy, date, dtype, regions, session){

    ## The toplevel for this composition:
    toplevel <- "Subtype"

    sublevel <- NULL

    customtop <- c("subTypeParser")

    ## The columsn to be selected for this composition:
    colselect <- c(
        "Name", "QTY", "CCY", "PX Cost", "PX Last", "Value", "Accrd", "Value (%)", "Exposure", "Exp (%)", "PnL (Unrl)", "PnL (%Inv)", "order"
    )

    ## The summary addons for this composition:
    summaryaddon <- c("currency", "country", "region", "sector")

    ## Get the indata:
    getPrintableHoldings(portfolio, ccy, date, dtype, toplevel, sublevel, customtop, colselect, regions, summaryaddon, session)

}


##' A function to define a fund report composition
##'
##' This is a description.
##'
##' @param portfolio The portfolio id
##' @param ccy The currency of reporting
##' @param date The date of reporting
##' @param dtype The date type of reporting
##' @param regions The list with the country to region mapping
##' @param session The rdecaf session
##' @return A data-frame with the printable format of holdings
##' @export
COMPOSITION3 <- function(portfolio, ccy, date, dtype, regions, session){

    ## The toplevel for this composition:
    toplevel <- "Subtype"

    ## The columsn to be selected for this composition:
    colselect <- c(
        "Name", "QTY", "CCY", "Expiry", "Rate", "PX Cost", "PX Last", "Value", "Value (%)", "Exposure", "Exp (%)", "PnL (Unrl)", "PnL (%Inv)", "order"
    )

    sublevel <- NULL

    customtop <- c("subTypeParser",
                   "ctypeByRegionAndDirection",
                   "ctypeByDuration"
                   ##"longStraddle",
                   ##"shortStraddle",
                   ##"closedOption",
                   ##"calendarSpread",
                   ##"coveredCall",
                   ##"callSpread",
                   ##"putSpread",
                   ##"syntheticLong"
                   )

    ## The summary addons for this composition:
    summaryaddon <- c("currency", "country", "region", "sector")

    ## Get the indata:
    getPrintableHoldings("portfolio"=portfolio,
                         "ccy"=ccy,
                         "date"=date,
                         "dtype"=dtype,
                         "toplevel"=toplevel,
                         "sublevel"=sublevel,
                         "customTop"=customtop,
                         "colselect"=colselect,
                         "regions"=regions,
                         "summaryaddon"=summaryaddon,
                         "session"=session,
                         ##calendarSpreadExcl=TRUE,
                         ##longStraddleExcl=TRUE,
                         ##shortStraddleExcl=TRUE,
                         ##closedOptionExcl=TRUE,
                         ##synthLongExcl=TRUE,
                         ##callSprdExcl=TRUE,
                         ##putSprdExcl=TRUE,
                         ##cvrdCallExcl=TRUE,
                         ctbrInclude=c("Share", "EQY", "Common Stock"),
                         ctbdInclude=c("Option", "Share"),
                         ctbdHorizon=list("steps"=c(30),
                                          "periodicity"="days"))

}


##' A function to define a fund report composition
##'
##' This is a description.
##'
##' @param portfolio The portfolio id
##' @param ccy The currency of reporting
##' @param date The date of reporting
##' @param dtype The date type of reporting
##' @param regions The list with the country to region mapping
##' @param session The rdecaf session
##' @return A data-frame with the printable format of holdings
##' @export
COMPOSITION4 <- function(portfolio, ccy, date, dtype, regions, session){

    ## The toplevel for this composition:
    toplevel <- "Subtype"

    ## The sublevel for this compositions:
    sublevel <- NULL

    ## The custom top levels for this composition:
    customtop <- c("subTypeParser", "ctypeByRegionAndDirection")

    ## The columsn to be selected for this composition:
    colselect <- c(
        "Name", "QTY", "CCY", "PX Cost", "PX Last", "Value", "Value (%)", "Exposure", "Exp (%)", "PnL (Unrl)", "PnL (%Inv)", "order"
    )

    ## The summary addons for this composition:
    summaryaddon <- c("currency", "country", "region", "sector")

    ## Get the indata:
    getPrintableHoldings("portfolio"=portfolio,
                         "ccy"=ccy,
                         "date"=date,
                         "dtype"=dtype,
                         "toplevel"=toplevel,
                         "sublevel"=sublevel,
                         "customTop"=customtop,
                         "colselect"=colselect,
                         "regions"=regions,
                         "summaryaddon"=summaryaddon,
                         "session"=session,
                         ctbrInclude=c("Share"))
}


##' Defines a function to retreat the report data:
##'
##' @param indata Data as returned by `getPrintableHoldings` function.
##' @return Retreated report data:
##' @export
retreatReport <- function (indata) {
    ## Get holdings:
    holdings <- indata$holdings

    fixit <- function (x) as.numeric(gsub("%", "", gsub(" ", "", gsub(",", "", x))))

    ## Revisit holdings:
    holdings$QTY <- fixit(holdings$QTY)
    holdings$"PX Cost" <- fixit(holdings$"PX Cost")
    holdings$"PX Last" <- fixit(holdings$"PX Last")
    holdings$Value <- fixit(holdings$Value)
    holdings$Accrd <- .emptyToNULL(fixit(holdings$Accrd))
    holdings$"Value (%)" <- fixit(holdings$"Value (%)") / 100
    holdings$Exposure <- fixit(holdings$Exposure)
    holdings$"Exp (%)" <- fixit(holdings$"Exp (%)") / 100
    holdings$"PnL (Unrl)" <- fixit(holdings$"PnL (Unrl)")
    holdings$"PnL (%Inv)" <- fixit(holdings$"PnL (%Inv)") / 100
    holdings$order <- fixit(holdings$order)

    ## Slive the data to be printed:
    ## exdata <- holdings[,1:11]

    ## Done, return the reconstracted input data:
    list(holdings=holdings, consolidation=indata$consolidation)
}


##' Writes table header.
##'
##' Writes the table header to the `row` of `sheet` with given
##' alignments.
##'
##' @param sheet Sheet to write the table header to.
##' @param row Row of sheet to write the table header to.
##' @param header Characters vector containing table header columns.
##' @param alignments Vector of values from `{r, l, c}` for cell.
##' @param baseStyle Base style for the cells
##' @return Next row number.
##' @export
writeTableHeader <- function (sheet, row, header, alignments, baseStyle) {

    ## Translate alignments and filter out nulls:
    alignments <- Filter(Negate(is.null), lapply(alignments, function (x) .lookupAlignment()[[x]]))

    ## Ensure that we have same size header and alignment vectors:
    assertit(length(header) == length(alignments), "Header column and alignment vectors are not of same size")

    ## First, create the row:
    rows <- createRow(sheet, rowIndex=row)

    ## Now create the cells:
    cells <- createCell(rows, colIndex=1:length(header))

    ## Set cell values:
    mapply(setCellValue, cells[1,], header)

    ## Create the list of styles:
    styles <- lapply(alignments, function (x) baseStyle + x)

    ## Apply styles:
    mapply(setCellStyle, cells[1,], styles)

    ## We should be done by now. Return the next row:
    row + 1
}


##' Writes a value to cell to the sheet[row, col] and styles it.
##'
##' @param sheet Sheet to write the cell to.
##' @param row Row address of the cell.
##' @param col Col address of the cell.
##' @param value Cell value.
##' @param style Cell style.
##' @return written cell.
##' @export
writeCell <- function (sheet, row, col, value, style) {
    ## Attempt to get the row:
    rows <- getRows(sheet, row)

    ## Check if we have an existing row, if not create:
    if (length(rows) == 0) {
        rows <- createRow(sheet, rowIndex=row)
    }

    ## Attempt to get the cell:
    cells <- getCells(rows, colIndex=col)

    ## Check if we have an existing cell and get it, if not create one:
    if (is.null(cells)) {
        cell <- createCell(rows, colIndex=col)[1,1][[1]]
    } else {
        cell <- cells[[1]]
    }

    ## Set the cell value:
    setCellValue(cell, value, showNA=FALSE)

    ## Set the cell style:
    setCellStyle(cell, style)

    ## Return the cell:
    cells
}


##' Writes the fund report.
##'
##' @param report Report data.
##' @param file Path to file to write the excel to.
##' @return The workbook written to the file.
##' @export
writeFundReport <- function (report, file) {

    ## If no holdings, mask one:
    if (is.null(report$holdings)) {
        report$holdings <- data.frame("Name"=NA,
                                      "QTY"=NA,
                                      "CCY"=NA,
                                      "Value"=NA,
                                      "Value (%)"=NA,
                                      "Exposure"=NA,
                                      "order"=1,
                                      check.names=FALSE)
    }

    ## Define function globals:
    .SHEETNAME <- "Default"
    .GLOBAL.FONT.NAME <- "DejaVu Sans"
    .GLOBAL.FONT.SIZE <- 9
    .GLOBAL.BORDER.COLOR <- "black"
    .GLOBAL.BORDER.PEN <- "BORDER_MEDIUM"
    .GLOBAL.FILL.COLOR <- "#e2dcea"
    .TITLE.FONT.SIZE <- 28
    .WIDTH <- length(which(safeGrep(colnames(report$holdings), "order") == 0))

    ## Get the holdings:
    holdings <- report$holdings[, 1:.WIDTH]

    ## Create the workbook:
    workbook <- createWorkbook(type="xlsx")

    ## Create the default sheet:
    sheet <- createSheet(workbook, sheetName=.SHEETNAME)

    ## Define the global default cell style:
    styleGlobal <- CellStyle(workbook) + Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=.GLOBAL.FONT.SIZE)

    styleShrcls <- styleGlobal + Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=7, isBold=TRUE) +
        Fill(backgroundColor="#000000", foregroundColor=.GLOBAL.FILL.COLOR, pattern="SOLID_FOREGROUND")

    ## Define the cell style for "TITLE":
    styleTitle <- styleGlobal +
        Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=.TITLE.FONT.SIZE, isBold=TRUE) +
        Fill(backgroundColor="#000000", foregroundColor=.GLOBAL.FILL.COLOR, pattern="SOLID_FOREGROUND")

    ## Define the cell style for "LABEL":
    styleLabel <- styleGlobal +
        Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=.GLOBAL.FONT.SIZE, isBold=TRUE)

    ## Define the cell style for "TABLE HEADER":
    styleHeader <- styleGlobal +
        Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=.GLOBAL.FONT.SIZE, isBold=TRUE) +
        Border(color=.GLOBAL.BORDER.COLOR, pen=c("BORDER_MEDIUM", "BORDER_THIN"), position=c("TOP", "BOTTOM")) +
        Fill(backgroundColor="#000000", foregroundColor=.GLOBAL.FILL.COLOR, pattern="SOLID_FOREGROUND")

    ## Starting row:
    nextRow <- 1

    ## Add Title. First, make sure that we merge the region:
    addMergedRegion(sheet, 1, 1, 1, .WIDTH)

    ## Now add the title:
    writeCell(sheet, nextRow, 1, report$consolidation$fund$name, styleTitle + Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER"))
    nextRow <- nextRow + 1
    nextRow <- nextRow + 1

    ## Add Currency:
    writeCell(sheet, nextRow, 1, "Currency", styleLabel)
    writeCell(sheet, nextRow, 2, report$consolidation$ccy, styleGlobal)
    nextRow <- nextRow + 1

    ## Is the portfolio a mandate?
    isMandate <- all(sapply(report$consolidation$pxinfo, function(x) length(x[["shareclass"]][["subscriptions"]]) == 0 &
                                                                     length(x[["shareclass"]][["feeschedules"]]) == 0))

    ## Add ISIN if fund:
    if (!isMandate) {
        ## Add ISIN:
        writeCell(sheet, nextRow, 1, "ISIN", styleLabel)
        writeCell(sheet, nextRow, 2, .emptyToNA(report$consolidation$isin), styleGlobal)  ## TODO: Add ISIN
        nextRow <- nextRow + 1
    }

    ## Add Launch Date:
    writeCell(sheet, nextRow, 1, "Launch Date", styleLabel)
    writeCell(sheet, nextRow, 2, .formatDate(.emptyToNA(report$consolidation$inception)), styleGlobal + DataFormat("DD/MM/YYYY"))  ## TODO: Add Launch Date
    nextRow <- nextRow + 1

    ## Add Report Date:
    writeCell(sheet, nextRow, 1, "Report Date", styleLabel)
    writeCell(sheet, nextRow, 2, .formatDate(.emptyToNA(report$consolidation$asof)), styleGlobal + DataFormat("DD/MM/YYYY"))
    nextRow <- nextRow + 1
    nextRow <- nextRow + 1

    ## Define the table header cell alignments:
    ##
    ## Note that the alignment is problematic with the current version
    ## of xlsx library. Indeed, it works with xls files, but then we
    ## have other problems. It seems to me that the xlsx writing is
    ## problematic, most likely due to the POI library version xlsx
    ## library is using.
    ##
    ## Therefore, let's define the first column as "centered" so that
    ## all following cells are also centered. Once the bug is fixed,
    ## we can use the first statement instead.

    columnStyles <- list("Name"        =list("align"="c", "rowStyle"="sGlobalRowFillRowFont0", "width"=34L * 256L),
                         "QTY"         =list("align"="c", "rowStyle"="sGlobalRowFillRowFont1", "width"=14L * 256L),
                         "CCY"         =list("align"="c", "rowStyle"="sGlobalRowFillRowFont2", "width"= 4L * 256L),
                         "Expiry"      =list("align"="c", "rowStyle"="sGlobalRowFillRowFont2", "width"=11L * 256L),
                         "Rate"        =list("align"="c", "rowStyle"="sGlobalRowFillRowFont1", "width"=11L * 256L),
                         "PX Cost"     =list("align"="c", "rowStyle"="sGlobalRowFillRowFont3", "width"=13L * 256L),
                         "PX Last"     =list("align"="c", "rowStyle"="sGlobalRowFillRowFont3", "width"=13L * 256L),
                         "Value"       =list("align"="c", "rowStyle"="sGlobalRowFillRowFontX1","width"=13L * 256L),
                         "Accrd"       =list("align"="c", "rowStyle"="sGlobalRowFillRowFont1", "width"=10L * 256L),
                         "Value (%)"   =list("align"="c", "rowStyle"="sGlobalWgtFillRowFont0", "width"=10L * 256L),
                         "Exposure"    =list("align"="c", "rowStyle"="sGlobalRowFillRowFontX1","width"=13L * 256L),
                         "Exp (%)"     =list("align"="c", "rowStyle"="sGlobalRowFillRowFont4", "width"=10L * 256L),
                         "PnL (Unrl)"  =list("align"="c", "rowStyle"="sGlobalRowFillPnlFont0", "width"=12L * 256L),
                         "PnL (%Inv)"  =list("align"="c", "rowStyle"="sGlobalRowFillPnlFont1", "width"=11L * 256L))


    sGlobalRowFont0 <- function() {styleGlobal + rowFont + DataFormat("#,##0.0")}
    sGlobalRowFont0X <- function() {styleGlobal + rowFont + DataFormat("#,##0")}
    styleShrclass0 <- function () {styleShrcls + Alignment(h="ALIGN_CENTER")}

    globalAlign <- function(h, indent) {
        alignment <- Alignment(h=h)
        alignment[["indent"]] <- indent
        alignment
    }

    sGlobalRowFillRowFont0 <- function () {styleGlobal + rowFill + rowFont}
    sGlobalRowFillRowFont1 <- function () {styleGlobal + rowFill + rowFont + DataFormat("#,##0.0") + globalAlign("ALIGN_LEFT", 1)}
    sGlobalRowFillRowFontX1<- function () {styleGlobal + rowFill + rowFont + DataFormat("#,##0") + globalAlign("ALIGN_LEFT", 1)}
    sGlobalRowFillRowFont2 <- function () {styleGlobal + rowFill + rowFont + DataFormat("@") +  globalAlign("ALIGN_CENTER", 0)}
    sGlobalRowFillRowFont3 <- function () {styleGlobal + rowFill + rowFont + DataFormat("#,##0.00") + globalAlign("ALIGN_LEFT", 1)}
    sGlobalRowFillRowFont4 <- function () {styleGlobal + rowFill + rowFont + DataFormat("#,##0.0 %")+ globalAlign("ALIGN_LEFT", 1)}
    sGlobalWgtFillRowFont0 <- function () {styleGlobal + wgtFill + rowFont + DataFormat("#,##0.0 %")+ globalAlign("ALIGN_LEFT", 1)}
    sGlobalRowFillPnlFont0 <- function () {styleGlobal + rowFill + pnlFont + DataFormat("#,##0")+ globalAlign("ALIGN_LEFT", 1)}
    sGlobalRowFillPnlFont1 <- function () {styleGlobal + rowFill + pnlFont + DataFormat("#,##0.0 %")+ globalAlign("ALIGN_LEFT", 1)}

    ## Define the table headers:
    tableHeader <- colnames(holdings)

    ## How shall the table headers be aligned?
    tableAligns <- unlist(sapply(columnStyles[tableHeader], function(x) x[["align"]]))

    ## Write the table header (and a blank one):
    nextRow <- writeTableHeader(sheet, nextRow, tableHeader, tableAligns, styleHeader) + 1

    ## Iterate over each holding row and add to the sheet:
    for (i in 1:nrow(holdings)) {

        ## Is it a position row?
        isPosition <- is.na(report$holdings$order[i])

        ## Is it a sub-total?
        isSubtotal <- !isPosition && grepl("SUBTOTAL", toupper(report$holdings$Name[i]))

        ## Is it a sub-title:
        isSubtitle <- !isPosition && !isSubtotal

        ## Get the row color first:
        if (isSubtitle) {
            rowColor <- "#FFFFFF"
        } else if (isSubtotal) {
            rowColor <- "#C4BD97"
        } else {
            rowColor <- ifelse(i %% 2 == 0, "#E6E6E6", "#F0F0F0")
        }

        ## Define the row fill:
        rowFill <- Fill(backgroundColor="#000000", foregroundColor=rowColor, pattern="SOLID_FOREGROUND")

        if (isSubtotal) {
            wgtForegroundColor <- rowColor
        } else if (isSubtitle) {
            wgtForegroundColor <- "#FFFFFF"
        } else {
            wgtForegroundColor <- "#D3D3D3"
        }

        ## wgtFill <- Fill(backgroundColor="#000000", foregroundColor=ifelse(isSubtotal, rowColor, "#D3D3D3"), pattern="SOLID_FOREGROUND")
        wgtFill <- Fill(backgroundColor="#000000", foregroundColor=wgtForegroundColor, pattern="SOLID_FOREGROUND")

        ## Define the font size (subtitles slightly bigger):
        rowFontSize <- .GLOBAL.FONT.SIZE + ifelse(isSubtitle, 3 - report$holdings$order[i], 0)

        ## Define the row font:
        rowFont <- Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=rowFontSize, isBold=!isPosition, isItalic=isSubtotal)

        ## Get the column index with the PnL:
        pnlIdx <- which(safeGrep(tableHeader, "PnL") == "1")[1]

        ## Define the PnL fill:
        if (!isSubtitle && !is.na(holdings[i, pnlIdx]) && holdings[i, pnlIdx] < 0) {
            pnlFont <- Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=rowFontSize, isBold=!isPosition, isItalic=isSubtotal, color="#AA3333")
        } else if (!isSubtitle && !is.na(holdings[i, pnlIdx]) && holdings[i, pnlIdx] > 0) {
            pnlFont <- Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=rowFontSize, isBold=!isPosition, isItalic=isSubtotal, color="#33AA33")
        } else {
            pnlFont <- rowFont
        }

        ## Define the list of cell styles:
        cellStyles <- lapply(columnStyles[tableHeader], function(x) do.call(x[["rowStyle"]], list()))

        ## Get the cell values:
        cellValues <- holdings[i, , drop=FALSE]

        ## Define the writer function:
        .writer <- function (idx) writeCell(sheet, nextRow, idx, cellValues[1, idx], cellStyles[[idx]])

        ## Write cells:
        lapply(1:.WIDTH, .writer)

        ## Redefine the next row:
        nextRow <- nextRow + 1

        ## Give a break after subtotals?
        if (isSubtotal) {
            ## writeCell(sheet, nextRow, 8, NA, styleGlobal + Fill(backgroundColor="#000000", foregroundColor="#D3D3D3", pattern="SOLID_FOREGROUND"))
            nextRow <- nextRow + 1
        }
    }

    ## If it is a mandate, write mandate summary:
    if (isMandate) {
        ## Add NAV row name:
        writeCell(sheet, nextRow, 2, "NAV", styleLabel)
        ## Add the Performance YTD row name:
        writeCell(sheet, nextRow+1, 2, "Peformance (YTD)", styleLabel)

        ## Define the global default cell style:
        for (px in report$consolidation$pxinfo) {
            ## Add NAV value:
            writeCell(sheet, nextRow,   4, .emptyToNA(px$nav$qty), sGlobalRowFont0())
            ## Add the performance ytd value:
            writeCell(sheet, nextRow+1, 4, .emptyToNA(px$ytdext), sGlobalRowFillRowFont4())
        }
        ## And update next rows:
        ## nextRow <- nextRow + 2
    }

    ## If it is a fund, write the fund summary:
    if (!isMandate) {
        ## Add number of certificates:
        writeCell(sheet, nextRow,   2, "Shareclass", styleLabel)
        writeCell(sheet, nextRow+1, 2, "# Certificates", styleLabel)
        writeCell(sheet, nextRow+2, 2, "NAV/Share", styleLabel)
        writeCell(sheet, nextRow+3, 2, "NAV", styleLabel)
        writeCell(sheet, nextRow+4, 2, "GAV", styleLabel)
        writeCell(sheet, nextRow+5, 2, "Peformance (YTD)", styleLabel)

        stCol <- 4

        ## Define the global default cell style:
        for (px in report$consolidation$pxinfo) {

            ## Add number of certificates:
            writeCell(sheet, nextRow, stCol, .emptyToNA(px$shareclass$name),  styleShrclass0())
            ## Add number of certificates:
            writeCell(sheet, nextRow+1, stCol, .emptyToNA(px$sharecount_curr), sGlobalRowFont0())
            ## Add NAV/Share:
            writeCell(sheet, nextRow+2, stCol, .emptyToNA(px$px_clsccy$qty), sGlobalRowFont0())
            ## Add NAV:
            writeCell(sheet, nextRow+3, stCol, .emptyToNA(px$nav_adjusted$qty), sGlobalRowFont0X())
            ## Add AuM:
            writeCell(sheet, nextRow+4, stCol, .emptyToNA(px$gav_clsccy$qty), sGlobalRowFont0X())
            ## Add Peformance (YTD):
            writeCell(sheet, nextRow+5, stCol, .emptyToNA(px$ytdext), sGlobalRowFillRowFont4())

            ## Compute the required width for the shareclass name cell:
            requiredWidth <- ifelse(is.null(px$shareclass$name), 0, nchar(px$shareclass$name) * 180)

            ## Update the width(s) for corresponding columns:
            columnStyles[tableHeader][[stCol]]$width <- max(columnStyles[tableHeader][[stCol]]$width,  requiredWidth)

            ## Next shareclass info, if any:
            stCol <- stCol + 1
        }

        ## And update the new row:
        ## nextRow <- nextRow + 6
    }

    ## Lastly, we will set print settings:
    sheet$getPrintSetup()$setLandscape(TRUE);
    sheet$getPrintSetup()$setPaperSize(J("org/apache/poi/ss/usermodel/PaperSize", "valueOf", "A4_PAPER"))
    sheet$getPrintSetup()$setFitWidth(.jshort(1L))

    ## Compute the scale to be used:
    scale <- as.integer((37500 / sum(sapply(columnStyles[tableHeader], function(x) x[["width"]]))) * 100)

    ## Dynamically set scale:
    sheet$getPrintSetup()$setScale(.jshort(scale))

    sheet$setMargin(.jshort(0L), 0.2);
    sheet$setMargin(.jshort(1L), 0.2);
    sheet$setMargin(.jshort(2L), 0.2);
    sheet$setMargin(.jshort(3L), 0.2);

    sheet$setRepeatingRows(J("org/apache/poi/ss/util/CellRangeAddress", "valueOf", "1:9"))

    ## Set the column widths:
    lapply(1:length(columnStyles[tableHeader]), function(i) {
        sheet$setColumnWidth(as.integer(i-1), as.integer(columnStyles[[tableHeader[i]]][["width"]]))
    })

    titleRow <- sheet$getRow(0L)
    titleRow$setHeightInPoints(.jfloat(titleRow$getHeightInPoints() * .TITLE.FONT.SIZE / 10))

    ## Save the workbook:
    saveWorkbook(workbook, file=file)

    ## Done, return:
    workbook
}


##' TODO
##'
##' @param orderedHoldings TODO.
##' @param nestedHoldings TODO.
##' @return TODO.
##' @export
appendCash <- function(orderedHoldings, nestedHoldings){

    ## If no cash, return holdings:
    if (NROW(orderedHoldings[["cash"]]) == 0) {
        return(nestedHoldings)
    }

    ## Get the cash positions:
    cash <- orderedHoldings[["cash"]]

    ## Append the title for cash:
    cash <- rbind(c("Cash", rep(NA, NCOL(cash)-1)), cash)

    ## Apend the Cash subtotal row:x
    cash <- rbind(cash, c("Cash Subtotal", rep(NA, NCOL(cash) -1)))

    ## Safely rbind cash and other holdings:
    nestedHoldings <- safeRbind(list(cash, nestedHoldings))

    ## Fill the isHeader value for cash title:
    nestedHoldings[nestedHoldings[,"Name"] == "Cash", "isHeader"] <- "TRUE"

    ## Fill the order value for cash title:
    nestedHoldings[nestedHoldings[,"Name"] == "Cash", "order"] <- "1"

    ## Fill the isFooter value for cash subtotal:
    nestedHoldings[nestedHoldings[,"Name"] == "Cash Subtotal", "isFooter"] <- "TRUE"

    ## Fill the order value for cash subtotal:
    nestedHoldings[nestedHoldings[,"Name"] == "Cash Subtotal", "order"] <- "1"

    ## Return:
    nestedHoldings
}


##' TODO
##'
##' @param holdings TODO.
##' @return TODO.
##' @export
getSubtotalledHoldings <- function(holdings) {

    ## Divide the holdings to list by order indices.
    orderIdx <- lapply(na.omit(unique(holdings[,"order"])), function(x) holdings[,"order"] == x)

    ## Replace NA's with FALSE:
    orderIdx <- lapply(orderIdx, function(x) which(ifelse(is.na(x), FALSE, x)))

    ## Name the list elements with order:
    names(orderIdx) <- na.omit(unique(holdings[,"order"]))

    ## Create index pairs representing the start and end of chunk:
    idxPairs <- lapply(orderIdx, function(x) cbind(x[seq(1, length(x), 2)],
                                                   x[seq(2, length(x), 2)]))

    ## Iterate over index pairs and run the getSummary for subtotals:
    allSubtotals <- lapply(idxPairs, function(x) do.call(rbind, apply(x, MARGIN=1, function(y) getSummary(holdings, y))))

    ## Iterate over index pairs and assign the values to correct subtotal row index:
    for (i in 1:length(idxPairs)){

        ## Get the footer row index:
        footerIdx <- idxPairs[[i]][,2]

        ## Get the subtotal values:
        vals <- allSubtotals[[i]]

        ## Assign to footer row index:
        holdings[footerIdx, colnames(vals)] <- vals
    }

    ## Done, return:
    holdings

}


##' TODO
##'
##' @param holdings TODO.
##' @param idxpair TODO
##' @return TODO.
##' @export
getSummary <- function(holdings, idxpair){
    ## Prepare the chunks:
    chunk <-  holdings[idxpair[1]:idxpair[2],]

    ## Get the holding summaries and return:
    data.frame("Value"=sum(as.numeric(na.omit(chunk[,"Value"]))),
               "Value (%)"=sum(as.numeric(na.omit(chunk[,"Value (%)"]))),
               "Exposure"=sum(as.numeric(na.omit(chunk[,"Exposure"]))),
               "Exp (%)"=sum(as.numeric(na.omit(chunk[,"Exp (%)"]))),
               "PnL (Unrl)"=sum(as.numeric(na.omit(chunk[, "PnL (Unrl)"]))),
               "PnL (%Inv)"=mean(as.numeric(na.omit(chunk[,"PnL (%Inv)"]))), check.names=FALSE)
}


##' Defines the alignment translation table.
##'
##' @return TODO.
##' @export
.lookupAlignment <- function() {
    list(r=Alignment(h="ALIGN_RIGHT"), c=Alignment(h="ALIGN_CENTER"), l=Alignment(h="ALIGN_LEFT"))
}
