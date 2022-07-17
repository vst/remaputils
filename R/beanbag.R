##' Gets the previous NAN either using external or internal valuations.
##'
##' This is a description.
##'
##' @param portfolio The portfolio id.
##' @param date The report date.
##' @param ccy The portoflio currency.
##' @param period The period memnonic, i.e Y-0 for last year end.
##' @param external TRUE if external valuation is to be used.
##' @param session The rdecaf session.
##' @param inception The inception date. Default is 1900-01-01
##' @param useInt Should the internal NAV be used if external does not exist? Default is FALSE.
##' @param tolerance The number of days to consider as tolerance for the previous date in external table. Default is 2.
##' @return A list.
##' @export
getPreviousNAV <- function(portfolio, date, ccy, period, external, session, inception=as.Date("1900-01-01"), useInt=FALSE, tolerance=2) {

    ## Get the previous date:
    previousDate <- max(dateOfPeriod(period, date), inception)
    
    result <- list("value"=NA,"date"=previousDate)
                   

    ## If external is true, get previuos NAV from external table:
    if (external) {

        ## Get the external valuations:
        extVal <- as.data.frame(getResource("externalvaluations", params=list("format"="csv", "page_size"=-1, "portfolio"=portfolio), session=session))

        ## If not external valuation, retur NA:
        if (NROW(extVal) == 0) {
            return(list("value"=NA,
                        "date"=previousDate))
        }

        ## If any shareclass exists, construct NAV from shareclass sums:
        if (any(!is.na(extVal[, "shareclass"]))) {

            extVal <- extVal[!is.na(extVal[, "shareclass"]),]

            ## Iterate over shareclasses:
            lastVals <- do.call(rbind, lapply(1:length(unique(extVal[, "shareclass"])), function(i) {

                id <- unique(extVal[, "shareclass"])[i]
                shrclsVals <- extVal[extVal[, "shareclass"] == id, c("date", "ccy", "nav")]

                return(data.frame(valueOfNearestDate(targetDate=previousDate,
                                                     data=shrclsVals,
                                                     tolerance=tolerance,
                                                     dateField="date",
                                                     valueField="nav"), "ccy"=shrclsVals[1, "ccy"]))


            }))

            fx <- lapply(lastVals[, "ccy"], function(cy) getOhlcObsForSymbol(session, paste0(cy, ccy), previousDate, 0)[, "close"])
            fx[which(lastVals[, "ccy"] == ccy)] <- 1

            return(list("value"=sum(lastVals[, "value"] * do.call(c, fx)),
                        "date"=previousDate))
        }

        result <- valueOfNearestDate(targetDate=previousDate,
                                     data=extVal,
                                     tolerance=tolerance,
                                     dateField="date",
                                     valueField="nav")

        if (!is.na(result[["value"]]) | !useInt) {
            ## If no shareclass exists, use the nav only:
            return(result)
        }
    }

    if (!external | is.na(result[["value"]])) {
        ## Get the asset evolution (pconsolidation)
        assEvl <- getAssetEvolution(portfolio, date, "2", session)
        

            return(valueOfNearestDate(targetDate=previousDate,
                                      data=assEvl,
                                      tolerance=tolerance,
                                      dateField="date",
                                      valueField="nav"))
    }

}


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
    tlevelIdx <- holdings[, "order"] == "1"
    tlevelIdx <- ifelse(is.na(tlevelIdx), FALSE, tlevelIdx)

    ## Get row index range for top level holdings:
    tlevelIndices <- cbind(which(tlevelIdx), c(tail(which(tlevelIdx), -1) - 1, NROW(holdings)))

    ## Get the holdings by top level:
    hTopLevel <- apply(tlevelIndices, MARGIN=1, function(x) holdings[x[1]:x[2], ])

    ## Rearrange the holdings:
    holdings <- do.call(rbind, hTopLevel[order(sapply(hTopLevel, function(h) median(na.omit(as.numeric(h[, "AClass 1 Order"])))))])

    ##
    grpSum <- getPositionsByGrp(holdings, col)

    ## Done, return list:
    list("holdings" = holdings,
         "subtotals"= holdings[tlevelIndices[,2], ],
         "asstLbts" = grpSum[["asstLbts"]],
         "asstLbtsSum"=grpSum[["asstLbtsSummary"]],
         "positions"=grpSum[["positions"]],
         "total"=sum(na.omit(holdings[is.na(holdings[, "order"]), "Value"] %>% as.numeric()))
         )
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
    grpAsstLbts <- lapply(grpPos, function(x) list("asset"=cleanNARowsCols(x[numerize(x[, "Value"]) > 0,]), "liability"=cleanNARowsCols(x[numerize(x[, "Value"]) < 0, ])))

    ## Assign the names:
    names(grpAsstLbts) <- unique(na.omit(holdings[, col]))

    ## Get the asset liability summary:
    grpLbtsSummary <- do.call(rbind, lapply(grpAsstLbts, function(x) c("totalAsset"=sum(numerize(safeColumn(x[["asset"]], "Value"))),
                                                                       "totalLiability"=sum(numerize(safeColumn(x[["liability"]], "Value")))
                                                                       )
                                                                       )
                                                                       )
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

    ## If length is 1, be neutral and return:
    if (length(values) == 1) {
        return(data.frame("value"=values,
                          "idx"=1,
                          "class"=1,
                          "col"=ifelse(values < 0, "#FF7F50", "#006400"),
                          stringsAsFactors=FALSE))
    }

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


##' A function to style multiple cols in data frame
##'
##' This is a description.
##'
##' @param x The data frame.
##' @param cols The column indices.
##' @param ... Arguments for kablExtra.
##' @return A data frame with style rows.
##' @export
multicol_spec <- function(x, cols, ...) {
    for (col in cols)
        x <- kableExtra::column_spec(x, col, ...)
    x
}



##' Prepares the NAV data frame.
##'
##' This is a description.
##'
##' @param x A list with "portfolio", "ccy", "currentNAV", "previous NAV".
##' @param inception The launch date.
##' @param pxinfo The YTD Performance.
##' @param investments The investment/transfer trades.
##' @param useIntYtd Should the internal YTD be used if external doesn't exist? Default is FALSE.
##' @return A data frame.
##' @export
beanbagNAVTable <- function (x, inception, pxinfo, investments=NULL, useIntYtd=FALSE) {

    ## Prepare the name column:
    names <- c("Name",
               "Currency",
               "Inception",
               paste0("NAV (", x[["date"]], ")"),
               paste0("GAV (", x[["date"]], ")"),
               paste0("NAV (", x[["previousDate"]], ")"),
               "#Shares",
               "ISIN",
               "NAV/Share",
               trimws(paste0("Perf (YTD) ", ifelse(!is.na(pxinfo[, "isin"]), as.character(pxinfo[, "shareclass"]), ""))))

    ytd <- safeNull(sapply(pxinfo[,"ytdext"], function(x) ifelse(is.na(x), NA, percentify(x))))

    if (useIntYtd) {
        ytd <- ifelse(!is.na(ytd), ytd, safeNull(sapply(pxinfo[,"ytdint"], function(x) ifelse(is.na(x), NA, percentify(x)))))
    }

    ## Prepare the value column:
    value <- c(x[["portfolio"]],
               x[["ccy"]],
               as.character(inception),
               paste0(beautify(x[["currentNAV"]]), " ", x[["ccy"]]),
               paste0(beautify(x[["currentGAV"]]), " ", x[["ccy"]]),
               paste0(beautify(x[["previousNAV"]]), " ", x[["ccy"]]),
               paste(pxinfo[, "noshares"], collapse = ", "),
               paste(pxinfo[, "isin"], collapse = ", "),
               safeNull(x[["navshare"]]),
               ytd)

    if (!is.null(investments)) {

        if (is.null(pxinfo)) {
            pxinfo <- data.frame("isFund"=FALSE)
        }

        names <- c(names, ifelse(!any(pxinfo[, "isFund"]), "Net Transfers", "Net Subs/Redm"))
        value <- c(value, paste(beautify(sum(investments[, "valamt_refccy"])), x[["ccy"]]))
    }

    ## Make the data frame:
    df1 <- data.frame("Name"=names, "Value"=value, check.rows=FALSE, check.names=FALSE, stringsAsFactors=FALSE)

    ## For non funds:
    if (!any(pxinfo[, "isFund"])) {
        df1 <- df1[safeGrep(df1[, "Name"], "GAV ") == "0", ]
        df1 <- df1[!df1[, "Name"] == "ISIN", ]
        df1 <- df1[!df1[, "Name"] == "NAV/Share", ]
        df1 <- df1[!df1[, "Name"] == "#Shares", ]
    }

    ## Set the column to NULL:
    colnames(df1) <- NULL

    ## Set the rows to NULL:
    rownames(df1) <- NULL

    ## Done, return:
    return(df1)

}


##' Gets the benchmark elements for a portfolio.
##'
##' This is a description.
##'
##' @param portfolio The portfolio id.
##' @param start The desired start date.
##' @param end  The desired end date.
##' @param session The rdecaf session.
##' @param rfSymbol the risk free series, defaults to SHY.
##' @param id the benchmark ID, defaults to NA.
##' @param per the periodicity, defaults to M.
##' @param win the window, defaults to Y.
##' @return A list containing all applicable benchmark info.
##' @export
getBenchmark <- function(portfolio, start, end, session, rfSymbol=NA##"iShares 1-3 Year Treasury Bond ETF"
,id=NA
,per="M"
,win="Y"
) {

        benchmarkID <- if_else(is.na(id),as.numeric(getDBObject("portfolios", session, addParams=list("id"=portfolio))[, "benchmark"]),as.numeric(id))
        benchmarkSymbol <- ifelse(is.na(benchmarkID), NA, getResource("ohlcs", params=list("id"=benchmarkID), session=session)[["results"]][[1]][["symbol"]])
        benchmarkName <- benchmarkSymbol 
        benchmarkResource <- ifelse(is.na(benchmarkID), NA, getResource("resources", params=list("symbol"=benchmarkSymbol), session=session)[["results"]])
        if(!is.na(benchmarkResource)) {
        if(!is.null(benchmarkResource[[1]])) {
            benchmarkName <- benchmarkResource[[1]][["name"]]
        }
        }
        ## Initialize the query parameters:
        params <- list("benchmarks"=benchmarkID,
                       "start"=start,
                       "end"=end,
                       "frequency"="daily")

        if (is.na(benchmarkID)) {
            benchmark <- NULL
            benchmarkFlat <- NULL
        } else {
            benchmark <- getResource("performance", params=params, session=session)
            benchmarkFlat <- flattenPerformance(benchmark, "benchmarks", end, start, periodicity=per, window=win)
        }

        return(list(
        "benchmarkID"=benchmarkID,
        "benchmarkSymbol"=benchmarkSymbol,
        "benchmarkName"=benchmarkName,
        "benchmark"=benchmark,
        "benchmarkFlat"=benchmarkFlat
        )
        )

    }


##' Gets the riskfree elements for a portfolio.
##'
##' This is a description.
##'
##' @param rfSymbol the risk free series, defaults to NULL. Constant or symbol.
##' @param start The desired start date.
##' @param end  The desired end date.
##' @param session The rdecaf session.
##' @return a time series containing risk free rate.
##' @export
getRf <- function(rfSymbol=NULL,start,end,session) {

if(is.null(rfSymbol)) {return(NULL)}

start <- as.Date(start)
end <- as.Date(end)

if(class(rfSymbol)=="numeric") {
  close <- seq(1,1+rfSymbol,length.out=numerize(end-start)+1)
  rf <- data.frame(close=close,date=seq(start,end,1)) ##make sure dates and values are consistent.
}
else{
  rf <- getOhlcObsForSymbol("session"=session, "symbol"=rfSymbol, lte=end, lookBack=numerize(end-start))
  if(NROW(rf)==0) {
  rf <- data.frame(close=rep(0,numerize(end-start)+1),date=seq(start,end,1))
  }
  else {
  rf <- rf %>%
    right_join(data.frame(date=seq(start,end,1)), by="date") %>%
    fill(close,.direction="downup")
  }
  
rf <- rf %>% 
  mutate(indx=c(NA,diff(log(close)))) %>%
  mutate(close=cumsum(if_else(row_number()==1,1,indx)))
}
 
return(xts::as.xts(rf$close, order.by=rf$date))

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
##' @param period The period memnonic. Default is Y-0.
##' @param benchMark a list containing the elements from getBenchmark above. Defaults to NULL.
##' @param rF the risk free data. Defaults to NULL.
##' @return A list with the indexed cumulative return series and performance stats.
##' @export
getPerformance <- function(portfolio, start, end, freq="daily", session, period="Y-0", benchMark=NULL, rF=NULL) {

    ##
    start <- dateOfPeriod(period, end)

    periodN <- substr(period, 3, 3)

    if (any(mgrep(periodN, c("1", "2", "3", "4")) != "0")) {
        end <- dateOfPeriod(paste0(substr(period, 1, 2), as.numeric(substr(period, 3, 3)) - 1))
    }

    

    ## Construct the params:
    params <- list("portfolios"=portfolio,
                   "start"=start,
                   "end"=end,
                   "frequency"=freq)
                   


    print(paste0("Retrieving performance data for portfolio: ", portfolio))

    ## Get the asset evolves:
    performance <- rdecaf::getResource("performance", params=params, session=session)

    series <- NA

    ##stats <- function() {
        retval <- data.frame("MTD"=rep(NA, 4),
                             "YTD"=rep(NA, 4),
                             "Y1"=rep(NA, 4),
                             "Y2"=rep(NA, 4),
                             "Y3"=rep(NA, 4))
        cYear <- as.numeric(substr(Sys.Date(), 1, 4))
        colnames(retval) <- c("MTD", "YTD", as.character(cYear-1), as.character(cYear-2), as.character(cYear-3))
        rownames(retval) <- c("return", "maxddown", "stdev", "sharpe")
        ##return(retval)
    ##}
    stats <- retval
    
    xstats <- computeReturnStats(NULL,
                                     "price",
                                     "date",
                                     method="discrete",
                                     returnOnly=FALSE,
                                     benchmark=NULL)

    if (length(performance[["returns"]][["index"]]) == 0) {

        return(list("series"=series,
                    "stats"=stats,
                    "xstats"=xstats))
    }
    

    series <- unlist(performance[["indexed"]][["data"]])##sapply(performance[["indexed"]][["data"]], function(x) ifelse(is.null(x[[1]]), NA, x[[1]]))
    series[1] <- ifelse(is.na(series[1]), 1, series[1])
    series <- zoo::na.locf(series, fromLast=FALSE)
    
    ## Get the performance index series:
    series <- xts::as.xts(series,
                          order.by=as.Date(unlist(performance[["indexed"]][["index"]])))
                          
                          
    stats <- performance[["statistics"]][["univariate"]][["portfolios"]][[1]][["stats"]]

    stats <- lapply(1:length(stats), function(i) {
        stats[[i]]$stats <- lapply(stats[[i]]$stats, function(x) ifelse(is.null(x), NA, x))
        stats[[i]]
    })

    ## Get the stats:
    stats <- t(safeRbind(lapply(stats, function(x) do.call(cbind, x[["stats"]]))))

    ## Add the colnames:
    colnames(stats) <- sapply(performance[["statistics"]][["univariate"]][["portfolios"]][[1]][["stats"]], function(x) x[["label"]])

    stats <- stats[, !colnames(stats) == "CST"]          


    ## Compute the extra statistics
             
    xstats <- computeReturnStats(data.frame("price"=series, "date"=zoo::index(series),"ret"=unlist(performance[["returns"]][["data"]])),##as.Date(unlist(performance[["indexed"]][["index"]]))),
                                 "price",
                                 "date",
                                 method="discrete",
                                 returnOnly=FALSE,
                                 benchmark=benchMark,
                                 rfts=safeNull(rF))

    ## Return:
    list("series"=series,
         "stats"=stats,
         "xstats"=xstats)
}


##' Prepares the holdings detail data frame.
##'
##' This is a description.
##'
##' @param holdings The holdings data frame.
##' @param colSelect Which columns to select.
##' @param nav The net asset value.
##' @return A list with holdings details information.
##' @export
getHoldingsDetails <- function(holdings, colSelect, nav=NULL) {

    ## Replace the NA order with the highest available order + 1:
    holdings[is.na(holdings[, "order"]), "order"] <- max(na.omit(holdings[, "order"])) + 1

    ## Iterate over holding rows and remove parent name from tag name:
    for (row in 1:NROW(holdings)) {

        ## Is the row a header:
        isHeader <- as.logical(holdings[row, "isHeader"])

        ## If header, remove the parent name:
        if (isHeader) {
            subname <- paste0(holdings[row, "Name"], ",")
            holdings[, "Name"] <- gsub(subname, "", holdings[, "Name"])
        }
    }

    holdings[substr(holdings[, c("Name")], nchar(holdings[, "Name"])-2, nchar(holdings[, "Name"])) == " NA", "Name"] <- " NA"

    ## Trim all the names:
    holdings[, "Name"] <- trimws(holdings[, "Name"])
    holdings[holdings[, "Name"] == "NA", "Name"] <- ""
    holdings <- holdings[!isNAorEmpty(holdings[, "Name"]), ]

    ## Get the totals
    totals <- holdings[holdings[, "order"] == "1", c("Name", "Value", "Value (%)", "Exposure", "Exp (%)")]

    ## Compute the grand total values:
    grandTotal <- colSums(totals[, c("Value", "Value (%)", "Exposure", "Exp (%)")])

    ## Consider:
    consider <- apply(mgrep(totals[, "Name"], c("Cash", "Money Market")), MARGIN=1, function(x) all(x == "0"))

    if (!is.null(nav)) {
        grandTotal["Exp (%)"] <- sum(totals[consider, "Exposure"]) / nav
    } else {
        grandTotal["Exp (%)"] <- sum(totals[consider, "Exposure"]) / grandTotal["Value"]
    }

    grandTotal["Exposure"] <- sum(totals[consider, "Exposure"])

    ## Append a line to the end:
    holdings <- rbind(holdings, holdings[1, ])

    ## Append the grand total values:
    holdings[NROW(holdings), c("Value", "Value (%)", "Exposure", "Exp (%)")] <- grandTotal

    ## Name the line as 'Grand Total':
    holdings[NROW(holdings), "Name"] <- "Grand Total"

    ## Trim all the names:
    holdings[, "Name"] <- trimws(holdings[, "Name"])

    ## Get the row indices which are totals:
    totalRowIdx <- which(holdings[, "order"] == "1")

    ## Get the row indices which are subtotals:
    subtlRowIdx <- which(holdings[, "order"] == "2")

    ## Get the row indices which are subsubtotals:
    subtl2RowIdx <- which(holdings[, "order"] == "3")

    ## Get the row indices which are holdings:
    holdsRowIdx <- which(holdings[, "order"] == "3" | holdings[, "order"] == max(na.omit(holdings[, "order"])))

    ## Remove the order column:
    #holdings <- holdings[, !(colnames(holdings) == "order")]

    ## Remove the header column:
    #holdings <- holdings[, !(colnames(holdings) == "isHeader")]

    ## Remove the rownames:
    rownames(holdings) <- NULL

    colSelect <- colSelect[match(colnames(holdings), colSelect)]

    ## Select the columns:
    holdings <- holdings[, colSelect]

    ## Replace long country strings:
    if (any(colnames(holdings) == "Country")) {
        holdings[, "Country"] <- gsub("United States Of America", "United States", holdings[, "Country"])
    }

    ## Define the keys which are prices:
    priceKeys <- c("QTY", "PX Cost", "PX Last")

    ## Get the indices of the price keys:
    priceIdx <- lapply(priceKeys, function(x) colnames(holdings) == x)

    ##: TODO
    priceIdx <- apply(do.call(cbind, priceIdx), MARGIN=1, any)

    ## Beautify percentages:
    suppressWarnings(holdings[, priceIdx] <- apply(holdings[, priceIdx], MARGIN=2, function(x) round(x, 4)))

    ## Define the monetary keys:
    monetaryKeys <- c("QTY", "PX Cost", "PX Last", "Value", "Exposure", "Accrd", "PnL (Unrl)")

    ## Get the indices of the monetary keys:
    monetaryIdx <- apply(mgrep(colnames(holdings), monetaryKeys), MARGIN=1, function(x) any(x != "0"))

    ## Beautify monetary amounts:
    holdings[, monetaryIdx] <- apply(holdings[, monetaryIdx], MARGIN=2, function(x) trimws(beautify(x)))

    ## Define the percentage keys:
    percentageKeys <- c("Value (%)", "Exp (%)", "PnL (%)", "PnL (%Inv)", "PnL (Contr)")

    ## Get the indices of the percentage keys:
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
         "subtl2Idx"=subtl2RowIdx,
         "totalIdx"=totalRowIdx,
         "holdsIdx"=holdsRowIdx)
}


##' Prepares the holdings summary data frame.
##'
##' This is a description.
##'
##' @param holdings The holdings data frame
##' @param key The cash holdings key
##' @param col The cash column name
##' @return A list with holdings summary information.
##' @export
getHoldingsSummary <- function(holdings, col="Subtype", key="Cash") {

    ## Get the cash holdings:
    cashHoldings <-  holdings[safeCondition(holdings, col, key), ]

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

   ## Get the index of rows of order 1, i.e the totals:
    totalRowIdx <- which(holdings[, "order"] == "1")

    ## Get the index of rows of order !=1, i.e the subtotals:
    subtlRowIdx <- which(holdings[, "order"] != "1")

    ## Get rid of unwanted columns:
    holdings <- holdings[, !(colnames(holdings) == "order")]

    ## Get rid of unwanted columns:
    holdings <- holdings[, !(colnames(holdings) == "isHeader")]

    ## Get rid of unwanted columns:
    ## holdings <- holdings[, !(colnames(holdings) == "isFooter")]

    ## Remove rownames:
    rownames(holdings) <- NULL

    ## Beautify monetary amounts:
    holdings[, c("Value", "Exposure", "PnL (Unrl)")] <- apply(holdings[, c("Value", "Exposure", "PnL (Unrl)")], MARGIN=2, function(x) trimws(beautify(x)))

    ## Beautify percentages:
    holdings[, c("Value (%)", "Exp (%)", "PnL (%Inv)", "PnL (Contr)")] <- apply(holdings[, c("Value (%)", "Exp (%)", "PnL (%Inv)", "PnL (Contr)")], MARGIN=2, function(x) trimws(percentify(x)))

    ## Replace unwanted strings with empty:
    holdings[is.na(holdings)] <- ""
    holdings[holdings == "NA"] <- ""
    holdings[holdings == "NA %"] <- ""

    types <- holdings[, "Type"]

    ## Select the columns
    holdings <- holdings[, c("Name", "Value", "Value (%)", "Exposure", "Exp (%)", "PnL (Unrl)")]

    ## Done, return list:
    list("holdingsSummary"=holdings,
         "subtlIdx"=subtlRowIdx,
         "totalIdx"=totalRowIdx,
         "types"=types)
}


##' Gets the asset evolution for a portfolio from pconsolidation
##'
##' This is a description.
##'
##' @param portfolio The portfolio id.
##' @param date The date__lte.
##' @param years The number of years to look back.
##' @param session The rdecaf session,
##' @param ... Any additional parameters.
##' @return An data frame with NAV and AUM.
##' @export
getAssetEvolution <- function(portfolio, date, years="2", session, ...) {


    account <- ifelse(is.null(list(...)[["account"]]), NA, list(...)[["account"]])
    gteDate <- list(...)[["gteDate"]]
    ccy <- list(...)[["ccy"]]


    if (is.null(gteDate)) {
        gteDate <- dateOfPeriod(paste0("Y-", years))
    } else {
        gteDate <- gteDate
    }

    if (is.na(account)) {
        ## Define the params:
        params <- list("portfolio"=portfolio,
                       "account__isnull"="True",
                       "shareclass__isnull"="True",
                       "date__gte"=gteDate,
                       "date__lte"=date,
                       "page_size"=-1)
    } else {
        ## Define the params:
        params <- list("account"=account,
                       "account__isnull"="False",
                       "shareclass__isnull"="True",
                       "date__gte"=gteDate,
                       "date__lte"=date,
                       "page_size"=-1)
    }

    ## Get the pconsolidaton:
    pCons <- rdecaf::getResource("pconsolidations", params=params, session=session)

    ## Get all the valuation currencies in pCons:
    xccys <- unique(sapply(pCons, function(x) x[["ccy"]]))

    ## Get all the dates in pCons:
    xdates <- sapply(pCons, function(x) x[["date"]])

    ## If currency of container is not provided, assume one of the pCons currencies as reference:
    
    if (is.null(ccy)) {
        ccy <- xccys[[1]]
    }

    ## If there are multiple currencies, get fx rates for relevant period(s):
    if (!all(xccys == ccy)) {

        ## Construct the pairs:
        fxs <- paste0(xccys[xccys != ccy], ccy)

        ## Get the olhcs for pairs:
        obs <- lapply(fxs, function(fx) getOhlcObsForSymbol("session"=session, "symbol"=fx, "lte"=as.Date(max(xdates)), lookBack=NROW(xdates)))
        names(obs) <- fxs
    }

    ## Build the asset evolution data frame:
    as.data.frame(do.call(rbind, lapply(pCons, function(x) {

        ## nav:
        xnav <- x[["nav"]]

        ## aum:
        xaum <- x[["aum"]]

        ## date:
        xdate <- as.Date(x[["date"]])

        ## ccy:
        xccy <- x[["ccy"]]

        ## If pCons ccy equals container ccy, return values:
        if (xccy == ccy) {
            return(data.frame(nav=xnav, aum=xaum, date=xdate, stringsAsFactors = FALSE))
        }

        ## Get the corresponding fx rate for conversion:
        fx <- valueOfNearestDate(as.Date(xdate), obs[[paste0(xccy, ccy)]], 30, "date", "close")[["value"]]

        ## Convert and return:
        return(data.frame(nav=xnav*fx, aum=xaum*fx, date=xdate, stringsAsFactors = FALSE))

    })))

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
##' @param percentVals The row names which should be converted to percent.
##' @param roundN The number of digits to be rounded to for non percentage values. Default is 2.
##' @return A styled html table.
##' @export
beanbagPerformanceTable <- function(stats, percentVals=c("return", "stddev", "maxddown"), roundN=2) {

    ## If no data-frame, Return NULL:
    if (is.null(dim(stats))) {
        return(NULL)
    }

    stats <- as.data.frame(stats, stringsAsFactors=FALSE)

    ## Set all DTD values besides return to NA:
    stats[rownames(stats) != "return", "DTD"] <- NA

    ## Get the percentage indices:
    pctIdx <- apply(mgrep(rownames(stats), percentVals), MARGIN=1, function(x) any(x != "0"))

    ## Apply the rounding if provided:
    if (!is.null(roundN)) {
        stats[!pctIdx, ] <- apply(stats[!pctIdx, ], MARGIN=2, function(x) round(x, roundN))
    }

    ## Percentify rows:
    for (fld in percentVals) {
        if (any(rownames(stats) == fld)) {
            ## Percentify rows:
            stats[fld, ] <- trimws(percentify(stats[fld, ]))
        }
    }

    ## ## Round row(s):
    ## if (any(rownames(stats) == "sharpe")) {
    ##     ## Round row(s):
    ##     stats["sharpe", ] <- round(as.numeric(stats["sharpe",]), 2)
    ## }

    ## Get rid of unwanted characters:
    stats[stats == "0 %"] <- NA
    stats[stats == "NA %"] <- NA
    stats[stats == "NaN"] <- NA
    ## stats <- trimws(stats)

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
    ## cash <- rbind(cash, c("Cash Subtotal", rep(NA, NCOL(cash) -1)))

    ## Safely rbind cash and other holdings:
    nestedHoldings <- safeRbind(list(cash, nestedHoldings))

    ## Fill the isHeader value for cash title:
    nestedHoldings[nestedHoldings[,"Name"] == "Cash", "isHeader"] <- "TRUE"

    ## Fill the order value for cash title:
    nestedHoldings[nestedHoldings[,"Name"] == "Cash", "order"] <- "1"

    ## Fill the isFooter value for cash subtotal:
    ## nestedHoldings[nestedHoldings[,"Name"] == "Cash Subtotal", "isFooter"] <- "TRUE"

    ## Fill the order value for cash subtotal:
    ## nestedHoldings[nestedHoldings[,"Name"] == "Cash Subtotal", "order"] <- "1"

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
    orderIdx <- lapply(orderIdx, function(x) ifelse(is.na(x), FALSE, x))

    ## Name the list elements with order:
    names(orderIdx) <- na.omit(unique(holdings[,"order"]))

    idxPairs <- lapply(orderIdx, function(x) cbind(which(x), c(tail(which(x), -1) - 1, NROW(holdings))))

    ## idxPairs <- orderIdxx
    ## orderIdx <- lapply(orderIdx, function(x) diff(x))
    ## orderIdx[[1]] <- orderIdx[[1]] * -1

    ## idxPairs <- suppressWarnings(lapply(orderIdx, function(x) cbind(which(x== 1), which(x == -1))))

    ## idxPairs[[1]][NROW(idxPairs[[1]]), 2] <- NROW(holdings)


    ## Create index pairs representing the start and end of chunk:
    ## idxPairs <- lapply(orderIdx, function(x) cbind(x[seq(1, length(x), 2)],
    ##                                                x[seq(2, length(x), 2)]))

    ## Iterate over index pairs and run the getSummary for subtotals:
    allSubtotals <- lapply(idxPairs, function(x) do.call(rbind, apply(x, MARGIN=1, function(y) getSummary(holdings, y))))

    ## Iterate over index pairs and assign the values to correct subtotal row index:
    for (i in 1:length(idxPairs)){

        ## Get the footer row index:
        headerIdx <- idxPairs[[i]][, 1]

        ## Get the subtotal values:
        vals <- allSubtotals[[i]]

        ## Assign to footer row index:
        holdings[headerIdx, colnames(vals)] <- vals
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
