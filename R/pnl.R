##' A function to provide the pnl preemble.
##'
##' This is the description
##'
##' @param posBeg The beginning period positions from getFlatHoldings.
##' @param posEnd The ending period positions from getFlatHoldings.
##' @param resources The resources from rdecaf.
##' @param quants The quants data frame.
##' @param trades The trades data frame.
##' @param portfolio The portfolio id.
##' @param fxobs The fx rates.
##' @param ccy The portfolio currency.
##' @param session The rdecaf session.
##' @return A list with the pnl preemble.
##' @export
getPnlPreemble <- function(posBeg, posEnd, resources, quants, trades, portfolio, fxobs, ccy, session) {

    ## Filter the quants:
    quants <- quants[quants[, "ctype"] != 700, ]

    ## Filter the trades:
    trades <- trades[trades[, "ctype"] != 300, ]

    if (NROW(quants) == 0) {
        quants <- initDF(colnames(quants))
    }

    if (NROW(trades) == 0) {
        trades <- initDF(colnames(trades))
    }

    ## Append the trade resources for each quant:
    quants <- data.frame(quants, trades[match(quants[, "trade"], trades[, "id"]), c("resmain", "resaltn", "resundr")])

    ## Filter required columns:
    quants <- quants[, c("commitment", "ctype", "type", "quantity", "valamt", "resource", "symbol", "trade", "resmain", "resaltn", "resundr", "trade_reference")]

    ## Append the resource ctype to the quants:
    quants[, "resource_type"] <- resources[match(quants[, "resource"], resources[, "id"]), "ctype"]

    ## Rename the columns:
    colnames(quants) <- c("date", "ctype", "type", "qQty", "valamt", "qRes", "symbol", "trade", "tResmain", "tResaltn", "tResundr", "reference", "resCtype")

    ## Round quantities:
    quants[, "qQty"] <- round(as.numeric(quants[, "qQty"]), 4)

    ## Round quantities:
    quants[, "valamt"] <- round(as.numeric(quants[, "valamt"]), 4)

    ## Extend the start positions by closed ones:
    posBeg <- cleanNARowsCols(extendPositionByClosedQuants(quants, posBeg, resources))

    ## Extend the ned positions by closed ones:
    posEnd <- cleanNARowsCols(extendPositionByClosedQuants(quants, posEnd, resources))

    ## Remove the cash from beginning positions:
    posBeg <- posBeg[posBeg[, "Type"] != "Cash", ]
    posBeg <- posBeg[posBeg[, "Type"] != "FX Forward", ]
    posBeg <- posBeg[posBeg[, "Type"] != "FX Forward Contract", ]
    posBeg <- posBeg[posBeg[, "Type"] != "Time Deposit Contract", ]
    posBeg <- posBeg[posBeg[, "Type"] != "Loan Contract", ]
    posBeg <- posBeg[posBeg[, "Type"] != "Time Deposit", ]
    posBeg <- posBeg[posBeg[, "Type"] != "Loan", ]

    ## Remove the cash from end positions:
    posEnd <- posEnd[posEnd[, "Type"] != "Cash", ]
    posEnd <- posEnd[posEnd[, "Type"] != "FX Forward", ]
    posEnd <- posEnd[posEnd[, "Type"] != "FX Forward Contract", ]
    posEnd <- posEnd[posEnd[, "Type"] != "Time Deposit Contract", ]
    posEnd <- posEnd[posEnd[, "Type"] != "Loan Contract", ]
    posEnd <- posEnd[posEnd[, "Type"] != "Time Deposit", ]
    posEnd <- posEnd[posEnd[, "Type"] != "Loan", ]

    ## Extend the start positions by closed ones:
    posBeg <- cleanNARowsCols(posBeg)

    ## Extend the ned positions by closed ones:
    posEnd <- cleanNARowsCols(posEnd)

    colNames <- c("Name", "ID", "QTY", "Value", "Exposure", "Type", "Symbol")

    ##:
    if (NROW(posBeg) == 0) {
        posBeg <- initDF(colNames)
    }

    ##:
    if (NROW(posEnd) == 0) {
        posEnd <- initDF(colNames)
    }

    ## Append the resource quantity:
    posBeg[, "resqty"] <- resources[match(posBeg[, "ID"], resources[, "id"]), "quantity"]

    ## Append the resource quantity:
    posEnd[, "resqty"] <- resources[match(posEnd[, "ID"], resources[, "id"]), "quantity"]

    ## Append the resource quantity:
    quants[, "resqty"] <- resources[match(quants[, "qRes"], resources[, "id"]), "quantity"]

    ## Append the currency pair:
    quants[, "fx"] <- paste0(resources[match(quants[, "symbol"], resources[, "symbol"]), "ccymain"], ccy)

    matchedFX <- fxobs[match(quants[, "fx"], names(fxobs))]

    quants[, "fxrate"] <- do.call(c, lapply(1:NROW(quants), function(i) {

        matchedDates <- match(quants[i, "date"], matchedFX[[i]][, "date"])

        if (is.na(matchedDates)) {
            return(1)
        }

        matchedFX[[i]][matchedDates, "close"]
    }))

    quants[, "valamt(org)"] <- quants[, "valamt"]
    quants[, "valamt"] <- quants[, "valamt"] * quants[, "fxrate"]

    orderCols <- trades[match(quants[, "trade"], trades[, "id"]), c("pseudorder", "created")]
    quants <- quants[order(orderCols[, 1], orderCols[, 2], decreasing=FALSE), ]

    return(list("extendedQuants"=quants,
                "extendedPosBeg"=posBeg,
                "extendedPosEnd"=posEnd))

}


##' A wrapper function to get the pnl preemble.
##'
##' This is the description
##'
##' @param portfolio The portfolio id.
##' @param startDate The start date.
##' @param endDate The end date.
##' @param session The rdecaf session.
##' @return A list with the PnL preemble
##' @export
pnlPreembleWrapper <- function(portfolio, startDate, endDate, session) {

    ## Construct the portfolio params:
    pParams <- list("format"="csv", "page_size"=-1, "id"=portfolio)

    ## Get the portfolio:
    portfolio <- as.data.frame(getResource("portfolios", params=pParams, session=session))

    ## Get the stocks:
    stocks <- getStocks(portfolio, session, zero=1,endDate, c="portfolio")

    ## Get the resources:
    resources <- getResourcesByStock(stocks, session, getUnderlying=FALSE)

    ## Get the consolidations:
    consolidations <- lapply(c(startDate, endDate), function(dt) {
        getFlatHoldings(getResource("fundreport", params=list("fund"=portfolio[, "id"],
                                                              "ccy"=portfolio[, "rccy"],
                                                              "date"=dt,
                                                              "type"="commitment"),
                                    session=session)[["holdings"]])})

    ## The column selections for the consolidations:
    colSelect <- c("Name", "ID", "QTY", "Value", "Exposure", "Type", "Symbol")

    ## Get the starting period positions:
    posBeg <- consolidations[[1]][, colSelect]

    ## Get the ending period positions:
    posEnd <- consolidations[[2]][, colSelect]

    ## Construct the quants params:
    quantParams <- list("account__portfolio"=portfolio[, "id"], "format"="csv", "commitment__gte"=startDate, "page_size"=-1)

    ## Get the quents:
    quants <- as.data.frame(getResource("quants", params=quantParams, session=session))

    ## Get the trades for portfolio:
    trades <- getTradesFromContainerNames(portfolio[, "name"], session, "portfolios", gte=startDate)[["trades"]]

    ## Get the pnl preemble:
    getPnlPreemble(posBeg, posEnd, resources, quants, trades, portfolio, session)

}


##' A function to contextualize quants using the pnl preemble.
##'
##' This is the description
##'
##' @param pnlPreemble A list with the extended positions and quants.
##' @param dateBeg The beginning date.
##' @param dateEnd The ending date.
##' @return A list with the quants contexts for each position.
##' @export
contextualizeQuants <- function(pnlPreemble, dateBeg, dateEnd) {

    ## Get the extended beginning positions:
    extPosBeg <- pnlPreemble[["extendedPosBeg"]]
    noBeg <- NROW(extPosBeg) == 1 & all(is.na(extPosBeg[1, ]))

    ## Get the extended ending positions:
    extPosEnd <- pnlPreemble[["extendedPosEnd"]]
    noEnd <- NROW(extPosEnd) == 1 & all(is.na(extPosEnd[1, ]))

    if (noBeg & noEnd) {
        return(NULL)
    }

    ## Get the extended quants:
    quants <- pnlPreemble[["extendedQuants"]]

    ## For each position row,
    qu <- lapply(extPosBeg[, "ID"], function(id) quants[apply(quants[, c("tResmain", "tResaltn", "tResundr")], MARGIN=1, function(x) any(!is.na(match(x, id)))), ])

    isEmpty <- all(is.na(qu[[1]][, colnames(qu[[1]]) != "fx" & colnames(qu[[1]]) != "fxrate"])) & length(qu) == 1

    if (length(qu) == 0 | isEmpty) {
        return(NULL)
    }

    ## ##############################
    ## Special treatment for options.
    ## Override valamt with exposure.
    ## ##############################
    ## qu <- lapply(qu, function(x) {
    ##     isOption <- x[, "resCtype"] == "OPT"
    ##     any(isOption) || return(x)
    ##     x[isOption, "valamt"] <- abs(as.numeric(x[isOption, "resqty"]) * as.numeric(x[isOption, "qQty"])) * (as.numeric(x[isOption, "valamt"] / abs(as.numeric(x[isOption, "qQty"]))))
    ##     return(x)
    ## })
    ## ##############################
    ## ##############################

    ## Prepare the quant context list and return:
    retval <- lapply(1:NROW(qu), function(i) {

        ## Get the quants for current position
        quT <- qu[[i]]

        ## If no quants, mask a row:
        if (NROW(quT) == 0) {

            ## Initialise a data frame with NA's (the beginning position)
            temp <- as.data.frame(t(rep(NA, length(quT))), stringsAsFactors=FALSE)

            ## Remove colnames:
            colnames(temp) <- colnames(quT)

            ## Assign back to quT:
            quT <- temp

        } else {
            ## Else, append the beginning position:
            quT <- rbind(rep(NA, NCOL(quT)), quT)
        }

        ## The columns to be filled:
        tCols <- c("type", "date", "qQty", "valamt", "qRes", "symbol", "resqty", "reference")

        if (safeGrep(extPosBeg[i, "Type"], "Option") == "1") {
            isOption <- TRUE
        } else {
            isOption <- FALSE
        }


        ## Fill the columns with the beginning position values:
        quT[1, tCols] <- c("Start",
                           as.character(dateBeg),
                           as.numeric(extPosBeg[i, "QTY"]),
                           ifelse(isOption, as.numeric(safeNull(extPosBeg[i, "Value"])), as.numeric(safeNull(extPosBeg[i, "Exposure"]))),
                           ## as.numeric(extPosBeg[i, "Value"]),
                           ## as.numeric(safeNull(extPosBeg[i, "Exposure"])),
                           as.numeric(extPosBeg[i, "ID"]),
                           as.character(extPosBeg[i, "Symbol"]),
                           as.numeric(extPosBeg[i, "resqty"]),
                           NA)

        ## Append a row for the ending position:
        quT <- rbind(quT, rep(NA, NCOL(quT)))

        if (isOption) {
            posEndVal <- as.numeric(safeNull(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "Value"]))
        } else {
            posEndVal <- as.numeric(safeNull(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "Exposure"]))
        }

        ## Fill the column with the ending position values:
        quT[NROW(quT), tCols] <- c("End",
                                   as.character(dateEnd),
                                   as.numeric(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "QTY"]),
                                   ##as.numeric(safeNull(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "Exposure"])),
                                   posEndVal,
                                   as.numeric(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "ID"]),
                                   as.character(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "Symbol"]),
                                   as.character(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "resqty"]),
                                   NA)

        ## If position type is Share and any quant types have options, get rid of the options:
        if (extPosBeg[i, "Type"] == "Share" & any(safeCondition(quT, "resCtype", "OPT"))) {
            quT <- quT[!safeCondition(quT, "resCtype", "OPT"), ]
        }

        ## If position type is Option and any quant types have shares, get rid of the shares:
        if (extPosBeg[i, "Type"] == "Option Contract" & any(safeCondition(quT, "resCtype", "SHRE"))) {
            quT <- quT[!safeCondition(quT, "resCtype", "SHRE"), ]
        }

        ## Order by date:
        quT <- quT[order(quT[, "date"]), ]

        ## Get rid of cash/currency
        quT <- quT[apply(mgrep(quT[, "type"], c("Cash", "Currency", "Premium Payment", "I/O")), MARGIN=1, function(x) all(x == "0")), ]

        ## Which quants are actions?
        actions <- quT[, "type"] != "Start" & quT[, "type"] != "End"

        if (any(actions)) {
            #quT[actions, "valamt"] <- as.numeric(quT[actions, "valamt"]) * ifelse(quT[actions, "qQty"] > 0, -1, 1)
        }

        quT <- quT[, c("date", "symbol", "type", "qQty", "valamt", "resqty", "reference")]

        ## Append columns for pnl computation:
        quT[, c("PNL::isInc", "PNL::isCls", "PNL::isFee", "PNL::QTY", "PNL::tQTY", "PNL::InvAmt", "PNL::CumInv", "PNL::Income", "PNL::Real")] <- NA

        ## Done, return
        quT
    })

    ## Assign the names and return:
    ## retval <- lapply(1:length(retval), function(i) list("symbol"=extPosBeg[i, "Symbol"], "type"=extPosBeg[i, "Type"], "quants"=retval[[i]]))

    names(retval) <- extPosBeg[, "Symbol"]

    retval

}


##' Extends the positions data frame from getFlatholdings with opened/closed quants in between position dates.
##'
##' This is the description
##'
##' @param quants The quants data frame from rdecaf
##' @param positions The positions data frame from getFlatHoldings
##' @param resources The resources data frame from rdecaf
##' @return The extended positions data frame.
##' @export
extendPositionByClosedQuants <- function(quants, positions, resources) {

    ## Get the columns indices which have the RES key word:
    residCols <- apply(mgrep(toupper(colnames(quants)), c("RESMAIN", "RESALTN", "RESUNDR")), MARGIN=1, function(x) any(x != "0"))

    if (NROW(quants) == 0) {
        return(positions)
    }

    if (NROW(quants) == 1) {
        uniqueQuantres <- na.omit(unique(as.numeric(quants[, residCols])))
    } else {
        ## Get all unique resource id from txns (including the trade resources!!):
        uniqueQuantres <- try(na.omit(unique(do.call(c, apply(quants[, residCols], MARGIN=2, unique)))), silent=TRUE)
        if (class(uniqueQuantres) == "try-error") {
            uniqueQuantres <- try(na.omit(unique(do.call(c, lapply(quants[, residCols], MARGIN=2, unique)))), silent=TRUE)
        }
    }

    ## Match the unique quant resources with the position resource id:
    matchId <- match(uniqueQuantres, positions[, "ID"])

    ## Get the quant id's not present in positions:
    unmatchedID <- uniqueQuantres[is.na(matchId)]

    if (length(unmatchedID) == 0) {
        return(positions)
    }

    ## Initialise the append data.frame:
    appendD <- as.data.frame(matrix(NA, length(unmatchedID), NCOL(positions)))

    ## Assign the colnames:
    colnames(appendD) <- colnames(positions)

    ## Fill the names:
    appendD[, "Name"] <- ellipsify(toupper(resources[match(unmatchedID, resources[, "id"]), "name"]))

    ## Fill the resource id:
    appendD[, "ID"]  <- unmatchedID

    ## Fill the quantities:
    appendD[, "QTY"] <- 0

    ## File the values:
    appendD[, "Value"] <- 0

    ## Fill the types:
    appendD[, "Type"] <- resources[match(unmatchedID, resources[, "id"]), "type"]

    ## File the symbol:
    appendD[, "Symbol"] <- resources[match(unmatchedID, resources[, "id"]), "symbol"]

    ## Rbind unmatched quant id to positions as 0 positions and return:
    rbind(positions, appendD)
}


##' Computes the pnl using the quant context list.
##'
##' This is the description
##'
##' @param quantContext A list with the quant context.
##' @return A list with the quant context extended by pnl computation.
##' @export
computePnL <- function(quantContext) {

    ## If no context is provided, return NULL:
    if (length(quantContext) == 0) {
        return(NULL)
    }

    ## HEBELE:
    retval <- lapply(1:NROW(quantContext), function(row) {

        story <- quantContext[[row]]

        story <- story[story[, "type"] != "PnL", ]

        if (all(story[, "qQty"] == "0")) {
            return(list("PnLs"=NULL,
                        "Totals"=NULL))
        }

        if (all(story[story[, "type"] == "Start" | story[, "type"] == "End", "qQty"] == "0")) {
            return(list("PnLs"=NULL,
                        "Total"=NULL))
        }

        story[, "PNL::isInc"] <- !apply(mgrep(story[, "type"], c("Dividend", "Coupon", "PnL")), MARGIN=1, function(x) all(x == "0"))
        story[, "PNL::isFee"] <- !apply(mgrep(story[, "type"], c("Fee")), MARGIN=1, function(x) all(x == "0"))

        isStart <- story[, "type"] == "Start"

        if (all(!isStart)) {
            isStart[1] <- TRUE
        }

        isEnd   <- story[, "type"] == "End"
        isInc   <- story[, "PNL::isInc"]
        isFee   <- story[, "PNL::isFee"]

        story[, "PNL::QTY"] <- as.numeric(story[, "qQty"])
        story[isEnd, "PNL::QTY"] <- 0
        story[isInc | isFee, "PNL::QTY"] <- 0

        story[, "PNL::tQTY"] <- cumsum(as.numeric(!isInc) * as.numeric(!isFee * as.numeric(!isEnd)) * story[, "PNL::QTY"])

        story[, "PNL::isCls"] <-  c(0, diff(abs(story[, "PNL::tQTY"]))) < 0

        story[, "PNL::isNew"] <-  story[, "qQty"] == story[, "PNL::tQTY"] & story[, "type"] != "Start" & story[, "type"] != "End"

        isCls   <- story[, "PNL::isCls"]

        isInv <- !isEnd & !isFee & !isInc & !isCls

        if (story[isStart, "qQty"] == 0) {
            isStart[which(isInv)[2]] <- TRUE
            isStart[1] <- FALSE
            story[isStart, "valamt"] <- as.numeric(story[isStart, "valamt"]) * sign(as.numeric(story[isStart, "qQty"]))
            isInv[1] <- FALSE
        }

        story[isInv, "PNL::InvAmt"] <- as.numeric(story[isInv, "valamt"])
        story[isCls, "PNL::InvAmt"] <- as.numeric(story[isCls, "valamt"]) * sign(as.numeric(story[isCls, "qQty"]))

        story[is.na(story[, "PNL::InvAmt"]), "PNL::InvAmt"] <- 0

        ## ############################################################################
        ## ############################################################################
        for (row in 1:NROW(story)) {

            if (row == 1) {
                story[row, "PNL::CumInv"] <- story[row, "PNL::InvAmt"]
                next
            }

            if (story[row, "PNL::isNew"]) {
                story[row, "PNL::CumInv"] <- story[row, "PNL::InvAmt"]
                next
            }

            story[row, "PNL::CumInv"] <- story[row, "PNL::InvAmt"] + story[row-1, "PNL::CumInv"]

        }
        ## ############################################################################
        ## ############################################################################

        ## ## #################################
        ## story[, "PNL::CumInv"] <- cumsum(story[, "PNL::InvAmt"])
        ## story[, "PNL::CumInv"] <- cumsum(story[, "PNL::InvAmt"] * ifelse(story[, "PNL::tQTY"] == 0, story[, "PNL::"], 1))
        ## story[, "PNL::CumInv"] <- cumsum(story[, "PNL::InvAmt"] * ifelse(story[, "PNL::tQTY"] == 0, 0, 1))
        ## ## #################################

        story[, "PNL::Income"] <-  as.numeric(isInc) * as.numeric(story[, "qQty"])
        story[, "PNL::Fees"]   <-  as.numeric(isFee) * as.numeric(story[, "qQty"])

        fullAmt <- abs(as.numeric(story[isCls, "valamt"]) / as.numeric(story[isCls, "qQty"])) * -as.numeric(story[which(isCls) -1, "PNL::tQTY"])
        story[isCls, "PNL::Real"] <- (fullAmt + story[which(isCls) -1, "PNL::CumInv"]) * (as.numeric(story[isCls, "qQty"])) / as.numeric(story[which(isCls) -1, "PNL::tQTY"])

        story[is.na(story[, "PNL::Real"]), "PNL::Real"] <- 0

        totalRealised <- sum(story[, "PNL::Real"])

        if (any(isCls)) {
             story[which(isCls)[1]:NROW(story), "PNL::CumInv"] <- story[which(isCls)[1]:NROW(story), "PNL::CumInv"] + totalRealised
        }

        unrlsd <- as.numeric(story[NROW(story), "valamt"]) - as.numeric(story[NROW(story), "PNL::CumInv"])
        unrlsd <- ifelse(is.na(unrlsd), 0, unrlsd)

        realsd <- sum(as.numeric(story[, "PNL::Real"]))

        income <- sum(as.numeric(story[, "PNL::Income"]))

        tofees <- sum(as.numeric(story[, "PNL::Fees"]))

        total <- unrlsd + realsd + income + tofees

        story <- list("PnLs"=story,
                      "Totals"=data.frame("Unrealised"=unrlsd,
                                          "Realised"=realsd,
                                          "Income"=income,
                                          "Fees"=tofees,
                                          "Total"=total,
                                          "ROI"=total / story[tail(which(isInv), 1), "PNL::CumInv"]))

    })

    names(retval) <- names(quantContext)

    retval

}

##' Gets the pnl endpoint from decaf.
##'
##' This is the description
##'
##' @param params a list of additional parameters.
##' @param session the decaf session.
##' @return A list with the pnl ledger data.
##' @export
bare_get <- function (params=list(), session=NULL) {
  
  ## Get or create a session:
  if (is.null(session)) {
    session <- rdecaf::readSession()
  }
  
  ## Get the base url to start to build the endpoint URL:
  url <- httr::parse_url(session$location)
  
  ## Add paths ensuring that path seperator is not duplicated and a
  ## trailing path seperator is added:
  url$path <- c(sub("/$", "", gsub("//", "/", ...)), "/")
  
  ## Add params:
  url$query <- params
  
  ## Construct the endpoint URL:
  url <- httr::build_url(url)
  
  ## Get the resource:
  response <- httr::GET(url, httr::add_headers(Authorization=rdecaf:::.authorizationHeader(session)))
  
  ## Get the status:
  status <- response$status_code
  
  ## If the status code is not 200, raise an error:
  if (status != 200) {
    stop(sprintf("%s returned a status code of '%d'.\n\n  Details provided by the API are:\n\n%s", url, status, httr::content(response, as="text")))
  }
  
  ## Return (note that we are suppressing messages):
  suppressMessages(httr::content(response))
}

##' flattens the pnl endpoint list data from decaf.
##'
##' This is the description
##'
##' @param grp the returned list of ledgers from function above.
##' @return A list with the flattened pnl ledger data.
##' @export
flattenEvents <- function(grp) {
  
  nameAccount <- unlist(sapply(grp[["entries"]], function(x) safeNull(sapply(x$event$contents$holding$accounts, function(y) y$name)))) %>% as.vector() %>% unique()
  nameAccount <- paste(nameAccount[!is.na(nameAccount)],collapse="")
  aClass <- unlist(sapply(grp[["entries"]], function(x) safeNull(sapply(x$holding$tags$classification, function(y) y$name)))) %>% as.vector() %>% unique()
  aClass <- paste(aClass[!is.na(aClass)],collapse="|")
  
  ## Construct the ledger:
  ledger <- data.frame("id"=as.character(safeNull(grp[["artifact"]]$id)),
                       "type"=safeNull(grp[["artifact"]]$type),
                       "subtype"=safeNull(grp[["artifact"]]$subtype),
                       "symbol"=safeNull(grp[["artifact"]]$symbol),
                       "name"=safeNull(grp[["artifact"]]$name),
                       "currency"=safeNull(grp[["artifact"]]$currency),
                       "tag"=sapply(grp[["entries"]], function(x) x$event$tag),
                       "date"=sapply(grp[["entries"]], function(x) x$date),
                       "qty"=sapply(grp[["entries"]], function(x) x$quantity),
                       "valRef"=sapply(grp[["entries"]], function(x) x$value_ref_qty),
                       "valOrg"=sapply(grp[["entries"]], function(x) x$value_org_qty),
                       "valQty"=sapply(grp[["entries"]], function(x) safeNull(x$event$contents$holding$artifact$quantity)),
                       "pxcostRef"=sapply(grp[["entries"]], function(x) safeNull(x$cost_price_ref)),
                       "pxcostOrg"=sapply(grp[["entries"]], function(x) safeNull(x$cost_price_org)),
                       "pxlastRef"=sapply(grp[["entries"]], function(x) safeNull(x$last_price_ref)),
                       "pxlastOrg"=sapply(grp[["entries"]], function(x) safeNull(x$last_price_org)),
                       "quantType"=sapply(grp[["entries"]], function(x) safeTry(try(x$quant$action_quant_type, silent=TRUE))),
                       "nameAccount"=nameAccount,
                       "typeDetail"=sapply(grp[["entries"]], function(x) safeNull(x$event$contents$holding$artifact$type$name)),
                       "valAbsOrg"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$value$abs$org)),
                       "valAbsRef"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$value$abs$ref)),
                       "valNetOrg"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$value$net$org)),
                       "valNetRef"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$value$net$ref)),
                       "expAbsOrg"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$exposure$abs$org)),
                       "expAbsRef"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$exposure$abs$ref)),
                       "expNetOrg"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$exposure$net$org)),
                       "expNetRef"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$exposure$net$ref)),
                       "investment"=sapply(grp[["entries"]], function(x) safeNull(x$holding$investment$value$ref)),
                       "investmentOrg"=sapply(grp[["entries"]], function(x) safeNull(x$holding$investment$value$org)),
                       "px"=sapply(grp[["entries"]],function(x) safeNull(x$quant$action$main_price))
                       ,stringsAsFactors = FALSE) %>%
     dplyr::mutate(assetClass=aClass
                  ,pxFactor=1
                  ,exclude=FALSE
                  ) 
  
  
  if(NROW(ledger)==0) {
    return(NULL)
  }
  
  return(list("ledger"=ledger))
  
}


##' flattens the pnl endpoint list data from decaf.
##'
##' This is the description
##'
##' @param session the decaf session.
##' @return A flat df containing the session artifacts and associated asset class.
##' @export
getStocksAndAssets <- function(session) {

  ac <- getDBObject("assetclasses",session=session) %>% dplyr::select(id,contains("path")) 
  ac$assetClass <- apply(ac[,-1],1, function(x) paste(x[!is.na(x)],collapse="|")) 
  
  stocks <- data.frame() %>% 
    bind_rows(
      lapply(getResource("stocks",session=session), function(s) data.frame(artifact=s$artifact))
    ) %>% 
    unique()
  
  stnA <- getResourcesByStock(stocks=stocks, session=session) %>% 
    dplyr::select(id,quantity,country,sector,issuer,assetclass) %>% 
    dplyr::left_join(ac %>% select(-contains("path")) %>% mutate_if(is.numeric,as.character),by=c("assetclass"="id")) 
  stnA$assetclass <- NULL
  
  return(stnA)

}

##' Overwrites columns that have missing date from the pnl endpoint.
##'
##' This is the description
##'
##' @param ledge the list containing data frame being overwritten from flatten events.
##' @param res the resource data to feed quantity and asset class data columns.
##' @param joinC the column name used to join the data frames.
##' @return A flat df containing the ledgers with the correct asset class info.
##' @export
overwriteEvents <- function(ledge,res,joinC="id") {

  ledger <- ledge$ledger
  
  cnames <- colnames(ledger)
  cnames <- cnames[!cnames %in% c("assetClass","pxFactor","exclude")]
  
  ledger <- ledger[,cnames] %>%
    dplyr::left_join(res,by=joinC) %>%
    dplyr::mutate(pxFactor=if_else(is.na(quantity),1,as.numeric(quantity)))
    
    
  return(list("ledger"=ledger))

}


##' Contextualizes the flattened PnL data.
##'
##' This is the description
##'
##' @param flat the list containing clean data frame ledgers.
##' @param excludedTags the tags to exclude from context in the ledger df.
##' @return A list containing the contextualized ledger data frame and the original one.
##' @export
contextEvents <- function(flat,excludedTags=c("exclude","pnl")) {
  
  ledger <- flat$ledger %>% 
    dplyr::filter(!tag %in% excludedTags) %>% 
    dplyr::mutate(
      valRef=if_else(type!="BOND"&!is.na(valQty),valQty*if_else(tag %in% c("opening","closing"),qty,abs(qty))*pxlastRef*sign(if_else(tag %in% c("opening","closing"),1,valRef)),valRef) ##for future trade contracts expressed in PNL
      ,valOrg=if_else(type!="BOND"&!is.na(valQty),valQty*if_else(tag %in% c("opening","closing"),qty,abs(qty))*pxlastOrg*sign(if_else(tag %in% c("opening","closing"),1,valOrg)),valOrg)
      ,fees=if_else(!is.na(quantType)&str_detect(quantType,"fee"),1,0)
      ,income=if_else(fees==0&tag=="income",1,0)
    ) 
  
  
  
  isStart <- ledger[, "tag"] == "opening"
  
  isEnd   <- ledger[,"tag"] == "closing"
  isInc   <- ledger[,"tag"] == "income"
  isFee   <- ledger[,"fees"] == 1
  
  isMrg   <- if_else(is.na(ledger[,"quantType"]),FALSE,str_detect(unlist(ledger[,"quantType"]),"split"))
  
  ledger[, "tqty"] <-cumsum(as.numeric(!isInc) * as.numeric(!isFee) * as.numeric(!isEnd) * ledger[,"qty"])
  
  ##isCls <- c(0, diff(abs(ledger[, "tqty"]))) < 0 ##should work for both short and long positions
  isCls <- sign(ledger$qty)<0 & !isFee & !isInc & !isMrg &!isEnd
  
  ##isInv <- !isFee & !isInc & !isMrg &!isEnd
  isInv <- sign(ledger$qty)>=0 & !isFee & !isInc & !isMrg &!isEnd
  
  if (ledger[isStart, "qty"] == 0) {
    isStart[which(isInv)[2]] <- TRUE
    isStart[1] <- FALSE
    isInv[1] <- FALSE
  }
  
  ledger[,"isStart"] <- isStart
  
  ledger[,"isEnd"] <- isEnd
  
  ledger[,"isCls"] <- isCls & !isMrg
  
  
  if (ledger[isEnd, "qty"] == 0) {
    ledger <- ledger %>% 
      dplyr::mutate(isEnd=row_number()==max(if_else(isCls,row_number(),as.integer(0))))
    isEnd <- ledger$isEnd
  }
  
  ledger[,"isInv"] <- isInv & !isCls & !isEnd
  
  ledger$isInc <- isInc
  
  ledger$isFee <- isFee
  
  ledger <- ledger %>% 
    dplyr::mutate(sign=if_else(tag %in% c("opening","closing")|isEnd,1,sign(qty))) %>% 
    dplyr::mutate(
      income=cumsum(if_else(income==1,sign*valRef,0)),
      fees=cumsum(if_else(fees==1,sign*valRef,0)),
      realized=as.numeric(NA)
    )
  
  
  dubious <- FALSE
  
  if(!all(is.na(ledger$px))) {
    
    if(min(ledger$px,na.rm=TRUE)>0) {  
      
      pxmin <- min(ledger$px,na.rm=TRUE)
      pxmax <- max(ledger$px,na.rm=TRUE)
      
      dubious <- if_else(dubious==FALSE,pxmax/pxmin>2,dubious)
      
    }
    
  }
  
  realized <- 0
  
  
  if(any(ledger$isCls)) {
    
    test <- ledger[1,]$pxcostRef*ledger[NROW(ledger),]$qty/ledger[nrow(ledger),]$investment
    
    if(!is.na(test)&abs(1-test)<.01) {
      
      cutoff <- min(ledger$date)  
      
      if(any(ledger$tqty==0)) {
        cutoff <- max(ledger[which(ledger$tqty==0),]$date)
      }
      wac <- ledger %>% 
        dplyr::filter(date>=cutoff,isInv) %>% 
        dplyr::mutate(px=if_else(is.na(pxlastRef),as.numeric(px),as.numeric(pxlastRef))) %>% 
        dplyr::mutate(px=px*qty/sum(qty)) %>% 
        dplyr::summarise(px=sum(px)) %>% 
        .[[1]]
      
      ledger[nrow(ledger),]$investment <- wac*ledger[nrow(ledger),]$qty
    }
    
    
    
    realized <- ledger[nrow(ledger),]$investment +
      sum(ledger[ledger$isCls,]$valRef) - ##add back sales
      sum(ledger[ledger$isInv,]$valRef)  ##subtract buys
    
    
    if(is.na(realized)|(!is.na(ledger[nrow(ledger),]$investment)&ledger[nrow(ledger),]$investment==0)) {
      
      ledger <- ledger %>% 
        dplyr::mutate(across(c(valRef,valOrg), ~ if_else(round(px)==1&pxFactor==.01,as.numeric(.x/pxFactor),as.numeric(.x))))  %>% 
        dplyr::mutate(bought=if_else(isInv,abs(qty),0),sold=if_else(isCls,abs(qty),0))
      
      if(any(isMrg)) {
        
        ledger <- ledger %>% 
          dplyr::group_by(date) %>% 
          dplyr::mutate(splitFactor=if_else(!is.na(quantType)&str_detect(quantType,"split"),
                                            max(if_else(sign(qty)==-1,as.numeric(abs(qty)),1))/max(if_else(sign(qty)==1,as.numeric(abs(qty)),1)),
                                            as.numeric(NA)
          ),
          splitFactor=if_else(row_number()==1|is.na(splitFactor),as.numeric(splitFactor),1)
          ) %>% 
          ungroup() %>% 
          fill(splitFactor,.direction="down") %>% 
          dplyr::mutate(splitFactor=cumprod(if_else(is.na(splitFactor),1,splitFactor)),splitFactor=if_else(is.na(splitFactor),1,splitFactor)) %>%
          dplyr::mutate(
            bought=bought*splitFactor,
            sold=sold*splitFactor
          )  %>% 
          select(-splitFactor)
        
      }
      
      ledger <- ledger %>% 
        dplyr::mutate(bought=cumsum(bought),sold=cumsum(sold))
      
      dubious <- if_else(!dubious,ledger[nrow(ledger),]$qty==0&ledger[nrow(ledger),]$sold!=ledger[nrow(ledger),]$bought,dubious)
      
      sales <- abs(sum(ledger[ledger$isCls,]$valRef,na.rm=TRUE))
      
      cost <- abs(sum(ledger[ledger$isInv,]$valRef,na.rm=TRUE))
      
      fctr <- ledger[nrow(ledger),]$bought/ledger[nrow(ledger),]$sold
      
      realized <- (fctr*sales) - cost
      
      ledger[nrow(ledger),]$investment <- cost
      ledger[nrow(ledger),]$investmentOrg <- sum(ledger[ledger$isInv,]$valOrg,na.rm=TRUE)
      
      
      ledger$bought <- NULL
      ledger$sold <- NULL
      
    }
    
  }
  
  
  ledger[1,]$valNetRef <- if_else(is.na(ledger[1,]$valNetRef),as.numeric(ledger[nrow(ledger),]$investment),as.numeric(ledger[1,]$valNetRef))
  
  ledger[nrow(ledger),]$realized <- realized
  
  ledger$dubious <- dubious
  
  return(list("ledger"=ledger,"ledgerOG"=flat$ledger))
  
}

##' Computes the PnL from the contextualized data.
##'
##' This is the description
##'
##' @param context the list containing contextualized data frame ledgers.
##' @return A list containing the ledger artifact info, the clean ledger data, the summarised one liner data, a dubious flag, and the original ledger data.
##' @export
computeEvents <- function(context) {
  
  ledger <- context$ledger 
  
  netValue <- ledger[nrow(ledger),]$valNetRef
  
  investment <- ledger[1,]$valNetRef + if_else(abs(ledger[ledger$tag=="opening",]$qty)>0,sum(ledger[(ledger$isInv&!ledger$isStart)|ledger$isCls,]$valRef*ledger[(ledger$isInv&!ledger$isStart)|ledger$isCls,]$sign,na.rm=TRUE),0) 
  
  unrealized <- netValue - investment
  
  investment <- investment * if_else(ledger[1,]$type=="LOAN",-1,1)
  
  realized <- ledger[nrow(ledger),]$realized * if_else(ledger[1,]$type=="LOAN",-1,1)
  
  ledger[NROW(ledger),]$pxcostRef <- if_else(is.na(ledger[1,]$pxlastRef),as.numeric(ledger[NROW(ledger),]$pxcostRef),as.numeric(ledger[1,]$pxlastRef))
  
  if(!is.na(ledger[NROW(ledger),]$pxcostRef)&ledger[NROW(ledger),]$pxcostRef>0&NROW(ledger[ledger$isInv,])==1&ledger[1,]$type!="LOAN") {
    investment <- (ledger[NROW(ledger),]$pxcostRef*ledger[NROW(ledger),]$qty*
                     if_else(is.na(ledger[NROW(ledger),]$valQty),1,as.numeric(ledger[NROW(ledger),]$valQty)))
    
    unrealized <- ledger[nrow(ledger),]$valRef - investment
  }
  
  if(ledger[1,]$type=="FXFWD"&abs(ledger[NROW(ledger),]$qty)>0) {
    unrealized <- context$ledgerOG[NROW(context$ledgerOG),]$valRef
  }
  
  if(ledger[1,]$type=="DEPO"&abs(ledger[NROW(ledger),]$qty)>0&!is.na(context$ledgerOG[NROW(context$ledgerOG),]$investment)) {
    unrealized <- context$ledgerOG[NROW(context$ledgerOG),]$valRef-context$ledgerOG[NROW(context$ledgerOG),]$investment
  }
  
  income <- ledger[nrow(ledger),]$income
  
  fees <- ledger[nrow(ledger),]$fees
  
  if(ledger[1,]$type=="CCY")
  {
    income <- 0
    fees <- 0
    unrealized <- 0
    realized <- 0 
  }
  
  if(ledger[nrow(ledger),]$qty==0) {
    unrealized <- 0
  }
  
  
  totalReturn <- replace_na(realized,0) + replace_na(unrealized,0) + income + fees
  
  inv2 <- investment
  inv3 <- ledger[NROW(ledger),]$investmentOrg * if_else(ledger[1,]$type=="LOAN",-1,1)
  
dat <- data.frame(
    startingNAV=safeNull(ledger[1,]$valRef)
  , startingNAVOrg=safeNull(ledger[1,]$valOrg)
  , capGainsRealized=safeNull(realized)
  , capGainsUnrealized=safeNull(unrealized)
  , pnl=safeNull(totalReturn)
  , account=safeNull(ledger[NROW(ledger),]$nameAccount)
  , instrument=safeNull(ledger[NROW(ledger),]$typeDetail)
  , endingNAV=safeNull(ledger[NROW(ledger),]$valRef)
  , endingNAVOrg=safeNull(ledger[NROW(ledger),]$valOrg)
  , endingQty=safeNull(ledger[NROW(ledger),]$qty)
  , exposureAbs=safeNull(ledger[NROW(ledger),]$expAbsRef)
  , exposureNet=safeNull(ledger[NROW(ledger),]$expNetRef)
  , exposureNetOrg=safeNull(ledger[NROW(ledger),]$expNetOrg)
  , gav=safeNull(ledger[NROW(ledger),]$valAbsRef)
  , nav=safeNull(ledger[NROW(ledger),]$valNetRef)
  , instrumentID=safeNull(ledger[NROW(ledger),]$id)
  , symbol=safeNull(ledger[NROW(ledger),]$symbol)
  , type=safeNull(ledger[NROW(ledger),]$type)
  , name=safeNull(ledger[NROW(ledger),]$name)
  , assetClass=safeNull(ledger[NROW(ledger),]$assetClass)
  , country=safeNull(ledger[NROW(ledger),]$country)
  , sector=safeNull(ledger[NROW(ledger),]$sector)
  , issuer=safeNull(ledger[NROW(ledger),]$issuer)
  , investment=safeNull(inv2)##ledger[NROW(ledger),]$investment
  , investmentOrg=safeNull(inv3)##ledger[NROW(ledger),]$investmentOrg
  , income=safeNull(ledger[NROW(ledger),]$income)
  , fees=safeNull(ledger[NROW(ledger),]$fees)
  , dubious=safeNull(ledger[NROW(ledger),]$dubious)
  , exclude=FALSE
  ,stringsAsFactors = FALSE)
  
  
  dat <- dat %>% 
    dplyr::mutate(startingNAVSynth=startingNAV,
                  startingNAVOrgSynth=startingNAVOrg,
                  endingNAVSynth=endingNAV,
                  endingNAVOrgSynth=endingNAVOrg,
                  exposureNetSynth=exposureNet,
                  exposureNetOrgSynth=exposureNetOrg
    )
  
  
  if(dat$startingNAV==0|dat$endingNAV==0|is.na(dat$exposureNet)) {
    
    sVals <- ledger %>% dplyr::filter(abs(qty)>0) %>% slice(1:1) %>% dplyr::mutate(valRef=sign*valRef,valOrg=sign*valOrg)
    if(NROW(sVals)==0) {sVals <- data.frame(valRef=0,valOrg=0)}
    eVals <- ledger %>% dplyr::filter(abs(qty)>0) %>% slice(NROW(.):NROW(.)) %>% dplyr::mutate(valRef=sign*valRef,valOrg=sign*valOrg)
    if(NROW(eVals)==0) {eVals <- data.frame(valRef=0,valOrg=0)}
    
    dat <- dat %>% 
      dplyr::mutate(startingNAVSynth=if_else(startingNAV==0,as.numeric(sVals$valRef),as.numeric(startingNAV)),
                    startingNAVOrgSynth=if_else(startingNAVOrg==0,as.numeric(sVals$valOrg),as.numeric(startingNAVOrg)),
                    endingNAVSynth=if_else(endingNAV==0,as.numeric(eVals$valRef),as.numeric(endingNAV)),
                    endingNAVOrgSynth=if_else(endingNAVOrg==0,as.numeric(eVals$valOrg),as.numeric(endingNAVOrg)),
                    exposureNetSynth=if_else(is.na(exposureNet),as.numeric(sVals$valRef),as.numeric(exposureNet)),
                    exposureNetOrgSynth=if_else(is.na(exposureNetOrg),as.numeric(sVals$valOrg),as.numeric(exposureNetOrg))
      )
  }
  
  dat <- dat %>% 
    dplyr::mutate(##investment=if_else(type %in% c("FXFWD","FUT"),as.numeric(exposureNetSynth),as.numeric(startingNAVSynth)),
                  investmentOrg=case_when(
                    type %in% c("FXFWD","FUT") ~ as.numeric(exposureNetOrgSynth),
                    type == "LOAN" ~ as.numeric(startingNAVOrgSynth)*-1,
                    !type %in% c("FXFWD","FUT","LOAN") ~ as.numeric(startingNAVOrgSynth)
                    ),
                  return=pnl/investment 
    ) %>% 
    dplyr::mutate(fxImpact=(endingNAVSynth/endingNAVOrgSynth-startingNAVSynth/startingNAVOrgSynth)*investmentOrg,
                  fxImpact=if_else(is.nan(fxImpact),as.numeric(NA),fxImpact)
    ) %>% 
    select(-contains("Synth")
    )
  
  dat <-dat %>%
    dplyr::mutate(
      assetClass=if_else(is.na(assetClass),"Undefined",as.character(assetClass)),
      aClass1=str_split(assetClass,"\\|")[[1]][1],
      aClass2=str_split(assetClass,"\\|")[[1]][2],
      aClass3=str_split(assetClass,"\\|")[[1]][3],
      aClass4=str_split(assetClass,"\\|")[[1]][4],
      aClass5=str_split(assetClass,"\\|")[[1]][5],
      aClass6=str_split(assetClass,"\\|")[[1]][6]
    ) 
  
  dubious <- unique(ledger$dubious)
  
  ## Make list and return:
  list(  "artifact"=as.list(as.data.frame(ledger[nrow(ledger),1:6],stringsAsFactors=FALSE))
       , "ledger"=ledger
       , "summary"=dat
       , "dubious"=dubious
       , "ledgerOG"=context$ledgerOG
  )
  
}

##' Wrapper function to return consumable PnL data.
##'
##' This is the description
##'
##' @param pnlst the pnl list of data elements from the above.
##' @param res the df containing the asset class elements to overwrite faulty data.
##' @return A list containing the the granular ledger info from the above and a summarized one liner position data frame of pnl agg info.
##' @export
wrapEvents <- function(pnlst,res) {
  
  
  ## Flatten the endpoint data:
  aa <- lapply(pnlst, function(grp) flattenEvents(grp))
  ##aa <- aa[which(!sapply(aa, is.null))]
  aa[sapply(aa,is.null)] <- NULL
  
  ##overrides
  AA <- lapply(aa, function(grp) overwriteEvents(grp,res=res))
  
  ## Contextualize the endpoint data
  bb <- lapply(AA, function(grp) contextEvents(grp))

  ## Compute the PnL
  cc <- lapply(bb, function(grp) computeEvents(grp))
  
  
  ##aggregate summaries portfolio level
  dat <- data.frame() %>% 
    bind_rows(
      lapply(cc, function(x) {
        
        x$summary
      }
      )
    )   
  

  list(
    "granular"=cc,
    "aggSummary"=dat
  )
  
}


