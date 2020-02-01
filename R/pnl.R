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
    quants <- quants[, c("commitment", "ctype", "type", "quantity", "valamt", "resource", "symbol", "trade", "resmain", "resaltn", "resundr")]

    ## Append the resource ctype to the quants:
    quants[, "resource_type"] <- resources[match(quants[, "resource"], resources[, "id"]), "ctype"]

    ## Rename the columns:
    colnames(quants) <- c("date", "ctype", "type", "qQty", "valamt", "qRes", "symbol", "trade", "tResmain", "tResaltn", "tResundr", "resCtype")

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
    posBeg <- posBeg[posBeg[, "Type"] != "Time Deposit", ]

    ## Remove the cash from end positions:
    posEnd <- posEnd[posEnd[, "Type"] != "Cash", ]
    posEnd <- posEnd[posEnd[, "Type"] != "FX Forward", ]
    posEnd <- posEnd[posEnd[, "Type"] != "FX Forward Contract", ]
    posEnd <- posEnd[posEnd[, "Type"] != "Time Deposit", ]

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
    colSelect <- c("Name", "ID", "QTY", "Value", "Type", "Symbol")

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

    ## Get the extended ending positions:
    extPosEnd <- pnlPreemble[["extendedPosEnd"]]

    ## Get the extended quants:
    quants <- pnlPreemble[["extendedQuants"]]

    ## For each position row,
    qu <- lapply(extPosBeg[, "ID"], function(id) quants[apply(quants[, c("tResmain", "tResaltn", "tResundr")], MARGIN=1, function(x) any(!is.na(match(x, id)))), ])

    if (length(qu) == 0) {
        return(NULL)
    }

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
        tCols <- c("type", "date", "qQty", "valamt", "qRes", "symbol", "resqty")

        ## Fill the columns with the beginning position values:
        quT[1, tCols] <- c("Start",
                           as.character(dateBeg),
                           as.numeric(extPosBeg[i, "QTY"]),
                           as.numeric(extPosBeg[i, "Value"]),
                           as.numeric(extPosBeg[i, "ID"]),
                           as.character(extPosBeg[i, "Symbol"]),
                           as.numeric(extPosBeg[i, "resqty"]))

        ## Append a row for the ending position:
        quT <- rbind(quT, rep(NA, NCOL(quT)))

        ## Fill the column with the ending position values:
        quT[NROW(quT), tCols] <- c("End",
                                   as.character(dateEnd),
                                   as.numeric(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "QTY"]),
                                   as.numeric(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "Value"]),
                                   as.numeric(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "ID"]),
                                   as.character(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "Symbol"]),
                                   as.character(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "resqty"]))


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

        quT <- quT[, c("date", "symbol", "type", "qQty", "valamt", "resqty")]

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

        if (all(story[, "qQty"] == "0")) {
            return(list("PnLs"=NULL,
                        "Totals"=NULL))
        }

        story[, "PNL::isInc"] <- !apply(mgrep(story[, "type"], c("Dividend", "Coupon")), MARGIN=1, function(x) all(x == "0"))
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

        story[, "PNL::CumInv"] <- cumsum(story[, "PNL::InvAmt"])

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
                                          "ROI"=total / story[tail(which(isInv),1), "PNL::CumInv"]))

    })

    names(retval) <- names(quantContext)

    retval

}
