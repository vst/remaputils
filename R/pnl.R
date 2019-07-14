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
##' @param session The rdecaf session.
##' @return A list with the pnl preemble.
##' @export
getPnlPreemble <- function(posBeg, posEnd, resources, quants, trades, portfolio, session) {

    ## Filter the quants:
    quants <- quants[quants[, "ctype"] != 700, ]

    ## Filter the trades:
    trades <- trades[trades[, "ctype"] != 300, ]

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
    posBeg <- extendPositionByClosedQuants(quants, posBeg, resources)

    ## Extend the ned positions by closed ones:
    posEnd <- extendPositionByClosedQuants(quants, posEnd, resources)

    ## Remove the cash from beginning positions:
    posBeg <- posBeg[posBeg[, "Type"] != "Cash", ]

    ## Remove the cash from end positions:
    posEnd <- posEnd[posEnd[, "Type"] != "Cash", ]

    ## Append the resource quantity:
    posBeg[, "resqty"] <- resources[match(posBeg[, "ID"], resources[, "id"]), "quantity"]

    ## Append the resource quantity:
    posEnd[, "resqty"] <- resources[match(posEnd[, "ID"], resources[, "id"]), "quantity"]

    ## Append the resource quantity:
    quants[, "resqty"] <- resources[match(quants[, "qRes"], resources[, "id"]), "quantity"]

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


        quT <- quT[, c("date", "symbol", "type", "qQty", "valamt", "resqty")]

        ## Append columns for pnl computation:
        quT[, c("PNL::isInc", "PNL::isCls", "PNL::QTY", "PNL::tQTY", "PNL::InvAmt", "PNL::CumInv", "PNL::Income", "PNL::Real")] <- NA

        ## Done, return
        quT
    })


    ## Assign the names:
    names(retval) <- extPosBeg[, "Symbol"]

    ## Done, return:
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

    ## Get all unique resource id from txns (including the trade resources!!):
    uniqueQuantres <- na.omit(unique(do.call(c, apply(quants[, residCols], MARGIN=2, unique))))

    ## Match the unique quant resources with the position resource id:
    matchId <- match(uniqueQuantres, positions[, "ID"])

    ## Get the quant id's not present in positions:
    unmatchedID <- uniqueQuantres[is.na(matchId)]

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

    lapply(1:NROW(quantContext), function(row) {

        story <- quantContext[[row]]
        story <- story[story[, "type"] != "Cash", ]
        story <- story[story[, "type"] != "Currency", ]

        story[, "PNL::isInc"] <- !apply(mgrep(story[, "type"], c("Dividend", "Coupon")), MARGIN=1, function(x) all(x == "0"))
        story[, "PNL::QTY"] <- as.numeric(story[, "qQty"])
        story[, "PNL::tQTY"] <- cumsum(as.numeric(!story[, "PNL::isInc"]) * story[, "PNL::QTY"])
        story[, "PNL::isCls"] <-  c(0, diff(abs(story[, "PNL::tQTY"]))) < 0
        story[, "PNL::InvAmt"] <-  as.numeric(!story[, "PNL::isInc"]) * (as.numeric(story[, "valamt"]) * ifelse(story[, "qQty"] < 0, -1, 1))
        story[, "PNL::Income"] <-  as.numeric(story[, "PNL::isInc"]) * as.numeric(story[, "PNL::QTY"])
        story[is.na(story[, "PNL::Income"]), "PNL::Income"] <-  0

        for (i in 1:NROW(story)) {

            if (i == 1) {
                story[i, "PNL::CumInv"] <- story[i, "PNL::InvAmt"]
                story[i, "PNL::Real"] <- 0
                next
            }

            realPart1 <- -(story[i, "PNL::InvAmt"] * (story[i-1, "PNL::tQTY"] / story[i, "PNL::QTY"]) - story[i-1, "PNL::CumInv"])
            realPart2 <- story[i, "PNL::QTY"] / story[i-1, "PNL::tQTY"]
            story[i, "PNL::Real"] <- realPart1 * realPart2 * as.numeric(story[i, "PNL::isCls"])

            story[i, "PNL::CumInv"] <- story[i-1, "PNL::CumInv"] + (story[i, "PNL::InvAmt"] * as.numeric(!story[i, "PNL::isInc"])) + story[i, "PNL::Real"]

        }

        story[is.na(story[, "PNL::CumInv"]), "PNL::CumInv"] <- 0
        story[is.na(story[, "PNL::Real"]), "PNL::Real"] <- 0

        story[, "PNL::Unrls"] <- NA
        story[, "PNL::TIncome"] <- NA
        story[, "PNL::TReal"] <- NA
        story[, "PNL::TPnL"] <- NA
        story[, "PNL::POSROI"] <- NA
        story[, "PNL::CONTR"] <- NA

        story[NROW(story), "PNL::Unrls"] <- story[NROW(story), "PNL::InvAmt"] - story[NROW(story)-1, "PNL::CumInv"]
        story[is.na(story[, "PNL::Unrls"]), "PNL::Unrls"] <-  0
        story[NROW(story), "PNL::TIncome"] <- sum(story[, "PNL::Income"])
        story[NROW(story), "PNL::TReal"] <- sum(story[, "PNL::Real"])

        story[NROW(story), "PNL::TPnL"] <- sum(story[NROW(story), c("PNL::Unrls", "PNL::TIncome", "PNL::TReal")])

        ## story[NROW(story), "PNL::POSROI"] <- story[NROW(story), "PNL::TPnL"] / tail(abs(story[which(story[-NROW(story), "PNL::CumInv"] != 0), "PNL::CumInv"]), 1)

        story

    })
}
