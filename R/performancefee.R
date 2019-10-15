##' A function to provide the performance fee preemble.
##'
##' This is the description
##'
##' @param perfMeta A list with the performance fee meta data.
##' @param session The rdecaf session.
##' @return A xts data frame with the performance fee preemble.
##' @export
performancePreemble <- function(perfMeta, session) {

    ## Get the partial journal entries of the performance fee accounts:
    pjournals <- getPJournalsByAccount(perfMeta[["accid"]], session)

    ## Get portfolio report:
    pconsol <- getPconsolidation(perfMeta[["portid"]], session)

    ## Get the shareclass nav timeseries:
    scTs <- do.call(rbind, lapply(pconsol, function(x) data.frame("date"=x$date,
                                                                  "sclass"=x$shareclass,
                                                                  "nav"=x$nav,
                                                                  "shares"=x$shares)))

    ## Get the nav time series for shareclass:
    scTs <- scTs[scTs[, "sclass"] == perfMeta[["scid"]], ]

    ## Get the external valuation for shareclass:
    extvaluation <- getExternalValuation(perfMeta[["portid"]], session)
    extvaluation <- extvaluation[extvaluation[, "shareclass"] == perfMeta[["scid"]], ]

    ## Get the investments for shareclass:
    investments <- getInvestments(perfMeta[["portid"]], session)
    investments <- investments[investments[, "shrcls"] == perfMeta[["scid"]], ]

    ## Get the benchmark series:
    benchmark <- getOhlcObsForSymbol(session, perfMeta[["benchmark"]], lookBack=2000)
    benchmark <- xts::as.xts(benchmark[, "close"], order.by=as.Date(benchmark[, "date"]))

    ## Get the Subscriptions:
    subscriptions <- investments[investments[, "qtymain"] > 0, c("commitment", "qtymain", "pxnavs", "shrcls")]

    ## Get the Redemptions:
    redemptions <- investments[investments[, "qtymain"] < 0, c("commitment", "qtymain", "pxnavs", "shrcls")]

    ## XTSify:
    scTsXts <- xts::as.xts(as.data.frame(scTs[, c("nav", "sclass", "shares")]), order.by=as.Date(scTs[, "date"]))

    ## Add the column names:
    colnames(scTsXts) <- c("nnav", "sc", "shares")

    ## Aggregate (sum) journal values by date and xtsify:
    pJournal <- aggregateAndXTSify("x"=pjournals,
                                   "aggColumn"="qtymain",
                                   "aggBy"="commitment",
                                   "fallbackDate"=zoo::index(scTsXts)[1])

    ## Append the cumulative partial journal entries:
    scTsXts <- cbind(scTsXts, "pjr"=cumsum(pJournal))

    ## Forward fill partial journal entries:
    scTsXts[, "pjr"] <- zoo::na.locf(scTsXts[, "pjr"])

    ## Deduct the partial journal entries from the nnav to obtain nav:
    scTsXts <- cbind(scTsXts, "nav"=as.numeric(scTsXts[, "nnav"] - scTsXts[, "pjr"]))

    ## Get the subscriptions for shareclass:
    scSubs <- subscriptions[subscriptions[, "shrcls"] == as.numeric(scTsXts[1, "sc"]), ]

    ## Get the redemptions for shareclass:
    scRedm <- redemptions[redemptions[, "shrcls"] == as.numeric(scTsXts[1, "sc"]), ]

    ## Aggregate (sum) subscriptions by date and xtsify:
    scSubs <- aggregateAndXTSify(scSubs, "aggColumn"="qtymain", "aggBy"="commitment","fallbackDate"=zoo::index(scTsXts)[1])

    ## Aggregate (sum) redemptions by date and xtsify:
    scRedm <- aggregateAndXTSify(scRedm, "aggColumn"="qtymain", "aggBy"="commitment","fallbackDate"=zoo::index(scTsXts)[1])

    ## XTSify subscriptions and append:
    scTsXts <- cbind(scTsXts, "subs"=scSubs)

    ## XTSify redemptions and append:
    scTsXts <- cbind(scTsXts, "redm"=scRedm)

    ## Trim the time series:
    scTsXts <- scTsXts[which(!is.na(scTsXts[, "subs"]))[1]:NROW(scTsXts), ]

    ## XTSify benchmark and append:
    scTsXts <- cbind(scTsXts, "benchmark"=benchmark[zoo::index(benchmark) >= zoo::index(scTsXts)[1], ])

    ## If no external valuation, mask:
    if (NROW(extvaluation) == 0) {
        scextval <- data.frame("nav"=NA, "date"=zoo::index(scTsXts)[1])
    } else {
        ## Get the external valuation for shareclass:
        scextval <- extvaluation[extvaluation[, "shareclass"] == as.numeric(scTsXts[1, "sc"]), ]
    }

    ## If no external valuation, mask:
    if (NROW(scextval) == 0) {
        scextval <- data.frame("nav"=NA, "date"=zoo::index(scTsXts)[1])
    }

    ## XTSify external valuation and append:
    scTsXts <- cbind(scTsXts, "nnavE"=xts::as.xts(scextval[, "nav"], order.by=as.Date(scextval[, "date"])))

    ## Add the column names:
    colnames(scTsXts)[NCOL(scTsXts)] <- "nnavE"

    ## Compute the external nav and append:
    ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    scTsXts <- cbind(scTsXts, "navE"=as.numeric(scTsXts[, "nnavE"] - scTsXts[, "pjr"]))
    ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ## Forward-fill benchmark:
    scTsXts[, "benchmark"] <- zoo::na.locf(scTsXts[, "benchmark"])

    ## Backward-fill benchmark:
    scTsXts[, "benchmark"] <- zoo::na.locf(scTsXts[, "benchmark"], fromLast=TRUE)

    ## Set NA subscriptions to 0:
    scTsXts[is.na(scTsXts[, "subs"]), "subs"] <- 0

    ## Set NA redemptions to 0:
    scTsXts[is.na(scTsXts[, "redm"]), "redm"] <- 0

    ## Get stichted nav and append:
    scTsXts <- cbind(scTsXts, "navS"=as.numeric(ifelse(is.na(scTsXts[, "navE"]), scTsXts[, "nav"], scTsXts[, "navE"])))

    ## Get last day of period:
    lastDay <- getLastDayOfPeriod(zoo::index(scTsXts), "Friday", "quarterly")

    ## Append reset day (i.e last day of period) to the xts data frame:
    scTsXts <- cbind(scTsXts, "reset"=lastDay[, "lastDayOfPeriod"])

    ## Append the performance fee account id:
    scTsXts <- cbind(scTsXts, "pacc"=perfMeta[["accid"]])

    ## Append the external nav with forward filled NA's:
    scTsXts <- cbind(scTsXts, "navES"=as.numeric(zoo::na.locf(scTsXts[, "navE"])))

    ## Get the outstanding shares?
    scTsXts <- cbind(scTsXts, "outstanding"=as.numeric(cumsum(scTsXts[, "subs"] - abs(scTsXts[, "redm"]))))

    ## Done, return:
    return(scTsXts)

}


##' This function prepares the payload and syncs the PJE's to the performance fee account.
##'
##' This is the description
##'
##' @param x The data frame with following columns: "perfFeeTotal", "resmain", "pacc", "sc".
##' @param session The rdecaf session.
##' @param aTypeName The name of the analytical type.
##' @return NULL
##' @export
performanceFeeAccountant <- function(x, session, aTypeName="Performance Fees") {

    ## Compute the qtymain (i.e difference of total liabilities)
    qtymain <- as.numeric(diff(x[, "perfFeeTotal"]))

    ## Set NA's to zero:
    qtymain[is.na(qtymain)] <- 0

    ## Construct the actions data frame:
    actions <- data.frame("qtymain"=round(qtymain, 2),
                          "commitment"=zoo::index(x),
                          "resmain"=as.numeric(x[, "resmain"]),
                          "accmain"=as.numeric(x[, "pacc"]),
                          "ctype"=300,
                          "atype"=paste0("dcf:analyticaltype?name=", aTypeName),
                          "is_auto"=TRUE,
                          "shrcls"=as.numeric(x[, "sc"])[1],
                          "notes"="AUTO-PJE:Performance",
                          "pxmain"=1,
                          "guid"=NA)

    ## Construct and assign the guid:
    actions[, "guid"] <- apply(actions, MARGIN=1, function(x) digest(paste0(paste0(x[c("commitment", "resmain", "accmain", "ctype")], collapse=""), "AUTO")))

    ## Get rid of rownames:
    rownames(actions) <- NULL

    ## The payload:
    payload <- toJSON(list(actions=actions), auto_unbox = TRUE, na = "null", digits = 10)

    ## Inbulk with sync:
    response <- postResource("imports/inbulk", payload=payload, params=list(sync="True"), session = session)
}
