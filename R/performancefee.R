##' A function to provide the performance fee preemble.
##'
##' This is the description
##'
##' @param perfMeta A list with the performance fee meta data.
##' @param session The rdecaf session.
##' @param useExternal Should the external valuation table be used?
##' @param useExternalOnlyForReset Use external only for reset?
##' @return A xts data frame with the performance fee preemble.
##' @export
performancePreemble <- function(perfMeta, session, useExternal=FALSE, useExternalOnlyForReset=TRUE) {

    getPJournalsByAccount <- function (id, session) {
        params <- list(account= id, format = "csv", page_size = -1)
        as.data.frame(getResource("quants", params = params, session = session))
    }

    ## Get the partial journal entries of the performance fee accounts:
    pjournals <- getPJournalsByAccount(perfMeta[["accid"]], session)

    if (NROW(pjournals) > 0) {
        pjournals <- pjournals[pjournals[, "ctype"] == 700, ]
    }

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
    if (useExternal | useExternalOnlyForReset) {
        extvaluation <- getExternalValuation(perfMeta[["portid"]], session)
        if (NROW(extvaluation) != 0) {
            extvaluation <- extvaluation[extvaluation[, "shareclass"] == perfMeta[["scid"]], ]
        }
    }

    ## Get the investments for shareclass:
    investments <- getInvestments(perfMeta[["portid"]], session)
    investments <- investments[investments[, "shrcls"] == perfMeta[["scid"]], ]

    if (isNAorEmpty(perfMeta[["benchmark"]]) | is.null(perfMeta[["benchmark"]])) {
        benchmark <- xts::as.xts(rep(100, 2001), order.by=as.Date(seq(Sys.Date() - 2000, Sys.Date(), 1)))
    } else {
        ## Get the benchmark series:
        benchmark <- getOhlcObsForSymbol(session, perfMeta[["benchmark"]], lookBack=2000)
        benchmark <- xts::as.xts(benchmark[, "close"], order.by=as.Date(benchmark[, "date"]))
    }

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
                                   "aggColumn"="quantity",
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
    scSubs <- cbind(scSubs, "noshares"=scSubs[, "qtymain"] / scSubs[, "pxnavs"])

    ## Get the redemptions for shareclass:
    scRedm <- redemptions[redemptions[, "shrcls"] == as.numeric(scTsXts[1, "sc"]), ]
    scRedm <- cbind(scRedm, "noshares"=scRedm[, "qtymain"] / scRedm[, "pxnavs"])

    ## Aggregate (sum) subscriptions by date and xtsify:
    scSubsShrs <- aggregateAndXTSify(scSubs, "aggColumn"="noshares", "aggBy"="commitment","fallbackDate"=zoo::index(scTsXts)[1])
    scSubs <- aggregateAndXTSify(scSubs, "aggColumn"="qtymain", "aggBy"="commitment","fallbackDate"=zoo::index(scTsXts)[1])

    ## Aggregate (sum) redemptions by date and xtsify:
    scRedmShrs <- aggregateAndXTSify(scRedm, "aggColumn"="noshares", "aggBy"="commitment","fallbackDate"=zoo::index(scTsXts)[1])
    scRedm <- aggregateAndXTSify(scRedm, "aggColumn"="qtymain", "aggBy"="commitment","fallbackDate"=zoo::index(scTsXts)[1])

    ## XTSify subscriptions and append:
    scTsXts <- cbind(scTsXts, "subs"=scSubs, "subsShrs"=scSubsShrs)

    ## XTSify redemptions and append:
    scTsXts <- cbind(scTsXts, "redm"=scRedm, "redmShrs"=scRedmShrs)

    ## Trim the time series:
    scTsXts <- scTsXts[which(!is.na(scTsXts[, "subs"]))[1]:NROW(scTsXts), ]

    ## XTSify benchmark and append:
    scTsXts <- cbind(scTsXts, "benchmark"=benchmark[zoo::index(benchmark) >= zoo::index(scTsXts)[1], ])

    ## Get last day of period:
    lastDay <- getLastDayOfPeriod(zoo::index(scTsXts), "Friday", perfMeta[["resetFreq"]])

    ## Append reset day (i.e last day of period) to the xts data frame:
    scTsXts <- cbind(scTsXts, "reset"=lastDay[, "lastDayOfPeriod"])

    if (useExternal) {
        ## If no external valuation, mask:
        if (NROW(extvaluation) == 0) {
            scextval <- data.frame("nav"=NA, "date"=zoo::index(scTsXts)[1])
        } else {
            ## Get the external valuation for shareclass:
            scextval <- extvaluation[extvaluation[, "shareclass"] == as.numeric(scTsXts[1, "sc"]), ]
        }
    } else if (useExternalOnlyForReset) {
        ## If no external valuation, mask:
        if (NROW(extvaluation) == 0) {
            scextval <- data.frame("nav"=NA, "date"=zoo::index(scTsXts)[1])
        } else {
            ## Get the external valuation for shareclass:
            resetDates <- zoo::index(scTsXts)[scTsXts[, "reset"] == 1]
            extvaluation <- extvaluation[extvaluation[, "shareclass"] == as.numeric(scTsXts[1, "sc"]), ]
            scextval <- extvaluation[na.omit(match(resetDates, extvaluation[, "date"])), ]
            if (NROW(scextval) == 0) {
                scextval <- data.frame("nav"=NA, "date"=zoo::index(scTsXts)[1])
            }
        }
    } else {
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
    ## Set NA subscriptions to 0:
    scTsXts[is.na(scTsXts[, "subsShrs"]), "subsShrs"] <- 0
    ## Set NA redemptions to 0:
    scTsXts[is.na(scTsXts[, "redmShrs"]), "redmShrs"] <- 0

    scTsXts[scTsXts[, "navE"] == 0, "navE"] <- NA

    ## Get stichted nav and append:
    scTsXts <- cbind(scTsXts, "navS"=as.numeric(ifelse(is.na(scTsXts[, "navE"]), scTsXts[, "nav"], scTsXts[, "navE"])))

    ## Get last day of period:
    lastDay <- getLastDayOfPeriod(zoo::index(scTsXts), "Friday", perfMeta[["resetFreq"]])

    ## Append reset day (i.e last day of period) to the xts data frame:
    scTsXts <- cbind(scTsXts, "reset"=lastDay[, "lastDayOfPeriod"])

    ## Append the performance fee account id:
    scTsXts <- cbind(scTsXts, "pacc"=perfMeta[["accid"]])

    ## Append the external nav with forward filled NA's:
    scTsXts <- cbind(scTsXts, "navES"=as.numeric(zoo::na.locf(scTsXts[, "navE"])))

    ## Get the outstanding shares?
    scTsXts <- cbind(scTsXts, "outstanding"=as.numeric(cumsum(scTsXts[, "subs"] - abs(scTsXts[, "redm"]))))

    ## Overwrite reset days if necessary:
    if (!is.null(perfMeta[["resetOverwrite"]])) {
        scTsXts[mmatch(as.Date(names(perfMeta[["resetOverwrite"]])),  data.frame(zoo::index(scTsXts))), "reset"] <- 0
        scTsXts[mmatch(as.Date(unlist(perfMeta[["resetOverwrite"]])), data.frame(zoo::index(scTsXts))), "reset"] <- 1
    }

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
##' @param isAuto Flag to set the record to AUTO. Default is FALSE.
##' @return NULL
##' @export
performanceFeeAccountant <- function(x, session, aTypeName="Performance Fees", isAuto=FALSE) {

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
                          "is_auto"=isAuto,
                          "shrcls"=as.numeric(x[, "sc"])[1],
                          "notes"="AUTO-PJE:Performance",
                          "pxmain"=1,
                          "guid"=NA)

    ## Construct and assign the guid:
    actions[, "guid"] <- apply(actions, MARGIN=1, function(x) digest::digest(paste0(paste0(x[c("commitment", "resmain", "accmain", "ctype")], collapse=""), "AUTO")))

    ## Get rid of rownames:
    rownames(actions) <- NULL

    ## The payload:
    payload <- toJSON(list(actions=actions), auto_unbox = TRUE, na = "null", digits = 10)

    ## Inbulk with sync:
    response <- postResource("imports/inbulk", payload=payload, params=list(sync="True"), session = session)
}
