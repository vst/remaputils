##' Provides the internal NAV/Share data frame
##'
##' This is the description
##'
##' @param portfolio The portfolio id.
##' @param ccy The portfolio currency.
##' @param date The report date.
##' @param session The rdecaf session
##' @return A data frame with the internal nav/share price.
##' @export
getInternalNAVShare <- function(portfolio, ccy, date, session) {

    ## Get the persisted consolidation:
    pConsolidation <- getResource("pconsolidations", params=list("portfolio"=portfolio, "page_size"=-1), session=session)

    if (length(pConsolidation) == 0) {
        return(data.frame("date"=seq(dateOfPeriod("Y-0"), date, 1), "price"=100, "ccy"=ccy))
    }

    ## Get the internal price and return:
    do.call(rbind, lapply(1:length(pConsolidation), function(i) {

        ## Get the current list element:
        pCons <- pConsolidation[[i]]

        ## If date of consolidation is passed report date, return NULL:
        if (pCons[["date"]] > date) {
            return(NULL)
        }

        ## If ccy of consolidation is not portfolio ccy, return NULL:
        if (pCons[["ccy"]] != ccy) {
            return(NULL)
        }

        ## If price is null , return NULL:
        if (is.null(pCons[["price"]])) {
            return(NULL)
        }

        ## Construct the data frame and return:
        return(data.frame("date"=pCons[["date"]], "price"=pCons[["price"]], "ccy"=pCons[["ccy"]]))
    }))

}
