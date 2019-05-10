##' A function to flatten M005 files.
##'
##' This is the description
##'
##' @param data The sarasin M005 data
##' @return Returns a flat file
##' @export
sarasinFlattenM005 <- function(data) {
    ## Flatten and return:
    do.call(rbind, lapply(1:length(data), function(i) {
        data.frame("portfolio"=     safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["ptfl"]]), silent=TRUE)),
                   "account"=       safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["ctlrAcct"]]), silent=TRUE)),
                   "qtymain"=       safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["posnQty"]]), silent=TRUE)),
                   "posamnt"=       safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["posAmt"]][["text"]]), silent=TRUE)),
                   "accrual"=       safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["accrAmt"]][["text"]]), silent=TRUE)),
                   "refCCY"=        safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["ownrRefCcy"]]), silent=TRUE)),
                   "ccymain"=       safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["ccy"]]), silent=TRUE)),
                   "id"=            safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asst"]]), silent=TRUE)),
                   "type"=          safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["type"]]), silent=TRUE)),
                   "isin"=          safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["isin"]]), silent=TRUE)),
                   "pxfactor"=      safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["prcFactor"]]), silent=TRUE)),
                   "pxmain"=        safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["latestPrc"]][["text"]]), silent=TRUE)),
                   "positionDate"=  safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["effDt"]]), silent=TRUE)),
                   "resReference"=  safeTry(try(.emptyToNA(tail(sapply(data[[i]][["hldPos"]][["hldId"]][["asstIDs"]], function(x) x[["text"]]), 1)), silent=TRUE)),
                   "pricingDate"=   safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["latestPrcDate"]]),silent=TRUE)),
                   "description"=   safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["desc"]]),silent=TRUE)),
                   "country"=       safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["cntry"]]),silent=TRUE)),
                   "primeFX"=       safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldFxFwdDetails"]][["primeFxAmt"]][[".attrs"]]),silent=TRUE)),
                   "secndFX"=       safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldFxFwdDetails"]][["secondFxAmt"]][[".attrs"]]), silent=TRUE)),
                   "fwdFX"=         safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldFxFwdDetails"]][["exchRt"]]), silent=TRUE)),
                   "fwdFXtradeDt"=  safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldFxFwdDetails"]][["tradeDt"]]), silent=TRUE)),
                   "fwdFXexpireDt"= safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldFxFwdDetails"]][["valueDt"]]), silent=TRUE)),
                   "fwdFXqtyPrime"= safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldFxFwdDetails"]][["primeFxAmt"]][["text"]]), silent=TRUE)),
                   "fwdFXqtySecnd"= safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldFxFwdDetails"]][["secondFxAmt"]][["text"]]), silent=TRUE)),
                   "bondrate"=      safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["asstBondDetails"]][["intRate"]]),silent=TRUE)),
                   "bondconvday"=   safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["asstBondDetails"]][["calcBasis"]]),silent=TRUE)),
                   "bondfrequency"= safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["asstBondDetails"]][["intFreq"]]),silent=TRUE)),
                   "bondmaturity"=  safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["asstBondDetails"]][["matDate"]]),silent=TRUE)),
                   "bondlaunch"=    safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["asstBondDetails"]][["intDate"]]),silent=TRUE)),
                   "mmname"=        safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["asstMnyMktDetails"]][["mnyMktName"]]),silent=TRUE)),
                   "mmtype"=        safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["asstMnyMktDetails"]][["mnyMktType"]]),silent=TRUE)),
                   "mmmaturity"=    safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["asstMnyMktDetails"]][["mnyMktTerm"]][["text"]]),silent=TRUE)),
                   "mmrate"=        safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["asstMnyMktDetails"]][["intRate"]]),silent=TRUE)),
                   "mmqtymain"=     safeTry(try(.emptyToNA(data[[i]][["hldPos"]][["hldId"]][["asstDetail"]][["asstMnyMktDetails"]][["mnyMktAmt"]][["text"]]),silent=TRUE)),
                   stringsAsFactors=FALSE)
    }))
}


##' A function to flatten M001 files.
##'
##' This is the description
##'
##' @param data The sarasin M005 data
##' @return Returns a flat file
##' @export
sarasinFlattenM001 <- function(data) {
    ## Flatten and return:
    do.call(rbind, lapply(1:length(data), function(i) {
        data.frame("portfolio"=     safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["ptfl"]]), silent=TRUE)),
                   "account"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["ctlrAcct"]]), silent=TRUE)),
                   "direction"=     safeTry(try(.emptyToNA(data[[i]][["mvmtHdr"]][[".attrs"]][["mvmtTp"]]), silent=TRUE)),
                   "qtymain"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["qty"]]), silent=TRUE)),
                   "qtyaltn"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt2"]][["netAmt"]][["text"]]), silent=TRUE)),
                   "grsAmnt"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["grAmt"]][["text"]]), silent=TRUE)),
                   "netAmnt"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["netAmt"]][["text"]]), silent=TRUE)),
                   "accrual"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["accrAmt"]][["text"]]), silent=TRUE)),
                   "refCCY"=        safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["ownrRefCcy"]]), silent=TRUE)),
                   "ccymain"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["ccy"]]), silent=TRUE)),
                   "ccyaltn"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt2"]][["hldId"]][["ccy"]]), silent=TRUE)),
                   "id"=            safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asst"]]), silent=TRUE)),
                   "isin"=          safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["isin"]]), silent=TRUE)),
                   "effDate"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["effDt"]]), silent=TRUE)),
                   "setDate"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["settleDt"]]), silent=TRUE)),
                   "pxfactor"=      safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["prcFactor"]]), silent=TRUE)),
                   "pxmain"=        safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["prc"]][["text"]]), silent=TRUE)),
                   "positionDate"=  safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["effDt"]]), silent=TRUE)),
                   "pricingDate"=   safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["latestPrcDate"]]),silent=TRUE)),
                   "description"=   safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["desc"]]),silent=TRUE)),
                   "txndesc"=       safeTry(try(.emptyToNA(data[[i]][["mvmtHdr"]][["cmt"]]), silent=TRUE)),
                   "txnReference"=  safeTry(try(.emptyToNA(data[[i]][["mvmtSrc"]][["ctlrMvmtRef"]]), silent=TRUE)),
                   "resReference"=  safeTry(try(.emptyToNA(tail(sapply(data[[i]][["hldCpt1"]][["hldId"]][["asstIDs"]], function(x) x[["text"]]), 1)), silent=TRUE)),
                   "type"=          safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["type"]]), silent=TRUE)),
                   "rectype"=       safeTry(try(.emptyToNA(data[[i]][["mvmtSrc"]][["srcRecType"]]), silent=TRUE)),
                   "txntype"=       safeTry(try(.emptyToNA(data[[i]][["mvmtSrc"]][["srcTransType"]]), silent=TRUE)),
                   "country"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["cntry"]]),silent=TRUE)),
                   "primeFX"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldFxFwdDetails"]][["primeFxAmt"]][[".attrs"]]),silent=TRUE)),
                   "secndFX"=       safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldFxFwdDetails"]][["secondFxAmt"]][[".attrs"]]), silent=TRUE)),
                   "fwdFX"=         safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldFxFwdDetails"]][["exchRt"]]), silent=TRUE)),
                   "fwdFXtradeDt"=  safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldFxFwdDetails"]][["tradeDt"]]), silent=TRUE)),
                   "fwdFXexpireDt"= safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldFxFwdDetails"]][["valueDt"]]), silent=TRUE)),
                   "fwdFXqtyPrime"= safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldFxFwdDetails"]][["primeFxAmt"]][["text"]]), silent=TRUE)),
                   "fwdFXqtySecnd"= safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldFxFwdDetails"]][["secondFxAmt"]][["text"]]), silent=TRUE)),
                   "bondrate"=      safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["asstBondDetails"]][["intRate"]]),silent=TRUE)),
                   "bondconvday"=   safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["asstBondDetails"]][["calcBasis"]]),silent=TRUE)),
                   "bondfrequency"= safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["asstBondDetails"]][["intFreq"]]),silent=TRUE)),
                   "bondmaturity"=  safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["asstBondDetails"]][["matDate"]]),silent=TRUE)),
                   "bondlaunch"=    safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["asstBondDetails"]][["intDate"]]),silent=TRUE)),
                   "mmname"=        safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["asstMnyMktDetails"]][["mnyMktName"]]),silent=TRUE)),
                   "mmtype"=        safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["asstMnyMktDetails"]][["mnyMktType"]]),silent=TRUE)),
                   "mmmaturity"=    safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["asstMnyMktDetails"]][["mnyMktTerm"]][["text"]]),silent=TRUE)),
                   "mmrate"=        safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["asstMnyMktDetails"]][["intRate"]]),silent=TRUE)),
                   "mmqtymain"=     safeTry(try(.emptyToNA(data[[i]][["hldCpt1"]][["hldId"]][["asstDetail"]][["asstMnyMktDetails"]][["mnyMktAmt"]][["text"]]),silent=TRUE)),
                   stringsAsFactors=FALSE)
    }))
}


##' A function to return the sarasin security type vs resource ctype mapping
##'
##' This is the description
##'
##' @return Returns a vector with the mapping.
##' @export
sarasinCTypeMapper <- function() {
    c("FRN"="BOND",
      "Fund"="SHRE",
      "Bond"="BOND",
      "SP"="SP",
      "Hybrid"="BOND",
      "FXFwd"="FXFWD",
      "ConvBond"="BOND",
      "Share"="SHRE",
      "Cash"="CCY",
      "MMLoan"="LOAN",
      "MMDeposit"="DEPO")
}


##' A function to return the sarasin security type vs resource stype mapping
##'
##' This is the description
##'
##' @return Returns a vector with the mapping.
##' @export
sarasinSTypeMapper <- function() {
    c("FRN"="FRN",
      "Fund"="Fund",
      "Bond"=NA,
      "Hybrid"=NA,
      "FXFwd"=NA,
      "ConvBond"="Convertible",
      "Share"="Equity",
      "Cash"=NA,
      "MMLoan"=NA)
}
