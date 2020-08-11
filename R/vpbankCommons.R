##' A function to return the security type mapper
##'
##' This is the description
##'
##' @param reference VP Bank's instrument reference data frame.
##' @return Returns a list with the security type mappers.
##' @export
vpbankSecTypeMapper <- function(reference) {

    ## Get list by asset group:
    byAstGrp <- extractToList(reference, "ASSET_GROUP_TEXT")

    ## Internal function
    auxFun <- function(data, astGrp, ctype, stype=TRUE) {

        result <- apply(data[[astGrp]], MARGIN=1, function(x) {

            stype <- ifelse(stype, as.character(x["ASSET_TYPE_TEXT"]), NA)

            list("ctype"=ctype, "stype"=stype)

        })

        names(result) <- as.character(data[[astGrp]][, "ASSET_TYPE_TEXT"])

        return(result)
    }

    ## Map the funds to share:
    retval <- auxFun(byAstGrp, "Funds", "SHRE")

    ## Map the shares to share:
    retval <- c(retval, .auxFun(byAstGrp, "Shares", "SHRE"))

    ## Map the bonds to bond:
    retval <- c(retval, .auxFun(byAstGrp, "Bonds", "BOND"))

    ## Map the options to opt:
    retval <- c(retval, .auxFun(byAstGrp, "Options", "OPT"))

    ## Map the futures to fut:
    retval <- c(retval, .auxFun(byAstGrp, "Futures", "FUT"))

    ## Map the money market deposit to depo:
    retval <- c(retval, .auxFun(byAstGrp, "money market deposit", "DEPO", FALSE))

    ## Map the credit to loan:
    retval <- c(retval, .auxFun(byAstGrp, "Credit", "LOAN", FALSE))

    ## Map the structured products to sp:
    retval <- c(retval, .auxFun(byAstGrp, "Structured products", "SP"))

    ## Map fx forwards and cash instruments:
    retval <- c(retval, list("FX forward"=list("ctype"="FXFWD", "stype"=NA),
                             "FX Trade Forward"=list("ctype"="FXFWD", "stype"=NA),
                             "FX Swap near Leg"=list("ctype"="FXFWD", "stype"=NA),
                             "FX Swap Far Leg"=list("ctype"="FXFWD", "stype"=NA),
                             "Swap - Foreign Exchange"=list("ctype"="FXFWD", "stype"=NA),
                             "Public Money Account"=list("ctype"="CCY", "stype"=NA)))

    ## Done, return:
    return(retval)

}


##' A function to return the column mappers
##'
##' This is the description
##'
##' @return Returns a list with the column mappers.
##' @export
vpbankPOSColumnMapper <- function() {
    ## Columns to be selected:
    list("CUSNO"="PORTNAME",
         "PORNO"="ACCNAME",
         "PORCUR"="REFCCY",
         "ISINNO"="ISIN",
         "QUANTITY"="QTY",
         "EVALDATE"="POSDATE",
         "SECPRICE"="PXLAST",
         "SECPRICEDATE"="PXDATE",
         "INSTRTXT"="SECDESCR",
         "SECNO"="SECID",
         "POSCUR"="SECCCY",
         "SECTXT"="SECNAME")
}
