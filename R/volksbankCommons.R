##' A function to return the security type mapper
##'
##' This is the description
##'
##' @return Returns a list with the security type mappers.
##' @export
volksbankSecTypeMapper <- function() {
    ## Columns to be selected:
    list("HEBELE"=list("ctype"="BOND",
                       "stype"=NA))
}


##' A function to return the column mappers
##'
##' This is the description
##'
##' @return Returns a list with the column mappers.
##' @export
volksbankPosColumnMappers <- function() {
    ## Columns to be selected:
    list("IDCLL"="ACCOUNT",
         "VCURR"="REFCCY",
         "DT"="POSDATE",
         ##"V11"="ISIN",
         "DES"="SECDESC1",
         "SECTYPE"="SECTYPE",
         ##"V14"="SECCCY",
         ##"V15"="CPN",
         ##"V16"="ISSUEDATE",
         ##"V21"="MATURITY",
         ##"V23"="MMMATURITY",
         ##"V20"="FREQ",
         ##"V102"="PXCOST",
         ##"V81"="PXLAST",
         "AMOUNT"="QTY",
         ##"V98"="QTYCASH",
         "ACCDET"="VALSECCCY",
         ##"V97"="MMCCY",
         ##"V96"="MMTYPE",
         ##"V104"="FWDRATE",
         "INTRATE"="INTEREST")
}
