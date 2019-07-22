##' A function to return the security type mapper
##'
##' This is the description
##'
##' @return Returns a list with the security type mappers.
##' @export
juliusbaerSecTypeMapper <- function() {
    ## Columns to be selected:
    list("151"=list("ctype"="BOND",
                    "stype"=NA),
         "155"=list("ctype"="BOND",
                    "stype"=NA),
         "162"=list("ctype"="BOND",
                    "stype"=NA),
         "170"=list("ctype"="BOND",
                    "stype"=NA),
         "169"=list("ctype"="SP",
                    "stype"=NA),
         "DEPO"=list("ctype"="DEPO",
                     "stype"=NA),
         "201"=list("ctype"="SHRE",
                    "stype"="Stocks"),
         "203"=list("ctype"="SHRE",
                    "stype"="Stocks"),
         "217"=list("ctype"="SHRE",
                    "stype"="ADR"),
         "275"=list("ctype"="SHRE",
                    "stype"="Equity Fund"),
         "276"=list("ctype"="SHRE",
                    "stype"="Equity Fund"),
         "282"=list("ctype"="SHRE",
                    "stype"="Equity Fund"),
         "251"=list("ctype"="SHRE",
                    "stype"="Fixed Income"),
         "249"=list("ctype"="SHRE",
                    "stype"="Alternative"),
         "722"=list("ctype"="SHRE",
                    "stype"="Alternative"),
         "723"=list("ctype"="OTHER",
                    "stype"=NA))
}


##' A function to return the column mappers
##'
##' This is the description
##'
##' @return Returns a list with the column mappers.
##' @export
juliusbaerPosColumnMappers <- function() {
    ## Columns to be selected:
    list("V3"="ACCOUNT",
         "V5"="REFCCY",
         "V6"="POSDATE",
         "V11"="ISIN",
         "V12"="SECDESC1",
         "V76"="SECTYPE",
         "V14"="SECCCY",
         "V15"="CPN",
         "V16"="ISSUEDATE",
         "V21"="MATURITY",
         "V23"="MMMATURITY",
         "V20"="FREQ",
         "V102"="PXCOST",
         "V81"="PXLAST",
         "V80"="QTY",
         "V98"="QTYCASH",
         "V99"="VALSECCCY",
         "V97"="MMCCY",
         "V96"="MMTYPE",
         "V104"="FWDRATE",
         "V17"="INTEREST")
}
