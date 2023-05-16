##' A function to return the security type mapper
##'
##' This is the description
##'
##' @return Returns a list with the security type mappers.
##' @export
blueoceanSecTypeMapper <- function() {
    ## Columns to be selected:
    list("EQUITIES"=list("ctype"="SHRE",
                         "stype"=NA),
         "FUNDS"=list("ctype"="SHRE",
                      "stype"=NA),
         "BONDS"=list("ctype"="BOND",
                      "stype"=NA),
         "FOREX FORWARD CONTRACTS"=list("ctype"="FXFWD",
                                        "stype"=NA))

}


##' A function to return the column mappers
##'
##' This is the description
##'
##' @return Returns a list with the column mappers.
##' @export
blueoceanPosColumnMappers <- function() {
    ## Columns to be selected:
    list("PORTFOLIO"="ACCOUNT",
         #""="REFCCY",
         "LINKPRICEIDENTIFIER"="POSDATE",
         "ISINCODE"="ISIN",
         "DESCRIPTION"="SECDESC1",
         "SECURITYFAMILY"="SECTYPE",
         "CURRENCY"="SECCCY",
         "ISSUEDATE"="ISSUEDATE",
         "MATURITYDATE"="MATURITY",
         "LOCALCOSTPRICE"="PXCOST",
         "MARKETPRICE"="PXLAST",
         "MARKETPRICEDATE"="SECPXDATE",
         "QUANTITY"="QTY",
         "LOCALMARKETVALUE"="QTYCASH",
         "LOCALTOTALCOST"="FWDRATE",
         "FXFROMQUANTITY"="FWDMAINQTY",
         "FXTOQUANTITY"="FWDALTNQTY",
         "FXFROMCURRENCY"="FWDCCYMAIN",
         "FXTOCURRENCY"="FWDCCYALTN")
}
