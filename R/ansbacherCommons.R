##' A function to return the security type mapper
##'
##' This is the description
##'
##' @return Returns a list with the security type mappers.
##' @export
ansbacherSecTypeMapper <- function() {
    ## Columns to be selected:
    list("100"=list("ctype"="CCY",
                    "stype"=NA),
         "140"=list("ctype"="ZCPN",
                    "stype"="T-Bill"),
         "400"=list("ctype"="SHRE",
                    "stype"="Equities"),
         "455"=list("ctype"="SP",
                    "stype"=NA),
         "200"=list("ctype"="BOND",
                    "stype"=NA),
         "300"=list("ctype"="SHRE",
                    "stype"="Fixed Income"),
         "120"=list("ctype"="FXFWD",
                    "stype"=NA),
         "310"=list("ctype"="SHRE",
                    "stype"="Alternative"),
         "320"=list("ctype"="SHRE",
                    "stype"="ETF|Index"),
         "220"=list("ctype"="BOND",
                    "stype"=NA),
         "335"=list("ctype"="SHRE",
                    "stype"="Alternative"))
}


##' A function to return the column mappers
##'
##' This is the description
##'
##' @return Returns a list with the column mappers.
##' @export
ansbacherColumnMappers <- function() {
    ## Columns to be selected:
    list("ACCDATE"="POSDATE",
         "REFCCY"="REFCCY",
         "CLIENT"="ACCOUNT",
         "DESC_SHORT"="PORTNAME",
         "PVAL_ASSETTYPE"="ASSETTYPE",
         "PVAL_INSTRUMENTTYPE"="SECTYPE",
         "PVAL_ASSETCCY"="SECCCY",
         "PVAL_SECURITY_NBJOURL"="ACCRDAYS",
         "PVAL_FXRATE_ASSETCCY_VALUECCY"="SECREFFX",
         "PVAL_MATURITY"="MATURITY2",
         "PVAL_SECURITY_DTECHL"="MATURITY",
         "PVAL_SIGN"="SIGN",
         "PVAL_SECURITY_PYRISKS"="COUNTRY",
         "PVAL_VALUE_ASSETCCY"="VALUESECCCY",
         "PVAL_VALUE_VALUECCY"="VALUEREFCCY",
         "PVAL_SECURITY_DTCRSL"="SECPXDATE",
         "PVAL_SUBTYPE"="SUBTYPE",
         "PVAL_LIQUIDITY_CCYDESC"="LIQDESC",
         "PVAL_LIQUIDITY_IBAN"="CASHACCNO",
         "PVAL_LIQUIDITY_BOOKCOST"="QTYCASH",
         "PVAL_SECURITY_IDPRIVS"="IDCCY",
         "PVAL_SECURITY_TITISINS"="ISIN",
         "PVAL_SECURITY_TITTLKS"="TELEKURS",
         "PVAL_SECURITY_LIBS"="SECDESC1",
         "PVAL_SECURITY_LIBLG1S"="SECDESC2",
         "PVAL_SECURITY_FRQREVS"="FREQ",
         "PVAL_SECURITY_TXINTD"="INTEREST",
         "PVAL_SECURITY_DTEMISSL"="ISSUEDATE",
         "PVAL_SECURITY_TYPQTS"="QUOTETYPE",
         "PVAL_SECURITY_CRSD"="PXLAST",
         "PVAL_SECURITY_CRSDEVS"="PXCCY",
         "PVAL_SECURITY_CRSEVALD"="SECPXVALCCY",
         "PVAL_SECURITY_PXREVD"="PXCOST",
         "PVAL_SECURITY_QTD"="QTY",
         "PVAL_FOREX_DEVACHS"="SOLDCCY",
         "PVAL_FOREX_DEVVTES"="BOUGHTCCY",
         "PVAL_FOREX_MTACHD"="SOLDQTY",
         "PVAL_FOREX_MTVTED"="BOUGHTQTY",
         "PVAL_FOREX_XRATEFRXD"="FWDRATE",
         "PVAL_FOREX_XRATEFRXTERMED"="CURRENTFWDRATE",
         "PVAL_FOREX_MTPNLD"="FXFWDPNL")
}
