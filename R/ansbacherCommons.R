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
         "110"=list("ctype"="DEPO",
                    "stype"="Short-Term"),
         "140"=list("ctype"="ZCPN",
                    "stype"="T-Bill"),
         "400"=list("ctype"="SHRE",
                    "stype"="Stocks"),
         "455"=list("ctype"="SP",
                    "stype"=NA),
         "200"=list("ctype"="BOND",
                    "stype"=NA),
         "203"=list("ctype"="BOND",
                    "stype"=NA),
         "202"=list("ctype"="BOND",
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
ansbacherCLIV001Columns <- function() {
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
         "PVAL_VALUE_ASSETCCY"="QTYCASH",
         "PVAL_SECURITY_IDPRIVS"="IDCCY",
         "PVAL_SECURITY_TITISINS"="ISIN",
         "PVAL_SECURITY_TITTLKS"="TELEKURS",
         "PVAL_SECURITY_LIBS"="SECDESC1",
         "PVAL_SECURITY_LIBLG1S"="SECDESC2",
         "PVAL_SECURITY_FRQREVS"="FREQ",
         "PVAL_SECURITY_TXINTD"="INTEREST",
         "PVAL_SHORTTERM_TXINTD"="SHRTRM_INTEREST",
         "PVAL_SHORTTERM_TERMEFIXEB"="SHRTRM_FIXEDTERM",
         "PVAL_SHORTTERM_DTCREAL"="SHRTRM_ISSUEDT",
         "PVAL_SHORTTERM_VALACHDEVTITD"="SHRTRM_QTY",
         "PVAL_SHORTTERM_LIBEXTS"="SHRTRM_ISSUER_DESC",
         "PVAL_SHORTTERM_EMETTEURLBLGS"="SHRTRM_LONG_DESC",
         "PVAL_SHORTTERM_REVANESTIMEVALD"="SHRTRM_ESTINCOME",
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


##' A function to return the column mappers
##'
##' This is the description
##'
##' @return Returns a list with the column mappers.
##' @export
ansbacherITGI002Columns <- function() {
    ## Columns to be selected:
    list("H__DTSYSL"="SYSDATE",
         "H__DTCPTL"="ACCDATE",
         "P__IDCLL"="ACCOUNT",
         "P__IDPERSL"="CLIENT",
         "O__CDAPPS"="APPCODE",
         "O__DTCPTL"="BOOKDATE",
         "O__FICHES"="OPRCODE",
         "O__DTEXEL"="EXEDATE",
         "O__DTVALL"="VALDATE",
         "O__LIBOPS"="TXNTYPE",
         "O__MVTCTA_LIBCPS"="TXNDESC",
         "O__TYPINS"="SECTYPE",
         "D__IOI"="DEBCRED",
         "O__CDOPS"="TXNCODE",
         "D__DEVCPTEXTS"="ACCFXCODE",
         ##"D__DEVCPTS"="ACCTYPE",
         ##"D__DEVOPS"="TXNCCY",
         "D__DEVCPTS"="TXNCCY",
         "D__DEVOPS"="ACCTYPE",
         "D__DEVOPEXTS"="TXNCCYCODE",
         "D__XRATETRND"="SECACCFX",
         "D__MTFRSD"="FEEAMOUNT",
         ##"D__MTNETCPTD"="NETAMOUNTREF",
         ##"D__MTNETDEVOPD"="NETAMOUNTSEC",
         "D__MTNETCPTD"="NETAMOUNTSEC",
         "D__MTNETDEVOPD"="NETAMOUNTREF",
         "D__MTD"="QTY",
         "D__MTCOMD"="COMAMOUNT",
         "D__MTCOURTD"="BRKAMOUNT",
         "D__IDTIS"="SECID",
         "D__GRES"="SECTYPECODE",
         "D__IDPRIVS"="SECIDENT",
         "D__LIBTIT1S"="SECDESC1",
         "D__LIBTIT2S"="SECDESC2",
         "D__LIBTIT3S"="SECDESC3",
         "D__QTD"="SECQTY",
         "D__CRSTID"="PXCOST",
         "D__CDPYEXTS"="SECCOUNTRY",
         "D__CDBSEXTS"="SECMARKET",
         "D__MTCOMBQED"="BANKCOMM",
         "D__MTCOUD"="COUPON")
}


ansbacherParser <- function(data, fileType) {

    ## Get the column mappers:
    columns <-  do.call(paste0("ansbacher", fileType, "Columns"), list())

    ## Get the columns:
    for (col in names(columns)) {
        data[, col] <- safeColumn(data, col)
    }

    ##
    data <- data[, names(columns)]

    ## Assign new column names:
    colnames(data) <- as.character(sapply(columns, identity))

    data[, "FREQ"] <- safeColumn(data, "FREQ")

    ## Parse the FREQ column:
    data[isNAorEmpty(as.character(data[, "FREQ"])), "FREQ"] <- NA

    ## Swtich the FREQ column:
    data[, "FREQ"] <- sapply(data[, "FREQ"], function(f) switch(as.character(f), "yearly"=1, "half-yearly"=2, "quarterly"=4, "NA"=NA))

    ## Done, return:
    data

}


ansbacherFilePreemble <- function(data) {

    if (any(safeGrep(colnames(data), "REPNAME") == "1")) {
        colName <- "REPNAME"
    }

    if (any(safeGrep(colnames(data), "H__REPORTNAME") == "1")) {
        colName <- "H__REPORTNAME"
    }

    ## ## Get the file type (position or transaction?)
    fileType <- gsub("_", "", as.character(data[, colName])[1])

    if (colName == "REPNAME") {
        ## Exclude 'group' client types:
        data <- data[data[, "CLIENT_TYPE"] == "portfolio", ]
    }

    ## Parse the ansbacher data:
    data <- ansbacherParser(data, fileType)

    ## Done, return:
    return(list("data"=data,
                "type"=fileType))

}
