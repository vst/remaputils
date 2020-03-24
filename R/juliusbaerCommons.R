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
         "V10"="SECID",
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
         "V90"="MMID",
         "V96"="MMTYPE",
         "V104"="FWDRATE",
         "V17"="INTEREST")
}


##' A function to return the column mappers
##'
##' This is the description
##'
##' @return Returns a list with the column mappers.
##' @export
juliusbaerTraColumnMappers <- function() {
    ## Columns to be selected:
    list("V2"="RECTYPE",
         "V3"="ADVISNO",
         "V4"="ACCOUNT",
         "V9"="REFCCY",
         "V10"="TELEKURS",
         "V11"="SECDESC",
         "V12"="ISIN",
         "V14"="TRADECCY",
         "V17"="TXNTYPE", ## GA-CODE
         ## LPM   = "DELIVERY AGAINST PAYMENT"
         ## SOU   = "SUBSCRIPTION"
         ## ACT   = "SPOT PURCHASE"
         ## RPM   = "RECEIPT AGAINST PAYMENT"
         ## 115-L = "FIXED-TERM FIDUCIARY DEPOSIT LIQUIDATION"
         ## 115-I = "FIXED-TERM FIDUCIARY DEPOSIT INTERSEST"
         ## 115-C = "FIXED-TERM FIDUCIARY DEPOSIT FIXED-TERM FIDUCIARY DEPOSIT COMMISSION "
         ## 115-N = "FIXED-TERM FIDUCIARY DEPOSIT NEW CONTRACT"
         ## INT   = "INTEREST"
         ## 210-L = "FX FORWARD LIQUIDATION"
         ## 210-N = "FX FORWARD NEW CONTRACT"
         ## 200   = "SPOT ORDERS"
         ## 100   = "PAYMENT ORDERS"
         ## TRS   = "CLIENT TRANSFER (EXIT)"
         ## 931   = "FLAT FEE (CFF)"
         ## 152   = "CLIENT CREDIT (EXTRAGROUP)"
         ## DIV   = "DIVIDEND"
         ## RBT   = "REDEMPTION"
         ## RAC   = "REDEMPTION"
         ## 271   = "THIRD PARTY MANAGEMENT FEES INT"
         ## 110-N = "CALL FIDUCIARY DEPOSIT NEW CONTRACT"
         ## 110-P = "CALL FIDUCIARY DEPOSIT ADJUSTMENT"
         ## 110-I = "CALL FIDUCIARY DEPOSIT INTEREST"
         ## 110-C = "CALL FIDUCIARY DEPOSIT COMMISSION"
         ## VCT = "SPOT SALE"
         ## 999 = "OTHER BOOKINGS"
         "V18"="TXNTYPE2",
         ## ECE = "EXCHANGE (ENTRY)"
         ## ECS = "EXCHANGE (EXIT)"
         ## SSV = "EXIT VALUELESS SECURITIES"
         ## VCT = "SPOT SALE"
         ## DIE = "DISTRIBUTION (ENTRY)"
         ## DIS = "DISTRIBUTION (EXIT)"
         "V19"="ISPERF", ##PERF-NEUTRAL
         "V20"="QTY", ## DBEW = Number of units or nominal value, KBEW = account credit or debit
         "V21"="DIRECTION",
         "V23"="PXMAIN", ## TIT-KURS
         "V24"="PXFACTOR", ## TIT-KURS-MULTFAKT
         "V26"="PXCCY",
         "V27"="BOOKDATE",
         "V28"="VALUEDATE", ## VAL-DAT
         "V29"="TXNTEXT1",
         "V30"="TXNTEXT2",
         "V31"="TXNTEXT3",
         "V32"="NETAMOUNT", ## NETTOBETRAG-TITWRG
         "V34"="BROKERAGE", ## COURTAGE
         "V37"="INCOMETAX", ## CPS-STEUERN
         "V38"="TXNREFERENCE", ## TR-REFNR
         "V40"="BALANCE", ## BESTAND-NEU
         "V50"="TRADEDATE", ## ABS-DAT 08, n Trade date (Format CCYYMMDD)
         "V55"="SECCCY", ## TIT-WRG-ISO-CD
         "V59"="FEETYP1", ## ??
         "V60"="FEEAMT1", ## ??
         "V61"="FEECCY1", ## ??
         "V62"="FEETYP2", ## ??
         "V63"="FEEAMT2", ## ??
         "V64"="FEECCY2", ## ??
         "V65"="FEETYP3", ## ??
         "V66"="FEEAMT3", ## ??
         "V67"="FEECCY3", ## ??
         "V68"="FEETYP4", ## ??
         "V69"="FEEAMT4", ## ??
         "V70"="FEECCY4", ## ??
         "V71"="FEETYP5", ## ??
         "V72"="FEEAMT5", ## ??
         "V73"="FEECCY5", ## ??
         "V74"="FEETYP6", ## ??
         "V75"="FEEAMT6", ## ??
         "V76"="FEECCY6", ## ??
         "V77"="FEETYP7", ## ??
         "V78"="FEEAMT7", ## ??
         "V79"="FEECCY7", ## ??
         "V80"="FEETYP8", ## ??
         "V81"="FEEAMT8", ## ??
         "V82"="FEECCY8", ## ??
         "V83"="FEETYP9", ## ??
         "V84"="FEEAMT9", ## ??
         "V85"="FEECCY9", ## ??
         "V86"="FEETYP10", ## ??
         "V87"="FEEAMT10", ## ??
         "V88"="FEECCY10", ## ??
         "V109"="MMID")
}
