eBankVSLink <<- c("CAPITAL"="",
                 "GUARANTEEACCOUNT"="",
                 "CTFUTURE-COMMODITY"="COM_FUT",
                 "CTFUTURE-SHAREINDEX"="IND_FUT",
                 "CTFUTURE-METALS"="MET_FUT",
                 "FXFORWARD"="",
                 "BOND"="STR_BD",
                 "PERPETUALBOND"="PERP_BD",
                 "SHARI'ABOND"="SHAR_BD",
                 "CONVERTIBLEBOND"="CONV_BD",
                 "FIXEDINCOMEFUND"="BOND_FD",
                 "REGISTEREDSHARE"="ORD_SH",
                 "CALLOPTIONONEQUITY"="IND_OPT",
                 "PUTOPTIONONEQUITY"="IND_OPT",
                 "CALLOPTIONSHAREINDEX"="IND_OPT",
                 "PUTOPTIONONEQUITYINDEX"="IND_OPT",
                 "BEARERSHARE"="ORD_SH",
                 "FUTUREONEQUITYINDEX"="IND_FUT",
                 "EQUITYFUND"="SHARE_FD",
                 "HEDGEFUNDS-ARBITRAGEFIXEDINCOME"="HEDGE_FD",
                 "PRIVATEEQUITY-MISCELLANEOUS"="PRIV_SH",
                 "PRIVATEEQUITY-PRIVATEINVESTMENTS"="PRIV_SH",
                 "SHS/STRPR/CAPNPROTPARTICIP"="SP_SH",
                 "BD/STRPR/CAPNPROTPARTICIP"="SP_BD",
                 "METAL-SILVER"="PRECMET_NOVAT",
                 "METAL-GOLD"="PRECMET_NOVAT",
                 "FUTUREONMETALS"="COM_FUT",
                 "FUTURE-COMMODITY"="COM_FUT",
                 "CALLWARRANTONEQUITY"="SH_WRT",
                 "HEDGEFUNDS-ARBITRAGEMULTISTRATEGY"="HEDGE_FD",
                 "HEDGEFUNDS-EVENTDRIVEN"="HEDGE_FD",
                 "HEDGEFUNDS-FUNDOFFUNDS"="HEDGE_FD",
                 "HEDGEFUNDS-LONG/SHORT"="HEDGE_FD",
                 "MISCELLANEOUS-GUARANTEES"="",
                 "MIXEDFUND"="HEDGE_FD",
                 "HEDGEFUNDS-CTA"="HEDGE_FD",
                 "HEDGEFUNDS-EQUITIESMARKETNEUTRAL"="HEDGE_FD",
                 "CALLOPTIONONCURRENCY"="FXOTC_OPT",
                 "PUTOPTIONONCURRENCY"="FXOTC_OPT",
                 "PUTOPTIONONSHORTTERMINTERESTRATE"="INTR_OPT",
                 "CCY/STRPR/MISC"="SP_MISC",
                 "METAL-PLATINUM"="PRECMET_NOVAT",
                 "MONEYMARKETFUND"="MMKT_FD",
                 "HEDGEFUNDS-MISCELLANEOUS"="HEDGE_FD",
                 "FUND-METAL"="HEDGE_FD",
                 "MISCELLANEOUS"="")


##' This function normalizes the position file from Pictet's ebanking system.
##'
##' This is the description
##'
##' @param filePath The data frame with flattened records.
##' @param session The rdecaf session.
##' @return Returns the normalised records.
##' @export
pictetEBankingNormalizer <- function(filePath, session) {

    ## Get system accounts:
    sysAccounts <- as.data.frame(getResource("accounts", params=list("format"="csv", "page_size"=-1), session=session))

    ## Get the base file name:
    fileName <- strsplit(tail(strsplit(filePath, "/")[[1]], 1), ".xlsx")[[1]]

    ## Convert to csv using libreoffice CLI:
    ## system("/usr/lib/libreoffice/program/soffice --headless --convert-to csv --outdir /tmp/ ~/Downloads/Holdings_20190606145751.xlsx")

    ## Read the data:
    data <- read.csv(paste0("/tmp/", fileName, ".csv"), header=TRUE, sep=",")

    ## Parse the column names:
    colnames(data) <- trimConcatenate(colnames(data))

    data[, "accmain"] <- sysAccounts[match(paste0("00", as.character(data[, "ACCOUNTNR"])), as.character(sysAccounts[, "name"])), "id"]

    data <- data[!is.na(data[, "accmain"]), ]

    data <- data[!data[, "accmain"] == 25, ]
    data <- data[!data[, "accmain"] == 30, ]

    ## Get the cash positions:
    cshIdx <- data[, "ASSETCLASS"] == "Cash" & data[, "SUBASSETCLASS"] == "Current accounts"  |
              data[, "ASSETCLASS"] == "Metals" & isNAorEmpty(as.character(data[, "UNDERLYING1FINANCIALINSTRUMENTTYPE"]))

    ## Get the forward positions:
    fwdIdx <- data[, "ASSETCLASS"] == "Cash" &
              data[, "SUBASSETCLASS"] == "Forward accounts" &
              nchar(as.character(data[, "UNDERLYING1"])) == 3 &
              safeGrep(toupper(data[, "DESCRIPTION"]), "OPTION") == "0"

    ## Get the security positions:
    secIdx <- !cshIdx & !fwdIdx

    cshPositions <- data[cshIdx, ]
    fwdPositions <- data[fwdIdx, ]
    secPositions <- data[secIdx, ]

    fwdPositions[, "RATE"] <- fwdPositions[, "VALUATIONINREFERENCECURRENCY"] / fwdPositions[, "QUANTITY"]
    fwdPositions[, "QTYNATIVE"] <- abs(fwdPositions[, "QUANTITY"])
    fwdPositions[, "QTYNATIVESIGN"] <- ifelse(fwdPositions[, "QUANTITY"] < 0, "-", "+")
    fwdPositions[, "QTY"] <- abs(fwdPositions[, "VALUATIONINREFERENCECURRENCY"])
    fwdPositions[, "MATURITY"] <- gsub("-", "", as.Date(as.character(fwdPositions[, "MATURITY"]), format="%m/%d/%Y"))
    fwdPositions[, "ISSUEDATE"] <- NA

    cshPositions[, "ACCOUNTNAME"] <- cshPositions[, "SUBASSETCLASS"]
    cshPositions[, "ACCOUNTNAME"] <- cshPositions[, "SUBASSETCLASS"]
    cshPositions[, "ACCOUNTCURRENCY"] <- cshPositions[, "POSITIONCURRENCY"]
    cshPositions[, "QTY"] <- cshPositions[, "QUANTITY"]
    cshPositions[, "CLIENTNO"] <- cshPositions[, "ACCOUNTNR"]

    ## Map the instrument type to the PicLink SECQTYPEALQ convetion:
    secPositions[, "SECTYPEALQ"] <-  eBankVSLink[match(trimConcatenate(as.character(secPositions[, "FINANCIALINSTRUMENTTYPE"])), names(eBankVSLink))]
    secPositions <- secPositions[!isNAorEmpty(secPositions[, "SECTYPEALQ"]), ]

    secPositions[, "CONTRACTSIZE"] <- as.numeric(ifelse(is.na(secPositions[, "CONTRACTSIZE"]), 1, secPositions[, "CONTRACTSIZE"]))
    secPositions[, "QUOTETYPE"]    <- ifelse(secPositions[, "MARKETPRICEP"] != "P", "C", "P")
    secPositions[, "CURRENCY"] <- as.character(secPositions[, "PRICECURRENCY"])
    secPositions[, "SECURITYCURRENCY"] <- secPositions[, "CURRENCY"]
    secPositions[isNAorEmpty(secPositions[, "SECURITYCURRENCY"]), "SECURITYCURRENCY"] <- as.character(secPositions[isNAorEmpty(secPositions[, "SECURITYCURRENCY"]), "POSITIONCURRENCY"])
    secPositions[isNAorEmpty(secPositions[, "CURRENCY"]), "CURRENCY"] <- as.character(secPositions[isNAorEmpty(secPositions[, "CURRENCY"]), "POSITIONCURRENCY"])
    secPositions[, "PXLAST"] <- secPositions[, "MARKETPRICEINPRICECURRENCY"]
    secPositions[, "SECPXDATE"] <- as.Date(as.character(secPositions[, "MARKETPRICEDATE"]), format="%m/%d/%Y")
    secPositions[, "ISOCOUNTRY"] <- secPositions[, "ISSUERCOUNTRYCODE"]
    secPositions[, "INTERESTMETHOD"] <- "Act/Act"
    secPositions[, "CPNFIRSTDATE"] <- NA
    secPositions[, "ISSUEDATE"] <- NA
    secPositions[, "SECURITYNAME"] <- secPositions[, "DESCRIPTION"]
    secPositions[, "TELEKURS"] <- secPositions[, "TELEKURSID"]
    secPositions[, "INTERESTRATE"] <- secPositions[, "CURRENTINTERESTRATE1"]
    secPositions[, "ISOCURRENCY2"] <- secPositions[, "CURRENCY"]
    secPositions[, "CUSIP"] <- NA
    secPositions[, "FX2"] <- secPositions[, "EXCHANGERATETOREFERENCECURRENCY"]
    secPositions[, "PXCOSTCURRENCY"] <- secPositions[, "CURRENCY"]
    secPositions[, "refCcy"] <- secPositions[, "REFERENCECURRENCY"]
    secPositions[, "QTY"] <- secPositions[, "QUANTITY"]
    secPositions[, "CLIENTNO"] <- secPositions[, "ACCOUNTNR"]
    ## secPositions[!is.na(secPositions[, "GROSSUNITCOSTINPOSITIONCURRENCY"]), "PXCOST"] <- secPositions[!is.na(secPositions[, "GROSSUNITCOSTINPOSITIONCURRENCY"]), "GROSSUNITCOSTINPOSITIONCURRENCY"]

    missingPrice <- is.na(secPositions[, "PXLAST"])
    secPositions <- secPositions[!missingPrice, ]

    ## valueDate <- as.Date(as.character(data[1, "VALUATIONDATE"]), format="%m/%d/%Y")
    ## missingPSymbols <- paste(gsub(" ", "-", secPositions[missingPrice, "ISIN"]), secPositions[missingPrice, "SECURITYCURRENCY"], "[PICTET]")
    ## aa <- lapply(missingPSymbols, function(s) getOhlcObsForSymbol(session, s, lte=valueDate, lookBack=300))
    ## secPositions[missingPrice, "PXLAST"] <- lapply(aa, function(x) valueOfNearestDate(valueDate, x, Inf, valueField="close")[["value"]])
    ## secPositions[missingPrice, "QTY"] <- abs(round(secPositions[missingPrice, "QTY"] / secPositions[missingPrice, "CONTRACTSIZE"] / secPositions[missingPrice, "PXLAST"]))

    return(list("L206"=secPositions,
                "L126"=secPositions,
                "L120"=cshPositions,
                "L122"=fwdPositions))


    ## computeDuration <- function(faceValue, coupon, frequency, ytm, currentDate, maturity) {

    ##     ## Construct the cashflow data frame:
    ##     cashflow <- data.frame("dates"=rev(seq(as.Date(maturity), as.Date(currentDate), by=-(365/frequency))),
    ##                            "cash"=coupon / frequency)

    ##     ## Add the redemption:
    ##     cashflow[NROW(cashflow), "cash"] <- cashflow[NROW(cashflow), "cash"] + 100

    ##     ## Compute present values of cash flows:
    ##     cashflow[, "PV"] <- cashflow[, "cash"] / (1+(ytm/frequency)/100)^seq(1:NROW(cashflow))

    ##     ## Comput the periods duration:
    ##     cashflow[, "tDur"] <- (cashflow[, "PV"] / sum(cashflow[, "PV"])) * as.numeric((cashflow[,"dates"] - as.Date(currentDate)) / 365)

    ##     ## Return the duration:
    ##     sum(cashflow[, "tDur"])

    ## }

    ## strBondIdx <- secPositions[, c("SECTYPEALQ")] == "STR_BD"

    ## auxFun <- function(duration, faceValue, coupon, ytm, currentDate, maturity) {

    ##     frequencies <- c(1, 2, 4, 12)

    ##     retval <- sapply(c(1, 2, 4, 12), function(frq) {
    ##         computeDuration(faceValue, coupon, frq, ytm, currentDate, maturity)
    ##     })


    ##     dist <- abs(retval - duration)

    ##     frequencies[dist == min(dist)][1]

    ## }

    ## strBond <- secPositions[strBondIdx, ]

    ## freqs <- sapply(1:NROW(strBond), function(i) {

    ##     strB <- strBond[i, ]

    ##     auxFun("duration"=strB[, "DURATION"],
    ##            "faceValue"=100,
    ##            "coupon"=strB[, "CURRENTINTERESTRATE1"],
    ##            "ytm"=strB[, "YIELDTOMATURITY"],
    ##            "currentDate"=as.Date(as.character(strB[, "VALUATIONDATE"]), format="%m/%d/%Y"),
    ##            "maturity"=as.Date(as.character(strB[, "MATURITY"]), format="%m/%d/%Y"))

    ## })
    ## browser()
    ## computeDuration(100, 3.068, 4, 3.875271, "2018-12-31", "2020-12-18")

}



##' A function to normalise the pictet flattened files for give ltype.
##'
##' This is the description
##'
##' @param data The data frame with flattened records.
##' @param ltype The ltype of the records.
##' @param accounts the account mapping.
##' @return Returns the normalised records.
##' @export
pictetFileNormalizer <- function(data, ltype, accounts) {

    ## Get rid of NA columns:
    data <- data[,!apply(data, MARGIN=2, function(x) all(is.na(x)))]

    ## Parse the colnames of the data:
    colnames(data) <- mgsub(colnames(data), c("L001-", "L007-"))

    ## Get the file mapper:
    map <- pictetFileMappers()[[ltype]]

    ## If no map, return NULL:
    if (is.null(map)) {
        return(NULL)
    }

    ## Get the records:
    records <- data.frame(do.call(cbind, lapply(names(map), function(n) safeColumn(data, n))),
                          stringsAsFactors=FALSE)

    ## Write the column names:
    colnames(records) <- as.character(unlist(map))

    ## Grep the account name:
    greppedAccountID <- as.character(apply(mgrep(safeColumn(records, "CLIENTNO"), names(accounts)), MARGIN=1, function(x) names(x[x != "0"])))

    ## Get the account name:
    records[, "accountName"] <- as.character(accounts[match(greppedAccountID, names(accounts), incomparables=NA)])

    ## Append the ltype:
    records[, "ltype"] <- ltype

    ## Done, return:
    records
}


##' A function to return the ltype file mappers.
##'
##' This is the description
##'
##' @return Returns a list with the file mappers.
##' @export
pictetFileMappers <- function() {
    list("L206"=list("NOVAL"=list("name"="PICTETID"),
                     "NOVAL-TLK-NOUV"=list("name"="TELEKURS"),
                     "NOVAL-ISIN"=list("name"="ISIN"),
                     "NOVAL-SEDOL"=list("name"="SEDOL"),
                     "NOVAL-CUSIP"=list("name"="CUSIP"),
                     "NMVAL"=list("name"="SECURITYNAME"),
                     "QTDER-CALC-FCONV-D6"=list("name"="CONTRACTSIZE"),
                     "COGENR-CPTA"=list("name"="QUOTETYPE"),
                     "LGRGENR-VAL"=list("name"="GRPSECURITY"),
                     "COGENR-1-VAL"=list("name"="SECURITYTYPE"),
                     "COGENR-VAL-SWT"=list("name"="SECURITYTYPESWIFT"),
                     "COGENR-VAL-AVQ"=list("name"="SECTYPEALQ"),
                     "COGENR-1-VAL"=list("name"="SECRITYTYPEPICTET1"),
                     "COGENR-2-VAL"=list("name"="SECRITYTYPEPICTET2"),
                     "COGENR-3-VAL"=list("name"="SECRITYTYPEPICTET3"),
                     "COMONL-ISO-TIT"=list("name"="ISOCURRENCY1"),
                     "COMONL-ISO-COURS"=list("name"="ISOCURRENCY2"),
                     "COMONL-ISO-RV"=list("name"="CURRENCY"),
                     "JJPAIM-CPS"=list("name"="CPNPAYDAY"),
                     "MMPAIM-CPS"=list("name"="CPNPAYMONTH"),
                     "COPERI-PAIM-CPS"=list("name"="CPNFREQ"),
                     "TXINT-VAL"=list("name"="INTERESTRATE"),
                     "DSPAIM-CPS-PREC"=list("name"="CPNPREV"),
                     "DSPAIM-CPS-NEXT"=list("name"="CPNNEXT"),
                     "COBRCH-TLK"=list("name"="TELEKURSCATEGORY"),
                     "COPAYSL-ISO-DOM"=list("name"="ISOCOUNTRY"),
                     "DSJCE"=list("name"="ISSUEDATE"),
                     "DSRBT-DERN"=list("name"="MATURITY"),
                     "DSDEN"=list("name"="CALLDATE"),
                     "COTAUX-INT-VAR"=list("name"="ISVARIABLE"),
                     "COMONL-ISO-RITI"=list("name"="RISKCURRENCY"),
                     "COPAYSL-ISO-RITI"=list("name"="RISKCOUNTRY"),
                     "NOVAL-ISIN-TISJA"=list("name"="ISINUNDERLYING"),
                     "Q9TISJA"=list("name"="QTYUNDERLYING"),
                     "DSPAIM-CPS-DERN"=list("name"="CPNLASTDATE"),
                     "COCALC-INT"=list("name"="INTERESTMETHOD"),
                     "CORAT-STP"=list("name"="RATINGSNP"),
                     "CORAT-MOOD"=list("name"="RATINGMOODYS"),
                     "DSPAIM-CPS-PREM"=list("name"="CPNFIRSTDATE"),
                     "OPP2-LVL-1"=list("name"="OPPCLASS1"),
                     "OPP2-LVL-2"=list("name"="OPPCLASS2")),
         "L218"=list("NOVAL-ISIN"=list("name"="ISIN"),
                     "GRCOURS-VAL-EVAL"=list("name"="PRICE"),
                     "EXCOURS-VAL-EVAL"=list("name"="EXPONENT"),
                     "OPCOURS-VAL-DALI"=list("name"="OPERAND"),
                     "SIEXP-COURS-VAL-EVAL"=list("name"="EXPONENTSIGN"),
                     "DSCOURS"=list("name"="PRCIEDATE")),
         "L120"=list("NOCLI-DOS"=list("name"="CLIENTNO"),
                     "CLCC"=list("name"="ACCKEY"),
                     "COID-REC-EST-DALI"=list("name"="IDENTIFICATIONCODE"),
                     "SOCC-CPTA"=list("name"="QTY"),
                     "SISOLDE-CC-CPTA"=list("name"="QTYSIGN"),
                     "MTEST"=list("name"="VALUATION"),
                     "SIMEST"=list("name"="VALUATIONSIGN"),
                     "CLCC"=list("name"="CURRENTACCOUNTKEY"),
                     "COMONL-ISO-CC"=list("name"="ACCOUNTCURRENCY"),
                     "NMRUB-CC"=list("name"="ACCOUNTNAME"),
                     "S9CC-CPTA"=list("name"="QTY2"),
                     "SISOLDE-CC-CPTA-2"=list("name"="QTY2SIGN"),
                     "NOVAL-ISIN"=list("name"="ISIN"),
                     "NOVAL-NEN-L9"=list("name"="SECURITYNUMBER")),
         "L126"=list("NOCLI-DOS"=list("name"="CLIENTNO"),
                     "COID-REC-EST-DALI"=list("name"="IDENTIFICATIONCODE"),
                     "NOSEQ-REC-EST-DALI"=list("name"="SOMEID"),
                     "DSCOURS"=list("name"="SECPXDATE"),
                     "NOVAL-NEN-L9"=list("name"="SECURITYCODE"),
                     "PRUNI-BRUT-MONRV"=list("name"="PXCOST"),
                     "NOVAL-ISIN"=list("name"="ISIN"),
                     "SOQTE"=list("name"="QTY"),
		     "CHMONCO-MONES"=list("name"="FX1"),
                     "CHMONRV-MONES"=list("name"="FX2"),
                     "SIMONT-NOMI-MONES"=list("name"="QTYSIGN"),
                     "COGENR-CPTA"=list("name"="QUOTETYPE"),
                     "COMONL-ISO-VAL"=list("name"="SECURITYCURRENCY"),
                     "COMONL-ISO-RV"=list("name"="PXCOSTCURRENCY"),
                     "COMONL-ISO-COURS"=list("name"="PXLASTCURRENCY"),
                     "CUVAL"=list("name"="PXLAST"),
                     "MTEST"=list("name"="VALUATION"),
                     "SIMEST"=list("name"="VALUATIONSIGN"),
                     "MTINT-COURU-MONES"=list("name"="ACCRDINTEREST"),
                     "SIMONT-INT-COURU-MONES"=list("name"="SIGNACCRD"),
                     "COBRCH-TLK"=list("name"="TELEKURSCATEGORY"),
                     "NOVAL-TLK-NOUV"=list("name"="TELEKURS")),
         "L110"=list("MTEST"=list("name"="VALUATION"),
                     "SIMEST"=list("name"="VALSIGN"),
                     "NOCLI-DOS"=list("name"="CLIENTNO")),
         "L121"=list("NOCLI-DOS"=list("name"="CLIENTNO"),
                     "COID-REC-EST-DALI"=list("name"="IDENTIFICATIONCODE"),
                     "COID-REC-EST-DALI"=list("name"="LTYPE"),
                     "MTEST"=list("name"="QTY"),
                     "SIMEST"=list("name"="QTYSIGN"),
                     "NOVAL-ISIN"=list("name"="ISIN"),
                     "COMONC-CC"=list("name"="CCY")),
         "L122"=list("NOCLI-DOS"=list("name"="CLIENTNO"),
                     "COPAYSL-ISO-VAL"=list("name"="COUNTRY"),
                     "CORUB-CC"=list("name"="ACCTYPECODE"),
                     "COMONC-CC"=list("name"="CURRENCY"),
                     "COID-REC-EST-DALI"=list("name"="IDENTIFICATIONCODE"),
                     "COID-REC-EST-DALI"=list("name"="LTYPE"),
                     "S9CC-CPTA"=list("name"="QTYNATIVE"),
                     "SISOLDE-CC-CPTA-2"=list("name"="QTYNATIVESIGN"),
                     "MTEST"=list("name"="QTY"),
                     "SIMEST"=list("name"="QTYSIGN"),
                     "NOVAL-ISIN"=list("name"="ISIN"),
                     "COMONC-CC"=list("name"="CCY"),
                     "PCBNET-NREAL-MONES"=list("name"="PNLPERC"),
                     "BNNET-MONES"=list("name"="PNLUNREALVALCCY"),
                     "SOCC-CPTA"=list("name"="RECORDDAY"),
                     "SIBNET-NREAL-MONES"=list("name"="PNLUNREALVALCCYSIGN"),
                     "NMCONTR"=list("name"="DESCRIPTION"),
                     "CHTERM-ANC"=list("name"="RATE"),
                     "DSJCE"=list("name"="ISSUEDATE"),
                     "DSECH"=list("name"="MATURITY")),
         "L001"=list("NOCNTNR-AVQ"=list("name"="CLIENTNO"),
                     "CLCC-PAIM-TIT"=list("name"="PAYMENT"),
                     ##"CORUB-CC-PAIM-TIT"=list("name"="PAYMENTTYPE"),
                     "CORUB-DOS"=list("name"="PAYMENTTYPE"),
                     "COCPTA-TIT"=list("name"="DIRECTION"),
                     "TELGN-1-TRAN"=list("name"="DESC1"),
                     "TEREF-OPER-OP"=list("name"="REFERENCE"),
                     "NOVAL-ISO"=list("name"="ISONO"),
                     "COGENR-CPTA"=list("name"="QUOTETYPE"),
                     "NOVAL-ISIN"=list("name"="ISIN"),
                     "COOPER"=list("name"="TXNCODE"),
                     "DSFACTU"=list("name"="BOOKDATE"),
                     "DSOPER"=list("name"="TRADEDATE"),
                     "DSVAL"=list("name"="VALUEDATE"),
                     "CUTIT-1"=list("name"="PXCOST"),
                     "COMONL-ISO-OPER"=list("name"="TRADECURRENCY"),
                     "MT-WTAX"=list("name"="LUXTAX"),
                     "CH-FWT"=list("name"="SWISSTAX"),
                     "OTH-TAX"=list("name"="OTHRTAX"),
                     "MTBRUT-MONOP"=list("name"="GRSAMOUNT"),
                     "MTNET-INTERM-MON-CC"=list("name"="NETAMOUNT"),
                     "MTINT-MONOP"=list("name"="ACCRDINTEREST"),
                     "SIMONT-INT-MONOP"=list("name"="ACCRDINTERESTSIGN"),
                     "MTCTGE-MON-CC"=list("name"="BRKFEES"),
                     "SIMONT-CTGE-MON-CC"=list("name"="BRKFEESSIGN"),
                     "MTFCOR-MONOP"=list("name"="CRSPFEE"),
                     "SIMONT-FCOR-MONOP"=list("name"="CRSPFEESIGN"),
                     "MTNET-MON-CC"=list("name"="NETCASHAMOUNT"),
                     "SIMONT-NET-MON-CC"=list("name"="NETCASHAMOUNTSIGN"),
                     "QTFACTU-D5"=list("name"="QTYSECURITY")),
         "L002"=list("NOCNTNR-AVQ"=list("name"="CLIENTNO")),
         "L004"=list("NOCNTNR-AVQ"=list("name"="CLIENTNO")),
         "L003"=list("NOCNTNR-AVQ"=list("CLIENTNO"),
                     "NOCNTNR-AVQ"=list("AVALOQNO"),
                     ##"CORUB-CC"=list("ACCTYPE"),
                     "DSORD"=list("ORDERDATE"),
                     "DSCPTA"=list("BOOKDATE"),
                     "COCPTA-ESP"=list("CASHBOOKCODE"),
                     "COMONL-ISO"=list("CCY"),
                     "MTTRAN-ACPTA"=list("QTY"),
                     "SIMONT-TRAN-ACPTA"=list("QTYSIGN"),
                     "DSVAL"=list("SETTLEMENT"),
                     "CHTRAN"=list("FXRATE"),
                     "MTFTRAN-MON-CC"=list("TXNCOST1"),
                     "SIMONT-FTRAN-MON-CC"=list("TXNCOST1SIGN"),
                     "MTCOM-TRAN-MON-CC"=list("TXNCOST2"),
                     "SIMONT-COM-TRAN-MON-CC"=list("TXNCOST2SIGN"),
                     "MTFTRAN-MONOP"=list("TXNEXP1"),
                     "SIMONT-FTRAN-MONOP"=list("TXNEXP1SIGN"),
                     "MTCOM-TRAN-MONOP"=list("TXNEXP2"),
                     "SIMONT-COM-TRAN-MONOP"=list("TXNEXP2SIGN"),
                     "TELGN-1-TRAN"=list("DESC1"),
                     "TELGN-2-TRAN"=list("DESC2"),
                     "TELGN-3-TRAN"=list("DESC3"),
                     "TELGN-4-TRAN"=list("DESC4"),
                     "TELGN-5-TRAN"=list("DESC5"),
                     "TEREF-OPER-OP"=list("REFERENCE"),
                     "COOPER-MESP-ORG"=list("CASH MOVEMENT OPERATION CODE"),
                     "COOPER-MESP-TYP"=list("CASH MOVEMENT OPERATION TYPE CODE"),
                     "COOPER-MESP-TYP-COMPL"=list("CASH MOVEMENT COMPLMENTARY OPERATION TYPE CODE"),
                     "COTRANS-COLLECT"=list("Transfer between Collection (client) code."),
                     "COTRANS-BP"=list("Transfer between Business Partner (client) code."),
                     "COTRANS-CONT"=list("Transfer between Containers (portfolio) code."),
                     "COOPER-MESP"=list("TXNCODE")),
         "L006"=list("NOCNTNR-AVQ"=list("CLIENTNO"),
                     "NOCNTNR-AVQ"=list("AVALOQNO"),
                     ##"CORUB-CC"=list("ACCTYPE"),
                     "DSOPER"=list("TRADEDATE"),
                     "DSVAL"=list("SETTLEMENT"),
                     "M9MVT-ESP"=list("QTY"),
                     "SIMONT-MVT-ESP"=list("QTYSIGN"),
                     "COMONL-ISO-CC"=list("ACCCCY"),
                     "TEMVT-ESP"=list("DESC1"),
                     "COOPER-ESP-DALI"=list("ISPERFORMANCE"),
                     "M9TVA-MON-CC"=list("VATQTY"),
                     "SIMONT-TVA-MON-CC"=list("VATQTYSIGN"),
                     "M9IA-MON-CC"=list("WITHHOLDINGQTY"),
                     "SIMONT-IA-MONCC"=list("WITHHOLDINGQTYSIGN"),
                     "COOPER-MESP"=list("TXNCODE"),
                     "TEREF-OPER-OP"=list("REFERENCE"),
                     "NOVAL-ISIN"=list("ISIN"),
                     "MTFTRAN-MON-CC"=list("TXNQTY"),
                     "SIMONT-FTRANS-MON-CC"=list("TXNQTYSIGN")),
         "L007"=list("NOVAL-TLK-NOUV"=list("name"="TELEKURS"),
                     "TEREF-OPER-OP"=list("name"="REFERENCE"),
                     "OPE-TEFACTU-L73"=list("name"="TXNTEXT")))
}


##' A function to return the transaction code vs trade ctype mapping.
##'
##' This is the description
##'
##' @return Returns a vector with the mapping.
##' @export
pictetTXNCode <- function() {
    c("SPCC"="30",
      "SPLS"="30",
      "SPTF"="30",
      "ECR M2"="30",
      "SPESBA"="30",
      "VIFR"="20")
}


##' A function to return the pictet security type vs resource ctype mapping
##'
##' This is the description
##'
##' @return Returns a vector with the mapping.
##' @export
pictetSecTypeMap <- function() {
    list("HEDGE_FD"=list("ctype"="SHRE"),
         "ORD_SH"=list("ctype"="SHRE"),
         "MMKT_FD"=list("ctype"="SHRE"),
         "METCOM_FD"=list("ctype"="SHRE"),
         "SH_WRT"=list("ctype"="SHRE"),
         "DIV_R_CERT"=list("ctype"="SHRE"),
         "REINV_RIGHT"=list("ctype"="OTHER"),
         "IND_FUT"=list("ctype"="FUT"),
         "PRECMET_NOVAT"=list("ctype"="COMM"),
         "ABS_RET_FD"=list("ctype"="SHRE"),
         "SP_SH"=list("ctype"="SP"),
         "STR_BD"=list("ctype"="BOND"),
         "SHAR_BD"=list("ctype"="BOND"),
         "PRIV_SH"=list("ctype"="SHRE"),
         "BOND_FD"=list("ctype"="SHRE"),
         "SHARE_FD"=list("ctype"="SHRE"),
         "FIXT_GAR"=list("ctype"="OTHER"),
         "T_BOND"=list("ctype"="BOND"),
         "PERP_BD"=list("ctype"="BOND"),
         "IND_BD"=list("ctype"="BOND"),
         "SP_BD"=list("ctype"="BOND"),
         "FIXT_ADV"=list("ctype"="LOAN"),
         "SP_MISC"=list("ctype"="SP"),
         "SEALED_ENV"=list("ctype"="OTHER"),
         "INTR_OPT"=list("ctype"="OPT"),
         "ALLOC_FD"=list("ctype"="SHRE"),
         "FUND_FD"=list("ctype"="SHRE"),
         "CONV_BD"=list("ctype"="BOND"),
         "MET_FUT"=list("ctype"="FUT"),
         "COM_FUT"=list("ctype"="FUT"),
         "SP_HYBR"=list("ctype"="SP"),
         "FXOTC_OPT"=list("ctype"="OPT"),
         "IND_OPT"=list("ctype"="OPT"))
}
