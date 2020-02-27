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
         "402"=list("ctype"="SHRE",
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
    list(##"ACCDATE"="POSDATE",
         ##"LASTACCDATE"="LASTACCDATE",
         "LASTACCDATE"="POSDATE",
         "ACCDATE"="LASTACCDATE",
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
         "PVAL_SECURITY_IDTIS"="INTERNALREF",
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
         "O__FLAGEXTI"="REVERSAL",
         "O__CDOPS"="TXNCODE",
         "D__DEVCPTEXTS"="ACCFXCODE",
         "D__DEVCPTS"="TXNCCY",
         "D__DEVOPS"="ACCTYPE",
         "D__DEVOPEXTS"="TXNCCYCODE",
         "D__XRATETRND"="SECACCFX",
         "D__MTFRSD"="FEEAMOUNT",
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
    data[, "FREQ"] <- sapply(data[, "FREQ"], function(f) switch(as.character(f), "yearly"=1, "half-yearly"=2, "monthly"=12, "quarterly"=4, "NA"=NA))

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

    if (safeGrep(file, "SEC_V001") == "1") {
        fileType <- "SECV001"
    }

    dataByAccount <- extractToList(data, "ACCOUNT")

    types <- c("liquidity", "forex", "security", "shortterm")

    data <- do.call(rbind, lapply(dataByAccount, function(x) {
        missing <- types[is.na(match(types, unique(as.character(x[, "SUBTYPE"]))))]
        if (length(missing) > 0) {
            add <- initDF(colnames(x), length(missing))
            add[, "SUBTYPE"] <- missing
            add[, "accmain"] <- x[1, "accmain"]
            add[, "POSDATE"] <- x[1, "POSDATE"]
            add[, "ACCOUNT"] <- x[1, "ACCOUNT"]
            add[, "QTY"] <- 0
            add[, "SECDESC1"] <- "MASKED"
            x <- rbind(x, add)
        }
        return(x)
    }))

    ## Done, return:
    return(list("data"=data,
                "type"=fileType))

}


ansbacherSecurityResourceMapper <- function(data, accounts, resources, session, ...) {
    ## Run the resource preemble:
    result <- ansbacherResourcePreemble(data, accounts, resources, session)
}


ansbacherLiquidityResourceMapper <- function(data, accounts, resources, session, ...) {
    ## Get the resmains:
    data[, "resmain"] <- resources[match(as.character(data[, "SECCCY"]), resources[, "symbol"]), "id"]

    ## Create resource if NA:
    if (any(is.na(data[, "resmain"]))) {

        ## Get the currencies to be created:
        ccys <- as.character(data[is.na(data[, "SECCCY"]), "SECCCY"])

        ## Post and get return value:
        retval <- createCashResource(data.frame("symbol"=ccys), session)

        ## Assing the created currencies' resmain:
        data[is.na(data[, "resmain"]), "resmain"] <- sapply(retval, function(x) x$id)
    }

}


ansbacherForexResourceMapper <- function(data, accounts, resources, session, ...) {
    ## Construct the guid:
    guid <- paste0("ANSBACHER:", data[, "ACCOUNT"], data[, "BOUGHTCCY"], data[, "SOLDCCY"], data[, "FWDRATE"], data[, "MATURITY2"])
    guid <- as.character(sapply(guid, digest))

    ## Interpret the main ccy:
    mainCCY <- apply(data[, c("BOUGHTCCY", "SOLDCCY", "SECCCY")], MARGIN=1, function(x) x[which(x[3] != x[1:2])])

    ## Interpret the altn ccy:
    altnCCY <- apply(data[, c("BOUGHTCCY", "SOLDCCY", "SECCCY")], MARGIN=1, function(x) x[which(x[3] == x[1:2])])

    ## Interpret the main ccy:
    mainQTYIdx <- apply(data[, c("BOUGHTCCY", "SOLDCCY", "SECCCY")], MARGIN=1, function(x) which(x[3] != x[1:2]))

    ## Interpret the main ccy:
    altnQTYIdx <- apply(data[, c("BOUGHTCCY", "SOLDCCY", "SECCCY")], MARGIN=1, function(x) which(x[3] == x[1:2]))

    ## TODO:
    mainQTY <- as.numeric(sapply(1:NROW(data), function(i) data[i, c("BOUGHTQTY", "SOLDQTY")][mainQTYIdx[i]])) * ifelse(mainQTYIdx == 2, -1, 1)

    ## TODO:
    altnQTY <- as.numeric(sapply(1:NROW(data), function(i) data[i, c("BOUGHTQTY", "SOLDQTY")][altnQTYIdx[i]])) * ifelse(altnQTYIdx == 2, -1, 1)

    ## Match and get resmain:
    data[, "resmain"] <- resources[match(guid, resources[, "guid"]), "id"]

    ## Create resource if missing:
    if (any(is.na(data[, "resmain"]))) {

        ## NA resmains:
        naResmain <- is.na(data[, "resmain"])

        ## The data frame:
        df <- data.frame("ccymain"=mainCCY[naResmain],
                         "ccyaltn"=altnCCY[naResmain],
                         "settlement"=as.character(data[naResmain, "MATURITY2"]),
                         "launch"=NA,
                         "pxmain"=data[naResmain, "FWDRATE"],
                         "isFlip"=FALSE,
                         "guid"=guid[naResmain],
                         stringsAsFactors=FALSE)

        ## Create the FX Forward:
        retval <- createFXFwdResource(df, session)

        ## Assign the resulting resource id:
        data[naResmain, "resmain"] <- sapply(retval, function(x) x$id)
        resources <- getSystemResources(session)
    }
}



## This the security master mapper:
ansbacherResourcePreemble <- function(records, accounts, resources, session, ...) {

    ## If resources is empty, create USD:
    if (NROW(resources) == 0) {
        createCashResource(data.frame("symbol"="USD"), session)
        resources <- getSystemResources(session)
    }

    ## Match the resmains and append:
    records[, "symbol"] <- paste(records[, "ISIN"], records[, "SECCCY"], "[ABACHER]")

    ## Match the resmains and append:
    records[, "resmain"] <- resources[match(records[, "symbol"], resources[, "symbol"], incomparables=NA), "id"]

    ## If all resmains exist, return NULL:
    if (!any(is.na(records[, "resmain"]))) {
        return(list("records"=records, "resources"=resources))
    }

    origRecords <- records

    records <- records[is.na(records[, "resmain"]), ]

    ## Get the ctype:
    records[, "ctype"] <- sapply(match(records[, "SECTYPE"], names(ansbacherSecTypeMapper())), function(i) ansbacherSecTypeMapper()[[i]][["ctype"]])

    ## Get the stype:
    records[, "stype"] <- sapply(match(records[, "SECTYPE"], names(ansbacherSecTypeMapper())), function(i) ansbacherSecTypeMapper()[[i]][["stype"]])

    ## Get the records:
    records[, "ISIN"] <- gsub(" ", "-", records[,"ISIN"])

    ## Initialise columns:
    records[, c("ticker", "figi")] = NA

    records[, "CONTRACTSIZE"] <- 1

    ## For QUTETYPE C use 0.01:
    records[records[, "QUOTETYPE"] == "nominal", "CONTRACTSIZE"] <- 0.01

    ## Run the figi:
    figiResult <-  figi(records, idType="ID_ISIN", fld="ISIN", ccy="SECCCY", "d49bdbc7-7b61-4791-bf67-7a543af1b5ab")

    ## Match the figi result identifier with the records identifier:
    matchIdx <- match(paste0(records[, "ISIN"], records[, "SECCCY"]), paste0(figiResult[, "idValue"], figiResult[, "currency"]))

    ## Append the symbol from figi result to records:
    records[, "ticker"] <- safeNull(figiResult[matchIdx, "symbol"])

    ## Get the symbol:
    records[, "symbol"] <- paste(records[, "ISIN"], records[, "SECCCY"], "[ABACHER]")

    ## Match the resmains and append:
    records[, "resmain"] <- resources[match(records[, "symbol"], resources[, "symbol"], incomparables=NA), "id"]

    ## If all resmains exist, return NULL:
    if (!any(is.na(records[, "resmain"]))) {
        return(list("records"=records, "resources"=resources))
    }

    naResmain <- is.na(records[, "resmain"])
    ## origRecords <- records

    ## Get the records with NA resmain:
    records <- records[is.na(records[, "resmain"]), ]

    ## Treat the country information:
    country <- dbRemapCountryTreater(as.character(records[, "COUNTRY"]), "countrymaps")

    ## Matp the daycount convention:
    convday <- "Act/Act"

    ## Infer the interest payment frequency:
    firstCpnDate <- as.Date(as.character(records[, "POSDATE"])) - records[, "ACCRDAYS"]
    issueDate <- as.character(records[, "ISSUEDATE"])
    freq <- records[, "FREQ"]

    records[, "quantity"] <- records[, "CONTRACTSIZE"]

    ## Combine the existing and new resources:
    abacherResources  <- data.frame("name"=records[, "SECDESC2"],
                                    "resmain"=records[, "resmain"],
                                    "symbol"=records[, "symbol"],
                                    "isin"=records[, "ISIN"],
                                    "ticker"=records[, "ticker"],
                                    "telekurs"=records[, "TELEKURS"],
                                    "figi"=records[, "figi"],
                                    "stype"=records[, "stype"],
                                    "reference"=records[, "INTERNALREF"],
                                    "cusip"=safeColumn(records, "CUSIP"),
                                    "ctype"=records[, "ctype"],
                                    "ccymain"=records[, "SECCCY"],
                                    "pxmain"=records[, "INTEREST"],
                                    "expiry"=ifelse(isNAorEmpty(as.character(records[, "MATURITY"])), NA, as.character(records[, "MATURITY"])),
                                    "issued"=ifelse(isNAorEmpty(as.character(issueDate)), NA, as.character(issueDate)),
                                    "launch"=ifelse(isNAorEmpty(as.character(issueDate)), NA, as.character(issueDate)),
                                    "callput"=NA,
                                    "quantity"=records[, "CONTRACTSIZE"],
                                    "convday"=convday,
                                    "frequency"=freq,
                                    "description"=NA, #
                                    "country"=country,
                                    stringsAsFactors=FALSE)

    ## Set 0 prics to 1:
    abacherResources[, "pxmain"] <- ifelse(abacherResources[, "pxmain"] == 0, NA, abacherResources[, "pxmain"])

    if (any(is.na(abacherResources[, "resmain"]))) {
        ## Create missing resources if necessary:
        abacherResources <- pictetCreateResources(abacherResources, session)
    }

    return(list("records"=origRecords, "resources"=getSystemResources(session)))
}


ansbacherShorttermResourceMapper <- function(data, accounts, resources, session, ...) {

    ## Construct the guid:
    guid <- paste0("ANSBACHER:",
                   data[, "ACCOUNT"],
                   data[, "SECCCY"],
                   data[, "SHRTRM_ISSUEDT"],
                   data[, "MATURITY2"],
                   data[, "SHRTRM_INTEREST"])

    guid <- as.character(sapply(guid, digest))

    ## Match and get resmain:
    data[, "resmain"] <- resources[match(guid, resources[, "guid"]), "id"]

    ## Create resource if missing:
    if (any(is.na(data[, "resmain"]))) {

        ## NA resmains:
        naResmain <- is.na(data[, "resmain"])

        ## The data frame:
        df <- data.frame("ccymain"=data[naResmain, "SECCCY"],
                         "ctype"="DEPO",
                         "stype"="Short-Term",
                         "id"=NA,
                         "expiry"=as.character(data[naResmain, "MATURITY2"]),
                         "launch"=as.character(data[naResmain, "SHRTRM_ISSUEDT"]),
                         "pxmain"=data[naResmain, "SHRTRM_INTEREST"],
                         "convday"="Act/Act",
                         "guid"=guid[naResmain],
                         stringsAsFactors=FALSE)

        ## Prepare the payload;
        payload <- toJSON(list(artifacts=df), auto_unbox=TRUE, na="null", digits=10)

        ## Post and get the response:
        response <- try(postResource("imports/inbulk", payload=payload, params=list(sync="True"), session = session), silent = TRUE)

    }

    return(NULL)

}


ansbacherShorttermMapper <- function(data, accounts, resources, session, ...) {

    maskedData <- data[data[, "SECDESC1"] == "MASKED", ]
    data <- data[data[, "SECDESC1"] != "MASKED", ]

    if (NROW(data) == 0) {

        maskedData[, "resmain"] <- NA
        pWisePos <- extractToList(maskedData, "accmain")
        stocks <- do.call(rbind, lapply(pWisePos, function(p) XXgetPositionComparison(p, resources, maskedData[1, "POSDATE"], "Time Deposit", session)))
        stocks[is.na(stocks[, "PX Last"]), "PX Last"] <- 1
        stocks[is.na(stocks[, "pxext"]), "pxext"] <- 1
        ## Push the differences:
        pushPosDifferences(stocks, "Ansbacher", session)
        return(NULL)
    }

    ## Construct the guid:
    guid <- paste0("ANSBACHER:",
                   data[, "ACCOUNT"],
                   data[, "SECCCY"],
                   data[, "SHRTRM_ISSUEDT"],
                   data[, "MATURITY2"],
                   data[, "SHRTRM_INTEREST"])

    guid <- as.character(sapply(guid, digest))

    ## Match and get resmain:
    data[, "resmain"] <- resources[match(guid, resources[, "guid"]), "id"]



    ## Create resource if missing:
    if (any(is.na(data[, "resmain"]))) {

        ## NA resmains:
        naResmain <- is.na(data[, "resmain"])

        ## The data frame:
        df <- data.frame("ccymain"=data[naResmain, "SECCCY"],
                         "ctype"="DEPO",
                         "stype"="Short-Term",
                         "id"=NA,
                         "expiry"=as.character(data[naResmain, "MATURITY2"]),
                         "launch"=as.character(data[naResmain, "SHRTRM_ISSUEDT"]),
                         "pxmain"=data[naResmain, "SHRTRM_INTEREST"],
                         "convday"="Act/Act",
                         "guid"=guid[naResmain],
                         stringsAsFactors=FALSE)

        ## Prepare the payload;
        payload <- toJSON(list(artifacts=df), auto_unbox=TRUE, na="null", digits=10)

        ## Post and get the response:
        response <- try(postResource("imports/inbulk", payload=payload, params=list(sync="True"), session = session), silent = TRUE)

        ## Assign the resulting resource id:
        data[naResmain, "resmain"] <- sapply(response[[1]][["artifacts"]], function(x) x[[1]])
        resources <- getSystemResources(session)
    }


    data[, "QTY"] <- data[, "VALUESECCCY"]
    data[, "PXLAST"] <- 1
    data[, "PXCOST"] <- 1

    maskedData[, colnames(data)[is.na(match(colnames(data), colnames(maskedData)))]] <- NA
    data <- rbind(data, maskedData)

    ## Get the account wise positions:
    pWisePos <- extractToList(data, "accmain")

    stocks <- do.call(rbind, lapply(pWisePos, function(p) XXgetPositionComparison(p, resources, data[1, "POSDATE"], "Time Deposit", session)))

    stocks[is.na(stocks[, "PX Last"]), "PX Last"] <- 1
    stocks[is.na(stocks[, "pxext"]), "pxext"] <- 1

    ## Push the differences:
    pushPosDifferences(stocks, "Ansbacher", session)

    return(NULL)

}


ansbacherExpirefwdMapper <- function(data, accounts, resources, session, ...) {

    ## Get the active stocks for containers:
    stocks <- getStocksFromContainerNames(session, "accounts", unique(data[, "ACCOUNT"]), zero=0, date=data[1, "POSDATE"])

    ## Get the resources:
    resources <- getResourcesByStock(stocks, session)

    ## Append the ctype:
    stocks[, "ctype"] <- resources[match(stocks[, "artifact"], resources[, "id"], incomparables=NA), "ctype"]

    ## Append the expiry:
    stocks[, "expiry"] <- resources[match(stocks[, "artifact"], resources[, "id"], incomparables=NA), "expiry"]

    ## Append the expiry:
    stocks[, "symbol"] <- resources[match(stocks[, "artifact"], resources[, "id"], incomparables=NA), "symbol"]

    ## Which index are the expired forwards:
    expiredFwd <- stocks[, "ctype"] == "FXFWD" & (stocks[, "expiry"] < as.character(data[1, "POSDATE"]))

    ## If no expired forwards, return NULL:
    if (all(!expiredFwd)) {
        return(NULL)
    }

    ## Get the expired forwards:
    expiredFwds <- stocks[expiredFwd, ]

    expiredFwds[, "pxmain"] <- 1 + sapply(expiredFwds[, "symbol"], function(x) head(getOhlcObsForSymbol(session, x, lte=as.Date(as.character(data[1, "POSDATE"])), lookBack=30), 1)[, "close"])

    ##
    df <- data.frame("accmain"=expiredFwds[, "account"],
                     "resmain"=expiredFwds[, "artifact"],
                     "ctype"="20",
                     "commitment"=expiredFwds[, "expiry"],
                     "qtymain"=-expiredFwds[, "quantity"],
                     "pxmain"=expiredFwds[, "pxmain"])

    payload <- toJSON(list(actions=df), auto_unbox = TRUE, na = "null", digits = 10)
    print(paste0("Posting expired forward trades: ", NROW(df)))
    response <- postResource("imports/inbulk", params = list(sync = "True"), payload = payload, session = session)
    return("Successfully pushed new increments")

}


ansbacherSecurityMapper <- function(data, accounts, resources, session, ...) {

    maskedData <- data[data[, "SECDESC1"] == "MASKED", ]
    data <- data[data[, "SECDESC1"] != "MASKED", ]

    ## Run the resource preemble:
    result <- ansbacherResourcePreemble(data, accounts, resources, session)

    data <- result[["records"]]
    data[, "POSDATE"] <- as.character(data[, "POSDATE"])
    data[, "SECPXDATE"] <- as.character(data[, "SECPXDATE"])

    if (NROW(maskedData) == 0) {
        maskedData <- NULL
    }

    ## Get the records:
    data <- safeRbind(list(data, maskedData))

    ## Get the resources:
    resources <- result[["resources"]]

    ## Assign the resmains:
    data[, "resmain"] <- resources[match(data[, "symbol"], resources[, "symbol"]), "id"]

    ## Push the OHLC:
    pushOhlc(data[, "symbol"], data[, "PXLAST"], as.character(data[, "SECPXDATE"]), session)

    ## Get the position date:
    pDate <- as.character(data[1, "POSDATE"])

    ## Get the account wise positions:
    pWisePos <- extractToList(data, "accmain")

    ## Get the stocks comparisons between decaf and ansbacher:
    stocks <- do.call(rbind, lapply(pWisePos, function(pPos) XXgetPositionComparison(pPos,
                                                                                    resources,
                                                                                    pDate,
                                                                                    "Security",
                                                                                    session)))


    stocks[is.na(stocks[, "PX Last"]), "PX Last"] <- 1
    stocks[is.na(stocks[, "pxext"]), "pxext"] <- 1

    ## Push the differences:
    pushPosDifferences(stocks, "Ansbacher", session)

}


ansbacherLiquidityMapper <- function(data, accounts, resources, session, ...) {

    ## Get the resmains:
    data[, "resmain"] <- resources[match(as.character(data[, "SECCCY"]), resources[, "symbol"]), "id"]

    ## Create resource if NA:
    if (any(is.na(data[, "resmain"]) & data[, "SECDESC1"] != "MASKED")) {

        ## Get the currencies to be created:
        ccys <- as.character(data[is.na(data[, "SECCCY"]), "SECCCY"])

        ## Post and get return value:
        retval <- createCashResource(data.frame("symbol"=ccys), session)

        ## Assing the created currencies' resmain:
        data[is.na(data[, "resmain"]), "resmain"] <- sapply(retval, function(x) x$id)
    }

    ## Assign the cash quantity to QTY:
    data[, "QTY"] <- data[, "QTYCASH"]

    ## Get the position date:
    pDate <- as.character(data[1, "POSDATE"])

    ## Get the account wise positions:
    pWisePos <- extractToList(data, "accmain")

    ## Get the stocks comparisons between decaf and ansbacher:
    stocks <- do.call(rbind, lapply(pWisePos, function(pPos) XXgetPositionComparison(pPos,
                                                                                    resources,
                                                                                    pDate,
                                                                                    "Cash",
                                                                                    session
                                                                                    )))

    ## Push the differences:
    pushPosDifferences(stocks, "Ansbacher", session)

}





ansbacherForexMapper <- function(data, accounts, resources, session, ...) {

    maskedData <- data[data[, "SECDESC1"] == "MASKED", ]
    data <- data[data[, "SECDESC1"] != "MASKED", ]

    if (NROW(data) == 0) {
        maskedData[, "resmain"] <- NA
        pWisePos <- extractToList(maskedData, "accmain")
        stocks <- do.call(rbind, lapply(pWisePos, function(p) XXgetPositionComparison(p, resources, maskedData[1, "POSDATE"], "FX Forward", session)))
        stocks[is.na(stocks[, "PX Last"]), "PX Last"] <- 1
        stocks[is.na(stocks[, "pxext"]), "pxext"] <- 1
        ## Push the differences:
        pushPosDifferences(stocks, "Ansbacher", session)
        return(NULL)
    }


    ## Construct the guid:
    guid <- paste0("ANSBACHER:", data[, "ACCOUNT"], data[, "BOUGHTCCY"], data[, "SOLDCCY"], data[, "FWDRATE"], data[, "MATURITY2"])
    guid <- as.character(sapply(guid, digest))

    ## Interpret the main ccy:
    mainCCY <- apply(data[, c("BOUGHTCCY", "SOLDCCY", "SECCCY")], MARGIN=1, function(x) {
        x[which(x[3] != x[1:2])]
    })

    ## Interpret the altn ccy:
    altnCCY <- apply(data[, c("BOUGHTCCY", "SOLDCCY", "SECCCY")], MARGIN=1, function(x) {
        x[which(x[3] == x[1:2])]
    })

    ## Interpret the main ccy:
    mainQTYIdx <- apply(data[, c("BOUGHTCCY", "SOLDCCY", "SECCCY")], MARGIN=1, function(x) {
        which(x[3] != x[1:2])
    })

    ## Interpret the main ccy:
    altnQTYIdx <- apply(data[, c("BOUGHTCCY", "SOLDCCY", "SECCCY")], MARGIN=1, function(x) {
        which(x[3] == x[1:2])
    })

    ## TODO:
    mainQTY <- as.numeric(sapply(1:NROW(data), function(i) data[i, c("BOUGHTQTY", "SOLDQTY")][mainQTYIdx[i]])) * ifelse(mainQTYIdx == 2, -1, 1)

    ## TODO:
    altnQTY <- as.numeric(sapply(1:NROW(data), function(i) data[i, c("BOUGHTQTY", "SOLDQTY")][altnQTYIdx[i]])) * ifelse(altnQTYIdx == 2, -1, 1)

    ## Match and get resmain:
    data[, "resmain"] <- resources[match(guid, resources[, "guid"]), "id"]

    ## Create resource if missing:
    if (any(is.na(data[, "resmain"]))) {

        ## NA resmains:
        naResmain <- is.na(data[, "resmain"])

        ## The data frame:
        df <- data.frame("ccymain"=mainCCY[naResmain],
                         "ccyaltn"=altnCCY[naResmain],
                         "settlement"=as.character(data[naResmain, "MATURITY2"]),
                         "launch"=NA,
                         "pxmain"=data[naResmain, "FWDRATE"],
                         "isFlip"=FALSE,
                         "guid"=guid[naResmain],
                         stringsAsFactors=FALSE)

        ## Create the FX Forward:
        retval <- createFXFwdResource(df, session)

        ## Assign the resulting resource id:
        data[naResmain, "resmain"] <- sapply(retval, function(x) x$id)
        resources <- getSystemResources(session)
    }

    data[, "symbol"] <- resources[match(data[, "resmain"], resources[, "id"]), "symbol"]
    data[, "isFlip"] <- resources[match(data[, "resmain"], resources[, "id"]), "pxflip"] == "True"

    data[, "QTY"] <- mainQTY
    data[, "PXLAST"] <- data[, "CURRENTFWDRATE"]
    data[, "PXCOST"] <- 1

    close <- as.numeric(data[, "CURRENTFWDRATE"])

    ## Push the OHLC:
    pushOhlc(data[, "symbol"],  close, as.character(data[, "POSDATE"]), session)

    maskedData[, colnames(data)[is.na(match(colnames(data), colnames(maskedData)))]] <- NA
    data <- rbind(data, maskedData)

    ## Get the account wise positions:
    pWisePos <- extractToList(data, "accmain")

    ## Get the stocks comparisons between decaf and ansbacher:
    stocks <- do.call(rbind, lapply(pWisePos, function(pPos) XXgetPositionComparison(pPos,
                                                                                    resources,
                                                                                    data[1, "POSDATE"],
                                                                                    "FX Forward",
                                                                                    session)))

    stocks[is.na(stocks[, "PX Last"]), "PX Last"] <- 1
    stocks[is.na(stocks[, "pxext"]), "pxext"] <- 1

    ## Push the differences:
    pushPosDifferences(stocks, "Ansbacher", session)

}



ansbacherCashTXN <- function(data, res, session) {

    ## Create the pure cash transaction guid's:
    guid <- apply(data[, c("OPRCODE", "NETAMOUNTSEC", "BOOKDATE", "ACCOUNT", "TXNCCY")], MARGIN=1, function(x) digest(paste0("ORD", trimws(x))))

    ## Prepare the data frame:
    records <- data.frame("ctype"="20",
                          "accmain"=data[, "accmain"],
                          "pxmain"=1,
                          "resmain"=res[match(data[, "TXNCCY"], res[, "symbol"]), "id"],
                          "commitment"=data[, "EXEDATE"],
                          "qtymain"=round(data[, "NETAMOUNTSEC"], 2),
                          "extfld1tag"="Trade: ",
                          "extfld1val"=NA,
                          "extfld2tag"="Type:",
                          "extfld2val"="ORDINARY CASH",
                          "extfld3tag"="Resource:",
                          "extfld3val"=NA,
                          "notes"=paste0(data[, "TXNTYPE"], ": ", data[, "TXNDESC"]),
                          "reference"=data[, "OPRCODE"],
                          "guid"=guid,
                          stringsAsFactors=FALSE)

    ## Exclude the 0 qtymain's:
    records <- records[records[, "qtymain"] != 0, ]

    ## Prepare the payload;
    payload <- toJSON(list(actions=records), auto_unbox=TRUE, na="null", digits=10)

    ## Post and get the response:
    response <- try(postResource("imports/inbulk", payload=payload, params=list(sync="True"), session = session), silent = TRUE)


}


ansbacherCreateOtherResource <- function(data, session) {

    ## If any NA resmains, create OTHER resource:
    if (any(is.na(data[, "mainres"]))) {

        ## TODO:
        nonRes <- data[, "APPCODE"] == "COU"

        ## Get the na resmain index:
        naRes <- is.na(data[, "mainres"]) & !nonRes

        ## Construct the prefix for the symbol.
        prefix <- sapply(strsplit(as.character(data[naRes, "SECIDENT"]), "\\."), function(x) x[1])

        ## Construct the symbol:
        symbol <- paste(prefix, data[naRes, "TXNCCY"], "[ABACHER]")

        ## Get the price factor for the resource:
        pxfactor <- data[naRes, "pxfactor"]

        ## Get the ccymain for the resource:
        ccymain <-data[naRes, "TXNCCY"]

        ## Construct the data frame:
        df <- data.frame("symbol"=symbol,
                         "isin"=ifelse(isIsin(prefix), prefix, NA),
                         "reference"=data[naRes, "SECID"],
                         "name"=data[naRes, "SECDESC1"],
                         "pxfactor"=ifelse(pxfactor == 0, 1, pxfactor),
                         "ccymain"=ifelse(ccymain == "NO", "USD", ccymain))

        ## Create the resource as Other:
        result <- createOtherResource(df, session)
        data[naRes, "mainres"] <- sapply(result, function(x) x$id)

        naRes <- is.na(data[, "mainres"])
        data[naRes, "mainres"] <- data[!naRes, ][match(data[naRes, "SECID"], data[!naRes, "SECID"]), "mainres"]
    }

    return(data)
}
