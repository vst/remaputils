##' R wrapper around bdlhs.
##'
##' @param host Remote SFTP server hostname.
##' @param port Remote SFTP server port.
##' @param user Remote SFTP username.
##' @param pass Remote SFTP password.
##' @param flds The fields.
##' @param tcks The tickers.
##' @param exe Path to or name of `bdlhs` executable.
##' @return A data frame
##' @export
bdlhs <- function (host, port, user, pass, flds, tcks, exe="bdlhs") {
    ## Check if the bdlhs executable is installed:
    if (Sys.which(exe) == "") {
        stop("bdlhs executable is not found.")
    }

    ## Define the arguments:
    args <- list("--host"=host, "--port"=port, "--user"=user, "--pass"=pass, "--flds"=paste(flds, collapse=" "))

    ## Prepare arguments:
    args <- as.character(sapply(names(args), function (x) paste0(x, "=\"", args[[x]], "\"")))

    ## Run the application:
    resdata <- system2(exe, args=args, stdout=TRUE, input=paste(tcks, collapse="\n"))

    ## Read the output and return:
    read.csv(text=resdata, sep="|")
}


##' Function to prepare ticker for request.
##'
##' @param session The decaf session.
##' @param date The date to query stocks.
##' @param zero Should closed stocks be included? 1 = yes, - = no.
##' @param underlying Should underlying instruments be included?
##' @param fldsByCtype The list with fields to be considered for each ctype.
##' @param other Shall OTHER assets be considered as well?
##' @return A list with a) tickers for price, b) tickers for reference, c) the resource data frame
##' @export
requestableTickers <- function(session, date, zero, underlying, fldsByCtype, other=FALSE) {

    ## Prepare the params for the stock request:
    params <- list(page_size=-1, format="csv", date=date, zero=zero)

    ## Get the stocks:
    stocks <- as.data.frame(rdecaf::getResource("stocks", params=params, session=session))

    ## Get the resources by stocks:
    resources <- remaputils::getResourcesByStock(stocks, session, getUnderlying=TRUE)

    ## Consider other assets if required:
    if (other) {
        othersRes <- as.data.frame(rdecaf::getResource("resources", params=list(page_size=-1, format="csv", ctype="OTHER"), session=session))
        othersRes <- othersRes[apply(mgrep(othersRes[, c("symbol")], c("Index", "Curncy")), MARGIN=1, function(x) any(x!="0")), ]
        expired <- othersRes[, "expiry"] < Sys.Date() - 1
        expired <- ifelse(is.na(expired), FALSE, expired)
        resources <- safeRbind(list(resources, othersRes[!expired, ]))
    }

    ## Get ohlc identifiers:
    resources <- data.frame(resources, "priceIds"=ifelse(!is.na(resources[, "ohlccode"]), resources[, "ohlccode"], resources[, "symbol"]))

    ## Check if identifer is bbg ticker:
    resources <- resources[isBBGTicker(as.character(resources[, "priceIds"])), ]

    ## Check if all instruments have base field information:
    baseInfo <- resources[, fldsByCtype[["BASE"]]]

    ## Check which ones have missing base information:
    missingBaseInfo <- apply(baseInfo, MARGIN=1, function(x) any(is.na(x)))

    ## Exclude the base field information:
    fldsByCtype <- fldsByCtype[!names(fldsByCtype) == "BASE"]

    ## Check for missing instrument-specific information:
    missingSpecificInfo <- apply(do.call(cbind, lapply(1:length(fldsByCtype), function(i) {

        isCtype <- resources[, "ctype"] == names(fldsByCtype)[i]
        isMissing <- apply(resources[, fldsByCtype[[i]]], MARGIN=1, function(x) any(is.na(x)))

        isCtype & isMissing
    })), MARGIN=1, any)

    ## Return ohlc id's with missing information:
    list("reference"=as.character(resources[missingBaseInfo | missingSpecificInfo, "priceIds"]),
         "prices"=as.character(resources[, "priceIds"]),
         "resources"=resources)

}


##' A list with the mapping between bbg and decaf fields
##'
##' @return A list
##' @export
.bbgDecafFieldMap <- list("BASE"=c("ID_ISIN"="isin",
                                  "COUNTRY_FULL_NAME"="country",
                                  "ISSUER"="issuer",
                                  "INDUSTRY_SECTOR"="sector",
                                  "RTG_SP_LT_LC_ISSUER_CREDIT"="sp_rating",
                                  "RTG_SP_OUTLOOK"="sp_outlook",
                                  "PRIMARY_EXCHANGE_NAME"="mic"),
                         "BOND"=c("MATURITY"="expiry",
                                  "CPN"="pxmain",
                                  "FIRST_CPN_DT"="launch",
                                  "CPN_FREQ"="frequency"),
                         "OPT" =c("LAST_TRADEABLE_DT"="expiry", "OPT_STRIKE_PX"="pxmain"),
                         "FUT" =c("LAST_TRADEABLE_DT"="last_tradable", "FUT_NOTICE_FIRST"="first_notice"))


##' A list with the tranform functions for bbg results by field.
##'
##' @return A list
##' @export
.transBBGResults <- list("LAST_TRADEABLE_DT"=function(x) {as.Date(x, format="%Y%m%d")},
                        "FUT_NOTICE_FIRST"=function(x) {as.Date(x, format="%Y%m%d")},
                        "MATURITY"=function(x) {as.Date(x, format="%Y%m%d")},
                        "ID_ISIN"=function(x) {as.character(x)},
                        "COUNTRY_FULL_NAME"=function(x){as.character(x)},
                        "ISSUER"=function(x){as.character(x)},
                        "INDUSTRY_SECTOR"=function(x){as.character(x)},
                        "RTG_SP_LT_LC_ISSUER_CREDIT"=function(x){as.character(x)},
                        "RTG_SP_OUTLOOK"=function(x){as.character(x)},
                        "PRIMARY_EXCHANGE_NAME"=function(x){as.character(x)},
                        "CPN"=function(x){as.numeric(x)},
                        "FIRST_CPN_DT"=function(x) {as.Date(x, format="%Y%m%d")},
                        "CPN_FREQ"=function(x) {as.integer(x)},
                        "OPT_STRIKE_PX"=function(x) {as.numeric(x)})
