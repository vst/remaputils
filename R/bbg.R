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
##' @return A vector with bloomberg tickers.
##' @export
requestableTickers <- function(session, date, zero, underlying, fldsByCtype) {

    ## Prepare the params for the stock request:
    params <- list(page_size=-1, format="csv", date=date, zero=zero)

    ## Get the stocks:
    stocks <- as.data.frame(rdecaf::getResource("stocks", params=params, session=session))

    ## Get the resources by stocks:
    resources <- remaputils::getResourcesByStock(stocks, session, getUnderlying=TRUE)

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
    as.character(resources[missingBaseInfo | missingSpecificInfo, "priceIds"])

}
