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
##' @param zero Should closed stocks be included? 1 = yes, - = no.
##' @param date The date for the stocks.
##' @param underlying Should underlying instruments be included?
##' @param fldsByCtype The list with fields to be considered for each ctype.
##' @param other Shall OTHER assets be considered as well?
##' @param field The field which should be considered in decaf resources.
##' @param ohlc Should the ohlccode be considered as an id.
##' @return A list with a) tickers for price, b) tickers for reference, c) the resource data frame
##' @export
prepareBBGDLData <- function(session, zero=Sys.Date(), date, underlying, fldsByCtype, other=FALSE, field="symbol", ohlc=TRUE) {

    ## Prepare the params for the stock request:
    params <- list(page_size=-1, format="csv", date=date, zero=zero)

    ## Get the stocks:
    stocks <- as.data.frame(rdecaf::getResource("stocks", params=params, session=session))

    ## Get the resources by stocks:
    resources <- remaputils::getResourcesByStock(stocks, session, getUnderlying=underlying)

    ## Consider other assets if required:
    if (other) {

        ## Get OTHER resources:
        othersRes <- as.data.frame(rdecaf::getResource("resources", params=list(page_size=-1, format="csv", ctype="OTHER"), session=session))

        ## Grep Index and Curncy suffices:
        othersRes <- othersRes[apply(mgrep(othersRes[, field], c("Index", "Curncy")), MARGIN=1, function(x) any(x!="0")), ]

        ## Only consider OTHER resources which are still alive:
        expired <- othersRes[, "expiry"] < Sys.Date() - 1

        ## Only consider OTHER resources which are still alive:
        expired <- ifelse(is.na(expired), FALSE, expired)

        if (NROW(othersRes) > 0) {
            ## Append OTHER ctypes to the resources:
            resources <- safeRbind(list(resources, othersRes[!expired, ]))
        }
    }

    if (ohlc) {
        ## Get ohlc identifiers:
        resources <- data.frame(resources, "priceIds"=ifelse(!is.na(resources[, "ohlccode"]), resources[, "ohlccode"], resources[, field]))
    } else {
        resources <- data.frame(resources, "priceIds"=resources[, field])
    }

    ## Check if identifer is bbg ticker:
    resources <- resources[isBBGTicker(as.character(resources[, "priceIds"])), ]

    ## Initialise the ctypeFlds list:
    ctypeFlds <- list()

    ## Check if all instruments have base field information:
    ctypeFlds[["BASE"]] <- resources[, c("symbol", "priceIds", as.character(fldsByCtype[["BASE"]]))]

    ## Exclude the BASE field list element:
    fldsByCtypex <- fldsByCtype[-1]

    ## Iterate of the fields by ctype and append:
    for (i in 1:length(fldsByCtypex)) {

        ctypeRes  <- resources[resources[, "ctype"] == names(fldsByCtypex)[i], ]
        ctypeFlds[[names(fldsByCtypex)[i]]] <- as.data.frame(ctypeRes[, c("symbol", "priceIds", as.character(fldsByCtypex[[i]]))], stringsAsFactors=FALSE)

    }

    ## 1: Only consider instruments with missing fields
    ## 2: Rename column names to bbg memnonic
    reference <- lapply(1:length(ctypeFlds), function(i) {

        ## Return NULL if no instruments:
        if (NROW(ctypeFlds[[i]]) == 0) {
            return(NULL)
        }

        ## Get the column names:
        colNames <- colnames(ctypeFlds[[i]])

        ## Get the field names:
        fldCtype <- fldsByCtype[[names(ctypeFlds)[i]]]

        ##
        ctypeFlds[[i]][, 3:NCOL(ctypeFlds[[i]])] <- t(apply(ctypeFlds[[i]][, -c(1, 2)], MARGIN=1, is.na))
        ctypeFlds[[i]] <- ctypeFlds[[i]][apply(ctypeFlds[[i]][, -c(1, 2)], MARGIN=1, any), ]

        if (NROW(ctypeFlds[[i]]) == 1) {
            ctypeFlds[[i]][, c(1,2)] <- c(as.character(ctypeFlds[[i]][1, 1]), as.character(ctypeFlds[[i]][1, 2]))
        } else {
            ctypeFlds[[i]][, c(1,2)] <- as.data.frame(apply(ctypeFlds[[i]][, c(1,2)], MARGIN=2, as.character), stringsAsFactors=FALSE)
        }

        colnames(ctypeFlds[[i]]) <- c(colNames[c(1,2)], names(fldCtype)[match(colNames[3:NCOL(ctypeFlds[[i]])], fldCtype)])
        ctypeFlds[[i]] <- as.data.frame(ctypeFlds[[i]], stringsAsFactors=FALSE)
        ctypeFlds[[i]]
    })

    ## Name the reference list:
    names(reference) <- names(fldsByCtype)

    ## Return ohlc id's with missing information:
    list("reference"=reference,
         "prices"=resources[, c("symbol", "priceIds")],
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
