##' A function to provide the figi type mapping:
##'
##' This is a description.
##'
##' @return Returns a matrix with the figi type mapping.
##' @export
figiCtype <- function() {
    c("EURO-DOLLAR"="BOND",
      "EURO MTN"="BOND",
      "EURO NON-DOLLAR"="BOND",
      "EURO-ZONE"="BOND",
      "GLOBAL"="BOND",
      "Equity WRT"="SHRE",
      "Equity Index"="OTHER",
      "Right"="SHRE",
      "Closed-End Fund"="SHRE",
      "Index WRT"="SHRE",
      "Stapled Security"="SHRE",
      "ETP"="SHRE",
      "Common Stock"="SHRE",
      "Dutch Cert"="SHRE",
      "Open-End Fund"="SHRE",
      "NVDR"="SHRE",
      "Depositary Receipt"="SHRE",
      "DOMESTIC MTN"="BOND",
      "DOMESTIC"="BOND",
      "Unit"="SHRE",
      "Open-End Fund"="SHRE",
      "ADR"="SHRE",
      "REIT"="SHRE",
      "Fund of Funds"="SHRE",
      "DOMESTIC"="SP")
}

##' A function to provide the figi type currency mapping:
##'
##' This is a description.
##'
##' @return Returns a matrix with the figi type currency mapping.
##' @export
securityTypeCurrency <- function() {
    c("EURO-DOLLAR"="USD",
      "EURO MTN"="EUR",
      "EURO NON-DOLLAR"="EUR")
}



##' A wrapper function call figi for instruments without resmains in data frame:
##'
##' This is a description.
##'
##' @param data The data-frame
##' @param idType The type of ID to use for openFigi. Default is ID_ISIN.
##' @param fld The column name of the data-frame which has the corresponding instrument id.
##' @param ccy The column name for the currency of the instrument.
##' @param figiApi The api key of the openFigi account.
##' @return Returns the original data-frame with figi information appended.
##' @export
figiWrapper <- function(data, idType="ID_ISIN", fld="isin", ccy="ccymain", figiApi) {

    ## Get the NA resources:
    naResources <- is.na(safeColumn(data, "resmain"))

    if (any(naResources)) {

        ## Hebele
        figiResult <- figi(data[naResources, ], idType, fld, ccy, figiApi)

        ## Match the figi isin to our data isins:
        figiIdx <- match(paste0(data[, fld], data[, ccy]), paste0(figiResult[,"idValue"], figiResult[,"currency"]))
        dataIdx <- !is.na(figiIdx)
        figiIdx <- figiIdx[!is.na(figiIdx)]

        ## Get the symbols:
        data[dataIdx, "symbol"] <- as.character(figiResult[figiIdx,"symbol"])

        ## Get the symbols:
        data[dataIdx,"name"] <- as.character(figiResult[figiIdx,"name"])

        ## Get the symbols:
        data[dataIdx,"ctype"] <- as.character(figiResult[figiIdx,"ctype"])

        ## Get the symbols:
        data[dataIdx,"stype"] <- as.character(figiResult[figiIdx,"stype"])

        ## Get the coupon:
        data[dataIdx,"cpn"] <- figiResult[figiIdx,"cpn"]

        ## Get the maturity:
        data[dataIdx,"maturity"] <- figiResult[figiIdx,"maturity"]

    }

    ## Done, return:
    data

}


##' A function to get openFigi information on data for given identifier and currency.
##'
##' This is a description.
##'
##' @param data The data-frame
##' @param idType The type of ID to use for openFigi. Default is ID_ISIN.
##' @param fld The column name of the data-frame which has the corresponding instrument id.
##' @param ccy The column name for the currency of the instrument.
##' @param figiApi The api key of the openFigi account.
##' @return Returns a data-frame with openFigi information.
##' @export
figi <- function(data, idType="ID_ISIN", fld="isin", ccy="ccymain", figiApi){

    ## Prepare the figi job:
    figiJob <- prepareOpenFigiData(data, idType, fld, ccy)

    ## Securities with no currency:
    missingCurrency <- nchar(as.character(figiJob[,"currency"])) == 0 | is.na(figiJob[,"currency"])

    ## If any missing currencies, run the figi call separately:
    if (any(missingCurrency)) {

        ## Seperate jobs:
        figiResult1 <- figiCall(figiJob[missingCurrency, c("idType", "idValue")], figiApi)
        figiResult2 <- figiCall(figiJob[!missingCurrency,], figiApi)

        ## Add the currencies
        figiResult1 <- data.frame(figiResult1, "currency"=securityTypeCurrency()[match(figiResult1[,"securityType"], names(securityTypeCurrency()))])

        ## Recombine the figi result:
        figiResult <- rbind(figiResult1, figiResult2[, match(colnames(figiResult1), colnames(figiResult2))])

    } else {

        ## Call the figi:
        figiResult <- figiCall(figiJob, figiApi)
    }

    ## Treat the figi result and return:
    figiResultTreater(figiResult)

}


##' A function to treat and enrich openFigi information.
##'
##' This is a description.
##'
##' @param figiResult A data-frame from function figiCall.
##' @return Returns a enriched/treated version of the data-frame from figiCall function.
##' @export
figiResultTreater <- function(figiResult){

    ## Get rid of no isin's:
    figiResult <- figiResult[!nchar(as.character(figiResult[,"idValue"])) == 0,]

    ## Get rid of no market sectors:
    figiResult <- figiResult[!is.na(figiResult[,"marketSector"]),]

    ## Hebele:
    figiResult <- data.frame(figiResult, "ctype"=figiCtype()[match(figiResult[,"securityType"], names(figiCtype()))])

    ## Hebele:
    figiResult <- data.frame(figiResult, "stype"=figiResult[,"securityType"])

    ## Construct the exchange code for symbol:
    exchCode <- ifelse(figiResult[,"ctype"] == "SHRE", as.character(figiResult[,"exchCode"]), "")

    ## Construct the symbol:
    figiResult[,"symbol"] <- gsub("  ", " ", paste(figiResult[,"ticker"], exchCode, figiResult[,"marketSector"], sep = " "))

    ## Is bond?
    isBond <- safeCondition(figiResult, "ctype", "BOND")

    ## For bonds, get the second string in symbol containing the coupon information:
    cpnRaw <- sapply(strsplit(figiResult[isBond, "symbol"], " "), function(x) x[2])

    ## Replace Floating and variable flag:
    cpn <- gsub("F", "0", cpnRaw)
    cpn <- gsub("V", "", cpn)

    ## Assing the coupon rates to bonds:
    figiResult[isBond,"cpn"] <- cpn

    if (any(isBond)) {
        figiResult[isBond, "maturity"] <- as.character(as.Date(bondMaturityFromTicker(figiResult[isBond,"symbol"]), format="%m/%d/%y"))
    } else {
        figiResult[,"maturity"] <- NA
    }

    ## Construct the names: 1. Define as character:
    figiResult[,"name"] <- as.character(figiResult[,"name"])

    ## Replace bond name:
    figiResult[isBond, "name"] <- gsub("Corp", "", figiResult[isBond, "symbol"])

    ## Get rid of NA's, duplicate words and double white spaces:
    figiResult[, "symbol"] <- sapply(strsplit(gsub("NA", "", figiResult[,"symbol"]), " "), function(x) trimDws(paste0(unique(x), collapse=" ")))

    ## Done, return:
    figiResult

}


##' A helper function to flatten the figiCall results:
##'
##' This is a description.
##'
##' @param x ??
##' @param job ??
##' @return Returns a flattened data-frame from the openFigi POST result.
##' @export
flatten <- function(x, job){

    ## Initialise the return value:
    df <- NULL

    ## Loop through the rows:
    for (i in 1:NROW(job)) {

        ## Get the first element of list:
        temp <- data.frame(t(sapply(x[[i]]$data[[1]], function(x) ifelse(is.null(x), NA, x))))

        ## If element has no result, return NA:
        if (NCOL(temp) == 0){

            ## Create row with NA's:
            add <- t(data.frame(rep(NA, 7)))

            ## Assign the colnames:
            colnames(add) <- c("name", "figi", "marketSector", "ticker", "securityType", "securityType2", "exchCode")

        ## If element has results, create data frame:
        } else {
            add <- data.frame("name"=temp$name,
                              "figi"=temp$figi,
                              "marketSector"=temp$marketSector,
                              "ticker"=temp$ticker,
                              "securityType"=temp$securityType,
                              "securityType2"=temp$securityType2,
                              "exchCode"=temp$exchCode)
        }

        ## Append row to return value
        df <- rbind(df, add)
    }

    ## Combine original job with results and return:
    cbind(job, df)
}


##' A function to infer the currency from an instrument identifier and market sector.
##'
##' This is a description.
##'
##' @param df The data-frame with marketSector and idValue columns.
##' @return Returns the original data-frame enriched with the currency.
##' @import httr
##' @export
getCurrencyFromFigiExchCode <- function(df){

    ## Iterate over df:
    for (row in 1:NROW(df)) {

        ## Get the market secotrs:
        mktSctr <- as.character(df[row, "marketSector"])

        ## Get the id values:
        idValue <- as.character(df[row, "idValue"])

        ## Print the status:
        print(paste0("curling ", row, " of ", NROW(df), " for: ", idValue))

        url <- sprintf("https://openfigi.com/search/search-constants/searchOptions?marketSector=%s&simpleSearchString=%s", mktSctr, idValue)
        curlResponse <- httr::content(httr::GET(url))
        df[row,"ccymain"] <- curlResponse[[1]][[2]][[3]][[1]]$value
    }

    ## Return:
    df

}


##' A function to call the openFigi api for a given openFigi job data-frame.
##'
##' This is a description.
##'
##' @param job A data-frame from prepareOpenFigiData.
##' @param apiKey The api key of the openFigi account.
##' @return Returns a flattened data-frame with results from the openFigi api call.
##' @import httr
##' @import stats
##' @export
figiCall <- function(job, apiKey){

    ## If no job, return null:
    if (NROW(job) == 0){
        return(NULL)
    }

    ## Assign the endpoint:
    endpoint <- "https://api.openfigi.com/v1/mapping"

    ## We can only run 100 queries in one job. Let's split to batches:
    str <- strsplit(as.character(NROW(job) / 100), "\\.")[[1]]
    str[2] <- ifelse(nchar(str[2]) == 1, paste0(str[2], 0), str[2])
    batches <- as.numeric(na.omit(cumsum(c(rep(100, as.numeric(str[1])), as.numeric(str[2])))))

    ## Starting index of first batch:
    start <- 1

    ## Initialse return value:
    dfx <- NULL

    ## Loop through the batches:
    for (end in batches) {

        print(paste0("Calling Openfigi ... ", end, "/", NROW(job)))

        ## Select the bacth from jobs:
        batch <- job[(start:end),]

        ## Get the post result:
        result <- httr::POST(endpoint, config=httr::add_headers("Content-Type"="text/json", "X-OPENFIGI-APIKEY"=apiKey), body=toJSON(batch))

        ## Flatten the result:
        df <- flatten(httr::content(result), batch)

        ## Append the return value:
        dfx <- rbind(dfx, df)

        ## Increment the starting index of the new batch:
        start <- start + 100
    }

    ## Return:
    dfx
}


##' A function to prepare the data-frame for the figiCall function.
##'
##' This is a description.
##'
##' @param df The data-frame
##' @param idType The type of ID to use for openFigi. Default is ID_ISIN.
##' @param fld The column name of the data-frame which has the corresponding instrument id.
##' @param ccy The column name for the currency of the instrument.
##' @return Returns the openFigi comform data-frame.
##' @export
prepareOpenFigiData <- function(df, idType, fld, ccy){

    ## Prepare the data frame:
    data <- data.frame("idType"=idType,
                       "idValue"=df[, fld],
                       "currency"=df[,ccy])

    ## Only select non-duplicated ISIN's:
    data[!duplicated(data[,"idValue"]),]

}
