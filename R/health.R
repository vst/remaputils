##' This function checks whether ohlccodes are dubious.
##'
##' This is a description
##'
##' @param resources A data-frame with the resources from rdecaf.
##' @param jaccardCoeff The threshold for the jaccard string distance coefficient.
##' @return The resource data-frame with dubious ohlccodes.
##' @export
dubiousOHLCcodes <- function(resources, jaccardCoeff) {

    ## Get the resources which have OHLC codes:
    resourceWithOHLCcode <- resources[!isNAorEmpty(resources[, "ohlccode"]), ]

    ## If none has ohlccodes, return NULL:
    if (NROW(resourceWithOHLCcode) == 0) {
        return(NULL)
    }

    ## Which ones fail to grep the ohlccode in symbol:
    grepMethod <- apply(resourceWithOHLCcode, MARGIN=1, function(row) safeGrep(row["symbol"], row["ohlccode"]) == "0")

    ## Which ones have a very dissimilar ohlccode compared to symbol:
    jaccardMethod <- as.numeric(apply(resourceWithOHLCcode, MARGIN=1, function(x) stringdist(as.character(x["symbol"]), as.character(x["ohlccode"]), method="jaccard"))) > jaccardCoeff

    ## Get the dubious ohlc codes:
    dubiousOHLCcodes <- resourceWithOHLCcode[grepMethod | jaccardMethod,]

    ## If none dubious, return NULL:
    if (NROW(dubiousOHLCcodes) == 0) {
        return(NULL)
    }

    ## Done, return:
    dubiousOHLCcodes

}


##' This function checks whether trade dates are dubious.
##'
##' This is a description
##'
##' @param trades A data-frame with the trades from rdecaf.
##' @param backdatedness The number of days after which a trade should have been created.
##' @return The trades data-frame with dubious trade dates.
##' @export
dubiousTradeDates <- function(trades, backdatedness) {

    ## Trades with creation dates over N days of commitment date:
    aPriorCreation <- as.Date(trades[, "created"]) - as.Date(trades[, "commitment"]) > backdatedness

    ## Dates with commitment date later than creation date:
    exAnteCreation <- as.Date(trades[, "created"]) < as.Date(trades[, "commitment"])

    ## Filter and return:
    trades <- trades[aPriorCreation | exAnteCreation, ]

}


##' This is a wrapper function for ohlc health function.
##'
##' This is a description
##'
##' @param asof The asof date. Default is Sys.Date().
##' @param ohlccodes The ohlccodes to be check. If NULL, it gets it from the stocks. Default is NULL.
##' @param underlying Shall the underlying be checked as well? Default is FALSE
##' @param lookBack The lookback periodin days. Default is 365.
##' @param session The rdecaf session.
##' @return A data frame with unhealthy ohlc observations.
##' @export
ohlcHealthWrapper <- function(asof=Sys.Date(), ohlccodes=NULL, underlying=FALSE, lookBack=365, session) {

    ## If no ohlc codes are provided, get from stocks:
    if (is.null(ohlccodes)) {

        ## Get the stocks:
        stocks <- as.data.frame(getResource("stocks", params=list("format"="csv", "page_size"=-1, "asof"=asof), session=session))

        ## Get the resources:
        resources <- getResourcesByStock(stocks, session, underlying)

        ## Get ohlccodes:
        ohlccodes <- unique(ifelse(is.na(resources[, "ohlccode"]), resources[, "symbol"], resources[, "ohlccode"]))

    }

    ## Get the ohlc observations:
    ohlcObs <- lapply(ohlccodes, function(code) getOhlcObsForSymbol(session, code, lte=asof, lookBack=lookBack + 100, excludeWeekends = TRUE, addFields = NULL))

    ## Mask data if ohlc observation is empty:
    ohlcObs <- lapply(1:length(ohlccodes), function(i) {

        ## If we have ohlc observations, return the same:
        NROW(ohlcObs[[i]]) == 0 || return(ohlcObs[[i]])

        df <- initDF(colnames(ohlcObs[[i]]))

        df[, "symbol"] <- ohlccodes[i]

        df

    })

    ## Run the ohlc health check on ohlc obs:
    ohlcObsHealth <- do.call(rbind, lapply(ohlcObs, function(obs) ohlcHealth(obs, asof=Sys.Date(), lookBack=lookBack)))

    ## Remove the NA symbols:
    ohlcObsHealth <- ohlcObsHealth[!is.na(ohlcObsHealth[, "Symbol"]), ]

    ## Get the suspects:
    ohlcObsSuspects <- ohlcObsHealth[apply(ohlcObsHealth, MARGIN=1, function(x) any(trimws(x[2:4]))), ]

    ## Done, return:
    ohlcObsSuspects

}


##' This function checks the health of the OHLC observations.
##'
##' This is a description
##'
##' @param ohlc A data-frame with the ohlc observations.
##' @param asof The date to compare ohlc observations to.
##' @param lookBack The number of days to look back for the check.
##' @return A data-frame with state of ohlc series.
##' @export
ohlcHealth <- function(ohlc, asof=Sys.Date(), lookBack=5) {

  ## Convert close to numeric, date to date:
  ohlc[, "close"] <- as.numeric(ohlc[, "close"])

  ohlc[, "date"] <- as.Date(ohlc[, "date"])

  ## N days condition:
  ndays <- NROW(ohlc) ==  0 | all(is.na(ohlc[, "close"]))

  ## Full time series to find gaps
  anygaps <- FALSE
  gapDate <- NA %>% as.Date()
  gaps <- data.frame()
  flagDate <- data.frame()

  if(!all(is.na(ohlc$date))) {
  gaps <- data.frame(date=seq.Date(from=min(ohlc$date,na.rm=TRUE),to=max(ohlc$date,na.rm=TRUE),by="day")) %>%
    left_join(ohlc,by="date") %>%
    mutate(flag=ifelse(is.na(close),1,0),week=paste(lubridate::year(date),lubridate::week(date))) %>%
    group_by(week) %>%
    mutate(gaps=sum(flag)) %>% 
    ungroup()

  flagDate <- gaps  %>%
    dplyr::filter(flag==1&gaps>4) %>%
    ungroup() %>%
    arrange(date) %>%
    slice(1:1)
  }
  if(NROW(flagDate)>0) {
  anygaps <- TRUE
  gapDate <- gaps %>%
    dplyr::filter(!is.na(id),date<flagDate$date)  %>%
    arrange(desc(date))  %>%
    slice(1:1)  %>%
    .[[1]]
  }

  ## Prepare the data frame:
  result <- data.frame("Symbol"=ohlc[1, "symbol"],
                       "No PX in N days"=as.logical(ifelse(ndays, TRUE, all(asof - ohlc[, "date"] > lookBack))),
                       "No PX at all"=as.logical(ndays),
                       "No PX change"=as.logical(ifelse(ndays, TRUE, all(diff(ohlc[, "close"]) == 0))),
                       "Last PX update"=max(as.Date(ohlc[, "date"]),na.rm=TRUE),
                       "Any PX gaps"=anygaps,
                       "First gap date"=gapDate,
                       check.names = FALSE,
                       stringsAsFactors = FALSE)

  ## Done, return:
  result

}


##' This function checks whether symbols are dubious w.r.t Bloomberg symbology.
##'
##' This is a description
##'
##' @param symbol A vector with symbols
##' @return A vector of length symbols with TRUE or FALSE.
##' @export
dubiousSymbolBBG <- function(symbol) {

    ## Split each symbol by space.
    splitSymbol <- strsplit(symbol, " ")

    ## Check each split symbol for inconsistency and return:
    sapply(splitSymbol, function(x) length(x) == 1 | all(is.na(match(tail(x), c("Equity", "Corp", "Curncy", "Index", "Comdty")))))
}
