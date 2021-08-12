##' Returns df of time series from marketstack
##'
##' This is a description.
##'
##' @param timeOfDay e.g. eod or intraday (15 min int, avoid/unreliable)
##' @param mstackAPIKey marketstack API key
##' @param mstackSymbol ticker per marketstack, e.g. RUT.INDX = Russell 2000
##' @param mstackSort default = DESC
##' @param mstackStart starting date, fmt = YYYY-MM-DD, defaults to current date less a week
##' @param mstackEnd ending date, fmt = YYYY-MM-DD, defaults to current date
##' @param mstackLimit limit on records returned, defaults to 1000 (max allowed)
##' @return A data frame with market data.
##' @export
mstackData <- function(timeOfDay="eod",mstackAPIKey,mstackSymbol,mstackSort="DESC",mstackStart=Sys.Date()-7,mstackEnd=Sys.Date(),mstackLimit=1000) {

mstackInt <- ""

if(timeOfDay=="intraday") {
    mstackInt <- "&interval=15min"
}

scrty <- fromJSON(
      paste0(
          "http://api.marketstack.com/v1/",
          timeOfDay,
          "?access_key=",
          mstackAPIKey,
          "&symbols=",
          mstackSymbol,
          mstackInt,
          "&sort=",
          mstackSort,
          "&date_from=",
          mstackStart,
          "&date_to=",
          mstackEnd,
          "&limit=",
          mstackLimit
          )
      ) %>% 
      .[[2]] 

return(scrty)

}

##' Returns df of tickers from marketstack
##'
##' This is a description.
##'
##' @param access marketstack API key
##' @param search free text value to search for e.g. ETF, ARCX (NYSE). Defaults to empty
##' @param searchOrExchange are we filtering my name/ticker or exchange, default is exchange, change to &search for ticker
##' @param lim limit of records to return, defaults to max of 1000
##' @param EOD filter to look at just data that EOD, defaults to true
##' @param country if we want to filter by country 2-digit code for currency. Defaults to US
##' @return A data frame with market data.
##' @export
mstackSrchTicker <- function(access,search="",searchOrExchange="&exchange=",lim=1000,EOD=c(TRUE),country="US") {

ETF <- fromJSON(
      paste0(
          "http://api.marketstack.com/v1/tickers",
          "?access_key=",
           access,
           searchOrExchange,
           search,
          "&limit=",
          lim
          )
      ) %>% 
      .[[2]] %>%
      dplyr::filter(
      has_eod %in% EOD
      ) %>% 
      select(symbol,name,stock_exchange)
      
if(!is.null(country)) {
ETF <- ETF %>%
      dplyr::filter(stock_exchange$country_code==country)

}
return(ETF)

}

