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
