##' A function to get the enriched holdings data-frame.
##'
##' This is the description
##'
##' @param holdings The holdings data-frame as returned by getFlatHoldings
##' @param nav The nav as returned by getResource("consolidation").
##' @param gav The gav as returned by getResource("consolidation").
##' @param regions The data-frame with the country to region mapping.
##' @param resources The data-frame as returned by getResource("resources")
##' @return A data-frame with the enriched holdings.
##' @import rdecaf
##' @export
getEnrichedHoldings <- function(holdings, nav, gav, regions, resources){

    ## If regions mapper is supplied, append to the data-frame:
    if (!is.null(regions)){
        regions <- toupper(unlist(regions))
        regions <- data.frame("country"=regions, "region"=toupper(gsub("[[:digit:]]", "", names(regions))), row.names=NULL)
        regions <- regions[match(toupper(holdings[,"Country"]), regions[,"country"]), "region"]
    }

    ## Get the match index:
    matchIdx <- match(holdings[,"ID"], resources[,"id"])

    ##:
    holdings[is.na(holdings[,"Subtype"]), "Subtype"] <- holdings[is.na(holdings[,"Subtype"]), "Type"]

    ## All 'Money' subtypes to 'Cash'.
    holdings[holdings[,"Subtype"] == "Money", "Subtype"] <- "Cash"

    ## Enrich the holdings data-frame and return:
    data.frame(holdings,
               "Region"=safeTry(try(as.character(regions))),
               "Value (%)"=safeTry(try(holdings[,"Value"] / nav, silent=TRUE)),
               "Exp (%)"=safeTry(try(holdings[,"Exposure"] / nav, silent=TRUE)),
               "Expiry"=resources[matchIdx, "expiry"],
               "Call/Put"=safeCondition(resources[matchIdx, ], "callput", "True"),
               "Rate"=ifelse(is.null(resources[matchIdx, "pxmain"]), NA, resources[matchIdx, "pxmain"]),
               "Underlying"=ifelse(is.null(resources[matchIdx, "underlying"]), NA, resources[matchIdx, "underlying"]),
               check.names=FALSE)

}


##' A wrapper function to get the enriched holdings based on consolidation parameters.
##'
##' This is the description
##'
##' @param params The parameters for the consolidation.
##' @param resources The data-frame as returned by getResource("resources").
##' @param session The rdecaf session.
##' @param charLimit The character cutoff for the name of the holdings. Default is 50.
##' @param regions The data-frame with the country to region mapping.
##' @return The enriched holdings data-frame.
##' @import rdecaf
##' @export
getHoldingsWrapper <- function(params, resources, session, charLimit=50, regions=NULL){

    ## Retrieve the consolidation:
    consolidation <- getResource("consolidation", params=params, session=session)

    ## Flatten the holdings:
    holdings <- getFlatHoldings(consolidation[["holdings"]], charLimit=charLimit)

    ## Enrich the holdings:
    getEnrichedHoldings(holdings, consolidation[["nav"]], consolidation[["gav"]], regions, resources)
}


##' A function to flatten the consolidation object.
##'
##' This is the description
##'
##' @param x The holdings element from getResource("consolidation").
##' @param charLimit The character cutoff for the name of the holdings. Default is 50.
##' @return Returns the flat holdings data-frame.
##' @import rdecaf
##' @export
getFlatHoldings <- function(x, charLimit=30){
    holdings <- lapply(x, function(h) data.frame("Name"=as.character(ellipsify(h[["artifact"]][["name"]], charLimit=charLimit)),
                                                 "Account"=h[["accounts"]][[1]][["id"]],
                                                 "Symbol"=as.character(h[["artifact"]][["symbol"]]),
                                                 "ID"=as.numeric(h[["artifact"]][["id"]]),
                                                 "CCY"=as.character(h[["artifact"]][["ccy"]]),
                                                 "Type"=as.character(trimws(gsub("Contract", "", h[["artifact"]][["type"]][["name"]]))),
                                                 "Subtype"=as.character(ifelse(isNAorEmpty(h[["artifact"]][["stype"]]), NA,h[["artifact"]][["stype"]])),
                                                 "Country"=capitalise(as.character(ifelse(isNAorEmpty(h[["artifact"]][["country"]]), NA, h[["artifact"]][["country"]]))),
                                                 "Sector"=capitalise(as.character(ifelse(isNAorEmpty(h[["artifact"]][["sector"]]), NA, h[["artifact"]][["sector"]]))),
                                                 "QTY"=as.numeric(h[["quantity"]]),
                                                 "PX Cost"=as.numeric(h[["investment"]][["px"]][["org"]]),
                                                 "PX Last"=as.numeric(h[["valuation"]][["px"]][["org"]]),
                                                 "Value"=safeNull(as.numeric(h[["valuation"]][["value"]][["net"]][["ref"]])),
                                                 "Exposure"=safeNull(as.numeric(h[["valuation"]][["exposure"]][["abs"]][["ref"]])),
                                                 "PnL (Unreal)"=safeNull(as.numeric(h[["pnl"]])),
                                                 "PnL (% of Inv.)"=safeNull(as.numeric(h[["pnl_to_investment"]])),
                                                 check.names=FALSE))
    ## Get the holdings:
    classify(as.data.frame(do.call(rbind, holdings), check.names=FALSE))
}
