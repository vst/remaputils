##' A function to get the value amounts of transfers for a portfolio.
##'
##' This is the description
##'
##' @param portfolio The portfolio id.
##' @param since The date since.
##' @param until The date until.
##' @param currency The portfolio currency.
##' @param session The rdecaf session.
##' @return A data frame with the converted value amounts of transfers.
##' @export
getTransferValueAmounts <- function(portfolio, since, until, currency, session) {

    ## Get the transfer quants:
    quants <- getDBObject("quants", session, addParams=list("account__portfolio"=portfolio, "trade__ctype"=30))

    ## Mutliply valamt's with direction:
    quants[, "valamt"] <- as.numeric(quants[, "valamt"]) *  ifelse(quants[, "quantity"] < 0, -1, 1)

    ## Filter by dates:
    quants <- quants[quants[, "commitment"] > since & quants[, "commitment"] < until, c("commitment", "valamt", "valccy")]

    ## Convert the values by currency:
    quants <- convertValuesByFXRate(quants, ccyFld = "valccy", dtFld = "commitment", valFld = c("valamt"), convertTo=currency, session)

    ## Done, return:
    return(quants)
}

##' A function to sync trades in batches
##'
##' This is the description
##'
##' @param ctype The trade ctype.
##' @param portfolio The portfolio id.
##' @param gte Great than date.
##' @param session The rdecaf session.
##' @return A data frame with the trades for desired ctype.
##' @export
getTradeByCtypeForPortfolio <- function(ctype, portfolio, gte=NULL, session) {

    ## Construct the params:
    params <- list("format"="csv", "page_size"=-1, "ctype"=ctype, "accmain__portfolio"=portfolio, "commitment__gte"=gte)

    ## Get and return:
    as.data.frame(getResource("trades", params=params, session=session))
}


##' A function to sync trades in batches
##'
##' This is the description
##'
##' @param trades The trades data frame.
##' @param ccy The currency to convert valamt's to.
##' @param session The rdecaf session.
##' @return A data frame with the trades for desired ctype.
##' @export
getTXNsOfTrades <- function(trades, ccy=NULL, session) {

    ## If trades is empty, return NULL:
    if (NROW(trades) == 0) {
        return(NULL)
    }

    ## Get the transaction id's:
    txnIds <- na.omit(as.character(unlist(trades[,safeGrep(colnames(trades), "quant.") == "1"])))

    ## Construct the params:
    params <- list("page_size"=-1,
                   "format"="csv",
                   "id__in"=paste(txnIds, collapse=","))

    ## Get and return:
    quants <- as.data.frame(getResource("quants", params=params, session=session))

    ## If ccy is null, return quants:
    if (is.null(ccy)) {
        return(quants)
    }

    ## Append the fx pair for the conversion:
    quants[, "fxpair"] <- paste0(quants[, "valccy"], ccy)

    ## Get the unique fx pairs and the min and max dates:
    fxratesQ <- lapply(extractToList(quants, "fxpair"), function(x) data.frame("pair"=x[1, "fxpair"],
                                                                               "gte"=min(x[, "commitment"]),
                                                                               "lte"=max(x[, "commitment"]),
                                                                               stringsAsFactors=FALSE))


    ## Get the fx rates:
    fxrates <- lapply(fxratesQ, function(x) getOhlcObsForSymbol("session"=session,
                                                                "symbol"=x[, "pair"],
                                                                "lte"=x[, "lte"],
                                                                "lookBack"=x[, "lte"]-x[, "gte"]))


    ## Name the fx rates:
    names(fxrates) <- unique(quants[, "fxpair"])

    ## Iterate over quant rows and apppend converted valamt's:
    quants <- do.call(rbind, lapply(1:NROW(quants), function(i) {

        ## Get the current fx pair for row:
        cPair <- quants[i, "fxpair"]

        ## Get the corresponding fx rate for row:
        cRates <- fxrates[[cPair]]

        ## Initialise the value fo the nearesth date variable:
        vond <- list()

        ## If no fx rates for row, set vond value to 1, else get the value for closest date:
        if (NROW(cRates) == 0) {
            vond[["value"]] <- 1
        } else {
            ## Get the close of the nearest date:
            vond <- valueOfNearestDate(quants[i, "commitment"], cRates, tolerance=10, dateField="date", valueField="close", nameField=NULL)
        }

        ## Compute the net value amount in reference currency:
        quants[i, "valamt_refccy"] <- as.numeric(quants[i, "valamt"]) * vond[["value"]] * ifelse(as.numeric(quants[i, "quantity"]) < 0, -1, 1)

        ## Return the transaction/quant for row:
        quants[i, ]

    }))

    ## Done, return:
    quants

}


##' A function to sync trades in batches
##'
##' This is the description
##'
##' @param trades The trades data frame.
##' @param session The DECAF session info
##' @param batchSize The desired batch sizes. Default=150.
##' @return NULL
##' @import rdecaf
##' @export
syncTradesByBatch <- function(trades, session, batchSize=500) {

    ## Create the batches:
    batches <- createBatches(NROW(trades), batchSize)

    ## Interate over batches and push:
    for (i in 1:length(batches[[1]])) {

        ## Get the start index:
        start <- batches$startingIdx[[i]]

        ## Get the end index
        end <- batches$endingIdx[[i]]

        ## Get the payload:
        payload <- toJSON(list("actions"=trades[start:end,]), auto_unbox=TRUE, na="null", digits=10)

        print(paste0("Posting trades ", start, ":", end, " of ", NROW(trades)))

        ## Push and get response:
        response <- postResource("imports/inbulk", params=list(sync="True"), payload=payload, session = session)
    }

    ## Done, return:
    return(NULL)

}


##' A function to get trades from session using container names:
##'
##' This is the description
##'
##' @param containerNames A vector with container names
##' @param session The DECAF session info
##' @param type The container type
##' @param gte A date object to express 'Greater Than' for commitment date of trades. Default is NULL.
##' @return A data-frame with DECAF trades for container.
##' @import rdecaf
##' @export
getTradesFromContainerNames <- function(containerNames, session, type, gte=NULL) {

    if (type == "accounts") {

        ## Construct the account params:
        params <- list("page_size"=-1,
                       "name__in"=paste(containerNames, collapse=","))

        ## Get the portfolios:
        container <- do.call(rbind, getResource(type, params=params, session=session))

        accounts <- container[, "id"]
    }

    if (type == "portfolios") {

        ## Construct the portfolio params:
        params <- list("page_size"=-1,
                       "format"="csv",
                       "name__in"=paste(containerNames, collapse=","))

        ## Get the portfolios:
        container <- as.data.frame(getResource(type, params=params, session=session))

        ## Get the account id:
        accounts <- as.numeric(na.omit(unique(unlist(container[, grep("accounts.", colnames(container))]))))
    }

    ## Get the account wise trades and return:
    list("trades"=getAccountWiseTrades(accounts, session, gte),
         "container"=container)

}


##' A function to get account-wise trades from a DECAF instance.
##'
##' This is the description
##'
##' @param accounts A vector with account id's.
##' @param session The DECAF session info.
##' @param gte The date after which trades should be considered.
##' @param dateType The type, either commitment, settlement, created or updated. Default is commitment.
##' @return A data-frame with DECAF trades.
##' @import rdecaf
##' @export
getAccountWiseTrades <- function(accounts, session, gte=NULL, dateType="commitment") {

    ## Initialise the trade list:
    trades <- list()

    ##:
    print(sprintf("%s accounts to retrieve trades from.", length(accounts)))

    ## Retrieve account-wise trades
    for (i in 1:length(accounts)) {

        ## Get the trades list:
        params <- list("accmain"=accounts[i],
                       "page_size"=-1,
                       "format"="csv")



        if (!is.null(gte)) {
            paramsExt <- c(params, as.character(gte))
            names(paramsExt) <- c(names(params), sprintf("%s__gte", dateType))
        } else {
            paramsExt <- params
        }

        ## :
        print(sprintf("Retrieving trades for account no %s (id:%s) on %s", i, accounts[i], session[["location"]]))

        ##:
        trds  <- as.data.frame(getResource("trades", params=paramsExt, session=session))

        ## If no trades, next:
        if (NROW(trds) == 0) {
            next
        }

        ## Do the nested ordering:
        trds <- nestedOrdering(trds, c("commitment", "executedat", "pseudorder", "created"))

        ## Change date formats to character:
        for (fld in c("commitment", "settlement", "created", "updated")) {
            ## Get the trades:
            trds[, fld] <- as.character(trds[, fld])
        }

        ## Append the system trades:
        trades <- c(trades, list(trds))
    }

    ## Safely bind and return:
    safeRbind(trades)
}


##' A function to get account-wise trades from a DECAF instance.
##'
##' This is the description
##'
##' @param ids A vector with container id's.
##' @param containerType The container type.
##' @param session The DECAF session info.
##' @param gte The date after which trades should be considered.
##' @return A data-frame with DECAF trades.
##' @import rdecaf
##' @export
getContainerWiseTrades <- function(ids, containerType="accounts", session, gte=NULL) {

    ## Initialise the trade list:
    trades <- list()

    if (containerType == "accounts") {
        params <- list("accmain"=NA,
                       "page_size"=-1,
                       "format"="csv",
                       "commitment__gte"=gte)}

    if (containerType == "portfolios") {
        params <- list("portfolio"=NA,
                       "page_size"=-1,
                       "format"="csv",
                       "commitment__gte"=gte)}

    ## Retrieve account-wise trades
    for (i in 1:length(ids)) {

        ## Get the trades list:
        params[[1]] <- ids[i]

        trds  <- as.data.frame(getResource("trades", params=params, session=session))

        ## If no trades, next:
        if (NROW(trds) == 0) {
            next
        }

        ## Do the nested ordering:
        trds <- nestedOrdering(trds, c("commitment", "executedat", "pseudorder", "created"))

        ## Change date formats to character:
        for (fld in c("commitment", "settlement", "created", "updated")) {
            ## Get the trades:
            trds[, fld] <- as.character(trds[, fld])
        }

        ## Append the system trades:
        trades <- c(trades, list(trds))
    }

    ## Safely bind and return:
    safeRbind(trades)
}


##' TODO:
##' '
##' This is the description
##'
##' @param portfolio A vector of portfolio id's
##' @param session The rdecaf session
##' @param gte A date after which the trades should be considered
##' @param lte A date before which the trades should be considered. Default is set to gte.
##' @param nojournal Either "True" or "False" indicating whether journal entries shall be omitted.
##' @return A vector with string representing the decaf url
##' @export
getLinkToTradesByDate <- function(portfolio, session, gte, lte=NULL, nojournal="False") {

    ## If lte is null, set to gte:
    if (is.null(lte)) {
        lte <- as.character(gte)
    }

    ## Construct the link:
    paste0(gsub("api", "", session[["location"]]),
           "trade?accmain__portfolio=", portfolio,
           "&commitment__gte=", as.character(gte),
           "&commitment__lte=", as.character(lte),
           "&nojournal=", nojournal)

}


##' A function to get the non-performance related actions for a portfolio.
##'
##' This is the description
##'
##' @param portfolio A vector of portfolio id's
##' @param pxinfo The data frame with the pxinfo
##' @param date The asof date
##' @param ccy The desired ccy.
##' @param session The rdecaf session
##' @return A data frame
##' @export
getNPRTxns <- function(portfolio, pxinfo, date, ccy, session) {

    ## Get the trade ctype for the fund vs mandates:
    ctype <- ifelse(any(do.call(rbind, pxinfo)[, "isFund"]), "35", "30")

    ## Get the investment/transfer trades:
    invstm <- getTradeByCtypeForPortfolio(ctype, portfolio, gte=dateOfPeriod("Y-0", date), session)

    ## The the investments' quants:
    invstm <- getTXNsOfTrades(invstm, ccy, session)

    ## Filter by date:
    invstm[invstm[, "commitment"] <= date, "valamt_refccy"]

    ## Filter by type:
    invstm <- invstm[invstm[, "ctype"] == "45" | invstm[, "ctype"] == "30", ]

    ## Mask if non:
    if (is.null(invstm)) {
        invstm <- data.frame("valamt_refccy"=0)
    }

    ## Done, return:
    return(invstm)

}


##' A function to map portfolio info to a trades data frame by account id, referenced as "account".
##'
##' This is the description
##'
##' @param data a trades data frame
##' @param session The rdecaf session
##' @return A data frame
##' @export
getPortNameByAccount <- function(data,session) {

  portfolios <- getDBObject("portfolios", session) %>%
    select(id,name,rccy,starts_with("account")) %>%
    pivot_longer(cols=!c(id,name,rccy), names_to="temp",values_to = 'account') %>%
    dplyr::filter(!is.na(account)) %>%
    select(-temp) %>%
    rename(portfolio=id,portfolioName=name)

  df <- data %>%
    inner_join(portfolios,by=c("account"))

  return(df)

}


##' A function to get initial premium date and value for a trades data frame.
##'
##' This is the description
##'
##' @param data a trades data frame
##' @param minval a numeric value floor to exclude records below this threshold
##' @return A list
##' @export
getInitialPremium <- function(data,minval=1000) {

  prem <- data %>% dplyr::filter(ctype==30)
  if(NROW(prem)==0) {
    prem <- data
  }

  prem <- prem %>%
    group_by(commitment) %>%
    mutate(value=sum(as.numeric(valamt))) %>%
    ungroup() %>%
    dplyr::filter(value>=minval) %>%
    ##arrange(commitment) %>%
    ##slice(1:1)
    ##rowwise() %>%
    dplyr::filter(commitment==min(commitment))


  if(NROW(prem)==0) {
    return(list("premium"=0,"date"=as.Date(NA),"data"=NULL))
  }

  return(
    list("premium"=unique(prem$value),
         "date"=unique(prem$commitment),
         "data"=prem %>% select(id) %>% mutate(isInitialPremiumSuspect=TRUE)
         )
    )

}


##' A function to flag all suspected premium payments from trades data, for all cash trades.
##'
##' This is the description
##'
##' @param portfolio portfolio ID
##' @param fpKeywords list of column and contents to identify as false positives
##' @param transferMin min threshold decimal of initial premium to count a transfer as a true premium paid
##' @param session The rdecaf session
##' @return A data frame
##' @export
detectTransfers <- function(portfolio,
                            fpKeywords=list("stype"="reversal","remarks"="fee"),
                            transferMin=.035,
                            session) {

  ## Print some stuff:
  print(sprintf("Going for portfolio %s", portfolio))

  ## Get trades:
  trades <- getDBObject("trades",session, addParams=list(accmain__portfolio=portfolio,nojournal="True"))

  ## Short Circuit
  if (NROW(trades)==0) {
    return(NULL)
  }

  trades <- trades %>%
    select(accmain_name, accmain, id, commitment, resmain_ctype, ctype, stype, remarks, qtymain,
           pxcost, pxmain, resmain_stype, resmain_symbol, resmain_type, starts_with("tag")) %>%
    mutate_at(c("qtymain","pxcost","pxmain","id","accmain","ctype"),as.numeric) %>%
    mutate(commitment=as.Date(commitment)) %>%
    mutate_at(vars(-commitment,-qtymain,-pxcost,-pxmain,-id,-accmain,-ctype),as.character) %>%
    rename(account=accmain)

  trades <- do.call("getPortNameByAccount",list(trades,session))

  ## Short Circuit
  if(NROW(trades)==0) {
    print("No Trades")
    return(NULL)
  }

  print(unique(trades$portfolioName))

  ## Get transactions
  trans <- getDBObject("quants",session, addParams=list(account__portfolio=portfolio, refccy=unique(trades$rccy))) %>%
    select(trade, refamt, commitment, type, symbol, valamt) %>%
    rename(id=trade) %>%
    mutate(valamt=if_else(is.na(refamt),as.numeric(valamt),as.numeric(refamt))) %>%
    select(-refamt)

  trades <- trades %>%
    inner_join(trans %>% select(id,valamt), by="id") %>%
    group_by(across(c(-valamt))) %>%
    summarise(valamt=sum(valamt)) %>%
    ungroup() %>%
    mutate(valamt=sign(qtymain)*valamt)

  ## Get the cash trades:
  cashTrades <- trades %>%
    dplyr::filter(resmain_ctype=="CCY")

  ## Short Circuit
  if(NROW(cashTrades)==0) {
    print("No Cash Trades")
    ##return(NULL)
  }

  ## get first premium
  fPremium <- getInitialPremium(trades,1000)

  print(paste("First Premium:",fPremium[["premium"]]))

  ## Move this part outside function ##
  ## Get the non-cash inflows/outflows
  othrFlow <- trades %>%
    dplyr::filter(resmain_ctype!="CCY",ctype==20)

  ## get reference ccy
  ## refCCY <- trades %>% select(rccy) %>% unique() %>% .[[1]]

  ## Get transaction details for value amount for non-cash flows
  othrFlowTrans <- othrFlow %>%
    select(id) %>%
    inner_join(trans, by="id") %>%
    mutate(inout=1,valamt=round(if_else(type=="Outflow",as.numeric(valamt),-1*as.numeric(valamt))),ctype=20) %>%
    select(inout, valamt, commitment, symbol, ctype)


  ## Get non-cash trades that can be joined by remark as extra layer e.g. 151556
  othrFlowTrade <- othrFlow %>% ## get rid of reversals
    anti_join(othrFlow %>% dplyr::filter(abs(qtymain)>0) %>% mutate(qtymain=-1*qtymain), by=c("qtymain","pxcost","pxmain","commitment","remarks","resmain_ctype","resmain_stype","resmain_symbol")) %>%
    dplyr::filter(!is.na(remarks)) %>%
    dplyr::filter(trimws(remarks)!="") %>%
    group_by(commitment,remarks) %>%
    summarise(qtymain=sum(qtymain)) %>%
    ungroup() %>%
    mutate(revsign=if_else(qtymain<=0,1,0),rmrk=1,ctype=20) %>%
    select(-qtymain)


  ## Map to back to cash flows to remove true negatives
  cf1<- trades %>%
    dplyr::filter(id %in% fPremium[["data"]]$id) %>%
    bind_rows(cashTrades) %>% ##[cashTrades$id==14247,] %>%
    unique()
  cf <- cf1 %>%
    mutate(valamt=round(valamt),isTransfer=if_else(ctype==30,TRUE,FALSE),isSuspect=TRUE,revsign=if_else(qtymain>0,1,0)) %>%
    left_join(othrFlowTrans,by=c("valamt","commitment","resmain_symbol"="symbol","ctype")) %>%
    left_join(othrFlowTrade,by=c("commitment","remarks","revsign","ctype")) %>%
    left_join(fPremium[["data"]],by="id") %>%
    mutate(isSuspect=if_else(is.na(inout)&is.na(rmrk)&abs(valamt)>=transferMin*fPremium[["premium"]],isSuspect,FALSE),
           ## isTransferSus=if_else(abs(qtymain)<transferMin*fPremium[["premium"]],FALSE,isTransfer),
           isInitialPremiumSuspect=if_else(##aggvalamt==round(fPremium[["premium"]]) & commitment==fPremium[["date"]],TRUE,FALSE
             id %in% fPremium[["data"]]$id,TRUE,FALSE
             ),
           misclassified=if_else(isInitialPremiumSuspect & resmain_ctype!="CCY",TRUE,FALSE)

    ) %>%
    select(-inout,-rmrk,-revsign)

  if(NROW(cf)!=NROW(cf1)) {
    ##print("Record discrepancy identified, halt process.")
    ##return(NULL)
    stop("Record discrepancy identified, halt process.")
  }

  ##if(sum(cf$isInitialPremiumSuspect,na.rm=TRUE)==0) {
  ##
  ##cf <- cf %>%
  ##  bind_rows(trades %>% dplyr::filter(id %in% fPremium[["data"]]$id) %>% mutate(valamt=round(valamt),isTransfer=if_else(ctype==30,TRUE,FALSE),isSuspect=TRUE,isInitialPremiumSuspect=TRUE,misclassified=TRUE))
  ##
  ##print("Missclassification Suspect")
  ##}

  if(sum(cf$isInitialPremiumSuspect,na.rm=TRUE)==0) {
    ##print("Initial premium record(s) missing, halt process.")
    ##return(NULL)
    stop("Initial premium record(s) missing, halt process.")
  }

  ##dupes <- cf %>% group_by(id) %>% mutate(n=n()) %>% arrange(desc(n))

  ## ADD THIS BACK
  for(i in 1:length(fpKeywords)) {
   cf <- cf %>%
     mutate(isSuspect=ifelse(grepl(tolower(paste(fpKeywords[[i]],collapse="|")),tolower(!!sym(names(fpKeywords)[[i]]))),FALSE,isSuspect))
  }


 ##flag <- lapply(seq_along(fpKeywords),
 ##               function (i) {
 ##                 t <- grepl(tolower(paste(fpKeywords[[i]],collapse="|")),tolower(cf[,names(fpKeywords)[[i]]]))
 ##               }
 ##) %>%
 ##  as.data.frame() %>%
 ##  rowSums()
 ##
 ##cf[as.logical(flag),"isSuspect"] <- FALSE


  cashFlowMapd <- cf %>%
    mutate_if(is.logical,function(x) ifelse(is.na(x),FALSE,x)) %>%
    mutate(initialPremium=fPremium[["premium"]],initialPremiumDate=fPremium[["date"]]) %>%
    ## mutate_at(c("isTransfer", "isSuspect", "isFirstTransfer"),function(x) ifelse(is.na(x),FALSE,x)) %>%
    select(!contains(c("isInitialPremiumSuspect", "isTransfer", "isSuspect","misclassified")), isInitialPremiumSuspect, isTransfer, isSuspect,misclassified)

  return(cashFlowMapd)

}
