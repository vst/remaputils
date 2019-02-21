##' A function to push ohlc observations
##'
##' This is the description
##'
##' @param symbol The vector of symbols.
##' @param close The vector of close values.
##' @param date The vector of dates.
##' @param session The rdecaf session.
##' @return Returns NULL
##' @export
pushOhlc <- function(symbol, close, date, session) {

    ## Construct the data-frame:
    ohlcObs <- data.frame("symbol"=symbol,
                          "close"=close,
                          "date"=date,
                          stringsAsFactors=FALSE)

    ## Create batches:
    batches <- createBatches(NROW(ohlcObs), 500)

    ## Iterate over batches and push:
    for (i in 1:length(batches[[1]])) {

        ## The starting index:
        strt <- batches[["startingIdx"]][i]

        ## The ending index:
        ends <- batches[["endingIdx"]][i]

        ## Get the payload:
        payload <- toJSON(ohlcObs[strt:ends,], auto_unbox=TRUE, na = c("null"))

        print(paste0("Posting prices ", strt, ":", ends, " of ", NROW(ohlcObs)))

        ## Push:
        result <- httr::POST(paste0(session[["location"]], "/ohlcobservations/updatebulk/"),
                             httr::authenticate(session[["username"]], session[["password"]]),
                             body=payload,
                             httr::add_headers(.headers = c("Content-Type"="application/json")))
    }

}


##' A function to inbulk portfolios.
##'
##' This is the description
##'
##' @param guid The vector with guids. If NULL (default), creates using guidPrefix and xguid.
##' @param name The vector with names.
##' @param rccy The vector of currencies.
##' @param team The vector of teams id's.
##' @param guidPrefix A string to use as prefix (e.g Custodian Name)
##' @param xguid A vector with the id to be transformed to guid.
##' @param session The rdecaf session
##' @return Returns NULL
##' @export
inbulkPortfolio <- function (guid=NULL, name, rccy, team, guidPrefix=NULL, xguid=NULL, session) {

    ## If guid is null, construct one:
    if (is.null(guid)) {
        guid <- as.character(sapply(paste0(guidPrefix, xguid), function(id) paste0("~XID.portfolio.", digest::digest(id))))
    }

    ## Create the payload:
    payload <- jsonlite::toJSON(list("portfolios"=data.frame("guid"=guid,
                                                             "name"=name,
                                                             "rccy"=rccy,
                                                             "team"=team)), auto_unbox=TRUE, na="null")
    ## Sync portfolios:
    response <- rdecaf::postResource("imports/inbulk", params=list(sync="True"), payload=payload, session = session)

    ## Done, return:
    list("response"=response,
         "guid"=guid,
         "name"=name,
         "id"=sapply(response[[1]][["portfolios"]], function(x) x[[1]]))

}

##' A function to inbulk accounts.
##'
##' This is the description
##'
##' @param guid The vector with guids. If NULL (default), creates using guidPrefix and xguid.
##' @param name The vector with names.
##' @param portfolio The vector with portfolio id's.
##' @param custodian The vector with custodian id's.
##' @param guidPrefix A string to use as prefix (e.g Custodian Name)
##' @param xguid A vector with the id to be transformed to guid.
##' @param session The rdecaf session
##' @return Returns NULL
##' @export
inbulkAccount <- function (guid=NULL, name, portfolio, custodian, guidPrefix=NULL, xguid=NULL, session) {

    ## If guid is null, construct one:
    if (is.null(guid)) {
        guid <- as.character(sapply(paste0(guidPrefix, xguid), function(id) paste0("~XID.account.", digest::digest(id))))
    }

    ## Create the payload:
    payload <- toJSON(list("accounts"=data.frame("guid"=guid,
                                                 "name"=name,
                                                 "portfolio"=portfolio,
                                                 "custodian"=custodian)), auto_unbox=TRUE, na="null")
    ## Sync portfolios:
    response <- postResource("imports/inbulk", params=list(sync="True"), payload=payload, session = session)

    ## Done, return:
    list("response"=response,
         "guid"=guid,
         "name"=name,
         "id"=sapply(response[[1]][["accounts"]], function(x) x[[1]]))

}


##' A function to post resources in batches.
##'
##' This is the description
##'
##' @param resources The resource data frame.
##' @param batchSize The size of the batches.
##' @param session The DECAF session info.
##' @return Returns NULL
##' @export
postResourcesByBatch <- function(resources, batchSize=1000, session) {

    ## Initialise the ending index:
    endingIdx <- seq(0, NROW(resources), batchSize)

    ## Create the starting index:
    startingIdx <- endingIdx + 1

    ## Update the ending index:
    endingIdx <- c(tail(endingIdx, -1), NROW(resources) - tail(endingIdx, 1) + tail(endingIdx, 1))

    ## Push the resources:
    for (i in 1:length(endingIdx)) {

        ## Batch:
        batch <- resources[startingIdx[i]:endingIdx[i], ]

        print(paste0("Posting batch: ", startingIdx[i], ":", endingIdx[i]))

        ## Push the resources:
        result <- rdecaf::postResource("resources", "imports", payload=jsonlite::toJSON(batch, auto_unbox=TRUE), session=session)

    }

    return(NULL)

}


##' A function to get trades from session using container names:
##'
##' This is the description
##'
##' @param containerNames A vector with container names:
##' @param containerType The container type
##' @param session The DECAF session info.
##' @return A data-frame with DECAF trades for portfolio.
##' @import rdecaf
##' @export
getContainer <- function(containerNames, containerType, session) {

    ## If no container names, return NULL:
    if (NROW(containerNames) == 0) {
        return(NULL)
    }

    ## If container type is accounts, run:
    if (containerType == "accounts") {

        ## Construct the account params:
        params <- list("page_size"=-1,
                       "name__in"=paste(containerNames, collapse=","))

        ## Get the portfolios:
        container <- safeRbind(getResource(containerType, params=params, session=session))

    }

    ## If container type is portfolios, run:
    if (containerType == "portfolios") {

        ## Construct the portfolio params:
        params <- list("page_size"=-1,
                       "format"="csv",
                       "name__in"=paste(containerNames, collapse=","))

        ## Get the portfolios:
        container <- as.data.frame(getResource(containerType, params=params, session=session))

    }

    ## Done, return:
    container

}


##' A function to provide the enriched trade data-frame from source session.
##'
##' This is the description
##'
##' @param sourceSession The rdecaf session for source instance.
##' @param targetSession The rdecaf session for target instance.
##' @param containerMap TODO:
##' @param containerType TODO:
##' @param gte The commitment date after which the trades should be considered. Default is NULL.
##' @return A trade data-frame with mapped/created accmains and auxiliary resource information.
##' @export
xDecafPreemble <- function(sourceSession, targetSession, containerMap, containerType, gte=NULL) {

    ## Print message:
    print(paste0("Retrieving trades by container names from source for ", containerType, " ..."))

    ## Get the trades by portfolio name:
    tradesAndContainer <- getTradesFromContainerNames(names(containerMap),
                                                      sourceSession,
                                                      type=containerType,
                                                      gte=gte)

    ## Get the vision portfolios:
    sourceContainer <- tradesAndContainer[["container"]]

    ## Get the vision trades by portfolio:
    sourceTrades <- tradesAndContainer[["trades"]]

    ## Get the portfolio account map:
    portAccMap <- as.data.frame(getAccountPortfolioMap(sourceContainer, containerType))

    ## Match and add the ucpbh names to the portAccMap
    portAccMap[, "target"] <- sapply(containerMap[match(portAccMap[, "container_name"], names(containerMap))], function(x) x[["tcontainername"]])

    ## Add the container type information:
    portAccMap[, "containerType"] <- rep(containerType, NROW(portAccMap))

    taccountname <- lapply(containerMap, function(x) x[["taccountname"]])
    taccountname <- sapply(taccountname, function(x) ifelse(is.null(x), NA, x))
    portAccMap[, "taccountname"] <- taccountname

    ## Print message:
    print(paste0("Retrieving stocks and corresponding resources from source for ", containerType, " ..."))

    ## Get stocks:
    sourceStocks <- getStocks(sourceContainer, sourceSession, zero = 1, date = Sys.Date(), c = substr(containerType, 1, nchar(containerType) -1))

    ## Get the vision resources by stock:
    sourceResources <- getResourcesByStock(sourceStocks, sourceSession)

    ## Return, if no trades:
    if (NROW(sourceTrades) == 0) {
        return(list("trades"=NULL,
                    "resources"=sourceResources,
                    "portAccMap"=portAccMap))
    }

    ## Append the ucapbh portfolioNames to the vision trades:
    sourceTrades <- data.frame(sourceTrades, "port_name_target"=portAccMap[match(sourceTrades[, "accmain"], portAccMap[, "account"]), "target"],
                               stringsAsFactors=FALSE)

    sourceTrades <- data.frame(sourceTrades, "acc_name_target"=portAccMap[match(sourceTrades[, "accmain"], portAccMap[, "account"]), "taccountname"],
                               stringsAsFactors=FALSE)

    ## Append the instrument currencies to the vision trades:
    sourceTrades <- data.frame(sourceTrades, "ccymain"=as.character(sourceResources[match(sourceTrades[, "resmain"], sourceResources[, "id"]), "ccymain"]),
                               stringsAsFactors=FALSE)

    ## Print message:
    print("Mapping / Creating accounts ...")

    ## Map and overwrite the vision accmain id's with ucapbh accmain id's:
    sourceTrades[, "accmain"] <- accountMapper(portfolio=trimConcatenate(sourceTrades[, "port_name_target"]),
                                               currency=as.character(sourceTrades[,"ccymain"]),
                                               session=targetSession,
                                               accountName=sourceTrades[, "acc_name_target"])

    ## Map and append the resource quantity to vision trades:
    sourceTrades <- data.frame(sourceTrades,
                               "resmain_quantity_source"=as.character(sourceResources[match(sourceTrades[, "resmain"], sourceResources[, "id"]), "quantity"]),
                               stringsAsFactors=FALSE)

    ## Map and append the resource isin to vision trades:
    sourceTrades <- data.frame(sourceTrades, "isin"=sourceResources[match(sourceTrades[, "resmain"], sourceResources[, "id"]), "isin"],
                               stringsAsFactors=FALSE)

    ## Map and append the resource name to vision trades:
    sourceTrades <- data.frame(sourceTrades, "name"=sourceResources[match(sourceTrades[, "resmain"], sourceResources[, "id"]), "name"],
                               stringsAsFactors=FALSE)

    ## Map and append the resource name to vision trades:
    sourceTrades <- data.frame(sourceTrades, "resmain_ccymain"=sourceResources[match(sourceTrades[, "resmain"], sourceResources[, "id"]), "ccymain"],
                               stringsAsFactors=FALSE)

    ## Done, return:
    list("trades"=sourceTrades,
         "resources"=sourceResources,
         "portAccMap"=portAccMap)

}



##' A function to return a flat account to portfolio mapping using the portfolio data-frame
##'
##' This is the description
##'
##' @param container The rdecaf portfolio data-frame
##' @param containerType TODO:
##' @return A data-frame with the flat account to portfolio id's
##' @export
getAccountPortfolioMap <- function(container, containerType) {

    ## If accounts, map according to accounts and return:
    if (containerType == "accounts") {

        ## Get the account/portfolio information
        portAccMap <- cbind(container[, "id"], container[, "portfolio"], container[, "name"])

        ## Name the column:
        colnames(portAccMap) <- c("account", "portfolio", "container_name")

        ## Done, return:
        return(portAccMap)
    }

    ## Initialse the portAccMap
    portAccMap <- data.frame()

    ## Get the account indices in portfolio:
    accountIdx <- grep("accounts.", colnames(container))

    ## Iterate over the accIdx:
    for (accIdx in accountIdx) {

        portAccMap <- rbind(portAccMap, cbind(container[, accIdx], container[, "id"], container[, "name"]))
    }

    ## Get rid of NA accounts:
    portAccMap <- portAccMap[!is.na(portAccMap[,1]),]

    ## Name the column:
    colnames(portAccMap) <- c("account", "portfolio", "container_name")

    ## Done, return:
    portAccMap

}


##' A function to push a payload to a decaf instance.
##'
##' This is the description
##'
##' @param payload The payload in json.
##' @param endpoint The endpoint to be pushed to.
##' @param session The rdecaf session.
##' @param import Boolean to indicate whether to use the import extension. Default is TRUE.
##' @param inbulk Boolean to indicate whether to use the inbulk extension. Default is FALSE.
##' @param params Additional parameters.
##' @return A list with the httr response and a message.
##' @import rdecaf
##' @export
pushPayload <- function(payload, endpoint, session, import=TRUE, inbulk=FALSE, params=NULL) {

    ## If import, then extend the url with "imports" and push payload:
    if (import & !inbulk) {

        response <- try(postResource(endpoint, "imports", payload=payload, session=session), silent=TRUE)
        response <- response[[1]]
    }

    ## If inbulk, extend the url and push payload:
    if (inbulk) {

        response <- try(postResource("imports/inbulk", payload=payload, params=params, session=session), silent=TRUE)
        response <- list("id"=response)
    }

    ## If not import, use the endpoint itself and push payload:
    if (!import & !inbulk) {
        response <- try(postResource(endpoint, payload=payload, session=session), silent=TRUE)
    }

    ## Handle the return value and message:
    if (class(response) == "try-error"){
        val <- NA
        msg <- "ERROR"
    } else {
        val <- response$id
        msg <- "SUCCESS"
    }

    ## Done, return:
    list("id"=val,
         "msg"=msg)

}


##' A function to create a trade reference.
##'
##' This is the description
##'
##' @param date The trade date.
##' @param account The account id.
##' @param resource The resource id.
##' @param qty The quantity of the trade.
##' @param px The price of the trade.
##' @param ext A custom extension. Default is NULL.
##' @return A reference string.
##' @export
createReferenceTrade <- function(date, account, resource, qty, px, ext=NULL){

    ## Construct the record reference.
    ## 1. Date as numeric
    ## 2. Account id
    ## 3. Resource id (max 6 characters)
    ## 4. Number of characters in qtymain (max 12 characters)
    ## 5. First 4 characters of abs(qtymain) * 100000
    ## 6. Buy or Sell indicator ("B" or "S")
    paste0(as.numeric(as.Date(date)),
           account,
           substr(resource, 1, 6),
           ifelse(nchar(qty) > 12, 12, nchar(qty)),
           substr(as.numeric(abs(qty)*100000), 1, 4),
           ifelse(qty > 0, "B", "S"),
           substr(as.numeric(abs(px)*100000), 1, 4),
           ext)
}


##' A function to create the file pipeline for decaf-imports.
##'
##' This is the description
##'
##' @param isLocal Boolean to indicate whether we are in a test environment. Default is FALSE.
##' @param providers A vector with file provider names. If NULL, all existing providers will be considers. Default is NULL.
##' @param append A vector with non-file data providers (i.e API). Any non-file data provider will be appended to the pipeline. Default is NULL.
##' @param processedFiles Boolean to indicate whether processed files should be included. Default is FALSE.
##' @param ignores A vector with key words to ignore for the pipeline.
##' @return A reference string.
##' @export
getPipeline <- function(isLocal,
                        providers=NULL,
                        append=NULL,
                        processedFiles=FALSE,
                        ignores=c(".zip", ".xml", ".txt", "provider_resource_id.csv")){

    ## List all the folders under files:
    folders <- list.files("files/")

    ## Exclude the archive folder:
    folders <- folders[-grep("archive", folders)]

    ## Exclude the archive folder:
    folders <- folders[-grep("stashed", folders)]

    ## Exclude the incoming folder:
    folders <- folders[-grep("incoming", folders)]

    ## List all the files under the folders:
    files <- do.call(c, lapply(folders, function(f) list.files(paste0("files/", f), full.names=TRUE, recursive=TRUE)))

    ## Ignore zips:
    ignoreIdx <- unique(do.call(c, lapply(ignores, function(x) grep(x, files))))

    if (length(ignoreIdx) > 0) {
        files <- files[-ignoreIdx]
    }

    ## Grep temporary files:
    tempFiles <- grep("\\~", files)

    ## Exclude temporary files, if any:
    if (length(tempFiles) > 0) {
        files <- files[-grep("\\~", files)]
    }

    ## Read processed files:
    processed <- as.character(read.csv("files/processed.csv", header=FALSE)[,1])

    ## If processed files should be returned as well overwrite processed file names:
    if (processedFiles) {
        processed <- getRandString(12)
    }

    ## Get the list of file names:
    fileNames <- do.call(c, lapply(strsplit(files, "/"), function(f) f[length(f)]))

    ## Match processed files:
    matchedFiles <- match(fileNames, processed)

    ## List all the files under the folders:
    files <- files[is.na(matchedFiles)]

    ## If only single provider should be returned, filter:
    if (!isNAorEmpty(providers)) {
        files <- files[sapply(strsplit(files, "/"), function(x) !all(is.na(match(providers, x[2]))))]
    }

    ## Check if any file is zip:
    isZip <- safeGrep(files, ".zip") == "1"

    if (any(isZip)) {

        zipFiles <- files[isZip]

        extDirs <- sapply(strsplit(zipFiles, "/"), function(x) paste(x[-length(x)], collapse="/"))

        unzip(zipFiles, exdir=extDirs)

        file.remove(zipFiles)

        unzippedFiles <- list.files(extDirs, recursive=TRUE)

        fileNames <- do.call(c, lapply(strsplit(unzippedFiles, "/"), function(f) f[length(f)]))

        matchedFiles <- match(fileNames, processed)

        unzippedFiles <- unzippedFiles[is.na(matchedFiles)]

        files <- c(files, unzippedFiles)

    }

    ## Append the api providers:
    files <- c(files, append)

    ## Finally, return:
    return(files)

}


##' A function to queue incoming files to be considered for file pipeline.
##'
##' This is the description
##'
##' @param mapper The income file mapper.
##' @return Produces a log of the queueing process and writes to the log of particular session.
##' @export
queueIncoming <- function(mapper){

    ## Get the incoming files:
    incoming <- list.files("files/_incoming", recursive=TRUE)

    ## Get the incoming files full path:
    incomingFull <- list.files("files/_incoming", full.names=TRUE, recursive=TRUE)

    ## Assign files to the folders in list:
    folderAssignment <- lapply(names(mapper), function(m) incoming[grep(m, trimConcatenate(incoming))])

    ## Folder assignment full:
    folderAssignmentFull <- lapply(names(mapper), function(m) incomingFull[grep(m, trimConcatenate(incoming))])

    ## Assign the folder names:
    names(folderAssignment) <- mapper

    ## Iterate over the assigned folders:
    for (i in 1:length(folderAssignment)){

        ## Get current folder:
        files <- folderAssignment[[i]]

        ## Current folder full:
        filesFull <- folderAssignmentFull[[i]]

        ## Ingore, if not file:
        if (length(files) == 0){
            next
        }

        ## Is folder?
        isFolder <- safeGrep(files, "/") == "1"

        ## If any is folder, reassign file name:
        if (any(isFolder)) {
            files[isFolder] <- sapply(strsplit(files[isFolder], "/"), function(x) tail(x, 1))
        }

        ## Gauge if it already has file name extension:
        hasFileExt <- nchar(files) > 30

        ## If yes, then keep same name. Else create new name:
        if (all(hasFileExt)) {
            newFiles <- files
        } else {
            ## Set new file name(s):
            newFiles <- do.call(c, lapply(files, function(f) extendFileName(f, randomStrN=4)))
        }

        ## Set the new path(s):
        newPath <- paste0("files/", mapper[i], "/", newFiles)

        ## Move files:
        file.rename(filesFull, newPath)

        ## Message:
        messageHandler(infotext=paste0("Queued ", newPath))
    }

    ## Get the incoming files:
    incoming <- list.files("files/_incoming")
    incomingFull <- list.files("files/_incoming", full.names=TRUE)

    ## Iterate over the incoming files, if any:
    if (length(incoming) > 0){

        ## Iterate over incoming files:
        for (i in 1:length(incoming)){

            ## Incoming file:
            rem <- incoming[i]

            ## Memorise the original name:
            remOrg <- rem

            ## Memorise the full path:
            remFull <- incomingFull[i]

            ## Get the file extension:
            ext <- sapply(strsplit(rem, "\\."), function(x) tail(x, 1))

            ## Upper case the incoming file name without the extension:
            rem <- toupper(gsub("\\.", "", gsub(ext, "", rem)))

            ## Get rid of unnecessary strings:
            rem <- trimws(gsub("\\(1)", "", rem))

            ## Compute a coefficient to be applied to the jaccard string distance.
            ## The less characters the string has, the lower the coefficient, hence
            ## more permissable in the matching process.
            coeff <- 1 - (1 / nchar(rem))
            strDist <- stringdist(rem, as.character(mapper), method="jaccard") * coeff

            ## The fuzzy matching has to comform with minimum requirements:
            ## 1. Jaccard coefficient has to be below 0.2.
            ## 2. The string distance coefficient has to be the minimum.
            ## 3. The string distance coefficient may not be -Inf.
            remAssignment <- strDist < 0.2 & strDist == min(strDist) & abs(strDist) < Inf

            ## If matching criteria are met, place file in the right folder:
            if (any(remAssignment)) {
                ## Exten the file name with a random string:
                newFile <- extendFileName(remOrg, randomStrN=4)
                ## Set the new path(s):
                newPath <- paste0("files/", mapper[remAssignment], "/", newFile)
                ## Move files:
                file.rename(remFull, newPath)
            }
        }
    }

    ## Check the incoming files:
    incoming <- list.files("files/_incoming")

    if (length(incoming) > 0){
        ## Message:
        messageHandler(warntext=paste0(". File could NOT be queued:", incoming))
    }
}


##' A function runs the java demail.
##'
##' This is the description
##'
##' @param address The address of the email server.
##' @param jarPath The path of the demail jar file.
##' @param host The name of the email host.
##' @param port The port of the email server.
##' @param folder The name of the folder on the email server.
##' @param directory The local directory to sync to.
##' @param archive The name of the archive folder on server.
##' @param exclude TODO
##' @param since TODO
##' @param until TODO
##' @return A message to show the log of the demail process.
##' @export
runDemail <- function(address, jarPath, host, port, folder, directory, archive, exclude=NULL, since=Sys.Date(), until=Sys.Date()){

    ## Get user name of email:
    user <- strsplit(address, "@")[[1]][1]

    ## Get the email pw:
    demailpw <- jsonlite::fromJSON("~/.decaf.json")[["settings"]][["demail"]][[user]]

    ## Comman-line friendly parse the email password
    demailpw <- as.factor(paste0("'", as.factor(demailpw), "'"))

    ## Construct the java jar command argument:
    jarsArg <- sprintf("java -jar %s", jarPath)
    ## jarsArg <- sprintf("java -Dmail.imap.fetchsize=1048576 -jar %s", jarPath)
    jarsArg <- "demail"

    ## Construct the host command argument:
    hostArg <- sprintf("--host %s", host)

    ## Construct the port command argument:
    portArg <- sprintf("--port %s", port)

    ## Construct the user command argument:
    userArg <- sprintf("--user %s", address)

    ## Construct the user command argument:
    passArg <- sprintf("--pass %s", demailpw)

    ## Construct folder command argument:
    fldrArg <- sprintf("--folder %s", folder)

    ## Construct the directory command argument:
    dirsArg <- sprintf("--directory %s", directory)

    ## Construct the archive command argument:
    archArg <- sprintf("--archive %s", archive)

    ##sincArg <- sprintf("--since %s", "2018-11-08")
    sincArg <- sprintf("--since %s", since)

    ##untlArg <- sprintf("--until %s", "2018-11-08")
    untlArg <- sprintf("--until %s", until)

    ## Construct the command:
    runCmd <- paste(jarsArg,
                    "download-attachments",
                    hostArg,
                    portArg,
                    "--ssl",
                    userArg,
                    passArg,
                    fldrArg,
                    dirsArg,
                    archArg,
                    sincArg,
                    untlArg)

    print(runCmd)

    ## Run demail:
    result <- try(system(runCmd), silent=TRUE)

    ## Handle message:
    messageHandler(result, 1, errortext=" Couldn't run demail command.")
 }


##' A function to call the system command rsync
##'
##' This is the description
##'
##' @param server The server address
##' @param location The location on the server.
##' @param target The target directory locally.
##' @param folderNames The names of the local folders.
##' @param exclude A vector with keys to exclude from syncin.
##' @return A message to show the log of the rsync call.
##' @export
rSync <- function(server, location, target, folderNames, exclude=""){

    ## If we have to exclude keywords, overwrite exclude:
    if (any(!isNAorEmpty(exclude))) {
        exclude <- paste("--exclude", exclude, collapse=" ")
    }

    ## If servers is local, omit server string:
    server <- ifelse(server=="local", "", paste0(server, ":"))

    ## Run the rsync commands:
    for (f in 1:length(folderNames)) {
        cmd <- trimDws(sprintf("rsync -auzvP %s %s%s%s/ %s%s/", exclude, server, location, names(folderNames)[f], target, folderNames[f]))
        result <- try(system(cmd), silent = TRUE)
    }

    ## Message:
    messageHandler(result, 0, condition="greater", errortext=paste0(" error code: ", result))
}
