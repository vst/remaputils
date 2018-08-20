##' A function to push a payload to a decaf instance.
##'
##' This is the description
##'
##' @param payload The payload in json.
##' @param endpoint The endpoint to be pushed to.
##' @param session The rdecaf session.
##' @param import Boolean to indicate whether to use the import extension. Default is TRUE.
##' @return A list with the httr response and a message.
##' @import rdecaf
##' @export
pushPayload <- function(payload, endpoint, session, import=TRUE){

    ## If import, then extend the url with "imports" and push payload:
    if (import) {
        response <- try(postResource(endpoint, "imports", payload=payload, session=session), silent=TRUE)
        response <- response[[1]]
    }

    ## If not import, use the endpoint itself and push payload:
    if (!import) {
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

    if (length(ignoreIdx) > 0){
        files <- files[-ignoreIdx]
    }

    ## Grep temporary files:
    tempFiles <- grep("\\~", files)

    ## Exclude temporary files, if any:
    if (length(tempFiles) > 0){
        files <- files[-grep("\\~", files)]
    }

    ## Read processed files:
    processed <- as.character(read.csv("files/processed.csv", header=FALSE)[,1])

    ## If processed files should be returned as well overwrite processed file names:
    if (processedFiles){
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
    incoming <- list.files("files/_incoming")

    ## Get the incoming files full path:
    incomingFull <- list.files("files/_incoming", full.names=TRUE)

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
##' @return A message to show the log of the demail process.
##' @export
runDemail <- function(address, jarPath, host, port, folder, directory, archive){

    ## Get user name of email:
    user <- strsplit(address, "@")[[1]][1]

    ## Get the email pw:
    demailpw <- jsonlite::fromJSON("~/.decaf.json")[["settings"]][["demail"]][[user]]

    ## Comman-line friendly parse the email password
    demailpw <- as.factor(paste0("'", as.factor(demailpw), "'"))

    ## Construct the java jar command argument:
    jarsArg <- sprintf("java -jar %s", jarPath)

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

    ## Run demail:
    result <- try(system(paste(jarsArg,
                               "download-attachments",
                               hostArg,
                               portArg,
                               "--ssl",
                               userArg,
                               passArg,
                               fldrArg,
                               dirsArg,
                               archArg)), silent=TRUE)
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

    ## Run the rsync commands:
    for (f in 1:length(folderNames)) {
        result <- try(system(trimDws(sprintf("rsync -auzvP %s %s:%s%s/ %s%s/", exclude, server, location, names(folderNames)[f], target, folderNames[f]))), silent=TRUE)
    }

    ## Message:
    messageHandler(result, 0, condition="greater", errortext=paste0(" error code: ", result))
}
