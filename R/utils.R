##' A function to check multiple conditions row-wise in data-frame.
##'
##' This is a description.
##'
##' @param df A data-frame.
##' @param inCols TODO
##' @param candCols TODO
##' @param inKeys TODO
##' @param exKeys TODO
##' @param inclToIns TOOD
##' @return Returns a vector with logicals.
##' @export
getIndex <- function(df, inCols, candCols=NULL, inKeys=NULL, exKeys=NULL, inclToIns=NULL) {

    ## If 1 inKeys in provided, duplicate:
    if (length(inKeys) == 1) {
        inKeys <- c(inKeys, inKeys)
    }
    ## If 1 exKeys in provided, duplicate:
    if (length(exKeys) == 1) {
        exKeys <- c(exKeys, exKeys)
    }

    ## Define a auxiliary function:
    auxFun <- function(df, x) {
        if (is.null(x)) {
            return(x)
        }
        apply(do.call(cbind, lapply(1:length(x), function(i) {
            apply(mgrep(df[, names(x)[i]], x[[i]]), MARGIN=1, function(z) any(z != "0"))
        })), MARGIN=1, any)
    }

    ## Define a auxiliary function:
    auxFun2 <- function(df, x) {
        if (is.null(x)) {
            return(rep(FALSE, NROW(df)))
        }
        apply(do.call(cbind, lapply(1:length(x), function(i) {
            .x <- apply(df[, names(x)], MARGIN=2, toupper)
            mxgrep(.x, toupper(x[[i]]))
        })), MARGIN=1, function(z) any(!is.na(z)))
    }

    ## Get the inclusive column logicals:
    inCols <- auxFun(df, inCols)

    ## Get the candidate column logicals:
    candCols <- auxFun(df, candCols)

    ## Get the inclusive key column logicals:
    inKeys <- auxFun2(df, inKeys)

    ## Get the exclusive key column logicals:
    exKeys <- auxFun2(df, exKeys)

    ## If some keys are automatically to be considered in inclusive key column, add:
    if (!is.null(inclToIns)) {
        inKeys <- inKeys | apply(do.call(cbind, lapply(1:length(inclToIns), function(i) {
            apply(mgrep(df[, names(inclToIns)[i]], inclToIns[[i]]), MARGIN=1, function(x) any(x != "0"))
        })), MARGIN=1, any)
    }

    ## Run the condition and return:
    inCols | candCols & inKeys & !exKeys

}


##' This function checks if value is a whole number:
##'
##' This is a description.
##'
##' @param x A vector with values.
##' @param tol The tolerance.
##' @return Returns a boolean vector.
##' @export
isWholenumber <- function(x, tol=.Machine$double.eps^0.5)  {

    ## Initialise the return value:
    retval <- rep(FALSE, length(x))

    ## Are the values numeric?
    isNumeric <- !is.na(suppressWarnings(sapply(x, function(z) try(as.numeric(z), silent=TRUE))))

    ## Set non-numeric values to FALSE:
    retval[!isNumeric] <- FALSE

    ## For any numeric value, check if it is integer:
    if (any(isNumeric)) {
        retval[ isNumeric] <- abs(as.numeric(x[isNumeric]) - round(as.numeric(x[isNumeric]))) < tol
    }

    ## Return boolean:
    retval
}


##' This function greps single key in all columns of a data-frame.
##'
##' This is a description.
##'
##' @param df A data-frame.
##' @param key The key to be grepped.
##' @return Returns a data-frame with the grepped key.
##' @export
mxgrep <- function(df, key) {

    ## Get the key list:
    kList <- lapply(colnames(df), function(x) ifelse(safeGrep(df[, x], key) == "1", df[, x], NA))

    ## Get the avaloq identifiers:
    apply(do.call(cbind, kList), MARGIN=1, function(x) ifelse(all(is.na(x)), NA, x[!is.na(x)]))
}


##' A function to grep multiple keys in vector.
##'
##' This is a description.
##'
##' @param x  A vector with strings.
##' @param keys A vector with keys to grep in x.
##' @return A 'x' by 'keys' size data frame with the grepped keys.
##' @export
mgrep <- function(x, keys) {

    ## For each key in keys, safely grep in vector x:
    kList <- lapply(keys, function(key) safeGrep(x, key))

    ## In each key list, replace the "1" (matched key) with the key itself:
    ## And combine to matrix:
    kMatrix <- do.call(cbind, lapply(1:length(kList), function(m) {
        kList[[m]][kList[[m]] == "1"] <- keys[m]
        kList[[m]]
    }))

    ## Assign row names:
    rownames(kMatrix) <- x

    ## Assign column names:
    colnames(kMatrix) <- keys

    return(kMatrix)
}


##' A function to parse offset date time object.
##'
##' This is a description.
##'
##' @param x  A vector with offset date.
##' @param tz The desired time zone.
##' @param format The format of the input.
##' @return A list with the parsed date time.
##' @export
parseOffsetDateTime <- function (x, format="%Y-%m-%dT%H:%M:%OS%z", tz="Asia/Singapore") {

    ## Apply strptime to the offset date time object:
    datetime <- strptime(x, format=format, tz=tz)

    ## Return the date time list:
    list("datetime"=datetime,
         "date"=strftime(datetime, "%Y-%m-%d"),
         "time"=strftime(datetime, "%H:%M:%OS6"))
}


##' A function initialises a data frame with desired colnames and rows.
##'
##' This is a description.
##'
##' @param colNames A vector with desired column names.
##' @param nRow The desired number of rows. Default is 1.
##' @return A data-frame with NA's
##' @export
initDF <- function(colNames, nRow=1) {

    ## Create the data-frame:
    df <- as.data.frame(matrix(NA, nrow=nRow, ncol=length(colNames)),
                        check.names=FALSE)

    ## Assign the column names:
    colnames(df) <- colNames

    ## Done, return
    df
}


##' A function which tells whether now is desired day and/or time
##'
##' This is a description.
##'
##' @param tz The time zone
##' @param gte Current time has to be greater than this to be TRUE. Expressed in \%H:\%M:\%S.
##' @param lte Current time has to be less than this to be TRUE. Expressed in H:M:S.
##' @param inWeekdays Current day has to match with any of these days. Expressed in c('MON', 'TUE', ...). Default are all weekdays.
##' @param tDate The target date time object to check against. Default is Sys.time().
##' @return TRUE or FALSE.
##' @export
itsTime <- function(tz="UTC", gte="00:00:01", lte="23:59:59", inWeekdays=c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"), tDate=Sys.time()) {
    ## Get the current date time in desired time zone:
    currentDateTime <- as.POSIXct((format(tDate, "tz"=tz)))
    ## Get current time in desired time zone:
    currentTime <- format(currentDateTime, "%H:%M:%S")
    ## Get current weekday in desired time zone:
    currentWeekday <- toupper(weekdays(currentDateTime, abbreviate=TRUE))
    ## Is today the day?
    itsTime <- any(safeGrep(inWeekdays, currentWeekday) == "1")
    ## is now the time?
    itsTime & (currentTime > gte & currentTime < lte)
}


##' TODO:
##'
##' This is a description
##'
##' @param x TODO
##' @return TODO
##' @export
.removeList <- function(x) {
    x[!sapply(x, is.list)]
}


##' A function to provide the start and end indices of desired batches.
##'
##' This is a description.
##'
##' @param n The length
##' @param batchSize The desired batch size
##' @return A list with starting and ending indices
##' @export
createBatches <- function(n, batchSize) {

    ## Initialise the ending index:
    endingIdx <- seq(0, n, batchSize)

    ## Create the starting index:
    startingIdx <- endingIdx + 1

    ## Update the ending index:
    endingIdx <- c(tail(endingIdx, -1), n - tail(endingIdx, 1) + tail(endingIdx, 1))

    ## Done, return:
    list("startingIdx"=startingIdx,
         "endingIdx"=endingIdx)
}


##' A function to set specific columns to NULL
##'
##' This is a description.
##'
##' @param df A data-frame.
##' @param cols A vector with the selective column names.
##' @return A data-frame.
##' @export
setColsToNull <- function(df, cols) {

    ## Iterate over columns
    for (col in cols) {

        ## Set column to NULL:
        df[, col] <- NULL
    }

    ## Done, return:
    df
}


##' A function to treat special characters:
##'
##' This is a description.
##'
##' @param df A data-frame.
##' @param cols A vector with the selective column names to be treated.
##' @return A data-frame.
##' @export
specialCharacterTreater <- function(df, cols=NULL) {

    ## If cols param is null, use colnames:
    if (is.null(cols)) {
        cols <- colnames(df)
        }

    ## Iterate over cols:
    for (col in cols) {

        ## Remove non-ascii characters:
        df[, col] <- iconv(df[, col], "latin1", "ASCII", sub="")
    }

    ## Done, return
    df
}


##' A function to grep the NA and replace with ""
##'
##' This is a description.
##'
##' @param x A vector
##' @param repl The value to replace the NA's with
##' @return A vector with NA's removed.
##' @export
.replaceNA <- function(x, repl="") {

    ## Get the index with NA's
    naIdx <- grep("NA", x)

    ## If NA, replace with empty:
    if (length(naIdx) > 0){
        x[naIdx] <- repl
    }

    ## Return:
    x
}


##' A function to give the value of the nearest date.
##'
##' This is a description.
##'
##' @param targetDate The target date
##' @param data A data frame with the date and value column.
##' @param tolerance The maximum allowed distance in days.
##' @param dateField The column name of the date in data.
##' @param valueField The column name of the value field.
##' @param nameField The column name of the name field.
##' @return A list with date and value.
##' @export
valueOfNearestDate <- function(targetDate, data, tolerance, dateField="date", valueField="price", nameField=NULL) {

    ## Compute the date difference:
    dateDist <- data[, dateField] - targetDate

    ## Filter the data for negative distances:
    data <- data[dateDist <= 0,]
    dateDist <- dateDist[dateDist <= 0]

    ## If empty, return NA:
    if (length(dateDist) == 0) {
        return(list("date"=NA,
                    "value"=NA,
                    "name"=NA))
    }

    ## Get the minimum absolute distance:
    minDistIdx <- suppressWarnings(which(abs(dateDist) == min(abs(dateDist))))

    ## If distance is higher than tolerance, return NA
    if (all(abs(dateDist)[minDistIdx] > tolerance)) {
        return(list("date"=NA,
                    "value"=NA,
                    "name"=NA))
    }

    ## Done, return the value and corresponding date:
    return(list("date"=data[, dateField][minDistIdx],
                "value"=data[, valueField][minDistIdx],
                "name"=ifelse(is.null(nameField), NA, data[, nameField][minDistIdx])))

}


##' TODO
##'
##' This is a description.
##'
##' @param value TODO
##' @return TODO
##' @export
.emptyToNA <- function (value) {
    if(is.null(value) || length(value) == 0) {
        NA
    } else {
        value
    }
}


##' TODO
##'
##' This is a description.
##'
##' @param value TODO
##' @return TODO
##' @export
.emptyToNULL <- function (value) {
    if(is.null(value) || length(value) == 0) {
        NULL
    } else {
        value
    }
}


##' Formats the given ISO string date to european style.
##'
##' This is a description.
##'
##' @param value ISO string date.
##' @return European string date.
##' @export
.formatDate <- function (value) {
    ifelse(is.na(value) || is.null(value), NA, format(as.Date(value), "%d/%m/%Y"))
}


##' Stop with error if condition is FALSE.
##'
##' This is a description.
##'
##' @param condition Boolean value.
##' @param error Error message.
##' @return Nothing but exception!
##' @export
assertit <- function (condition, error) {
    if (!condition) {
        stop(error)
    }
}


##' TODO:
##'
##' TODO:
##'
##' @param df A data-frame.
##' @param inCols TODO
##' @param idCols TODO
##' @param addConditions TODO
##' @return A list with the row indices and the group indices of the strategy.
##' @export
indexCompiler <- function(df, inCols, idCols=NULL, addConditions=NULL) {

    ## If no idCols, set to inCols:
    if (is.null(idCols)) {
        idCols <- names(inCols)[1]
    }

    ## Initialise the reval:
    retval <- list("index"=NULL,
                   "grpIdx"=NULL)

    ## Which values in inclusive columns to be considered:
    consider <- apply(do.call(cbind, lapply(1:length(inCols), function(i) {
        apply(do.call(cbind, lapply(inCols[[i]], function(z) df[, names(inCols)[i]] == z)), MARGIN=1, any)
    })), MARGIN=1, all)

    ## Create the consider id:
    considerId <- ifelse(consider, "CONSIDER", sapply(1:NROW(df), function(z) getRandString()))

    ## Construct the composite id:
    idColVals <- apply(as.data.frame(df[, unlist(idCols)]), MARGIN=2, function(x) sapply(x, function(z) ifelse(is.na(z), getRandString(), z)))

    if (NROW(df) == 1) {
        df[, "composite"] <- paste0(trimDws(paste0(idColVals, collapse="")), considerId)
    } else {
        df[, "composite"] <- paste0(apply(idColVals, MARGIN=1, function(x) trimDws(paste0(x, collapse=""))), considerId)
    }

    ## Get the table with occurances:
    occurances <- data.frame(table(as.character(df[, "composite"])))

    ## Get the duplicated composite id's:
    isDuplicate <- !is.na(match(df[, "composite"], as.character(occurances[occurances[, "Freq"] == 2, 1])))

    ## Append the duplicated indices:
    df[isDuplicate, "idx"] <- which(isDuplicate)

    ## Filter the duplicated:
    duplicates <- df[isDuplicate, ]

    ## Separate the duplicated by unique composite id in list:
    duplicateWise <- lapply(unique(duplicates[, "composite"]), function(z) duplicates[z == duplicates[, "composite"], ])

    ## Iterate over additional conditions:
    for (cond in addConditions) {

        ## Return initial return value if empty:
        if (length(duplicateWise) == 0) {
            return(retval)
        }

        ## Only carry on with elements which fulfil the condition:
        duplicateWise <- duplicateWise[sapply(duplicateWise, cond)]

        ## Return initial return value if empty:
        if (length(duplicateWise) == 0) {
            return(retval)
        }

    }

    ## Append the indices of group to every element:
    duplicateWise <- lapply(duplicateWise, function(z) data.frame(z, "comp_idx"=(as.list(z[, "idx"]))))

    ## Safely bind the list:
    duplicates <- safeRbind(duplicateWise)

    ## Assing the index:
    retval[["index"]] = duplicates[, "idx"]

    ## Assign the group indices:
    retval[["grpIdx"]] = duplicates[, grep("comp_idx", colnames(duplicates))]

    ## Return:
    retval
}


##' A function to gsub multiple patterns.
##'
##' This is a description.
##'
##' @param str The original string.
##' @param patterns A vector with string patterns.
##' @param replace A string to replace the patterns with. Default is "".
##' @return The new string.
##' @export
mgsub <- function(str, patterns, replace="") {

    ## Iterate over the patterns:
    for (p in patterns) {
        ## gsub pattern with replacement
        str <- trimDws(gsub(p, replace, str))
    }

    ## Done, return:
    str
}


##' A function to gsub strings.
##'
##' This is a description.
##'
##' @param string The original string.
##' @param cleaningMaps The vector with the pattern and the replacement.
##' @return The cleaned string,
##' @export
stringCleaner <- function(string, cleaningMaps=c("&#62;LINK&#60;/a&#62;"=">LINK</a>",
                                                 "&#60;a href"="<a href")) {

    ## Clean the HTML text:
    for (row in 1:NROW(cleaningMaps)) {
        string <- gsub(names(cleaningMaps)[row], as.character(cleaningMaps[row]), string)
    }

    string
}


##' A function to convert integers to string deterministically.
##'
##' This is a description.
##'
##' @param int A vector of integers.
##' @return A vector with strings.
##' @export
integerToString <- function(int) {

    ## The work horse function:
    auxFun <- function(x) {
        paste(sapply(1:nchar(int), function(i) {
            toupper(letters)[as.numeric(substr(int, i, i))]
        }), collapse="")
    }

    ## Sapply and return:
    sapply(int, auxFun)

}


##' A function to order a data-frame based on multiple columns:
##'
##' This is a description.
##'
##' @param df A data-frame.
##' @param colNames The names of the columns to be considered for order. The columns must be numeric.
##' @return The ordered data-frame.
##' @export
nestedOrdering <- function(df, colNames) {
    ## Order and return:
    df[do.call(order, lapply(apply(df[, colNames], MARGIN=2, function(x) list(x)), unlist)), ]
}


##' A function to match a string with candidates:
##'
##' This is a description.
##'
##' @param sourceStr A vector with strings to match from.
##' @param targetStr A vector with strings to match against.
##' @param jaccard The acceptable threshold for the jaccard coefficient.
##' @return A vector with matching target indices:
##' @import stringdist
##' @export
stringMatch <- function(sourceStr, targetStr, jaccard=0.15){

    ## Try exact match:
    matchingIndices <- match(sourceStr, targetStr)

    ## If we have a match, return:
    if (all(!is.na(matchingIndices))) {
        return(matchingIndices)
    }

    ## The remaining source strings:
    remaining <- is.na(matchingIndices)

    ## Compute jaccard string distance coefficient:
    coef <- lapply(sourceStr[remaining], function(r) stringdist(r, targetStr, "jaccard"))

    ## Get the index of minimum acceptable string distance:
    candIdx <- lapply(coef, function(x) ifelse(min(x) > jaccard, NA, which(x == min(x))))

    ## If multiple matches, assign NA:
    candIdx <- lapply(candIdx, function(x) ifelse(length(x) > 1, NA, x))

    ## Reassign the remaining matches:
    matchingIndices[remaining] <- do.call(c, candIdx)

    return(matchingIndices)
}


##' A function to replace null and length 0 value with NA
##'
##' This is the description
##'
##' @param x A variable
##' @return Returns x or NA.
##' @export
safeNull <- function(x){

    ## If null, return NA:
    if (is.null(x)) {
        return(NA)
    }

    ## If length is zero, return NA:
    if (length(x) == 0){
        return(NA)
    }

    ## Else, return value:
    x
}


##' A function to trim string and transform to upper case.
##'
##' This is a description.
##'
##' @param str A string.
##' @return Returns the transformed string.
##' @export
trimConcatenate <- function(str){

    ## Trim all single white spaces:
    str <- trimws(toupper(str))

    ## Remove dots from string:
    str <- gsub("\\.", "",str)

    ## Iterate and remove double white spaces:
    for (i in 1:10){
        str <- gsub(" ", "", str)
    }

    ## Done, return:
    str
}


##' A function to check condition of numeric value:
##'
##' This is a description.
##'
##' @param vector The numeric value to check condition for.
##' @param val The numeric value to check condition against>
##' @param condition Either of 'equal', 'greater', 'smaller'.
##' @return Returns either NA, TRUE or FALSE.
##' @export
checkCondition <- function(vector, val, condition="equal"){

    ## Switch string to the operator:
    condition <- switch(condition, "equal"="==", "greater"=">", "smaller"="<")

    ## If such condition doesn't exist, return NA:
    if (is.null(condition)){
        print("No valid condition given. Choose from 'equal', 'greater', 'smaller'")
        return(NA)
    }

    ## Evaluate the condition:
    sapply(vector, function(x) eval(parse(text=paste0(x, condition, val))))
}


##' A function to extend file name by random/date string.
##'
##' This is a description.
##'
##' @param str A string.
##' @param customExt A custom extention of the string. Default is NULL.
##' @param randomStrN The random string character size. Default is NULL.
##' @param addTime Boolean to indicate whether time should be added. Default is TRUE.
##' @return The extended file name.
##' @export
extendFileName <- function(str, customExt=NULL, randomStrN=NULL, addTime=TRUE){

    ## Find the lost dot:
    lastDot <- regexpr("\\.[^\\.]*$", str)[1]

    ## Split string by last dot:
    prefix <- substr(str, 1, lastDot-1)
    suffix <- substr(str, lastDot, nchar(str))

    ## Add custom string:
    if (!is.null(customExt)){
        prefix <- paste0(prefix, "_", customExt)
    }

    ## Add random string:
    if (!is.null(randomStrN)){
        prefix <- paste0(prefix, "_", getRandString(len=randomStrN))
    }

    ## Add time:
    if (addTime) {
        time <- paste0(strsplit(as.character(Sys.time()), " ")[[1]], collapse="_")
        prefix <- paste0(prefix, "_", time)

        Sys.sleep(1)
    }

    ## Return:
    paste0(prefix, suffix)

}


##' A function to parse command line arguments
##'
##' This is a description.
##'
##' @param commandArgs Command line arguments whereby arguments sets are separated by --
##' @return A list with the arguments:
##' @import utils
##' @export
argsParser <- function(commandArgs){

    ## Get argument name indices:
    argNameIdx <- grep("--", commandArgs)

    ## Get the argument value indices:
    argValueIdx <- c(diff(argNameIdx) -1, length(commandArgs) - tail(argNameIdx, 1))

    ## Get the argument names:
    argNames <- gsub("--", "", commandArgs[argNameIdx])

    ## Initialise the return value:
    args <- list()

    ## Iterate over the argument name indices and fill args:
    for (i in 1:length(argNameIdx)){
        args[[argNames[i]]] <- commandArgs[seq(argNameIdx[i]+1, argNameIdx[i] + argValueIdx[i])]
    }

    ## Return:
    args
}


##' If try-error, return NA:
##'
##' This is a description.
##'
##' @param x A object of try.
##' @param nullToNa Boolean to indicate whether null should be replaced to NA. Default is TRUE.
##' @return NA or valid value.
##' @export
safeTry <- function(x, nullToNa=TRUE){

    ## If class is try-error, return NA:
    if (class(x) == "try-error") {
        return(NA)
    }

    if (nullToNa & is.null(x)){
        return(NA)
    }

    if (length(x) == 0) {
        return(NA)
    }

    ## Return:
    x
}


##' A function to produce dates in requested periodicity.
##'
##' This is a description.
##'
##' @param periodLetter Any of "D", "W", "M", "Q", "Y"
##' @param yearsOfHistory The amount of years to lookback. Default is 3.
##' @param endOfWeek For weekly periodicity, the end of week day. Default is "Friday".
##' @return A sequence of dates for periodicity.
##' @export
periodDates <- function(periodLetter, yearsOfHistory=3, endOfWeek="Friday"){

    ## Generate date sequence:
    dateSeq <- seq(Sys.Date() - (365*yearsOfHistory), Sys.Date(), by=1)

    ## The function map:
    functionMap <- list("D"=function(x){x},
                        "W"=function(x){
                            x[weekdays(x) == "Friday"]
                        },
                        "M"=function(x){
                            x[!months(x) == c(tail(months(x), -1), tail(months(x) , 1))]
                        },
                        "Q"=function(x) {
                            x[!quarters(x) == c(tail(quarters(x), -1), tail(quarters(x) , 1))]
                        },
                        "Y"=function(x){
                            x[!substr(x, 1, 4) == c(tail(substr(x, 1, 4), -1), tail(substr(x, 1, 4) , 1))]
                        })

    ## Apply function and return:
    do.call(functionMap[[periodLetter]], list(dateSeq))

}


##' A function to get the date of a specific verbosely expressed date (expressed by memnonic).
##'
##' This is a description
##'
##' @param memnonic Follow the convention: [Period Letter] - [lookback], e.g "D-0" or "W-3".
##' @return The specific date of the date memnonic.
##' @export
dateOfPeriod <- function(memnonic="D-0"){

    ## Get current date:
    today <- Sys.Date()

    ## Get the period letter:
    period <- strsplit(memnonic, "-")[[1]][1]

    ## Get the lag:
    lag <- as.numeric(strsplit(memnonic, "-")[[1]][2])

    ## Get the period dates
    series <- periodDates(period)

    ## Apply lag and return:
    series[length(series) - lag]

}


##' A function to safely rbind a list with data frames.
##'
##' This is a description.
##'
##' @param x A list with data frames.
##' @return A single data frame.
##' @export
safeRbind <- function(x){

    if (length(x) == 0) {
        return(NULL)
    }

    if (is.list(x[[1]])) {
        colNames <- unique(do.call(c, lapply(x, names)))
        df <- as.data.frame(matrix(NA, ncol=length(colNames), nrow=length(x)), check.names=FALSE)
    } else {
        ## Ge the unique column names of all ucapbh resources:
        colNames <- unique(do.call(c, lapply(x, colnames)))
        df <- as.data.frame(matrix(NA, ncol=length(colNames), nrow=sum(sapply(x, NROW))), check.names=FALSE)
    }

    ## Reassign the column names:
    colnames(df) <- colNames

    ## Initialse the starting row:
    sRow <- 1

    ## Iterate over the ucapbh resource list and populate the new data frame:
    for (i in 1:length(x)){

        ## Get the current data frame from list:
        ca <- x[[i]]

        if (class(ca) == "list") {
            ca <- data.frame(do.call(cbind, .removeList(ca)), stringsAsFactors=FALSE, check.names=FALSE)
        }

        ## Compute the ending row given current data-frames dimension:
        eRow <- NROW(ca) + sRow - 1

        ## Assign current data-frame to the main df.
        df[sRow:eRow, colnames(ca)] <- ca

        ## Compute the new starting row:
        sRow <- sRow + NROW(ca)
    }

    ## Return:
    data.frame(df, stringsAsFactors=FALSE, check.names=FALSE)
}


##' A Wrapper function for formatting numbers:
##'
##' This is a description.
##'
##' @param number An array of numeric values.
##' @param nsmall The number of digits.
##' @return Formated number(s):
##' @export
beautify <- function(number, nsmall=0){
    format(as.numeric(as.character(number)), nsmall=nsmall, big.mark=",")
}


##' A function for formatting percentages:
##'
##' This is a description.
##'
##' @param number An array of numeric values.
##' @param digit the number of digits. Default is 2.
##' @return A string of percentage:
##' @export
percentify <- function(number, digit=2){
    paste0(round(as.numeric(number) * 100, digit), " %")
}


##' A function to round up/down to the the nearest value.
##'
##' This is a description.
##'
##' @param x An array of numeric values.
##' @param nearest The value to be rounded to.
##' @param dir The direction to be rounded to (up or down).
##' @return Formated number(s):
##' @export
nearesth <- function(x, nearest=0.1, dir="up"){
    ## Define the functions:
    functionMap <- list("up"=function(x){ceiling(x / nearest) * nearest},
                        "down"=function(x){floor(x / nearest) * nearest})

    ## Call the functions:
    do.call(functionMap[[dir]], list(x))
}


##' A Wrapper function to source R scripts from specified paths.
##'
##' This is a description.
##'
##' @param paths The path where the R scripts are located.
##' @param pattern The pattern of the R scripts (default = *.R).
##' @return Source R scripts:
##' @export
sourceFromFolder <- function(paths, pattern="*.R"){
    sourced <- lapply(paths, function(path) sapply(list.files(path, pattern=pattern, full.names=TRUE), source, .GlobalEnv))
}


##' A function to safely call a column in a data-frame whether column
##' exists or not.
##'
##' This is a description.
##'
##' @param df The data-frame.
##' @param col The expected column name.
##' @return The column values or NA.
##' @export
safeColumn <- function(df, col) {
    ## Check if expiry column exists:
    value <- try(df[,col], silent=TRUE)

    ## If column doesn't exist, set to NA:
    if (class(value) == "try-error") {
        ## Get the expiry of the future:
        df[, col] <- NA
    }

    ## Done, return:
    df[, col]
}


##' A function to exclude rows in which a specified column
##' has a key.
##'
##' This is a description.
##'
##' @param data The data-frame.
##' @param field The field names to be excluded.
##' @param keys The keys to look for.
##' @return The data-frame with the excluded rows.
##' @export
excludeRowsWithKeys <- function(data, field, keys){

    ## Iterate over the keys:
    for (key in keys){

        ## Try to grep the key in column:
        greppedKeys <- grep(toupper(key), toupper(data[, field]))

        ## If key was grepped, exclude row:
        if (length(greppedKeys) > 0){
            data <- data[-greppedKeys,]
        }
    }

    ## Return data:
    return(data)
}


##' A function to check whether value is empty or NA:
##'
##' This is a description.
##'
##' @param str A vector of strings.
##' @return TRUE or FALSE.
##' @export
isNAorEmpty <- function(str){

    aux <- function(x) {
        is.na(x) | nchar(x) == 0
    }

    if (is.null(str)) {
        str <- ""
    }

    sapply(str, aux)
}


##' A function to trim double white spaces.
##'
##' This is a description.
##'
##' @param str A vector of strings.
##' @return A vector of strings with double white space removed.
##' @export
trimDws <- function(str){

    ## Store original string vector:
    oldStr <- str

    ## Get the non NA strings index:
    nonNA <- !is.na(str)

    ## Get the non NA strings:
    str <- str[nonNA]

    ## Sub double white space with single white space:
    newStr <- gsub("  ", " ", str)

    ## Do while no double white space:
    while (any(str != newStr)) {

        ## Assign str to new variable:
        newStr <- str

        ## Sub double white space with single white space:
        str <- gsub("  ", " ", str)
    }

    ## Assign new strings to non NA string index:
    oldStr[nonNA] <- str

    ## Done, return:
    oldStr

}


##' A function to transform arbitrary date formats into YYYY-MM-DD.
##'
##' This is a description.
##'
##' @param str A single date string.
##' @param origin The origin for numerical date inputs (default = "1899-12-30".
##' @return A date value.
##' @export
parseDate <- function(str, origin="1899-12-30"){

    ## Return string if null:
    if (is.null(str)) {
        return(str)
    }

    ## Make sure str in character:
    str <- as.character(str)

    ## Compute the number of characters in string:
    ncharStr <- nchar(str)

    ## Try the first possible format:
    trial <- as.Date(str, format="%Y-%m-%d")

    ## If trial was error, try next possible format:
    if (any(is.na(trial) && ncharStr >= 8)) {
        trial <- as.Date(str, format="%d/%m/%Y")
    }

    ## If trial was error, try next possible format:
    if (any(is.na(trial) &&  ncharStr > 6)) {
        trial <- as.Date(str, format="%d/%m/%y")
    }

    ## If trial was error, try next possible format:
    if (any(is.na(trial) && ncharStr > 6)) {
        trial <- as.Date(str, format = "%m/%d/%Y")
    }

    ## If trial was error, try next possible format:
    if (any(is.na(trial) && ncharStr > 6)) {
        trial <- as.Date(str, format="%d%m%Y")
    }

    ## If trial was error, try next possible format:
    if (any(is.na(trial) && ncharStr > 6)) {
        trial <- as.Date(str, format="%Y%m%d")
    }

    ## If trial was error, try next possible format:
    if (any(is.na(trial) && ncharStr > 6)) {
        trial <- as.Date(str, format="%d.%m.%Y")
    }

    ## If trial was error, try next possible format:
    if (any(is.na(trial) && ncharStr == 6)) {
        trial <- as.Date(str, format="%d%m%y")
    }

    ## If trial was error, try next possible format:
    if (any(is.na(trial))) {
        trial <- as.Date(as.numeric(str), origin="1899-12-30")
    }

    ## Return:
    trial
}

##' A function clean to NA rows and columns.
##'
##' This is a description.
##'
##' @param df A data frame.
##' @param ratio The ratio of NA's to number of values in row.
##' @return A data frame with cleaned NA rows and columns.
##' @export
cleanNARowsCols <- function(df, ratio=0.85){

    ## Determine if rows are to be excluded:
    naRows <- apply(df, MARGIN=1, function(x) {

        if (!is.null(ratio)) {
            r <- sum(isNAorEmpty(x)) / length(x)
            retval <- r > ratio
        } else {
            retval <- all(isNAorEmpty(x))
        }
    })

    ## Exclude the rows:
    df <- df[!naRows,]

    ## Determine the columns to be excluded:
    naCols <- apply(df, MARGIN=2, function(x) all(isNAorEmpty(x)))

    ## Exclude the columns and return:
    df[,!naCols]
}


##' A function to produce a random string.
##'
##' This is a description.
##'
##' @param n The number of random strings.
##' @param len The desired lenght of the random string.
##' @return A random string.
##' @export
getRandString <- function(n=1, len=12) {
    sapply(1:n, function(x) paste(sample(c(rep(0:9,each=5), LETTERS, letters), len), collapse=""))
}


## alphaNumSingleWord <- function(str) {
##     concernIdx <- which(do.call(c, lapply(strsplit((as.character(str)), " "), function(x) length(x) == 1)))
##     numbers <- grepl("^[[:digit:]]+$", str[concernIdx])
##     letters <- grepl("^[[:alpha:]]+$", str[concernIdx])
##     both <- grepl("^[[:digit:][:alpha:]]+$", str[concernIdx])
##     idx <- xor((letters | numbers), both)
##     data.frame("index"=concernIdx[idx], "name"=as.character(str[concernIdx[idx]]))

## }


##' Shortens the string and attaches ellipsis.
##'
##' This is a description.
##'
##' @param str A string.
##' @param charLimit The maximum number of characters.
##' @param capitalise Boolean to indicate if string should be capitalised. Default is TRUE.
##' @return Ellipsified string.
##' @export
ellipsify <- function(str, charLimit=30, capitalise=TRUE){
    ## The ellipsis vector:
    ellipsis <- ifelse(nchar(as.character(str)) > charLimit, "...", "")
    ## Paste and return:
    str <- paste0(substr(str, 1, charLimit), ellipsis)

    if (capitalise) {
        str <- toupper(str)
    }

    ## Done, return:
    str
}


##' Replaces NA's with empty value
##'
##' This is a description.
##'
##' @param data A data frame.
##' @return Data-frame with no NA's.
##' @export
cleanNA <- function(data){
    ## Clean NA's:
    if (NROW(data) == 1) {
        data[is.na(data)] <- ""
    } else {
        data <- data.frame(apply(data, MARGIN=2, function(x) trimws(ifelse(is.na(x), "", x))))
    }
    data
}


##' A function to count keys in strings.
##'
##' This is a description.
##'
##' @param str A vector with strings.
##' @param key Key to be found.
##' @return A vector of counts.
##' @export
safeGrep <- function (str, key) {
    ## Count key in strings and return:
    as.character(sapply(str, function(s) length(grep(key, s))))
}


##' A function to capitalise a string.
##'
##' This is a description.
##'
##' @param str A vector with strings.
##' @return Capitalised strings.
##' @export
capitalise <- function(str){

    ## If string is empty or NA, return:
    if (all(isNAorEmpty(str))) {
        return(str)
    }

    ## Capitalise each word in each string:
    capitalised <- lapply(strsplit(str, " "), function(x) sapply(x, function(y) paste0(toupper(substr(y, 1, 1)), tolower(substr(y, 2, nchar(y))))))

    ## Recombine words in each string and return:
    sapply(capitalised, function(x) paste0(x, collapse=" "))

}


##' A function to determine whether string resembles ISIN:
##'
##' This is a description.
##'
##' @param str A vector with strings.
##' @return A vector with either string or NA.
##' @export
isIsin <- function(str){


    ## Make sure strings are character.
    str <- as.character(str)

    ## Split each string by space and take first element.
    word <- do.call(c, lapply(str, function(s) strsplit(s, " ")[[1]][1]))

    ## Split each word by @ and take first element.
    word <- do.call(c, lapply(word, function(w) strsplit(w, "@")[[1]][1]))

    ## Are the words 12 characters?
    twelveChars <- ifelse(isNAorEmpty(word), FALSE, nchar(word) == 12)

    ## Are the first 2 characters letters?
    twoChars <- grepl("^[A-Za-z]+$", substr(word, 1, 2), perl = T)

    ## Does the string have digits?
    hasDigit <- grepl("[[:digit:]]", word)

    ## Are the 3 conditions met?
    twelveChars & twoChars & hasDigit

}


##' A function to transform each column in data frame to
##' the correct class.
##'
##' This is a description.
##'
##' @param df The data frame.
##' @return The data frame with correct column classes.
##' @export
classify <- function(df){

    ## Make sure the column names of data frame are unchecked.
    df <- as.data.frame(df, check.names=FALSE)

    ## Iterate over columns:
    for (col in 1:NCOL(df)) {

        ## First, try logical for column:
        isError <- all(is.na(as.logical(df[,1])))

        ## If no error, then as logical:
        if (!isError) {
            df[,col] <- as.logical(df[,col])
            next
        }

        ## Now, try date for column:
        isError <- class(try(as.Date(as.character(df[,col])), silent=TRUE)) == "try-error"

        ## If date, then as date:
        if (!isError) {
            df[,col] <- as.Date(as.character(df[,col]))
            next
        }

        ## Now, try numeric:
        isError1 <- suppressWarnings(try(any(is.na(as.numeric(as.character(df[,col])))), silent=TRUE))
        isError2 <- suppressWarnings(class(try(as.numeric(as.character(df[,col])), silent=TRUE)) == "try-error")
        isError <- isError1 | isError2

        ## If no error, then as numeric:
        if (!isError) {
            df[,col] <- as.numeric(trimws(as.character(df[,col])))
            next
        }

        ## Finally, as character.
        df[,col] <- trimws(as.character(df[,col]))
    }

    ## Data frame with unchecked names and return:
    as.data.frame(df, check.names=FALSE)
}


##' A function to whether values in data frame column
##' satisfies a condition.
##'
##' This is a description.
##'
##' @param df The data frame.
##' @param col The column to be checked.
##' @param condition The condition to be checked.
##' @return A vector with TRUE or FALSE
##' @export
safeCondition <- function(df, col, condition){
    hasCondition <- df[,col] == condition
    ifelse(is.na(hasCondition), FALSE, hasCondition)
}


##' This function attempts to transform irregular number
##' formats into numerical values.
##'
##' This is a description.
##'
##' @param str A vector of strings.
##' @return A vector of numerical values.
##' @export
numerize <- function(str){

    ## If all are NA, return NA:
    if (all(is.na(str))) {
        return(str)
    }

    ## If all dash, return NA:
    if (all(str == "-")) {
        return(rep(NA, NROW(str)))
    }

    ## Make sure str is character:
    str <- as.character(str)

    ## Compute the number of characters:
    chars <- nchar(str)

    ## Get the first character:
    char <- substr(str, 1, 1)

    ## Get the sign of the value:
    vSign <- ifelse(char == "-", "-", "")

    ## The auxilirary functon:
    auxFun <- function(chars, str){

        ## Initialise the value:
        val <- NULL

        ## If NA, return NA:
        if (is.na(chars)){
            return(NA)
        }

        ## Iterate over the characters in string:
        for (i in 1:chars) {

            ## Get the current character:
            char <- substr(str, i, i)

            ## If character is non-numeric, append empty value to val:
            val <- suppressWarnings(paste0(val, ifelse(is.na(as.numeric(char)), "", char)))

            ## If character is not dot, append empty value to val:
            val <- suppressWarnings(paste0(val, ifelse(char == ".", ".", "")))
        }

        ## Return val:
        val
    }

    ## Run the auxiliary function to each string:
    val <- sapply(1:length(str), function(i) {
        auxFun(chars[i], str[i])
    })

    ## Add the sign of the number, change to numeric and return:
    suppressWarnings(as.numeric(paste0(vSign, val)))
}
