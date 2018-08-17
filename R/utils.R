##' A function to match a string with candidates:
##'
##' @param sourceStr A vector with strings to match from.
##' @param targetStr A vector with strings to match against.
##' @param jaccard The acceptable threshold for the jaccard coefficient.
##' @return A vector with matching target indices:
##' @import stringdist
##' @export
stringMatch <- function(sourceStr, targetStr, jaccard=0.10){

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
##' @param vector The numeric value to check condition for.
##' @param val The numeric value to check condition against>
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
##' @param string A string.
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
    if (!is.null(randomStr)){
        prefix <- paste0(prefix, "_", getRandString(randomStr))
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
##' @param x A object of try.
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
##' @param x A list with data frames.
##' @return A single data frame.
##' @export
safeRbind <- function(x){

    ## Ge the unique column names of all ucapbh resources:
    colNames <- unique(do.call(c, lapply(x, colnames)))

    ## Initialise the new resource data frame:
    df <- as.data.frame(matrix(NA, ncol=length(colNames), nrow=sum(sapply(x, NROW))))

    ## Reassign the column names:
    colnames(df) <- colNames

    ## Initialse the starting row:
    sRow <- 1

    ## Iterate over the ucapbh resource list and populate the new data frame:
    for (i in 1:length(x)){

        ## Get the current data frame from list:
        ca <- x[[i]]

        ## Compute the ending row given current data-frames dimension:
        eRow <- NROW(ca) + sRow - 1

        ## Assign current data-frame to the main df.
        df[sRow:eRow, colnames(ca)] <- ca

        ## Compute the new starting row:
        sRow <- sRow + NROW(ca)
    }

    ## Return:
    df
}


##' A Wrapper function for formatting numbers:
##'
##' @param number An array of numeric values.
##' @param nsmalll The number of digits.
##' @return Formated number(s):
##' @export
beautify <- function(number, nsmall=0){
    format(as.numeric(number), nsmall=nsmall, big.mark=",")
}


##' A function for formatting percentages:
##'
##' @param number An array of numeric values.
##' @return A string of percentage:
##' @export
percentify <- function(number){
    paste0(round(as.numeric(number) * 100, 2), " %")
}


##' A function to round up/down to the the nearest value.
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
##' @param data The data-frame.
##' @param column The column name.
##' @param keys The keys to look for.
##' @return The data-frame with the excluded rows.
##' @export
excludeRowsWithKeys <- function(data, field, keys){

    ## Iterate over the keys:
    for (key in keys){

        ## Try to grep the key in column:
        greppedKeys <- grep(toupper(key), toupper(data[,field]))

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
##' @param str A vector of strings.
##' @return TRUE or FALSE.
##' @export
isNAorEmpty <- function(str){

    if (is.null(str)) {
        str <- ""
    }

    is.na(str) | nchar(str) == 0
}


##' A function to trim double white spaces.
##'
##' @param str A vector of strings.
##' @return A vector of strings with double white space removed.
##' @export
trimDws <- function(str){

    ## Get rid of the first set of double spaces.
    newStr <- gsub("  ", " ", str)

    ## While double white space exists, iterate:
    while (any(str != newStr)) {

        ## Assign string to new variable:
        newStr <- str

        ## Remove double white spaces
        str <- gsub("  ", " ", str)
    }

    ## Done, return:
    str
}


##' A function to transform arbitrary date formats into YYYY-MM-DD.
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
##' @param str A string.
##' @param charLimit The maximum number of characters.
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
##' @param str A vector with strings.
##' @param key Key to be found.
##' @return A vector of counts.
##' @export
safeGrep <- function (str, key) {
    ## Count key in strings and return:
    sapply(str, function(s) length(grep(key, s)))
}


##' A function to capitalise a string.
##'
##' @param str A vector with strings.
##' @return Capitalised strings.
##' @export
capitalise <- function(str){

    ## If string is empty or NA, return:
    if (all(isNAorEmpty(str))) {
        return(str)
    }

    ## Capitalise string and return:
    paste0(toupper(substr(str, 1, 1)), tolower(substr(str, 2, nchar(str))))
}


##' A function to determine whether string resembles ISIN:
##'
##' @param str A vector with strings.
##' @return A vector with either string or NA.
##' @export
isIsin <- function(str){

    ## Make sure strings are character.
    str <- as.character(str)

    ## Split each string by space and take first element.
    word <- strsplit(str, " ")[[1]][1]

    ## Split each word by @ and take first element.
    word <- strsplit(word, "@")[[1]][1]

    ## Are the words 12 characters?
    twelveChars <- nchar(word) == 12

    ## Are the first 2 characters letters?
    twoChars <- grepl("^[A-Za-z]+$", substr(word, 1, 2), perl = T)

    ## Does the string have digits?
    hasDigit <- grepl("[[:digit:]]", word)

    ## Are the 3 conditions met?
    isIsin <- twelveChars & twoChars & hasDigit

    ## If no ISIN resemblence, add NA's and return:
    ifelse(isIsin, as.character(word), NA)

}


##' A function to transform each column in data frame to
##' the correct class.
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
