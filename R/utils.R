##' A function to normalise country names.
##'
##' This is a description.
##'
##' @param str A string vector.
##' @return The vector with the normalised country names.
##' @export
countryNormalizer <- function(str) {

    ## The country normalizer mapping:
    countryMap <- list("United States of America"=c("United States of America", "US", "United States"),
                       "Germany"=c("Germany", "Deutschland", "DE"),
                       "Switzerland"=c("Switzerland", "Schweiz"),
                       "India"=c("India", "India (Republic of)"),
                       "South Korea"=c("South Korea", "Korea (Republic of) (South)"),
                       "China"=c("China", "China (People's Republic of)"),
                       "Brazil"=c("Brazil", "BR"),
                       "British Virgin Islands"=c("British Virgin Islands", "British Virgin"),
                       "Cayman Islands"=c("Cayman Islands", "KY"),
                       "United Kingdom"=c("United Kingdom", "UK", "Britain"),
                       "United Arab Emirates"=c("United Arab Emirates", "UAE"))

    ## Iterate over the string vector:
    retval <- as.character(sapply(str, function(x) {

        ## If string is NA, return the same:
        if (is.na(x)) {
            return(NA)
        }

        ## Attempt to match the string to the countrMap list elements:
        idx <- sapply(countryMap, function(z) any(toupper(trimws(z)) == toupper(trimws(x))))

        ## If any match, use the name of the corresponding countryMap list:
        if (any(idx)) {
            return(as.character(names(countryMap)[idx]))
        }

        ## If no match, return the same:
        return(as.character(x))
    }))

    ## Finally, capitalise the string and return:
    return(capitalise(retval))
}


##' A function to trim excess white spaces.
##'
##' This is a description.
##'
##' @param str A string.
##' @return The trimmed string.
##' @export
trimExcessWs <- function(str){

    ## Get rid of double white spaces:
    excWs <- grep("  ", str)

    ## Do while single white space only:
    while (length(excWs) > 0){
        str <- gsub("  ", " ", str)
        excWs <- grep("  ", str)
    }

    ## Done, return:
    as.character(str)
}


##' A function to safely check the condition of a vector of a string.
##'
##' This is a description.
##'
##' @param str A vector with strings.
##' @param key Key to be found.
##' @return A vector with "1" for TRUE and "0" for FALSE.
##' @export
safeMatch <- function (str, key) {

    ## Get the match:
    isMatch <- ifelse(str == key, "1", "0")

    ## Replace NA's with "0":
    ifelse(is.na(isMatch), "0", isMatch)
}


##' Thsi funciton returns rows of a treated data frame which are different compared to the original
##'
##' This is the description
##'
##' @param altered The altered data frame.
##' @param original The original data frame.
##' @param idField The field to be used to align the data frames.
##' @return A data frame with altered rows only.
##' @export
getDissimilarRows <- function(altered, original, idField) {

    ## Align the original to the altered by id field:
    original <- original[match(altered[, idField], original[, idField]), ]

    ## Get the NA's identical?
    nanCompare <- is.na(altered) == is.na(original)

    ## Are the values identical?
    valCompare <- altered == original

    ## Replace NA's in values check with the NA check:
    valCompare[is.na(valCompare)] <- nanCompare[is.na(valCompare)]

    ## Get the rows where any value is different:
    altered[!apply(valCompare, MARGIN=1, all), ]

}


##' This function produces a vector with the month year for a date's year.
##' Such as: Jan 19, Feb 19, ... , Dec 19 if date is 2019-06-01.
##'
##' This is the description
##'
##' @param date The date.
##' @return A vector with the month year for the date's year in %h %y
##' @export
getMonthYearOfCurrentYear <- function(date) {
    unique(format(seq(as.Date(paste0(format(date, "%Y"), "-01-01")), as.Date(paste0(format(date, "%Y"), "-12-31")), 1), format="%h %y"))
}


##' In a data frame, check if multiple columns fulfil multiple conditions.
##'
##' This is the description
##'
##' @param df The data frame.
##' @param cols A vectors of column names.
##' @param conditions The conditions against which to check the columns.
##' @return A vector with TRUE or FALSE.
##' @export
mCondition <- function(df, cols, conditions) {

    ## For each column name, run the mgrep functions for the conditions:
    conditionCheck <- do.call(cbind, lapply(cols, function(col) mgrep(df[, col], conditions)))

    ## Check if any of the conditions apply and return:
    apply(conditionCheck, MARGIN=1, function(x) any(x != 0))
}


##' Set NA's for columns in data frame to a desired value.
##'
##' This is the description
##'
##' @param df The data frame.
##' @param cols A vectors of column names.
##' @param val The value NA's should be changed to. Default is 0.
##' @return A data frame.
##' @export
setNAColTo <- function(df, cols, val=0) {

    ## Apply the ifelse to the columns in data frame:
    df[, cols] <- apply(df[, cols], MARGIN=2, function(x) ifelse(is.na(x), val, x))

    ## Done, return:
    return(df)
}


##' A function to indicate the last week day within a specific periodicity.
##'
##' This is the description
##'
##' @param dates A vector with dates.
##' @param day The weekday to be considered ("Monday", "Tuesday", ... ,"Friday", etc.)
##' @param period The periodicity give in single letter ("W", "M", "Q", "Y").
##' @return The date frame with the lastDayOfPeriod boolean column.
##' @export
getLastDayOfPeriod <- function(dates, day, period) {

    ## Get the last date:
    lastDate <- tail(dates, 1)

    ## Get the period memnonic:
    periodMemnonic <- paste0(switch(period, weekly="W", monthly="M", quarterly="Q", semiannually="S", yearly="Y"), "-0")

    ## Get the period function:
    pFun <- list("weekly"=function(x){cumsum(weekdays(x) == "Friday")},
                 "monthly"=function(x){paste0(months(x), substr(x, 3,4))},
                 "quarterly"=function(x){paste0(quarters(x), substr(x, 3,4))},
                 "semiannually"=function(x){paste0(ifelse(numerize(substr(x, 6, 10)) >= 100 & numerize(substr(x, 6, 10)) <= 0630, "S1", "S2"), substr(x, 3,4))},
                 "yearly"=function(x){substr(x, 1, 4)})

    ## Add 365 days to last date provided create a sequence 1 year ahead:
    auxSeries <- seq(lastDate, lastDate + 365, 1)

    ## Run the period function:
    pSeries <- do.call(pFun[[period]], list(auxSeries))

    ## Determine the next ending date of period:
    auxPeriodEnd <- as.Date(auxSeries[which(!duplicated(pSeries))[2]])

    if ((lastDate + 1 > dateOfPeriod(periodMemnonic, auxPeriodEnd))) {
        datesx <- dates
    } else {
        ## Append missing dates to complete current period:
        datesx <- c(dates, seq(lastDate + 1, dateOfPeriod(periodMemnonic, auxPeriodEnd), 1))
    }

    ##: TODO:
    datesx <- data.frame("dates"=as.character(datesx), "period"=cumsum(!duplicated(do.call(pFun[[period]], list(datesx)))))

    ## Mark the days of the dates which are our target day:
    datesx[, "days"] <- weekdays(as.Date(as.character(datesx[, "dates"]))) == day

    ## For each unique period, find the last day which is our target day:
    lastDayOfPeriod <- do.call(c, lapply(unique(datesx[, "period"]), function(x) as.character(tail(datesx[datesx[, "period"] == x & datesx[, "days"], 1], 1))))

    ## Match last day of period dates with our original dates vector:
    dateMatch <- match(dates, as.Date(lastDayOfPeriod))

    ## Append the last day of period column to dates:
    dates <- data.frame(dates, "lastDayOfPeriod"=FALSE)

    ## Market the last days in period:
    dates[!is.na(dateMatch), "lastDayOfPeriod"] <- TRUE

    ## Done, return:
    return(dates)

}


##' A function aggregates a value column by a second column and transforms it to xts class.
##'
##' This is the description
##'
##' @param x A data frame with the columns names as ascribed to aggColumn and aggBy.
##' @param aggColumn The name of the column to be aggregated.
##' @param aggBy The name of the column to be aggregated by.
##' @param fallbackDate If data frame has NROW==0, we need a fallback date.
##' @return A time-series object of class xts.
##' @export
aggregateAndXTSify <- function(x, aggColumn="qtymain", aggBy="commitment", fallbackDate=Sys.Date()) {

    ## If data frame is empty, mask values:
    if (NROW(x) == 0) {
        x <- data.frame("V1"=0, "V2"=fallbackDate)
        colnames(x) <- c(aggColumn, aggBy)
    }

    ## Aggregate the values:
    x <- aggregate(as.numeric(x[, aggColumn]), list(x[, aggBy]), sum)

    ## Assign the column names:
    colnames(x) <- c(aggBy, aggColumn)

    ## Tranform to xts and return:
    xts::as.xts(as.numeric(x[, aggColumn]), order.by=x[, aggBy])

}


##' A function to match a vector with a data frame with multiple columns.
##'
##' This is the description
##'
##' @param matchFrom The vector to be matched.
##' @param matchTo The data frame to be matched to.
##' @return A vector with the matching indices.
##' @export
mmatch <- function(matchFrom, matchTo) {

    ## Initialise the matching index vector with NA's.
    matchIdx <- rep(NA, NROW(matchFrom))

    ## Iterate over the the matchTo data.frame by columns:
    for (col in 1:NCOL(matchTo)) {

        ## Match the current column of matchTo:
        matchIdxT <- match(matchFrom, matchTo[, col])

        ## Replace NA's in the matching index with the current matching index of current matchTo column:
        matchIdx <- ifelse(is.na(matchIdx), matchIdxT, matchIdx)
    }

    ## Done, return:
    return(matchIdx)
}


##' A function which provides the YTD slice of the data frame.
##'
##' This is the description
##'
##' @param df The data frame with a 'date' column.
##' @param date the report date.
##' @return A data frame with the YTD slice of the data frame or NULL.
##' @export
getYTDSlice <- function(df, date) {

    ## If df is NULL or no rows, return NULL
    if (is.null(df) | NROW(df) == 0) {
        return(NULL)
    }

    ## Get the year of the report date:
    year <- substr(date, 1, 4)

    ##:
    exception <- any(dateOfPeriod("Y-0", date) == df[, "date"]) & NROW(df) == 1

    ## If no year corresponds to year of report date, return NULL:
    if (all(substr(df[, "date"], 1, 4) != year) & !exception) {
        return(NULL)
    }

    ## Get the period change:
    periodChange <- c(0, diff(as.numeric(substr(df[, "date"], 1,4)))) != 0

    ## If none corresponds to the period change, set last to TRUE:
    if (all(!periodChange)) {
        periodChange[length(periodChange)] <- TRUE
    }

    ## Return the slice:
    df[1:which(periodChange)[1],]
}


##' A function which provides the QTD slice of the data frame.
##'
##' This is the description
##'
##' @param df The data frame with a 'date' column.
##' @param date the report date.
##' @return A data frame with the QTD slice of the data frame or NULL.
##' @export
getQTDSlice <- function(df, date) {

    ## If df is NULL or no rows, return NULL
    if (is.null(df) | NROW(df) == 0) {
        return(NULL)
    }

    ## Get the quarter-year for the report date:
    qtrYear <- paste0(quarters(date), substr(date, 1,4))

    ## Get the quarters of the dates:
    qtrs <- quarters(df[, "date"])

    ## If no quarter-year matches, return NULL:
    if (all(paste0(qtrs, substr(date, 1, 4)) != qtrYear)) {
        return(NULL)
    }

    ## Initialise the period change:
    periodChange <- rep(FALSE, NROW(df))

    ## Determine the index of the period changes:
    periodChangeIdx <- !qtrs == c(tail(qtrs, -1), tail(qtrs , 1))

    ## If no period change is identified, set the last
    ## period change index to TRUE and return:
    if (all(!periodChangeIdx)) {
        periodChange[length(periodChange)] <- TRUE
        return(df[1:which(periodChange)[1],])
    }

    ## Get the period change indices which are not the last observation:
    shift <- which(periodChangeIdx) < length(periodChange)

    ## Shift the period change index for non-last members and set to TRUE:
    periodChange[which(periodChangeIdx)[shift] + 1] <- TRUE

    ## Return the slice:
    df[1:which(periodChange)[1],]
}


##' A function which provides the MTD slice of the data frame.
##'
##' This is the description
##'
##' @param df The data frame with a 'date' column.
##' @param date the report date.
##' @return A data frame with the MTD slice of the data frame or NULL.
##' @export
getMTDSlice <- function(df, date) {

    ## If df is NULL or no rows, return NULL
    if (is.null(df) | NROW(df) == 0) {
        return(NULL)
    }

    ## Get the month-year of the report date:
    monYear <- substr(date, 1, 7)

    ## Get the previous months month-year:
    prevMonYear <- substr(dateOfPeriod("M-0", date), 1, 7)

    ## If no date matches the month-year of the report date, return NULL:
    if (all(substr(df[, "date"], 1, 7) != monYear)) {
        return(NULL)
    }

    ## Determine the indices with the period change:
    periodChange <- c(0, diff(as.numeric(substr(df[, "date"], 6, 7)))) != 0

    if (all(substr(df[, "date"], 1, 7) == monYear)) {
        periodChange[length(periodChange)] <- TRUE

    }

    ## If no date corresponds to a period change, return NULL:
    if (all(!periodChange)) {
        return(NULL)
    }

    ## Get the slice until first period change:
    df <- df[1:which(periodChange)[1],]

    ## Remove any row which does not correspond to current month-year and previous month-year:
    df[substr(df[, "date"], 1, 7) == monYear | substr(df[, "date"], 1, 7) == prevMonYear, ]

}


##' A function which provides the WTD slice of the data frame.
##'
##' This is the description
##'
##' @param df The data frame with a 'date' column.
##' @param date the report date.
##' @return A data frame with the WTD slice of the data frame or NULL.
##' @export
getWTDSlice <- function(df, date) {

    ## If df is NULL or no rows, return NULL
    if (is.null(df) | NROW(df) == 0) {
        return(NULL)
    }

    ## The distance in days of report date:
    distance <- 7

    ## Get the week of the day of report date:
    dayOfWeek <- weekdays(date)

    ## If target day falls on Sunday, set distance to 9:
    if (dayOfWeek == "Sunday") {
        dayOfWeek <- "Friday"
        distance <- 9
    }

    ## If target day falls on Saturday, set distance to 8:
    if (dayOfWeek == "Saturday") {
        dayOfWeek <- "Friday"
        distance <- 8
    }

    ## Determin which dates fall within the distance:
    inWeek <- date - df[, "date"] <= distance

    ## Determine which dates are within the week and are the same weekday name:
    periodChange <- weekdays(df[, "date"]) == dayOfWeek & inWeek

    ## Set the first to FALSE:
    periodChange[1] <- FALSE

    ## If no qualifies, set the last from within the week to TRUE:
    if (all(!periodChange)) {
        periodChange[tail(which(inWeek), 1)] <- TRUE
    }

    ## If none, qualified, return NULL:
    if (all(!periodChange)) {
        return(NULL)
    }

    ## Return the slice:
    df[1:which(periodChange)[1],]
}


##' A function which provides the DTD slice of the data frame.
##'
##' This is the description
##'
##' @param df The data frame with a 'date' column.
##' @param date the report date.
##' @return A data frame with the DTD slice of the data frame or NULL.
##' @export
getDTDSlice <- function(df, date) {

    ## If df is NULL or no rows, return NULL
    if (is.null(df) | NROW(df) == 0) {
        return(NULL)
    }

    ## The target date:
    targetDay <- date - 1

    ## If target day falls on Sunday, substract 2:
    if (weekdays(targetDay) == "Sunday") {
        targetDay <- targetDay - 2
    }

    ## If target day falls on Saturyday, substract 1:
    if (weekdays(targetDay) == "Saturday") {
        targetDay <- targetDay - 1
    }

    ## Which date corresponds to the target date:
    periodChange <- df[, "date"] == targetDay

    ## If no such date, return NULL:
    if (all(!periodChange)) {
        return(NULL)
    }

    ## Return the slice:
    df[1:which(periodChange)[1],]
}


##' Extract unique column names to list
##'
##' This is a description.
##'
##' @param df The data frame
##' @param colname The column to be extracted
##' @return A list
##' @export
extractToList <- function(df, colname) {

    lst <- lapply(unique(df[, colname]), function(x) df[x == df[, colname], ])
    names(lst) <- unique(df[, colname])

    return(lst)
}


##' This function checks if 2 numbers match
##'
##' This is a description.
##'
##' @param num1 The first number
##' @param num2 The second number
##' @param pctTolerance The percentage tolerance
##' @param absTolerance The absolute tolerance
##' @return TRUE or FALSEg
##' @export
numberEquivalency <- function (num1, num2, pctTolerance=0.001, absTolerance=0.05) {

    ## QTY1 as numeric:
    num1 <- as.numeric(num1)

    ## QTY@ as numeric:
    num2 <- as.numeric(num2)

    ## If any of the quantities is NA, return FALSE:
    if (is.na(num1) | is.na(num2)) {
        return(FALSE)
    }

    ## First, compute the abs difference:
    absDiff <- abs(num1 - num2)

    ## Secondly, compute the percentage difference:
    pctDiff <- absDiff / abs(num1)

    ## Return TRUE if any
    absDiff < absTolerance | pctDiff < pctTolerance

}


##' This function
##'
##' This is a description.
##'
##' @param x A numeric vector.
##' @param factor The factor to expand the limits with.
##' @return A list with the expanded upper and lower limits
##' @export
expandLimits <- function(x, factor=0.1) {
    list("lower"=min(x) - abs(min(x) * factor),
         "upper"=max(x) + abs(min(x) * factor))
}

##' This function
##'
##' This is a description.
##'
##' @param v1 A vector with strings to be matched.
##' @param v2 A vector with candidate matching strings.
##' @param param The stringdist method's parameter.
##' @param dir Bigger or smaller than the param?
##' @param method The method for stringdist.
##' @return A ordered vector
##' @export
getStringDistanceIndex <- function(v1, v2, param=0.2, dir="smaller", method="jaccard") {

    ## Compute the distance:
    dst <- lapply(v1, function(n) stringdist::stringdist(n, v2, method=method))

    if (dir == "smaller") {
        result <- sapply(dst, function(x) ifelse(any(x < param), which(x == min(x)), NA))
    }

    if (dir == "bigger") {
        result <- sapply(dst, function(x) ifelse(any(x > param), which(x == min(x)), NA))
    }

    ## Return:
    result

}


##' This function orders a vector by the keys provided.
##'
##' This is a description.
##'
##' @param x A vector
##' @param keys The vector of keys in order of preference.
##' @return A ordered vector
##' @export
orderByKey <- function(x, keys) {

    ## Grep the vector of keys in the string:
    grepped <- mgrep(toupper(x), toupper(keys))

    ## Get the ranking of each string:
    rankedKeys <- apply(grepped, MARGIN=1, function(row) (row != "0") * 1:length(keys))

    ## If any of the ranking is 0 since it couldnt be grepped, assign Inf:
    ranking <- as.numeric(apply(rankedKeys, MARGIN=2, function(col) ifelse(all(col == 0), Inf, col[col != 0])))

    ## Order the vector and return:
    x[order(ranking)]
}


##' A function to forward fill NA's
##'
##' This is a description.
##'
##' @param x A vector
##' @return A vector
##' @export
forwardFillNA <- function(x){

    isNA <- is.na(x)

    x[which(!isNA)[c(1,1:sum(!isNA))][cumsum(!isNA)+1]]
}


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

    ## If key is NA, return NA:
    if (is.na(key)) {
        return(rep(NA, NROW(df)))
    }

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
##' @param useMatch Shall match be used instead of grep?
##' @return A 'x' by 'keys' size data frame with the grepped keys.
##' @export
mgrep <- function(x, keys, useMatch=FALSE) {

    ## For each key in keys, safely grep/match in vector x:
    if (useMatch) {
            kList <- lapply(keys, function(key) safeMatch(x, key))
        } else {
            kList <- lapply(keys, function(key) safeGrep(x, key))
        }

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
    dateDist <- as.Date(data[, dateField]) - as.Date(targetDate)

    ## Filter the data for negative distances:
    if (any(dateDist < 0)) {
        data <- data[dateDist <= 0, ]
        dateDist <- dateDist[dateDist <= 0]
    }

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
##' @param replace The value to be returned if NULL. Default = NA.
##' @return Returns x or replace.
##' @export
safeNull <- function(x, replace=NA){

    ## If null, return NA:
    if (is.null(x)) {
        return(replace)
    }

    ## If length is zero, return NA:
    if (length(x) == 0){
        return(replace)
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
##' @param yearsOfHistory The amount of years to lookback. Default is 5.
##' @param endOfWeek For weekly periodicity, the end of week day. Default is "Friday".
##' @param date The date to traverse back from.
##' @return A sequence of dates for periodicity.
##' @export
periodDates <- function(periodLetter, yearsOfHistory=5, endOfWeek="Friday", date=Sys.Date()){

    ## Generate date sequence:
    dateSeq <- seq(date - (365 * yearsOfHistory), date, by=1)

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

                        "S" = function(x) {
                            y <- ifelse(numerize(substr(x, 6, 10)) >= 100 & numerize(substr(x, 6, 10)) <= 0630, "S1", "S2")
                            x[!y == c(tail(y, -1), tail(y, 1))]
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
##' @param date The date to traverse back from.
##' @return The specific date of the date memnonic.
##' @export
dateOfPeriod <- function(memnonic="D-0", date=Sys.Date()){

    ## Get the period letter:
    period <- strsplit(memnonic, "-")[[1]][1]

    ## Get the lag:
    lag <- as.numeric(strsplit(memnonic, "-")[[1]][2])

    ## Get the period dates
    series <- periodDates(period, date=date)

    ## Remove the date in the series for the weekly query:
    if (period == "W") {
        series <- series[series != date]
    }

    ## Apply lag and return:
    series[length(series) - lag]

}


##' A function to safely rbind a list with data frames.
##'
##' This is a description.
##'
##' @param x A list with data frames.
##' @param convertToString Shall all values be converted to strings. Default is FALSE.
##' @return A single data frame.
##' @export
safeRbind <- function(x, convertToString=FALSE){

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

    ## Iterate over the list and populate the new data frame:
    for (i in 1:length(x)){

        ## Get the current data frame from list:
        ca <- x[[i]]

        if (class(ca) == "list") {
            ca <- data.frame(do.call(cbind, .removeList(ca)), stringsAsFactors=FALSE, check.names=FALSE)
        }

        ## Compute the ending row given current data-frames dimension:
        eRow <- NROW(ca) + sRow - 1

        ## Convert values to string if required:
        if (convertToString) {
            ca <- dfToStrings(ca)
        }

        ## Assign current data-frame to the main df.
        df[sRow:eRow, colnames(ca)] <- ca

        ## Compute the new starting row:
        sRow <- sRow + NROW(ca)
    }

    ## Return:
    data.frame(df, stringsAsFactors=FALSE, check.names=FALSE)
}


##' This function converts data frame into matrix with strings
##'
##' This is a description.
##'
##' @param df The data frame.
##' @return A matrix with strings.
##' @export
dfToStrings <- function(df) {

    ## If single row, apply & transpose:
    if (NROW(df) == 1) {
        df <- t(apply(df, MARGIN=2, function(x) trimws(as.character(x))))

    }

    ## If multiple rows, apply:
    if (NROW(df) > 1) {
        df <- apply(df, MARGIN=2, function(x) trimws(as.character(x)))
    }

    ## Done, return:
    return(df)
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
    paste0(format(round(as.numeric(number) * 100, digit), nsmall=digit), " %")
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

    ## Short circuit out:
    if (NROW(df) == 0) {
        return(NA)
    }

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
        is.na(x) | nchar(x) == 0 | x == " "
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
    if (any(is.na(trial) && ncharStr >= 8)) {
        trial <- as.Date(str, format="%Y/%m/%d")
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
##' @param onlyRows Shall only rows be cleaned? Default is FALSE.
##' @return A data frame with cleaned NA rows and columns.
##' @export
cleanNARowsCols <- function(df, ratio=0.85, onlyRows=FALSE){

    ## Return if no data:
    if (NROW(df) == 0) {
        return(df)
    }

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

    ## If onlyRows is FALSE, clean columns, too:
    if (!onlyRows) {

        ## Determine the columns to be excluded:
        naCols <- apply(df, MARGIN=2, function(x) all(isNAorEmpty(x)))
        ## Exclude the columns and return:
        df <- df[,!naCols]
    }

    ## Done, return:
    return(df)
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

    naStr <- is.na(str)

    ## The ellipsis vector:
    ellipsis <- ifelse(nchar(as.character(str)) > charLimit, "...", "")
    ## Paste and return:
    str <- paste0(substr(str, 1, charLimit), ellipsis)

    if (capitalise) {
        str <- toupper(str)
    }

    ## NA's stay NA:
    str[naStr] <- NA

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

    if (all(is.na(df[, col]))) {
        return(rep(FALSE, NROW(df)))
    }

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


##' This function returns the deployment string name given session info
##'
##' This is a description.
##'
##' @param session the rdecaf session.
##' @return A character vector of the deployment name.
##' @export
getDepName <- function(session) {
    return(str_remove(str_split(session$location,"\\.")[[1]][1],"https://") %>% toupper())
}
