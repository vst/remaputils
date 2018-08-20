##' This function handles the log messages
##'
##' This is a description
##'
##' @param x The result value of a condition. Default 1.
##' @param val The value to check against. Default 0.
##' @param condition The condition to check for. Either "equal", "smaller" or "greater". Default is equal.
##' @param errortext The text to be included in the error message. Default is NULL.
##' @param infotext The info text to be included in the success case. Default is NULL.
##' @param warntext The warning text to be included in the dubious success case. Default is NULL.
##' @param logger The logger parameter for logwarn, logerror, loginfo. Default is "".
##' @return A vector of numerical values.
##' @export
messageHandler <- function(x=1, val=0, condition="equal", errortext=NULL, infotext=NULL, warntext=NULL, logger=""){

    ## If there is a warning text, log and return:
    if (!is.null(warntext)) {
        logging::logwarn(paste0("WARNING IN ", sys.call(-1)[[1]], warntext), logger=logger)
        return(NULL)
    }

    ## Log error:
    if (checkCondition(x, val, condition)) {
        logging::logerror(paste0("ERROR IN ", sys.call(-1)[[1]], errortext), logger=logger)
    } else {
        ## Log success:
        logging::loginfo(paste0("SUCCESS IN ", sys.call(-1)[[1]], infotext), logger=logger)
    }
}


##' This function registers the log.
##'
##' This is a description.
##'
##' @return Returns the message of the log resitration.
##' @export
registerLog <- function(){

    ## Get working directory:
    mainDir <- getwd()

    ## Get logger name:
    splitName <- tail(strsplit(getwd(), "/")[[1]], 2)

    ## Construct the logger name:
    logger <- paste(toupper(splitName), collapse="-")
    assign("logger", logger, envir = .GlobalEnv)

    ## Construct the log file name:
    file <- paste0(tolower(logger), ".log")

    ## Register the handler for writing to log file:
    logging::addHandler(logging::writeToFile, logger=logger, file=file)

    ## Register the handler for writing to console:
    logging::addHandler(logging::writeToConsole)
    logging::loginfo(paste0("Starting new run:", Sys.time()), logger)

    ## Message:
    messageHandler()
}
