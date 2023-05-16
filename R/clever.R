##' The main clever function which prepares the standard input for jobs:
##'
##' This is the description
##'
##' @return NULL.
##' @export
clever <- function () {

    ## Set error to null to leak secrets:
    options(error = NULL)

    ## Get the job id:
    job <- Sys.getenv("DECAF_CLEVER_JOB_ID", "00000000-0000-0000-0000-000000000000")

    ## Get the job configuration:
    config <- jsonlite::fromJSON(Sys.getenv("DECAF_CLEVER_JOB_CONFIG", "{}"))

    ## Get the job payload:
    payload <- jsonlite::fromJSON(Sys.getenv("DECAF_CLEVER_JOB_PROGRAM_DATA_PAYLOAD", "{}"))

    ## Build rdecaf session:
    session <- rdecaf::makeSession("location"=sprintf("%s/api", config[["auth"]][["url"]]),
                                   "apikey"=config[["auth"]][["apikey"]],
                                   "apisecret"=config[["auth"]][["apisecret"]])

    ## Build rdecaf client:
    client <- rdecaf::DecafClient[["new"]](
        "url"=config[["auth"]][["url"]],
        "credentials"=list("apikey"=config[["auth"]][["apikey"]],
                           "apisecret"=config[["auth"]][["apisecret"]]))

    ## Check that we have a valid session:
    rdecaf::getResource("me", session = session)

    ## Call the function:
    runCleverFunction(payload[["function"]],
                      list("client"=client,
                           "session"=session,
                           "arguments"=payload[["arguments"]],
                           "config"=config,
                           "job"=job))
}


##' Issues a DECAF Clever annotation output message and stops the
##' script with the same message.
##'
##' @param msg Annotation message.
##' @export
clever_stop <- function(msg) {
    clever_annotate(msg)
    stop(msg)
}


##' A function to generate the clever message syntax.
##'
##' This is the description
##'
##' @param msg A message as string.
##' @export
clever_annotate <- function(msg) {
    cat(sprintf("::clever::annotate::message=%s\n", msg))
}


run_cleverExample <- function(client, session, arguments, config, job) {

    ## Extract argument names:
    argument_names <- names(arguments)

    ## Check if expected arguments are provided:
    if (!("check_this" %in% argument_names)) {
        clever_stop("Argument 'check_this' is not set.")
    } else if (!("check_that" %in% argument_names)) {
        clever_stop("Argument 'check_that' is not set.")
    }

    ## Call the workhorse function:
    retval <- cleverExample(arguments[["check_this"]], arguments[["check_that"]], session)

    ## Annotate with the returned value:
    clever_annotate(retval)
}


cleverExample <- function (check_this, check_that, session) {

    ## Get the server timestamp:
    info <- rdecaf::getResource("info", session = session)

    ## Return the value:
    return(sprintf("Healthy. Server timestamp is %s.", info[["timeutc"]]))
}


##' The clever wrapper for running a function.
##'
##' This is the description
##'
##' @param func The function to run.
##' @param client The list with the client info.
##' @param session The rdecaf session.
##' @param arguments The list with workhorse arguments.
##' @param config The list with the clever configuration.
##' @param job The job information
##' @return NULL. Email with the alert will be sent.
##' @export
runCleverFunction <- function(func, client, session, arguments, config, job) {

    ## Define the expected arguments:
    ## expArgs <- c("session", "resources", "emailParams", "deployment", "url", "fieldName")
    expArgs <- formals(myfun)[!names(formals(func)) %in% "..."]
    expArgs <- names(Filter(function(x) class(x) == "name", expArgs))

    ## If any argument missing, return:
    if (any(sapply(expArgs, function(x) !exists(x)))) {
        clever_stop(sprintf("Missing arguments: %s", paste0(names(which(sapply(expArgs, function(x) !exists(x)))), collapse=", ")))
    }

    ## Check the email params:

    ## Run the function:
    retval <- try(do.call(func, arguments))

    ##
    if (class(retval) == "try-error") {
        print(retval)
        clever_stop("Error in Function!")
    }

    if (retval == "Email Failed!") {
        clever_stop(retval)
    }

    ## Done, return with annotation:
    clever_annotate(retval)

}
