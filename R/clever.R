##' The main clever function which prepares the standard input for jobs:
##'
##' This is the description
##'
##' @return NULL.
##' @export
clever <- function () {

    ## Set error to null to leak secrets:
    options(error=NULL)

    ## Get the job id:
    job <- Sys.getenv("DECAF_CLEVER_JOB_ID", "00000000-0000-0000-0000-000000000000")

    ## Get the job configuration:
    config <- jsonlite::fromJSON(Sys.getenv("DECAF_CLEVER_JOB_CONFIG", "{}"))

    ## Get job parameters:
    params <- jsonlite::fromJSON(Sys.getenv("DECAF_CLEVER_JOB_PARAMS", "{}"))

    ## Get API location, apikey and apisecret for the session:
    apiurl <- sprintf("%s/api", config[["auth"]][["url"]])
    apikey <- config[["auth"]][["credentials"]][["apikey"]]
    apiscr <- config[["auth"]][["credentials"]][["apisecret"]]

    ## Build rdecaf session:
    session <- rdecaf::makeSession("location"=apiurl, "apikey"=apikey, "apisecret"=apiscr)

    ## Build rdecaf client:
    client <- do.call(rdecaf::DecafClient[["new"]], config[["auth"]])

    ## Check that we have a valid session:
    client$bare$get("/apis/estate/me", failonerror=TRUE)

    ## Call the function:
    runCleverFunction(params[["function"]],
                      "client"=client,
                      "session"=session,
                      "config"=config,
                      "job"=job,
                      "arguments"=params[["arguments"]])
}


##' Issues a DECAF Clever annotation output message and stops the
##' script with the same message.
##'
##' @param msg Annotation message.
##' @export
clever_stop <- function(msg) {
    ## Make sure that the message is not NULL.
    if (is.null(msg)) {
        msg <- "NULL"
    }

    ## Issue a DECAF Clever job annotation with the message:
    clever_annotate(msg)

    ## Raise exception with the message:
    stop(msg)
}


##' A function to generate the clever message syntax.
##'
##' This is the description
##'
##' @param msg A message as string.
##' @export
clever_annotate <- function(msg) {
    ## Make sure that the message is not NULL.
    if (is.null(msg)) {
        msg <- "NULL"
    }

    ## Issue a DECAF Clever job annotation with the message:
    cat(sprintf("::clever::annotate::message=%s\n", msg))
}

##' The clever wrapper for running a function.
##'
##' This is the description
##'
##' @param funcname The Name of the function to run.
##' @param client The list with the client info.
##' @param session The rdecaf session.
##' @param arguments The list with workhorse arguments.
##' @param config The list with the clever configuration.
##' @param job The job information
##' @return NULL. Email with the alert will be sent.
##' @export
runCleverFunction <- function(funcname, client, session, config, job, arguments) {
    ## Get the function itself:
    func <- eval(parse(text=funcname))

    ## Get formals spec for the func:
    funcFormals <- formals(args(func))

    ## Define the expected arguments:
    expArgs <- funcFormals[!names(funcFormals) %in% "..."]
    expArgs <- names(Filter(function(x) class(x) == "name", expArgs))

    ## If any argument missing, return:
    if (any(sapply(expArgs, function(x) !exists(x)))) {
        clever_stop(sprintf("Missing arguments: %s", paste0(names(which(sapply(expArgs, function(x) !exists(x)))), collapse=", ")))
    }

    ## Prepare actual arguments:
    actualArgs <- c(list(client=client, session=session, config=config, job=job), arguments)

    ## Run the function:
    retval <- try(do.call(func, actualArgs))

    ## Check the return value:
    if (class(retval) == "try-error") {
        print(retval)
        clever_stop("Error in Function!")
    }

    ## Done, return with annotation:
    clever_annotate(retval)

}


##' DECAF Clever job example.
##'
##' This example attempts to log some DECAF Instance information that is
##' retrieved from DECAF Instance API.
##'
##' @param session `rdecaf` session.
##' @param fail If `TRUE`, the function will stop with an error message.
##' @param ... Further arguments to be ignored.
##' @export
clever_example <- function (session, fail=FALSE, ...) {

    ## Get the server timestamp:
    info <- rdecaf::getResource("info", session=session)

    ## Get the server timestamp:
    version <- rdecaf::getResource("version", session=session)

    ## Log DECAF instance server timestamp:
    cat(sprintf("DECAF Barista version is %s.\n", version[["version"]]))

    ## Log DECAF instance server timestamp:
    cat(sprintf("DECAF Instance identifier is %s-%s.\n", info[["code"]], info[["type"]]))

    ## Log DECAF instance server timezone:
    cat(sprintf("DECAF Instance timezone is %s.\n", info[["timezone"]]))

    ## Log DECAF instance server timestamp:
    cat(sprintf("DECAF Instance timestamp is %s.\n", info[["timeutc"]]))

    ## Log DECAF instance server timestamp:
    cat(sprintf("DECAF Instance local time is %s.\n", info[["timeloc"]]))

    ## Fail if the call-site has requested so:
    if (fail) {
        stop("I am a failure!")
    }
}
