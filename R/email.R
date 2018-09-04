##' A function to send email with a custom email body.
##'
##' This is the description
##'
##' @param from The sender information.
##' @param emailList The vector with recipient emal addresses.
##' @param subject The subject of the email.
##' @param body The body of the email.
##' @param isLocal Boolean to indicate whether it is a test environment. Default is FALSE.
##' @param html Boolean to indicate whether body is html. Defaul is TRUE.
##' @import mailR
##' @import jsonlite
##' @import httr
##' @return A java-object of class org.apache.commons.mail.SimpleEmail
##' @export
emailReport <- function(from, emailList, subject, body, isLocal=FALSE, html=TRUE){

    ## All settings:
    globalSettings <- jsonlite::fromJSON("~/.decaf.json")[["settings"]]

    ## Global smtp settings:
    smtpSettings <- globalSettings[["smtp"]]

    ## If local deployment, send to local email. Otherwise to list:
    if (isLocal) {
        recipient <-  as.character(emailList[names(emailList) == "local"])
    } else {
        recipient <- as.character(emailList)
    }

    ## This is the smtp params:
    smtp <- list(host.name=smtpSettings[["host"]],
                 port=smtpSettings[["port"]],
                 user.name=smtpSettings[["user"]],
                 passwd=smtpSettings[["pass"]],
                 TLS=smtpSettings[["tls"]])

    ## Send to slack if production:
    if (!isLocal) {
        httr::POST(globalSettings[["slack"]][["webhook"]], body=list(text=subject), encode="json")
    }

    ## Send email:
    send.mail(from=from,
              to=recipient,
              subject=subject,
              body=body,
              smtp=smtp,
              html=html,
              authenticate=TRUE)
}


##' A function to send system emails using the an HTML email template.
##'
##' The function searces for keywords in an HTML template and replaces
##' the keywords with some other HTML string.
##'
##' @param template The HTML template.
##' @param updateText A list with the keyword and the corresponding replacement string.
##' @param emailParams The parameter list for the email report function.
##' @param timezone The timezone to be used. Default is CET.
##' @return A java-object of class org.apache.commons.mail.SimpleEmail
##' @export
syncUpdateEmail <- function(template, updateText, emailParams, timezone="CET") {

    ## Get the system time:
    sysTime <- format(Sys.time(), "tz"=timezone)

    ## Iterate over updateText and replace placeholders:
    for (i in 1:length(updateText)) {

        ## Get the name of placeholder:
        plcHolder <- names(updateText)[[i]]

        ## Get the replacement text:
        rplcText <- updateText[[i]]

        ## Get the line which has placeholder:
        isPlcHolder <- do.call(c, lapply(template, function(x) length(grep(plcHolder, x)) > 0))

        ## Replace the line with the placeholder:
        template[isPlcHolder] <- gsub(plcHolder, rplcText, template[isPlcHolder])
    }

    ## Construct the subject:
    subject <- paste0(updateText[["DEPLOYMENT"]], " DECAF Data Update: ", sysTime)

    ## Push the email:
    emailReport(emailParams[["from"]],
                emailParams[["emailList"]],
                subject=subject,
                body=paste(template, collapse=""),
                emailParams[["isLocal"]])
}
