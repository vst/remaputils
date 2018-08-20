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
