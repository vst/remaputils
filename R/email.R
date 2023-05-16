##' A function to email alerts.
##'
##' This is the description
##'
##' @param df The data frame to be shown as html at bottom of email.
##' @param team The name of team. To be provided if team-wise email is chosen. Default=NULL.
##' @param emailList The list of email addresses to be sent to.
##' @param emailParams The email parameters.
##' @param provider The name of the data provider. Default = 'DECAF'.
##' @param caption The caption for the table. Default = NULL.
##' @param sourceType The name of the source. Descriptive only. Default = API.
##' @param emailBody The email body text.
##' @param deployment The name of the deployment.
##' @param subject The subject of the email.
##' @param session The rdecaf session.
##' @param attachments The path to files for attachment. Default NULL.
##' @return NULL. An email with the alert will be sent.
##' @export
emailAlert <- function(df,
                       team=NULL,
                       emailList,
                       emailParams,
                       provider="DECAF",
                       caption=NULL,
                       sourceType="API",
                       emailBody,
                       deployment,
                       subject,
                       session,
                       attachments=NULL) {

    ## If empty data frame, return NULL:
    !NROW(df) == 0 || return(NULL)

    ## Prepare the data frame as html:
    result <- as.character(emailHTMLTable(df,
                                          provider=provider,
                                          caption=caption,
                                          sourceType=sourceType))

    ## Parse LINK columns, if any:
    result <- gsub("&#62;LINK&#60;/a&#62;", ">LINK<aya/a>", result)
    result <- gsub("&#60;a href", "<a href", result)

    ## Prepare the greeting:
    greeting <- strsplit(paste0("Dear ", team, " Team"), " ")[[1]]
    greeting <- trimExcessWs(paste(greeting[!duplicated(greeting)], collapse=" "))

    ## Prepare the email content:
    .UPDATETEXT <- list(GREETINGPLACEHOLDER=greeting,
                        EMAILBODYPLACEHOLDER=emailBody,
                        CALLTOACTIONPLACEHOLDER = "Go to System",
                        DEPLOYMENT=deployment,
                        URLPLACEHOLDER=gsub("/api", "", session[["location"]]),
                        FINALPARAGRAPHPLACEHOLDER = "Please contact us if you experience any issues or have questions/feedback.",
                        ADDRESSPLACEHOLDER="",
                        GOODBYEPLACEHOLDER="Best Regards,<br>DECAF TEAM",
                        ADDENDUMPLACEHOLDER=result)

    ## Override email list:
    emailParams[["emailList"]] <- emailList

    ## Send:
    syncUpdateEmail(template=readLines("../assets/update_email.html"),
                    updateText=.UPDATETEXT,
                    emailParams = emailParams,
                    subject=subject,
                    attachments=attachments)

}


##' A function to extract a data frame to a list based on ownership.
##'
##' This is the description
##'
##' @param df The data frame.
##' @param fld The column name where account id information is.
##' @param session The rdecaf session.
##' @return A team wise list with the segregated data frame, team name and users.
##' @export
separateDFByTeam <- function(df, fld, session) {

    ## Construct the params:
    params <- list("page_size"=-1,
                   "format"="csv")

    ## Get the users:
    users <- as.data.frame(getResource("users", params=params, session=session))

    ## Get the teams:
    teams <- as.data.frame(getResource("teams", params=params, session=session))

    ## Get the accounts:
    accounts <- as.data.frame(getResource("accounts", params=params, session=session))

    ## Appned the portfolio id to trades:
    df[, "portfolio"] <- accounts[match(df[, fld], accounts[, "id"]), "portfolio"]

    ## Iterate over teams and separete by team users:
    teamWise <- apply(teams, MARGIN=1, function(row) {

        ## Get the trades with portfolio id matching portfolio id in team:
        df <- df[!is.na(match(df[, "portfolio"] , as.numeric(na.omit(row[safeGrep(names(row), "portfolios.") == "1"])))), ]

        list("df"=df,
             "users"=users[!is.na(mxgrep(users[, safeGrep(colnames(users), "teams.") == "1"], row["id"])), ],
             "team"=row["name"])
    })

    ## Done, return:
    return(teamWise)

}


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
##' @param attachments The paths to attachments.
##' @import mailR
##' @import jsonlite
##' @import httr
##' @return A java-object of class org.apache.commons.mail.SimpleEmail
##' @export
emailReport <- function(from, emailList, subject, body, isLocal=FALSE, html=TRUE, attachments=NULL){

    ## Get the job configuration:
    config <- jsonlite::fromJSON(Sys.getenv("DECAF_CLEVER_JOB_CONFIG", "null"))

    if (is.null(config)) {

        ## All settings:
        globalSettings <- jsonlite::fromJSON("~/.decaf.json")[["settings"]]

        ## Global smtp settings:
        smtpSettings <- globalSettings[["smtp"]]

    } else {

        smpt$Settings <- config[["smtp"]]
    }

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
        ## httr::POST(globalSettings[["slack"]][["webhook"]], body=list(text=subject), encode="json")
    }

    ## Send email:
    send.mail(from=from,
              to=recipient,
              subject=subject,
              body=body,
              smtp=smtp,
              attach.files=attachments,
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
##' @param subject The subject of the email.
##' @param attachments The paths to attachments.
##' @return A java-object of class org.apache.commons.mail.SimpleEmail
##' @export
syncUpdateEmail <- function(template, updateText, emailParams, timezone="CET", subject=" DECAF Data Update: ", attachments=NULL) {

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
    subject <- paste0(updateText[["DEPLOYMENT"]], subject, sysTime)

    ## Push the email:
    emailReport(emailParams[["from"]],
                emailParams[["emailList"]],
                subject=subject,
                body=paste(template, collapse=""),
                isLocal=emailParams[["isLocal"]],
                attachments=attachments)
}


##' A function to generate email-friendly HTML table.
##'
##' TODO:
##'
##' @param df A data-frame.
##' @param provider The name of the provider of the data to be used in the footer.
##' @param caption The caption of the table to be used.
##' @param sourceType The type of the source data to be used for the footer.E.g., 'Api'.
##' @param rowGroups A list of two elements of the same length. The first
##' element will contain the row spans (i.e. a numeric atomic vector) whereas
##' the second element will contain the names (i.e. a character atomic vector).
##' See the examples for more info. Defauls to NULL
##' @param rowBGColor A list of two elements with the row indices for alternative background color.
##' Default in NULL in which case it constructs itself.
##' @param collapse The table collapse parameters. Default is separate.
##' @param spacing The cell spacing of the table. Default is 4px.",
##' @param footer The footer to be used. Default is NULL in which case it constructs itself.
##' @return A java-object of class org.apache.commons.mail.SimpleEmail
##' @export
emailHTMLTable <- function(df,
                           provider,
                           caption,
                           sourceType="API",
                           rowGroups=NULL,
                           rowBGColor=list(),
                           collapse="separate",
                           spacing="4px",
                           footer=NULL) {

    ## Construct the alternative background color for rows if NULL:
    if (length(rowBGColor) == 0) {
        rowBGColor[["rowsOdd"]] <- seq(min(NROW(df), 2), NROW(df) + 1, 2)
        rowBGColor[["rowsEvn"]] <- seq(min(NROW(df), 3), NROW(df) + 1, 2)
    }

    ## Construct the footer if NULL:
    if (is.null(footer)) {
        footer <- paste0("Source Reference: ", toupper(provider), " " , sourceType)
    }

    ## Get the html table for df:
    tableHTML::tableHTML(df,
                         border=0,
                         spacing=spacing,
                         collapse=collapse,
                         row_groups=rowGroups,
                         footer=footer,
                         caption=caption,
                         rownames=FALSE) %>%
    tableHTML::add_css_caption(css = list(c('color', 'font-size', 'text-align', 'margin-bottom'), c("#666666", '18px', 'center', '10px'))) %>%
    tableHTML::add_css_footer(css = list(c('color', 'font-size', 'text-align', 'margin-top'), c("#999999", '12px', 'center', '10px'))) %>%
    tableHTML::add_css_column(css = list(c("text-align"), c("left")), columns=colnames(df)) %>%
    tableHTML::add_css_row(css = list(c('background-color'), c('#E5E8E8')), rows = rowBGColor[["rowsOdd"]]) %>%
    tableHTML::add_css_row(css = list(c('background-color'), c('#BFC9CA')), rows = rowBGColor[["rowsEvn"]])

}
