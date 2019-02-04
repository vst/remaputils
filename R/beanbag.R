##' A function to define a fund report composition
##'
##' This is a description.
##'
##' @param portfolio The portfolio id
##' @param ccy The currency of reporting
##' @param date The date of reporting
##' @param dtype The date type of reporting
##' @param regions The list with the country to region mapping
##' @param session The rdecaf session
##' @return A data-frame with the printable format of holdings
##' @export
COMPOSITION1 <- function(portfolio, ccy, date, dtype, regions, session){

    ## The toplevel for this composition:
    toplevel <- "Subtype"

    ## The sublevel for this compositions:
    sublevel <- NULL

    ## The custom top levels for this composition:
    customtop <- c("subTypeParser", "ctypeByRegionAndDirection")

    ## The columsn to be selected for this composition:
    colselect <- c(
        "Name", "QTY", "CCY", "PX Cost", "PX Last", "Value", "Accrd", "Value (%)", "Exposure", "Exp (%)", "PnL (Unrl)", "PnL (%Inv)", "order"
    )

    ## The summary addons for this composition:
    summaryaddon <- c("currency", "country", "region", "sector")

    ## Get the indata:
    getPrintableHoldings(portfolio, ccy, date, dtype, toplevel, sublevel, customtop, colselect, regions, summaryaddon, session, ctbrInclude=c("Share"))

}


##' A function to define a fund report composition
##'
##' This is a description.
##'
##' @param portfolio The portfolio id
##' @param ccy The currency of reporting
##' @param date The date of reporting
##' @param dtype The date type of reporting
##' @param regions The list with the country to region mapping
##' @param session The rdecaf session
##' @return A data-frame with the printable format of holdings
##' @export
COMPOSITION2 <- function(portfolio, ccy, date, dtype, regions, session){

    ## The toplevel for this composition:
    toplevel <- "Subtype"

    sublevel <- NULL

    customtop <- c("subTypeParser")

    ## The columsn to be selected for this composition:
    colselect <- c(
        "Name", "QTY", "CCY", "PX Cost", "PX Last", "Value", "Accrd", "Value (%)", "Exposure", "Exp (%)", "PnL (Unrl)", "PnL (%Inv)", "order"
    )

    ## The summary addons for this composition:
    summaryaddon <- c("currency", "country", "region", "sector")

    ## Get the indata:
    getPrintableHoldings(portfolio, ccy, date, dtype, toplevel, sublevel, customtop, colselect, regions, summaryaddon, session)

}


##' A function to define a fund report composition
##'
##' This is a description.
##'
##' @param portfolio The portfolio id
##' @param ccy The currency of reporting
##' @param date The date of reporting
##' @param dtype The date type of reporting
##' @param regions The list with the country to region mapping
##' @param session The rdecaf session
##' @return A data-frame with the printable format of holdings
##' @export
COMPOSITION3 <- function(portfolio, ccy, date, dtype, regions, session){

    sourceFromFolder("~/projects/remaputils/R", pattern="*.R")

    ## The toplevel for this composition:
    toplevel <- "Subtype"

    ## The columsn to be selected for this composition:
    colselect <- c(
        "Name", "QTY", "CCY", "Expiry", "Rate", "PX Cost", "PX Last", "Value", "Value (%)", "Exposure", "Exp (%)", "PnL (Unrl)", "PnL (%Inv)", "order"
    )

    sublevel <- NULL

    customtop <- c("subTypeParser",
                   "ctypeByRegionAndDirection",
                   "ctypeByDuration",
                   "longStraddle",
                   "shortStraddle",
                   "closedOption",
                   "calendarSpread",
                   "coveredCall",
                   "callSpread",
                   "putSpread",
                   "syntheticLong"
                   )

    ## The summary addons for this composition:
    summaryaddon <- c("currency", "country", "region", "sector")

    ## Get the indata:
    getPrintableHoldings("portfolio"=portfolio,
                         "ccy"=ccy,
                         "date"=date,
                         "dtype"=dtype,
                         "toplevel"=toplevel,
                         "sublevel"=sublevel,
                         "customTop"=customtop,
                         "colselect"=colselect,
                         "regions"=regions,
                         "summaryaddon"=summaryaddon,
                         "session"=session,
                         calendarSpreadExcl=TRUE,
                         longStraddleExcl=TRUE,
                         shortStraddleExcl=TRUE,
                         closedOptionExcl=TRUE,
                         synthLongExcl=TRUE,
                         callSprdExcl=TRUE,
                         putSprdExcl=TRUE,
                         cvrdCallExcl=TRUE,
                         ctbrInclude=c("Share", "EQY", "Common Stock"),
                         ctbdInclude=c("Option", "Share"),
                         ctbdHorizon=list("steps"=c(10, 30),
                                          "periodicity"="days"))

}


##' A function to define a fund report composition
##'
##' This is a description.
##'
##' @param portfolio The portfolio id
##' @param ccy The currency of reporting
##' @param date The date of reporting
##' @param dtype The date type of reporting
##' @param regions The list with the country to region mapping
##' @param session The rdecaf session
##' @return A data-frame with the printable format of holdings
##' @export
COMPOSITION4 <- function(portfolio, ccy, date, dtype, regions, session){

    ## The toplevel for this composition:
    toplevel <- "Subtype"

    ## The sublevel for this compositions:
    sublevel <- NULL

    ## The custom top levels for this composition:
    customtop <- c("subTypeParser", "ctypeByRegionAndDirection")

    ## The columsn to be selected for this composition:
    colselect <- c(
        "Name", "QTY", "CCY", "PX Cost", "PX Last", "Value", "Value (%)", "Exposure", "Exp (%)", "PnL (Unrl)", "PnL (%Inv)", "order"
    )

    ## The summary addons for this composition:
    summaryaddon <- c("currency", "country", "region", "sector")

    ## Get the indata:
    getPrintableHoldings("portfolio"=portfolio,
                         "ccy"=ccy,
                         "date"=date,
                         "dtype"=dtype,
                         "toplevel"=toplevel,
                         "sublevel"=sublevel,
                         "customTop"=customtop,
                         "colselect"=colselect,
                         "regions"=regions,
                         "summaryaddon"=summaryaddon,
                         "session"=session,
                         ctbrInclude=c("Share"))
}


##' Defines a function to retreat the report data:
##'
##' @param indata Data as returned by `getPrintableHoldings` function.
##' @return Retreated report data:
##' @export
retreatReport <- function (indata) {
    ## Get holdings:
    holdings <- indata$holdings

    fixit <- function (x) as.numeric(gsub("%", "", gsub(" ", "", gsub(",", "", x))))

    ## Revisit holdings:
    holdings$QTY <- fixit(holdings$QTY)
    holdings$"PX Cost" <- fixit(holdings$"PX Cost")
    holdings$"PX Last" <- fixit(holdings$"PX Last")
    holdings$Value <- fixit(holdings$Value)
    holdings$Accrd <- .emptyToNULL(fixit(holdings$Accrd))
    holdings$"Value (%)" <- fixit(holdings$"Value (%)") / 100
    holdings$Exposure <- fixit(holdings$Exposure)
    holdings$"Exp (%)" <- fixit(holdings$"Exp (%)") / 100
    holdings$"PnL (Unrl)" <- fixit(holdings$"PnL (Unrl)")
    holdings$"PnL (%Inv)" <- fixit(holdings$"PnL (%Inv)") / 100
    holdings$order <- fixit(holdings$order)

    ## Slive the data to be printed:
    ## exdata <- holdings[,1:11]

    ## Done, return the reconstracted input data:
    list(holdings=holdings, consolidation=indata$consolidation)
}


##' Writes table header.
##'
##' Writes the table header to the `row` of `sheet` with given
##' alignments.
##'
##' @param sheet Sheet to write the table header to.
##' @param row Row of sheet to write the table header to.
##' @param header Characters vector containing table header columns.
##' @param alignments Vector of values from `{r, l, c}` for cell.
##' @param baseStyle Base style for the cells
##' @return Next row number.
##' @export
writeTableHeader <- function (sheet, row, header, alignments, baseStyle) {

    ## Translate alignments and filter out nulls:
    alignments <- Filter(Negate(is.null), lapply(alignments, function (x) .lookupAlignment()[[x]]))

    ## Ensure that we have same size header and alignment vectors:
    assertit(length(header) == length(alignments), "Header column and alignment vectors are not of same size")

    ## First, create the row:
    rows <- createRow(sheet, rowIndex=row)

    ## Now create the cells:
    cells <- createCell(rows, colIndex=1:length(header))

    ## Set cell values:
    mapply(setCellValue, cells[1,], header)

    ## Create the list of styles:
    styles <- lapply(alignments, function (x) baseStyle + x)

    ## Apply styles:
    mapply(setCellStyle, cells[1,], styles)

    ## We should be done by now. Return the next row:
    row + 1
}


##' Writes a value to cell to the sheet[row, col] and styles it.
##'
##' @param sheet Sheet to write the cell to.
##' @param row Row address of the cell.
##' @param col Col address of the cell.
##' @param value Cell value.
##' @param style Cell style.
##' @return written cell.
##' @export
writeCell <- function (sheet, row, col, value, style) {
    ## Attempt to get the row:
    rows <- getRows(sheet, row)

    ## Check if we have an existing row, if not create:
    if (length(rows) == 0) {
        rows <- createRow(sheet, rowIndex=row)
    }

    ## Attempt to get the cell:
    cells <- getCells(rows, colIndex=col)

    ## Check if we have an existing cell and get it, if not create one:
    if (is.null(cells)) {
        cell <- createCell(rows, colIndex=col)[1,1][[1]]
    } else {
        cell <- cells[[1]]
    }

    ## Set the cell value:
    setCellValue(cell, value, showNA=FALSE)

    ## Set the cell style:
    setCellStyle(cell, style)

    ## Return the cell:
    cells
}


##' Writes the fund report.
##'
##' @param report Report data.
##' @param file Path to file to write the excel to.
##' @return The workbook written to the file.
##' @export
writeFundReport <- function (report, file) {

    ## If no holdings, mask one:
    if (is.null(report$holdings)) {
        report$holdings <- data.frame("Name"=NA,
                                      "QTY"=NA,
                                      "CCY"=NA,
                                      "Value"=NA,
                                      "Value (%)"=NA,
                                      "Exposure"=NA,
                                      "order"=1,
                                      check.names=FALSE)
    }

    ## Define function globals:
    .SHEETNAME <- "Default"
    .GLOBAL.FONT.NAME <- "DejaVu Sans"
    .GLOBAL.FONT.SIZE <- 9
    .GLOBAL.BORDER.COLOR <- "black"
    .GLOBAL.BORDER.PEN <- "BORDER_MEDIUM"
    .GLOBAL.FILL.COLOR <- "#e2dcea"
    .TITLE.FONT.SIZE <- 28
    .WIDTH <- length(which(safeGrep(colnames(report$holdings), "order") == 0))

    ## Get the holdings:
    holdings <- report$holdings[, 1:.WIDTH]

    ## Create the workbook:
    workbook <- createWorkbook(type="xlsx")

    ## Create the default sheet:
    sheet <- createSheet(workbook, sheetName=.SHEETNAME)

    ## Define the global default cell style:
    styleGlobal <- CellStyle(workbook) + Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=.GLOBAL.FONT.SIZE)

    styleShrcls <- styleGlobal + Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=6, isBold=TRUE) +
        Fill(backgroundColor="#000000", foregroundColor=.GLOBAL.FILL.COLOR, pattern="SOLID_FOREGROUND")

    ## Define the cell style for "TITLE":
    styleTitle <- styleGlobal +
        Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=.TITLE.FONT.SIZE, isBold=TRUE) +
        Fill(backgroundColor="#000000", foregroundColor=.GLOBAL.FILL.COLOR, pattern="SOLID_FOREGROUND")

    ## Define the cell style for "LABEL":
    styleLabel <- styleGlobal +
        Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=.GLOBAL.FONT.SIZE, isBold=TRUE)

    ## Define the cell style for "TABLE HEADER":
    styleHeader <- styleGlobal +
        Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=.GLOBAL.FONT.SIZE, isBold=TRUE) +
        Border(color=.GLOBAL.BORDER.COLOR, pen=c("BORDER_MEDIUM", "BORDER_THIN"), position=c("TOP", "BOTTOM")) +
        Fill(backgroundColor="#000000", foregroundColor=.GLOBAL.FILL.COLOR, pattern="SOLID_FOREGROUND")

    ## Starting row:
    nextRow <- 1

    ## Add Title. First, make sure that we merge the region:
    addMergedRegion(sheet, 1, 1, 1, .WIDTH)

    ## Now add the title:
    writeCell(sheet, nextRow, 1, report$consolidation$fund$name, styleTitle + Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER"))
    nextRow <- nextRow + 1
    nextRow <- nextRow + 1

    ## Add Currency:
    writeCell(sheet, nextRow, 1, "Currency", styleLabel)
    writeCell(sheet, nextRow, 2, report$consolidation$ccy, styleGlobal)
    nextRow <- nextRow + 1

    ## Add ISIN:
    writeCell(sheet, nextRow, 1, "ISIN", styleLabel)
    writeCell(sheet, nextRow, 2, .emptyToNA(report$consolidation$isin), styleGlobal)  ## TODO: Add ISIN
    nextRow <- nextRow + 1

    ## Add Launch Date:
    writeCell(sheet, nextRow, 1, "Launch Date", styleLabel)
    writeCell(sheet, nextRow, 2, .formatDate(.emptyToNA(report$consolidation$inception)), styleGlobal + DataFormat("DD/MM/YYYY"))  ## TODO: Add Launch Date
    nextRow <- nextRow + 1

    ## Add Report Date:
    writeCell(sheet, nextRow, 1, "Report Date", styleLabel)
    writeCell(sheet, nextRow, 2, .formatDate(.emptyToNA(report$consolidation$asof)), styleGlobal + DataFormat("DD/MM/YYYY"))
    nextRow <- nextRow + 1
    nextRow <- nextRow + 1

    ## Define the table header cell alignments:
    ##
    ## Note that the alignment is problematic with the current version
    ## of xlsx library. Indeed, it works with xls files, but then we
    ## have other problems. It seems to me that the xlsx writing is
    ## problematic, most likely due to the POI library version xlsx
    ## library is using.
    ##
    ## Therefore, let's define the first column as "centered" so that
    ## all following cells are also centered. Once the bug is fixed,
    ## we can use the first statement instead.


    columnStyles <- list("Name"        =list("align"="c", "rowStyle"="sGlobalRowFillRowFont0", "width"=28L * 256L),
                         "QTY"         =list("align"="l", "rowStyle"="sGlobalRowFillRowFont1", "width"=12L * 256L),
                         "CCY"         =list("align"="l", "rowStyle"="sGlobalRowFillRowFont2", "width"= 4L * 256L),
                         "Expiry"      =list("align"="l", "rowStyle"="sGlobalRowFillRowFont2", "width"=12L * 256L),
                         "Rate"        =list("align"="l", "rowStyle"="sGlobalRowFillRowFont1", "width"=12L * 256L),
                         "PX Cost"     =list("align"="l", "rowStyle"="sGlobalRowFillRowFont3", "width"=12L * 256L),
                         "PX Last"     =list("align"="l", "rowStyle"="sGlobalRowFillRowFont3", "width"=12L * 256L),
                         "Value"       =list("align"="l", "rowStyle"="sGlobalRowFillRowFont1", "width"=13L * 256L),
                         "Accrd"       =list("align"="l", "rowStyle"="sGlobalRowFillRowFont1", "width"= 8L * 256L),
                         "Value (%)"   =list("align"="l", "rowStyle"="sGlobalWgtFillRowFont0", "width"= 9L * 256L),
                         "Exposure"    =list("align"="l", "rowStyle"="sGlobalRowFillRowFont1", "width"=13L * 256L),
                         "Exp (%)"     =list("align"="l", "rowStyle"="sGlobalRowFillRowFont4", "width"= 8L * 256L),
                         "PnL (Unrl)"  =list("align"="l", "rowStyle"="sGlobalRowFillPnlFont0", "width"=10L * 256L),
                         "PnL (%Inv)"  =list("align"="l", "rowStyle"="sGlobalRowFillPnlFont1", "width"= 8L * 256L))

    ## Define the table headers:
    tableHeader <- colnames(holdings)

    ## How shall the table headers be aligned?
    tableAligns <- unlist(sapply(columnStyles[tableHeader], function(x) x[["align"]]))

    ## Write the table header (and a blank one):
    nextRow <- writeTableHeader(sheet, nextRow, tableHeader, tableAligns, styleHeader) + 1

    ## Iterate over each holding row and add to the sheet:
    for (i in 1:nrow(holdings)) {

        ## Is it a position row?
        isPosition <- is.na(report$holdings$order[i])

        ## Is it a sub-total?
        isSubtotal <- !isPosition && grepl("SUBTOTAL", toupper(report$holdings$Name[i]))

        ## Is it a sub-title:
        isSubtitle <- !isPosition && !isSubtotal

        ## Get the row color first:
        if (isSubtitle) {
            rowColor <- "#FFFFFF"
        } else if (isSubtotal) {
            rowColor <- "#C4BD97"
        } else {
            rowColor <- ifelse(i %% 2 == 0, "#E6E6E6", "#F0F0F0")
        }

        ## Define the row fill:
        rowFill <- Fill(backgroundColor="#000000", foregroundColor=rowColor, pattern="SOLID_FOREGROUND")

        if (isSubtotal) {
            wgtForegroundColor <- rowColor
        } else if (isSubtitle) {
            wgtForegroundColor <- "#FFFFFF"
        } else {
            wgtForegroundColor <- "#D3D3D3"
        }

        ## wgtFill <- Fill(backgroundColor="#000000", foregroundColor=ifelse(isSubtotal, rowColor, "#D3D3D3"), pattern="SOLID_FOREGROUND")
        wgtFill <- Fill(backgroundColor="#000000", foregroundColor=wgtForegroundColor, pattern="SOLID_FOREGROUND")

        ## Define the font size (subtitles slightly bigger):
        rowFontSize <- .GLOBAL.FONT.SIZE + ifelse(isSubtitle, 3 - report$holdings$order[i], 0)

        ## Define the row font:
        rowFont <- Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=rowFontSize, isBold=!isPosition, isItalic=isSubtotal)

        pnlIdx <- safeGrep(tableHeader, "PnL")

        ## Define the PnL fill:
        if (!isSubtitle && !is.na(holdings[i, 11]) && holdings[i, 11] < 0) {
            pnlFont <- Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=rowFontSize, isBold=!isPosition, isItalic=isSubtotal, color="#AA3333")
        } else if (!isSubtitle && !is.na(holdings[i, 11]) && holdings[i, 11] > 0) {
            pnlFont <- Font(workbook, name=.GLOBAL.FONT.NAME, heightInPoints=rowFontSize, isBold=!isPosition, isItalic=isSubtotal, color="#33AA33")
        } else {
            pnlFont <- rowFont
        }

        globalAlignment <- Alignment(h="ALIGN_LEFT", indent=30)
        sGlobalRowFont0 <- function() {styleGlobal + rowFont + DataFormat("#,##0.00")}
        styleShrclass0 <- function () {styleShrcls + Alignment(h="ALIGN_CENTER")}
        sGlobalRowFillRowFont0 <- function () {styleGlobal + rowFill + rowFont}
        sGlobalRowFillRowFont1 <- function () {styleGlobal + rowFill + rowFont + DataFormat("#,##0.00") + globalAlignment}
        sGlobalRowFillRowFont2 <- function () {styleGlobal + rowFill + rowFont + DataFormat("@") + globalAlignment}
        sGlobalRowFillRowFont3 <- function () {styleGlobal + rowFill + rowFont + DataFormat("#,##0.00") + Alignment(h="ALIGN_CENTER"}
        sGlobalRowFillRowFont4 <- function () {styleGlobal + rowFill + rowFont + DataFormat("#,##0.00 %")+ globalAlignment}
        sGlobalWgtFillRowFont0 <- function () {styleGlobal + wgtFill + rowFont + DataFormat("#,##0.00 %")+ globalAlignment}
        sGlobalRowFillPnlFont0 <- function () {styleGlobal + rowFill + pnlFont + DataFormat("#,##0.00")+ globalAlignment}
        sGlobalRowFillPnlFont1 <- function () {styleGlobal + rowFill + pnlFont + DataFormat("#,##0.00 %")+ globalAlignment}

        ## Define the list of cell styles:
        cellStyles <- lapply(columnStyles[tableHeader], function(x) do.call(x[["rowStyle"]], list()))

        ## Get the cell values:
        cellValues <- holdings[i, , drop=FALSE]

        ## Define the writer function:
        .writer <- function (idx) writeCell(sheet, nextRow, idx, cellValues[1, idx], cellStyles[[idx]])

        ## Write cells:
        lapply(1:.WIDTH, .writer)

        ## Redefine the next row:
        nextRow <- nextRow + 1

        ## Give a break after subtotals?
        if (isSubtotal) {
            ## writeCell(sheet, nextRow, 8, NA, styleGlobal + Fill(backgroundColor="#000000", foregroundColor="#D3D3D3", pattern="SOLID_FOREGROUND"))
            nextRow <- nextRow + 1
        }
    }

    ## Is the portfolio a mandate?
    isMandate <- all(sapply(report$consolidation$pxinfo, function(x) length(x[["shareclass"]][["subscriptions"]]) == 0 &
                                                                     length(x[["shareclass"]][["feeschedules"]]) == 0))
    ## If it is a mandate, write mandate summary:
    if (isMandate) {
        ## Add NAV row name:
        writeCell(sheet, nextRow, 2, "NAV", styleLabel)
        ## Add the Performance YTD row name:
        writeCell(sheet, nextRow+1, 2, "Peformance (YTD)", styleLabel)

        ## Define the global default cell style:
        for (px in report$consolidation$pxinfo) {
            ## Add NAV value:
            writeCell(sheet, nextRow,   4, .emptyToNA(px$nav$qty), sGlobalRowFont0())
            ## Add the performance ytd value:
            writeCell(sheet, nextRow+1, 4, .emptyToNA(px$ytdext), sGlobalRowFont0())
        }
        ## And update next rows:
        nextRow <- nextRow + 2
    }

    ## If it is a fund, write the fund summary:
    if (!isMandate) {
        ## Add number of certificates:
        writeCell(sheet, nextRow,   2, "Shareclass", styleLabel)
        writeCell(sheet, nextRow+1, 2, "# Certificates", styleLabel)
        writeCell(sheet, nextRow+2, 2, "NAV/Share", styleLabel)
        writeCell(sheet, nextRow+3, 2, "NAV", styleLabel)
        writeCell(sheet, nextRow+4, 2, "GAV", styleLabel)
        writeCell(sheet, nextRow+5, 2, "Peformance (YTD)", styleLabel)

        stCol <- 4

        ## Define the global default cell style:
        for (px in report$consolidation$pxinfo) {

            ## Add number of certificates:
            writeCell(sheet, nextRow, stCol, .emptyToNA(px$shareclass$name),  styleShrclass0())
            ## Add number of certificates:
            writeCell(sheet, nextRow+1, stCol, .emptyToNA(px$sharecount_curr), sGlobalRowFont0())
            ## Add NAV/Share:
            writeCell(sheet, nextRow+2, stCol, .emptyToNA(px$px_clsccy$qty), sGlobalRowFont0())
            ## Add NAV:
            writeCell(sheet, nextRow+3, stCol, .emptyToNA(px$nav_adjusted$qty), sGlobalRowFont0())
            ## Add AuM:
            writeCell(sheet, nextRow+4, stCol, .emptyToNA(px$gav_clsccy$qty), sGlobalRowFont0())
            ## Add Peformance (YTD):
            writeCell(sheet, nextRow+5, stCol, .emptyToNA(px$ytdext), sGlobalRowFont0())

            ## Compute the required width for the shareclass name cell:
            requiredWidth <- ifelse(is.null(px$shareclass$name), 0, nchar(px$shareclass$name) * 120)

            ## Update the width(s) for corresponding columns:
            columnStyles[tableHeader][[stCol]]$width <- max(columnStyles[tableHeader][[stCol]]$width,  requiredWidth)

            ## Next shareclass info, if any:
            stCol <- stCol + 1
        }
        ## And update the new row:
        nextRow <- nextRow + 6
    }

    ## Lastly, we will set print settings:
    sheet$getPrintSetup()$setLandscape(TRUE);
    sheet$getPrintSetup()$setPaperSize(J("org/apache/poi/ss/usermodel/PaperSize", "valueOf", "A4_PAPER"))
    sheet$getPrintSetup()$setFitWidth(.jshort(1L))

    ## Compute the scale to be used:
    scale <- as.integer((35000 / sum(sapply(columnStyles[tableHeader], function(x) x[["width"]]))) * 100)

    ## Dynamically set scale:
    sheet$getPrintSetup()$setScale(.jshort(scale))

    sheet$setMargin(.jshort(0L), 0.2);
    sheet$setMargin(.jshort(1L), 0.2);
    sheet$setMargin(.jshort(2L), 0.2);
    sheet$setMargin(.jshort(3L), 0.2);

    sheet$setRepeatingRows(J("org/apache/poi/ss/util/CellRangeAddress", "valueOf", "1:9"))

    ## Set the column widths:
    lapply(1:length(columnStyles[tableHeader]), function(i) {
        sheet$setColumnWidth(as.integer(i-1), as.integer(columnStyles[[tableHeader[i]]][["width"]]))
    })

    titleRow <- sheet$getRow(0L)
    titleRow$setHeightInPoints(.jfloat(titleRow$getHeightInPoints() * .TITLE.FONT.SIZE / 10))

    ## Save the workbook:
    saveWorkbook(workbook, file=file)

    ## Done, return:
    workbook
}


##' TODO
##'
##' @param orderedHoldings TODO.
##' @param nestedHoldings TODO.
##' @return TODO.
##' @export
appendCash <- function(orderedHoldings, nestedHoldings){

    ## If no cash, return holdings:
    if (NROW(orderedHoldings[["cash"]]) == 0) {
        return(nestedHoldings)
    }

    ## Get the cash positions:
    cash <- orderedHoldings[["cash"]]

    ## Append the title for cash:
    cash <- rbind(c("Cash", rep(NA, NCOL(cash)-1)), cash)

    ## Apend the Cash subtotal row:x
    cash <- rbind(cash, c("Cash Subtotal", rep(NA, NCOL(cash) -1)))

    ## Safely rbind cash and other holdings:
    nestedHoldings <- safeRbind(list(cash, nestedHoldings))

    ## Fill the isHeader value for cash title:
    nestedHoldings[nestedHoldings[,"Name"] == "Cash", "isHeader"] <- "TRUE"

    ## Fill the order value for cash title:
    nestedHoldings[nestedHoldings[,"Name"] == "Cash", "order"] <- "1"

    ## Fill the isFooter value for cash subtotal:
    nestedHoldings[nestedHoldings[,"Name"] == "Cash Subtotal", "isFooter"] <- "TRUE"

    ## Fill the order value for cash subtotal:
    nestedHoldings[nestedHoldings[,"Name"] == "Cash Subtotal", "order"] <- "1"

    ## Return:
    nestedHoldings
}


##' TODO
##'
##' @param holdings TODO.
##' @return TODO.
##' @export
getSubtotalledHoldings <- function(holdings) {

    ## Divide the holdings to list by order indices.
    orderIdx <- lapply(na.omit(unique(holdings[,"order"])), function(x) holdings[,"order"] == x)

    ## Replace NA's with FALSE:
    orderIdx <- lapply(orderIdx, function(x) which(ifelse(is.na(x), FALSE, x)))

    ## Name the list elements with order:
    names(orderIdx) <- na.omit(unique(holdings[,"order"]))

    ## Create index pairs representing the start and end of chunk:
    idxPairs <- lapply(orderIdx, function(x) cbind(x[seq(1, length(x), 2)],
                                                   x[seq(2, length(x), 2)]))

    ## Iterate over index pairs and run the getSummary for subtotals:
    allSubtotals <- lapply(idxPairs, function(x) do.call(rbind, apply(x, MARGIN=1, function(y) getSummary(holdings, y))))

    ## Iterate over index pairs and assign the values to correct subtotal row index:
    for (i in 1:length(idxPairs)){

        ## Get the footer row index:
        footerIdx <- idxPairs[[i]][,2]

        ## Get the subtotal values:
        vals <- allSubtotals[[i]]

        ## Assign to footer row index:
        holdings[footerIdx, colnames(vals)] <- vals
    }

    ## Done, return:
    holdings

}


##' TODO
##'
##' @param holdings TODO.
##' @param idxpair TODO
##' @return TODO.
##' @export
getSummary <- function(holdings, idxpair){
    ## Prepare the chunks:
    chunk <-  holdings[idxpair[1]:idxpair[2],]

    ## Get the holding summaries and return:
    data.frame("Value"=sum(as.numeric(na.omit(chunk[,"Value"]))),
               "Value (%)"=sum(as.numeric(na.omit(chunk[,"Value (%)"]))),
               "Exposure"=sum(as.numeric(na.omit(chunk[,"Exposure"]))),
               "Exp (%)"=sum(as.numeric(na.omit(chunk[,"Exp (%)"]))),
               "PnL (Unrl)"=sum(as.numeric(na.omit(chunk[, "PnL (Unrl)"]))),
               "PnL (%Inv)"=mean(as.numeric(na.omit(chunk[,"PnL (%Inv)"]))), check.names=FALSE)
}


##' Defines the alignment translation table.
##'
##' @return TODO.
##' @export
.lookupAlignment <- function() {
    list(r=Alignment(h="ALIGN_RIGHT"), c=Alignment(h="ALIGN_CENTER"), l=Alignment(h="ALIGN_LEFT"))
}
