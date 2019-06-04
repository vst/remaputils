##' Provides bar plot.
##'
##' This is the description
##'
##' @param df A data frame with columns 'value' and 'date.
##' @param title The title of the plot.
##' @param colors The color(s).
##' @return A bar plot.
##' @export
barPlot <- function(df, title, colors="darkslategray3") {

    ## If no date, return:
    if (is.null(df)) {
        return(barplot(1,
                       labels="NODATA",
                       border="gray",
                       cex=1,
                       main=colnames(df)[1],
                       cex.main=1.5))
    }

    ## Get the dates:
    date <- as.Date(df[, "date"])

    ## Get the values:
    values <- as.numeric(df[, "values"])

    ## Length out:
    lout <- 10

    ## X-Axis Index:
    axis1Index <- seq(0, length(date), length.out=lout)

    ## X-Axis Labels:
    axis1Labls <- format(as.Date(date[seq(1, length(date), length.out=lout)]), "%b %d, %y")

    ## Y-Axis Index:
    axis2Index <- seq(min(values), max(values), length.out=lout)

    ## X-Axis Labels:
    axis2Labls <- percentify(values[order(values)][seq(1, length(values), length.out=lout)])

    ## Now, run the plot:
    par(mai = c(1.25, 1, 0.75, 0.75))
    barplot(round(values, 4),
            "width"=0.9,
            "main"=title,
            "yaxt"="n", "xaxt"="n", "ylab"="", "xlab"="",
            "col"=colors,
            "border"=NA,
            "cex.main"=2,
            "cex.axis"=1.5)

    ## Add the custom x-axis:
    axis(1, cex.axis=1.3, at=axis1Index, labels=axis1Labls, las=2)

    ## Add the cusotm y-axis:
    axis(2, cex.axis=1.3, at=axis2Index, labels=axis2Labls, las=2)

}


##' Provides a stacked bar chart.
##'
##' This is the description
##'
##' @param df A data frame. df[, 1] == label, df[,2] == value
##' @param colors The color palette to be used.
##' @return A stacked bar chart.
##' @export
stackedBarChart <- function(df, colors=RColorBrewer::brewer.pal(9, "GnBu")[3:9]) {

    ## If null, return empty pie:
    if (is.null(df)) {
        return(barplot(1,
                       labels="NODATA",
                       col=terrain.colors(1, alpha = 0.8),
                       border="gray",
                       cex=1,
                       main=colnames(df)[1],
                       cex.main=1.5))
    }

    ## If NA value, override:
    df[, 1] <- ifelse(is.na(df[, 1]), "Notavailable", df[,1])
    df[, 2] <- ifelse(is.na(df[, 2]), 0, df[,2])

    ## Aggregate:
    aggs <- aggregate(df[, 2], list(df[, 1]), sum)

    ## Get the slices:
    slices <- aggs[,2]

    ## Get the labels
    if (mean(nchar(aggs[,1])) == 3) {
        lbls <- aggs[,1]
    } else {
        lbls <- capitalise(ellipsify(aggs[,1], 15))
    }

    ## Compute the percentagaes:
    pct <- slices / sum(slices)

    ## Group small values into "Others"
    if (sum(abs(pct) < 0.01) > 1) {
        othersval <- sum(slices[abs(pct) < 0.01])
        slices <- c(slices[!(abs(pct) < 0.01)], othersval)
        lbls <- c(lbls[!(abs(pct) < 0.01)], "Others")
    }

    ## Compute the percentages:
    pct <- as.data.frame(round(slices / sum(slices) * 100))

    ## Assign the row names:
    rownames(pct) <- lbls

    ## Plot the stacked bar chart and the legend:
    par(mai = c(0.4, 0.4, 0.4, 0.4))
    barplot(as.numeric(pct[,1]),
            col=colors[1:NROW(pct)],
            border="white",
            xlab="",
            ylab=NULL,
            space=0.04,
            xaxt="n")
    legend("topright",
           rownames(pct),
           fill=colors[1:NROW(pct)],
           bg=adjustcolor("white", alpha=0.60),
           cex=1.25,
           pt.cex=1,
           box.lwd=0.01,
           box.lty=0,
           text.col=adjustcolor("black", alpha=0.8))
}


##' Provides an line time series plot.
##'
##' This is the description
##'
##' @param df A data frame with 'value' and 'date' columns.
##' @param smooth The smoothing parameter.
##' @param limitFactor The factor with which to expand the limits.
##' @param title The title of the plot.
##' @param ylab The y-axis name.
##' @param col The color for the polygon.
##' @return An line time series plot.
##' @export
timeSeriesPlot <- function(df, smooth=0.3, limitFactor=0.05, title, ylab="", col="darkslategray3") {

    ## Transform and arrange data frame:
    trans <- timeSeriesTransform(df, smooth, limitFactor=limitFactor)

    ## Get the date:
    date <- trans[["date"]]

    ## Get the value:
    value <- trans[["value"]]

    ## Length out:
    lout <- 10

    ## X-Axis Index:
    axis1Index <- seq(1, length(date), length.out=lout)

    ## X-Axis Labels:
    axis1Labls <- format(as.Date(date[seq(1, length(date), length.out=lout)]), "%b %d, %y")

    ## Y-Axis Index:
    axis2Index <- seq(trans[["limits"]]$lower, trans[["limits"]]$upper, length.out=lout)

    ## X-Axis Labels:
    axis2Labls <- round(axis2Index * 100, 2)

    par(mai = c(1.25, 1, 0.75, 0.75))
    plot(value, main=title, cex.main=2, cex.axis=1.5, lty=4, type="l", lwd=3,
         ylim=c(trans[["limits"]][["lower"]], trans[["limits"]][["upper"]]),
         yaxt="n", xaxt="n", ylab="", xlab="",
         col=col, bty="n")

    ## Add the custom x-axis:
    axis(1, cex.axis=1.3, at=axis1Index, labels=axis1Labls, las=2)

    ## Add the cusotm y-axis:
    axis(2, cex.axis=1.3, at=axis2Index, labels=axis2Labls, las=2)

    ## Place the Y-Label:
    mtext("Index", cex=1.5, font=2, side=2, at=par('usr')[4]*1.0075, las=2)

}


##' Provides an area time series plot.
##'
##' This is the description
##'
##' @param df A data frame with 'value' and 'date' columns.
##' @param smooth The smoothing parameter.
##' @param limitFactor The factor with which to expand the limits.
##' @param title The title of the plot.
##' @param ylab The y-axis name.
##' @param col The color for the polygon.
##' @param ylabFun The transformation function to apply to the ylab values.
##' @return An area time series plot.
##' @export
areaTimeSeriesPlot <- function(df, smooth=0.3, limitFactor=0.1, title, ylab="", col="darkslategray3", ylabFun=NULL) {

    ## Transform and arrange data frame:
    trans <- timeSeriesTransform(df, smooth, limitFactor)

    ## Get the date:
    date <- trans[["date"]]

    ## Get the value:
    value <- trans[["value"]]

    ## Length out:
    lout <- 10

    ## X-Values for Polygon:
    xPolygon <- do.call(c, lapply(1:length(date), function(x) rep(x, 2)))

    ## Y-Values for Polygon:
    yPolygon <- c(c(trans[["limits"]]$lower, value[1]), do.call(c, lapply(value[-1], function(x) rep(x, 2))))
    yPolygon[length(yPolygon)] <- trans[["limits"]]$lower

    ## X-Axis Index:
    axis1Index <- seq(1, length(date), length.out=lout)

    ## X-Axis Labels:
    axis1Labls <- format(as.Date(date[seq(1, length(date), length.out=lout)]), "%b %d, %y")

    ## Y-Axis Index:
    axis2Index <- seq(trans[["limits"]]$lower, trans[["limits"]]$upper, length.out=lout)

    ## X-Axis Labels:
    axis2Labls <- axis2Index

    ## If ylabTransform no null, apply:
    if (!is.null(ylabFun)) {
        axis2Labls <- ylabFun(axis2Labls)
    }

    ## Now, run the plot:
    par(mai = c(1.25, 1.1, 0.75, 0.75))
    plot(value, main=title, cex.main=2, cex.axis=1.5,
         ylim=c(trans[["limits"]][["lower"]], trans[["limits"]][["upper"]]),
         yaxt="n", xaxt="n", ylab="", xlab="",
         col="white", bty="n")

    ## Add the polygon:
    polygon(xPolygon, yPolygon, col=col, border=FALSE)

    ## Add the custom x-axis:
    axis(1, cex.axis=1.3, at=axis1Index, labels=axis1Labls, las=2)

    ## Add the cusotm y-axis:
    axis(2, cex.axis=1.3, at=axis2Index, labels=axis2Labls, las=2)

    ## Place the Y-Label:
    mtext(ylab, cex=1.5, font=2, side=2, at=par('usr')[4]*1.009, las=2)
}
