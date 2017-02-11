##
## Copyright 2016 Brent Kaplan
##
## This file is part of beezdemand.
##
## beezdemand is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 2.
##
## beezdemand is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with beezdemand.  If not, see <http://www.gnu.org/licenses/gpl-2.0.html>.
##
## summary
## R script for plotting demand functions
##
## lseq derived from
## @emdbook = auxiliary functions and data sets for "Ecological Models and Data", (Copyright - 2016 - Ben Bolker - GPLv2+)
## link @ https://cran.r-project.org/web/packages/emdbook/index.html
## license @ https://cran.r-project.org/web/licenses/GPL-2
##
##
##


##' Creates a logarithmically spaced sequence.
##'
##' A vector of logarithmically spaced values primarily used for plotting. Adapted from library("emdbook").
##' @title Create Logarithmically Spaced Sequence
##' @param from Starting value. Default value is 0.0000000001.
##' @param to Ending value. Default value is 1000.
##' @param length.out Number of values in vector. Default value is 14.
##' @return Vector
##' @author library("emdbook")
##' @export
lseq <- function(from=.0000000001, to=1000, length.out=14) {
  exp(seq(log(from), log(to), length.out = length.out))
}

## Create small ticks
##' Creates minor ticks for use in plotting.
##'
##'
##' @title Create Minor Tick Sequence
##' @param maj Value from function lseq
##' @return Vector
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
minTicks <- function(maj) {
    minticks <- vector(length = (length(maj)-1) * 10)
    for (i in 1:length(maj)) {
        if (i == length(maj)) {
            return(minticks)
        }
        if (i == 1) {
            minticks <- seq(maj[i], maj[i + 1], length.out = 10)
        } else {
            minticks <- c(minticks, seq(maj[i], maj[i + 1], length.out = 10))
        }
    }
}

##' Creates annotation layer
##'
##' Inherit and extend layer for use in ggplot draw
##' @title annotation_logticks2
##' @param base base for drawing in scale
##' @param sides sides to draw, by default bottom and left
##' @param scaled true by default
##' @param short short tick settings
##' @param mid mid tick settings
##' @param long long tick settings
##' @param colour default to black colour
##' @param size size for labels
##' @param linetype default linetype
##' @param alpha default alpha level
##' @param data data to include
##' @param color colors to include
##' @return ggplot2 layer
##' @author Shawn Gilroy <shawn.gilroy@@temple.edu>
##' @export
annotation_logticks2 <- function(base = 10, sides = "bl", scaled = TRUE, short = unit(0.1, "cm"),
                                 mid = unit(0.2, "cm"), long = unit(0.3, "cm"), colour = "black",
                                 size = 0.5, linetype = 1, alpha = 1, data =data.frame(x = NA), color = NULL, ...) {
  if (!is.null(color)) {
    colour <- color
  }

  layer(
    data = data,
    mapping = NULL,
    stat = StatIdentity,
    geom = GeomLogticks,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      base = base,
      sides = sides,
      scaled = scaled,
      short = short,
      mid = mid,
      long = long,
      colour = colour,
      size = size,
      linetype = linetype,
      alpha = alpha,
      ...
    )
  )
}

##' Creates plots
##'
##' Creates and saves plots of individual demand curves
##' @title Plot Curves
##' @param adf Data frame (long form) of purchase task data.
##' @param dfrow A row of results from FitCurves
##' @param fit A nls model object from FitCurves
##' @param outdir Directory where plots are saved
##' @param fitfail Boolean whether there's a valid nls model object
##' @param tobquote Character string to be evaluated
##' @param vartext Character vector to match demand indices
##' @return Nothing
##' @author Brent Kaplan <bkaplan4@@ku.edu>, Shawn Gilroy <shawn.gilroy@@temple.edu>
##' @export
PlotCurves <- function(adf, dfrow, fit, outdir = "../plots/", fitfail, tobquote, vartext) {
    require(ggplot2)

    if (!fitfail) {
        tempnew <- data.frame(x = seq(min(adf$x[adf$x > 0]), max(adf$x), length.out = 1000), k = dfrow[["K"]])
        if (dfrow[["Equation"]] == "hs") {
            tempnew$y <- 10^(predict(fit, newdata = tempnew))
        } else if (dfrow[["Equation"]] == "koff") {
            tempnew$y <- predict(fit, newdata = tempnew)
        }

        tempnew$expend <- tempnew$x * tempnew$y

        png(file = paste0(outdir, "Participant-", dfrow[["Participant"]], ".png"))

        pointFrame <- data.frame(X=adf$x, Y=adf$y)

        if (0 %in% pointFrame$X) {
          # If the points contain a qFree (x = 0), use faceted grid arrangement
          #

          pointFrame$mask <- 1
          tempnew$mask <- 1

          pointFrame[pointFrame$X == 0,]$mask <- 0
          pointFrame[pointFrame$X == 0,]$X <- 0.00001

          logChart <- ggplot(pointFrame,aes(x=X,y=Y)) +
            geom_point(size=2, shape=21, show.legend=T) +
            facet_grid(.~mask, scales="free_x", space="free") +
            geom_line(data=tempnew, aes(x=x, y=y)) +
            scale_x_log10(breaks=c(0.00001,  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                          labels=c("QFree",  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
            scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                          labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
            coord_cartesian(ylim=c(0.1, max(pointFrame$Y))) +
            ggtitle(paste("Participant", dfrow[["Participant"]], sep = "-")) +
            theme(strip.background = element_blank(),
                  strip.text = element_blank(),
                  panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_rect(colour = "white",
                                              fill=FALSE,
                                              size=0),
                  axis.line.x = element_line(colour = "black"),
                  axis.line.y = element_line(colour = "black"),
                  axis.text.x=element_text(colour="black"),
                  axis.text.y=element_text(colour="black"),
                  text = element_text(size=16),
                  plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.key = element_rect(fill = "transparent", colour = "transparent")) +
            annotation_logticks2(sides="l", data = data.frame(X= NA, mask = 0)) +
            annotation_logticks2(sides="b", data = data.frame(X= NA, mask = 1)) +
            labs(x = "Price", y = "Reported Consumption")

        } else {
          logChart <- ggplot(tempnew,aes(x=tempnew$x,y=tempnew$y)) +
            geom_line(show.legend=F) +
            geom_point(data=pointFrame, aes(x=pointFrame$X, y=pointFrame$Y), size=2, shape=21, show.legend=T) +
            scale_x_log10(breaks=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                          labels=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
            scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                          labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
            coord_cartesian(ylim=c(0.1, max(pointFrame$Y))) +
            ggtitle(paste("Participant", dfrow[["Participant"]], sep = "-")) +
            annotation_logticks() +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_rect(colour = "white",
                                              fill=FALSE,
                                              size=0),
                  axis.line.x = element_line(colour = "black"),
                  axis.line.y = element_line(colour = "black"),
                  axis.text.x=element_text(colour="black"),
                  axis.text.y=element_text(colour="black"),
                  text = element_text(size=16),
                  plot.title = element_text(hjust = 0.5),
                  legend.title=element_blank(),
                  legend.position = "bottom",
                  legend.key = element_rect(fill = "transparent", colour = "transparent")) +
            labs(x = "Price", y = "Reported Consumption")

        }

        print(logChart)
        graphics.off()

    } else {
      # If the points contain a qFree (x = 0), use faceted grid arrangement
      #

      png(file = paste0(outdir, "Participant-", dfrow[["Participant"]], ".png"))

      pointFrame <- data.frame(X=adf$x, Y=adf$y)

      if (0 %in% pointFrame$X) {
          pointFrame$mask <- 1

          pointFrame[pointFrame$X == 0,]$mask <- 0
          pointFrame[pointFrame$X == 0,]$X <- 0.00001

          logChart <- ggplot(pointFrame,aes(x=X,y=Y)) +
            geom_point(size=2, shape=21, show.legend=T) +
            facet_grid(.~mask, scales="free_x", space="free") +
            scale_x_log10(breaks=c(0.00001,  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                          labels=c("QFree",  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
            scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                          labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
            coord_cartesian(ylim=c(0.1, max(pointFrame$Y))) +
            ggtitle(paste("Participant", dfrow[["Participant"]], sep = "-")) +
            theme(strip.background = element_blank(),
                  strip.text = element_blank(),
                  panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_rect(colour = "white",
                                              fill=FALSE,
                                              size=0),
                  axis.line.x = element_line(colour = "black"),
                  axis.line.y = element_line(colour = "black"),
                  axis.text.x=element_text(colour="black"),
                  axis.text.y=element_text(colour="black"),
                  text = element_text(size=16),
                  plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.key = element_rect(fill = "transparent", colour = "transparent")) +
            annotation_logticks2(sides="l", data = data.frame(X= NA, mask = 0)) +
            annotation_logticks2(sides="b", data = data.frame(X= NA, mask = 1)) +
            labs(x = "Price", y = "Reported Consumption")

        } else {
            logChart <- ggplot(data=pointFrame, aes(x=pointFrame$X, y=pointFrame$Y)) +
              geom_point(size=2, shape=21, show.legend=T) +
              scale_x_log10(breaks=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                            labels=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
              scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                            labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
              coord_cartesian(ylim=c(0.1, max(pointFrame$Y))) +
              ggtitle(paste("Participant", dfrow[["Participant"]], sep = "-")) +
              annotation_logticks() +
              theme(panel.background = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "white",
                                                fill=FALSE,
                                                size=0),
                    axis.line.x = element_line(colour = "black"),
                    axis.line.y = element_line(colour = "black"),
                    axis.text.x=element_text(colour="black"),
                    axis.text.y=element_text(colour="black"),
                    text = element_text(size=16),
                    plot.title = element_text(hjust = 0.5),
                    legend.title=element_blank(),
                    legend.position = "bottom",
                    legend.key = element_rect(fill = "transparent", colour = "transparent")) +
              labs(x = "Price", y = "Reported Consumption")

        }

        print(logChart)

        graphics.off()
    }
}
