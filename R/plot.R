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
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
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
##' @param ... additional arguments
##' @return ggplot2 layer
##' @author Shawn Gilroy <shawn.gilroy@@temple.edu>
##' @importFrom ggplot2 layer GeomLogticks StatIdentity PositionIdentity unit
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
##' @param dat FitCurves object with 4 elements (dfres, newdats, adfs, fits)
##' @param outdir Directory where plots are saved
##' @param device Type of file. Default is "png". Can be "pdf"
##' @param ending Optional. Can specify to only plot through a certain number of datasets
##' @param ask Can view plots one by one. If TRUE, plots will not save
##' @param ... Pass arguments to PlotCurve
##' @return Nothing
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>, Shawn Gilroy <shawn.gilroy@@temple.edu>
##' @importFrom ggplot2 ggsave
##' @export
PlotCurves <- function(dat, outdir = "../plots/", device = "png", ending = NULL, ask = F, ...) {
  
  if (!all(c("dfres", "newdats", "adfs") %in% names(dat))) {
    stop("Object should be from FitCurves")
  }
  
  if (!dir.exists(outdir)){
    dir.create(outdir)
  }
  
  if (is.null(ending)) {
    ending <- length(dat$fits)
  }

  par(ask = ask)
  
  for (i in 1:ending) {
    ggp <- PlotCurve(dat$adfs[[i]], dat$dfres[i, ], dat$newdats[[i]], ...)
    if (!class(ggp)[[1]] == "character") {
      if (ask) {
        print(ggp)
      } else {
        if (device = "png") {
          png(file = paste0(outdir, "Participant-", dat$dfres[i, "ID"], ".png"))
          print(ggp)
          graphics.off()
        } else if (device = "pdf") {
          pdf(file = paste0(outdir, "Participant-", dat$dfres[i, "ID"], ".pdf"))
          print(ggp)
          graphics.off()
        }
      }
    } else {
      next()
    }
  }
}

##' Creates a single plot object
##'
##' Creates individual demand curves
##' @title Plot Curve
##' @param adf Data frame (long form) of purchase task data.
##' @param dfrow A row of results from FitCurves
##' @param newdats A newdat dataframe from FitCurves
##' @param yscale Scaling of y axis. Default is "log". Can also take "linear"
##' @return ggplot2 graphical object
##' @author Shawn Gilroy <shawn.gilroy@@temple.edu>
##' @importFrom ggplot2 geom_point geom_line geom_segment coord_cartesian ggtitle aes annotation_logticks element_blank element_line element_rect element_text expand_limits guide_legend guides labs scale_x_log10 scale_y_log10 theme theme_bw xlab ylab
##' @export
PlotCurve <- function(adf, dfrow, newdats, yscale = "log") {
  if (!any(adf$y > 0)) {
   return(print("Warning: No positive consumption values!"))
  }
  
  if (!all(is.na(newdats$y))) {
    segmentFrame <- data.frame(x1 = c(0),
                               x2 = c(0),
                               y1 = c(0),
                               y2 = c(0))

    segmentFrame[1, "x1"] <- dfrow[["Pmaxd"]]
    segmentFrame[1, "x2"] <- dfrow[["Pmaxd"]]
    segmentFrame[1, "y1"] <- 0

    lowPrice <- 0.001

    # Lengthen out the curve domain
    highPrice <- max(adf$x) * 2

   if (dfrow[["Equation"]] == "hs") {
      segmentFrame[1, "y2"] <- 10^((log(dfrow[["Q0d"]])/log(10)) + dfrow[["K"]] * (exp(-dfrow[["Alpha"]] * dfrow[["Q0d"]] * dfrow[["Pmaxd"]]) - 1))
    } else if (dfrow[["Equation"]] == "koff") {
      segmentFrame[1, "y2"] <- dfrow[["Q0d"]] * 10^(dfrow[["K"]] * (exp(-dfrow[["Alpha"]] * dfrow[["Q0d"]] * dfrow[["Pmaxd"]]) - 1))
    }
    tempnew <- newdats
    pointFrame <- data.frame(X=adf$x, Y=adf$y)

    if (0 %in% pointFrame$X) {
      # Split axes are warranted here
      
      pointFrame$mask <- 1
      tempnew$mask <- 1

      pointFrame[pointFrame$X == 0,]$mask <- 0
      pointFrame[pointFrame$X == 0,]$X <- 0.0001

      segmentFrame$mask <- 1

      plt <- ggplot2::ggplot(pointFrame,aes(x=X,y=Y)) +
        ggplot2::geom_line(data=tempnew, aes(x=x, y=y)) +
        ggplot2::geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), show.legend = F, data = segmentFrame, linetype=2) +
        ggplot2::geom_point(size=3, shape=21, show.legend=T, colour = "black", fill = "white", alpha = .9, stroke = 1) +
        ggplot2::facet_grid(.~mask, scales="free_x", space="free_x") +
        ggplot2::scale_x_log10(breaks=c(0.0001,  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c("0.00",  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
        ggplot2::coord_cartesian(ylim=c(min(c(0.1, tempnew$y)), max(tempnew$y) * 1.1)) +
        ggplot2::ggtitle(paste("Participant", dfrow[["ID"]], sep = "-")) +
        beezdemand::theme_apa() +
        ggplot2::theme(strip.background = element_blank(),
              strip.text = element_blank(),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size=16)) +
        annotation_logticks2(sides="b", data = data.frame(X= NA, mask = 1)) +
        ggplot2::labs(x = "Price", y = "Reported Consumption")
    
      if (yscale == "log") {
        plt <- plt +
          ggplot2::scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
          annotation_logticks2(sides="l", data = data.frame(X= NA, mask = 0))
      } 
       
      plt <- plt +
        ggplot2::theme(axis.text.x = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                axis.text.y = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                axis.ticks.length = unit(-0.15, "cm"),
                axis.title.x = element_text(face = "bold", margin = unit(c(-0.1, 0, 0, 0), "cm")),
                axis.title.y = element_text(face = "bold", margin = unit(c(0, -0.1, 0, 0), "cm")))
    
    } else {
      # Regular representation

      plt <- ggplot2::ggplot(pointFrame,aes(x=X,y=Y)) +
        ggplot2::geom_line(data=tempnew, aes(x=x, y=y)) +
        ggplot2::geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), show.legend = F, data = segmentFrame, linetype=2) +
        ggplot2::geom_point(size=3, shape=21, show.legend=T, colour = "black", fill = "white", alpha = .9, stroke = 1) +
        ggplot2::scale_x_log10(breaks=c(0.00001,  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c(0.00001,  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
        ggplot2::coord_cartesian(ylim=c(min(c(0.1, tempnew$y)), max(tempnew$y) * 1.1)) +
        ggplot2::ggtitle(paste("Participant", dfrow[["ID"]], sep = "-")) +
        annotation_logticks(sides = "b") +
        beezdemand::theme_apa() +
        ggplot2::theme(strip.background = element_blank(),
              strip.text = element_blank(),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size=16)) +
        ggplot2::labs(x = "Price", y = "Reported Consumption")
      
      if (yscale == "log") {
        plt <- plt +
          ggplot2::scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                        labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
          ggplot2::annotation_logticks(sides="l")
      } 
      
      plt <- plt +
        ggplot2::theme(axis.text.x = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.text.y = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.ticks.length = unit(-0.15, "cm"),
              axis.title.x = element_text(face = "bold", margin = unit(c(-0.1, 0, 0, 0), "cm")),
              axis.title.y = element_text(face = "bold", margin = unit(c(0, -0.1, 0, 0), "cm")))
    
    }
    plt
    

  } else {
    # fitting failed in these instances

    pointFrame <- data.frame(X=adf$x, Y=adf$y)

    if (0 %in% pointFrame$X) {
      # Split axes are warranted
      pointFrame$mask <- 1

      pointFrame[pointFrame$X == 0,]$mask <- 0
      pointFrame[pointFrame$X == 0,]$X <- 0.0001

      plt <- ggplot2::ggplot(pointFrame,aes(x=X,y=Y)) +
        ggplot2::geom_point(size=3, shape=21, show.legend=T, colour = "black", fill = "white", alpha = .9, stroke = 1) +
        ggplot2::geom_blank(data = data.frame(X=0.001,
                                     Y=0.001,
                                     mask=1)) +
        ggplot2::geom_blank(data = data.frame(X=max(adf$x)*2,
                                     Y=max(adf$y),
                                     mask=1)) +
        ggplot2::facet_grid(.~mask, scales="free_x", space="free_x") +
        ggplot2::scale_x_log10(breaks=c(0.0001,  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c("0.00",  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
        ggplot2::coord_cartesian(ylim=c(0.1, max(pointFrame$Y) * 1.1)) +
        ggplot2::ggtitle(paste("Participant", dfrow[["ID"]], sep = "-")) +
        beezdemand::theme_apa() +
        ggplot2::theme(strip.background = element_blank(),
              strip.text = element_blank(),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size=16)) +
        annotation_logticks2(sides="b", data = data.frame(X= NA, mask = 1)) +
        ggplot2::labs(x = "Price", y = "Reported Consumption")
      
      if (yscale == "log") {
        plt <- plt +
          ggplot2::scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                        labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
          annotation_logticks2(sides="l", data = data.frame(X= NA, mask = 0))
      } 
      
      plt <- plt +
        ggplot2::theme(axis.text.x = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.text.y = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.ticks.length = unit(-0.15, "cm"),
              axis.title.x = element_text(face = "bold", margin = unit(c(-0.1, 0, 0, 0), "cm")),
              axis.title.y = element_text(face = "bold", margin = unit(c(0, -0.1, 0, 0), "cm")))
 
    } else {
      # Regular representation

      plt <- ggplot2::ggplot(pointFrame,aes(x=X,y=Y)) +
        ggplot2::geom_point(size=3, shape=21, show.legend=T, colour = "black", fill = "white", alpha = .9, stroke = 1) +
        ggplot2::geom_blank(data = data.frame(X=0.001,
                                     Y=0.001)) +
        ggplot2::geom_blank(data = data.frame(X=max(adf$x)*2,
                                     Y=max(adf$y))) +
        ggplot2::scale_x_log10(breaks=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
        ggplot2::coord_cartesian(ylim=c(0.1, max(pointFrame$Y) * 1.1)) +
        ggplot2::ggtitle(paste("Participant", dfrow[["ID"]], sep = "-")) +
        ggplot2::annotation_logticks(sides = "b") +
        beezdemand::theme_apa() +
        ggplot2::theme(strip.background = element_blank(),
              strip.text = element_blank(),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size=16)) +
        ggplot2::labs(x = "Price", y = "Reported Consumption")
      
      if (yscale == "log") {
        plt <- plt +
          ggplot2::scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                        labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
          ggplot2::annotation_logticks(sides="l")
      } 
      
      plt <- plt +
        ggplot2::theme(axis.text.x = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.text.y = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.ticks.length = unit(-0.15, "cm"),
              axis.title.x = element_text(face = "bold", margin = unit(c(-0.1, 0, 0, 0), "cm")),
              axis.title.y = element_text(face = "bold", margin = unit(c(0, -0.1, 0, 0), "cm")))

    }

    plt
  }
}


##' APA theme for ggplot
##'
##' Theme for ggplot graphics that closely align with APA formatting
##' @title APA Theme
##' @param plot.box Boolean for a box around the plot
##' @return ggplot theme
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @export
theme_apa <- function(plot.box = FALSE){
    helv <- "Helvetica"

    out <- theme(
        plot.title = element_text(family = helv, size = 14, face = "bold", colour = "black"),
        axis.title.x = element_text(family = helv, size = 14, colour = "black"),
        axis.title.y = element_text(family = helv, size = 14, angle = 90, colour = "black"),
        axis.text.x = element_text(family = helv, size = 11, colour = "black"),
        axis.text.y = element_text(family = helv, size = 11, colour = "black"),
        axis.ticks = element_line(colour="black"))

    if (plot.box) {
        out <- out + theme(panel.background = element_rect(fill = "white",
                colour = "black"), panel.border = element_rect(fill = NA,
                colour = "white"), axis.line = element_line())
    } else {
        out <- out + theme(panel.background = element_blank(),
                           panel.border = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           axis.line = element_line(colour = "black"))
    }
    out
}
