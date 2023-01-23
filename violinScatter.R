
library(ggplot2)

# violinScatter <- function(nominalPlotWidth = 900,
#                          pointSize = 3.3,
#                          initals = FALSE,
#                          filename = "violinPlot.png") {
#
#  ## ADD WRAPPER FUNCTION AND TEST
#}

getPlotDimensions <- function(nominalPlotWidth, pointSize) {
  # These plot dimensions in pixels are based on the .png files produced by
  # ggplot2::ggsave() ver 3.3.6 using RStudio for Windows.
  # No promises are made for .png files otherwise produced.

  plotDimensions <- list()

  plotDimensions$effectivePlotWidthPx <- floor(nominalPlotWidth*0.9083-58.201)
  plotDimensions$plotHeightPx         <- floor(nominalPlotWidth * (1+5^(1/2))/2.5)
  plotDimensions$pointDiameterPx      <- floor(pointSize * 8.9383 + 2.7359)
  plotDimensions$pointXSpacingPx      <- floor(plotDimensions$pointDiameterPx * 0.8125)
  plotDimensions$alternateRowOffsetPx <- plotDimensions$pointDiameterPx - plotDimensions$pointXSpacingPx
  plotDimensions$maxYBins             <- floor(plotDimensions$plotHeightPx * 0.043 * 3.3 / pointSize)

  plotDimensions
}

getXValues <- function(plotDimensions) {
  midPointRowOne <- floor(plotDimensions$effectivePlotWidthPx / 2) -
    plotDimensions$alternateRowOffsetPx
  midPointRowTwo <- floor(plotDimensions$effectivePlotWidthPx / 2) +
    plotDimensions$alternateRowOffsetPx
  maxPoints <- floor(midPointRowOne / plotDimensions$pointXSpacingPx)
  leftPointsRowOne <- rep(NA, maxPoints)
  rightPointsRowOne <- rep(NA, maxPoints)
  leftPointsRowTwo <- rep(NA, maxPoints)
  rightPointsRowTwo <- rep(NA, maxPoints)
  allPointsRowOne <- rep(NA, maxPoints * 2) + 1
  allPointsRowOne[1] <- midPointRowOne
  allPointsRowTwo <- rep(NA, maxPoints * 2) + 1
  allPointsRowTwo[1] <- midPointRowTwo

  for(i in 1:maxPoints) {
    leftPointsRowOne[i] <- midPointRowOne + i * plotDimensions$pointXSpacingPx
    rightPointsRowOne[i] <- midPointRowOne - i * plotDimensions$pointXSpacingPx
    leftPointsRowTwo[i] <- midPointRowTwo - i * plotDimensions$pointXSpacingPx
    rightPointsRowTwo[i] <- midPointRowTwo + i * plotDimensions$pointXSpacingPx
  }

  for(i in 1:maxPoints) {
    allPointsRowOne[i * 2] <- leftPointsRowOne[i]
    allPointsRowOne[i * 2 + 1] <- rightPointsRowOne[i]
    allPointsRowTwo[i * 2] <- leftPointsRowTwo[i]
    allPointsRowTwo[i * 2 + 1] <- rightPointsRowTwo[i]
  }

  list(allPointsRowOne, allPointsRowTwo)
}

getLabels <- function(n) {
  plotLabels <- rep(NA, n)
  for (i in 1:length(plotLabels)) {
    plotLabels[i] <- paste(sample(LETTERS, size = 2), collapse = "")
  }

  plotLabels
}

binYValues <- function(yValues, plotDimensions) {
  bins <- seq(from = 0, to = 100, by = 100/plotDimensions$maxYBins)
  binnedYValues <- rep(NA, length(yValues))
  for(i in 1:length(yValues)) {
    x <- which.min(abs(bins-yValues[i]))
    binnedYValues[i] <- bins[x]
  }
  binnedYValues
}

assignXValues <- function(yVals, xValues) {
  uniqueYs <- sort(unique(yVals))
  output <- as.data.frame(yVals)
  output$xVals <- NA

  for(i in 1:length(uniqueYs)) {
    if(i %% 2 == 0) {
      output[output$yVals == uniqueYs[i], "xVals"] <- xValues[[2]][1:length(output[output$yVals == uniqueYs[i], "xVals"])]
    } else {
      output[output$yVals == uniqueYs[i], "xVals"] <- xValues[[1]][1:length(output[output$yVals == uniqueYs[i], "xVals"])]
    }
  }
  return(output)
}

# Wrapper for running actual data below

sampleData <- read.csv("sampleData.csv")

yVals <- sampleData$average
pointSize <- 3.3
nominalPlotWidth <- 900

plotDimensions <- getPlotDimensions(nominalPlotWidth, pointSize)

yValsBinned <- binYValues(yVals, plotDimensions)

xValues <- getXValues(plotDimensions)

finalOut <- assignXValues(yValsBinned, xValues)

finalOut$labels <- getLabels(dim(finalOut)[[1]])

testplot <- ggplot(finalOut, aes(xVals, yVals, label = labels)) +
  ylim(0, 100) +
  xlim(1, 759) +
  geom_point(shape = 21, size = pointSize, color = "white", fill = "#606FAF", stroke = 0.2, position = position_jitter(width = 0.2, height = 0.2, seed = 42)) +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 5, color = "#606FAF"),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(color = "#606FAF", size = 0.1),
        panel.grid.major.x = element_blank()) +
#geom_vline(xintercept = 379, size = 0.1) +
#geom_vline(xintercept = 1, size = 0.1) +
#geom_vline(xintercept = 759, size = 0.1) +
geom_text(color = "white", fontface = "bold", size = 0.9, position = position_jitter(width = 0.2, height = 0.2, seed = 42))




ggsave("delme7.png",
       testplot,
       width = 900,
       height = plotDimensions$plotHeightPx,
       units = "px")



