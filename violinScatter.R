


# Note - Y AXIS text size is not variable depending on point size.
# will have to add this to plot dimensions

#Address error with geom_text when labels = NULL

violinScatter <- function(dataFrame,
                          dataColumn,
                          labelColumn = NULL,
                          nominalPlotWidth = 900,
                          pointSize = 3.3,
                          filename = "violinPlot.png") {
  require(ggplot2)

  if(!dataColumn %in% names(dataFrame)) {
    stop("dataColumn not found in dataFrame")
  } else if (!is.numeric(dataFrame[, dataColumn])) {
    stop("dataColumn is not of mode numeric")
  } else {
    yVals <- dataFrame[, dataColumn]
  }

  if(is.null(labelColumn)) {
    labels <- rep(NA, length(yVals))
  } else if (!labelColumn %in% names(dataFrame)) {
    stop("labelColumn not found in dataFrame")
  } else if (!is.character(dataFrame[, labelColumn])) {
    stop("labelColumn is not of mode character")
  } else {
    labels <- dataFrame[, labelColumn]
  }

  plotDimensions <- getPlotDimensions(nominalPlotWidth, pointSize)
  yValsBinned <- binYValues(yVals, plotDimensions)
  xValues <- getXValues(plotDimensions)
  outputData <- assignXValues(yValsBinned, xValues)
  outputData$labels <- labels

  plot <- ggplot(outputData, aes(xVals, yVals, label = labels)) +
    ylim(0, 100) +
    xlim(1, plotDimensions$effectivePlotWidthPx) +
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
    geom_text(color = "white", fontface = "bold", size = plotDimensions$fontSize, position = position_jitter(width = 0.2, height = 0.2, seed = 42))

  ggsave(filename,
         plot,
         width = nominalPlotWidth,
         height = plotDimensions$plotHeightPx,
         units = "px")

  message("Plot saved to ", filename)
}

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
  plotDimensions$fontSize             <- pointSize/3.3 * 0.9

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
      whichXvalues <- 1
    } else {
      whichXvalues <- 2
    }

    numXValuesNeeded <- length(output[output$yVals == uniqueYs[i], "xVals"])
    xValuesSubset <- xValues[[whichXvalues]][1:numXValuesNeeded]
    output[output$yVals == uniqueYs[i], "xVals"] <- xValuesSubset
  }

  randomRows <- sample(nrow(output))
  output <- output[randomRows, ]
  return(output)
}

# testing

sampleY <- read.csv("sampleData.csv")
sampleY <- as.data.frame(sampleY)
sampleY$words <- rep("AA", 100)




