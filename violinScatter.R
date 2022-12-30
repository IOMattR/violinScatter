
zavi <- read.csv("zavi.csv")

zavi$const <- 1

ggplot(zavi, aes(const, Signal, label = Name)) +
  ylim(0, 100) +
  xlim (0.5, 1.5) +
  geom_sina(maxwidth = 0.5,
            jitter_y = FALSE,
            seed = 456,
            shape = 21, size = 12, color = "white", fill = "#606FAF", stroke = 1.05) +
  theme(panel.background = element_rect(fill = "white", color = "white"))





# for each y position / x position have a set of "set" plot positions. that you assign to the x vars.


testdata1 <- matrix(data = c(rep(40, length(makeXValues(3.3, 900)[[1]])), makeXValues(3.3, 900)[[1]]), ncol = 2, nrow = length(makeXValues(3.3, 900)[[1]]))

testdata2 <- matrix(data = c(rep(42, length(makeXValues(3.3, 900)[[2]])), makeXValues(3.3, 900)[[2]]), ncol = 2, nrow = length(makeXValues(3.3, 900)[[2]]))

testdata1 <- as.data.frame(testdata1)
testdata2 <- as.data.frame(testdata2)

testdata <- rbind(testdata1, testdata2)

testdata$rand <- runif(dim(testdata)[1])

testdata <- testdata[order(testdata$rand), ]




testdata <- matrix(data = c(10,1,
                            20,759,
                            30,340,
                            40,315,
                            50,331,
                            60,356,
                            70,381), byrow = TRUE, ncol = 2, nrow = 7)

testdata <- as.data.frame(testdata)
colnames(testdata) <- c("signal", "const")

testdata[1,2] <- -2.3
testdata[2,2] <- 0
testdata[3,2] <- 2.3
testdata$name <- makeLabels(dim(testdata)[1])


# Easy solution would be to specify position_jitter in both geom_text and geom_jitter with the same seed.


testplot <- ggplot(testdata, aes(const, signal, label = name)) +
  ylim(0, 100) +
  xlim(1, 759) +
  geom_point(shape = 21, size = 3.3, color = "white", fill = "#606FAF", stroke = 0.2, position = position_jitter(width = 0.2, height = 0.2, seed = 42)) +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 5, color = "#606FAF"),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(color = "#606FAF", size = 0.1),
        panel.grid.major.x = element_blank()) +
  geom_text(color = "white", fontface = "bold", size = 1.1, position = position_jitter(width = 0.2, height = 0.2, seed = 42))












  effectivePlotWidth <- function(nominalPlotWidth) {
    x <- floor(nominalPlotWidth*0.9083-58.201)
    x
  }

  pointPixelWidth <- function(pointSize) {
    x <- floor(pointSize*8.9383 + 2.7359)
    x
  }



  makeXValues <- function(pointSize, nominalPlotWidth) {
    plotWidth <- effectivePlotWidth(nominalPlotWidth)
    xPitch <- floor(pointPixelWidth(pointSize)*0.8125)
    pointOverlap <- pointPixelWidth(pointSize)-xPitch
    midPointRowOne <- floor(plotWidth/2)-pointOverlap
    maxPoints <- floor(midPointRowOne/xPitch)

    leftPoints <- rep(NA, maxPoints)
    rightPoints <- rep(NA, maxPoints)

    for(i in 1:maxPoints) {
      leftPoints[i] <- midPointRowOne - i * xPitch
      rightPoints[i] <- midPointRowOne + i *xPitch
    }

    allPoints <- rep(NA, maxPoints * 2)+1
    for(i in 1:maxPoints) {
      allPoints[i*2] <- leftPoints[i]
      allPoints[i*2+1] <- rightPoints[i]
    }

    allPoints[1] <- midPointRowOne
    allPoints <- sort(allPoints)

    allPointsRowTwo <- allPoints+pointOverlap*2

    list(allPoints, allPointsRowTwo)
}



makeLabels <- function(n) {

  plotLabels <- rep(NA, n)
  for (i in 1:length(plotLabels)) {
    plotLabels[i] <- paste(sample(LETTERS, size = 2), collapse = "")
  }

  plotLabels
}



# get y values
# bin them into even values
# assign them an X value

plotHeight <- function(nominalPlotWidth) {
  plotHeight <- floor(nominalPlotWidth*(1+5^(1/2))/2.5)
  plotHeight
}

## This fun needs to have pointSize as an input!
getYBins <- function(nominalPlotWidth) {
  height <- plotHeight(nominalPlotWidth)
  maxBins <- floor(height*0.043)
  bins <- seq(from = 0, to = 100, by = ceiling(100/maxBins))
  bins
}


binYValues <- function(yValues, bins) {

  binnedYValues <- rep(NA, length(yValues))
  for(i in 1:length(yValues)) {
    x <- which.min(abs(bins-yValues[i]))
    binnedYValues[i] <- bins[x]
  }
  binnedYValues
}


zavi_binned <- binYValues(zavi$Signal, getYBins(900))


getXsubset <- function(xValues, points, side = "left") {
  if(length(xValues) %% 2 == 0) stop("Even number of possible X Values")
  if(points > length(xValues)) stop("More points than available X Values")
  if(points <= 0) stop("Points must be greater than 0")
  midpoint <- which(xValues == median(xValues))

  if(points %% 2 == 0) {
     if(side == "left") {
        leftSubset <- floor(points / 2)
        rightSubset <- floor(points / 2) - 1
      } else {
        leftSubset <- floor(points / 2) - 1
        rightSubset <- floor(points / 2)
      }
  } else {
    leftSubset <- floor(points / 2)
    rightSubset <- floor(points / 2)
  }
  startingX <- midpoint - leftSubset
  endingX <- midpoint + rightSubset

  returnVals <- xValues[startingX:endingX]

  if(length(returnVals) == 1) {
    return(returnVals)
  } else {
    return(sample((xValues[startingX:endingX]), size = length(xValues[startingX:endingX])))
  }
}






assignXValues <- function(yVals, xVals) {
  uniqueYs <- sort(unique(yVals))
  output <- as.data.frame(yVals)
  output$xVals <- NA

  for(i in 1:length(uniqueYs)) {
    if(i %% 2 == 0) {
       output[output$yVals == uniqueYs[i], "xVals"] <- getXsubset(xVals[[2]],
                                                                  points = length(output[output$yVals == uniqueYs[i], "xVals"]),
                                                                  side = "left")
    } else {
      output[output$yVals == uniqueYs[i], "xVals"] <- getXsubset(xVals[[1]],
                                                                 points = length(output[output$yVals == uniqueYs[i], "xVals"]),
                                                                 side = "right")
    }
  }
return(output)
}


windsorize <- function(x, minVal = 0, maxVal = 100) {
  x[x < minVal] <- minVal
  x[x > maxVal] <- maxVal
  x
}


x <- zavi$Signal

x <- rnorm(500, 50, 19)
x <- windsorize(x)
x <- floor(x)

#Input data needed

talentSignal <- zavi$Signal
candidateInitals <- makeLabels(length(x))
pointSize <- 3.1
width <- 900

#Get the Y bins that the Y values will be mapped to

availableYValues <- getYBins(width)

#Assign the talentSignal to the Y bins

talentSignalBinned <- binYValues(talentSignal, availableYValues)

availableXValues <- makeXValues(pointSize = pointSize, nominalPlotWidth = width)

finalOut <- assignXValues(talentSignalBinned, availableXValues)

finalOut$labels <- makeLabels(dim(finalOut)[1])
finalOut$labels <- rep("", dim(finalOut)[1])


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
 # geom_vline(xintercept = 379)
geom_text(color = "white", fontface = "bold", size = 0.9, position = position_jitter(width = 0.2, height = 0.2, seed = 42))






outputWidth <- width
outputHeight <- outputWidth*(1+5^(1/2))/2.5

ggsave("testy_plot_rescale_rand9.png",
       testplot,
       width = outputWidth,
       height = outputHeight,
       units = "px")


# as the pointSize goes down, more y jitter will normally be needed to avoid banding
# although some banding will be needed
######!!!! AVAILABLE Y BINS NEEDS TO DEPEND ON POINT SIZE !!!!!

