#function for multiple plots of all data
multi_plotFun <- function(aPlate, rowNum, tabRes) {
  # Set up some plotting parameters
  plateData <- aPlate[, -1]
  absWells <- length(plateData[1, ])
  maxy <- max(plateData, na.rm = TRUE)*1.1 # To set the maximum of the whole plate
  par(mfrow = c(rowNum, (absWells / rowNum))) # Organisation of multiple plots
  par(mar = c(0.2, 0.2, 0.2, 0.2)) # Dimensions for figure
  # Generate the plots from plotmake_fun
  lapply(seq_along(plateData), function(k) {
    plotmake_fun(aPlate, tabRes, k, axx="n", axy="n", maxy)
  }) 
}


