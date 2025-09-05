multi_plotFun <- function(PLATE, ROWNUM, TABRES) {
  # Set up some plotting parameters
  TabRes <- TABRES
  RowNum <- ROWNUM

  plateData <- PLATE[, -1]
  absWells <- length(plateData[1, ])
  par(mfrow = c(RowNum, (absWells / RowNum))) # Organisation of multiple plots
  par(mar = c(0.2, 0.2, 0.2, 0.2)) # Dimensions for figure

  # Generate the plots from plotmake_fun
  lapply(seq_along(plateData), function(k) {
    # for(k in seq_along(plateData)){ #This is an alternative loop

    plotmake_fun(PLATE, TabRes, k, axx="n", axy="n")
  }) # remove this ')' with alternative loop
}


