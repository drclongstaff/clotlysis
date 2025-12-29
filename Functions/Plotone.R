# This function plots data from a selected well
one_plotFun <- function(aPlate, wellNum, tabRes) {
  # Set plotting parameters
  k<-which(colnames(aPlate)==wellNum)[1]-1
  par(mar = c(4, 4, 1, 1)) # dimensions for figure
  maxy <- max(aPlate[[k+1]]) #in this case each plot keeps its max
  # Only one plot in this case so no looping needed
  plotmake_fun(aPlate, tabRes, k, axx="s", axy="s", maxy)
}



