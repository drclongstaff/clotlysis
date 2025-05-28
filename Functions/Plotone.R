# This function plots data from all the wells
one_plotFun <- function(PLATE, WELLNUM, TABRES) {
  # Set plotting parameters
  TabRes <- TABRES

  #k <- WELLNUM
  k<-which(colnames(PLATE)==WELLNUM)[1]-1
  par(mar = c(4, 4, 1, 1)) # dimensions for figure

  # Only one plot in this case so no looping needed
  plotmake_fun(PLATE, Time, TabRes, mint, maxt, maxy, samples, k, axx="s", axy="s")
}



