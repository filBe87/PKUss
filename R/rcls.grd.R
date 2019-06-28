### =========================================================================
### rcls.grd
### =========================================================================
#' Reclassifying grids function
#'
#' Function for reclassifying grid files to get a combined statification from
#' more than one grid
#'
#' @param in_grid The grid to be reclassified.
#' @param no.classes The number of desired new classes.
#' @details This function reclassifies the input grid into a number of new
#' classes that the user defines. The boundaries of each class are decided
#' automatically by splitting the range of values of the input grid into
#' the user defined number of classes.
#' @return Returns a reclassified Raster object
#' @author Achilleas Psomas, Niklaus Zimmermann
#' @export
#' @examples
#'
#' bio3<- raster(system.file("external/bioclim/current/bio3.grd",package="biomod2"))
#' bio12<- raster(system.file("external/bioclim/current/bio12.grd",package="biomod2"))
#' B3.rcl<-rcls.grd(bio3,9)
#' B12.rcl<-rcls.grd(bio12,9)
#' B3B12.comb <- B12.rcl+B3.rcl*10
#'
#' # Plotting a histogram of the classes
#' hist(B3B12.comb,breaks=100,col=heat.colors(88))
#' # Plotting the new RasterLayer (9x9 classes)
#' plot(B3B12.comb,col=rev(rainbow(88)),main="Stratified map")
#'
rcls.grd=function(in_grid, no.classes){
  new_classes <- classIntervals(getValues(in_grid), no.classes,
                                style = "equal")
  new_classes_breaks <- new_classes$brks
  new_classes_limits <- matrix(ncol = 3, nrow = no.classes)
  classes <- 1:no.classes
  for (i in 1:no.classes) {
    new_classes_limits[i, 1] <- new_classes_breaks[i]
    new_classes_limits[i, 2] <- new_classes_breaks[i + 1]
    new_classes_limits[i, 3] <- classes[i]
  }
  in_grid_reclass <- reclassify(in_grid, new_classes_limits,
                                include.lowest = TRUE)
  return(in_grid_reclass)
}
