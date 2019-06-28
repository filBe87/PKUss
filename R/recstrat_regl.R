### =========================================================================
### recstrat_regl
### =========================================================================
#' Random Ecologically Stratified Sampling of equal numbers
#'
#' This function randomly takes an equal number of samples per class
#' in the stratification layer.
#'
#' @param in_grid The stratification grid to be sampled.
#' @param sample_no The total number of pixels to be sampled.
#' @details The number of classes in the stratification layer is determined
#' automatically from the integer input map. If the number of pixels in a
#' class is higher than the number of samples, then a random selection without
#' re-substitution is performed, otherwise all pixels of that class are selected.
#' @return Returns a dataframe with the selected sampling locations their
#' coordinates and the strata they belong in.
#' @author Achilleas Psomas, Niklaus Zimmermann
#' @export
#' @examples
#'
#'bio3<- raster(system.file("external/bioclim/current/bio3.grd",package="biomod2"))
#'bio12<- raster(system.file("external/bioclim/current/bio12.grd",package="biomod2"))
#'B3.rcl<-rcls.grd(bio3,9)
#'B12.rcl<-rcls.grd(bio12,9)
#'B3B12.comb <- B12.rcl+B3.rcl*10
#'B3B12.regl_samples <- recstrat_prop(B3B12.comb,100)
#'plot(B3B12.comb)
#'points(B3B12.regl_samples$x,B3B12.regl_samples$y,pch=16,cex=0.6,col=B3B12.regl_samples$class)

#'
recstrat_regl=function(in_grid, sample_no){

  strata <- na.omit(unique(getValues(in_grid)))
  strata_no <- length(strata)
  if (sample_no < strata_no) {
    stop("Stoping Execution: The number of samples is lower the total number of unique classes")
  }
  else {
    samples_per_class <- round(sample_no/strata_no)
  }
  in_grid_SPixels <- as(in_grid, "SpatialPointsDataFrame")
  result_list <- list()
  for (j in 1:length(strata)) {
    grid_sel <- in_grid_SPixels[in_grid_SPixels@data[, 1] ==
                                  strata[j], ]
    optimal_samples_per_class <- ifelse(dim(grid_sel@data)[1] >
                                          samples_per_class, samples_per_class, dim(grid_sel@data)[1])
    sp_points <- grid_sel[sample(1:nrow(grid_sel), optimal_samples_per_class,
                                 replace = FALSE), ]
    result_list[[j]] <- cbind(sp_points@coords, sp_points@data)
  }
  result <- do.call("rbind", result_list)
  names(result) <- c("x", "y", "class")
  return(result)
}
