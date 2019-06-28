### =========================================================================
### recstrat_prop
### =========================================================================
#' Random Ecologically Stratified Sampling of propotional numbers
#'
#' This function randomly collects a user-defined total number of samples
#' from the stratification layer.
#'
#' @param in_grid The stratification grid to be sampled.
#' @param sample_no The total number of pixels to be sampled.
#' @details The number of samples per class are determined proportional to
#' the abundance of each class. The number of classes in the stratification
#' layer are determined automatically from the integer input map. If the
#' proportion of samples for a certain class is below one then no samples
#' are collected for this class.
#' @return Returns a dataframe with the selected sampling locations their
#' coordinates and the strata they belong in.
#' @author Achilleas Psomas, Niklaus Zimmermann
#' @export
#' @examples
#'
#' bio3<- raster(system.file("external/bioclim/current/bio3.grd",package="biomod2"))
#' bio12<- raster(system.file("external/bioclim/current/bio12.grd",package="biomod2"))
#' B3.rcl<-rcls.grd(bio3,9)
#' B12.rcl<-rcls.grd(bio12,9)
#' B3B12.comb <- B12.rcl+B3.rcl*10
#' B3B12.prop_samples <- recstrat_prop(B3B12.comb,100)
#' plot(B3B12.comb)
#' points(B3B12.prop_samples$x,B3B12.prop_samples$y,pch=16,cex=0.6,col=B3B12.prop_samples$class)

recstrat_prop=function(in_grid, sample_no){

  strata <- na.omit(unique(getValues(in_grid)))
  strata_no <- length(strata)
  in_grid_SPixels <- as(in_grid, "SpatialPointsDataFrame")
  total_pixels <- nrow(in_grid_SPixels@data)
  strata_stats <- table(in_grid_SPixels$layer)
  strata_stats_sorted <- as.data.frame(sort(strata_stats, decreasing = TRUE))
  pixels_largest_strata <- max(strata_stats_sorted$Freq)
  proportion_largest_strata <- round((pixels_largest_strata *
                                        sample_no)/total_pixels)
  result_list <- list()
  for (j in 1:length(strata)) {
    grid_sel <- in_grid_SPixels[in_grid_SPixels@data[, 1] ==
                                  strata[j], ]
    proportion <- ceiling(log(dim(grid_sel@data)[1])/log(pixels_largest_strata) *
                            proportion_largest_strata)
    optimal_samples_per_class <- ifelse(proportion < dim(grid_sel@data)[1],
                                        proportion, dim(grid_sel@data)[1])
    sp_points <- grid_sel[sample(1:nrow(grid_sel), optimal_samples_per_class,
                                 replace = FALSE), ]
    sample_points <- cbind(sp_points@coords, class = sp_points@data[,
                                                                    1])
    result_list[[j]] <- sample_points
  }
  result <- data.frame(do.call("rbind", result_list))
  names(result) <- c("x", "y", "class")
  return(result)
}
