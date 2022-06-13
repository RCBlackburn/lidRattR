#' Individual voxel-based SVi function
#'
#' This function takes the height values for each lidar point point cloud and
#' is used within lidR::voxel_metrics() to calculate the sub-voxel density (SVi) for each voxel.
#' @param z height
#' @keywords lidar voxel metrics
#' @export
#' @examples
#' voxels <- voxel_metrics(las, func = vox_mt(Z), res = 2)

vox_mt <- function(z)
{
  SVi = length(z)

  metrics =list(
    SVi = SVi
  )
  return(metrics)}

#' Individual voxel-based variable function
#'
#' This function provides individual voxel-based variables including the frequency ratio (FRDi),
#' number of returns below each voxel (PDi), and the number of returns above each voxel (PDi_above),
#' @param vox lasmetrics3d object created by lidR::voxel_metrics() using the vox_mt() function
#' @keywords lidar voxel metrics
#' @import dplyr
#' @import data.table
#' @import tidyr
#' @export
#' @examples
#' std_voxel()
#'
vox_mt2 <- function(vox, res)
{
  fullvox <- data.table(vox, id_xyz = paste0(vox$X,"-", vox$Y,"-",vox$Z))
  fullvox$SVi[is.na(fullvox$SVi)] <- 0

  ##### Individual voxel metrics
  ### FR_SVi is the frequency ratio of the number of returns in a voxel in relation to total returns (in Pearse et al. 2019 this is FR_Di)
  fullvox$FR_SVi <- fullvox$SVi/sum(fullvox$SVi)

  ### P_Di is the number of returns below each voxel (Pearse et al. 2019; Kim et al. 2016 uses returns above)
  # create a vector for Z in each iteration
  unique_Z <- unique(fullvox$Z)

  # calculate points below each voxel

  point_blow <- fullvox[,as.list(lapply(unique(Z), function(x){
    sum(SVi[Z < x])})),
    by = list(X,Y)]
  colnames(point_blow)[3:ncol(point_blow)] <- paste0(unique(fullvox$Z))
  point_blow <- point_blow %>%
    tidyr::pivot_longer(cols = 3:ncol(point_blow), names_to = "Z", values_to = "npoints_below")

  point_blow$id_xyz = paste0(point_blow$X,"-",point_blow$Y,"-",point_blow$Z)
  fullvox <- fullvox %>% left_join(point_blow[,c(4,5)], by = "id_xyz")
  # NAs returned for 0 points so it is appropriate to replace them with 0


  ### P_Di_above is the number of returns above each voxel (Kim et al. 2016 uses returns above as P_Di)
  # calculate points below each voxel
  point_above <- fullvox[,as.list(lapply(unique(Z), function(x){
    sum(SVi[Z > x])})),
    by = list(X,Y)]
  colnames(point_above)[3:ncol(point_above)] <- paste0(unique(fullvox$Z))
  point_above <- point_above %>%
    tidyr::pivot_longer(cols = 3:ncol(point_above), names_to = "Z", values_to = "npoints_above")

  point_above$id_xyz = paste0(point_above$X,"-",point_above$Y,"-",point_above$Z)
  fullvox <- fullvox %>% left_join(point_above[,c(4,5)], by = "id_xyz")

  ### FR_Di is the frequency ratio of the number of returns above a voxel in relation to total returns (Kim et al. 2016)
  fullvox$FR_Di = fullvox$npoints_above/sum(fullvox$SVi)

  metrics =data.frame(
    X = fullvox$X,
    Y = fullvox$Y,
    Z = fullvox$Z,
    SVi = fullvox$SVi,
    FRDi = fullvox$FR_Di,
    PDi = fullvox$npoints_below,
    PDi_above = fullvox$npoints_above
  )
  names(metrics)[4:ncol(metrics)] <- paste0(names(metrics)[4:ncol(metrics)], "_", res)
  return(metrics)
}

#' Voxelize function
#'
#' This function provides individual voxel-based variables including sub-voxel density (SVi), frequency ratio (FRDi),
#' number of returns below each voxel (PDi), and the number of returns above each voxel (PDi_above).
#' @param las las object
#' @param res voxel cubic resolution in units of las object
#' @keywords lidar voxel metrics
#' @import data.table
#' @import tidyr
#' @import lidR
#' @export
#' @examples
#' las <- readLAS(LASfile)
#' las_vox <- ind_vox(las, res = 2)

ind_vox <- function(las, res = res){
  vox <- lidR::voxel_metrics(las, func = vox_mt(Z), res = res, all_voxels = TRUE)
  vox_2 <- vox_mt2(vox, res = res)
  out <- LAS(vox_2, header = las@header, crs = las@crs, index = las@index )
}




#' Voxel-based variable summary statistics
#'
#' This function provides summarized voxel-based variables including for each of the variables created using the
#' ind_vox() function. Summary statistics include the mean, median, varaince, standard deviation, coefficient of variation,
#' IQR, skewness, and kurtosis.
#' @param las voxelized las object
#' @param res voxel cubic resolution in units of las object
#' @keywords lidar voxel metrics
#' @import data.table
#' @export
#' @examples
#' las <- readLAS(LASfile)
#' las_vox <- vox(las, res = 2)
#' voxel_summaries <- vox_sum(las_vox, 2)
#'
vox_sum <-  function(las_vox, res){
  las_vox <- las_vox@data
  means <- apply(las_vox[,4:ncol(las_vox)], 2, mean, na.rm = TRUE)
  names(means)<- paste0(names(means), "_", "mean")
  meds <- apply(las_vox[,4:ncol(las_vox)], 2, median, na.rm = TRUE)
  names(meds)<- paste0(names(meds), "_", "med")
  var <- apply(las_vox[,4:ncol(las_vox)], 2, var, na.rm = TRUE)
  names(var)<- paste0(names(var), "_",  "var")
  sd <- apply(las_vox[,4:ncol(las_vox)], 2, sd, na.rm = TRUE)
  names(sd)<- paste0(names(sd), "_", "sd")
  cv <- apply(las_vox[,4:ncol(las_vox)], 2, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)})
  names(cv)<- paste0(names(cv), "_", "cv")
  IQR <- apply(las_vox[,4:ncol(las_vox)], 2,IQR, na.rm = T)
  names(IQR)<- paste0(names(IQR), "_", "IQR")
  skew <- apply(las_vox[,4:ncol(las_vox)], 2, function(x) {(sum((x - mean(x, na.rm = T))^3,na.rm = T)
                                                            /length(x))/(sum((x - mean(x, na.rm = T))^2,na.rm = T)
                                                                         /length(x))^(3/2)})
  names(skew)<- paste0(names(skew), "_", "skew")
  kurt <- apply(las_vox[,4:ncol(las_vox)], 2, function(x) {length(x)*sum((x- mean(x, na.rm = T))^4, na.rm = T)/
      (sum((x - mean(x, na.rm = T))^2, na.rm = T)^2)})
  names(kurt)<- paste0(names(var), "_", "kurt")
  pct_fill_vox <- data.frame(pct_fill_vox = as.numeric(table(las_vox$SVi== 0)[1])/ length(las_vox$SVi))
  names(pct_fill_vox) <- paste0(names(pct_fill_vox), "_", res)
  data <- data.frame(c(means, meds, var, sd, cv, IQR, skew, kurt, pct_fill_vox))

  return(data)
}


#' Voxel-based variable summary statistics for lidR::pixel_metrics() function
#'
#' This function provides summarized voxel-based variables just as the vox_sum() function.
#' This can be used in conjunction is lidR::pixel_metrics() to make raster layers of voxel-based summary statistics.
#' @param SVi
#' @param FRDi
#' @param PDi
#' @param PDi_above
#' @keywords lidar voxel metrics
#' @import data.table
#' @export
#' @examples
#' las <- readLAS(LASfile)
#' las_vox <- vox(las, res = 2)
#' v_metrics <- pixel_metrics(las_vox, func = .vox_raster, res = 20)
#'
#'
vox_sum_raster <- function(SVi, FRDi, PDi, PDi_above, vox_res){
  las_vox <- data.frame(SVi,FRDi, PDi, PDi_above)
  means <- apply(las_vox, 2, mean, na.rm = TRUE)
  names(means)<- paste0(names(means), "_", vox_res, "_", "mean")
  meds <- apply(las_vox, 2, median, na.rm = TRUE)
  names(meds)<- paste0(names(meds), "_", vox_res, "_", "med")
  var <- apply(las_vox, 2, var, na.rm = TRUE)
  names(var)<- paste0(names(var), "_", vox_res, "_", "var")
  sd <- apply(las_vox, 2, sd, na.rm = TRUE)
  names(sd)<- paste0(names(sd), "_", vox_res, "_", "sd")
  cv <- apply(las_vox, 2, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)})
  names(cv)<- paste0(names(cv), "_", vox_res, "_",  "cv")
  IQR <- apply(las_vox, 2,IQR, na.rm = T)
  names(IQR)<- paste0(names(IQR), "_", vox_res, "_",  "IQR")
  skew <- apply(las_vox, 2, function(x) {(sum((x - mean(x, na.rm = T))^3,na.rm = T)
                                                            /length(x))/(sum((x - mean(x, na.rm = T))^2,na.rm = T)
                                                                         /length(x))^(3/2)})
  names(skew)<- paste0(names(skew), "_", vox_res, "_", "skew")
  kurt <- apply(las_vox, 2, function(x) {length(x)*sum((x- mean(x, na.rm = T))^4, na.rm = T)/
      (sum((x - mean(x, na.rm = T))^2, na.rm = T)^2)})
  names(kurt)<- paste0(names(var),  "_", vox_res, "_",  "kurt")
  pct_fill_vox <- data.frame(pct_fill_vox = as.numeric(table(las_vox[1]== 0)[1])/ length(las_vox[1]))
  names(pct_fill_vox) <- paste0(names(pct_fill_vox))

  data <- data.frame(c(means, meds, var, sd, cv, IQR, skew, kurt, pct_fill_vox))

  return(data)

  }


#' Tree variables to point cloud
#'
#' This function creates a point cloud with tree merics that can be summarized
#' @param las las object
#' @keywords lidar tree metrics
#' @import data.table
#' @import tidyr
#' @import lidR
#' @export
#' @examples
#' las <- readLAS(LASfile)
#' las_tree <- segment_trees(las, li2012())
#' las_tree <- ind_tree(las_tree)
#'
ind_tree <- function(las){
  trees <- lidR::crown_metrics(las, .stdtreemetrics)
  locs <- data.frame(sf::st_coordinates(trees), trees[c(3,4)])
  locs <- locs[-6]
  names(locs)[5] <- "ca"
  trees_las <- suppressWarnings(lidR::LAS(locs, header = las@header, crs = las@crs, index = las@index))
}



#' Tree-based variable summary statistics
#'
#' This function provides summarized tree-based variables  for each of the variables created using the
#' ind_tree() function. Summary statistics include the mean, standard deviation, and coefficient of variation.
#' @param las las object from ind_tree() function
#' @keywords lidar tree metrics
#' @export
#' @examples
#' las <- readLAS(LASfile)
#' las_tree <- segment_trees(las, li2012())
#' las_tree <- ind_tree(las_tree)
#' plot_t_metrics <- tree_sum(las_tree)

tree_sum <- function(las_tree){
  data <- las_tree@data
  tree_metrics <- data.frame(
  ntrees = length(data$Z),
  ht_mean =  mean(data$Z, na.rm = TRUE),
  ht_sd = sd(data$Z, na.rm = TRUE),
  ht_cv = sd(data$Z, na.rm = TRUE)/mean(data$Z, na.rm = TRUE),
  npts_mean = mean(data$npoints, na.rm = TRUE),
  npts_sd = sd(data$npoints, na.rm = TRUE),
  npts_cv = sd(data$npoints, na.rm = TRUE)/mean(data$npoints, na.rm = TRUE),
  ca_mean = mean(data$ca),
  ca_sd = sd(data$ca),
  ca_cv = sd(data$ca, na.rm = TRUE)/mean(data$ca, na.rm = TRUE))
  return(tree_metrics)
}

#' Tree-based variable summary statistics for lidR::pixel_metrics() function
#'
#' This function provides summarized tree-based variables just as the tree_sum() function.
#' This can be used in conjunction is lidR::pixel_metrics() to make raster layers of tree-based summary statistics.
#' @param Z
#' @param npoints
#' @param ca
#' @keywords lidar tree metrics
#' @export
#' @examples
#' las <- readLAS(LASfile)
#' las_tree <- segment_trees(las, li2012())
#' las_tree <- ind_tree(las_tree)
#' t_metrics <- pixel_metrics(las_tree, tree_sum_raster(Z, npoints, ca), res = 20)
#'

tree_sum_raster <- function(Z, npoints, ca){
  tree_metrics <- list(
  ntrees = length(Z),
  ht_mean =  mean(Z, na.rm = TRUE),
  ht_sd = sd(Z, na.rm = TRUE),
  ht_cv = sd(Z, na.rm = TRUE)/mean(Z, na.rm = TRUE),
  npts_mean = mean(npoints, na.rm = TRUE),
  npts_sd = sd(npoints, na.rm = TRUE),
  npts_cv = sd(npoints, na.rm = TRUE)/mean(npoints, na.rm = TRUE),
  ca_mean = mean(ca),
  ca_sd = sd(ca),
  ca_cv = sd(ca, na.rm = TRUE)/mean(ca, na.rm = TRUE))
  return(tree_metrics)
}
