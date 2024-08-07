#' Individual voxel-based SVi function
#'
#' @description This function takes the height values (Z) for each lidar point point cloud and
#' is used within lidR::voxel_metrics() to calculate the sub-voxel i (SVi) density (SViD) and a list of Z values for for each sub-voxel.
#' @param z height
#' @keywords lidar voxel
#' @export
#' @examples
#' voxels <- voxel_metrics(las, func = vox_mt(Z), res = 2)

vox_mt <- function(z)
{
  SViD = length(z)

  metrics =list(
    SViD = SViD,
    z_list = list(z)
  )
  return(metrics)}

#' Individual voxel-based variable function
#'
#' @description This function provides individual voxel-based variables including the frequency ratio (FRDi),frequency ratio (FRSVi),
#' number of returns below each sub-voxel (SVi) (PDiBelow), and the number of returns above each SVi (PDiAbove),
#' sum of returns within a sub-voxels (SVsum), classifcation of a sub-voxel maximum (SVM),
#' median height of the number of returns above (SVMmdAbove) or within (SVMmd) SVMs
#' @param vox lasmetrics3d object created by lidR::voxel_metrics() using the vox_mt() function
#' @param res SVi cubic resolution in units of las object
#' @param max_z if numeric, the final height for all sub-voxels
#' @keywords lidar voxel
#' @import data.table
#' @export

vox_mt2 <- function(vox, res, max_z)
{
  fullvox <- data.table(vox,
                        id_xy = paste0(vox$X,"-", vox$Y),
                        id_xyz = paste0(vox$X,"-", vox$Y,"-",vox$Z))
  if(is.numeric(max_z) & max_z != max(fullvox$Z)){

  z_need <- seq(max(fullvox$Z), max_z, res)[-1]
  new_z <- lapply(z_need, function(x){paste0(unique(fullvox$id_xy),"-", x)})
  new_id <- unlist(new_z)
  empty_z <- data.frame(X = do.call(rbind, lapply(strsplit(new_id, "-"), `[`, 1)),
                        Y = do.call(rbind, lapply(strsplit(new_id, "-"), `[`, 2)),
                        Z = do.call(rbind, lapply(strsplit(new_id, "-"), `[`, 3)),
                        SViD = 0,
                        z_list = NA,
                        id_xy = paste0(do.call(rbind, lapply(strsplit(new_id, "-"), `[`, 1)),
                                       "-",
                                       do.call(rbind, lapply(strsplit(new_id, "-"), `[`, 2))),
                        id_xyz = new_id
  )
  fullvox <- rbind(fullvox, empty_z)
}

  fullvox$SViD[is.na(fullvox$SViD)] <- 0
  fullvox[unlist(lapply(fullvox$z_list, is.null)),]$z_list <- NA
  ##### Individual voxel metrics
  ### P_Di is the number of returns below each voxel (Pearse et al. 2019; Kim et al. 2016 uses returns above)

  # create a vector for Z in each iteration
  unique_Z <- unique(fullvox$Z)

  # calculate points below each voxel
  fullvox$Z <- as.numeric(fullvox$Z)
  point_blow <- fullvox[,as.list(lapply(unique(Z), function(x){
    sum(SViD[Z < x])})),
    by = list(X,Y)]
  colnames(point_blow)[3:ncol(point_blow)] <- paste0(unique(fullvox$Z))
  point_blow <- melt(point_blow, id.vars = c("X","Y"), variable.name = "Z", value.name = "npoints_below")

  point_blow$id_xyz = paste0(point_blow$X,"-",point_blow$Y,"-",point_blow$Z)
  fullvox <- merge(fullvox,point_blow[,c(4,5)], by = "id_xyz")
  # NAs returned for 0 points so it is appropriate to replace them with 0

  ### P_Di_above is the number of returns above each voxel (Kim et al. 2016 uses returns above as P_Di)
  # calculate points above each voxel
  point_above <- fullvox[,as.list(lapply(unique(Z), function(x){
    return(list(
      m_z = median(unlist(z_list[Z >x]), na.rm = TRUE),
      p_a = sum(SViD[Z > x])))}
    )
    ),
    by = list(X,Y)]

  colnames(point_above)[3:ncol(point_above)] <- paste0(unique(fullvox$Z))
  points_am <- point_above[seq(1, nrow(point_above), 2), ]
  points_am <- suppressWarnings(data.table::as.data.table(lapply(points_am,  function(x) unlist(x))))
  points_am[is.na(points_am)] <- 0
  points_am <- melt(points_am, id.vars = c("X","Y"), variable.name = "Z", value.name = "pa_med")
  points_am$id_xyz = paste0(points_am$X,"-",points_am$Y,"-",points_am$Z)

  points_ab <- point_above[seq(2, nrow(point_above), 2), ]
  points_ab <- suppressWarnings(data.table::as.data.table(lapply(points_ab,  function(x) unlist(x))))
  points_ab <- melt(points_ab, id.vars = c("X","Y"), variable.name = "Z", value.name = "npoints_above")
  points_ab$id_xyz = paste0(points_ab$X,"-",points_ab$Y,"-",points_ab$Z)

  points_ab <- merge(points_ab,points_am)
  fullvox <- merge(fullvox, points_ab[,c(4,5,6)], by = "id_xyz")

  ### FR_SVi is the frequency ratio of the number of returns in a SVi in relation to total returns in a SV(in Pearse et al. 2019 this is FR_Di)
  SVsum <- fullvox[, .(SVsum = sum(SViD)), by = "id_xy"]
  fullvox <- merge(fullvox, SVsum, "id_xy")
  fullvox$FR_SVi = fullvox$SViD/fullvox$SVsum
  fullvox[is.na(fullvox$FR_SVi)]$FR_SVi <- 0

  ### FR_Di is the frequency ratio of the number of returns above a SVi in relation to total returns in a SV (Kim et al. 2016)
  fullvox$FR_Di = fullvox$npoints_above/fullvox$SVsum
  fullvox[is.na(fullvox$FR_Di)]$FR_Di <- 0

  ### sub-voxel maximums
  maxSVM <- fullvox[, .SD[which.max(SViD)], by = "id_xy"]
  maxSVM[unlist(lapply(maxSVM$z_list, is.null)),]$z_list <- 0

  ## SVM_M median height of total returns in SVM (Kim et al. 2016)
  maxSVM$SVM_m <- unlist(lapply(maxSVM$z_list, FUN = median))
  maxSVM[is.na(maxSVM$SVM_m),]$SVM_m <- 0

  maxSVM$SVM <- 1

  ## SVM_D median height of returns above svm
  fullvox <- merge(fullvox[,-7], maxSVM[,c(2, 14, 15)], by = "id_xyz", all = TRUE)
  fullvox[is.na(fullvox$SVM),]$SVM <- 0


  metrics = data.table(
    X = as.numeric(fullvox$X),
    Y = as.numeric(fullvox$Y),
    Z = as.numeric(fullvox$Z),
    vox_res = res,
    SViD = fullvox$SViD,
    FRDi = fullvox$FR_Di,
    FRSVi = fullvox$FR_SVi,
    PDiBelow = fullvox$npoints_below,
    PDiAbove = fullvox$npoints_above,
    SVsum = fullvox$SVsum,
    SVMmdAbove = fullvox$pa_med,
    SVM = fullvox$SVM,
    SVMmd = fullvox$SVM_m


  )
  return(metrics)
}

#' Voxel-based variable summary statistics
#'
#' @description This function provides summarized voxel-based variables including the mean, median, variance, standard deviation, coefficient of variation,
#' IQR, skewness, and kurtosis. Summaries for this function are done to the entire area provided either directly or within the
#' lidR::pixel_metrics() function.
#' @keywords lidar voxel
#' @import data.table
#' @export

vox_sum <- function(SViD,FRDi, FRSVi, PDiBelow, PDiAbove, SVsum, SVMmdAbove, SVMmd, SVM, vox_res){
  las_vox <- data.table(SViD,FRDi, FRSVi, PDiBelow, PDiAbove, SVsum, SVMmdAbove, SVMmd, SVM)
  SVM <- las_vox[las_vox$SVM == 1]
  SVM$SVM <- NULL
  means <- apply(SVM[,6:ncol(SVM)], 2, mean, na.rm = TRUE)
  names(means)<- paste0(names(means), "_", vox_res, "_","mean")
  meds <- apply(SVM[,6:ncol(SVM)], 2, median, na.rm = TRUE)
  names(meds)<- paste0(names(meds), "_", vox_res, "_", "med")
  var <- apply(SVM[,6:ncol(SVM)], 2, var, na.rm = TRUE)
  names(var)<- paste0(names(var), "_",   vox_res, "_","var")
  sd <- apply(SVM[,6:ncol(SVM)], 2, sd, na.rm = TRUE)
  names(sd)<- paste0(names(sd), "_",  vox_res, "_","sd")
  cv <- apply(SVM[,6:ncol(SVM)], 2, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)})
  names(cv)<- paste0(names(cv), "_",  vox_res, "_","cv")
  IQR <- apply(SVM[,6:ncol(SVM)], 2,IQR, na.rm = T)
  names(IQR)<- paste0(names(IQR), "_",  vox_res, "_","IQR")
  skew <- apply(SVM[,6:ncol(SVM)], 2, function(x) {(sum((x - mean(x, na.rm = T))^3,na.rm = T)
                                                    /length(x))/(sum((x - mean(x, na.rm = T))^2,na.rm = T)
                                                                 /length(x))^(3/2)})
  names(skew)<- paste0(names(skew), "_",  vox_res, "_","skew")
  kurt <- apply(SVM[,6:ncol(SVM)], 2, function(x) {length(x)*sum((x- mean(x, na.rm = T))^4, na.rm = T)/
      (sum((x - mean(x, na.rm = T))^2, na.rm = T)^2)})
  names(kurt)<- paste0(names(kurt), "_", vox_res, "_", "kurt")
  data_svm <- c(means, meds, var, sd, cv, IQR, skew, kurt)

  las_vox <- las_vox[,-c("SVsum", "SVMmd", "SVM", "SVMmdAbove")]
  means <- apply(las_vox, 2, mean, na.rm = TRUE)
  names(means)<- paste0(names(means), "_", vox_res, "_","mean")
  meds <- apply(las_vox, 2, median, na.rm = TRUE)
  names(meds)<- paste0(names(meds), "_", vox_res, "_", "med")
  var <- apply(las_vox, 2, var, na.rm = TRUE)
  names(var)<- paste0(names(var), "_",   vox_res, "_","var")
  sd <- apply(las_vox, 2, sd, na.rm = TRUE)
  names(sd)<- paste0(names(sd), "_",  vox_res, "_","sd")
  cv <- apply(las_vox, 2, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)})
  names(cv)<- paste0(names(cv), "_",  vox_res, "_","cv")
  IQR <- apply(las_vox, 2,IQR, na.rm = T)
  names(IQR)<- paste0(names(IQR), "_",  vox_res, "_","IQR")
  skew <- apply(las_vox, 2, function(x) {(sum((x - mean(x, na.rm = T))^3,na.rm = T)
                                                            /length(x))/(sum((x - mean(x, na.rm = T))^2,na.rm = T)
                                                                         /length(x))^(3/2)})
  names(skew)<- paste0(names(skew), "_",  vox_res, "_","skew")
  kurt <- apply(las_vox, 2, function(x) {length(x)*sum((x- mean(x, na.rm = T))^4, na.rm = T)/
      (sum((x - mean(x, na.rm = T))^2, na.rm = T)^2)})
  names(kurt)<- paste0(names(kurt), "_", vox_res, "_", "kurt")
  pct_fill_vox <- data.frame(pct_fill_vox = as.numeric(sum(las_vox[,1] > 0 ))/ nrow(las_vox[,1]))
  names(pct_fill_vox) <- paste0(names(pct_fill_vox), "_", vox_res)
  data <- data.frame(c(means, meds, var, sd, cv, IQR, skew, kurt, pct_fill_vox,data_svm))

  return(data)

}



#' Voxel point cloud summary function
#'
#' @description This function provides individual voxel-based variables including the frequency ratio (FRDi),frequency ratio (FRSVi),
#' number of returns below each sub-voxel (SVi) (PDiBelow), and the number of returns above each SVi (PDiAbove),
#' sum of returns within a sub-voxels (SVsum), classifcation of a sub-voxel maximum (SVM), and
#' median height of the number of returns above (SVMmdAbove) or within (SVMmd) SVMs. These variables are summarized
#' for the entire point cloud including the mean, median, variance, standard deviation, coefficient of variation,
#' IQR, skewness, and kurtosis
#' @param las las object
#' @param vox_res SVi cubic resolution in units of las object
#' @param max_z If numeric, the final height for all sub-voxels. If NULL, the max height for the entire point cloud
#' (or chunk for catalog processing) will be used.
#' @keywords lidar voxels
#' @return A data frame with each column reporting 65 different voxel variables with the naming convention:
#' (voxel variable)_(voxel resolution)_(summary statistic)
#' @import data.table
#' @import lidR
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile)
#' las <- filter_poi(las, Classification != LASNOISE)
#' las <- normalize_height(las, knnidw(k = 8, p = 2))
#' las <- filter_poi(las, Z < 50 & Z >= 1.37 )
#' las <- decimate_points(las, random_per_voxel(res = 1, n = 8))
#' metrics <- voxel_summary(las, vox_res = 2, max_z = 40)
#' print(metrics)

voxel_summary <- function(las, vox_res = vox_res, max_z ){
  vox <- lidR::voxel_metrics(las, func = vox_mt(Z), res = vox_res, all_voxels = TRUE)
  vox_2 <- vox_mt2(vox, res = vox_res, max_z = max_z)
  out <- suppressWarnings(LAS(vox_2, header = las@header, crs = las@crs, index = las@index ))
  vox_sum(out$SViD, out$FRDi, out$FRSVi, out$PDiBelow, out$PDiAbove, out$SVsum, out$SVMmdAbove,
          out$SVMmd, out$SVM, vox_res[1])
}

#' Voxel raster summary function
#'
#' @description This function provides individual voxel-based variables including the frequency ratio (FRDi),frequency ratio (FRSVi),
#' number of returns below each sub-voxel (SVi) (PDiBelow), and the number of returns above each SVi (PDiAbove),
#' sum of returns within a sub-voxels (SVsum), classifcation of a sub-voxel maximum (SVM), and
#' median height of the number of returns above (SVMmdAbove) or within (SVMmd) SVMs. These variables are summarized
#' within each pixel including the mean, median, variance, standard deviation, coefficient of variation,
#' IQR, skewness, and kurtosis
#' @param las las object
#' @param vox_res SVi cubic resolution in units of las object
#' @param rast_res raster pixel resolution in units of las object
#' @param max_z If numeric, the final height for all sub-voxels. If NULL, the max height for the entire point cloud
#' (or chunk for catalog processing) will be used.
#' @keywords lidar voxel metrics
#' @return returns a SpatRaster with 65 different layers representing voxel variables summarized to the resolution defined in (rast_res) with the naming convention:
#' (voxel variable)_(voxel resolution)_(summary statistic)
#' @import data.table
#' @import lidR
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile)
#' las <- filter_poi(las, Classification != LASNOISE)
#' las <- normalize_height(las, knnidw(k = 8, p = 2))
#' las <- filter_poi(las, Z < 50 & Z >= 1.37 )
#' las <- decimate_points(las, random_per_voxel(res = 1, n = 8))
#' metrics <- voxel_raster(las, vox_res = 2, rast_res = 10, max_z = 40)
#' plot(metrics)


voxel_raster <- function(las, vox_res = vox_res, rast_res =  rast_res, max_z ){
  vox <- lidR::voxel_metrics(las, func = vox_mt(Z), res = vox_res, all_voxels = TRUE)
  vox_2 <- vox_mt2(vox, res = vox_res, max_z = max_z)
  out <- LAS(vox_2, header = las@header, crs = las@crs, index = las@index)
  raster <- pixel_metrics(out,
                          func = vox_sum(SViD,FRDi, FRSVi, PDiBelow, PDiAbove, SVsum, SVMmdAbove, SVMmd, SVM, vox_res = vox_res[1]),
                          res = rast_res)
}

#' Tree-based variable summary statistics
#'
#' @description This function provides summarized tree-based variables including the mean, standard deviation, and coefficient of variation.
#' Summaries for this function are done to the entire area provided either directly or within the
#' lidR::pixel_metrics() function.
#' @param columns from the lidR::crown_metrics() function (i.e., X, Y, Z, npoints, ca)
#' @keywords lidar tree
#' @export


tree_sum <- function(Z,npoints,ca){
  data <- data.table(Z, npoints, ca)
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

#' Tree point cloud summary function
#'
#' @description This function provides individual tree-based variables including number of trees (ntrees),
#' height of trees (ht), number of points per tree (npts), and crown area (ca). These variables are summarized
#' for the entire point cloud including the mean, median, variance, standard deviation, coefficient of variation,
#' IQR, skewness, and kurtosis
#' @param las las object with tree ID column
#' @keywords lidar tree
#' @import data.table
#' @import lidR
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile)
#' las <- filter_poi(las, Classification != LASNOISE)
#' las <- normalize_height(las, knnidw(k = 8, p = 2))
#' las <- filter_poi(las, Z < 50 & Z >= 1.37 )
#' las <- decimate_points(las, random_per_voxel(res = 1, n = 8))
#' las_tree <- segment_trees(las, li2012())
#' tree_metrics <- tree_summary(las_tree)
#' print(tree_metrics)

tree_summary <- function(las){
  trees <- lidR::crown_metrics(las, .stdtreemetrics)
  locs <- data.frame(sf::st_coordinates(trees), trees[c(3,4)])
  locs <- locs[-6]
  names(locs)[5] <- "ca"
  trees_las <- suppressWarnings(lidR::LAS(locs, header = las@header, crs = las@crs, index = las@index))
  tree_sum(trees_las$Z,trees_las$npoints, trees_las$ca)
}

#' Tree raster summary function
#'
#' @description This function provides individual tree-based variables including number of trees (ntrees),
#' height of trees (ht), number of points per tree (npts), and crown area (ca). These variables are summarized
#' within each pixel including the mean, median, variance, standard deviation, coefficient of variation,
#' IQR, skewness, and kurtosis
#' @param las las object with tree ID column
#' @param rast_res desired spatial resolution of raster
#' @keywords lidar tree metrics
#' @import data.table
#' @import lidR
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile)
#' las <- filter_poi(las, Classification != LASNOISE)
#' las <- normalize_height(las, knnidw(k = 8, p = 2))
#' las <- filter_poi(las, Z < 50 & Z >= 1.37 )
#' las <- decimate_points(las, random_per_voxel(res = 1, n = 8))
#' las_tree <- segment_trees(las, li2012())
#' tree_metrics <- tree_raster(las_tree, rast_res = 20)
#' plot(tree_metrics)

tree_raster <- function(las, rast_res){
  trees <- lidR::crown_metrics(las, .stdtreemetrics)
  locs <- data.frame(sf::st_coordinates(trees), trees[c(3,4)])
  locs <- locs[-6]
  names(locs)[5] <- "ca"
  trees_las <- suppressWarnings(lidR::LAS(locs, header = las@header, crs = las@crs, index = las@index))
  raster <- pixel_metrics(trees_las,
                          func = tree_sum(Z, npoints, ca),
                          res = rast_res)
}
