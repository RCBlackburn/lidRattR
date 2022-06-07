#' Voxel metrics function
#'
#' This function takes the height values for each lidar point and
#' is used within lidR::voxel_metrics() and calculates the sub-voxel density (SVi) for each voxel.
#' @param z height
#' @keywords lidar voxel metrics
#' @export
#' @examples
#' voxels <- voxel_metrics(las, func = vox_mt(Z), res = resolution)

vox_mt <- function(z)
{
  SVi = length(z)

  metrics =list(
    SVi = SVi
  )
  return(metrics)}

#' Standard voxel metrics function
#'
#' This function provides voxel metrics off similar metrics in
#' Pearse et al. 2019 and Kim et al. 2016. The function also includes
#' additional summary statistics for height and intensity values within each voxel
#' @param vox a
#' @keywords lidar voxel metrics
#' @import data.table
#' @import dplyr
#' @export
#' @examples
#' std_voxel()
#'
vox_mt2 <- function(vox)
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
  return(metrics)
}

##outputs a voxelsed las with voxel metrics per voxel
vox <- function(las, res = res){
  vox <- lidR::voxel_metrics(las, func = vox_mt(Z), res = res, all_voxels = TRUE)
  vox_2 <- vox_mt2(vox)
  out <- LAS(vox_2, header = las@header, crs = las@crs, index = las@index )
}

vox_sum <-  function(las_vox, res){
  las_vox <- las_vox@data
  means <- apply(las_vox[,4:ncol(las_vox)], 2, mean, na.rm = TRUE)
  names(means)<- paste0(names(means), "_", res, "_", "mean")
  meds <- apply(las_vox[,4:ncol(las_vox)], 2, median, na.rm = TRUE)
  names(meds)<- paste0(names(meds), "_",  res, "_","med")
  var <- apply(las_vox[,4:ncol(las_vox)], 2, var, na.rm = TRUE)
  names(var)<- paste0(names(var), "_", res, "_", "var")
  sd <- apply(las_vox[,4:ncol(las_vox)], 2, sd, na.rm = TRUE)
  names(sd)<- paste0(names(sd), "_", res, "_", "sd")
  cv <- apply(las_vox[,4:ncol(las_vox)], 2, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)})
  names(cv)<- paste0(names(cv), "_", res, "_", "cv")
  IQR <- apply(las_vox[,4:ncol(las_vox)], 2,IQR, na.rm = T)
  names(IQR)<- paste0(names(IQR), "_", res, "_", "IQR")
  skew <- apply(las_vox[,4:ncol(las_vox)], 2, function(x) {(sum((x - mean(x, na.rm = T))^3,na.rm = T)
                                                            /length(x))/(sum((x - mean(x, na.rm = T))^2,na.rm = T)
                                                                         /length(x))^(3/2)})
  names(skew)<- paste0(names(skew), "_", res, "_", "skew")
  kurt <- apply(las_vox[,4:ncol(las_vox)], 2, function(x) {length(x)*sum((x- mean(x, na.rm = T))^4, na.rm = T)/
      (sum((x - mean(x, na.rm = T))^2, na.rm = T)^2)})
  names(kurt)<- paste0(names(var), "_", res, "_", "kurt")
  pct_fill_vox <- data.frame(pct_fill_vox = as.numeric(table(las_vox$SVi== 0)[1])/ length(las_vox$SVi))
  names(pct_fill_vox) <- paste0(names(pct_fill_vox), "_", res)
  data <- data.frame(c(means, meds, var, sd, cv, IQR, skew, kurt, pct_fill_vox))

  return(data)
}



vox_sum_raster <- function(SVi, FRDi, PDi, PDi_above){
  las_vox <- data.frame(SVi = SVi, FRDi = FRDi, PDi = PDi, PDi_above = PDi_above)
  means <- apply(las_vox, 2, mean, na.rm = TRUE)
  names(means)<- paste0(names(means), "_", "mean")
  meds <- apply(las_vox, 2, median, na.rm = TRUE)
  names(meds)<- paste0(names(meds), "_","med")
  var <- apply(las_vox, 2, var, na.rm = TRUE)
  names(var)<- paste0(names(var), "_", "var")
  sd <- apply(las_vox, 2, sd, na.rm = TRUE)
  names(sd)<- paste0(names(sd), "_",  "sd")
  cv <- apply(las_vox, 2, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)})
  names(cv)<- paste0(names(cv), "_", "cv")
  IQR <- apply(las_vox, 2,IQR, na.rm = T)
  names(IQR)<- paste0(names(IQR), "_", "IQR")
  skew <- apply(las_vox, 2, function(x) {(sum((x - mean(x, na.rm = T))^3,na.rm = T)
                                                            /length(x))/(sum((x - mean(x, na.rm = T))^2,na.rm = T)
                                                                         /length(x))^(3/2)})
  names(skew)<- paste0(names(skew), "_","skew")
  kurt <- apply(las_vox, 2, function(x) {length(x)*sum((x- mean(x, na.rm = T))^4, na.rm = T)/
      (sum((x - mean(x, na.rm = T))^2, na.rm = T)^2)})
  names(kurt)<- paste0(names(var), "_", "kurt")
  pct_fill_vox <- data.frame(pct_fill_vox = as.numeric(table(las_vox[1]== 0)[1])/ length(las_vox[1]))
  names(pct_fill_vox) <- paste0(names(pct_fill_vox))

  data <- data.frame(c(means, meds, var, sd, cv, IQR, skew, kurt, pct_fill_vox))

  return(data)

  }

.vox_raster <- ~vox_sum_raster(SVi, FRDi, PDi, PDi_above)


