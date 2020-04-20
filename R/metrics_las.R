#' Standard cloud metrics function
#'
#' This function provides a variety of area based metrics from Pearse et al. 2019 and Mitchell et al. 2012.
#' @param x X location.
#' @param y Y location.
#' @param z Z location.
#' @param ReturnNumber Return number column from las object.
#' @keywords lidar cloud metrics
#' @export
#' @examples
#' std_cloud()

std_cloud <- function(x, y, z, ReturnNumber)
{
  # number of returns
  npoints = length(z)
  npoints_1 = length(ReturnNumber[ReturnNumber == 1])
  npoints_2 = length(ReturnNumber[ReturnNumber == 2])
  npoints_3 = length(ReturnNumber[ReturnNumber == 3])
  # height statistics
  zmax = max(z)
  zmin = min(z)
  uniqz = unique(z)
  zmode = uniqz[which.max(tabulate(match(z, uniqz)))]
  zmean = mean(z)
  zqmean = sqrt(mean(z^2))
  zmed = median(z)
  zsd = sd(z)
  zvar = var(z)
  zcv = sd(z)/mean(z)
  zIQR = IQR(z)
  zaad = stats::mad(z, center = mean(z))
  zskew = e1071::skewness(z)
  zkurt = e1071::kurtosis(z)
  # L moments
  zL1 = Lmoments::Lmoments(z)[1]
  zL2 =Lmoments::Lmoments(z)[2]
  zL3 =Lmoments::Lmoments(z)[3]
  zL4 =Lmoments::Lmoments(z)[4]
  zLskew = Lmoments::Lcoefs(z)[3]
  zLkurt = Lmoments::Lcoefs(z)[4]
  # height percentiles
  percentHt1 = quantile(z, 0.01)
  percentHt5 = quantile(z, 0.05)
  percentHt10 = quantile(z, 0.1)
  percentHt15 = quantile(z, 0.15)
  percentHt20 = quantile(z, 0.20)
  percentHt25 = quantile(z, 0.25)
  percentHt30 = quantile(z, 0.30)
  percentHt40 = quantile(z, 0.40)
  percentHt50 = quantile(z, 0.50)
  percentHt60 = quantile(z, 0.60)
  percentHt70 = quantile(z, 0.70)
  percentHt75 = quantile(z, 0.75)
  percentHt80 = quantile(z, 0.80)
  percentHt90 = quantile(z, 0.90)
  percentHt95 = quantile(z, 0.95)
  percentHt99 = quantile(z, 0.99)
  # PErcent returns at height bins
  d5 = (sum(z <= 5)/length(z))*100
  d10 = (sum(z <= 10 & z > 5 )/length(z))*100
  d15 = (sum(z <= 15 & z > 10 )/length(z))*100
  d20 = (sum(z <= 20 & z > 15 )/length(z))*100
  d25 = (sum(z <= 25 & z > 20 )/length(z))*100
  d30 = (sum(z <= 30 & z > 25 )/length(z))*100
  d35 = (sum(z <= 35 & z > 30 )/length(z))*100
  d40 = (sum(z <= 40 & z > 35 )/length(z))*100
  d45 = (sum(z <= 45 & z > 40 )/length(z))*100
  d50 = (sum(z <= 50 & z > 45 )/length(z))*100
  d55 = (sum(z > 50 )/length(z))*100
  # canopy related metrics
  prct_10_1r = sum(z >= 10 & ReturnNumber == 1)/ length(z)
  pcrt_20_1r = sum(z >= 20 & ReturnNumber == 1)/ length(z)
  prct_1r_ab_mean = sum(mean(z) >= 20 & ReturnNumber == 1)/ length(z)
  prct_1r_ab_mode = sum(mode(z) >= 20 & ReturnNumber == 1)/ length(z)
  pcrt_10 = sum(z >= 10)/ length(z)
  pcrt_20 = sum(z >= 20)/ length(z)
  prct_ab_mean = sum(mean(z) >= 20)/ length(z)
  prct_ab_mode = sum(mode(z) >= 20)/ length(z)
  metrics = list(
    npts = npoints,
    npnts_1 = npoints_1,
    npnts_2 = npoints_2,
    npnts_3 = npoints_3,
    zmax = zmax,
    zmin = zmin,
    zmode = zmode,
    zmean = zmean,
    zqmean = zqmean,
    zmed = zmed,
    zsd = zsd,
    zvar = zvar,
    zcv = zcv,
    zIQR = zIQR,
    zaad = zaad,
    zskew = zskew,
    zkurt = zkurt,
    # L moments
    zL1 = zL1,
    zL2 = zL2,
    zL3 = zL3,
    zL4 = zL4,
    zLskew = zLskew,
    zLkurt = zLkurt,
    d5 = d5,
    d10 = d10,
    d15 = d15,
    d20 = d20,
    d25 = d25,
    d30 = d30,
    d35 = d35,
    d40 = d40,
    d45 = d45,
    d50 = d50,
    d55up = d55,
    pHt1_m = percentHt1,
    pHt5_m = percentHt5,
    pHt10_m = percentHt10,
    pHt15_m = percentHt15,
    pHt20_m = percentHt20,
    pHt25_m = percentHt25,
    pHt30_m = percentHt30,
    pHt40_m = percentHt40,
    pHt50_m = percentHt50,
    pHt60_m = percentHt60,
    pHt70_m = percentHt70,
    pHt75_m = percentHt75,
    pHt80_m = percentHt80,
    pHt90_m = percentHt90,
    pHt95_m = percentHt95,
    pHt99_m = percentHt99,
    prct_10_1r = prct_10_1r,
    pcrt_20_1r = pcrt_20_1r,
    prct_1r_ab_mean = prct_1r_ab_mean,
    prct_1r_ab_mode =  prct_1r_ab_mode,
    pcrt_10 = pcrt_10,
    pcrt_20 = pcrt_20,
    prct_ab_mean = prct_ab_mean,
    prct_ab_mode = prct_ab_mode

  )

  return(metrics)
}

#' Standard tree metrics function
#'
#' This function takes the output from tree_metrics() in lidR and creates area based summaries from the segmented trees.
#' @param trees A 'SpatialPointsDataFrame' created from lidR tree_metrics()
#' @keywords lidar tree metrics
#' @export
#' @examples
#' trees <- lastrees(las.t, watershed(chm))
#' trees <- tree_metrics(trees, func = .stdtreemetrics )
#' tree_stats <- std_trees(trees)

std_trees = function(trees)
{ sf::st_as_sf(trees)
  z = trees$Z
  crown_area = trees$convhull_area
  metrics = list(ntrees = nrow(trees),
                 ntree_5 = sum(z <= 5),
                 ntree_10 = sum(z <= 10 & z > 5),
                 ntree_15 = sum(z <= 15 & z > 10),
                 ntree_20 = sum(z <= 20 & z > 15),
                 ntree_25 = sum(z <= 25 & z > 20),
                 ntree_30 = sum(z <= 30 & z > 25),
                 ntree_35 = sum(z <= 35 & z > 30),
                 ntree_40_up = sum(z > 35),
                 max_tree_z = max(z),
                 min_tree_z = min(z),
                 qmean_tree_z = sqrt(mean(z^2)),
                 med_tree_z = median(z),
                 mean_tree_z = mean(z),
                 sd_tree_z = sd(z),
                 var_tree_z = var(z),
                 cv_tree_z = sd(z)/mean(z),
                 IQR_tree_z = IQR(z),
                 aad_tree_z = stats::mad(z, center = mean(z)),
                 skew_tree_z = e1071::skewness(z),
                 kurt_tree_z = e1071::kurtosis(z),
                 L1_tree_z = Lmoments::Lmoments(z)[1],
                 L2_tree_z =Lmoments::Lmoments(z)[2],
                 L3_tree_z =Lmoments::Lmoments(z)[3],
                 L4_tree_z =Lmoments::Lmoments(z)[4],
                 Lskew_tree_z = Lmoments::Lcoefs(z)[3],
                 Lkurt_tree_z = Lmoments::Lcoefs(z)[4],
                 max_tree_crown_area = max(crown_area),
                 min_tree_crown_area = min(crown_area),
                 qmean_tree_crown_area = sqrt(mean(crown_area^2)),
                 med_tree_crown_area = median(crown_area),
                 mean_tree_crown_area = mean(crown_area),
                 sd_tree_crown_area = sd(crown_area),
                 var_tree_crown_area = var(crown_area),
                 cv_tree_crown_area = sd(crown_area)/mean(crown_area),
                 IQR_tree_crown_area = IQR(crown_area),
                 aad_tree_crown_area = stats::mad(crown_area, center = mean(crown_area)),
                 skew_tree_crown_area = e1071::skewness(crown_area),
                 kurt_tree_crown_area = e1071::kurtosis(crown_area),
                 L1_tree_crown_area = Lmoments::Lmoments(crown_area)[1],
                 L2_tree_crown_area =Lmoments::Lmoments(crown_area)[2],
                 L3_tree_crown_area =Lmoments::Lmoments(crown_area)[3],
                 L4_tree_crown_area =Lmoments::Lmoments(crown_area)[4],
                 Lskew_tree_crown_area = Lmoments::Lcoefs(crown_area)[3],
                 Lkurt_tree_crown_area = Lmoments::Lcoefs(crown_area)[4]
  )
  null_list <- lapply(metrics,is.null)
  metrics[do.call(rbind,null_list)] <- NA
  return(as.data.frame(metrics))
}


#' Voxel metrics function
#'
#' This function takes the intensity and height values for each lidar point and
#' is used within lidR::voxel_metrics() which is used within lidRmts::std_voxel()
#' @param z height
#' @param i intensity
#' @keywords lidar voxel metrics
#' @export
#' @examples
#' voxels <- voxel_metrics(las, func = vox_mt(Z, as.numeric(Intensity)), res = resolution)

vox_mt <- function(z, i)
{
  metrics =list(
    SVi = length(z), # number of points in a voxel (notation from Pearse et al. 2019)
    med_z_vox = median(z),
    mean_z_vox = mean(z),
    var_z_vox = var(z),
    sd_z_vox = sd(z),
    cv_z_vox = sd(z)/mean(z),
    IQR_z_vox = IQR(z),
    skew_z_vox = e1071::skewness(z),
    kurt_z_vox = e1071::kurtosis(z),
    med_i_vox = median(i),
    mean_i_vox = mean(i),
    var_i_vox = var(i),
    sd_i_vox = sd(i),
    cv_i_vox = sd(i)/mean(i),
    IQR_i_vox = IQR(i),
    skew_i_vox = e1071::skewness(i),
    kurt_i_vox = e1071::kurtosis(i),
    Z_list = list(z),
    I_list = list(as.numeric(i))
  )
  return(metrics)
}

#' Voxel metrics for individual voxels
#'
#' This function provides a variety of voxel based metrics based off similar metrics in
#' Pearse et al. 2019 and Kim et al. 2016. The function also includes
#' additional summary statistics for height and intensity values within each voxel
#' @param las an las file (made for small, 0.01 ha, areas).
#' @param resolution the square dimensions of voxels.
#' @param vox_ht  max height of voxels on plot. This is best to be used as the max height of the trees wihtin plots
#' @param vox_x is a combination of min and max x of plot. Default is  c(min(vox$X), max(vox$X))
#' @param vox_y is a combination of min and max x of plot. Default is  c(min(vox$Y), max(vox$Y))
#' @keywords lidar voxel metrics
#' @import data.table
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' std_voxel()


std_voxel_all <- function(las, resolution, vox_ht = max(vox$Z),
                          vox_x = c(min(vox$X), max(vox$X)), vox_y = c(min(vox$Y), max(vox$Y)),
                          sf_poly){
  vox <- lidR::voxel_metrics(las, func = vox_mt(Z, as.numeric(Intensity)), res = 1)

  # create all possible voxels
  x = seq(vox_x[1], vox_x[2], resolution)
  y = seq(vox_y[1], vox_y[2], resolution)
  z = seq(min(vox$Z), vox_ht, resolution)
  all_vox = expand.grid(X = x, Y = y, Z = z)
  data.table::setDT(all_vox)

  # merge all and voxel_metrics() output
  fullvox = vox[all_vox, on = c("X", "Y", "Z")]

  fullvox.df <- as.data.frame(fullvox)
  full_vox.sf <- st_as_sf(fullvox.df, coords = c('X', 'Y'), crs = st_crs(sf_poly))
  fullvox.crop <- st_intersection(sf_poly, full_vox.sf)
  fullvox <- cbind(X = st_coordinates(fullvox.crop)[,1], Y = st_coordinates(fullvox.crop)[,2],
        fullvox.crop[,c(which(colnames(fullvox.crop) =="Z"):ncol(fullvox.crop))])
  fullvox <- as.data.frame(fullvox)
  # give each voxel a unique id and assign 0s and NAs where appropriate
  fullvox <- fullvox %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
  fullvox$SVi[is.na(fullvox$SVi)] <- 0
  null_list <- lapply(fullvox$Z_list,is.null)
  fullvox$Z_list[do.call(rbind,null_list)] <- NA
  fullvox$I_list[do.call(rbind,null_list)] <- NA

  ##### Individual voxel metrics
  ### FR_SVi is the frequency ratio of the number of returns in a voxel in relation to total returns (in Pearse et al. 2019 this is FR_Di)
  fullvox <- fullvox %>% mutate(FR_SVi = SVi/sum(fullvox$SVi))

  ### P_Di is the number of returns below each voxel (Pearse et al. 2019; Kim et al. 2016 uses returns above)
  # create a vector for Z in each iteration
  Zi = fullvox$Z

  # for loop to calculate points below each voxel
  point_blow = list()
  for(i in 1:length(unique(fullvox$Z))){
    np_b <- fullvox %>%
      group_by(X,Y) %>%
      filter(Z < unique(Z)[i]) %>%
      summarize(npoints_below = sum(SVi), Z= unique(Zi)[i])
    point_blow[[i]] <- np_b
  }

  # extract point below data and merge using the voxel id
  point_blow_all_vox <- do.call(rbind,point_blow)
  point_blow_all_vox <- point_blow_all_vox %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
  fullvox <- fullvox %>% left_join(as.data.frame(point_blow_all_vox[,c(3,5)]), by = "id_xyz", )

  ### P_Di_above is the number of returns above each voxel (Kim et al. 2016 uses returns above as P_Di)
  # for loop to calculate points below each voxel
  point_above = list()
  for(i in 1:length(unique(fullvox$Z))){
    np_a <- fullvox %>%
      group_by(X,Y) %>%
      filter(Z > unique(Z)[i]) %>%
      summarize(npoints_above = sum(SVi, na.rm = T), Z= unique(Zi)[i])
    point_above[[i]] <- np_a
  }

  # extract point below data and merge using the voxel id
  point_above_all_vox <- do.call(rbind,point_above)
  point_above_all_vox <- point_above_all_vox %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
  fullvox <- fullvox %>% left_join(as.data.frame(point_above_all_vox[,c(3,5)]), by = "id_xyz", )
  # NAs returned for 0 points so it is appropriate to replace them with 0
  fullvox$npoints_above[is.na(fullvox$npoints_above)] <- 0

  ### FR_Di is the frequency ratio of the number of returns above a voxel in relation to total returns (Kim et al. 2016)
  fullvox <- fullvox %>% mutate(FR_Di = npoints_above/sum(fullvox$SVi))

  ### I_Di is the median intensity of returns above each voxel
  # for loop to calculate median intensity above each voxel
  i_above = list()
  for(i in 1:length(unique(fullvox$Z))){
    i_a <- fullvox %>% filter(Z > Z[i])
    i_a$I_list<- lapply( i_a$I_list, "length<-", max(lengths( i_a$I_list)))
    i_abv <- as.data.frame(do.call(rbind, i_a$I_list))

    i_a_merge <- cbind(as.data.frame(i_a), i_abv)
    i_a_merge <- cbind(X = i_a_merge$X, Y = i_a_merge$Y, i_a_merge[,ncol(i_a)+1:(ncol(i_a_merge)-ncol(i_a))])
    i_Di_mt <- i_a_merge %>% pivot_longer(cols = 3:ncol(i_a_merge)) %>%
      group_by(X,Y) %>%
      summarize(Z = unique(fullvox$Z)[i], i_Di = median(value, na.rm = T))
    i_above[[i]] <- i_Di_mt
  }

  # extract point below data and merge using the voxel id
  i_above_all_vox <- do.call(rbind,i_above)
  i_above_all_vox <- i_above_all_vox %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
  fullvox <- fullvox %>% left_join(as.data.frame(i_above_all_vox[,c(4,5)]), by = "id_xyz" )

  return(fullvox)
}


#' Standard voxel metrics function
#'
#' This function provides a variety of voxel based metrics based off similar metrics in
#' Pearse et al. 2019 and Kim et al. 2016. The function also includes
#' additional summary statistics for height and intensity values within each voxel
#' @param las an las file (made for small, 0.01 ha, areas).
#' @param resolution the size of voxels wanted for a majority of metrics unless defined by
#' different height bins in Pearse et al. 2019.
#' @keywords lidar voxel metrics
#' @import data.table
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' std_voxel()



std_voxel <- function(las, resolution){
  #
  vox <- lidR::voxel_metrics(las, func = vox_mt(Z, as.numeric(Intensity)), res = resolution)

  # create all possible voxels
  x = seq(min(vox$X), max(vox$X), resolution)
  y = seq(min(vox$Y), max(vox$Y), resolution)
  z = seq(min(vox$Z), max(vox$Z), resolution)
  all_vox = expand.grid(X = x, Y = y, Z = z)
  data.table::setDT(all_vox)

  # merge all and voxel_metrics() output
  fullvox = vox[all_vox, on = c("X", "Y", "Z")]

  # give each voxel a unique id and assign 0s and NAs where appropriate
  fullvox <- fullvox %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
  fullvox$SVi[is.na(fullvox$SVi)] <- 0
  null_list <- lapply(fullvox$Z_list,is.null)
  fullvox$Z_list[do.call(rbind,null_list)] <- NA
  fullvox$I_list[do.call(rbind,null_list)] <- NA

  ##### Individual voxel metrics
  ### FR_SVi is the frequency ratio of the number of returns in a voxel in relation to total returns (in Pearse et al. 2019 this is FR_Di)
  fullvox <- fullvox %>% mutate(FR_SVi = SVi/sum(fullvox$SVi))

  ### P_Di is the number of returns below each voxel (Pearse et al. 2019; Kim et al. 2016 uses returns above)
  # create a vector for Z in each iteration
  Zi = fullvox$Z

  # for loop to calculate points below each voxel
  point_blow = list()
  for(i in 1:length(unique(fullvox$Z))){
    np_b <- fullvox %>%
      group_by(X,Y) %>%
      filter(Z < unique(Z)[i]) %>%
      summarize(npoints_below = sum(SVi), Z= unique(Zi)[i])
    point_blow[[i]] <- np_b
  }

  # extract point below data and merge using the voxel id
  point_blow_all_vox <- do.call(rbind,point_blow)
  point_blow_all_vox <- point_blow_all_vox %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
  fullvox <- fullvox %>% left_join(as.data.frame(point_blow_all_vox[,c(3,5)]), by = "id_xyz", )

  ### P_Di_above is the number of returns above each voxel (Kim et al. 2016 uses returns above as P_Di)
  # for loop to calculate points below each voxel
  point_above = list()
  for(i in 1:length(unique(fullvox$Z))){
    np_a <- fullvox %>%
      group_by(X,Y) %>%
      filter(Z > unique(Z)[i]) %>%
      summarize(npoints_above = sum(SVi, na.rm = T), Z= unique(Zi)[i])
    point_above[[i]] <- np_a
  }

  # extract point below data and merge using the voxel id
  point_above_all_vox <- do.call(rbind,point_above)
  point_above_all_vox <- point_above_all_vox %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
  fullvox <- fullvox %>% left_join(as.data.frame(point_above_all_vox[,c(3,5)]), by = "id_xyz", )
  # NAs returned for 0 points so it is appropriate to replace them with 0
  fullvox$npoints_above[is.na(fullvox$npoints_above)] <- 0

  ### FR_Di is the frequency ratio of the number of returns above a voxel in relation to total returns (Kim et al. 2016)
  fullvox <- fullvox %>% mutate(FR_Di = npoints_above/sum(fullvox$SVi))

  ### I_Di is the median intensity of returns above each voxel
  # for loop to calculate median intensity above each voxel
  i_above = list()
  for(i in 1:length(unique(fullvox$Z))){
    i_a <- fullvox %>% filter(Z > Z[i])
    i_a$I_list<- lapply( i_a$I_list, "length<-", max(lengths( i_a$I_list)))
    i_abv <- as.data.frame(do.call(rbind, i_a$I_list))

    i_a_merge <- cbind(as.data.frame(i_a), i_abv)
    i_a_merge <- cbind(X = i_a_merge$X, Y = i_a_merge$Y, i_a_merge[,ncol(i_a)+1:(ncol(i_a_merge)-ncol(i_a))])
    i_Di_mt <- i_a_merge %>% pivot_longer(cols = 3:ncol(i_a_merge)) %>%
      group_by(X,Y) %>%
      summarize(Z = unique(fullvox$Z)[i], i_Di = median(value, na.rm = T))
    i_above[[i]] <- i_Di_mt
  }

  # extract point below data and merge using the voxel id
  i_above_all_vox <- do.call(rbind,i_above)
  i_above_all_vox <- i_above_all_vox %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
  fullvox <- fullvox %>% left_join(as.data.frame(i_above_all_vox[,c(4,5)]), by = "id_xyz" )
  ##### Summarized voxel metrics
  ### Start by summarizing individual voxel metrics
  voxel_summ <- fullvox %>% summarise(z_med_med = median(med_z_vox, na.rm = T),
                                      z_med_mean = mean(med_z_vox, na.rm = T),
                                      z_med_var = var(med_z_vox, na.rm = T),
                                      z_med_sd = sd(med_z_vox, na.rm = T),
                                      z_med_cv = sd(med_z_vox, na.rm = T)/mean(med_z_vox, na.rm = T),
                                      z_med_IQR = IQR(med_z_vox, na.rm = T),
                                      z_med_skew = e1071::skewness(med_z_vox, na.rm = T),
                                      z_med_kurt = e1071::kurtosis(med_z_vox, na.rm = T),
                                      z_mean_med = median(mean_z_vox, na.rm = T),
                                      z_mean_mean = mean(mean_z_vox, na.rm = T),
                                      z_mean_var = var(mean_z_vox, na.rm = T),
                                      z_mean_sd = sd(mean_z_vox, na.rm = T),
                                      z_mean_cv = sd(mean_z_vox, na.rm = T)/mean(mean_z_vox, na.rm = T),
                                      z_mean_IQR = IQR(mean_z_vox, na.rm = T),
                                      z_mean_skew = e1071::skewness(mean_z_vox, na.rm = T),
                                      z_mean_kurt = e1071::kurtosis(mean_z_vox, na.rm = T),
                                      z_var_med = median(var_z_vox, na.rm = T),
                                      z_var_mean = mean(var_z_vox, na.rm = T),
                                      z_var_var = var(var_z_vox, na.rm = T),
                                      z_var_sd = sd(var_z_vox, na.rm = T),
                                      z_var_cv = sd(var_z_vox, na.rm = T)/mean(var_z_vox, na.rm = T),
                                      z_var_IQR = IQR(var_z_vox, na.rm = T),
                                      z_var_skew = e1071::skewness(var_z_vox, na.rm = T),
                                      z_var_kurt = e1071::kurtosis(var_z_vox, na.rm = T),
                                      z_sd_med = median(sd_z_vox, na.rm = T),
                                      z_sd_mean = mean(sd_z_vox, na.rm = T),
                                      z_sd_var = var(sd_z_vox, na.rm = T),
                                      z_sd_sd = sd(sd_z_vox, na.rm = T),
                                      z_sd_cv = sd(sd_z_vox, na.rm = T)/mean(sd_z_vox, na.rm = T),
                                      z_sd_IQR = IQR(sd_z_vox, na.rm = T),
                                      z_sd_skew = e1071::skewness(sd_z_vox, na.rm = T),
                                      z_sd_kurt = e1071::kurtosis(sd_z_vox, na.rm = T),
                                      z_cv_med = median(cv_z_vox, na.rm = T),
                                      z_cv_mean = mean(cv_z_vox, na.rm = T),
                                      z_cv_var = var(cv_z_vox, na.rm = T),
                                      z_cv_sd = sd(cv_z_vox, na.rm = T),
                                      z_cv_cv = sd(cv_z_vox, na.rm = T)/mean(cv_z_vox, na.rm = T),
                                      z_cv_IQR = IQR(cv_z_vox, na.rm = T),
                                      z_cv_skew = e1071::skewness(cv_z_vox, na.rm = T),
                                      z_cv_kurt = e1071::kurtosis(cv_z_vox, na.rm = T),
                                      z_IQR_med = median(IQR_z_vox, na.rm = T),
                                      z_IQR_mean = mean(IQR_z_vox, na.rm = T),
                                      z_IQR_var = var(IQR_z_vox, na.rm = T),
                                      z_IQR_sd = sd(IQR_z_vox, na.rm = T),
                                      z_IQR_cv = sd(IQR_z_vox, na.rm = T)/mean(IQR_z_vox, na.rm = T),
                                      z_IQR_IQR = IQR(IQR_z_vox, na.rm = T),
                                      z_IQR_skew = e1071::skewness(IQR_z_vox, na.rm = T),
                                      z_IQR_kurt = e1071::kurtosis(IQR_z_vox, na.rm = T),
                                      z_skew_med = median(skew_z_vox, na.rm = T),
                                      z_skew_mean = mean(skew_z_vox, na.rm = T),
                                      z_skew_var = var(skew_z_vox, na.rm = T),
                                      z_skew_sd = sd(skew_z_vox, na.rm = T),
                                      z_skew_cv = sd(skew_z_vox, na.rm = T)/mean(skew_z_vox, na.rm = T),
                                      z_skew_IQR = IQR(skew_z_vox, na.rm = T),
                                      z_skew_skew = e1071::skewness(skew_z_vox, na.rm = T),
                                      z_skew_kurt = e1071::kurtosis(skew_z_vox, na.rm = T),
                                      z_kurt_med = median(kurt_z_vox, na.rm = T),
                                      z_kurt_mean = mean(kurt_z_vox, na.rm = T),
                                      z_kurt_var = var(kurt_z_vox, na.rm = T),
                                      z_kurt_sd = sd(kurt_z_vox, na.rm = T),
                                      z_kurt_cv = sd(kurt_z_vox, na.rm = T)/mean(kurt_z_vox, na.rm = T),
                                      z_kurt_IQR = IQR(kurt_z_vox, na.rm = T),
                                      z_kurt_skew = e1071::skewness(kurt_z_vox, na.rm = T),
                                      z_kurt_kurt = e1071::kurtosis(kurt_z_vox, na.rm = T),
                                      i_vox_med = median(med_i_vox, na.rm = T),
                                      i_med_mean = mean(med_i_vox, na.rm = T),
                                      i_med_var = var(med_i_vox, na.rm = T),
                                      i_med_sd = sd(med_i_vox, na.rm = T),
                                      i_med_cv = sd(med_i_vox, na.rm = T)/mean(med_i_vox, na.rm = T),
                                      i_med_IQR = IQR(med_i_vox, na.rm = T),
                                      i_med_skew = e1071::skewness(med_i_vox, na.rm = T),
                                      i_med_kurt = e1071::kurtosis(med_i_vox, na.rm = T),
                                      i_mean_med = median(mean_i_vox, na.rm = T),
                                      i_mean_mean = mean(mean_i_vox, na.rm = T),
                                      i_mean_var = var(mean_i_vox, na.rm = T),
                                      i_mean_sd = sd(mean_i_vox, na.rm = T),
                                      i_mean_cv = sd(mean_i_vox, na.rm = T)/mean(mean_i_vox, na.rm = T),
                                      i_mean_IQR = IQR(mean_i_vox, na.rm = T),
                                      i_mean_skew = e1071::skewness(mean_i_vox, na.rm = T),
                                      i_mean_kurt = e1071::kurtosis(mean_i_vox, na.rm = T),
                                      i_var_med = median(var_i_vox, na.rm = T),
                                      i_var_mean = mean(var_i_vox, na.rm = T),
                                      i_var_var = var(var_i_vox, na.rm = T),
                                      i_var_sd = sd(var_i_vox, na.rm = T),
                                      i_var_cv = sd(var_i_vox, na.rm = T)/mean(var_i_vox, na.rm = T),
                                      i_var_IQR = IQR(var_i_vox, na.rm = T),
                                      i_var_skew = e1071::skewness(var_i_vox, na.rm = T),
                                      i_var_kurt = e1071::kurtosis(var_i_vox, na.rm = T),
                                      i_sd_med = median(sd_i_vox, na.rm = T),
                                      i_sd_mean = mean(sd_i_vox, na.rm = T),
                                      i_sd_var = var(sd_i_vox, na.rm = T),
                                      i_sd_sd = sd(sd_i_vox, na.rm = T),
                                      i_sd_cv = sd(sd_i_vox, na.rm = T)/mean(sd_i_vox, na.rm = T),
                                      i_sd_IQR = IQR(sd_i_vox, na.rm = T),
                                      i_sd_skew = e1071::skewness(sd_i_vox, na.rm = T),
                                      i_sd_kurt = e1071::kurtosis(sd_i_vox, na.rm = T),
                                      i_cv_med = median(cv_i_vox, na.rm = T),
                                      i_cv_mean = mean(cv_i_vox, na.rm = T),
                                      i_cv_var = var(cv_i_vox, na.rm = T),
                                      i_cv_sd = sd(cv_i_vox, na.rm = T),
                                      i_cv_cv = sd(cv_i_vox, na.rm = T)/mean(cv_i_vox, na.rm = T),
                                      i_cv_IQR = IQR(cv_i_vox, na.rm = T),
                                      i_cv_skew = e1071::skewness(cv_i_vox, na.rm = T),
                                      i_cv_kurt = e1071::kurtosis(cv_i_vox, na.rm = T),
                                      i_IQR_med = median(IQR_i_vox, na.rm = T),
                                      i_IQR_mean = mean(IQR_i_vox, na.rm = T),
                                      i_IQR_var = var(IQR_i_vox, na.rm = T),
                                      i_IQR_sd = sd(IQR_i_vox, na.rm = T),
                                      i_IQR_cv = sd(IQR_i_vox, na.rm = T)/mean(IQR_i_vox, na.rm = T),
                                      i_IQR_IQR = IQR(IQR_i_vox, na.rm = T),
                                      i_IQR_skew = e1071::skewness(IQR_i_vox, na.rm = T),
                                      i_IQR_kurt = e1071::kurtosis(IQR_i_vox, na.rm = T),
                                      i_skew_med = median(skew_i_vox, na.rm = T),
                                      i_skew_mean = mean(skew_i_vox, na.rm = T),
                                      i_skew_var = var(skew_i_vox, na.rm = T),
                                      i_skew_sd = sd(skew_i_vox, na.rm = T),
                                      i_skew_cv = sd(skew_i_vox, na.rm = T)/mean(skew_i_vox, na.rm = T),
                                      i_skew_IQR = IQR(skew_i_vox, na.rm = T),
                                      i_skew_skew = e1071::skewness(skew_i_vox, na.rm = T),
                                      i_skew_kurt = e1071::kurtosis(skew_i_vox, na.rm = T),
                                      i_kurt_med = median(kurt_i_vox, na.rm = T),
                                      i_kurt_mean = mean(kurt_i_vox, na.rm = T),
                                      i_kurt_var = var(kurt_i_vox, na.rm = T),
                                      i_kurt_sd = sd(kurt_i_vox, na.rm = T),
                                      i_kurt_cv = sd(kurt_i_vox, na.rm = T)/mean(kurt_i_vox, na.rm = T),
                                      i_kurt_IQR = IQR(kurt_i_vox, na.rm = T),
                                      i_kurt_skew = e1071::skewness(kurt_i_vox, na.rm = T),
                                      i_kurt_kurt = e1071::kurtosis(kurt_i_vox, na.rm = T),
                                      P_Di_med = median(npoints_below, na.rm = T),
                                      P_Di_mean = mean(npoints_below, na.rm = T),
                                      P_Di_var = var(npoints_below, na.rm = T),
                                      P_Di_sd = sd(npoints_below, na.rm = T),
                                      P_Di_cv = sd(npoints_below, na.rm = T)/mean(npoints_below, na.rm = T),
                                      P_Di_IQR = IQR(npoints_below, na.rm = T),
                                      P_Di_skew = e1071::skewness(npoints_below, na.rm = T),
                                      P_Di_kurt = e1071::kurtosis(npoints_below, na.rm = T),
                                      npoints_above_med = median(npoints_above, na.rm = T),
                                      npoints_above_mean = mean(npoints_above, na.rm = T),
                                      npoints_above_var = var(npoints_above, na.rm = T),
                                      npoints_above_sd = sd(npoints_above, na.rm = T),
                                      npoints_above_cv = sd(npoints_above, na.rm = T)/mean(npoints_above, na.rm = T),
                                      npoints_above_IQR = IQR(npoints_above, na.rm = T),
                                      npoints_above_skew = e1071::skewness(npoints_above, na.rm = T),
                                      npoints_above_kurt = e1071::kurtosis(npoints_above, na.rm = T),
                                      FR_Di_med = median(FR_Di, na.rm = T),
                                      FR_Di_mean = mean(FR_Di, na.rm = T),
                                      FR_Di_var = var(FR_Di, na.rm = T),
                                      FR_Di_sd = sd(FR_Di, na.rm = T),
                                      FR_Di_cv = sd(FR_Di, na.rm = T)/mean(FR_Di, na.rm = T),
                                      FR_Di_IQR = IQR(FR_Di, na.rm = T),
                                      FR_Di_skew = e1071::skewness(FR_Di, na.rm = T),
                                      FR_Di_kurt = e1071::kurtosis(FR_Di, na.rm = T),
                                      i_Di_med = median(i_Di, na.rm = T),
                                      i_Di_mean = mean(i_Di, na.rm = T),
                                      i_Di_var = var(i_Di, na.rm = T),
                                      i_Di_sd = sd(i_Di, na.rm = T),
                                      i_Di_cv = sd(i_Di, na.rm = T)/mean(i_Di, na.rm = T),
                                      i_Di_IQR = IQR(i_Di, na.rm = T),
                                      i_Di_skew = e1071::skewness(i_Di, na.rm = T),
                                      i_Di_kurt = e1071::kurtosis(i_Di, na.rm = T)


  )


  ### SVM_med is the median height of all points within maximum density subvoxel (i.e., voxel) or each X,Y

  # find the median z for the voxels with highest number of points (SVi) for each
  SVM <- fullvox %>% group_by(X,Y) %>%
    summarize(Z = Z[which.max(SVi)], SVM_density = max(SVi), median_Z = med_z_vox[which.max(SVi)])

  ### SVM_above is distribution summary statistics of the height of all points above maximum density subvoxel (i.e., voxel) or each X,Y
  ### Pearse et al. (2019) used  only the median

  # for loop that identifies the SVM and calculates summary statistics for points above
  SVM_list = list()
  for(i in 1:nrow(SVM)){
    vox_above <- fullvox %>% filter(Z > SVM$Z[i] & X ==SVM$X[i] & Y == SVM$Y[i])
    vox_above$Z_list<- lapply(vox_above$Z_list, "length<-", max(lengths(vox_above$Z_list)))
    hts_above <- as.numeric(do.call(cbind,vox_above$Z_list))
    SVM_all_mts <- data.frame(SVM_med = median(hts_above, na.rm = T))
    SVM_all_mts <- cbind(SVM[i,], median_Z_above = median(hts_above, na.rm = T),
                         mean_Z_above = mean(hts_above, na.rm = T),
                         sd_Z_above = sd(hts_above, na.rm = T),
                         var_Z_above = sd(hts_above, na.rm = T)^2,
                         cv_Z_above = sd(hts_above, na.rm = T)/ mean(hts_above, na.rm = T),
                         IQR_Z_above = IQR(hts_above, na.rm = T),
                         skew_Z_above = e1071::skewness(hts_above, na.rm = T),
                         kurt_Z_above = e1071::kurtosis(hts_above, na.rm = T))
    SVM_list[[i]] <- SVM_all_mts
  }

  # extract calculated metrics
  SVM_all <- do.call(rbind, SVM_list)
  SVM_all <- as.data.frame(SVM_all)

  # create averages fro each metric (e.g., sd, variance, skewness)
  SVM_summ <- SVM_all %>% ungroup() %>%
    summarise(SVM_mean = mean(mean_Z_above, na.rm = T), SVM_median = mean(median_Z_above, na.rm = T),
              SVM_sd = mean(sd_Z_above, na.rm = T), SVM_var = mean(var_Z_above, na.rm = T),
              SVM_cv = mean(cv_Z_above, na.rm = T), SVM_cv = mean(cv_Z_above, na.rm = T),
              SVM_IQR = mean(IQR_Z_above, na.rm = T), SVM_skew = mean(skew_Z_above, na.rm = T),
              SVM_kurt = mean(kurt_Z_above, na.rm = T))


  voxel_summ <- cbind(voxel_summ, SVM_summ)



  ### ENL is the effective number of layers done and can be calculated in three ways

  # create column indicating if voxel has returns (1) or does not (0)
  fullvox$empty <- ifelse(fullvox$SVi == 0, 0,1)

  # for loop to calculate the proportion of empty voxels in each height bin (voxel height)
  layer_prop = list()
  for(i in 1:length(unique(fullvox$Z))){
    p <- fullvox %>% filter(Z == unique(Z)[i]) %>% summarize(p0 = sum(empty)/sum(fullvox$empty))
    layer_prop[[i]] <- p
  }
  p <- do.call(rbind,layer_prop)

  # calculate the different diversity indicies similar to Ehbrecht et al. (2016) and Pearse et al. (2019)
  p <-  p %>% mutate(pln = p0*log(p0))
  p <- p %>% mutate(p2 = p0^2)
  ENL_d0 <- sum(p$p0^0)
  ENL_d1 <- exp(-sum(p$pln)) ## exponential Shannon-Index
  ENL_d2 <- 1/sum(p$p2) ## inverse Simpson-Index

  # add indicies to voxel_summ df
  voxel_summ <- cbind(voxel_summ, ENL_d0, ENL_d1, ENL_d2)

  ### Sub-pixel canopy closure above (cc_above) is the measure of canopy closure

  cc_list = list()

  thr_above = seq(from = 3, to = 21, by = 3)

  for(i in 1:length(thr_above)){
    cc_above <- fullvox %>% filter(if(thr_above[i] < max(thr_above)) {Z <= thr_above[i]} else(Z <= max(Z))) %>% summarize(cc = sum(empty)/n())
    cc_above$thr_above <- paste0("cc_above_thr_", thr_above[i])
    cc_list[[i]] <- cc_above
  }

  # extract canopy closure and prepare to merge with voxel_summ
  cc_above <- do.call(rbind, cc_list)
  cc_above_df <- as.data.frame(t(cc_above[,1]))
  colnames(cc_above_df) <- cc_above[,2]

  # add closure metrics to voxel_summ df
  voxel_summ <- cbind(voxel_summ, cc_above_df)


  ### P_cc is the mean canopy closure and is calculated by the ratio of height bin point density to overall point density


  pcc_list = list()

  ht_bin = c(1, 5, 10, 15, 20)

  npoints_XY <- fullvox %>% group_by(X,Y) %>% summarize(npoints = sum(SVi, na.rm = T))

  for(i in 1:length(ht_bin)){
    point_density <- fullvox %>%
      filter(if(ht_bin[i] < max(ht_bin)) { Z >= ht_bin[i] & Z < ht_bin[i+1]}  else(Z >= max(ht_bin))) %>%
      group_by(X,Y) %>%
      summarize(pt_density = sum(SVi))
    if(nrow(point_density) == 0){next}
    point_density$pcc = point_density$pt_density/ npoints_XY$npoints
    point_density$ht_bin <- ht_bin[i]
    pcc_list[[i]] <- point_density
  }

  pcc_bins <- do.call(rbind, pcc_list)
  pcc_bins$pcc[is.na(pcc_bins$pcc)] <- 0
  pcc_XY <- pcc_bins %>% group_by(X,Y) %>% summarise(pcc = sum(pcc))

  pcc <- mean(pcc_XY$pcc)

  voxel_summ <- cbind(voxel_summ, pcc = pcc)

  ### VCI is a measure of the eveness or simulatiry of point densitys
  ### of height bins within a given las based on Shannon diversity inde

  vci_list = list()
  vci_bin <- c(.5,1,2,3,4,5)
  for(i in 1:length(vci_bin)){
    res <- vci_bin[i]
    vox_vci <- voxel_metrics(las, func = vox_mt(Z, as.numeric(Intensity)), res = res)
    # Generate all the possible voxels
    x = seq(min(vox_vci$X), max(vox_vci$X), res)
    y = seq(min(vox_vci$Y), max(vox_vci$Y), res)
    z = seq(min(vox_vci$Z), max(vox_vci$Z), res)
    all_vox_vci = expand.grid(X = x, Y = y, Z = z)
    data.table::setDT(all_vox_vci)

    # Merge both unsing a join
    fullvox_vci = vox_vci[all_vox_vci, on = c("X", "Y", "Z")]
    fullvox_vci <- fullvox_vci %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
    fullvox_vci$SVi[is.na(fullvox_vci$SVi)] <- 0
    npoints_XY_vci <- fullvox_vci %>% group_by(X,Y) %>% summarize(npoints = sum(SVi), na.rm = T)
    npoints_XY_vci$npoints[is.na(npoints_XY_vci$npoints)] <- 0

    # for loop to calculate VCI based on Shannon diversity inde
    layer_vci = list()
    for(j in 1:length(unique(fullvox_vci$Z))){
      p <- fullvox_vci %>%
        filter(Z == unique(Z)[j]) %>%
        summarize(p0 = sum(SVi)/sum(npoints_XY_vci$npoints))
      p$layer <- unique(fullvox_vci$Z)[j]
      layer_vci[[j]] <- p
    }
    p <- do.call(rbind,layer_vci)
    p$pxlog <- p$p0*log(p$p0)
    p$pxlog[is.na(p$pxlog)] <- 0
    vci_i <- data.frame(vci = (-sum(p$pxlog))/log(nrow(p)), bin = vci_bin[i])
    vci_list[[i]] <- vci_i
  }

  vcis <- do.call(rbind, vci_list)
  vci_all <- as.data.frame(t(vcis[,1]))
  colnames(vci_all) <- paste0("HB-",vcis[,2])

  # add to voxel summary df
  voxel_summ <- cbind(voxel_summ, vci_all)

  return(voxel_summ)
}
