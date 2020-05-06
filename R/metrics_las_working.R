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

std_cloud <- function(x, y, z, i, ReturnNumber)
{
  # number of returns
  npoints = length(z)
  RN = unique(ReturnNumber)
  per_RN = lapply(RN, function(x) length(ReturnNumber[ReturnNumber == x])/ length(z))
  names(per_RN) = paste0("per_RN_", 1:length(RN))
  # height statistics
  zmax = max(z)
  zmin = min(z)
  uniqz = unique(z)
  zmode = uniqz[which.max(tabulate(match(z, uniqz)))]
  zmean = mean(z)
  zqmean = sqrt(mean(z^2))
  zsd = sd(z)
  zvar = var(z)
  zcv = sd(z)/mean(z)
  zIQR = IQR(z)
  zaad = stats::mad(z, center = mean(z))
  zskew = (sum((z - zmean)^3)/npoints)/(sum((z - zmean)^2)/npoints)^(3/2)
  zkurt = npoints*sum((z - zmean)^4)/(sum((z - zmean)^2)^2)
  zentropy = lidR::entropy(z)
  # L moments
  zL1 = Lmoments::Lmoments(z)[1]
  zL2 =Lmoments::Lmoments(z)[2]
  zL3 =Lmoments::Lmoments(z)[3]
  zL4 =Lmoments::Lmoments(z)[4]
  zLskew = Lmoments::Lcoefs(z)[3]
  zLkurt = Lmoments::Lcoefs(z)[4]
  # height quantiles
  zquantiles = as.list(quantile(z, probs = c(.01,seq(0.05, 0.95, 0.05), .99)))
  names(zquantiles) = paste0("qHt_",substr(names(zquantiles),1,nchar(names(zquantiles))-1))
  # Percent returns at height bins
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
  # Deciles and cumulative deciles
  if (zmax <= 0)
  {
    d = rep(0, 9)
  }
  else
  {
    breaks = seq(0, zmax, zmax/10)
    d = findInterval(z, breaks)
    d = table(d)[1:10]
    d = d / sum(d)*100
    d = as.list(d[1:9])
  }
  names(d) = paste0("decile", 1:9)
  dcum = cumsum(d)
  names(dcum) = paste0("dcum", 1:9)
  # canopy related metrics
  prct_2_1r = sum(z >= 2 & ReturnNumber == 1)/ length(z)
  prct_10_1r = sum(z >= 10 & ReturnNumber == 1)/ length(z)
  pcrt_20_1r = sum(z >= 20 & ReturnNumber == 1)/ length(z)
  prct_1r_ab_mean = sum(z >= zmean & ReturnNumber == 1)/ length(z)
  prct_1r_ab_mode = sum(z >= zmode & ReturnNumber == 1)/ length(z)
  pcrt_2 = sum(z >= 2)/ length(z)
  pcrt_10 = sum(z >= 10)/ length(z)
  pcrt_20 = sum(z >= 20)/ length(z)
  prct_ab_mean = sum(z >= zmean)/ length(z)
  prct_ab_mode = sum( z >= zmode)/ length(z)
  # Intensity related metrics
  itotal = sum(i)
  imax = max(i)
  imin = min(i)
  uniqi = unique(i)
  imode = uniqi[which.max(tabulate(match(i, uniqi)))]
  imean = mean(i)
  iqmean = sqrt(mean(i^2))
  isd = sd(i)
  ivar = var(i)
  icv = sd(i)/mean(i)
  iIQR = IQR(i)
  iaad = stats::mad(i, center = mean(i))
  iskew = (sum((i - imean)^3)/npoints)/(sum((i - imean)^2)/npoints)^(3/2)
  ikurt = npoints*sum((i - imean)^4)/(sum((i - imean)^2)^2)
  ientropy = lidR::entropy(i)

  # Intesity cumulative quantiles
  icum = lapply(zquantiles, function(x) (sum(i[z <= x])/itotal)*100)
  names(icum) = paste0("icum_", names(icum))


  # output
  metrics = list(
    npts = npoints,
    zmax = zmax,
    zmin = zmin,
    zmode = zmode,
    zmean = zmean,
    zqmean = zqmean,
    zsd = zsd,
    zvar = zvar,
    zcv = zcv,
    zIQR = zIQR,
    zaad = zaad,
    zskew = zskew,
    zkurt = zkurt,
    zentropy = zentropy,
    # L moments
    zL1 = zL1,
    zL2 = zL2,
    zL3 = zL3,
    zL4 = zL4,
    zLskew = zLskew,
    zLkurt = zLkurt,
    itotal = itotal,
    imax = imax,
    imin = imin,
    imode = imode,
    imean = imean,
    iqmean = iqmean,
    isd = isd,
    ivar = ivar,
    icv = icv,
    iIQR = iIQR,
    iaad = iaad,
    iskew = iskew,
    ikurt = ikurt,
    ientropy = ientropy,
    pHtBin_5 = d5,
    pHtBin_10 = d10,
    pHtBin_15 = d15,
    pHtBin_20 = d20,
    pHtBin_25 = d25,
    pHtBin_30 = d30,
    pHtBin_35 = d35,
    pHtBin_40 = d40,
    pHtBin_45 = d45,
    pHtBin_50 = d50,
    pHtBin_55up = d55,
    p2_1r =  prct_2_1r,
    p10_1r = prct_10_1r,
    p20_1r = pcrt_20_1r,
    pab_mean_1r = prct_1r_ab_mean,
    pab_mode_1r =  prct_1r_ab_mode,
    p2 =  pcrt_2,
    p10 = pcrt_10,
    p20 = pcrt_20,
    pab_mean = prct_ab_mean,
    pab_mode = prct_ab_mode

  )

  return(c(metrics, per_RN, zquantiles, d, dcum, icum))
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

std_trees = function(trees){
  sf::st_as_sf(trees)
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
                 skew_tree_z = (sum((z -  mean(z)^3)/nrow(trees)))/(sum((z - mean(z))^2)/nrow(trees))^(3/2),
                 kurt_tree_z = nrow(trees)*sum((z - mean(z))^4)/(sum((z - mean(z))^2)^2),
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
                 skew_tree_crown_area = (sum((crown_area - mean(crown_area)^3)/nrow(trees)))/(sum((crown_area - mean(crown_area))^2)/nrow(trees))^(3/2),
                 kurt_tree_crown_area = nrow(trees)*sum((crown_area - mean(crown_area))^4)/(sum((crown_area- mean(crown_area))^2)^2),
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
    skew_z_vox = (sum((z - mean(z))^3)/length(z))/(sum((z - mean(z))^2)/length(z))^(3/2),
    kurt_z_vox = length(z)*sum((z - mean(z))^4)/(sum((z- mean(z))^2)^2),
    med_i_vox = median(i),
    mean_i_vox = mean(i),
    var_i_vox = var(i),
    sd_i_vox = sd(i),
    cv_i_vox = sd(i)/mean(i),
    IQR_i_vox = IQR(i),
    skew_i_vox = (sum((i - mean(i))^3)/length(z))/(sum((i - mean(i))^2)/length(z))^(3/2),
    kurt_i_vox = length(z)*sum((i - mean(i))^4)/(sum((i- mean(i))^2)^2),
    Z_list = list(z),
    I_list = list(as.numeric(i))
  )
  return(metrics)
}

#' Standard voxel metrics function
#'
#' This function provides a variety of voxel based metrics based off similar metrics in
#' Pearse et al. 2019 and Kim et al. 2016. The function also includes
#' additional summary statistics for height and intensity values within each voxel
#' @param las an las file (made for small, 0.01 ha, areas)
#' @param resolution the size of voxels wanted for a majority of metrics unless defined by
#' @param sf_poly sf polygon to clip voxels to. Required.
#' different height bins in Pearse et al. 2019.
#' @keywords lidar voxel metrics
#' @import tidyr
#' @import dplyr
#' @import data.table
#' @export
#' @examples
#' std_voxel()

std_voxel <- function(las, resolution, sf_poly){
  vox <- lidR::voxel_metrics(las, func = vox_mt(Z, as.numeric(Intensity)), res = resolution)

  # create all possible voxels
  x = seq(min(vox$X), max(vox$X), resolution)
  y = seq(min(vox$Y), max(vox$Y), resolution)
  z = seq(min(vox$Z), max(vox$Z), resolution)
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
  fullvox$I_list<- lapply( fullvox$I_list, "length<-", max(lengths( fullvox$I_list)))
  fullvox$Z_list<- lapply(fullvox$Z_list, "length<-", max(lengths(fullvox$Z_list)))
  ##### Individual voxel metrics
  ### FR_SVi is the frequency ratio of the number of returns in a voxel in relation to total returns (in Pearse et al. 2019 this is FR_Di)
  fullvox <- fullvox %>% mutate(FR_SVi = SVi/sum(fullvox$SVi))

  ### P_Di is the number of returns below each voxel (Pearse et al. 2019; Kim et al. 2016 uses returns above)
  # create a vector for Z in each iteration
  unique_Z = unique(fullvox$Z)

  # calculate points below each voxel
  point_blow <- lapply(unique_Z, function(x) fullvox %>%
           group_by(X,Y) %>%
           filter(Z < x) %>%
           summarize(npoints_below = sum(SVi), Z= x))

  # extract point below data and merge using the voxel id
  point_blow_all_vox <- do.call(rbind,point_blow)
  point_blow_all_vox <- point_blow_all_vox %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
  fullvox <- fullvox %>% left_join(as.data.frame(point_blow_all_vox[,c(3,5)]), by = "id_xyz", )

  ### P_Di_above is the number of returns above each voxel (Kim et al. 2016 uses returns above as P_Di)
  # calculate points below each voxel
  point_above <- lapply(unique_Z, function(x) fullvox %>%
           group_by(X,Y) %>%
           filter(Z > x) %>%
           summarize(npoints_above = sum(SVi,na.rm = T), Z= x))
  # extract point below data and merge using the voxel id
  point_above_all_vox <- do.call(rbind,point_above)
  point_above_all_vox <- point_above_all_vox %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
  fullvox <- fullvox %>% left_join(as.data.frame(point_above_all_vox[,c(3,5)]), by = "id_xyz", )
  # NAs returned for 0 points so it is appropriate to replace them with 0
  fullvox$npoints_above[is.na(fullvox$npoints_above)] <- 0

  ### FR_Di is the frequency ratio of the number of returns above a voxel in relation to total returns (Kim et al. 2016)
  fullvox <- fullvox %>% mutate(FR_Di = npoints_above/sum(fullvox$SVi))

  ### I_Di is the median intensity of returns above each voxel
  # calculate median intensity above each voxel
i_above <- lapply(unique_Z, function(x) {
    i_a <- fullvox %>% filter(Z > x)
    if(nrow(i_a) == 0){
      fullvox %>% group_by(X,Y) %>% summarize(Z = x, i_Di = NA)
      } else{
    i_abv <- data.table::as.data.table(do.call(rbind, i_a$I_list))
    i_a_merge <- cbind(data.table::as.data.table(i_a[1:2]), i_abv)
    i_Di_mt <- i_a_merge %>% data.table::melt(id = c("X","Y")) %>%
      group_by(X,Y) %>%
      summarize(Z = x, i_Di = median(value, na.rm = T))

      }
  }
  )

  # extract point below data and merge using the voxel id
  i_above_all_vox <- do.call(rbind,i_above)
  i_above_all_vox <- i_above_all_vox %>% mutate(id_xyz = paste0(X,"-",Y,"-",Z))
  fullvox <- fullvox %>% left_join(as.data.frame(i_above_all_vox[,c(4,5)]), by = "id_xyz" )

  zskew = (sum((z - zmean)^3)/npoints)/(sum((z - zmean)^2)/npoints)^(3/2)
  zkurt = npoints*sum((z - zmean)^4)/(sum((z - zmean)^2)^2)

  ##### Summarized voxel metrics
  ### Start by summarizing individual voxel metrics
  voxel_summ <- fullvox %>% summarise(z_med_med = median(med_z_vox, na.rm = T),
                                      z_med_mean = mean(med_z_vox, na.rm = T),
                                      z_med_var = var(med_z_vox, na.rm = T),
                                      z_med_sd = sd(med_z_vox, na.rm = T),
                                      z_med_cv = sd(med_z_vox, na.rm = T)/mean(med_z_vox, na.rm = T),
                                      z_med_IQR = IQR(med_z_vox, na.rm = T),
                                      z_med_skew = (sum((med_z_vox - mean(med_z_vox))^3)/nrow(vox))/(sum((med_z_vox - mean(med_z_vox))^2)/nrow(vox))^(3/2),
                                      z_med_kurt = nrow(vox)*sum((med_z_vox- mean(med_z_vox, na.rm = T))^4)/(sum((med_z_vox - mean(med_z_vox, na.rm = T))^2)^2),
                                      z_mean_med = median(mean_z_vox, na.rm = T),
                                      z_mean_mean = mean(mean_z_vox, na.rm = T),
                                      z_mean_var = var(mean_z_vox, na.rm = T),
                                      z_mean_sd = sd(mean_z_vox, na.rm = T),
                                      z_mean_cv = sd(mean_z_vox, na.rm = T)/mean(mean_z_vox, na.rm = T),
                                      z_mean_IQR = IQR(mean_z_vox, na.rm = T),
                                      z_mean_skew = (sum((mean_z_vox - mean(mean_z_vox))^3)/nrow(vox))/(sum((mean_z_vox - mean(mean_z_vox))^2)/nrow(vox))^(3/2),
                                      z_mean_kurt = nrow(vox)*sum((mean_z_vox- mean(mean_z_vox, na.rm = T))^4)/(sum((mean_z_vox - mean(mean_z_vox, na.rm = T))^2)^2),
                                      z_var_med = median(var_z_vox, na.rm = T),
                                      z_var_mean = mean(var_z_vox, na.rm = T),
                                      z_var_var = var(var_z_vox, na.rm = T),
                                      z_var_sd = sd(var_z_vox, na.rm = T),
                                      z_var_cv = sd(var_z_vox, na.rm = T)/mean(var_z_vox, na.rm = T),
                                      z_var_IQR = IQR(var_z_vox, na.rm = T),
                                      z_var_skew = (sum((var_z_vox - mean(var_z_vox))^3)/nrow(vox))/(sum((var_z_vox - mean(var_z_vox))^2)/nrow(vox))^(3/2),
                                      z_var_kurt = nrow(vox)*sum((var_z_vox- mean(var_z_vox, na.rm = T))^4)/(sum((var_z_vox - mean(var_z_vox, na.rm = T))^2)^2),
                                      z_sd_med = median(sd_z_vox, na.rm = T),
                                      z_sd_mean = mean(sd_z_vox, na.rm = T),
                                      z_sd_var = var(sd_z_vox, na.rm = T),
                                      z_sd_sd = sd(sd_z_vox, na.rm = T),
                                      z_sd_cv = sd(sd_z_vox, na.rm = T)/mean(sd_z_vox, na.rm = T),
                                      z_sd_IQR = IQR(sd_z_vox, na.rm = T),
                                      z_sd_skew =(sum((sd_z_vox - mean(sd_z_vox))^3)/nrow(vox))/(sum((sd_z_vox - mean(sd_z_vox))^2)/nrow(vox))^(3/2),
                                      z_sd_kurt = nrow(vox)*sum((sd_z_vox- mean(sd_z_vox, na.rm = T))^4)/(sum((sd_z_vox - mean(sd_z_vox, na.rm = T))^2)^2),
                                      z_cv_med = median(cv_z_vox, na.rm = T),
                                      z_cv_mean = mean(cv_z_vox, na.rm = T),
                                      z_cv_var = var(cv_z_vox, na.rm = T),
                                      z_cv_sd = sd(cv_z_vox, na.rm = T),
                                      z_cv_cv = sd(cv_z_vox, na.rm = T)/mean(cv_z_vox, na.rm = T),
                                      z_cv_IQR = IQR(cv_z_vox, na.rm = T),
                                      z_cv_skew = (sum((cv_z_vox - mean(cv_z_vox))^3)/nrow(vox))/(sum((cv_z_vox - mean(cv_z_vox))^2)/nrow(vox))^(3/2),
                                      z_cv_kurt = nrow(vox)*sum((cv_z_vox- mean(cv_z_vox, na.rm = T))^4)/(sum((cv_z_vox - mean(cv_z_vox, na.rm = T))^2)^2),
                                      z_IQR_med = median(IQR_z_vox, na.rm = T),
                                      z_IQR_mean = mean(IQR_z_vox, na.rm = T),
                                      z_IQR_var = var(IQR_z_vox, na.rm = T),
                                      z_IQR_sd = sd(IQR_z_vox, na.rm = T),
                                      z_IQR_cv = sd(IQR_z_vox, na.rm = T)/mean(IQR_z_vox, na.rm = T),
                                      z_IQR_IQR = IQR(IQR_z_vox, na.rm = T),
                                      z_IQR_skew =(sum((IQR_z_vox - mean(IQR_z_vox))^3)/nrow(vox))/(sum((IQR_z_vox - mean(IQR_z_vox))^2)/nrow(vox))^(3/2),
                                      z_IQR_kurt = nrow(vox)*sum((IQR_z_vox- mean(IQR_z_vox, na.rm = T))^4)/(sum((IQR_z_vox - mean(IQR_z_vox, na.rm = T))^2)^2),
                                      z_skew_med = median(skew_z_vox, na.rm = T),
                                      z_skew_mean = mean(skew_z_vox, na.rm = T),
                                      z_skew_var = var(skew_z_vox, na.rm = T),
                                      z_skew_sd = sd(skew_z_vox, na.rm = T),
                                      z_skew_cv = sd(skew_z_vox, na.rm = T)/mean(skew_z_vox, na.rm = T),
                                      z_skew_IQR = IQR(skew_z_vox, na.rm = T),
                                      z_skew_skew = (sum((skew_z_vox - mean(skew_z_vox))^3)/nrow(vox))/(sum((skew_z_vox - mean(skew_z_vox))^2)/nrow(vox))^(3/2),
                                      z_skew_kurt = nrow(vox)*sum((skew_z_vox- mean(skew_z_vox, na.rm = T))^4)/(sum((skew_z_vox - mean(skew_z_vox, na.rm = T))^2)^2),
                                      z_kurt_med = median(kurt_z_vox, na.rm = T),
                                      z_kurt_mean = mean(kurt_z_vox, na.rm = T),
                                      z_kurt_var = var(kurt_z_vox, na.rm = T),
                                      z_kurt_sd = sd(kurt_z_vox, na.rm = T),
                                      z_kurt_cv = sd(kurt_z_vox, na.rm = T)/mean(kurt_z_vox, na.rm = T),
                                      z_kurt_IQR = IQR(kurt_z_vox, na.rm = T),
                                      z_kurt_skew = (sum((kurt_z_vox - mean(kurt_z_vox))^3)/nrow(vox))/(sum((kurt_z_vox - mean(kurt_z_vox))^2)/nrow(vox))^(3/2),
                                      z_kurt_kurt = nrow(vox)*sum((kurt_z_vox- mean(kurt_z_vox, na.rm = T))^4)/(sum((kurt_z_vox - mean(kurt_z_vox, na.rm = T))^2)^2),
                                      i_vox_med = median(med_i_vox, na.rm = T),
                                      i_med_mean = mean(med_i_vox, na.rm = T),
                                      i_med_var = var(med_i_vox, na.rm = T),
                                      i_med_sd = sd(med_i_vox, na.rm = T),
                                      i_med_cv = sd(med_i_vox, na.rm = T)/mean(med_i_vox, na.rm = T),
                                      i_med_IQR = IQR(med_i_vox, na.rm = T),
                                      i_med_skew = (sum((med_i_vox - mean(med_i_vox))^3)/nrow(vox))/(sum((med_i_vox - mean(med_i_vox))^2)/nrow(vox))^(3/2),
                                      i_med_kurt = nrow(vox)*sum((med_i_vox- mean(med_i_vox, na.rm = T))^4)/(sum((med_i_vox - mean(med_i_vox, na.rm = T))^2)^2),
                                      i_mean_med = median(mean_i_vox, na.rm = T),
                                      i_mean_mean = mean(mean_i_vox, na.rm = T),
                                      i_mean_var = var(mean_i_vox, na.rm = T),
                                      i_mean_sd = sd(mean_i_vox, na.rm = T),
                                      i_mean_cv = sd(mean_i_vox, na.rm = T)/mean(mean_i_vox, na.rm = T),
                                      i_mean_IQR = IQR(mean_i_vox, na.rm = T),
                                      i_mean_skew =  (sum((mean_i_vox - mean(mean_i_vox))^3)/nrow(vox))/(sum((mean_i_vox - mean(mean_i_vox))^2)/nrow(vox))^(3/2),
                                      i_mean_kurt = nrow(vox)*sum((mean_i_vox- mean(mean_i_vox, na.rm = T))^4)/(sum((mean_i_vox - mean(mean_i_vox, na.rm = T))^2)^2),
                                      i_var_med = median(var_i_vox, na.rm = T),
                                      i_var_mean = mean(var_i_vox, na.rm = T),
                                      i_var_var = var(var_i_vox, na.rm = T),
                                      i_var_sd = sd(var_i_vox, na.rm = T),
                                      i_var_cv = sd(var_i_vox, na.rm = T)/mean(var_i_vox, na.rm = T),
                                      i_var_IQR = IQR(var_i_vox, na.rm = T),
                                      i_var_skew =  (sum((var_i_vox - mean(var_i_vox))^3)/nrow(vox))/(sum((var_i_vox - mean(var_i_vox))^2)/nrow(vox))^(3/2),
                                      i_var_kurt = nrow(vox)*sum((var_i_vox- mean(var_i_vox, na.rm = T))^4)/(sum((var_i_vox - mean(var_i_vox, na.rm = T))^2)^2),
                                      i_sd_med = median(sd_i_vox, na.rm = T),
                                      i_sd_mean = mean(sd_i_vox, na.rm = T),
                                      i_sd_var = var(sd_i_vox, na.rm = T),
                                      i_sd_sd = sd(sd_i_vox, na.rm = T),
                                      i_sd_cv = sd(sd_i_vox, na.rm = T)/mean(sd_i_vox, na.rm = T),
                                      i_sd_IQR = IQR(sd_i_vox, na.rm = T),
                                      i_sd_skew =  (sum((sd_i_vox - mean(sd_i_vox))^3)/nrow(vox))/(sum((sd_i_vox - mean(sd_i_vox))^2)/nrow(vox))^(3/2),
                                      i_sd_kurt = nrow(vox)*sum((sd_i_vox- mean(sd_i_vox, na.rm = T))^4)/(sum((sd_i_vox - mean(sd_i_vox, na.rm = T))^2)^2),
                                      i_cv_med = median(cv_i_vox, na.rm = T),
                                      i_cv_mean = mean(cv_i_vox, na.rm = T),
                                      i_cv_var = var(cv_i_vox, na.rm = T),
                                      i_cv_sd = sd(cv_i_vox, na.rm = T),
                                      i_cv_cv = sd(cv_i_vox, na.rm = T)/mean(cv_i_vox, na.rm = T),
                                      i_cv_IQR = IQR(cv_i_vox, na.rm = T),
                                      i_cv_skew =  (sum((cv_i_vox - mean(cv_i_vox))^3)/nrow(vox))/(sum((cv_i_vox - mean(cv_i_vox))^2)/nrow(vox))^(3/2),
                                      i_cv_kurt = nrow(vox)*sum((cv_i_vox- mean(cv_i_vox, na.rm = T))^4)/(sum((cv_i_vox - mean(cv_i_vox, na.rm = T))^2)^2),
                                      i_IQR_med = median(IQR_i_vox, na.rm = T),
                                      i_IQR_mean = mean(IQR_i_vox, na.rm = T),
                                      i_IQR_var = var(IQR_i_vox, na.rm = T),
                                      i_IQR_sd = sd(IQR_i_vox, na.rm = T),
                                      i_IQR_cv = sd(IQR_i_vox, na.rm = T)/mean(IQR_i_vox, na.rm = T),
                                      i_IQR_IQR = IQR(IQR_i_vox, na.rm = T),
                                      i_IQR_skew =  (sum((IQR_i_vox - mean(IQR_i_vox))^3)/nrow(vox))/(sum((IQR_i_vox - mean(IQR_i_vox))^2)/nrow(IQR_i_vox))^(3/2),
                                      i_IQR_kurt = nrow(vox)*sum((IQR_i_vox- mean(IQR_i_vox, na.rm = T))^4)/(sum((IQR_i_vox - mean(IQR_i_vox, na.rm = T))^2)^2),
                                      i_skew_med = median(skew_i_vox, na.rm = T),
                                      i_skew_mean = mean(skew_i_vox, na.rm = T),
                                      i_skew_var = var(skew_i_vox, na.rm = T),
                                      i_skew_sd = sd(skew_i_vox, na.rm = T),
                                      i_skew_cv = sd(skew_i_vox, na.rm = T)/mean(skew_i_vox, na.rm = T),
                                      i_skew_IQR = IQR(skew_i_vox, na.rm = T),
                                      i_skew_skew =  (sum((skew_i_vox - mean(skew_i_vox))^3)/nrow(vox))/(sum((skew_i_vox - mean(skew_i_vox))^2)/nrow(vox))^(3/2),
                                      i_skew_kurt = nrow(vox)*sum((skew_i_vox- mean(skew_i_vox, na.rm = T))^4)/(sum((skew_i_vox - mean(skew_i_vox, na.rm = T))^2)^2),
                                      i_kurt_med = median(kurt_i_vox, na.rm = T),
                                      i_kurt_mean = mean(kurt_i_vox, na.rm = T),
                                      i_kurt_var = var(kurt_i_vox, na.rm = T),
                                      i_kurt_sd = sd(kurt_i_vox, na.rm = T),
                                      i_kurt_cv = sd(kurt_i_vox, na.rm = T)/mean(kurt_i_vox, na.rm = T),
                                      i_kurt_IQR = IQR(kurt_i_vox, na.rm = T),
                                      i_kurt_skew =  (sum((kurt_i_vox - mean(kurt_i_vox))^3)/nrow(vox))/(sum((kurt_i_vox - mean(kurt_i_vox))^2)/nrow(vox))^(3/2),
                                      i_kurt_kurt = nrow(vox)*sum((kurt_i_vox- mean(kurt_i_vox, na.rm = T))^4)/(sum((kurt_i_vox - mean(kurt_i_vox, na.rm = T))^2)^2),
                                      P_Di_med = median(npoints_below, na.rm = T),
                                      P_Di_mean = mean(npoints_below, na.rm = T),
                                      P_Di_var = var(npoints_below, na.rm = T),
                                      P_Di_sd = sd(npoints_below, na.rm = T),
                                      P_Di_cv = sd(npoints_below, na.rm = T)/mean(npoints_below, na.rm = T),
                                      P_Di_IQR = IQR(npoints_below, na.rm = T),
                                      P_Di_skew =  (sum((npoints_below - mean(npoints_below))^3)/nrow(vox))/(sum((npoints_below - mean(npoints_below))^2)/nrow(vox))^(3/2),
                                      P_Di_kurt = nrow(vox)*sum((npoints_below- mean(npoints_below, na.rm = T))^4)/(sum((npoints_below - mean(npoints_below, na.rm = T))^2)^2),
                                      npoints_above_med = median(npoints_above, na.rm = T),
                                      npoints_above_mean = mean(npoints_above, na.rm = T),
                                      npoints_above_var = var(npoints_above, na.rm = T),
                                      npoints_above_sd = sd(npoints_above, na.rm = T),
                                      npoints_above_cv = sd(npoints_above, na.rm = T)/mean(npoints_above, na.rm = T),
                                      npoints_above_IQR = IQR(npoints_above, na.rm = T),
                                      npoints_above_skew =  (sum((npoints_above - mean(npoints_above))^3)/nrow(vox))/(sum((npoints_above - mean(npoints_above))^2)/nrow(vox))^(3/2),
                                      npoints_above_kurt = nrow(vox)*sum((npoints_above- mean(npoints_above, na.rm = T))^4)/(sum((npoints_above - mean(npoints_above, na.rm = T))^2)^2),
                                      FR_Di_med = median(FR_Di, na.rm = T),
                                      FR_Di_mean = mean(FR_Di, na.rm = T),
                                      FR_Di_var = var(FR_Di, na.rm = T),
                                      FR_Di_sd = sd(FR_Di, na.rm = T),
                                      FR_Di_cv = sd(FR_Di, na.rm = T)/mean(FR_Di, na.rm = T),
                                      FR_Di_IQR = IQR(FR_Di, na.rm = T),
                                      FR_Di_skew =  (sum((FR_Di - mean(FR_Di))^3)/nrow(vox))/(sum((FR_Di - mean(FR_Di))^2)/nrow(vox))^(3/2),
                                      FR_Di_kurt = nrow(vox)*sum((FR_Di- mean(FR_Di, na.rm = T))^4)/(sum((FR_Di - mean(FR_Di, na.rm = T))^2)^2),
                                      i_Di_med = median(i_Di, na.rm = T),
                                      i_Di_mean = mean(i_Di, na.rm = T),
                                      i_Di_var = var(i_Di, na.rm = T),
                                      i_Di_sd = sd(i_Di, na.rm = T),
                                      i_Di_cv = sd(i_Di, na.rm = T)/mean(i_Di, na.rm = T),
                                      i_Di_IQR = IQR(i_Di, na.rm = T),
                                      i_Di_skew =  (sum((i_Di - mean(i_Di))^3)/nrow(vox))/(sum((i_Di - mean(i_Di))^2)/nrow(vox))^(3/2),
                                      i_Di_kurt = nrow(vox)*sum((i_Di- mean(i_Di, na.rm = T))^4)/(sum((i_Di - mean(i_Di, na.rm = T))^2)^2),
                                      pct_fill_vox = nrow(vox) /nrow(fullvox)


  )

  ## summarize by hieght bins

  ### SVM_med is the median height of all points within maximum density subvoxel (i.e., voxel) or each X,Y

  # find the median z for the voxels with highest number of points (SVi) for each
  SVM <- fullvox %>% group_by(X,Y) %>%
    summarize(Z = Z[which.max(SVi)], SVM_density = max(SVi), median_Z = med_z_vox[which.max(SVi)])

  ### SVM_above is distribution summary statistics of the height of all points above maximum density subvoxel (i.e., voxel) or each X,Y
  ### Pearse et al. (2019) used  only the median

  SVM_list <- apply(SVM, MARGIN = 1, FUN = function(x) {
    vox_above <- fullvox %>% filter(Z > x["Z"] & X == x["X"] & Y == x["Y"])
    hts_above <- as.numeric(do.call(cbind,vox_above$Z_list))
    SVM_all_mts <- data.frame(SVM_med = median(hts_above, na.rm = T))
    SVM_all_mts <- as.data.frame(cbind(X = x["X"], Y = x["Y"], Z = x["Z"],
                         median_Z_above = median(hts_above, na.rm = T),
                         mean_Z_above = mean(hts_above, na.rm = T),
                         sd_Z_above = sd(hts_above, na.rm = T),
                         var_Z_above = sd(hts_above, na.rm = T)^2,
                         cv_Z_above = sd(hts_above, na.rm = T)/ mean(hts_above, na.rm = T),
                         IQR_Z_above = IQR(hts_above, na.rm = T),
                         skew_Z_above = e1071::skewness(hts_above, na.rm = T),
                         kurt_Z_above = e1071::kurtosis(hts_above, na.rm = T)))
  }
  )

  # extract calculated metrics
  SVM_all <- do.call(rbind, SVM_list)
  SVM_all <- SVM %>% left_join(SVM_all)

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
  p <- lapply(unique_Z, function(x){
    proportion <- fullvox %>% filter(Z == x) %>% summarize(p0 = sum(empty)/sum(fullvox$empty))
  })
  p <- do.call(rbind,p)

  # calculate the different diversity indicies similar to Ehbrecht et al. (2016) and Pearse et al. (2019)
  p <-  p %>% mutate(pln = p0*log(p0))
  p <- p %>% mutate(p2 = p0^2)
  ENL_d0 <- sum(p$p0^0)
  ENL_d1 <- exp(-sum(p$pln)) ## exponential Shannon-Index
  ENL_d2 <- 1/sum(p$p2) ## inverse Simpson-Index

  # add indicies to voxel_summ df
  voxel_summ <- cbind(voxel_summ, ENL_d0, ENL_d1, ENL_d2)

  ### Sub-pixel canopy closure above (cc_above) is the measure of canopy closure

  thr_above = seq(from = 3, to = 21, by = 3)
  cc_list <- lapply(thr_above, function(x) {
    cc_above <- fullvox %>%
      filter(if(x < max(thr_above)) {Z <= x} else(Z <= max(Z))) %>%
      summarize(cc = sum(empty)/n())
  }
  )
  names(cc_list) <- paste0("cc_above", thr_above)
  # extract canopy closure and prepare to merge with voxel_summ
  cc_above <- do.call(rbind, cc_list)
  cc_above_df <- as.data.frame(t(cc_above[,1]))
  colnames(cc_above_df) <- rownames(cc_above)

  # add closure metrics to voxel_summ df
  voxel_summ <- cbind(voxel_summ, cc_above_df)


  ### P_cc is the mean canopy closure and is calculated by the ratio of height bin point density to overall point density

  ht_bin = c(1, 5, 10, 15, 20)

  npoints_XY <- fullvox %>% group_by(X,Y) %>% summarize(npoints = sum(SVi, na.rm = T))

  pcc_list  <-  lapply(ht_bin, function(x) {
    point_density <- fullvox %>%
      filter(if(x < max(ht_bin)) { Z >=x & Z < (x+1)}  else(Z >= max(ht_bin))) %>%
      group_by(X,Y) %>%
      summarize(pt_density = sum(SVi))
    if(nrow(point_density) == 0) {return(NULL)} else{
    point_density$pcc = point_density$pt_density/ npoints_XY$npoints
    point_density$ht_bin <- x
    return(point_density)
    }
  } )

  pcc_bins <- do.call(rbind, pcc_list)
  pcc_bins$pcc[is.na(pcc_bins$pcc)] <- 0
  pcc_XY <- pcc_bins %>% group_by(X,Y) %>% summarise(pcc = sum(pcc))

  pcc <- mean(pcc_XY$pcc)

  voxel_summ <- cbind(voxel_summ, pcc = pcc)

  ### VCI is a measure of the eveness or simulatiry of point densitys
  ### of height bins within a given las based on Shannon diversity index
  ht_bin <- c(.5,1,2,3,4,5)
  VCI_list <- lapply(ht_bin, function(x) {
    VCI <- lidR::VCI(las@data$Z, max(las@data$Z), by =  x)
    })
  names(vci_list) <- paste0("vci_bin", ht_bin)
  vci_all <- t(as.data.frame(do.call(rbind,vci_list))[1])
  voxel_sum <- cbind(voxel_summ, vci_all)
  return(voxel_summ)
}
