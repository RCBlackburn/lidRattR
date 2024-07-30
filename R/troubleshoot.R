library(lidR)
library(terra)
library(sf)
library(lidRattR)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg = readLAScatalog(LASfile)
las <- readLAS(LASfile)
las <- clip_roi(las, st_buffer(st_centroid(st_as_sfc(st_bbox(las))), 20))
opt_chunk_buffer(ctg) <- 10
opt_chunk_size(ctg) <- 100
opt_select(ctg) <- "xyzicr"
opt_wall_to_wall(ctg) <- TRUE
opt_chunk_alignment(ctg) <- c(0,0)
opt_progress(ctg) <- TRUE
opt_stop_early(ctg) <- TRUE

plot(ctg, chunk = TRUE)
all_layers_catalog = function(cluster)
{
  las = readLAS(cluster)
  las = las_update(las)
  if (lidR::is.empty(las)) return(NULL)
  las = filter_poi(las, Classification != LASNOISE)
  if (lidR::is.empty(las)) return(NULL)
  las = normalize_height(las, knnidw(k = 8, p = 2))
  las = filter_poi(las, Z < 50 & Z >= 1.37 )
  if (lidR::is.empty(las)) return(NULL)
  las <- decimate_points(las, random_per_voxel(res = 1, n = 8))
  if (lidR::is.empty(las)) return(NULL)

  ### voxel metrics
  las_vox <- ind_vox(las, res = 1, max_z = 60)
  vmetrics <- pixel_metrics(las_vox,
                            func = vox_sum_raster(SVi, FRDi, FRSVi, PDi, PDi_above, sv_sum, vox_res = 1),
                            res = 10)

  tile0_ext = lidR::ext(las)
  vmetrics <- crop(vmetrics, tile0_ext)

  return(wrap(vmetrics))
}


layer_list <- catalog_apply(ctg, all_layers_catalog)

raster <- lapply(layer_list, terra::unwrap)
raster <- do.call(terra::merge, raster)

plot(raster)
writeRaster(raster, "testvox.tif", overwrite = TRUE)
