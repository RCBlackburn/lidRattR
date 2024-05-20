library(lidR)
library(data.table)
library(sf)

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile)

las <- clip_roi(las, st_buffer(st_centroid(st_as_sfc(st_bbox(las))),30))

plot(las)

las_vox <- ind_vox(las, res = 2)
overall_v_metrics <- vox_sum(las_vox, 2)
