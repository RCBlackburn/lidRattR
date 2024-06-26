% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lidRattR_functions.R
\name{voxel_raster}
\alias{voxel_raster}
\title{Voxel raster summary function}
\usage{
voxel_raster(las, vox_res = vox_res, rast_res = rast_res, max_z)
}
\arguments{
\item{las}{las object}

\item{vox_res}{SVi cubic resolution in units of las object}

\item{rast_res}{raster pixel resolution in units of las object}

\item{max_z}{If numeric, the final height for all sub-voxels. If NULL, the max height for the entire point cloud
(or chunk for catalog processing) will be used.}
}
\value{
returns a SpatRaster with 65 different layers representing voxel variables summarized to the resolution defined in (rast_res) with the naming convention:
(voxel variable)_(voxel resolution)_(summary statistic)
}
\description{
This function provides individual voxel-based variables including the frequency ratio (FRDi),frequency ratio (FRSVi),
number of returns below each sub-voxel (SVi) (PDiBelow), and the number of returns above each SVi (PDiAbove),
sum of returns within a sub-voxels (SVsum), classifcation of a sub-voxel maximum (SVM), and
median height of the number of returns above (SVMmdAbove) or within (SVMmd) SVMs. These variables are summarized
within each pixel including the mean, median, variance, standard deviation, coefficient of variation,
IQR, skewness, and kurtosis
}
\examples{
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile)
las <- filter_poi(las, Classification != LASNOISE)
las <- normalize_height(las, knnidw(k = 8, p = 2))
las <- filter_poi(las, Z < 50 & Z >= 1.37 )
las <- decimate_points(las, random_per_voxel(res = 1, n = 8))
metrics <- voxel_raster(las, vox_res = 2, rast_res = 10, max_z = 40)
plot(metrics)
}
\keyword{lidar}
\keyword{metrics}
\keyword{voxel}
