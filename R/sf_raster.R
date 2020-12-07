library (sf)
library (assertthat)

#' Convert raster to data frame
#'
#' @param r input raster
#' @param name_4_input_val the layer name in the input raster
#' @param name_4_out_val  the column name in the output data frame
#'
#' @return
#' @export raster_to_df
#'
#' @examples
raster_to_df <-function (r, name_4_input_val, name_4_out_val = 'val') {
  df_rast = rasterToPoints (r) %>%
    as_tibble () %>%
    #dplyr::rename (!!name_4_out_val := all_of (name_4_input_val))
    dplyr::rename (!!name_4_out_val := !!name_4_input_val)
  return (df_rast)
}

#' Get projection info from a sf object.
#'
#' @param sf_obj object to extract projection info from.
#'
#' @return projection info.
#' @export
#'
#' @examples get_crs_from_sf_obj (sf_obj)
get_crs_from_sf_obj <- function (sf_obj) {
  crs_str = st_crs (sf_obj)
  return (crs_str)
}


#' Convert data frame to sf object
#'
#' @param df data frame with lon and lat
#' @param crs_num_or_str projection string or the corresponding EPSG number
#'
#' @return sf object.
#' @export
#'
#' @examples convert_df_to_sf_obj (df, 4326)
#'
convert_df_to_sf_obj <- function (df, crs_num_or_str = 4326) {
  assertthat::assert_that('lon' %in% names (df))
  assertthat::assert_that('lat' %in% names (df))
  df_sf = df %>% st_as_sf(coords = c("lon", "lat"), crs = crs_num_or_str)
  return (df_sf)
}


#' Crop sf object by rectangle
#'
#' @param sf_obj sf object to be cropped
#' @param xmin x (or lon) min
#' @param xmax x (or lon) max
#' @param ymin y (or lat) min
#' @param ymax y (or lat) max
#'
#' @return cropped sf object
#' @export
#'
#' @examples crop_sf_obj_by_rect_bb (sf_obj, xmin, xmax, ymin, ymax)
crop_sf_obj_by_rect_bb <- function(sf_obj, xmin, xmax, ymin, ymax) {
  cropped  = st_crop(sf_obj, c(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))
  return (cropped)
}

#' Convert data frame with lon/lat to a raster object
#'
#' @param df data frame with columns of lon ant lat
#'
#' @return a raster object
#' @export df_with_lonlat_to_raster
#' @import raster
#' @examples essentially same as convert_df_xyz_to_raster
df_with_lonlat_to_raster <- function (df) {

  col_names <- colnames(df)

  assert_that ('lon' %in% col_names)
  assert_that ('lat' %in% col_names)

  coordinates(df) <- ~ lon + lat

  # to SpatialPixelsDataFrame
  gridded(df) <- TRUE

  # to raster
  r <- raster(df)

  # projection
  crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  return(r)
}

#' Crop sf obj by polygon
#'
#' @param sf_obj sf object to be cropped
#' @param polygon_mask polygon mask
#'
#' @return cropped sf object
#' @export
#'
#' @examples crop_sf_obj_by_polygon_mask (sf_obj, polygon_mask)
crop_sf_obj_by_polygon_mask <- function(sf_obj, polygon_mask) {
  cropped  = st_crop(sf_obj, st_bbox(polygon_mask))
  return (cropped)
}


#' Plot raster object using ggspatial
#'
#' @param r raster object
#' @param value_trans (from ggplot2 doc) For continuous scales, the name of a transformation object or the object itself. Built-in transformations include "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".
#' @param  breaks breaks for values
#' @return
#' @export plot_raster_layer_spatial
#' @import ggplot2
#' @import ggspatial
#' @import viridis
#' @import sf
#' @examples
#'
plot_raster_layer_spatial <- function (r, value_trans = 'identity', breaks = NULL) {

  # Use na.value = 'NA' to remove gray color at the border.
  p = ggplot () + layer_spatial (r)

  if (is.null(breaks)) {
      p = p + scale_fill_viridis (na.value = 'NA', trans = value_trans)
  } else {
      p = p + scale_fill_viridis (na.value = 'NA', trans = value_trans, breaks = breaks)
  }

  p = p + theme_minimal() +
          coord_sf (expand=F)

  return (p)
}

#' Convert data frame with xyz information to raster object.
#'
#' @param df data frame with x, y, z information
#' @param col_to_rasterize the column name for "z"
#'
#' @return
#' @export convert_df_xyz_to_raster
#' @import raster
#' @import dplyr
#' @examples
convert_df_xyz_to_raster <- function (df, col_to_rasterize) {
  if ('x' %in% names (df) & 'y' %in% names (df)) {
    sel_col = c('x', 'y', col_to_rasterize)
  } else if ('lon' %in% names (df) & 'lon' %in% names (df)) {
    sel_col = c('lon', 'lat', col_to_rasterize)
  } else stop ('Check x/y or lon/lat in the data frame')


  df = df %>% dplyr::select (one_of (sel_col))

  #df = df %>% dplyr::arrange (desc (lat))

  r = rst <- rasterFromXYZ(df)
}



#' Regrid raster object using the resample function of the raster package
#'
#' @param r Raster* object to be regridded
#' @param r_new_grid Raster* object with parameters that r should be resampled to
#' @param method sampling method. If the two (in & out) grids are similar in resolution, use 'ngb'. The other option is 'bilinear'
#'
#' @return
#' @export regrid_using_resample
#' @import raster
#' @examples
regrid_using_resample <- function (r, r_new_grid, method ='ngb') {
  r_on_new_grid <- resample(r, r_new_grid, method = method)
  return (r_on_new_grid)
}

#' Make raster template for regridding
#'
#' @param x_min the minimum X or lon (not center of the grid cell)
#' @param x_max the maximum X or lon (not center of the grid cell)
#' @param y_min the minimum Y or lat (not center of the grid cell)
#' @param y_max the maximum Y or lat (not center of the grid cell)
#' @param res spatial resolution in degrees. Assumes X and Y should have the same resolution.
#'
#' @return make raster template for regridding for any grids
#' @export make_raster_template
#' @import raster
#' @import sp
#' @examples
make_raster_template <- function(x_min, x_max, y_min, y_max, res) {
  xlon = seq(x_min, x_max, res)
  ylat = seq(y_min, y_max, res)

  dom_lonlat_r = expand.grid(xlon, ylat)
  names (dom_lonlat_r) = c('lon', 'lat')

  coordinates(dom_lonlat_r) = ~lon+lat
  # create an empty raster object to the extent of the points
  rast <- raster(ext=extent(dom_lonlat_r), resolution=res)

  rast_coord = coordinates(rast)

  return(rast)

}


#' Check if the data frame is on a regular (regularly spaced in X/Y) grid
#'
#' @param df data frame with X/Y or lon/lat information
#' @param res the spatial resolution of df
#' @param digits (from rasterFromXYZ) numeric, indicating the requested precision for detecting whether points are on a regular grid (a low number of digits is a low precision)
#'
#' @return TRUE if on a regular grid
#' @export check_if_regular_grid
#' @import raster
#' @note Adapted from raster::rasterFromXYZ
#' @examples
check_if_regular_grid <-function (df, res, digits = 5) {

  # Note: change to regular data.frame
  if (inherits (df, 'tbl') | inherits (df, 'tbl_df')) {
    df = data.frame (df)
  }

  if ('x' == names (df)[1] & 'y' == names (df)[2]) {
    cat ('coords are in x,y \n')
  } else if ('lon' == names (df)[1] & 'lon' == names (df)[2]) {
    cat ('coords are in lon, lat \n')
  } else stop ('Check x/y or lon/lat in the data frame')

  x <- sort(unique(df[, 1]))

  dx <- x[-1] - x[-length(x)]
  if (is.na(res[1])) {
    rx <- min(dx)
    for (i in 1:5) {
      rx <- rx/i
      q <- sum(round(dx/rx, digits = digits)%%1)
      if (q == 0) {
        break
      }
    }
    if (q > 0) {
      stop("x cell sizes are not regular")
    }
  } else {
    rx <- res[1]
    test <- sum(round(dx/rx, digits = digits)%%1)
    if (test > 0) {
      stop("x cell sizes are not regular")
    }
  }

  y <- sort(unique(df[, 2]))
  dy <- y[-1] - y[-length(y)]
  if (is.na(res[2])) {
    ry <- min(dy)
    for (i in 1:5) {
      ry <- ry/i
      q <- sum(round(dy/ry, digits = digits)%%1)
      if (q == 0) {
        break
      }
    }
    if (q > 0) {
      stop("y cell sizes are not regular")
    }
  } else {
    ry <- res[2]
    test <- sum(round(dy/ry, digits = digits)%%1)
    if (test > 0) {
      stop("y cell sizes are not regular")
    }
  }

  return (TRUE)
}


#' Add area in km^2 to data frame with spatial information (lon/lat or X/Y)
#'
#' @param df data frame with spatial information (lon/lat or X/Y)
#'
#' @return
#' @export add_area_km2_to_spatial_df
#' @import dplyr
#' @import assertthat
#' @examples
add_area_km2_to_spatial_df <- function (df) {

  #check_if_regular_grid (df)

  assert_that('lon' %in% names (df))
  assert_that("lat" %in% names(df))

  plon <- sort (unique(df$lon))
  plat <- sort(unique(df$lat))

  dom_lonlat <- expand.grid(plon, plat)
  names(dom_lonlat) <- c("lon", "lat")

  cellarea <- expand.grid(rep(mean(diff(plon)), length(plon)), c(diff(plat)[1], diff(plat)))
  dom_lonlat$area_km2 <- cellarea[, 1] * cellarea[, 2] * cos(dom_lonlat[, 2] * pi / 180) * (6375 * pi / 180)^2
  names(dom_lonlat)

  # ===============================================================================
  # Join
  # ===============================================================================
  df$lon_int = as.integer (df$lon * 10000)
  df$lat_int <- as.integer(df$lat * 10000)

  dom_lonlat$lon_int = as.integer(dom_lonlat$lon * 10000)
  dom_lonlat$lat_int <- as.integer(dom_lonlat$lat * 10000)

  dom_lonlat$lon = NULL
  dom_lonlat$lat <- NULL

  df_join = left_join (df, dom_lonlat, by = c('lon_int', 'lat_int'))
  assert_that (nrow(df_join) == nrow(df))
  assert_that (sum(is.na(df_join$area_km2)) == 0)
  return (df_join)
}
