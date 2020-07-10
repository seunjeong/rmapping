library (sf)
library (assertthat)

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
#'
#' @return
#' @export plot_raster_layer_spatial
#' @import ggplot2
#' @import ggspatial
#' @import viridis
#' @examples
#'
plot_raster_layer_spatial <- function (r) {

  # Use na.value = 'NA' to remove gray color at the border.
  p = ggplot () + layer_spatial (r) + scale_fill_viridis (na.value = 'NA')
  return (p)
}
