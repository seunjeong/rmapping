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
