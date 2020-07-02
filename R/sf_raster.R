#' Get projection info from a sf object.
#'
#' @param sf_obj object to extract projection info from.
#'
#' @return projection info.
#' @export
#'
#' @examples get_crs_from_sf_obj (sf_obj)

library (sf)
get_crs_from_sf_obj <- function (sf_obj) {
  crs_str = st_crs (sf_obj)
  return (crs_str)
}
