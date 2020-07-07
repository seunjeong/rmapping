require(USAboundaries)

#' Get US Zip Code boundaries in sf object
#'
#' @param epsg_num
#'
#' @return
#' @export get_us_zipcode
#'
#' @examples
get_us_zipcode <- function (epsg_num = 4326) {
  zip = us_zipcodes() %>% st_transform(crs=epsg_num)
  return (zip)
}
