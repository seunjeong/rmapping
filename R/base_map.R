require(USAboundaries)
require(USAboundariesData)
require(dplyr)
require(sf)

#' Get US Zip Code boundaries in sf object
#'
#' @param epsg_num
#'
#' @return
#' @export get_us_zipcode
#'
#' @examples
get_us_zipcode <- function (epsg_num = 4326) {
  zip = USAboundaries::us_zipcodes() %>% sf::st_transform(crs=epsg_num)
  return (zip)
}
