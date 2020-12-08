#' Get US Zip Code boundaries in sf object
#'
#' @param epsg_num
#'
#' @return
#' @export get_us_zipcode
#' @import USAboundaries
#' @import dplyr
#' @import sf
#' @import magrittr
#' @examples
get_us_zipcode <- function (epsg_num = 4326) {
  zip = USAboundaries::us_zipcodes() %>% sf::st_transform(crs=epsg_num)
  return (zip)
}

#' Get CONUS base map
#'
#' @param epsg_num integer for EPSG code
#'
#' @return
#' @export get_conus_map
#' @import sf
#' @import USAboundaries
#' @import dplyr
#' @examples
get_conus_map <- function (epsg_num = 4326) {
  us_state_high_res = USAboundaries::us_states(res = 'high') %>%
                      sf::st_transform(crs = epsg_num)
  us_conus_high_res = us_state_high_res %>%
      dplyr::filter (!state_abbr %in% c('AK', 'HI', 'PR', 'GU', 'MP', 'VI', 'AS'))
  return (us_conus_high_res)
}
