require (plyr)
#' Rename factors
#'
#' @param x: factor values to be renamed
#' @param vector_map: one to one map for the factors with renamed factors
#'
#' @return renamed factors
#' @export
#'
#' @examples
#' rename_factor (x, vector_map)
#' vector_map = c('< 3.0' = '727', '3.0 to 3.9' = '702', '4.0 to 4.9' = '612',
#   '5.0 to 5.9' = '519', '6.0 to 6.9' = '443', '7.0 to 7.9' = '366',
#   '8.0 to 8.9' = '296', '9.0 - 9.9' = '237', '\u2265 10' = '125')

rename_factor <- function (x, vector_map) {

  new_factor = plyr::revalue(x, vector_map)
  return (new_factor)
}
