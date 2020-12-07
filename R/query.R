#' Find index from a 2-D data frame (e.g., map)
#'
#' @param data_2D 2-D data frame where x/y or lon/lat are available and from which the index is derived.
#' @param query Data frame for query. Each row of the query (2-d matrix or 2-column data frame) looks for the closest value in distance (to the row) from the data and return its index corresponding to that value in data_2D.
#' @param num_index_per_query the number of indices to be returned - typically just one.
#' @param x the x (axis) name in data_2D
#' @param y the y (axis) name in data_2D
#' @param x_query the x (axis) name in the query data frame
#' @param y_query the y (axis) name in the query data frame
#'
#' @return
#' @export find_index_in_2D_df
#' @import FNN
#' @import dplyr
#' @examples
find_index_in_2D_df <- function (data_2D, query, num_index_per_query = 1, x = 'lon', y = 'lat', x_query = 'lon', y_query = 'lat') {
  assert_that ('lon' %in% names (data_2D) | 'x' %in% names (data_2D))
  assert_that ('lat' %in% names (data_2D) | 'y' %in% names (data_2D))

  idx = as.vector (FNN::knnx.index(data_2D %>% dplyr::select (one_of (x), one_of(y)),
                                   query %>% dplyr::select (one_of (x_query), one_of (y_query)) , k=num_index_per_query))
  assert_that (length (idx) == nrow (query))
  return (idx)
}
