require(tidyverse)
#' Make a long df from a list
#'
#' @param li a list with different lengths for each element of the list
#' @param name_for_list_names name for the names of the (original) list
#' @param name_for_value the column name for added values
#'
#' @return
#' @export list_to_pivot_longer
#'
#' @examples

list_to_pivot_longer <- function (li, names_of_list, col_name_to_add) {

  # Example
  # li = list('MWU' = c('ND', 'SD', 'NE', 'KS', 'MN', 'IA', 'MO', 'WI', 'IL', 'MI', 'IN', 'OH', 'KY'),
  #               'Texas' = c('TX', 'OK'),
  #               'SW' = c('CA', 'NV', 'UT', 'NM', 'CO', 'AZ'),
  #               'NW' = c('OR', 'WA', 'WY'),
  #               'MTWN' = c('ID', 'MT'),
  #               'NE' = c('ME', 'VT', 'NH', 'MA', 'NY', 'PA', 'CT', 'NJ', 'WV', 'VA', 'MD', 'DE', 'RI'))
  # df_long = df %>% pivot_longer(cols = names(df), names_to = 'region', values_to = 'state') %>%
  #     dplyr::filter(!is.na(state))


  # Example for a special case of "length"
  #> x = 1:5
  #> length (x) <- 10
  #> x
  #[1]  1  2  3  4  5 NA NA NA NA NA

  # We assign the length of each element of the list as the max of "lengths(li)"
  df = data.frame (lapply(li, "length<-", max(lengths(li))))

  df_long = df %>% pivot_longer(cols = names(df), names_to = name_for_list_names, values_to = name_for_value) %>%
    dplyr::filter(!is.na(col_name_to_add))

  return(df_long)
}


#' Convert matrix with no column or row names to a data frame
#'
#' @param mat 2-d matrix with no column or row names
#' @param var_1_name variable name for the first dim, default to 'x'
#' @param var_2_name variable name for the second dim, default to 'y'
#'
#' @return a data frame with xyz values
#' @export matrix_to_xyz_df
#' @import dplyr
#' @import reshape2
#' @examples foo = matrix_to_xyz_df (volcano)
# p = ggplot() + geom_contour(data = foo, aes(x=x, y=y, z = z, color=..level..))

matrix_to_xyz_df <- function (mat, var_1_name = 'x', var_2_name = 'y') {

  # simply melt
  df  = reshape2::melt(mat)

  names (df) = c(var_1_name, var_2_name, 'z')

  return (df)
}

