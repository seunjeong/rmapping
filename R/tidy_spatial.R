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
