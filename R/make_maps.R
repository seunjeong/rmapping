#' Add points to the existing map.
#'
#' @param p ggplot2 object
#' @param df_point data frame that contains the point data to plot
#' @param col_name_to_plot the column name of df_point for 'fill' or 'color'
#' @param col_name_for_size the column name of df_point for size
#' @param color_or_fill one of "fill" and "color"
#' @param pt_size the size of the point to be plotted
#' @param pt_stroke the stroke (the line thickness of the outline of the shape)
#'
#' @return p a ggplot2 object with points added
#' @export add_points_to_maps
#' @import sf
#' @import tidyverse
#' @import assertthat
#' @examples
#'
add_points_to_maps <- function (p, df_point, col_name_to_plot, col_name_for_size = NULL, color_or_fill = 'fill',
                                pt_size = 1.8, pt_stroke = 0.5)
{

    assert_that('lat'%in%names (df_point))
    assert_that('lon'%in%names (df_point))

    #if (is.null (col_name_for_size) | is.na (col_name_for_size)) {
    #    col_name_for_size = col_name_to_plot
    #}

    if (color_or_fill == 'fill') {
        if (is.null (col_name_for_size) | is.na (col_name_for_size)) {
            p = p + geom_point (data = df_point, aes (x = lon, y = lat, fill = get(col_name_to_plot)),
                            shape = 21, size = pt_size, stroke = pt_stroke)
        } else {
            p = p + geom_point (data = df_point, aes (x = lon, y = lat, fill = get(col_name_to_plot),
                              size = get(col_name_for_size)),
                              shape = 21)
        }
    } else if (color_or_fill == 'color') {
        p = p + geom_point (data = df_point, aes (x = lon, y = lat, color = get(col_name_to_plot)))
    }

    return (p)

}

if (FALSE) {
  library (spatialwithr)
  library (assertthat)
  library (sf)
  library (tidyverse)
  df_ca = spatialwithr::get_us_map_high_res() %>% dplyr::filter (state_abbr == 'CA')
  p = ggplot() + geom_sf (data = df_ca, fill=NA)

  # add
  df = housing %>% dplyr::select (lon = longitude, lat = latitude, pop = population) %>%
    dplyr::filter(pop > 5e3)
  p_pts = add_points_to_maps (p, df, "pop", "pop") + scale_fill_distiller(palette = "Paired")
}

