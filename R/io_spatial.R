require(ncdf4)

#' Get a variable from netcdf file or ncdf4 object.
#'
#' @param ncin string or ncdf4 object
#' @param var_name variable name of type string.
#'
#' @return
#' @export
#' @examples get_ncdf_var (ncin, var_name)

get_ncdf_var <- function (ncin, var_name) {
  if (class(ncin) != "ncdf4") {
    ncin <- nc_open(ncin)
  }

  return (ncvar_get(ncin, var_name))
}
