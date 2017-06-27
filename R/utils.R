#' Spatial overlay: at the spatial locations of object x retrieves the attributes from spatial object y
#'
#' Equivalent to sp::over
#'
#' @param x geometry of the queries
#' @param y layer from which the geometries or attributes are queried
#'
#' @return a data frame of the attributes of y corresponding to the locations of x
#' @export
#'
#' @examples
sf_over <- function(x, y) {
  out <- lapply(sf::st_intersects(x, y), function(z) {
    if (!length(z)) {
      ret <- y[1, , drop = FALSE]
      ret[] <- NA
    } else {
      ret <- y[z, , drop = FALSE]
    }
    st_set_geometry(ret, NULL)
  })
  dplyr::bind_rows(out)
}
