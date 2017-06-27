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

gg_fortify <- function(x) {
  if (!require("maptools")) stop("maptools is not installed")
  if (!requireNamespace("ggplot2")) stop("ggplot2 is not installed.")
  if (!requireNamespace("dplyr")) stop("dplyr is not installed.")
  x@data$ggid <- rownames(x@data)
  x_points <- ggplot2::fortify(x, region = "ggid")
  x_df <- dplyr::left_join(x_points, x@data, by = c("id" = "ggid"))
  x_df
}