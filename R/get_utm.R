#'
#' functions to get receiver and tagging metadata into session
#'
#' @name get_utm
#' @import data.table
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

get_utm <- function(df, crs=32633) {
  df %>%
    dplyr::filter(!is.na(.data$lon)) %>%
    sf::st_as_sf(., coords=c("lon", "lat")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(crs) %>%
    as(., "Spatial") %>%
    as_tibble %>%
    dplyr::rename(lon=coords.x1, lat=coords.x2)

}
