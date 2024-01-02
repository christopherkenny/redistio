#' Guess which columns contain administrative units
#'
#' @param shp an `sf` tibble that you want to draw with
#'
#' @return a named `list` of types
#' @export
#'
#' @examples
#' guess_admins(dc)
guess_admins <- function(shp) {

  to_guess <- c(
    'admin', 'subadmin',
    'county', 'region', 'province', 'division', 'parish', 'prefecture',
    'muni', 'town', 'municipality', 'county_muni',
    'vtd', 'voting_district', 'precinct'
  )

  noms <- names(shp)

  admin_cols <- noms[noms %in% to_guess]

  if (length(admin_cols) > 0) {
    admin_nas <- vapply(admin_cols, function(x) any(is.na(shp[[x]])), logical(1))
    admin_vals <- vapply(admin_cols, function(x) length(unique(shp[[x]])), numeric(1))

    list(
      admin    = admin_cols[which(!admin_nas & admin_vals > 1)],
      subadmin = admin_cols[which(admin_nas & admin_vals > 1)],
      multi    = admin_cols[which(!admin_nas & admin_vals > 1)],
      total    = admin_cols[which(!admin_nas & admin_vals > 1)]
    )
  } else {
    list(
      admin = NULL,
      subadmin = NULL,
      multi = NULL,
      total = NULL
    )
  }


}

#' Guess which columns contain election data
#'
#' @param shp an `sf` tibble that you want to draw with
#'
#' @return a named `list` of columns
#' @export
#'
#' @examples
#' guess_elections(dc)
guess_elections <- function(shp) {
  elecs <- shp |>
    sf::st_drop_geometry() |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::contains('_dem_'), dplyr::ends_with('_dem')) |>
    names() |>
    stringr::word(end = 2, sep = '_') |>
    unique()

  lapply(elecs, function(el) {
    vote_d <- shp |>
      dplyr::as_tibble() |>
      dplyr::select(
        dplyr::starts_with(paste0(el, "_dem")),
        dplyr::starts_with(paste0(el, "_rep"))
      )
    if (ncol(vote_d) != 2) {
      return(NULL)
    } else {
      list(
        dem = names(vote_d)[1],
        rep = names(vote_d)[2]
      )
    }
  }) |>
    stats::setNames(elecs) |>
    purrr::compact()
}

#' Guess and estimate which columns contain demographic data
#'
#' @param shp an `sf` tibble that you want to draw with
#'
#' @return a named `list` of columns
#' @export
#'
#' @examples
#' guesstimate_demographics(dc)
guesstimate_demographics <- function(shp) {

  shp <- shp |>
    sf::st_drop_geometry() |>
    dplyr::as_tibble()

  cols <- list()

  if ('pop' %in% names(shp)) {
    cols$pop <- shp |>
      dplyr::mutate(dplyr::across(dplyr::starts_with('pop_'), function(x) x / .data$pop, .names = 'pct_{col}')) |>
      dplyr::select(dplyr::starts_with('pct_pop'))
  }
  if ('vap' %in% names(shp)) {
    cols$vap <- shp |>
      dplyr::mutate(dplyr::across(dplyr::starts_with('vap_'), function(x) x / .data$vap, .names = 'pct_{col}')) |>
      dplyr::select(dplyr::starts_with('pct_vap'))
  }
  if ('cvap' %in% names(shp)) {
    cols$cvap <- shp |>
      dplyr::mutate(dplyr::across(dplyr::starts_with('cvap_'), function(x) x / .data$cvap, .names = 'pct_{col}')) |>
      dplyr::select(dplyr::starts_with('pct_cvap'))
  }

  if (length(cols) > 0) {
    cols |>
      dplyr::bind_cols()
  } else {
    NULL
  }
}

