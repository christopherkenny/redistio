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
