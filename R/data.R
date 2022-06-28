#' District of Columbia Geographic Data
#'
#' This file contains demographic, partisan, and geographic data for DC at the
#' voting district level.
#'
#' @name dc
#' @concept data
#' @format `sf` object
#' \describe{
#' \item{\code{GEOID20}}{US Census Geographic Identifier}
#' \item{\code{state}}{state abbreviation}
#' \item{\code{county}}{county name}
#' \item{\code{vtd}}{voting district identifier}
#' \item{\code{pop}}{total population}
#' \item{\code{pop_hisp}}{Hispanic population}
#' \item{\code{pop_white}}{White, non-Hispanic population}
#' \item{\code{pop_black}}{Black, non-Hispanic population}
#' \item{\code{pop_aian}}{American Indian and Alaskan Native, non-Hispanic population}
#' \item{\code{pop_asian}}{Asian, non-Hispanic population}
#' \item{\code{pop_nhpi}}{Native Hawaiian and Pacific Islander, non-Hispanic population}
#' \item{\code{pop_other}}{Other, non-Hispanic population}
#' \item{\code{pop_two}}{Two or More Races, non-Hispanic population}
#' \item{\code{vap}}{voting age population}
#' \item{\code{vap_hisp}}{Hispanic voting age population}
#' \item{\code{vap_white}}{White, non-Hispanic voting age population}
#' \item{\code{vap_black}}{Black, non-Hispanic voting age population}
#' \item{\code{vap_aian}}{American Indian and Alaskan Native, non-Hispanic voting age population}
#' \item{\code{vap_asian}}{Asian, non-Hispanic voting age population}
#' \item{\code{vap_nhpi}}{Native Hawaiian and Pacific Islander, non-Hispanic voting age population}
#' \item{\code{vap_other}}{Other, non-Hispanic voting age population}
#' \item{\code{vap_two}}{Two or More Races, non-Hispanic voting age population}
#' \item{\code{pre_16_dem_cli}}{votes for Clinton 2016, President (D)}
#' \item{\code{pre_16_rep_tru}}{votes for Trump 2016, President (R)}
#' \item{\code{uss_18_dem_bro}}{votes for Brown 2018, Shadow Senate (D)}
#' \item{\code{atg_18_dem_rac}}{votes for Karl Racine, Attorney General (D)}
#' \item{\code{pre_20_dem_bid}}{votes for Biden 2020, President (D)}
#' \item{\code{pre_20_rep_tru}}{votes for Trump 2020, President (R)}
#' \item{\code{uss_20_dem_str}}{votes for Strauss 2020, Shadow Senate (D)}
#' \item{\code{uss_20_rep_wei}}{votes for Weiss 2020, Shadow Senate (R)}
#' \item{\code{arv_16}}{average Republican vote in 2016}
#' \item{\code{adv_16}}{average Democratic vote in 2016}
#' \item{\code{arv_18}}{average Republican vote in 2018}
#' \item{\code{adv_18}}{average Democratic vote in 2016}
#' \item{\code{arv_20}}{average Republican vote in 2020}
#' \item{\code{adv_20}}{average Democratic vote in 2016}
#' \item{\code{nrv}}{normal Republican vote}
#' \item{\code{ndv}}{normal Democratic vote}
#' \item{\code{geometry}}{sf geometry}
#' \item{\code{adv_nbr}}{Advisory Neighborhood names estimated to VTDs}
#' \item{\code{ward}}{2022 ward lines estimated to VTDs}
#' }
#'
#' @examples
#' data(dc)
NULL
