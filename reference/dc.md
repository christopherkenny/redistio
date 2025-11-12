# District of Columbia Geographic Data

This file contains demographic, partisan, and geographic data for DC at
the voting district level.

## Format

`sf` object

- `GEOID20`:

  US Census Geographic Identifier

- `state`:

  state abbreviation

- `county`:

  county name

- `vtd`:

  voting district identifier

- `pop`:

  total population

- `pop_hisp`:

  Hispanic population

- `pop_white`:

  White, non-Hispanic population

- `pop_black`:

  Black, non-Hispanic population

- `pop_aian`:

  American Indian and Alaskan Native, non-Hispanic population

- `pop_asian`:

  Asian, non-Hispanic population

- `pop_nhpi`:

  Native Hawaiian and Pacific Islander, non-Hispanic population

- `pop_other`:

  Other, non-Hispanic population

- `pop_two`:

  Two or More Races, non-Hispanic population

- `vap`:

  voting age population

- `vap_hisp`:

  Hispanic voting age population

- `vap_white`:

  White, non-Hispanic voting age population

- `vap_black`:

  Black, non-Hispanic voting age population

- `vap_aian`:

  American Indian and Alaskan Native, non-Hispanic voting age population

- `vap_asian`:

  Asian, non-Hispanic voting age population

- `vap_nhpi`:

  Native Hawaiian and Pacific Islander, non-Hispanic voting age
  population

- `vap_other`:

  Other, non-Hispanic voting age population

- `vap_two`:

  Two or More Races, non-Hispanic voting age population

- `pre_16_dem_cli`:

  votes for Clinton 2016, President (D)

- `pre_16_rep_tru`:

  votes for Trump 2016, President (R)

- `uss_18_dem_bro`:

  votes for Brown 2018, Shadow Senate (D)

- `atg_18_dem_rac`:

  votes for Karl Racine, Attorney General (D)

- `pre_20_dem_bid`:

  votes for Biden 2020, President (D)

- `pre_20_rep_tru`:

  votes for Trump 2020, President (R)

- `uss_20_dem_str`:

  votes for Strauss 2020, Shadow Senate (D)

- `uss_20_rep_wei`:

  votes for Weiss 2020, Shadow Senate (R)

- `arv_16`:

  average Republican vote in 2016

- `adv_16`:

  average Democratic vote in 2016

- `arv_18`:

  average Republican vote in 2018

- `adv_18`:

  average Democratic vote in 2016

- `arv_20`:

  average Republican vote in 2020

- `adv_20`:

  average Democratic vote in 2016

- `nrv`:

  normal Republican vote

- `ndv`:

  normal Democratic vote

- `geometry`:

  sf geometry

- `adv_nbr`:

  Advisory Neighborhood names estimated to VTDs

- `ward`:

  2022 ward lines estimated to VTDs

## Examples

``` r
data(dc)
```
