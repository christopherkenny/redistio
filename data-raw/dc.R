library(tidyverse)
library(geomander)
dc <- get_alarm('DC')

dc <- rmapshaper::ms_simplify(
  input = dc,
  keep = 0.1,
  keep_shapes = TRUE
)

adv_nbr <- sf::read_sf('https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Administrative_Other_Boundaries_WebMercator/MapServer/54/query?outFields=*&where=1%3D1&f=geojson')
adv_nbr <- adv_nbr |> arrange(NAME)

# district <- sf::read_sf('https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Administrative_Other_Boundaries_WebMercator/MapServer/55/query?outFields=*&where=1%3D1&f=geojson')
# district <- district |> arrange(NAME)

wards <- sf::read_sf('https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Administrative_Other_Boundaries_WebMercator/MapServer/53/query?outFields=*&where=1%3D1&f=geojson')
wards <- wards |> arrange(NAME)

dc <- dc |>
  mutate(
    adv_nbr = adv_nbr$NAME[geo_match(from = ., to = adv_nbr, method = 'area')],
    # district = district$NAME[geo_match(from = ., to = district, method = 'area')],
    ward = geo_match(from = ., to = wards, method = 'area')
  )

dc <- sf::st_transform(dc, 4326)

usethis::use_data(dc, overwrite = TRUE, compress = 'xz')
