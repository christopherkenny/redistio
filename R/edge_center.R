edge_center_df <- function(shp, adj) {
  # Extract Centers
  suppressWarnings(centers <- sf::st_centroid(shp))
  sf::st_crs(centers) <- sf::st_crs(shp)
  cent_geom <- sf::st_geometry(centers)

  if (nrow(shp) == 1) {
    return(list(nb = NULL, centers = centers))
  }

  # Extract Edges
  nb <- lapply(adj, function(x) {
    x + 1L
  })

  edgedf <- tibble::tibble(
    start = rep(seq_along(nb), lengths(nb)),
    finish = unlist(nb)
  )
  edgedf <- edgedf |>
    dplyr::mutate(i = pmin(.data$start, .data$finish), j = pmax(.data$start, .data$finish)) |>
    dplyr::select(dplyr::all_of(c('i', 'j')))
  edgedf <- edgedf[!duplicated(edgedf), ]

  geoms <- lapply(seq_len(nrow(edgedf)), function(x) {
    sf::st_linestring(
      matrix(
        c(
          as.numeric(cent_geom[[edgedf$i[x]]]),
          as.numeric(cent_geom[[edgedf$j[x]]])
        ),
        nrow = 2,
        byrow = TRUE
      )
    )
  })

  edgedf <- edgedf |>
    dplyr::mutate(
      line_id = paste0(.data$i, '-', .data$j),
      geometry = sf::st_sfc(geoms)
    )

  suppressWarnings(nb <- sf::st_as_sf(edgedf))
  suppressWarnings(sf::st_crs(nb) <- sf::st_crs(shp))

  list(nb = nb, centers = centers)
}

new_single_edge <- function(centers, i, j) {
  geoms <- lapply(seq_along(i), function(x) {
    mat <- matrix(
      c(
        as.numeric(sf::st_geometry(centers)[[as.integer(i)[x]]]),
        as.numeric(sf::st_geometry(centers)[[as.integer(j)[x]]])
      ),
      nrow = 2,
      byrow = TRUE
    )
    sf::st_linestring(mat)
  })

  dplyr::tibble(
    i = i,
    j = j
  ) |>
    dplyr::mutate(geometry = sf::st_sfc(geoms)) |>
    sf::st_as_sf() |>
    sf::st_set_crs(sf::st_crs(centers))
}
