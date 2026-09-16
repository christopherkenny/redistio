init_edge_tracker <- function(edges_df) {
  tibble::tibble(
    i = edges_df$i,
    j = edges_df$j,
    original = TRUE,
    shown = TRUE
  )
}

add_edge_to_tracker <- function(tracker, i, j) {
  edge_i <- min(i, j)
  edge_j <- max(i, j)
  existing_idx <- which(tracker$i == edge_i & tracker$j == edge_j)

  if (length(existing_idx) > 0) {
    tracker$shown[existing_idx] <- TRUE
  } else {
    new_row <- tibble::tibble_row(
      i = edge_i,
      j = edge_j,
      original = FALSE,
      shown = TRUE
    )
    tracker <- tracker |>
      tibble::add_row(new_row)
  }

  tracker
}

remove_edge_from_tracker <- function(tracker, i, j) {
  edge_idx <- which(tracker$i == min(i, j) & tracker$j == max(i, j))

  if (length(edge_idx) > 0) {
    tracker$shown[edge_idx] <- FALSE
  }

  tracker
}

check_edge_state <- function(tracker, i, j) {
  edge_idx <- which(tracker$i == min(i, j) & tracker$j == max(i, j))

  if (length(edge_idx) == 0) {
    list(
      exists = FALSE,
      original = FALSE,
      shown = FALSE
    )
  } else {
    list(
      exists = TRUE,
      original = tracker$original[edge_idx],
      shown = tracker$shown[edge_idx]
    )
  }
}

edge_layer_id <- function(i, j) {
  edge <- sort(c(i, j))
  paste0(edge[1], '-', edge[2])
}

get_hidden_original_edge_ids <- function(tracker) {
  tracker |>
    dplyr::filter(.data$original, !.data$shown) |>
    dplyr::mutate(
      id = paste0(pmin(.data$i, .data$j), '-', pmax(.data$i, .data$j))
    ) |>
    dplyr::pull('id')
}

build_edge_visibility_filter <- function(tracker) {
  hidden_edges <- get_hidden_original_edge_ids(tracker)
  if (length(hidden_edges) == 0L) {
    return(NULL)
  }

  list(
    'match',
    mapgl::get_column('line_id'),
    as.list(hidden_edges),
    FALSE,
    TRUE
  )
}
