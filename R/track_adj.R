init_edge_tracker <- function(edges_df) {
  tibble::tibble(
    i = edges_df$i,
    j = edges_df$j,
    original = TRUE,
    shown = TRUE
  )
}

add_edge_to_tracker <- function(tracker, i, j) {
  existing_idx <- which(tracker$i == min(i, j) & tracker$j == max(i, j))

  if (length(existing_idx) > 0) {
    tracker$shown[existing_idx] <- TRUE
  } else {
    new_row <- tibble::tibble_row(
      i = i,
      j = j,
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
