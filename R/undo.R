undo_init <- function(max_undo = 10L) {
  list(
    undo = integer(length = max_undo),
    undo_id = integer(length = max_undo),
    #redo = integer(length = max_undo),
    max_undo = max_undo,
    undo_index = 0L
  )
}

undo_log <- function(l, i, id) {
  if (l$undo_index == l$max_undo) {
    l$undo[seq_len(l$max_undo - 1L)] <- l$undo[-1L]
    l$undo_id[seq_len(l$max_undo - 1L)] <- l$undo_id[-1L]
  } else {
    l$undo_index <- l$undo_index + 1L
  }

  l$undo[l$undo_index] <- i
  l$undo_id[l$undo_index] <- id

  l
}

undo_once <- function(l) {
  if (l$undo_index > 0) {
    l$undo_index <- l$undo_index - 1L
  }

  l
}
