undo_init <- function(max_undo = 10L) {
  list(
    undo = vector(mode = 'list', length = max_undo),
    #redo = integer(length = max_undo),
    max_undo = max_undo,
    undo_index = 0L
  )
}

undo_log <- function(l, pl) {
  if (l$undo_index == l$max_undo) {
    l$undo <- l$undo[-1L]
  } else {
    l$undo_index <- l$undo_index + 1L
  }

  l$undo[[l$undo_index]] <- pl

  l
}

undo_once <- function(l) {
  if (l$undo_index > 0) {
    l$undo_index <- l$undo_index - 1L
  }

  l
}
