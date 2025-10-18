log_adj <- function(len = 50L) {
  list(
    action = character(length = len),
    pair = matrix(NA_integer_, nrow = len, ncol = 2),
    comment = character(length = len)
  )
}

log_adj_update <- function(l, act, p, comment = '', len = 50L) {
  n <- which(l$action == '')
  if (length(n) == 0) {
    n <- length(l$action) + 1L
    l$action <- c(l$action, character(length = len))
    l$pair <- rbind(l$pair, matrix(NA_integer_, nrow = len, ncol = 2))
    l$comment <- c(l$comment, character(length = len))
  } else {
    n <- min(n)
  }
  l$action[n] <- act
  l$pair[n, ] <- p
  l$comment[n] <- comment
  l
}
