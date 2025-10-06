log_adj <- function(len = 100L) {
  list(
    action = character(length = len),
    pair = matrix(NA_integer_, nrow = len, ncol = 2)
  )
}

log_adj_update <- function(l, act, p, len = 100L) {
  n <- min(which(!is.na(l$action)))
  if (length(n) == 0) {
    n <- length(l$action) + 1L
    l$action <- c(l$action, character(length = len))
    l$pair <- rbind(l$pair, matrix(NA_integer_, nrow = len, ncol = 2))
  }
  l$action[n] <- act
  l$pair[n, ] <- p
}
