distr_pop <- function(pop, total, plan, ndists) {
  pops <- vapply(
    X = seq_len(ndists),
    FUN = function(x) sum(pop[which(plan == x)]),
    FUN.VALUE = double(1)
  )
  c(total - sum(pops), pops)
}
