distr_pop <- function(pop, total, plan, ndists) {
  pops <- vapply(
    X = seq_len(ndists),
    FUN = function(x) sum(pop[which(plan == x)]),
    FUN.VALUE = double(1)
  )
  c(total - sum(pops), pops)
}

generate_lock_icons <- function(ndists, locked_districts) {
  c(
    '',
    vapply(seq_len(ndists), function(i) {
      locked <- as.character(i) %in% locked_districts
      icon_char <- if (locked) '\U0001F512' else '\U0001F513'
      paste0(
        "<span class='lock-toggle' data-district='", i,
        "' onmousedown='event.stopPropagation()'",
        " onclick='Shiny.setInputValue(\"lock_click\", {district: ", i,
        ", time: Date.now()})'>",
        icon_char,
        '</span>'
      )
    }, character(1))
  )
}

# Keep only geometry and id column for mapgl rendering
# This dramatically reduces data sent to the JavaScript layer
create_mapgl_source <- function(shp, id_col = 'redistio_id') {
  shp[, id_col]
}
