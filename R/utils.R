prep_palette <- function(palette = NULL, ndists) {
  if (is.null(palette)) {
    if (rlang::is_installed('crayons')) {
      palette <- crayons::crayons$no_48
    } else {
      palette <- suppressWarnings(grDevices::palette.colors(
        n = ndists,
        'Polychrome 36'
      ))
    }
  }
  palette <- unname(as.character(palette))
  if (length(palette) != ndists) {
    if (length(palette) > ndists) {
      palette <- palette[seq_len(ndists)]
    } else {
      if (length(palette) < 1) {
        stop('`palette` must have at least one color.')
      }
      palette <- rep(palette, ceiling(ndists / length(palette)))[seq_len(
        ndists
      )]
    }
  }
  palette
}
