#' Add summary statistics to plans
#'
#' @param plans a `redist_plans` object
#' @param ref_plan an integer vector of district assignments
#' @param map a `redist_map` object. Required if `plans` contains summary statistics.
#' @param name name for the reference plan. Defaults to `"ref"`.
#' @param ... additional arguments (currently ignored)
#'
#' @return a `redist_plans` object with the reference plan added
#' @export
#'
#' @examples
#' dc_map <- redist::redist_map(dc, existing_plan = ward)
#' plans <- redist::redist_smc(dc_map, nsims = 10)
#' plans <- add_plan_stats(plans, dc_map$ward, map = dc_map, name = 'example')
add_plan_stats <- function(plans, ref_plan, map = NULL, name = NULL, ...) {
  if (!inherits(plans, 'redist_plans')) {
    stop('`plans` must be a `redist_plans` object.')
  }

  if (is.null(name)) {
    name <- 'ref'
  }

  if (!is.numeric(ref_plan)) {
    stop('`ref_plan` must be a numeric vector of district assignments.')
  }

  n_prec <- nrow(redist::get_plans_matrix(plans))
  if (length(ref_plan) != n_prec) {
    stop('`ref_plan` must have the same number of precincts as `plans`.')
  }

  if (dplyr::n_distinct(ref_plan) != dplyr::n_distinct(plans$district)) {
    stop('`ref_plan` must have the same number of districts as `plans`.')
  }

  # renumber if needed
  if (max(ref_plan, na.rm = TRUE) != dplyr::n_distinct(ref_plan)) {
    ref_plan <- match(ref_plan, unique(sort(ref_plan, na.last = TRUE)))
  }

  # if no summary stats, just add the reference
  existing_cols <- names(tibble::as_tibble(plans))
  base_cols <- c('draw', 'district')
  stat_cols <- setdiff(existing_cols, base_cols)

  if (length(stat_cols) == 0) {
    return(redist::add_reference(plans, ref_plan, name))
  }

  if (is.null(map)) {
    stop('`map` must be provided to calculate summary statistics for the reference plan.')
  }

  # create a single-plan redist_plans from ref_plan
  ref_plans <- redist::redist_plans(
    plans = ref_plan, map = map,
    algorithm = attr(plans, 'algorithm')
  )

  # compute matching statistics
  ref_plans <- calc_ref_stats(ref_plans, map, stat_cols)

  # set the draw name
  ref_plans$draw <- name

  # preserve attributes
  if ('chain' %in% existing_cols) {
    ref_plans$chain <- NA
  }
  attr(ref_plans, 'resampled') <- attr(plans, 'resampled')
  attr(ref_plans, 'compactness') <- attr(plans, 'compactness')
  attr(ref_plans, 'constraints') <- attr(plans, 'constraints')

  if (is.null(attr(plans, 'ndists'))) {
    attr(plans, 'ndists') <- max(as.matrix(plans)[, 1])
  }
  attr(ref_plans, 'ndists') <- attr(plans, 'ndists')

  # combine
  new_plans <- rbind(ref_plans, plans)
  m <- redist::get_plans_matrix(ref_plans)
  colnames(m)[1] <- name
  attr(new_plans, 'plans') <- cbind(m, redist::get_plans_matrix(plans))
  new_plans$draw <- factor(new_plans$draw, levels = unique(new_plans$draw))

  new_plans
}

#' Calculate reference plan statistics to match existing plans columns
#'
#' @param ref_plans a single-plan redist_plans object
#' @param map a redist_map object
#' @param stat_cols character vector of statistic column names to compute
#'
#' @return the ref_plans with matching statistics added
#' @noRd
calc_ref_stats <- function(ref_plans, map, stat_cols) {
  map_tb <- tibble::as_tibble(map)

  # population parity
  if ('plan_dev' %in% stat_cols) {
    ref_plans <- ref_plans |>
      dplyr::mutate(plan_dev = redist::plan_parity(map))
  }

  # total_vap
  if ('total_vap' %in% stat_cols && 'vap' %in% names(map)) {
    ref_plans <- ref_plans |>
      dplyr::mutate(total_vap = redist::tally_var(map, .data$vap))
  }

  # compactness - edge fraction kept
  if ('comp_edge' %in% stat_cols) {
    ref_plans <- ref_plans |>
      dplyr::mutate(
        comp_edge = redistmetrics::comp_frac_kept(plans = redist::pl(), map)
      )
  }

  # compactness - polsby popper
  if ('comp_polsby' %in% stat_cols) {
    ref_plans <- ref_plans |>
      dplyr::mutate(
        comp_polsby = redistmetrics::comp_polsby(plans = redist::pl(), map)
      )
  }

  # tally columns: pop_*, vap_*, *_dem_*, *_rep_*, adv_*, arv_*
  tally_patterns <- c('^pop_', '^vap_', '_dem_', '_rep_', '^adv_', '^arv_')
  for (col in stat_cols) {
    if (col %in% names(ref_plans)) next
    if (col %in% names(map) && any(vapply(tally_patterns, function(p) grepl(p, col), logical(1)))) {
      ref_plans <- ref_plans |>
        dplyr::mutate(!!col := redist::tally_var(map, map[[col]]))
    }
  }

  # ndv / nrv
  if ('ndv' %in% stat_cols && 'ndv' %in% names(map) && !'ndv' %in% names(ref_plans)) {
    ref_plans <- ref_plans |>
      dplyr::mutate(ndv = redist::tally_var(map, .data$ndv))
  }
  if ('nrv' %in% stat_cols && 'nrv' %in% names(map) && !'nrv' %in% names(ref_plans)) {
    ref_plans <- ref_plans |>
      dplyr::mutate(nrv = redist::tally_var(map, .data$nrv))
  }

  # ndshare
  if ('ndshare' %in% stat_cols && 'ndv' %in% names(ref_plans) && 'nrv' %in% names(ref_plans)) {
    ref_plans <- ref_plans |>
      dplyr::mutate(ndshare = .data$ndv / (.data$ndv + .data$nrv))
  }

  # partisan metrics from elections
  dem_cols <- stat_cols[grepl('^e_dvs$', stat_cols)]
  if ('e_dvs' %in% stat_cols) {
    # find election columns in map
    elecs <- names(map)[grepl('_dem_', names(map))]
    elec_prefixes <- unique(substr(elecs, 1, 6))

    if (length(elec_prefixes) > 0) {
      elect_results <- lapply(elec_prefixes, function(el) {
        d_col <- names(map)[grepl(paste0('^', el, '_dem_'), names(map))]
        r_col <- names(map)[grepl(paste0('^', el, '_rep_'), names(map))]
        if (length(d_col) != 1 || length(r_col) != 1) {
          return(NULL)
        }

        dvote <- map_tb[[d_col]]
        rvote <- map_tb[[r_col]]

        ref_plans |>
          dplyr::mutate(
            dem = redist::group_frac(map, dvote, dvote + rvote)
          ) |>
          tibble::as_tibble() |>
          dplyr::group_by(.data$draw) |>
          dplyr::transmute(
            draw = .data$draw,
            district = .data$district,
            e_dvs = .data$dem,
            pr_dem = .data$dem > 0.5,
            e_dem = sum(.data$dem > 0.5, na.rm = TRUE)
          )
      })

      elect_results <- Filter(Negate(is.null), elect_results)
      if (length(elect_results) > 0) {
        elect_tb <- do.call(dplyr::bind_rows, elect_results) |>
          dplyr::group_by(.data$draw, .data$district) |>
          dplyr::summarize(dplyr::across(dplyr::everything(), mean), .groups = 'drop')
        ref_plans <- dplyr::left_join(
          tibble::as_tibble(ref_plans), elect_tb,
          by = c('draw', 'district')
        )
        # restore class
        class(ref_plans) <- c('redist_plans', class(ref_plans))
      }
    }
  }

  # egap and pbias
  if (any(c('egap', 'pbias') %in% stat_cols)) {
    elecs <- names(map)[grepl('_dem_', names(map))]
    elec_prefixes <- unique(substr(elecs, 1, 6))

    if (length(elec_prefixes) > 0) {
      for (el in elec_prefixes) {
        d_col <- names(map)[grepl(paste0('^', el, '_dem_'), names(map))]
        r_col <- names(map)[grepl(paste0('^', el, '_rep_'), names(map))]
        if (length(d_col) != 1 || length(r_col) != 1) next

        dvote <- map_tb[[d_col]]
        rvote <- map_tb[[r_col]]

        if ('egap' %in% stat_cols && !'egap' %in% names(ref_plans)) {
          ref_plans <- ref_plans |>
            dplyr::mutate(
              egap = redistmetrics::part_egap(
                plans = redist::pl(), shp = map, rvote = rvote, dvote = dvote
              )
            )
        }
        if ('pbias' %in% stat_cols && !'pbias' %in% names(ref_plans)) {
          ref_plans <- ref_plans |>
            dplyr::mutate(
              pbias = redistmetrics::part_bias(
                plans = redist::pl(), shp = map, rvote = rvote, dvote = dvote
              )
            )
        }
        break # only use first election
      }
    }
  }

  # county/muni splits
  if ('county_splits' %in% stat_cols && 'county' %in% names(map)) {
    ref_plans <- ref_plans |>
      dplyr::mutate(
        county_splits = redistmetrics::splits_admin(
          plans = redist::pl(), map, .data$county
        )
      )
  }
  if ('muni_splits' %in% stat_cols && 'muni' %in% names(map)) {
    ref_plans <- ref_plans |>
      dplyr::mutate(
        muni_splits = redistmetrics::splits_sub_admin(
          plans = redist::pl(), map, .data$muni
        )
      )
  }

  # ensure all stat_cols are present (fill with NA for any we couldn't compute)
  for (col in stat_cols) {
    if (!col %in% names(ref_plans)) {
      ref_plans[[col]] <- NA_real_
    }
  }

  # reorder columns to match
  all_cols <- c('draw', 'district', stat_cols)
  present_cols <- intersect(all_cols, names(ref_plans))
  ref_plans <- ref_plans[, present_cols]

  ref_plans
}
