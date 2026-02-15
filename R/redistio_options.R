#' Set options for `redistio`
#'
#' @param theme a name of a bootswatch preset theme or other `bslib::bs_theme()` object
#' @param panels which panels to display in the app, `'draw'` is always shown.
#' @param select_color a color to use for highlighting selected districts
#' @param border_color a color to use for precinct borders in the map
#' @param palette_pop a color palette to use for whole people. Defaults to
#' `RColorBrewer::brewer.pal(n = 3, name = 'Purples')`.
#' @param palette_pct a color palette to use for percentages of people. Defaults
#' to `RColorBrewer::brewer.pal(n = 3, name = 'PuOr')`.
#' @param palette_party a color palette to use for parties
#' @param map_tiles a tileset to use for the map background, from `leaflet::providers`
#' @param leaflet_height height to pass to `leaflet::leafletOutput()`
#' @param crs a coordinate reference system to use in `leaflet::leaflet()`
#' @param na_color a color to use for unassigned precincts
#' @param layer_weight a stroke width to use for layers in `leaflet::leaflet()`
#' @param layer_color colors to use for layers in `mapgl::maplibre()`
#' @param locked_districts districts to lock on app start to stop edits
#' @param use_algorithms whether to use redistricting simulation algorithms
#' @param alg_max_districts maximum number of districts to use in algorithms
#' @param alg_max_sims maximum number of simulations to use in algorithms
#' @param use_plans whether to show the plans browser tab when `plans` is provided
#' @param use_comparisons whether to show the comparisons tab when `plans` and `plans_fn` are provided
#' @param use_planscore whether to use PlanScore to evaluate plans
#' @param save_assignment_path Output path to save assignment file to.
#' @param save_shape_path Output path to save shapefile to.
#' @param debounce Number of milliseconds to debounce map hover events. Defaults
#' to 150.
#' @param projection Maplibre projection to use. Default is `'mercator'`.
#' @param plot_theme a ggplot2 theme to apply to comparison plots. Default is
#' `ggplot2::theme_bw()`.
#' @param plot_geom a geom to use for district-level comparison plots, passed
#' to the `geom` argument of [redist::redist.plot.distr_qtys()]. Can be a
#' string like `'jitter'` (default) or a ggplot2 geom function like
#' `ggplot2::geom_violin`.
#' @param plot_ref_geom a geom function to use for the reference plan overlay
#' in district-level comparison plots, passed to the `ref_geom` argument of
#' [redist::redist.plot.distr_qtys()]. Should be a function that accepts `...`.
#' @param ... additional arguments (currently ignored)
#'
#' @return a `list`
#' @export
#'
#' @examples
#' redistio_options()
redistio_options <- function(theme = 'flatly',
                             panels = c('elections', 'demographics', 'integrity', 'algorithms', 'plans', 'comparisons'),
                             select_color = 'purple',
                             border_color = '#000000',
                             palette_pop = 'Purples',
                             palette_pct = 'PuOr',
                             palette_party = ggredist::ggredist$partisan,
                             map_tiles = mapgl::carto_style('voyager'),
                             leaflet_height = '91vh', crs = 4326,
                             na_color = '#0000',
                             layer_weight = 1.5, layer_color = '#000000',
                             locked_districts = NULL,
                             use_algorithms = TRUE, alg_max_districts = 3,
                             alg_max_sims = 100,
                             use_plans = TRUE,
                             use_comparisons = TRUE,
                             use_planscore = TRUE,
                             save_assignment_path = 'redistio.csv',
                             save_shape_path = 'redistio.geojson',
                             debounce = 150,
                             projection = 'mercator',
                             plot_theme = ggplot2::theme_bw(),
                             plot_geom = 'boxplot',
                             plot_ref_geom = NULL,
                             ...) {
  if (length(palette_pop) == 1) {
    palette_pop <- RColorBrewer::brewer.pal(n = 3, name = palette_pop)
  }
  if (length(palette_pct) == 1) {
    palette_pct <- RColorBrewer::brewer.pal(n = 3, name = palette_pct)
  }
  if (!is.numeric(debounce) || length(debounce) != 1 || debounce < 0) {
    stop('`debounce` must be a single nonnegative number.')
  }

  list(
    theme = theme,
    panels = panels,
    select_color = select_color,
    border_color = border_color,
    palette_pop = palette_pop,
    palette_pct = palette_pct,
    palette_party = palette_party,
    map_tiles = map_tiles,
    leaflet_height = leaflet_height,
    crs = crs,
    na_color = na_color,
    layer_weight = layer_weight,
    layer_color = layer_color,
    locked_districts = locked_districts,
    use_algorithms = use_algorithms,
    alg_max_districts = alg_max_districts,
    alg_max_sims = alg_max_sims,
    use_plans = use_plans,
    use_comparisons = use_comparisons,
    use_planscore = use_planscore,
    save_assignment_path = save_assignment_path,
    save_shape_path = save_shape_path,
    debounce = debounce,
    projection = projection,
    plot_theme = plot_theme,
    plot_geom = plot_geom,
    plot_ref_geom = plot_ref_geom
  )
}
