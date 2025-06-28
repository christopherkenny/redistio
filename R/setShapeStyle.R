update_shape_style <- function(leafl, fc, pal, rcp, shp, opac = 0.8, wt = 0.5) {
  if (fc == 'District') {
    leafl |>
      mapgl::set_paint_property(
        layer_id = 'precinct_fill',
        name = 'fill-color',
        value = discrete_palette(pal, rcp)
      ) |>
      mapgl::set_paint_property(
        layer_id = 'precinct_fill',
        name = 'fill-opacity',
        value = opac
      ) |>
      # TODO: this doesn't currently match onto anything for mapgl
      mapgl::set_paint_property(
        layer_id = 'precinct_fill',
        name = 'line-width',
        value = wt
      ) |>
      mapgl::set_paint_property(
        layer_id = 'precinct_fill',
        name = 'line-color',
        value = '#000'
      ) |>
      mapgl::clear_legend()
  } else {
    leafl |>
      mapgl::set_paint_property(
        layer_id = 'precinct_fill',
        name = 'fill-color',
        value = pal$expression
      ) |>
      mapgl::set_paint_property(
        layer_id = 'precinct_fill',
        name = 'fill-opacity',
        value = opac
      ) |>
      # TODO: this doesn't currently match onto anything for mapgl
      mapgl::set_paint_property(
        layer_id = 'precinct_fill',
        name = 'line-width',
        value = wt
      ) |>
      mapgl::set_paint_property(
        layer_id = 'precinct_fill',
        name = 'line-color',
        value = '#000'
      ) |>
      mapgl::clear_legend() |>
      mapgl::add_legend(
        legend_title = '',
        values = get_simple_legend_labels(pal),
        colors = get_simple_legend_colors(pal),
        type = 'continuous'
      )
  }
}
