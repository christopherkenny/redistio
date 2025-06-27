update_shape_style <- function(leafl, fc, pal, rcp, shp, opac = 0.8, wt = 0.5) {
  if (fc == 'District') {
    leafl |>
      mapgl::set_paint_property(
        layer_id = 'precinct_fill',
        name = 'fill-color',
        value = pal(shp[[fc]])
      ) |>
      mapgl::set_paint_property(
        layer_id = 'precinct_fill',
        name = 'fill-opacity',
        value = opac
      ) |>
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
        value = pal(shp[[fc]])
      ) |>
      mapgl::set_paint_property(
        layer_id = 'precinct_fill',
        name = 'fill-opacity',
        value = opac
      ) |>
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
        pal = pal,
        values = shp[[fc]],
        title = fc,
        opacity = opac,
        position = 'bottomright',
        layer_id = 'legend'
      )
  }
}
