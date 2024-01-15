# adapted from leaflet contributors
setShapeStyle <- function(map, data = leaflet::getMapData(map), layerId,
                          stroke = NULL, color = NULL,
                          weight = NULL, opacity = NULL,
                          fill = NULL, fillColor = NULL,
                          fillOpacity = NULL, dashArray = NULL,
                          smoothFactor = NULL, noClip = NULL,
                          options = NULL) {
  options <- c(
    list(layerId = layerId),
    options,
    leaflet::filterNULL(
      list(
        stroke = stroke, color = color,
        weight = weight, opacity = opacity,
        fill = fill, fillColor = fillColor,
        fillOpacity = fillOpacity, dashArray = dashArray,
        smoothFactor = smoothFactor, noClip = noClip
      )
    )
  )
  # evaluate all options
  options <- leaflet::evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))

  layerId <- options[[1]]
  style <- options[-1] # drop layer column

  # print(list(style=style))
  leaflet::invokeMethod(map, data, 'setStyle', 'shape', layerId, style)
}

the_javascripts <- shiny::tags$head(
  shiny::tags$script(
    shiny::HTML(
      '
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);
  //console.log(style);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
        layer.setStyle(style[i]);
    }
  });
};
'
    )
  )
)

update_shape_style <- function(leafl, fc, pal, rcp, shp, opac = 0.8) {
  if (fc == 'District') {
    leafl |>
      setShapeStyle(
        # data = shp,
        layerId = ~redistio_id,
        # line colors
        stroke = TRUE,
        weight = 0.5,
        color = '#000',
        # fill control
        fillOpacity = opac,
        fillColor = ~ pal(rcp)
      ) |>
      leaflet::removeControl('legend')
  } else {
    leafl |>
      setShapeStyle(
        # data = shp,
        layerId = ~redistio_id,
        # line colors
        stroke = TRUE,
        weight = 0.5,
        color = '#000',
        # fill control
        fillOpacity = opac,
        fillColor = ~ pal(shp[[fc]])
      ) |>
      leaflet::removeControl('legend') |>
      leaflet::addLegend(
        pal = pal,
        values = shp[[fc]],
        title = fc,
        opacity = opac,
        position = 'bottomright',
        layerId = 'legend'
      )
  }
}
