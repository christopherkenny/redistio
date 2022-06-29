setShapeFillColor <- function(map, data = leaflet::getMapData(map), layerId, fillColor) {

  options <- list(list(layerId = layerId))
  if (!is.null(fillColor)) options$fillColor <- fillColor
  options <- leaflet::evalFormula(options, data = data)
  options <- dplyr::bind_cols(options)
  layerId <- dplyr::select(options, .data$layerId)
  style <- dplyr::select(options, -.data$layerId)

  leaflet::invokeMethod(map = map, data = data, method = 'setStyle',
                        'shape', layerId, style)
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
