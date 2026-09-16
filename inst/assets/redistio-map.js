(function () {
  function findMap(id) {
    var widget = HTMLWidgets.find('#' + id);
    if (!widget || !widget.getMap) {
      return null;
    }
    return widget.getMap();
  }

  function withMap(message, callback, attempt) {
    var map = findMap(message.id);
    if (map && map.getLayer(message.layer_id || 'precinct_fill')) {
      callback(map);
      return;
    }
    if (attempt < 100) {
      window.setTimeout(function () {
        withMap(message, callback, attempt + 1);
      }, 100);
    }
  }

  Shiny.addCustomMessageHandler(
    'redistio-set-district-state',
    function (message) {
      var map = findMap(message.id);
      if (!map) {
        return;
      }

      for (var i = 0; i < message.feature_ids.length; i += 1) {
        map.setFeatureState(
          {
            source: message.source,
            id: message.feature_ids[i]
          },
          { district: message.districts[i] }
        );
      }
    }
  );

  Shiny.addCustomMessageHandler('redistio-enable-hover', function (message) {
    withMap(
      message,
      function (map) {
        var element = document.getElementById(message.id);
        var previous = element.redistioHover;
        if (previous) {
          map.off('mousemove', previous.layerId, previous.mousemove);
          map.off('mouseleave', previous.layerId, previous.mouseleave);
          window.clearTimeout(previous.timer);
        }

        var layerId = message.layer_id;
        var delay = Math.max(0, Number(message.delay) || 0);
        var lastId = null;
        var pending = null;
        var timer = null;
        var state = null;

        function emitHover() {
          timer = null;
          state.timer = null;
          if (!pending || pending.redistioId === lastId) {
            pending = null;
            return;
          }

          lastId = pending.redistioId;
          Shiny.setInputValue(
            message.id + '_feature_hover',
            {
              id: pending.id,
              properties: { redistio_id: pending.redistioId },
              layer: layerId
            },
            { priority: 'event' }
          );
          pending = null;
        }

        function mousemove(event) {
          if (!event.features || event.features.length === 0) {
            return;
          }
          var feature = event.features[0];
          var redistioId = String(feature.properties.redistio_id);
          if (redistioId === lastId) {
            window.clearTimeout(timer);
            timer = null;
            state.timer = null;
            pending = null;
            return;
          }
          if (pending && redistioId === pending.redistioId) {
            return;
          }
          pending = {
            id: feature.id,
            redistioId: redistioId
          };
          if (timer === null) {
            timer = window.setTimeout(emitHover, delay);
            state.timer = timer;
          }
        }

        function mouseleave() {
          window.clearTimeout(timer);
          timer = null;
          pending = null;
          if (lastId !== null) {
            lastId = null;
            Shiny.setInputValue(
              message.id + '_feature_hover',
              null,
              { priority: 'event' }
            );
          }
        }

        map.on('mousemove', layerId, mousemove);
        map.on('mouseleave', layerId, mouseleave);
        state = {
          layerId: layerId,
          mousemove: mousemove,
          mouseleave: mouseleave,
          timer: timer
        };
        element.redistioHover = state;
      },
      0
    );
  });
})();
