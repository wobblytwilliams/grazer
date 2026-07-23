/* global LeafletWidget, L, $ */
LeafletWidget.methods.addGrazerGroupedTimeslider = function(times, layerIds, groups, options) {
  var map = this;

  // Remove the upstream slider and its temporary GeoJSON markers. Its
  // dependency is retained for the jQuery UI range slider used below.
  if (map.sliderCntr) {
    map.sliderCntr.remove();
    delete map.sliderCntr;
  }
  if (map.grzSliderCntr) {
    map.grzSliderCntr.remove();
    delete map.grzSliderCntr;
  }

  var uniqueTimes = times.filter(function(value, index, values) {
    return values.indexOf(value) === index;
  }).sort();
  var timeIndex = {};
  uniqueTimes.forEach(function(value, index) {
    timeIndex[value] = index;
  });

  var records = layerIds.map(function(layerId, index) {
    return {
      layer: map.layerManager.getLayer("marker", layerId),
      parent: map.layerManager.getLayerGroup(groups[index], false),
      timeIndex: timeIndex[times[index]]
    };
  }).filter(function(record) {
    return record.layer && record.parent && Number.isFinite(record.timeIndex);
  });

  function setRange(first, last) {
    records.forEach(function(record) {
      var visible = record.timeIndex >= first && record.timeIndex <= last;
      var present = record.parent.hasLayer(record.layer);
      if (visible && !present) {
        record.parent.addLayer(record.layer);
      } else if (!visible && present) {
        record.parent.removeLayer(record.layer);
      }
    });
  }

  function formatRange(first, last) {
    if (uniqueTimes.length === 0) {
      return "";
    }
    if (first === last) {
      return uniqueTimes[first];
    }
    return uniqueTimes[first] + " - " + uniqueTimes[last];
  }

  var SliderControl = L.Control.extend({
    options: { position: options.position || "topright" },
    onAdd: function() {
      var container = L.DomUtil.create("div", "slider grz-timeslider");
      var sliderId = "grz-leaflet-slider-" + L.Util.stamp(map);
      var labelId = sliderId + "-timestamp";
      container.innerHTML =
        '<div id="' + sliderId + '" style="width:200px">' +
        '<div class="ui-slider-handle"></div>' +
        '<div id="' + labelId + '" style="width:200px; margin-top:13px; background-color:#FFFFFF; text-align:center; border-radius:5px;"></div>' +
        '</div>';

      L.DomEvent.disableClickPropagation(container);
      L.DomEvent.disableScrollPropagation(container);

      setTimeout(function() {
        var last = Math.max(0, uniqueTimes.length - 1);
        var slider = $("#" + sliderId);
        slider.slider({
          range: true,
          min: 0,
          max: last,
          values: options.showAllOnStart ? [0, last] : [0, 0],
          slide: function(event, ui) {
            setRange(ui.values[0], ui.values[1]);
            $("#" + labelId).text(formatRange(ui.values[0], ui.values[1]));
          }
        });
        var initial = options.showAllOnStart ? [0, last] : [0, 0];
        setRange(initial[0], initial[1]);
        $("#" + labelId).text(formatRange(initial[0], initial[1]));
      }, 0);

      return container;
    }
  });

  map.grzSliderCntr = new SliderControl();
  map.addControl(map.grzSliderCntr);
};

LeafletWidget.methods.offsetGrazerTimelineLayerControl = function(marginTopPx) {
  var map = this;

  // The slider's datetime label extends below the height Leaflet reserves for
  // the slider control. Give the next top-right control enough space to sit
  // below that label instead of covering it.
  setTimeout(function() {
    if (!map.currentLayersControl) {
      return;
    }
    var container = map.currentLayersControl.getContainer();
    if (!container) {
      return;
    }
    container.classList.add("grz-layer-control-below-timeline");
    container.style.marginTop = String(marginTopPx || 44) + "px";
  }, 0);
};

LeafletWidget.methods.addGrazerLayerDeselectAll = function(overlayGroups, label) {
  var map = this;

  setTimeout(function() {
    if (!map.currentLayersControl) {
      return;
    }
    var container = map.currentLayersControl.getContainer();
    var list = container ? container.querySelector(".leaflet-control-layers-list") : null;
    if (!list || list.querySelector(".grz-layer-deselect-all")) {
      return;
    }

    var separator = L.DomUtil.create("div", "leaflet-control-layers-separator", list);
    separator.setAttribute("aria-hidden", "true");

    var button = L.DomUtil.create("button", "grz-layer-deselect-all", list);
    button.type = "button";
    button.textContent = label || "Deselect all";
    button.style.display = "block";
    button.style.width = "100%";
    button.style.padding = "4px 8px";
    button.style.border = "1px solid #b5b5b5";
    button.style.borderRadius = "2px";
    button.style.background = "#ffffff";
    button.style.color = "#222222";
    button.style.cursor = "pointer";
    button.style.font = "12px/1.4 Arial, Helvetica, sans-serif";

    button.addEventListener("click", function(event) {
      event.preventDefault();
      event.stopPropagation();
      overlayGroups.forEach(function(group) {
        var layer = map.layerManager.getLayerGroup(group, false);
        if (layer && map.hasLayer(layer)) {
          map.removeLayer(layer);
        }
      });
    });
  }, 0);
};
