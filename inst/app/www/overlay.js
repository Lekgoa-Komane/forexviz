// Receives overlay series from R and merges them into the chart, so the
// chart is not redrawn and the zoom stays where it is.
$(document).ready(function () {
  Shiny.addCustomMessageHandler('forexviz_set_overlay', function (message) {
    var el = document.getElementById(message.id);
    if (!el) { return; }
    var chart = echarts.getInstanceByDom(el);
    if (!chart) { return; }
    el.forexvizOverlay = message.series;   // remember the latest overlays
    chart.setOption({ series: message.series });
    if (typeof el.forexvizUpdateLabels === 'function') {
      el.forexvizUpdateLabels(true);
    }
  });
});
