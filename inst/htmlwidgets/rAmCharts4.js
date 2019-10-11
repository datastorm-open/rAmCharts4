HTMLWidgets.widget({

  name: "rAmCharts4",

  type: "output",

  factory: function(el, width, height) {

    // create our sigma object and bind it to the element
    var chart, params;

    return {
      renderValue: function(x) {

        var chartdiv = document.getElementById(el.id); //.parentElement

        console.info(x);

        if(x.type != "Sunburst")
        {
           if(x.type != "TreeMap")
          {
            x.opts.data = HTMLWidgets.dataframeToD3(x.opts.data);
          }
        }

        chart = am4core.createFromConfig(x.opts, chartdiv, x.type);

        console.info(chart);
      },

      resize: function(width, height) {
        //chart.resize();
      },
    };
  }
});
