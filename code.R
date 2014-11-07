library(rCharts)
library(jsonlite)
library(quantmod)


#set up rCharts
#key is to define how to handle the data
rChartsTechan <- setRefClass(
  'rChartsTechan',
  contains = 'Dimple',
  methods = list(
    initialize = function(){
      callSuper(); 
    },
    getPayload = function (chartId) {
      # expect data to be in xts
      # open, high, low, close, volume, adj. close structure
      data = data.frame(
        Date = index(params$data)
        ,params$data
      )
      colnames(data) = c("Date","Open","High","Low","Close","Volume")
      data =  jsonlite::toJSON(
        data
      )
      chart = toChain(params$chart, 'myChart')
      controls_json = toJSON(params$controls)
      controls = setNames(params$controls, NULL)
      opts = toJSON2(params[!(names(params) %in% c('data', 'chart', 
                                                   'controls'))])
      list(opts = opts, data = data, chart = chart, chartId = chartId, 
           controls = controls, controls_json = controls_json)
    },
    render = function (chartId = NULL, cdn = F, static = T, standalone = F) 
    {
      params$dom <<- chartId %||% params$dom
      template = read_template(getOption("RCHART_TEMPLATE", templates$page))
      assets = Map("c", get_assets(LIB, static = static, cdn = cdn, 
                                   standalone = standalone), html_assets)
      html = render_template(template, list(params = params, assets = assets, 
                                            chartId = params$dom,
                                            width = params$width,
                                            height = params$height,
                                            script = .self$html(params$dom), 
                                            CODE = srccode, lib = LIB$name, tObj = tObj, container = container), 
                             partials = list(chartDiv = templates$chartDiv, afterScript = templates$afterScript %||% 
                                               "<script></script>"))
    }
  )
)


# now make a rChart with our rpart

rT <- rChartsTechan$new()
rT$setLib('http://timelyportfolio.github.io/rCharts_techan')
#rT$setLib('.')
rT$lib = 'techan'
rT$LIB$name = 'techan'
rT$setTemplate(
  chartDiv = "
  <div style = 'height:500;width:900;'>
  <{{container}} id = '{{ chartId }}' class = '{{ lib }}' style = 'height:100%;width:100%;'>
    <svg style = 'height:100%;width:100%;' viewBox = '0 0 {{width}} {{height}}'></svg>
  </{{ container}}>
  </div>
"
  ,script = "
    <script>

    var opts = {{{ opts }}};
    var data = {{{ data }}};

    var margin = {top: 20, right: 20, bottom: 30, left: 50},
            width = opts.width - margin.left - margin.right,
            height = opts.height - margin.top - margin.bottom;

    var parseDate = d3.time.format('%Y-%m-%d').parse;

    var x = techan.scale.financetime()
            .range([0, width]);

    var y = d3.scale.linear()
            .range([height, 0]);

    var candlestick = techan.plot.candlestick()
            .volumeOpacity(false) // Set to true for volume opacity
            .xScale(x)
            .yScale(y);

    var xAxis = d3.svg.axis()
            .scale(x)
            .orient('bottom');

    var yAxis = d3.svg.axis()
            .scale(y)
            .orient('left');

    var svg = d3.select( '#{{ chartId }} svg'  )
            //.attr('viewBox', '0 0 ' + (+width + margin.left + margin.right) + ' ' + (+height + margin.top + margin.bottom)  )
            //.style('height','100%')
            //.style('width','100%')
            .append('g')
            .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

    
      var accessor = candlestick.accessor();

      data = data.map(function(d) {
          return {
              date: parseDate(d.Date),
              open: +d.Open,
              high: +d.High,
              low: +d.Low,
              close: +d.Close,
              volume: +d.Volume
          };
      }).sort(function(a, b) { return d3.ascending(accessor.d(a), accessor.d(b)); });

      x.domain(data.map(accessor.d));
      y.domain(techan.scale.plot.ohlc(data, accessor).domain());

      svg.append('g')
              .datum(data)
              .attr('class', 'candlestick')
              .call(candlestick);

      svg.append('g')
              .attr('class', 'x axis')
              .attr('transform', 'translate(0,' + height + ')')
              .call(xAxis);

      svg.append('g')
              .attr('class', 'y axis')
              .call(yAxis)
              .append('text')
              .attr('transform', 'rotate(-90)')
              .attr('y', 6)
              .attr('dy', '.71em')
              .style('text-anchor', 'end')
              .text('Price ($)');
    </script>
"
)



rT$set(
  data = getSymbols(
    'SPY'
    ,from = "2012-12-31"
    ,auto.assign=F
  )
)
rT

#rT$publish( "rCharts candlesticks with techan.js" )



