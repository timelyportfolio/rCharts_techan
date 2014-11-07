library(rCharts)
library(jsonlite)
library(quantmod)


#set up rCharts
#key is to define how to handle the data
rChartsTechan <- setRefClass(
  "rChartsTechan",
  contains = "Dimple",
  methods = list(
    initialize = function(){
      callSuper(); 
    },
    getPayload = function (chartId) {
      # expect data to be in xts
      # open, high, low, close, volume, adj. close structure
      data =  jsonlite::toJSON(
        data.frame(
          date = index(data)
          ,data
        )
      )
      chart = toChain(params$chart, "myChart")
      controls_json = toJSON(params$controls)
      controls = setNames(params$controls, NULL)
      opts = toJSON2(params[!(names(params) %in% c("data", "chart", 
                                                   "controls"))])
      list(opts = opts, data = data, chart = chart, chartId = chartId, 
           controls = controls, controls_json = controls_json)
    }
  )
)


# now make a rChart with our rpart

rT <- rChartsTechan$new()
rT$setLib("http://timelyportfolio.github.io/rCharts_techan")
rT$lib = "techan"
rT$LIB$name = "techan"
rT$setTemplate(
  chartDiv = "
  <{{container}} id = '{{ chartId }}' class = '{{ lib }}' style = 'height:100%;width:100%;'></{{ container}}>
"
  ,script = "
    <script>

    </script>
"
)

rT$set(
  data = getSymbols(
    "SPY"
    ,auto.assign=F
  )
  , height = 800
  , width = 800
)
rT$show()


