# temp
# temp end
shinyUI(
  navbarPage(title="PoC Immobilier Montreal", theme=shinytheme("united"),
    # Page Home
    tabPanel('Home', 
      tags$head(
        # tags$script(src = "./lib/highcharts.js"),
        # tags$script(src = "./lib/modules/exporting.js"),
        # tags$script(src = "./lib/highcharts-more.js"),
        # tags$script(src = "./lib/bootstrap-switch.min.js"),
        # tags$script(src = "./lib/themes/grid-light.js"),
        tags$script(src = "./js/modified.js"),
        # tags$link(rel="stylesheet", type="text/css", href="./css/bootstrap-switch.min.css"),
        tags$link(rel="stylesheet", type="text/css", href="./css/modified.css")
        # tags$script(src = "//cdn.datatables.net/1.10.7/js/jquery.dataTables.min.js"),
        # tags$link(rel="stylesheet", type="text/css", href="//cdn.datatables.net/1.10.7/css/jquery.dataTables.min.css"),
        # tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });'))
      ),
      div(class='outer',
        leafletOutput('homeMap', height='100%'),
        absolutePanel(
          # panel setting
          top = 5, left = "auto", bottom = "auto",
          width = 330, height = "auto",
          id = "controls", class = "panel panel-default", draggable=TRUE,
          # input control
          h4('Display Criteria', class='header'),
          div(style="display:inline-block", numericInput('homeNbChambre', '#Bedroom', 1, width='90px', min=1, max=6)),
          div(style="display:inline-block", numericInput('homeNbSdB', '#Bathroom', 1, width='90px', min=1, max=4)),
          # div(style="display:inline-block", numericInput('homeMaxPrice', '#Price Max (in k)', 1000, width='140px', min=0, max=5*10^3, step=100)),
          checkboxInput('homeRemoveMaxPrice', 'Reasonable Price (< $1 million)', value=TRUE),
          htmlOutput('criteriaSelection'),
          selectInput('homeValueDisplay', 'Value in display: ', valueDisplayList, width='100%'),
          h4('In-area Statistics', class='header'),
          h6('Price Histogram', align='center'),
          div(style='margin-top:-40px;',
            metricsgraphicsOutput('homeHistArea', height='250px')
          ),
          div(
            htmlOutput('homeStatsArea')
          ),
          htmlOutput('areaSelection')
        )
      )     
    ),
    tabPanel("DataTable",
      dataTableOutput("dt_display")
    )
  )
)
