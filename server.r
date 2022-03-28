#temp

#fin temp

function(input, output, session) {

  # Data subsetting according to input
  dt_sel <- reactive({
    # input
    nbChambre <- input$homeNbChambre
    nbSdB <- input$homeNbSdB
    # maxPrice <- input$homeMaxPrice * 1000
    if (input$homeRemoveMaxPrice) {
      maxPrice <- 10^6
    } else {
      maxPrice <- Inf
    }
    # priceOutlier
    # data input
    dt <- dt
    # calc
    dt <- dt %>% 
    filter(
      NbChambres==nbChambre, 
      NbSallesBains== nbSdB,
      BuyPrice_recal <= maxPrice
    )
    validate(
      need(nrow(dt)>0, 'No house in selection...')
    )
    # return
    return(dt)
  })

  # criteria selection
  output$criteriaSelection <- renderUI({
    # info
    dt_sel <- dt_sel()
    # return
    if (nrow(dt_sel)==0) {
      return(p(class='bg-danger', 'No house in the selection...'))
    }
    stringReturn <- p(class='bg-info', 
                      paste0('# Properties: ', nrow(dt_sel)),
                      ' (', format.money(min(dt_sel$BuyPrice_recal)), ' ~ ', format.money(max(dt_sel$BuyPrice_recal)), ')'
                      )
    return(stringReturn)
  })

  # criteria & area selection
  dt_sel_inArea <- reactive({
    # input
    boundary <- input$homeMap_bounds
    # data input
    dt_sel <- dt_sel()
    # validation
    validate(need(!is.null(boundary), 'Waiting for Polygons initialized...'))
    # calc
    max_lat <- boundary[['north']]
    min_lat <- boundary[['south']]
    max_lng <- boundary[['east']]
    min_lng <- boundary[['west']]
    dt_sel <- dt_sel %>%
      filter(
        Lat <= max_lat,
        Lat >= min_lat,
        Lng <= max_lng,
        Lng >= min_lng
      )
    # return
    return(dt_sel)
  })


  output$homeHistArea <- renderMetricsgraphics({
    # data input
    dt_sel_inArea <- dt_sel_inArea()
    # calc return
    mjs_plot(dt_sel_inArea$BuyPrice_recal, left=35, decimals=0, top=-40, bottom=50) %>%
       mjs_histogram(bar_margin=1) %>%
       mjs_axis_x(extended_ticks=TRUE, xax_format='comma', xax_count=4, min_x=0, max_x=8*10^5) %>%
       mjs_axis_y(rug=TRUE, extended_ticks=FALSE, yax_count=4) %>%
       mjs_labs(x_label = 'Price', y_label = NULL)
  })

  output$homeStatsArea <- renderUI({
    # data input
    dt_sel_inArea <- dt_sel_inArea()
    # calc return
    res <- dt_sel_inArea %>% summarise(
        Min             = format.money(min(BuyPrice_recal)),
        '25 Percentile' = format.money(quantile(BuyPrice_recal, 0.25)),
        'Median'        = format.money(median(BuyPrice_recal)),
        '75 Percentile' = format.money(quantile(BuyPrice_recal, 0.75)),
        Max             = format.money(max(BuyPrice_recal)),
        Mean            = format.money(mean(BuyPrice_recal))
      )
    template_html <- '
    <table class="table table-striped table-condensed">
      {{#html_res}}
      <tr>
        <td>
          {{name}}
        </td>
        <td>
          {{value}}
        </td>
      </tr>
      {{/html_res}}
    </table>
    '
    html_res <- whisker::iteratelist(res)
    returned <- whisker::whisker.render(template_html)
    return(HTML(returned))
    # browser()
  })

  output$areaSelection <- renderUI({
    # info
    dt_sel_inArea <- dt_sel_inArea()
    # return
    if (nrow(dt_sel_inArea)==0) {
      return(p(class='bg-danger', 'No house in the area...'))
    }
    stringReturn <- p(class='bg-info', paste0('# Properties in area: ', nrow(dt_sel_inArea)))
    return(stringReturn)
  })


  shp_montreal_sel <- reactive({
    # stat input
    which_value <- input$homeValueDisplay
    # datainput
    shp_montreal <- shp_montreal
    dt_sel <- dt_sel()
    # calc
    if (nrow(dt_sel) > 0) {
      res <- dt_sel %>% group_by(PostalCode_head) %>% 
      summarise(
          count = n(),
          mean = mean(BuyPrice_recal),
          median = quantile(BuyPrice_recal, 0.5),
          quantile25 = quantile(BuyPrice_recal, 0.25),
          quantile75 = quantile(BuyPrice_recal, 0.75)
          )
      shp_montreal@data$CFSAUID <- as.character(shp_montreal@data$CFSAUID)
      res$value <- as.data.frame(res)[, which_value]
      shp_montreal@data <- left_join(shp_montreal@data, res, by=c('CFSAUID'='PostalCode_head'))
    } else {
      shp_montreal@data$col <- 'white'
    }
    return(shp_montreal)
  })

  # output for home map
  output$homeMap <- renderLeaflet({
  mb <- leaflet() %>% 
    # addTiles() %>%
    addProviderTiles("Thunderforest.Transport") %>%
    # tiles to verify
    # addProviderTiles("MtbMap") %>%
    # addProviderTiles("Stamen.TonerLines",
    #   options = providerTileOptions(opacity = 0.35)
    # ) %>%
    # addProviderTiles("Stamen.TonerLabels") %>%
    fitBounds(min(dt$Lng), min(dt$Lat), max(dt$Lng)*.998, max(dt$Lat))
    # mb <- mb %>% addPolygons(data=shp_montreal, layerId='init',
    #   fillColor='white', 
    #   color='grey', 
    #   weight = 2, 
    #   fillOpacity = 0.5
    #   )
  return(mb)
  })

  # observe for map
  observe({

    # observer
    shp_montreal_sel <- shp_montreal_sel()
    which_value <- input$homeValueDisplay
    # proxy map
    proxy <- leafletProxy('homeMap', data=shp_montreal_sel)
    # action
    # browser()
      binpal <- colorBin("OrRd", shp_montreal_sel$value, 9, pretty = TRUE)
      # res$col <- binpal(as.data.frame(res)[, which_value])
      # if (any(is.na(shp_montreal@data$col))) shp_montreal@data$col[is.na(shp_montreal@data$col)] <- 'white'
    proxy %>% clearGroup('priceIndex') %>% removeControl('legend') %>%
      addPolygons(layerId= ~CFSAUID, group='priceIndex',
      fillColor = ~binpal(value),
      color = 'grey',
      weight = 2,
      fillOpacity = 0.5,
      popup = ~paste0(
          '<h4 style="color: #777;">', CFSAUID, '</h4>',
            '# Properties: ', count,
            '<br>',
            'The ', which_value, ' price: ', format.money(round(value))
        )
    ) %>%
      addLegend("bottomleft", pal = binpal, values = ~value,
      title = paste0('The ', which_value, ' price'),
      labFormat = labelFormat(prefix = "$"),
      opacity = 1,
      layerId= 'legend'
    )
  })
  
  output$dt_display <- DT::renderDataTable(dt %>% select(-LongDescription))

}