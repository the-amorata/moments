source("~/apps/moments/util.r")
  
ref = mk_ref_dt()
ids = c('22746124421', '24373251717', '24373322885')

function(input, output, session) {  

  lapply(1:6, function(i) {output[[paste0('r',i)]] <- render_ui_row(i)})
  
  df <- reactive({
    req(input$n)
    req(input[[paste0('date', input$n)]])
    x = lapply(1:input$n, function(i) {
      data.table(date = as.character(input[[paste0('date',i)]])) 
    })
    unique(rbindlist(x))
  })
  
  rv = reactiveValues(mobile = NULL)
  observe({rv$mobile = session$clientData$output_orbit_width < 400})
  
  orbit_plot <- reactive({
    validate(need(df(), ''))
    x = day_xy(df())
    plot_orbit(x, ref, input$hex)
  })
  
  output$orbit <- renderImage({
    validate(need(orbit_plot(), ''))
    orbit_dim <- ifelse(rv$mobile, 320, 400)
    outfile <- tempfile(fileext='.png')
    
    # Generate a png
    ggsave(outfile, plot = orbit_plot(), bg = 'transparent', width = 11, height = 11, units = 'in')
    
    # Return a list
    list(src = outfile,
         width  = orbit_dim,
         height = orbit_dim,
         alt = "trouble loading image")
  }, deleteFile = TRUE)
  
  ### DETAILS ###
  
  # track the number of input boxes to render
  counter <- reactiveValues(n = 1)
  
  observeEvent(input$add_btn, {counter$n <- counter$n + 1})
  observeEvent(input$rm_btn, {if (counter$n > 1) counter$n <- counter$n - 1})
  
  output$details <- renderUI({
    lapply(seq_len(counter$n), function(i) render_detail_row(i, input$garment, input$hex))
  })
  
  order_details <- reactive({
    sapply(seq_len(counter$n), function(i) {
      q  = input[[paste0('q', i)]]
      ss = input[[paste0('shirt_size', i)]]
      sc = input[[paste0('shirt_color', i)]]
      paste0('(', paste(q, ss, sc, sep = ' - '),  ')')
    })
  })
  
  total_quantity <- reactive({
    qs = sapply(seq_len(counter$n), function(i) {
      as.numeric(input[[paste0('q', i)]])
    })
    sum(qs)
  })
  
  # output$od_test <- renderPrint({order_details()})
  # output$tq_test <- renderPrint({total_quantity()})
  # output$an_test <- renderPrint({no_null_details()})
  
  output$details_card <- renderUI({
    validate(need(orbit_plot(), 'create image'))
    validate(need(input$garment, 'choose garment size'))
    validate(need(input$image_size, 'choose image size'))
    fluidRow(
      p('select color, size, and quantity'),
      fluidRow(
        column(8, offset = 2,
          uiOutput('details'),
          p('want this in another size or color?'),
          fluidRow(
            column(6, actionButton('add_btn', NULL, icon = icon('plus'), width = '50%')),
            column(6, actionButton('rm_btn', NULL, icon = icon('minus'), width = '50%'))
          )
        )
      ),
      hr(),
      fluidRow(
        column(6, offset = 3,
          fluidRow(
            column(4, actionLink('example', 'samples')),
            column(4, actionLink('size_guide', 'size guide')),
            column(4, actionLink('bundles', 'bundles'))
          )
        )
      )
    )
  })
  
  observeEvent(input$example, {
    showModal(modalDialog(
      title = "samples",
      sample_ligtbox(),
      easyClose = TRUE,
      size = 'l'
    ))
  })
  
  observeEvent(input$size_guide, {
    showModal(modalDialog(
      title = "size guide",
      fluidRow(column(12, align = 'center', img(src = "sizeguide.jpg", width = '80%'))),
      easyClose = TRUE,
      size = 'l'
    ))
  })
  
  observeEvent(input$bundles, {
    showModal(modalDialog(
      title = "bundles",
      fluidRow(column(12, align = 'center', img(src = "discount.jpg", width = '80%'))),
      easyClose = TRUE,
      size = 'l'
    ))
  })
  
  no_null_details <- reactive({
    ad = sapply(seq_len(counter$n), function(i) {
      x = isTruthy(input[[paste0('q', i)]])
      y = isTruthy(input[[paste0('shirt_size', i)]])
      z = isTruthy(input[[paste0('shirt_color', i)]])
      c(x, y, z)
    })
    all(ad)
  })
  
  ### CONFIRMATION ###
  
  output$orbit_confirmation <- renderImage({
    validate(need(orbit_plot(), 'visit create page'))
    mobile = session$clientData$output_orbit_confirmation_width < 400
    orbit_dim <- ifelse(mobile, 320, 400)
    outfile <- tempfile(fileext='.png')
    
    # Generate a png
    ggsave(outfile, plot = orbit_plot(), bg = 'transparent', width = 11, height = 11, units = 'in')
    
    # Return a list
    list(src = outfile,
         width  = orbit_dim,
         height = orbit_dim,
         alt = "trouble loading image")
  }, deleteFile = TRUE)
  
  output$details_summary <- renderTable({
    lab = paste0(gsub('_', ' ', input$garment), 's', ' (total)')
    y = c(paste(total_quantity(), lab), order_details(), input$image_size)
    x = c('order summary:', rep('', length(y) - 1))
    data.table(x = x, y = y)
  }, align = 'cc', colnames = FALSE, bordered = FALSE, striped = FALSE)
  
  output$checkout <- renderUI({
    validate(need(orbit_plot(), 'visit create page'))
    validate(need(no_null_details(), 'please fill all fields in "details"'))
    fluidRow(
      fluidRow(
        column(8, offset = 2, align = 'center',
          em('heads up, what you see here will be printed.'),
          br(),
          em('what do you want to do?'),
          imageOutput('orbit_confirmation', height = 'auto', width = '90%')
        )
      ),
      fluidRow(
        tableOutput('details_summary'),
        column(8, offset = 2,
          fluidRow(
            column(6, actionButton('order', "PROCEED TO CHECKOUT")),
            column(6, actionButton('open_personalize', "CONTINUE PERSONALIZING"))
          )
        )
      )
    )
  })
  
  observeEvent(input$order, {
    fn = paste0(mk_filename(orbit_plot()), '.png')
    ggsave(fn, plot = orbit_plot(), 
           bg = 'transparent', width = 11, height = 11, units = 'in', 
           path = '~/apps/moments/plots/')
    id = ids[which(input$garment == c('tee', 'hoodie', 'long-sleeve'))]
    js$order(mk_url(fn, order_details(), id, total_quantity(), input$image_size))
  })
  
  observeEvent(input$open_personalize, {
    updateCollapse(session, 'main', open = 'create', close = 'review')
  })

}
