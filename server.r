source("~/apps//moments/util.r")     

ref = mk_ref_dt()
 
function(input, output, session) {     

  lapply(1:6, function(i) {output[[paste0('r',i)]] <- render_ui_row(i)})
  
  df <- reactive({
    x = lapply(1:input$n, function(i) {
      l = paste0('labs',i); b = paste0('date',i); h = paste0('cols',i)
      data.table(olab = input[[l]], 
                 bday = as.character(input[[b]]),
                 hex  = input[[h]]) 
    })
    rv = rbindlist(x)[,lab := lab_cat(olab), by = hex][,hl := paste(hex, lab)]
    unique(rv)
  })
  
  rv = reactiveValues(mobile = NULL)
  observe({rv$mobile = session$clientData$output_orbit_width < 400})
  
  orbit_plot <- reactive({
    validate(need(!is.null(input$labs1), ''))
    validate(need(df(), ''))
    x = day_xy(df())
    # plot_orbit(x, ref, input$leg, input$ps1, input$ps2, input$fs, input$lt, input$lm, input$lk)
    plot_orbit(x, ref, input$leg, rv$mobile)
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
    lapply(seq_len(counter$n), function(i) render_detail_row(i))
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
    fluidRow(
      p('select shirt color, size, and quantity'),
      column(3,
        fluidRow(
          column(6, actionButton('add_btn', NULL, icon = icon('plus'), width = '100%')),
          column(6, actionButton('rm_btn', NULL, icon = icon('minus'),  width = '100%'))
        )
      ),
      column(9, uiOutput('details')),
      fluidRow(
        column(6, offset = 3,
          fluidRow(
            column(4, actionLink('example', 'samples')),
            column(4, a('size guide', href = 'https://www.amoratadesigns.com/pages/size-guide', target = '_blank')),
            column(4, a('bundles', href = 'https://www.amoratadesigns.com/pages/bundles',  target = '_blank'))
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
  
  output$details_summary <- renderTable({
    y = c(paste(total_quantity(), 'shirts (total)'), order_details())
    x = c('order summary:', rep('', length(y) - 1))
    data.table(x = x, y = y)
  }, align = 'cc', colnames = FALSE, bordered = FALSE, striped = FALSE)
  
  output$checkout <- renderUI({
    validate(need(no_null_details(), 'please fill all fields in step 3'))
    fluidRow(
      em('heads up, what you see here will be printed. what do you want to do?'),
      imageOutput('orbit_confirmation', height = 'auto', width = '60%'),
      br(),
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
    js$order(mk_url(fn, order_details(), total_quantity(),  input$image_size))
  })
  
  observeEvent(input$open_personalize, {
    updateCollapse(session, 'main', open = '2. create', close = 'review')
  })

}
