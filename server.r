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
    validate(need(input$labs1, ''))
    validate(need(df(), ''))
    x = day_xy(df())
    # p = plot_orbit(x, ref, input$leg, input$ps1, input$ps2, input$fs, input$lt, input$lm, input$lk)
    plot_orbit(x, ref, input$leg, rv$mobile)
  })
  
  output$orbit <- renderImage({
    validate(need(orbit_plot(), ''))
    # Read plot2's width and height. These are reactive values, so this
    # expression will re-run whenever these values change.
    orbit_dim <- ifelse(rv$mobile, 320, 400)
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    # Generate a png
    ggsave(outfile, plot = orbit_plot(), bg = 'transparent', width = 11, height = 11, units = 'in')
    
    # Return a list
    list(src = outfile,
         width  = orbit_dim,
         height = orbit_dim,
         alt = "trouble loading image")
  }, deleteFile = TRUE)
  
  output$button <- renderUI({
    validate(need(input$ss != '', 'choose a shirt size'))
    validate(need(input$sc != '', 'choose a shirt color'))
    validate(need(input$gs != '', 'choose a graphic size'))
    actionButton('order', "PROCEED TO CHECKOUT")
  })
  
  observeEvent(input$example, {
    showModal(modalDialog(
      title = "samples",
      sample_ligtbox(),
      easyClose = TRUE,
      size = 'l'
    ))
  }) 
  
  onclick("order", {
    fn = paste0(mk_filename(orbit_plot()), '.png')
    ggsave(fn, plot = orbit_plot(), 
           bg = 'transparent', width = 11, height = 11, units = 'in', 
           path = '~/apps/moments/plots/')
    js$order(mk_url(fn, input$ss, input$sc, input$gs, input$q))
  })

}
