library(shiny)

checkoutCollapseUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    em('heads up, what you see here will be printed. what do you want to do?'),
    imageOutput(ns('confirmation_image'), height = 'auto', width = '60%'),
    br(),
    fluidRow(
      tableOutput(ns('details_summary')),
      column(8, offset = 2,
        fluidRow(
          column(6, actionButton(ns('order'), "PROCEED TO CHECKOUT")),
          column(6, actionButton(ns('open_personalize'), "CONTINUE PERSONALIZING"))
        )
      )
    )
  )
  
}

checkoutCollapse <- function(input, output, session, quantity, details, dv) {
  
  output$orbit_confirmation <- renderImage({
    validate(need(a(), 'visit create page'))
    orbit_dim <- ifelse(m, 320, 400)
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
}
