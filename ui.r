source("~/apps//moments/util.r") 

display_cols = c('#1b87e0','#ed635f','#6abf90','#ff814a','#8369a8','#f4de5b', '#FFFFFF', '#000000')
shirt_sizes  = c('xs','s','m', 'l', 'xl', 'xxl', 'xxxl')
shirt_colors = c('white', 'ash', 'heather grey', 'black')
garments  = c('<img src="garment-type/tee.jpg">' = 'tee',
              '<img src="garment-type/hoodie.jpg">' = 'hoodie', 
              '<img src="garment-type/long-sleeve.jpg">' = 'long-sleeve')
image_sizes  = c('<img src="graphic-size/across-the-chest.jpg">' = 'across chest 7"x7"', 
                 '<img src="graphic-size/left-pocket.jpg">' = 'left pocket 2.5"x2.5"')
no_zoom = "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, maximum-scale=1, user-scalable=0\"/>"

jsCode <- "shinyjs.order = function(params) {window.location.href = params[0];}"

fluidPage(
  
  title = 'MOMENTS',
  theme = "adjust.css",
  
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c('order')),
  
  fluidRow(
    column(
      8, 
      offset = 2, 
      align = 'center',
    
      tags$head(HTML(no_zoom)),
      h1("M O M E N T S"),
      
      bsCollapse(
        id = "main", 
        open = "intro",
        
        intro_card('banner.jpg'),

        bsCollapsePanel(
          'create',
          fluidRow(
            column(
              12, 
              align = 'center', 
              imageOutput('orbit')
            )
          ),
          fluidRow(
            column(
              10, 
              align = 'center', 
              offset = 1,
              column(
                4,
                p('how many moments would you like to plot?'),
                selectInput('n', NULL, 1:6, 1)
              ),
              column(
                4,
                p('select dates'),
                ui_row_output(1),
                ui_row_output(2),
                ui_row_output(3),
                ui_row_output(4),
                ui_row_output(5),
                ui_row_output(6)
              ),
              column(
                4,
                p('select color'),
                colourpicker::colourInput('hex', 
                                          NULL, 
                                          palette = 'limited',
                                          showColour = 'background',
                                          allowedCols = display_cols,
                                          value = display_cols[1])
              )
            )
          )
        ),
        
        garment_card('garment', garments),
        image_size_card('image_size', image_sizes),
        
        bsCollapsePanel('details', uiOutput('details_card')),
        bsCollapsePanel('review', uiOutput('checkout'))
      ),
      helpText(
        'comments/questions?', 
        a('contact amorata', href = 'mailto:info@amoratadesigns.com')
      )
    )
  )
)  

# put this after imageOutput to debug
# ,column(2, br(),
#         numericInput('ps1', 'Planet Size:', 18, 1, 24, 1),
#         numericInput('ps2', 'Legend Point Size:', 12, 1, 24, 1),
#         numericInput('fs', 'Font Size:', 16, 1, 24, 1),
#         numericInput('lt', 'Line Thickness:', 1.2, 0, 2, .1),
#         numericInput('lm', 'Legend Margin:', 1, 0, 3, .1),
#         numericInput('lk', 'Legend Key Dimensions:', 3, 0, 6, .1))
