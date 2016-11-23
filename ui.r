source("~/apps//moments/util.r") 

shirt_sizes  = c('xs','s','m', 'l', 'xxl', 'xxxl')
shirt_colors = c('creme', 'summer peach', 'light blue')

jsCode <- "
  shinyjs.order = function(params) {
    //console.log(params[0]);
    window.location.href = params[0];
  }"

fluidPage(
  
  title = 'Moments Around The Sun',
  theme = "adjust.css",
  
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c('order')),
  
  fluidRow(
    column(6, align = 'center', offset = 3,
           h1("MOMENTS"),
           p('see where the earth was on its journey around the sun.
              input up to six meaningful dates then personalize the labels and colors 
             (we left you a few examples).'))
  ),
  fluidRow(column(12, align = 'center', imageOutput('orbit'))
    # ,column(2, br(),
    #         numericInput('ps1', 'Planet Size:', 18, 1, 24, 1),
    #         numericInput('ps2', 'Legend Point Size:', 12, 1, 24, 1),
    #         numericInput('fs', 'Font Size:', 16, 1, 24, 1),
    #         numericInput('lt', 'Line Thickness:', 1.2, 0, 2, .1),
    #         numericInput('lm', 'Legend Margin:', 1, 0, 3, .1),
    #         numericInput('lk', 'Legend Key Dimensions:', 3, 0, 6, .1))
  ),   
  fluidRow(column(8, align = 'center', offset = 2,
    column(3, 
        p('how many moments would you like to plot?'),
        selectInput('n', NULL, 1:6, 4),
        checkboxInput('leg', 'labels', TRUE)),
    column(6,
      ui_row_output(1),
      ui_row_output(2),
      ui_row_output(3),
      ui_row_output(4),
      ui_row_output(5),
      ui_row_output(6)
    ),
  
  column(3,
    fluidRow(
      selectizeInput(inputId = 'ss', 
                     label = NULL, 
                     choices = shirt_sizes,
                     options = list(
                       placeholder = 'shirt size',
                       onInitialize = I('function() { this.setValue(""); }'))),
      selectizeInput(inputId = 'sc', 
                     label = NULL, 
                     choices = shirt_colors,
                     options = list(
                       placeholder = 'shirt color',
                       onInitialize = I('function() { this.setValue(""); }')))
    ),
    fluidRow(
      uiOutput('button')
      # column(6, uiOutput('button')),
      # column(6, actionLink('example', 'Samples'))
    ))
  ))
)