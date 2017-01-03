source("~/apps//moments/util.r") 

shirt_sizes  = c('xs','s','m', 'l', 'xl', 'xxl', 'xxxl')
shirt_colors = c('creme', 'summer peach', 'light blue')
graphic_sizes = c('across chest 7"x7"', 'left pocket 2.5"x2.5"')
image_sizes  = c('<img src="moments_ac.jpg">' = 'across chest 7"x7"', 
                 '<img src="moments_lp.jpg">' = 'left pocket 2.5"x2.5"')

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
  
  fluidRow(column(8, offset = 2, align = 'center',
    h1("M O M E N T S"),
    bsCollapse(id = "main", open = "intro",
      bsCollapsePanel('intro',
        img(src = 'jandra-intro.jpg', align = 'center', width = '100%', height = 'auto')
      ),
      bsCollapsePanel('1. graphic size',
        p('select between an across the chest graphic (7"x7") or a left pocket icon (2.5"x2.5")'),
        radioButtons_withHTML('image_size', NULL, choices = image_sizes, inline = TRUE)
      ),
      bsCollapsePanel('2. create',
        fluidRow(
          column(12, align = 'center', 
            imageOutput('orbit'))
        ),
        fluidRow(column(10, align = 'center', offset = 1,
          column(3,
            p('how many moments would you like to plot?'),
            selectInput('n', NULL, 1:6, 4),
            checkboxInput('leg', 'show labels', TRUE)
          ),
          column(9,
            ui_row_output(1),
            ui_row_output(2),
            ui_row_output(3),
            ui_row_output(4),
            ui_row_output(5),
            ui_row_output(6)
          )
        ))
      ),
      bsCollapsePanel('3. details', uiOutput('details_card')),
      bsCollapsePanel('review', uiOutput('checkout'))
    ),
    helpText(
      'comments/questions?', 
      a('contact amorata', href = 'mailto:info@amoratadesigns.com')
    )
  ))
)  


# put this after imageOutput to debug
# ,column(2, br(),
#         numericInput('ps1', 'Planet Size:', 18, 1, 24, 1),
#         numericInput('ps2', 'Legend Point Size:', 12, 1, 24, 1),
#         numericInput('fs', 'Font Size:', 16, 1, 24, 1),
#         numericInput('lt', 'Line Thickness:', 1.2, 0, 2, .1),
#         numericInput('lm', 'Legend Margin:', 1, 0, 3, .1),
#         numericInput('lk', 'Legend Key Dimensions:', 3, 0, 6, .1))



  # fluidRow(
  #   column(6, align = 'center', offset = 3,
  #          h1("M O M E N T S"),
  #          p('see where the earth was on its journey around the sun.
  #             input up to six meaningful dates then personalize the labels and colors 
  #            (we left you a few examples).'))
  # ),
  # fluidRow(column(12, align = 'center', imageOutput('orbit'))
  #   # ,column(2, br(),
  #   #         numericInput('ps1', 'Planet Size:', 18, 1, 24, 1),
  #   #         numericInput('ps2', 'Legend Point Size:', 12, 1, 24, 1),
  #   #         numericInput('fs', 'Font Size:', 16, 1, 24, 1),
  #   #         numericInput('lt', 'Line Thickness:', 1.2, 0, 2, .1),
  #   #         numericInput('lm', 'Legend Margin:', 1, 0, 3, .1),
  #   #         numericInput('lk', 'Legend Key Dimensions:', 3, 0, 6, .1))
  # ),   
  # fluidRow(column(8, align = 'center', offset = 2,
  #   column(3, 
  #     p('how many moments would you like to plot?'),
  #     selectInput('n', NULL, 1:6, 4),
  #     fluidRow(
  #       column(6, checkboxInput('leg', 'labels', TRUE)),
  #       column(6, actionLink('example', 'samples'))
  #     ),
  #     fluidRow(
  #       column(6, align = 'center', div(id = 'qlab', p('quantity'))),
  #       column(6, numericInput('q', NULL, 1, min = 1, step = 1))
  #     )
  #   ),
  #   column(6,
  #     ui_row_output(1),
  #     ui_row_output(2),
  #     ui_row_output(3),
  #     ui_row_output(4),
  #     ui_row_output(5),
  #     ui_row_output(6)
  #   ),
  # 
  # column(3,
  #   fluidRow(
  #     selectizeInput(inputId = 'ss', 
  #                    label = NULL, 
  #                    choices = shirt_sizes,
  #                    options = list(
  #                      placeholder = 'shirt size',
  #                      onInitialize = I('function() { this.setValue(""); }'))),
  #     selectizeInput(inputId = 'sc', 
  #                    label = NULL, 
  #                    choices = shirt_colors,
  #                    options = list(
  #                      placeholder = 'shirt color',
  #                      onInitialize = I('function() { this.setValue(""); }'))),
  #     selectizeInput(inputId = 'gs', 
  #                    label = NULL, 
  #                    choices = graphic_sizes,
  #                    options = list(
  #                      placeholder = 'graphic size',
  #                      onInitialize = I('function() { this.setValue(""); }')))
  #   ),
  #   fluidRow(
  #     uiOutput('button')
  #     # column(6, uiOutput('button')),
  #     # column(6, actionLink('example', 'Samples'))
  #   ))
  # ))
# )