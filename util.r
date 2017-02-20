pkgs = c("shiny", "data.table", "ggplot2", "colourpicker", 
         "RColorBrewer", "extrafont", "shinyjs", 'shinyBS')
invisible(lapply(pkgs, library, character.only = TRUE)); rm(pkgs)

### Important Constants
# e = 0.0167086; a = 149.6; b = sqrt(a^2*(1 - e^2))

### Functions

ellipse <- function(x, a = 149.6, b = 149.579116) b*sqrt(1 - (x/a)^2)
day2year <- function(x) gsub('(^[[:digit:]]{4}).*', '\\1-01-01', x)
doty <- function(x, y = day2year(x)) as.numeric(difftime(x, y, units = 'days'))
trim <- function(x) gsub("^\\s+|\\s+$", "", x)
lab_cat <- function(x) trim(gsub('\n$', '', paste(x, collapse = '\n')))
valid_date <- function(m,d) as.Date(paste('2000', m, d, sep='-'))

mk_filename <- function(data) {
  sprintf("%s_%s", as.integer(Sys.time()), digest::digest(data))
}

mk_url <- function(img_file, order_details, id, q, gs) {
  url_base = paste0('http://amorata.myshopify.com/cart/', id, ':', q, '?')
  file_base = 'http://amorata-apps.com:8787/files/apps/moments/plots/'
  
  img_attr = paste0('attributes[img-file]=', file_base, img_file)
  gs_attr  = paste0('attributes[graphic-size]=', gs)
  od_attr  = paste0('attributes[order-details]=', paste(order_details,  collapse = ' '))
  all_attr = paste(img_attr, gs_attr, od_attr, sep = '&')
  
  paste0(url_base, all_attr)
}

day_xy <- function(dt) {
  a = 149.6; b = 149.579116
  dt[,poty := doty(date)/365]
  dt[,x :=  a*sin(2*pi*poty - pi/2)]
  dt[,y := -b*sin(2*pi*poty)]
}

mk_ref_dt <- function(i = 100) {
  a = 149.6; b = 149.579116
  data.table(x =  a*sin(seq(0, 2*pi, pi/i) - pi/2),
             y = -b*sin(seq(0, 2*pi, pi/i)))
}

render_ui_row <- function(i) {
  
  manual_dates = c('2016-12-25', '2016-04-22', '1989-05-14', 
                   '1990-12-16', '2016-07-04', '2016-10-02')
  
  renderUI({
    fluidRow(
      column(
        12,
        dateInput(paste0('date',i),
                  NULL,
                  startview = "year",
                  format = "M d",
                  value = manual_dates[i])
      )
    )
  })
}

ui_row_output <- function(i) {
  condition = paste0('input.n > ', i-1)
  inputid = paste0('r',i)
  conditionalPanel(condition, fluidRow(column(12,uiOutput(inputid))))
}

# set_par <- function(x) {
#   # if (x != '') par(mar = c(3.5, 0, 0, 0), mgp = c(1.5, 0, 0), bg=NA)
#   if (x != '') par(mar = c(0, 0, 0, 0), mgp = c(0, 0, 0), bg=NA)
#   else par(mar = c(0.0, 0, 0, 0), mgp = c(0, 0, 0), bg=NA)
# }
# 
# plotPath <- function(dt, ref, hex) {
#   set_par('hey now')
#   plot.new()
#   plot.window(xlim = c(-160, 160), ylim = c(-160, 160))
#   polypath(ref[,x], ref[,y], col = NA, border = hex, rule = "winding", lwd = 3.5)
#   points(dt[,x], dt[,y],
#          #Label Options
#          # xlab = rv$lab, cex.lab = 4, axes = FALSE, ylab = '',
#          xlab = 'hey now', cex.lab = 4,  ylab = '', 
#          #Text Options
#          # font.lab = 4, family = 'Futura Md BT', col.lab = rv$hex,
#          font.lab = 4, family = 'Futura Md BT', col.lab = hex,
#          #Line Options
#          # col = rv$hex, nr = nrs('less'), lwd = 10)
#          col = hex, lwd = 10, xlim = c())
# }

plot_orbit <- function(dt, ref, hex) {
  p = ggplot() + 
    geom_path(data = ref, aes(x = x, y = y), size = 6, linetype = 1, color = hex) +
    geom_point(data = dt, aes(x = x, y = y), size = 40, color = hex) +
    ylim(-160, 160) + xlim(-160, 160)
  if (hex == '#FFFFFF') {p = p + orbit_theme_w} else {p = p + orbit_theme}
  p + coord_fixed()
}

orbit_theme <- theme(
  title = element_blank()
,plot.background = element_blank()
,panel.background = element_blank()
,legend.key = element_blank()
,legend.background = element_blank()
,panel.grid.major = element_blank()
,panel.grid.minor = element_blank()
,panel.border = element_blank()
,axis.text = element_blank()
,axis.ticks = element_blank()
,text = element_blank()
)

orbit_theme_w <- theme(
  title = element_blank()
  ,plot.background = element_rect(fill = "grey")
  ,panel.background = element_rect(fill = "grey")
  ,legend.key = element_blank()
  ,legend.background = element_rect(fill = "grey")
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,axis.text = element_blank()
  ,axis.ticks = element_blank()
  ,text = element_blank()
)

sample_ligtbox <- function() {
  tags$div(
    fluidRow(
      lapply(list.files('~/apps/moments/www/samples/'), function(x) {
        column(4, align = 'center', img(src = paste0('samples/', x), height = '260px'))
      })
    )
  )
}

selInput <- function(inputId, choices, placeholder) {
  selectizeInput(inputId = inputId, 
                 label = NULL, 
                 choices = choices,
                 options = list(
                   placeholder = placeholder,
                   onInitialize = I('function() { this.setValue(""); }')))
}

render_detail_row <- function(i, garment, hex) {
  shirt_sizes  = switch(garment,
                        tee = c('xs', 's', 'm', 'l', 'xl', 'xxl', 'xxxl'),
                        hoodie = c('xs', 's', 'm', 'l', 'xl', 'xxl'),
                        'long-sleeve' = c('s', 'm', 'l', 'xl', 'xxl'))
  shirt_colors = switch(garment,
                        tee = c('white', 'ash', 'heather grey', 'black'),
                        hoodie = c('white', 'heather grey', 'black'),
                        'long-sleeve' = c('white', 'black'))
  
  if (hex == '#FFFFFF') {shirt_colors = shirt_colors[!(shirt_colors %in% c('white',  'ash'))]}
  if (hex == '#000000') {shirt_colors = shirt_colors[shirt_colors != 'black']}
  if (hex == '#F4DE5B') {shirt_colors = shirt_colors[shirt_colors != 'white']}
  
  renderUI({
    fluidRow(
      column(4, selInput(paste0('shirt_color', i), shirt_colors, 'garmet color')),
      column(4, selInput(paste0('shirt_size', i), shirt_sizes, 'garment size')),
      column(4, numericInput(paste0('q',i), NULL, 1, 1, step = 1))
    )
  })
}

intro_card <- function(img_file) {
  bsCollapsePanel(
    'intro',
    img(src = img_file, 
        align = 'center', 
        width = '100%', 
        height = 'auto')
  )
}

garment_card <- function(inputId, choices) {
  bsCollapsePanel(
    'garment type',
    p('select garment type (tee, hoodie, long sleeve)'),
    radioButtons_withHTML(inputId, 
                          NULL, 
                          choices = choices, 
                          inline = FALSE)
  )
}

image_size_card <- function(inputId, choices) {
  bsCollapsePanel(
    'graphic size',
    p('across the chest (7"x7") or left pocket (2.5"x2.5")'),
    radioButtons_withHTML(inputId, 
                          NULL, 
                          choices = choices, 
                          inline = FALSE)
  )
}

# http://stackoverflow.com/questions/35783446/can-you-have-an-image-as-a-radiobutton-choice-in-shiny
radioButtons_withHTML <- function (inputId, label, choices, selected = NULL, inline = TRUE, width = NULL) {
  choices <- shiny:::choicesWithNames(choices)
  selected <- if (is.null(selected)) 
    choices[[1]]
  else {
    shiny:::validateSelected(selected, choices, inputId)
  }
  if (length(selected) > 1) 
    stop("The 'selected' argument must be of length 1")
  options <- generateOptions_withHTML(inputId, choices, selected, inline, type = "radio")
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline) 
    divClass <- paste(divClass, "shiny-input-container-inline")
  tags$div(id = inputId, style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), class = divClass, 
    shiny:::controlLabel(inputId, label), options)
}

generateOptions_withHTML <- function (inputId, choices, selected, inline, type = "checkbox") {
  options <- mapply(choices, names(choices), FUN = function(value, name) {
    inputTag <- tags$input(type = type, name = inputId, value = value)
    if (value %in% selected) 
      inputTag$attribs$checked <- "checked"
    if (inline) {
      tags$label(class = paste0(type, "-inline"), inputTag, 
                 tags$span(HTML(name)))
    }
    else {
      tags$div(class = type, tags$label(inputTag, tags$span(HTML(name))))
    }
  }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  div(class = "shiny-options-group", options)
}