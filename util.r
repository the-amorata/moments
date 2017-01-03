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

mk_url <- function(img_file, order_details, q, gs) {
  url_base = paste0('http://amorata.myshopify.com/cart/22746124421:', q, '?')
  file_base = 'http://amorata-apps.com:8787/files/apps/moments/plots/'
  
  img_attr = paste0('attributes[img-file]=', file_base, img_file)
  gs_attr  = paste0('attributes[graphic-size]=', gs)
  od_attr  = paste0('attributes[order-details]=', paste(order_details,  collapse = ' '))
  all_attr = paste(img_attr, gs_attr, od_attr, sep = '&')
  
  paste0(url_base, all_attr)
}

day_xy <- function(dt) {
  a = 149.6; b = 149.579116
  dt[,poty := doty(bday)/365]
  dt[,x :=  a*sin(2*pi*poty - pi/2)]
  dt[,y := -b*sin(2*pi*poty)]
}

mk_ref_dt <- function(i = 100) {
  a = 149.6; b = 149.579116
  data.table(x =  a*sin(seq(0, 2*pi, pi/i) - pi/2),
             y = -b*sin(seq(0, 2*pi, pi/i)))
}

render_ui_row <- function(i) {
  
  old_cols = c('#6784A9','#96CDC2','#F58C8D','#BC94C1','#C27186','#E7D49F')
  display_cols = c('#1b87e0', '#ed635f', '#6abf90', '#ff814a', '#8369a8', '#f4de5b' )
  
  manual_dates = c('2016-12-25', '2016-04-22', '1989-05-14', 
                   '1990-12-16', '2016-07-04', '2016-10-02')
  manual_labs = c('christmas', 'earth day', 'your birthday', 
                  'my birthday', 'moment #5', 'moment #6')
  
  renderUI({
    fluidRow(
      conditionalPanel(condition = "input.leg == true",
        column(4,
          textInput(paste0('labs',i), NULL, value = manual_labs[i]))),
      column(4,
        dateInput(paste0('date',i),
                  NULL,
                  startview = "year",
                  format = "M d",
                  value = manual_dates[i])),
      column(4,
             colourpicker::colourInput(paste0('cols',i), 
                                       NULL, 
                                       palette = 'limited', 
                                       allowedCols = display_cols,
                                       showColour = 'background',
                                       value = display_cols[i]))
    )
  })
}

ui_row_output <- function(i) {
  condition = paste0('input.n > ', i-1)
  inputid = paste0('r',i)
  conditionalPanel(condition, fluidRow(column(12,uiOutput(inputid))))
}

plot_orbit <- function(dt, ref, leg, mobile=FALSE) {
  fn = paste(Sys.time(), '.png', sep='')
  uhl = unique(dt[,.(hex, lab)])
  hf = factor(dt[,hl], levels = dt[,hl])
  leg_ncol = ifelse(nrow(uhl) < 4, 1, ifelse(nrow(uhl) > 4, 3, 2))
  if (leg == TRUE) {leg_val = 'bottom'} else {leg_val = 'none'}
  
  params = list(ps1=40, ps2=12, fs=35, lt=3.5, lm=0.5, lk=5)
  # if (mobile == TRUE) {params = lapply(params, function(x) x*(320/400))}
  
  p = ggplot() + 
    geom_path(data = ref, aes(x = x, y = y), size = params$lt, linetype = 1, color = '#4c4c4c') +
    geom_point(data = dt, aes(x = x, y = y, color = hf), size = params$ps1) +
    scale_color_manual(values = uhl[,hex], labels = uhl[,lab]) +
    ylim(-160, 160) + xlim(-160, 160) +
    guides(colour = guide_legend(override.aes = list(size=params$ps2, alpha = 1), ncol = leg_ncol)) +
    orbit_theme +
    theme(
      legend.margin = unit(params$lm, "line")
      ,legend.key.height = unit(params$lk, "line")
      ,legend.key.width = unit(params$lk, "line")
      ,legend.position = leg_val
      ,text = element_text(family = 'Futura Lt BT', size = params$fs, color = '#4c4c4c')
    ) + coord_fixed()
  p
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
)

sample_ligtbox <- function() {
  c('creme', 'summer peach', 'light blue')
  tags$div(
    fluidRow(
      column(4, align = 'center',
             h4('creme'),
             img(src = 'moments-creme.jpg', height = '260px')),
      column(4, align = 'center', 
             h4('summer peach'),
             img(src = 'moments-peach.jpg', height = '260px')),
      column(4, align = 'center',
             h4('light blue'),
             img(src = 'moments-blue.jpg', height = '260px'))
    )
  )
}

render_detail_row <- function(i) {
  shirt_sizes  = c('xs', 's', 'm', 'l', 'xl', 'xxl', 'xxxl')
  shirt_colors = c('creme', 'summer peach', 'light blue')
  
  renderUI({
    fluidRow(
      column(4, selInput(paste0('shirt_color', i), shirt_colors, 'shirt color')),
      column(4, selInput(paste0('shirt_size', i), shirt_sizes, 'shirt size')),
      column(4, numericInput(paste0('q',i), NULL, 1, 1, step = 1))
    )
  })
}

selInput <- function(inputId, choices, placeholder) {
  selectizeInput(inputId = inputId, 
                 label = NULL, 
                 choices = choices,
                 options = list(
                   placeholder = placeholder,
                   onInitialize = I('function() { this.setValue(""); }')))
}

# http://stackoverflow.com/questions/35783446/can-you-have-an-image-as-a-radiobutton-choice-in-shiny
radioButtons_withHTML <- function (inputId, label, choices, selected = NULL, inline = FALSE, width = NULL) {
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