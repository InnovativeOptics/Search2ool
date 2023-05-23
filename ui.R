library(bslib)
library(shiny)
library(tidyverse)

our_data <- readxl::read_excel("data/Master_1.xlsx",
                               sheet = "Lens_details")

oem_data <- readxl::read_excel("data/Master_1.xlsx",
                               sheet = "Laser_types") %>% 
  drop_na(`Eyewear Requirement`, `Compatible Lens 1`)

page_fluid(theme = bs_add_variables(
  bs_theme(
    version = 5,
    base_font = font_google("Karla"),
    bg = "white",
    fg = "black",
    primary = "royalblue"
  ),
  "border-radius" = "1px"
),
navs_tab_card(
  nav(
    title = h2(strong("Search by laser type")),
    fluidRow(
      column(4, align = 'center', h3(
        strong("Select your device")
      ))
      ,
      column(4,
             align = 'center',
             h5(
               selectInput(
                 width = "20em",
                 inputId = "mfg",
                 label = strong("Manufacturer"),
                 choices = sort(unique(oem_data$`Mfg`)),
                 selected = NULL,
               )
             )),
      column(4, align = 'center',
             h5(
               selectInput(
                 width = "20em",
                 inputId = "mod",
                 label = strong("Model"),
                 choices = NULL,
                 selected = NULL
               )
             ))
    ),
      fluidRow(
        column(6,
               # print confirmation statement
               h5(tableOutput("laserInfo")), align =
                 'center'),
        column(6, align = 'center', h4(strong(em(
          textOutput("devName")
        ))))
      )
      ,
      h5(
        fluidRow(
          column(8,
                 tableOutput("prod1"),
                 align = 'center'),
          column(4,
                 h4(strong(
                   htmlOutput("link_Lens1")
                 )),
                 htmlOutput("image_Lens1"),
                 align = 'center')
        ),
        
        fluidRow(
          column(8,
                 tableOutput("prod2"),
                 align = 'center'),
          column(4,
                 h4(strong(
                   htmlOutput("link_Lens2")
                 )),
                 htmlOutput("image_Lens2"),
                 align = 'center')
        ),
        fluidRow(
          column(8,
                 tableOutput("prod3"),
                 align = 'center'),
          column(4,
                 strong(htmlOutput("link_Lens3")),
                 htmlOutput("image_Lens3"),
                 align = 'center')
        )
    )
  ),
  nav(
    title = h2(strong("Search by wavelength")),
    fluidRow(
      column(4, align = "center",
             h3(
               strong("Enter your required specifications")
             )),
      column(
        4,
        align = "center",
        numericInput(
          "wl",
          h4(strong("Wavelength")),
          min = 200,
          max = 11000,
          step = 1,
          value = NULL
        )
      ),
      column(
        4,
        align = "center",
        numericInput(
          "od",
          h4(strong("Optical Density")),
          min = 0,
          max = 10,
          step = 0.5,
          value = 0
        )
      )
    )
    ,
    conditionalPanel("output.links",
                     h3(em("Compatible Lenses"))),
    fluidRow(column(12, align = 'center', h1(uiOutput(
      "links"
    )))),
    fluidRow(column(12, align = 'center', h2(uiOutput(
      "images"
    )))),
    fluidRow(column(12, h5(uiOutput("tables"))))
  ),
  card_footer(h3(
    a(href = "https://innovativeoptics.com/contact/", "Contact Us")
  ))
))