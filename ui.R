library(bslib)
library(shiny)
library(tidyverse)

our_data <- readxl::read_excel("data/Master_1.xlsx",
                               sheet = "Lens_details")

# oem_data <- readxl::read_excel("data/Master_1.xlsx",
#                                sheet = "Laser_types")  %>% 
#   drop_na(`Eyewear Requirement`, `Compatible Lens 1`)

oem_data <- read.csv("data/oemDataSearch.csv") 

page_fluid(theme = bs_add_variables(
  bs_theme(
    version = 5,
    base_font = font_google("Karla"),
    bg = "white",
    fg = "black",
    primary = "#00a033"
  ),
  "border-radius" = "5px"
),
navs_tab_card(
  nav(
    title = h3(strong("Search by laser type")),
    fluidRow(
      column(4, align = 'center', h4(
        strong("Select your device information")
      ))
      ,
      column(4,
             align = 'center',
             
               selectInput(
                 inputId = "mfg",
                 label = h4(strong("Manufacturer")),
                 choices = sort(unique(oem_data$`Mfg`)),
                 selected = NULL,
               )
             ),
      column(4, align = 'center',
             
               selectInput(
                 inputId = "mod",
                 label = h4(strong("Model")),
                 choices = NULL,
                 selected = NULL
               )
             )
    ),
      # fluidRow(
      #   column(6, align = 'center', h4(strong(em(
      #     textOutput("devName")
      #   )))),
      #   column(6,
      #          # print confirmation statement
      #          h5(tableOutput("laserInfo")), align =
      #            'right')
      # )
      # ,
    conditionalPanel("output.linksOEM",fluidRow(column(12,align='center',
                                                       h3(em("Compatible Lenses"))))),
        fluidRow(column(12, align = 'center', h1(uiOutput(
          "linksOEM"
        )))),
        fluidRow(column(12, align = 'center', h2(uiOutput(
          "imagesOEM"
        )))),
        fluidRow(column(12, h5(uiOutput("tablesOEM"))))
    
  ),
  nav(
    title = h3(strong("Search by wavelength")),
    fluidRow(
      column(4, align = "center",
             h4(
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
    conditionalPanel("output.links",fluidRow(column(12,align='center',
                     h3(em("Compatible Lenses"))))),
    fluidRow(column(12, align = 'center', h1(uiOutput(
      "links"
    )))),
    fluidRow(column(12, align = 'center', h2(uiOutput(
      "images"
    )))),
    fluidRow(column(12, h5(uiOutput("tables"))))
  )),
  card_footer(
    h4(
    a(href = "https://innovativeoptics.com/contact/", "Contact Us")
  ))
)