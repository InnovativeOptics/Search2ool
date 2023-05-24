library(tidyverse)
library(shiny)

our_data <- readxl::read_excel("data/Master_1.xlsx",
                               sheet = "Lens_details") %>% 
  mutate(VLT = scales::percent(as.numeric(VLT))) 

# oem_data <- readxl::read_excel("data/Master_1.xlsx",
#                                sheet = "Laser_types") %>% 
#   drop_na(`Eyewear Requirement`, `Compatible Lens 1`)
oem_data <- read.csv("data/oemDataSearch.csv")
search_data <- read.csv("data/wl_search_data_full.csv") %>% 
  select(2:4)

# Define server logic required to draw a histogram
function(input, output, session) {
  ### Search by wavelength section below
  fullData <- eventReactive(input$wl | input$od, {
    req(input$wl >= 200 & input$wl <= 11000)
    fullData <- search_data %>% 
      filter(Wavelength == round(input$wl, digits=0),
             OD >= input$od) %>% 
      arrange(OD)
    req(input$od <= max(fullData$OD))
    updateNumericInput(inputId = "od",max = max(fullData$OD))
    map(unique(fullData$Lens), ~tibble(filter(our_data, Lens == .x)))
  })
  # prototype dynamic rendering of tables -> tables, images, links
  output$tables <- renderUI({
    req(fullData())
    map(1:length(fullData()), ~renderTable(bordered = T,
                                           align = "c",
                                           striped=T,
                                           hover = F,
                                           width = "100%",
                                           colnames = T, na = "-",{
                                             fullData()[[.x]] %>% 
                                               select(c("Lens", "OD", "CE", "VLT","Material", "Summary"))
                                           })
    )
  })
  output$links <- renderUI({
    req(fullData())
    map(1:length(fullData()), ~HTML(
      c('<a href="',
        fullData()[[.x]]$Website,
        '", target = "_blank", title = "Click to browse frame styles">',fullData()[[.x]]$Lens,'</a>'
        
      ))
    )
  })
  output$images <- renderUI({
    req(fullData())
    map(1:length(fullData()), ~HTML(
      c('<a href="',
        fullData()[[.x]]$Website,
        '", target = "_blank", title = "Click to browse frame styles">
        <img src="',
        fullData()[[.x]]$Image,
        '", height = 65em></a>')
    ))
    
  })
  
  ### Search by laser type section below

  observeEvent(input$mfg,{
    freezeReactiveValue(input, "mod")
    # filter oem data to select mfg
    mfg_filtered_oem_data <- oem_data %>% 
      filter(`Mfg` == input$mfg)
    # update select input - laser model
    updateSelectInput(inputId = "mod",
                      choices = sort(unique(mfg_filtered_oem_data$`Mod`)),
                      selected = NULL)
  })
  selected_data <- eventReactive(input$mod,{
    req(input$mfg)
    selected_data <- oem_data %>% 
      filter(`Mfg` == input$mfg,
             `Mod` == input$mod)  
    
    map(unique(selected_data$Lens), ~tibble(filter(our_data, Lens == .x)))
  })
  
  output$tablesOEM <- renderUI({
    req(selected_data())
    map(1:length(selected_data()), ~renderTable(bordered = T,
                                           align = "c",
                                           striped=T,
                                           hover = F,
                                           width = "100%",
                                           colnames = T, na = "-",{
                                             selected_data()[[.x]] %>% 
                                               select(c("Lens", "OD", "CE", "VLT","Material", "Summary"))
                                           })
    )
  })
  output$linksOEM <- renderUI({
    req(selected_data())
    map(1:length(selected_data()), ~HTML(
      c('<a href="',
        selected_data()[[.x]]$Website,
        '", target = "_blank", title = "Click to browse frame styles">',selected_data()[[.x]]$Lens,'</a>'
        
      ))
    )
  })
  output$imagesOEM <- renderUI({
    req(selected_data())
    map(1:length(selected_data()), ~HTML(
      c('<a href="',
        selected_data()[[.x]]$Website,
        '", target = "_blank", title = "Click to browse frame styles">
        <img src="',
        selected_data()[[.x]]$Image,
        '", height = 65em></a>')
    ))
    
  })
  
  # 
  # laser_info <- eventReactive(input$mod,{
  #   tibble("Selected Device" = paste0(input$mfg, " ", input$mod),
  #          "Specifications (nm)" = selected_data()[[1]]$Eyewear.Requirement)
  # })
  # 
  # output$laserInfo <- renderTable(bordered = T,
  #                                 align = "c",
  #                                 striped=T,
  #                                 spacing='s',
  #                                 hover = F,
  #                                 {laser_info()})
  # output$devName <- renderText({
  #   paste0("Lenses for the ",
  #          input$mfg, " ", input$mod
  #   )
  # })
  # specs_data <- eventReactive(input$mod,{
  #   req(input$mfg)
  #   req(input$mod)
  #   tibble(our_data %>% 
  #            filter(`Lens` == head(selected_data()$`Compatible Lens 1`,1) | 
  #                     `Lens` == head(selected_data()$`Compatible Lens 2`,1) |
  #                     `Lens` == head(selected_data()$`Special Case`,1)) %>%
  #            mutate(VLT = scales::percent(as.numeric(VLT)),
  #                   "Part number" = Lens) %>% 
  #            select("Part number", "OD", "CE", "VLT", "Material", "Summary"))
  #   
  # })
  # 
  # lens_location1 <- eventReactive(input$mod,{
  #   our_data %>% 
  #     filter(`Lens` == head(selected_data()$`Compatible Lens 1`,1)) %>% 
  #     select(Image, Lens, Website)
  # })
  # lens_location2 <- eventReactive(input$mod,{
  #   our_data %>% 
  #     filter(`Lens` == head(selected_data()$`Compatible Lens 2`,1)) %>% 
  #     select(Image, Lens, Website)
  # })
  # lens_location3 <- eventReactive(input$mod,{
  #   our_data %>% 
  #     filter(`Lens` == head(selected_data()$`Special Case`,1)) %>% 
  #     select(Image, Lens, Website)
  # })
  # output$image_Lens1 <- renderText({
  #   req(input$mfg)
  #   req(input$mod)
  #   if(length(lens_location1()$Image) > 0){c('<img src="',
  #                                            lens_location1()$Image[[1]],
  #                                            '", height = 250px>')
  #   }
  # })
  # output$link_Lens1 <- renderText({
  #   req(input$mfg)
  #   req(input$mod)
  #   c('<a href="',
  #     lens_location1()$Website[[1]],
  #     '">',lens_location1()$Lens[[1]],' - frame styles</a>')
  #   
  # })
  # output$image_Lens2 <- renderText({
  #   req(input$mfg)
  #   req(input$mod)
  #   if(length(lens_location2()$Image) > 0){c('<img src="',
  #                                            lens_location2()$Image[[1]],
  #                                            '", height = 250px>')
  #   }
  # })
  # output$link_Lens2 <- renderText({
  #   req(input$mfg)
  #   req(input$mod)
  #   if(length(lens_location2()$Website) > 0){c('<a href="',
  #                                              lens_location2()$Website[[1]],
  #                                              '">',lens_location2()$Lens[[1]],' - frame styles</a>')
  #   }
  # })
  # output$image_Lens3 <- renderText({
  #   req(input$mfg)
  #   req(input$mod)
  #   if(length(lens_location3()$Image) > 0){c('<img src="',
  #                                            lens_location3()$Image[[1]],
  #                                            '", height = 250px>')}
  # })
  # output$link_Lens3 <- renderText({
  #   req(input$mfg)
  #   req(input$mod)
  #   if(length(lens_location3()$Website) > 0){c('<a href="',
  #                                              lens_location3()$Website[[1]],
  #                                              '">',lens_location3()$Lens[[1]],' IPL - frame styles</a>')}
  # })
  # output$prod1 <- renderTable(bordered = T,
  #                             align = "c",
  #                             striped=T,
  #                             hover = F,
  #                             width = "100%",
  #                             colnames = F, na = "-",
  #                             {lens1_tmp <- specs_data() %>% 
  #                               filter(`Part number` == lens_location1()$Lens[[1]])
  #                             tibble(" " = c("Lens", "OD", "CE", "VLT","Material", "Summary"),
  #                                    "Laser option 1" = t(lens1_tmp[1,]))
  #                             
  #                             
  #                             })
  # output$prod2 <- renderTable(bordered = T,
  #                             align = "c",
  #                             striped=T,
  #                             hover = F,
  #                             width = "100%",
  #                             colnames = F, na = "-",
  #                             {if(length(lens_location2()$Image) > 0){
  #                               lens2_tmp <- specs_data() %>% 
  #                                 filter(`Part number` == lens_location2()$Lens[[1]])
  #                               tibble(" " = c("Lens", "OD", "CE", "VLT","Material", "Summary"),
  #                                      
  #                                      "Laser option 2" = t(lens2_tmp[1,]))}
  #                               
  #                               
  #                             })
  # output$prod3 <- renderTable(bordered = T,
  #                             align = "c",
  #                             striped=T,
  #                             hover = F,
  #                             width = "100%",
  #                             colnames = F, na = "-",
  #                             {
  #                               if(length(lens_location3()$Image) > 0){
  #                                 lens3_tmp <- specs_data() %>% 
  #                                   filter(`Part number` == lens_location3()$Lens[[1]])
  #                                 tibble(" " = c("Lens", "OD", "CE", "VLT","Material", "Summary"),
  #                                        
  #                                        "Laser option 2" = t(lens3_tmp[1,]))}
  #                               
  #                               
                              }
