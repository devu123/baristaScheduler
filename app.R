# scheduling baristas
# by devuroasts
#loading libraries
library(dplyr)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(readxl)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(testit)
# no data needed

ui <- shinyUI(fluidPage(
  
  tags$head(
    # css styling
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Abel');
      
    * {
        font-family: 'Abel';
        font-weight: 500;
        line-height: 1.5;
    }
    
  
    h3{
  color: #F8DE7E ;
  }
    .tabbable > .nav > li > a                  {background-color: #d9b382;  color:#043927}
    .tabbable > .nav > li[class=active]    > a {background-color: #043927; color:#d9b382}
    
      body {
        background-color: #E1C699;
      }
       
      footer{
        position: fixed;
        left: 8%;
        bottom: 2%;
        width:100%;
        height: 80px;
        color: 'E6E6E6';
        z-index: 1000;
      }
      @media (max-width: 600px) {
      footer {
        display: none;
        width: 0%;
        height: 0%;
      }
}
      
  )"
    ))
  ),
  
  # App title ----
  titlePanel("~Barista Scheduling~"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = 'left',
                
                # Sidebar panel for inputs ----
                
                sidebarPanel(
                              #add barista name and hours
                             uiOutput("baristaName"), 
                             uiOutput("timeInput1"), 
                             uiOutput("timeInput2"), 
                             uiOutput("timeInput3"), 
                             uiOutput("timeInput4"), 
                             uiOutput("timeInput5"), 
                             uiOutput("timeInput6"), 
                             uiOutput("timeInput7"),
                             
                             #submit button
                             actionButton("submit", label = "Add Barista"),
                             #clear all baristas
                             actionButton("clear", label = "Clear All Baristas"),
       
                             #add cafe hours open
                             uiOutput("cafeInput1"), 
                             uiOutput("cafeInput2"), 
                             uiOutput("cafeInput3"), 
                             uiOutput("cafeInput4"), 
                             uiOutput("cafeInput5"), 
                             uiOutput("cafeInput6"), 
                             uiOutput("cafeInput7"),
                             #submit cafe time
                             actionButton("submitHours", label = "Input Cafe Hours"),
                             # build model, and solve model                      
                             actionButton("run", label = "Schedule")
                          
                )
                ,
                # tabs for displaying outputs ----
                mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel(h3("Barista Availability"), 
                                       dataTableOutput("availDataFrame")),
                              tabPanel(h3("Barista Schedules"),
                                       dataTableOutput("cafeHoursDataFrame"),
                                       dataTableOutput("scheduleDataFrame"))
                  )
                  #HTML('<footer>
                   #    <strong>Devuroasts</strong> 
                  #  </footer>')
                  )
  )
) 
)


server <- shinyServer(function(input, output) {
  
  # reactive barista hour pairs
  
  baristaPairs <- reactiveValues()
  baristaPairs$df <- data.frame(X1 = numeric(0),
                                X2 = numeric(0),
                                X3 = numeric(0),
                                X4 = numeric(0),
                                X5 = numeric(0),
                                X6 = numeric(0),
                                X7 = numeric(0),
                                X8 = numeric(0))
  
  cafeTime <- reactiveValues()
  cafeTime$df <- data.frame(X1 = numeric(0),
                                X2 = numeric(0),
                                X3 = numeric(0),
                                X4 = numeric(0),
                                X5 = numeric(0),
                                X6 = numeric(0),
                                X7 = numeric(0))
  
  # on submit, update list of baristas and hours
  observeEvent(input$submit, {

    addToBaristaPairs <- data.frame(cbind(input$barista, input$timeRange1, input$timeRange2, 
                                          input$timeRange3, input$timeRange4, input$timeRange5,
                                          input$timeRange6,input$timeRange7))
    

    if(!(tolower(trimws(input$barista)) %in% c(0, "", " ") | is.na(input$barista))){
      
      names(addToBaristaPairs) <- c('barista', 'timeAvail1', 'timeAvail2', 'timeAvail3', 'timeAvail4', 
                          'timeAvail5', 'timeAvail6', 'timeAvail7')
      addRows <- rbind(baristaPairs$df, addToBaristaPairs)
      names(addRows) <- c('barista', 'timeAvail1', 'timeAvail2', 'timeAvail3', 'timeAvail4', 
                                                              'timeAvail5', 'timeAvail6', 'timeAvail7')
      isolate(baristaPairs$df <- addRows)

    }
    })
  
  # on submitHours, log cafe time
  observeEvent(input$submitHours, {
    
    cafeHours <- data.frame(cbind(input$cafeTimeRange1, input$cafeTimeRange2, 
                                          input$cafeTimeRange3, input$cafeTimeRange4, input$cafeTimeRange5,
                                          input$cafeTimeRange6,input$cafeTimeRange7))
    
    
      names(cafeHours) <- c('cafeTimeAvail1', 'cafeTimeAvail2', 'cafeTimeAvail3', 'cafeTimeAvail4', 
                                    'cafeTimeAvail5', 'cafeTimeAvail6', 'cafeTimeAvail7')
      isolate(cafeTime$df <- cafeHours)
      
    
  })
  
  
  #clear all barista hours recorded on clicking clear
  observeEvent(input$clear, {
    
    isolate(baristaPairs$df <- data.frame(X1 = numeric(0),
                                            X2 = numeric(0),
                                            X3 = numeric(0),
                                            X4 = numeric(0),
                                            X5 = numeric(0),
                                            X6 = numeric(0),
                                            X7 = numeric(0),
                                            X8 = numeric(0)))
    print('cleared all baristas')
  })
  
  # reactive inputs defined here - input barista name
  output$baristaName <- renderUI({
    textInput(inputId = 'barista',
              label = 'Barista Name',
              value = "",
              width = '100%',
              placeholder = NULL)
  })
  
  ###################
  # barista availability throughout week
  output$timeInput1 <- renderUI({  sliderInput("timeRange1", label = "Day 1 Availability",
                                              min = 0,
                                              max = 24,
                                              value = c(0,
                                                        0))  })
  
  output$timeInput2 <- renderUI({  sliderInput("timeRange2", label = "Day 2 Availability",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  output$timeInput3 <- renderUI({  sliderInput("timeRange3", label = "Day 3 Availability",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  output$timeInput4 <- renderUI({  sliderInput("timeRange4", label = "Day 4 Availability",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  output$timeInput5 <- renderUI({  sliderInput("timeRange5", label = "Day 5 Availability",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  output$timeInput6 <- renderUI({  sliderInput("timeRange6", label = "Day 6 Availability",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  output$timeInput7 <- renderUI({  sliderInput("timeRange7", label = "Day 7 Availability",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  
  
  #################  
  #cafe hours
  output$cafeInput1 <- renderUI({  sliderInput("cafeTimeRange1", label = "Cafe Day 1 Hours",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  output$cafeInput2 <- renderUI({  sliderInput("cafeTimeRange2", label = "Cafe Day 2 Hours",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  output$cafeInput3 <- renderUI({  sliderInput("cafeTimeRange3", label = "Cafe Day 3 Hours",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  output$cafeInput4 <- renderUI({  sliderInput("cafeTimeRange4", label = "Cafe Day 4 Hours",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  output$cafeInput5 <- renderUI({  sliderInput("cafeTimeRange5", label = "Cafe Day 5 Hours",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  output$cafeInput6 <- renderUI({  sliderInput("cafeTimeRange6", label = "Cafe Day 6 Hours",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  output$cafeInput7 <- renderUI({  sliderInput("cafeTimeRange7", label = "Cafe Day 7 Hours",
                                               min = 0,
                                               max = 24,
                                               value = c(0,
                                                         0))  })
  
  
  ##############################
  
  # show list of baristas
  baristaData <- reactive({
    df <- data.frame(Barista = baristaPairs$df$barista,
                     day1_avail = baristaPairs$df$timeAvail1,
                     day2_avail = baristaPairs$df$timeAvail2,
                     day3_avail = baristaPairs$df$timeAvail3,
                     day4_avail = baristaPairs$df$timeAvail4,
                     day5_avail = baristaPairs$df$timeAvail5,
                     day6_avail = baristaPairs$df$timeAvail6,
                     day7_avail = baristaPairs$df$timeAvail7)
    
      })
  
  # show hours of cafe
  cafeHoursData <- reactive({
    df2 <- data.frame(day1_cafe_hours = cafeTime$df$cafeTimeAvail1,
                     day2_cafe_hours = cafeTime$df$cafeTimeAvail2,
                     day3_cafe_hours = cafeTime$df$cafeTimeAvail3,
                     day4_cafe_hours = cafeTime$df$cafeTimeAvail4,
                     day5_cafe_hours = cafeTime$df$cafeTimeAvail5,
                     day6_cafe_hours = cafeTime$df$cafeTimeAvail6,
                     day7_cafe_hours = cafeTime$df$cafeTimeAvail7)
    
  })
  # making it easy to see who works
  
  solDF <- data.frame(day1_cafe_hours = rep(0,24),
                      day2_cafe_hours = rep(0,24),
                      day3_cafe_hours = rep(0,24),
                      day4_cafe_hours = rep(0,24),
                      day5_cafe_hours = rep(0,24),
                      day6_cafe_hours = rep(0,24),
                      day7_cafe_hours = rep(0,24), 
                      stringsAsFactors = F)
  
  
  ## scheduling
  
  # # on run button, schedule baristas
  # 
  observeEvent(input$run, {
    
  # only allow if input values exist of cafe hours and barista hours
  if(length(cafeTime$df$cafeTimeAvail1) > 0 &
         length( baristaPairs$df$barista) > 0){
    
  
    
    #   
    # cafeHoursTemp = data.frame('day1_cafe_hours' = c(0,6), 'day2_cafe_hours' = c(0,0), 'day3_cafe_hours' = c(0,0),
    #            'day4_cafe_hours' = c(0,0), 'day5_cafe_hours' = c(0,0), 'day6_cafe_hours' = c(0,0),
    #            'day7_cafe_hours' = c(0,0), stringsAsFactors = F)
    
    cafeHoursTemp <- data.frame(day1_cafe_hours = cafeTime$df$cafeTimeAvail1,
                                day2_cafe_hours = cafeTime$df$cafeTimeAvail2,
                                day3_cafe_hours = cafeTime$df$cafeTimeAvail3,
                                day4_cafe_hours = cafeTime$df$cafeTimeAvail4,
                                day5_cafe_hours = cafeTime$df$cafeTimeAvail5,
                                day6_cafe_hours = cafeTime$df$cafeTimeAvail6,
                                day7_cafe_hours = cafeTime$df$cafeTimeAvail7, 
                                stringsAsFactors = F)
    
    # get list of cafe open hours as binary
    
    cafeHoursOpen <<- rep(0, 24 * 7)
    
    hourToStart <<- 0
    # by iterating through dataframe sequence and assigning 1 where open (where barista needed)
    
    sapply(cafeHoursTemp, function(X){
      
      X <- data.frame(X, stringsAsFactors = F)
      
      for(i in seq(X[1, ], X[2, ] - 1)){
        if(X[1,] != X[2, ]){
          cafeHoursOpen[[i + hourToStart + 1]] <<- 1
        }
      }
      hourToStart <<- hourToStart + 24
      
    })
    
    # show in tab
    output$cafeHoursVector <- renderDataTable( data.frame(cafeHoursOpen), options = 
                                                 list(paging = F, searching = F, ordering=F, 
                                                      language = list(
                                                        zeroRecords = "")))
    
    # make list of baristas hours using same process. 
    
    # baristaHoursTemp  = data.frame('Barista' = c('jah','jah', 'rule','rule'), 'day1_avail' = c(0,3,3,6),'day2_avail' = c(0,3,3,6), 'day3_avail' = c(0,9,0,1),
    #            'day4_avail' = c(0,0,0,9), 'day5_avail' = c(0,5,5,9), 'day6_avail' = c(0,9,0,1),
    #           'day7_avail' = c(0,8,8,9), stringsAsFactors = F)
    baristaHoursTemp = data.frame(Barista = baristaPairs$df$barista,
                                  day1_avail = baristaPairs$df$timeAvail1,
                                  day2_avail = baristaPairs$df$timeAvail2,
                                  day3_avail = baristaPairs$df$timeAvail3,
                                  day4_avail = baristaPairs$df$timeAvail4,
                                  day5_avail = baristaPairs$df$timeAvail5,
                                  day6_avail = baristaPairs$df$timeAvail6,
                                  day7_avail = baristaPairs$df$timeAvail7,
                                  stringsAsFactors = F)
    
    
    
    baristaHoursAvail <<- rep(0, 24 * 7 * length(unique(baristaHoursTemp$Barista)))
    
    baristaList <<- vector(mode = "list", length(unique(baristaHoursTemp$Barista))/2)
    baristaListCounter <<- 1
    baristaAdd <<- 0
    
    for( i in seq(1, length(baristaHoursTemp$Barista), 2)){
      
      # append name to barista list
      baristaList[[baristaListCounter]] <- as.character(baristaHoursTemp$Barista[[i]])
      baristaListCounter <<- baristaListCounter + 1
      
      # make running vector of hours per barista
      hourToStart <- 0
      
      sapply(baristaHoursTemp[which(baristaHoursTemp$Barista == baristaHoursTemp$Barista[[i]]), c(-1)], function(X){
        
        X <- data.frame(X, stringsAsFactors = F)
        # if last number isnt 0
        X[] <- lapply(X[], as.character)
        
        if(as.numeric(X[2, ]) != 0){
          
          
          # iterate through and assign 1 in the availability matrix for that barista
          for(q in seq(as.numeric(X[1, ]), as.numeric(X[2, ]) - 1)){
            if(as.numeric(X[1,]) != as.numeric(X[2, ])){
              
              baristaHoursAvail[[q + hourToStart + baristaAdd + 1]] <<- 1
            }
          }
        }
        hourToStart <<- hourToStart + 24
        
      })
      
      baristaAdd <<- baristaAdd + (24 * 7)
      
    }
    
    
    #reshape to matrix
    baristaHoursAvailMatrix <- matrix(baristaHoursAvail,ncol = length(unlist(baristaList)))

            
    #showing on panel
    output$baristaHoursVectors <- renderDataTable( data.frame(baristaHoursAvailMatrix), options = 
                                                     list(paging = F, searching = F, ordering=F, 
                                                          language = list(
                                                            zeroRecords = "")))
    
    
    baristaAvail <- data.frame(baristaHoursAvailMatrix, stringsAsFactors = F)
    baristaAvail$rowNames <- seq(1, nrow(baristaAvail))
    
    amountOfBaristas <- ncol(baristaHoursAvailMatrix)
    amountOfHours <- nrow(baristaHoursAvailMatrix)
    
    
    
    # which hours cant specific baristas work?
    baristaUnableConstraints = character()

    
    for( i in 1:length(baristaList)){
      
      baristaUnableAdd <- as.numeric(baristaAvail[which(baristaAvail[i] == 0), 'rowNames'])
      # add to the hours they cannot do
      print(baristaUnableAdd)
      if(length(baristaUnableAdd) > 0){
        
        baristaUnableConstraints <- paste0(baristaUnableConstraints, '
      add_constraint(sum_expr(x[i, j], i = ',i,', j = c(',
                                           paste0(baristaUnableAdd, collapse = ','),
                                           ')) == 0) %>% ')
        
      }
    }
    
    
    eval(parse( text = paste0("
    model <- MILPModel() %>% 
      add_variable(x[i,j], i = 1:amountOfBaristas, j = 1:amountOfHours, type = 'binary') %>%

      add_constraint(sum_expr(x[i, j], i = 1:amountOfBaristas) == cafeHoursOpen[j], j = 1:amountOfHours) %>%

      ",
                              paste0(baristaUnableConstraints, collapse = ''),
                              
                              "set_objective(sum_expr(x[i,j], i = 1:amountOfBaristas, j = 1:amountOfHours))")))
    
    #solving for solution
    result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
    
    if(result$status != 'infeasible'){
      
      sol = result %>% get_solution(x[i,j]) %>% filter(value == 1)
      
      # formatting nicely
      sol = sol %>% rowwise %>% do({
        X <- data.frame(., stringsAsFactors = F)
        X$variable <- NULL
        X$i <- baristaList[[X$i]]
        X$value <- 'CHOSEN'
        X
      })
      
      # making it easy to see who works
      
      sol$day <- ceiling(sol$j/24)
      # updating who works which day
      for(day in sol$day){
        solDay = sol[which(sol$day == day), ]
        
        rowsToUpdate <- solDay$j %% 24
        solDF[rowsToUpdate, day] <<- solDay$i
        
      }
     
      cafeScheduleDF <- solDF
      
    } else {
      cafeScheduleDF <- solDF[F, ]
     
    }
    
    #reset solution incase re run
    solDF <<- data.frame(day1_cafe_hours = rep(0,24),
                        day2_cafe_hours = rep(0,24),
                        day3_cafe_hours = rep(0,24),
                        day4_cafe_hours = rep(0,24),
                        day5_cafe_hours = rep(0,24),
                        day6_cafe_hours = rep(0,24),
                        day7_cafe_hours = rep(0,24), 
                        stringsAsFactors = F)
    
    # output solution
    if (length(cafeScheduleDF[cafeScheduleDF == 0]) > 0){
      
      cafeScheduleDF[cafeScheduleDF == 0] <- "~"
      
    }
    
    output$scheduleDataFrame <- renderDataTable( cafeScheduleDF, options = 
                                                   list(paging = F, searching = F, ordering=F, 
                                                        language = list(
                                                          zeroRecords = "No schedule possible!")))
    
  }
  })
  
  
  
  
  
  
  
  # standardize sig figs and no sci notation
  options(digits = 2)
  options(scipen = 999)
  
  #expose outputs
  
  output$availDataFrame <- renderDataTable( baristaData(), options = 
                                              list(paging = F, searching = F, ordering=F, 
                                                   language = list(
                                                     zeroRecords = "Added Baristas will show up here")))
  
  output$cafeHoursDataFrame <- renderDataTable( cafeHoursData(), options = 
                                                  list(paging = F, searching = F, ordering=F, 
                                                       language = list(
                                                         zeroRecords = "Cafe hours will show up here")))
  
  
}
)

#run app
shinyApp(ui, server)
