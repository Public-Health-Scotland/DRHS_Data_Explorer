#Name: Drug Related Hospital Statistics (DRHS) Data explorer page
#Author: Mike Smith
#Created: 24/01/2019
#Type: Data visualisation
#Written on: RStudio
#Written for: R version 3.5.1 
#Output: Shiny application

#This is the full version of the data explorer for the new DRHS dashboard. 
#The individual tabs are first constructed from smaller apps and then finally added in 
#to this one larger app

#Currently there are tabs for 
# Tab 1) A Home Page
# Tab 2) Time Trend (Geography)
# Tab 3) Time Trend (Substances)
# Tab 4) Age/Sex 

#There are two further tabs that are present but content still needs to be loaded
# Tab 5) Deprivation
# Tab 6) Table


#libraries
library(shiny)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(stringr)
library(DT)

##############################################.
############## Reading In Data ----
##############################################.


#So initially we need to read in the data. 

#Time trend data- this is for the first two tabs of the time trend broken 
#down to allow comparison across 1) Geography and 2) Drug admission 

#NOTE- the following is temporary and is conditional on 
#1) Where we decided to eventually store the final data set 
#2) The final format of the data set
time_trend <- read.csv("Z:\\Topics\\DrugRelatedHospitalStats\\Publications\\DRHS\\20181218\\Temp\\s05-temp01_XXXX_num_rate.csv")

#For now I will simply rename the columns to fit with the code (rather than
#having to go through and change all of the code)
colnames(time_trend)<- (c("Hospital.Clinical.Type",
                            "Activity", 
                            "Geography.Type",
                            "Geography",
                            "Years",
                            "Substance",
                            "Measure",
                            "Values"))

#We then create the options for users to choose from in the drop down menus. 
#Drug Types are created as list to allow different options dependent on the 
#Hospital admission types
clinical_types <- as.character(unique(time_trend$Hospital.Clinical.Type))
activity_measure <- as.character(unique(time_trend$Activity))
location_types <- as.character(unique(time_trend$Geography.Type))
locations<- as.character(unique(time_trend$Geography))
Scotland<-locations[1:3]
Health_Board<-locations[4:17]
ADP<- locations[18:48]
drug_types<- as.character(unique(time_trend$Substance))
drug_types1<- list("Main Categories" = as.character(unique(time_trend$Substance)[1:7]),
                  "Opioids Sub Categories" = as.character(unique(time_trend$Substance)[8:10]))
drug_types2<- as.character(unique(time_trend$Substance)[1:7])
measures<- as.character(unique(time_trend$Measure))


#Age and Sex data- this is for the Age/Sex demographic
age_sex <- read.csv("Z:\\Topics\\DrugRelatedHospitalStats\\Publications\\DRHS\\20181218\\Dashboard\\Data_Explorer2\\Age_Sex_Skeleton.csv")
#add in age/sex options
age <- as.character(unique(age_sex$Age))
sex <- as.character(unique(age_sex$Sex))
financial_years <- as.character(unique(age_sex$Years))
#we need to look at altering the data for the tornado chart so that male values
#negative to allow it to work 
#Convert males to negative (and remove all)
age_sex_male <- age_sex %>%
  filter(Sex == "Male"
         & Age != "All") %>%
  mutate(Values = Values * -1)
#Then remove females
age_sex_female <- age_sex %>%
  filter(Sex == "Female"
         & Age != "All")

#recombine them into one chart
age_sex_tornado <- rbind(age_sex_male, age_sex_female)

####Temporary####
#This is a temporary stop-gap as we move from replacing the skeleton data
#with real data and the change in factor names
#The following will be used for selection in the age/sex category
#until real data is acquired 
clinical_types2 <- as.character(unique(age_sex$Hospital.Clinical.Type))
drug_types1_1<- list("Main Categories" = as.character(unique(age_sex$Substance)[c(1,5:10)]),
                   "Opioids Sub Categories" = as.character(unique(age_sex$Substance)[2:4]))
drug_types2_2<- as.character(unique(age_sex$Substance)[c(1,5:10)])


##############################################.
############## User Interface ----
##############################################.
  {
    #Beginning of UI
    ui <- fluidPage(
      style = "width: 100%; height: 100%; max-width: 1200px;",
      tags$head(
        tags$style(
          type = "text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"
        ),
        
        #The following chunk of code does two things:
        # 1. Paints the ribbon that contains the tab headers white.
        # 2. Highlights the header of the active tab in blue.

        
        tags$style(
          HTML("
        .tabbable > .nav > li > a {color: #000000;}
        .tabbable > .nav > li[class = active] > a {background-color: #0072B2;color: #FFFFFF;}
            ")
)
          ),

#We are going to divide our UI into discrete sections, called tab panels.
#To do this, we need the layout "tabsetPanel()".

tabsetPanel(
  id = "Panels",
  
##############################################.
############## Home tab ----
##############################################.
  
  #We begin with an introduction tab, where we introduce the explorer and...
  #its purpose.
  #
  
  tabPanel(
    "Home",
    icon = icon("info-circle"),
    style = "float: top; height: 95%; width: 95%;
    background-color: #FFFFFF; border: 0px solid #FFFFFF;",
    column(2,
           h3("Data explorer")
    ),
    column(
      8,
      p(
        br(),
        "The explorer allows you to visualise DRHS data
        in a variety of ways."
      ),
      tags$ul(
        tags$li(
          tags$b(actionLink(
            "link_to_geography", "Geography"
          )),
          icon("line-chart"),
          " - shows data on specific DRHS activty over time, by comparing by location."
      ),
      
        tags$li(
          tags$b(actionLink(
            "link_to_substances", "Substances"
          )),
          icon("line-chart"),
          " - shows data on specific DRHS activty over time, by comparing by substances."
        ),
      tags$li(
        tags$b(actionLink(
          "link_to_age_sex", "Age/Sex"
        )),
        icon("child"),
        " - shows data on specific DRHS activty by age and sex."
      ),
      tags$li(
        tags$b(actionLink(
          "link_to_deprivation", "Deprivation"
        )),
        icon("bar-chart"),
        " - shows data on specific DRHS activty by deprivation."
      ),
      tags$li(
        tags$b(actionLink(
          "link_to_table", "Table"
        )),
        icon("table"),
        " - data tables that underly graphs."
      )
      ),
      
      
      p(
        "When using the data explorer, please take the following
        factors into consideration:"
      ),
      tags$ul(tags$li("Insert text here."),
              tags$li("More text here.")),
      
      p("Still need to amend this page"),
      
      p(
        br(),
        "If you have any trouble using the explorer or have further
        questions relating to the data, please contact us at:",
        tags$b(
          tags$a(href = "mailto:nss.isdtransformingpublishing@nhs.net",
          "nss.isdtransformingpublishing@nhs.net.")
          )
      )
      
      #End of column 8 part
      )
    #End of tab panel
  ),
  
  
##############################################.
############## Geography tab ----
##############################################.
  
  #Create a tab for geography data.
  #Insert the description a
  
  tabPanel(
    "Geography",
    icon = icon("line-chart"),
    style = "height: 95%; width: 95%; background-color: #FFFFFF;
    border: 0px solid #FFFFFF;",
    h3("Geography"),
    p(
      HTML(
        "This section will contain text about the DRHS data followed
        by link to the 4 types of buttons and lots of graphs that 
        explains all sort of geography stuff"
      )
      ),
    
    tags$ul(
      tags$li(
        tags$b("Download plot as a png"),
        icon("camera"),
        " - click this button to save the graph as an image
        (please note that Internet Explorer does not support this
        function)."
      ),
      tags$li(
        tags$b("Zoom"),
        icon("search"),
        " - zoom into the graph by clicking this button and then
        clicking and dragging your mouse over the area of the
        graph you are interested in."
      ),
      tags$li(
        tags$b("Pan"),
        icon("move", lib = "glyphicon"),
        " - adjust the axes of the graph by clicking this button
        and then clicking and moving your mouse in any direction
        you want."
      ),
      tags$li(
        tags$b("Reset axes"),
        icon("home"),
        " - click this button to return the axes to their
        default range."
      )
      ),
    
    p(
      br(),
      tags$b(
        "Note: Statistical disclosure control has been applied to protect
        patient confidentiality. Therefore, the figures presented here
        may not be additive and may differ to previous
        sources of information."
      )
      ),
    
    p(""),
    
    wellPanel(
      tags$style(
        ".well { background-color: #FFFFFF;
        border: 0px solid #336699; }"
      ),
      
      #Insert the reactive filters.
      #We have SIX filters at this point 
      # 1 - Hospital/Clinical type
      # 2 - Activity Measure
      # 3 - Geography Type
      # 4 - Geography (Multiple)
      # 5 - Substance
      # 6 - Measure
      
      column(
        4,
        
        uiOutput("time_trend_clinical_type")
        
      ),
      
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Activity_Measure",
          label = "Select Activity Type",
          choices = activity_measure,
          selected = "Stays"
        )
        ),
        
        column(
          4,
          
          uiOutput("time_trend_substance1")
          ),
      
      column(
        4 ,
        uiOutput("time_trend_location_types"), 
        downloadButton(outputId = "download_geography", 
                       label = "Download data", 
                       class = "geographybutton"),
        
        tags$head(
          tags$style(".geographybutton { background-color: 
                   #0072B2; } 
                   .geographybutton { color: #FFFFFF; }")
        )
        
      ), 
      column(
        4 ,
        uiOutput("time_trend_locations")
      ), 
      
              column(
                4 ,
                shinyWidgets::pickerInput(
                  inputId = "Measure",
                  label = "Select Measure",
                  choices = measures,
                  selected = "Rate"
                )
        
      )
      
    ),
    
    #In the main panel of the tab, insert the geography plot
    
    mainPanel(
      width = 12,
      plotlyOutput("geography_plot",
                   width = "1090px",
                   height = "600px"),HTML("<button data-toggle = 'collapse' href = '#geography'
                   class = 'btn btn-primary' id = 'geography_link'> 
                                          <strong> Show/hide table </strong></button>"),
      HTML("<div id = 'geography' class = 'collapse'>"),
      br(),
      dataTableOutput("geography_table"),
      HTML("</div>"),
      br(),
      br()
    )
    
    #End of tab panel
    ),

##############################################.
############## Substances tab ----
##############################################.

  tabPanel(
    "Substances",
    icon = icon("line-chart"),
    style = "height: 95%; width: 95%; background-color: #FFFFFF;
    border: 0px solid #FFFFFF;",
    h3("Substances"),
    p(
      HTML(
        "This section will contain text about the DRHS data followed
        by link to the 4 types of buttons and lots of graphs that 
        explains all sort of substances related stuff"
      )
      ),
    
    tags$ul(
      tags$li(
        tags$b("Download plot as a png"),
        icon("camera"),
        " - click this button to save the graph as an image
        (please note that Internet Explorer does not support this
        function)."
      ),
      tags$li(
        tags$b("Zoom"),
        icon("search"),
        " - zoom into the graph by clicking this button and then
        clicking and dragging your mouse over the area of the
        graph you are interested in."
      ),
      tags$li(
        tags$b("Pan"),
        icon("move", lib = "glyphicon"),
        " - adjust the axes of the graph by clicking this button
        and then clicking and moving your mouse in any direction
        you want."
      ),
      tags$li(
        tags$b("Reset axes"),
        icon("home"),
        " - click this button to return the axes to their
        default range."
      )
      ),
    
    p(
      br(),
      tags$b(
        "Note: Statistical disclosure control has been applied to protect
        patient confidentiality. Therefore, the figures presented here
        may not be additive and may differ to previous
        sources of information."
      )
      ),
    
    p(""),
    
    wellPanel(
      tags$style(
        ".well { background-color: #FFFFFF;
        border: 0px solid #336699; }"
      ),
      
      #Insert the reactive filters.
      #We have SIX filters at this point 
      # 1 - Hospital/Clinical type
      # 2 - Activity Measure
      # 3 - Geography Type
      # 4 - Geography (Multiple)
      # 5 - Substance
      # 6 - Measure
      
      column(
        6,
        
        uiOutput("time_trend_clinical_type2")
        
        
      ),
      
      column(
        6,
        shinyWidgets::pickerInput(
          inputId = "Activity_Measure2",
          label = "Select Activity Type",
          choices = activity_measure,
          selected = "Stays"
        )
      ),
      
      column(
        6,
        uiOutput("time_trend_location_types2"),
        
        uiOutput("time_trend_substance2"),
        downloadButton(outputId = "download_substances", 
                       label = "Download data", 
                       class = "substancesbutton"),
        
        tags$head(
          tags$style(".substancesbutton { background-color: 
                     #0072B2; } 
                     .substancesbutton { color: #FFFFFF; }")
          )
      ),
     
      
      column(
        6,
        uiOutput("time_trend_locations2"),
        
        
        shinyWidgets::pickerInput(
          inputId = "Measure2",
          label = "Select Measure",
          choices = measures,
          selected = "Rate"
        )
        
        
      )
    ),
    
    #In the main panel of the tab, insert the substances plot
    
    mainPanel(
      width = 12,
      plotlyOutput("substances_plot",
                   width = "1090px",
                   height = "600px"), 
      HTML("<button data-toggle = 'collapse' href = '#substances'
                   class = 'btn btn-primary' id = 'substances_link'> 
                   <strong> Show/hide table </strong></button>"),
      HTML("<div id = 'substances' class = 'collapse'>"),
      br(),
      dataTableOutput("substances_table"),
      HTML("</div>"),
      br(),
      br()
    )
    
    #End of tab panel
      ),


##############################################.
############## Age/Sex tab ----
##############################################.

tabPanel(
  "Age/Sex",
  icon = icon("child"),
  style = "height: 95%; width: 95%; background-color: #FFFFFF;
  border: 0px solid #FFFFFF;",
  h3("Age/Sex"),
  p(
    HTML(
      "This section will contain text about the DRHS data followed
      by link to the 4 types of buttons and lots of graphs that
      explains all sort of demographic age/sex"
    )
    ),
  
  tags$ul(
    tags$li(
      tags$b("Download plot as a png"),
      icon("camera"),
      " - click this button to save the graph as an image
      (please note that Internet Explorer does not support this
      function)."
    ),
    tags$li(
      tags$b("Zoom"),
      icon("search"),
      " - zoom into the graph by clicking this button and then
      clicking and dragging your mouse over the area of the
      graph you are interested in."
    ),
    tags$li(
      tags$b("Pan"),
      icon("move", lib = "glyphicon"),
      " - adjust the axes of the graph by clicking this button
      and then clicking and moving your mouse in any direction
      you want."
    ),
    tags$li(
      tags$b("Reset axes"),
      icon("home"),
      " - click this button to return the axes to their
      default range."
    )
    ),
  
  p(
    br(),
    tags$b(
      "Note: Statistical disclosure control has been applied to protect
      patient confidentiality. Therefore, the figures presented here
      may not be additive and may differ to previous
      sources of information."
    )
    ),
  
  p(""),
  
  wellPanel(
    tags$style(
      ".well { background-color: #FFFFFF;
      border: 0px solid #336699; }"
    ),
    
    #Insert the reactive filters.
    #We have FOUR filters at this point
    # 1 - Hospital/Clinical type
    # 2 - Activity Measure
    # 3 - Substance (dependent on Hospital/Clinic Type)
    # 4 - Measure
    
    column(6,
           uiOutput("age_sex_clinical_type")),
    
    column(
      6,
      shinyWidgets::pickerInput(
        inputId = "Activity_Measure3",
        label = "Select Activity Measure",
        choices = activity_measure,
        selected = "Stays"
      )
    ),
    
    column(6,
           uiOutput("age_sex_substance")),
    
    column(
      6 ,
      shinyWidgets::pickerInput(
        inputId = "Measure3",
        label = "Select Measure",
        choices = measures,
        selected = "Rate"
      )
    )
  ),
  p(
    br(),
    HTML(
      "Maybe insert some text here at this point to explain clearly that
      you can choose between the two options here? Maybe opt for a different
      colour scheme for the options as well? Tried altering the colour scheme
      but the css code required defeated me"
    )
    ),
  
  #In the main panel of the tab, insert the time trend plot
  mainPanel(tabsetPanel(
    type = "pills",
    tabPanel(
      "Time Trend",
      icon = icon("line-chart"),
      style = "height: 95%; width: 95%; background-color: #FFFFFF;
      border: 0px solid #FFFFFF;",
      br(),
      br(),
      column(
        6,
        shinyWidgets::pickerInput(
          inputId = "Age",
          label = "Select Age",
          choices = age,
          multiple = TRUE,
          selected = "All"
        ),
        downloadButton(
          outputId = "download_age_sex_trend",
          label = "Download data",
          class = "myagesextrendbutton"
        ),
        tags$head(
          tags$style(
            ".myagesextrendbutton { background-color:
            #0072B2; }
            .myagesextrendbutton { color: #FFFFFF; }"
          )
        )
      ),
      
      column(
        6,
        shinyWidgets::pickerInput(
          inputId = "Sex",
          label = "Select Sex",
          choices = sex,
          multiple = TRUE,
          selected = "All"
        )
      ),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      plotlyOutput("age_sex_time_plot",
                   width = "1090px",
                   height = "600px"),
      HTML(
        "<button data-toggle = 'collapse' href = '#ageandsextrend'
        class = 'btn btn-primary' id = 'age_and_sex_link'>
        <strong>Show/hide table</strong></button>"
      ),
      HTML("<div id = 'ageandsextrend' class = 'collapse'>"),
      br(),
      dataTableOutput("age_sex_trend_table"),
      HTML("</div>"),
      br(),
      br()
      ),
    
    tabPanel(
      "Bar Chart",
      icon = icon("bar-chart"),
      style = "height: 95%; width: 95%; background-color: #FFFFFF;
      border: 0px solid #FFFFFF;",
      br(),
      br(),
      column(
        12,
        shinyWidgets::sliderTextInput(
          inputId = "Financial_Year",
          label = "Select Financial Year",
          choices = financial_years,
          selected = "2017/2018",
          grid = T,
          animate = T,
          width = "1090px"
        ),
        downloadButton(
          outputId = "download_age_sex_year",
          label = "Download data",
          class = "myagesexyearbutton"
        ),
        tags$head(
          tags$style(
            ".myagesexyearbutton { background-color:
                #0072B2; }
                .myagesexyearbutton { color: #FFFFFF; }"
          )
        )
      ),
      mainPanel(
        width = 12,
        plotlyOutput("age_sex_year_plot",
                     width = "1090px",
                     height = "600px"),
        HTML(
          "<button data-toggle = 'collapse' href = '#ageandsexyear'
              class = 'btn btn-primary' id = 'age_and_sex_link'>
              <strong>Show/hide table</strong></button>"
        ),
        HTML("<div id = 'ageandsexyear' class = 'collapse'>"),
        br(),
        dataTableOutput("age_sex_year_table"),
        HTML("</div>"),
        br(),
        br()
      )
    )
    ))
  
  #End of tab panel
    ),

##############################################.
############## Deprivation tab ----
##############################################.
tabPanel(
  "Deprivation",
  icon = icon("bar-chart")
    ),


##############################################.
############## Table tab ----
##############################################.

tabPanel(
  "Table",
  icon = icon("table")
)

  #End of tabset panel
    )
#End of UI part
  )
    
    
##############################################.      
############## Server ----
##############################################.  
    
    server  <-  function(input, output, session)
    {
      #These observeEvent() commands will be combined with action buttons in...
      #the User Interface to allow the user to navigate to each tab by clicking...
      #on links in the Introduction page (in addition to the classic way of...
      #navigating, which is by clicking on the tab header itself).
      
##############################################.
############## Home Tab ----
##############################################.
      
      observeEvent(
        input$link_to_geography,
        {
          updateTabsetPanel(session, "Panels", selected = "Geography")
        })
      observeEvent(
        input$link_to_substances,
        {
          updateTabsetPanel(session, "Panels", selected = "Substances")
        })
      observeEvent(
        input$link_to_age_sex,
        {
          updateTabsetPanel(session, "Panels", selected = "Age/Sex")
        })
      observeEvent(
        input$link_to_deprivation,
        {
          updateTabsetPanel(session, "Panels", selected = "Deprivation")
        })
      observeEvent(
        input$link_to_table,
        {
          updateTabsetPanel(session, "Panels", selected = "Table")
        })
      

##############################################.
############## Geography tab ----
##############################################.  
      
      #We need to include the input for geography types in the server section 
      #rather than the UI section. 
      #This is because the 'location' input is dependent on the 'location
      #type' input. 
      
      
      output$time_trend_location_types <- renderUI({
        shinyWidgets::pickerInput(inputId = "Geography_type", 
                                  label = "Select Location Type (multiple selection)",
                                  choices = location_types, 
                                  selected = "Scotland", 
                                  multiple = TRUE)
      })
      #As Geography Type "Scotland" now has three options within it
      #we may have to switch to if/else function. 

      output$time_trend_locations <- renderUI({
        shinyWidgets::pickerInput(inputId = "Geography",
                                  label = "Select Location (multiple selection)",  
                                  choices =
                                    if(input$Geography_type != "Scotland")
                                    {c("Scotland",
                                       sort(
                                         unique(
                                           as.character(
                                             time_trend$Geography
                                             [time_trend$Geography.Type == input$Geography_type]
                                           )
                                         )
                                       )
                                    )
                                    }
                                  else{
                                    sort(
                                      unique(
                                        as.character(
                                          time_trend$Geography
                                          [time_trend$Geography.Type == input$Geography_type]
                                        )
                                      )
                                    )
                                    }
                                  ,
                                  multiple = TRUE,
                                  selected = "Scotland"
        )
      }) 
      
   
### The following works in a fashion in that as long as Health Board is chosen as 
### a location type then the location output works- otherwise not?
      
#      output$time_trend_locations <- renderUI({
#        shinyWidgets::pickerInput(
#          inputId = "Geography",
#          label = "Select location (multiple selections allowed)",
#          choices =
#            if (input$Geography_type [1] == "Scotland" &
#                input$Geography_type [2] == "ADP" &
#                input$Geography_type [3] == "Health Board") {
#              c(Scotland, Health_Board, ADP)
#            } else if (input$Geography_type [1] == "Scotland" &
#                       input$Geography_type [2] == "ADP") {
#              c(Scotland, ADP)
#            } else if (input$Geography_type [1] == "Scotland" &
#                       input$Geography_type [2] == "Health Board") {
#              c(Scotland, Health_Board)
#            } else if (input$Geography_type [1] == "ADP" &
#                       input$Geography_type [2] == "Health Board") {
#              c(Health_Board, ADP)
#            } else if (input$Geography_type == "Scotland") {
#              Scotland
#            } else if (input$Geography_type == "ADP") {
#               ADP
#            } else if (input$Geography_type == "Health Board") {
#               Health_Board
#            } 
#          ,
#          multiple = TRUE,
#          selected = "Scotland"
#        )
#      })
      
      output$time_trend_clinical_type <- renderUI({
        shinyWidgets::pickerInput(inputId = "Hospital_Clinic_Type", 
                                  label = "Select Hospital-Clinical Type",
                                  choices = clinical_types, 
                                  selected = "Combined (General acute/Psychiatric) - Combined (Mental and Behavioural/Overdose)")
      })

      output$time_trend_substance1 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Substances",
                                  label = "Select Drug Type",  
                                  choices = (if(str_detect(input$Hospital_Clinic_Type, " Overdose"))
                                    drug_types1
                                    else
                                      drug_types2), 
                                  selected = "Opioids"
        )
      }) 
      
         
      #we can then plot the graph based on the user input.
      #First we create a subset based on user input
      
      geography_new <- reactive({
        time_trend %>%
          filter(
            Hospital.Clinical.Type %in% input$Hospital_Clinic_Type
            & Activity %in% input$Activity_Measure
            & Geography %in% input$Geography
            & Substance %in% input$Substances
            & Measure %in% input$Measure 
          )
      })
      
      #then we can plot the actual graph, with labels
      output$geography_plot <- renderPlotly({
        #first the tooltip label
        tooltip_geography <- paste0(
          "Financial year: ",
          geography_new()$Years,
          "<br>",
          "Location: ",
          geography_new()$Geography,
          "<br>",
          "Clinical Type: ",
          geography_new()$Hospital.Clinical.Type,
          "<br>",
          geography_new()$Measure,": ",
          geography_new()$Values
        )
        
        #Create the main body of the chart.
        
        plot_ly(
          data = geography_new(),
          #plot- we wont bother at this point with tailored colour
          x = ~  Years,
          y = ~  Values,
          color = ~  Geography,
            
          #tooltip
          text = tooltip_geography,
          hoverinfo = "text",
          #type
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(size = 8),
          width = 1000,
          height = 600
        ) %>%
          
          #add in title to chart
          
          
          layout(
            #Title
          title =
                   paste0( input$Hospital_Clinic_Type,  ", "  ,
                            input$Activity_Measure , " ",input$Measure,
                           " by ", input$Geography_type),
                 separators = ".",
          
          #y=axis formatting       
           yaxis = list(
                                      
                   exponentformat = "none",
                   
                   separatethousands = TRUE,
                   
                   range = c(0, max(geography_new()$Values, na.rm = TRUE) +
                               (max(geography_new()$Values, na.rm = TRUE)
                                * 10 / 100)),
                   
                   title = paste0(c(
                     rep("&nbsp;", 20),
                     input$Measure,
                     rep("&nbsp;", 20),
                     rep("\n&nbsp;", 3)
                   ),
                   collapse = ""),
                   showline = TRUE,
                   ticks = "outside"
                   
                 ),
               
                 #Set the tick angle to minus 45. It's the only way for the x...
                 #axis tick labels (fin. years) to display without overlapping...
                 #with each other.
                 #Wrap the x axis title in blank spaces so that it doesn't...
                 #overlap with the x axis tick labels.
                 
                 xaxis = list(tickangle = -45,
                              title = paste0(c(rep("&nbsp;", 20),
                                               "<br>",
                                               "Financial year",
                                               rep("&nbsp;", 20),
                                               rep("\n&nbsp;", 3)),
                                           collapse = ""),
                              showline = TRUE,
                              ticks = "outside"),
                 
                 #        #Fix the margins so that the graph and axis titles have enough...
                 #       #room to display nicely.
                 #      #Set the font sizes.
                 #
                 margin = list(l = 90, r = 60, b = 120, t = 90),
                 font = list(size = 13),
                 titlefont = list(size = 15),
                 
                 #insert legend
                 showlegend = TRUE,
                 legend = list(orientation = 'h',
                               x = 0,
                               y = -0.5,
                               bgcolor = 'rgba(255, 255, 255, 0)',
                               bordercolor = 'rgba(255, 255, 255, 0)')) %>%
          
          #Remove unnecessary buttons from the modebar.
          
          config(displayModeBar = TRUE,
                 modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                               'zoomOut2d', 'autoScale2d',
                                               'toggleSpikelines',
                                               'hoverCompareCartesian',
                                               'hoverClosestCartesian'),
                 displaylogo = F, collaborate = F, editable = F)
        
      })
      
      #Create new data table for geography
      
      geography_new_table<-reactive({time_trend %>%
          filter(
            Hospital.Clinical.Type %in% input$Hospital_Clinic_Type
            & Activity %in% input$Activity_Measure
            & Geography %in% input$Geography
            & Substance %in% input$Substances
            & Measure %in% input$Measure
          )%>%
          select(Years, Hospital.Clinical.Type,Activity,Geography.Type,
                 Geography, Substance,Values)
      })
      
      
      #Insert table
      output$geography_table <- renderDataTable({
        datatable(geography_new_table(),
                  colnames = c("Financial Year",
                               "Hospital - Clinical Type",
                               "Activity Measure",
                               "Location Type",
                               "Location",
                               "Substance",
                               input$Measure),
                  rownames = FALSE,
                  style = "Bootstrap"
        )
      })
      
      
      
      
      output$download_geography <- downloadHandler(
        filename = 'time_trend_geography_data.csv',
        content = function(file) {
          write.table(geography_new_table(), 
                      file,
                      #Remove row numbers as the CSV file already has row numbers.
                      
                      row.names = FALSE,
                      col.names = c("Financial year", "Hospital-Clinical Type", 
                                    "Activity Measure" ,"Location Type","Location", 
                                    "Substance", 
                                    input$Measure), 
                      sep = ",")
        }
      )
      
##############################################.
############## Substances tab ----
##############################################.
      
      #We need to include the input for substances types in the server section 
      #rather than the UI section. 
      #This is because the 'location' input is dependent on the 'location
      #type' input. 
      
      
      output$time_trend_location_types2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Geography_type2", 
                                  label = "Select type of location",
                                  choices = location_types, 
                                  selected = "Scotland")
      })
      
      output$time_trend_locations2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Geography2",
                                  label = "Select Location",  
                                  choices = sort(
                                    unique(
                                      as.character(
                                        time_trend$Geography
                                        [time_trend$Geography.Type %in% input$Geography_type2]
                                      )
                                    )
                                  ),
                                  selected = "Scotland"
        )
      }) 
      
      
      output$time_trend_clinical_type2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Hospital_Clinic_Type2", 
                                  label = "Select Hospital-Clinical Type",
                                  choices = clinical_types, 
                                  selected = "Combined (General acute/Psychiatric) - Combined (Mental and Behavioural/Overdose)")
      })
      
      output$time_trend_substance2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Substances2",
                                  label = "Select Drug Type (multiple selection)",  
                                  choices = (if(str_detect(input$Hospital_Clinic_Type2, "Overdose"))
                                  drug_types1
                                  else
                                  drug_types2),
                                  multiple = TRUE, 
                                  selected = "Opioids"
        )
      }) 

      #we can then plot the graph based on the user input.
      #First we create a subset based on user input
    
      substances_new <- reactive({
        time_trend %>%
          filter(
            Hospital.Clinical.Type %in% input$Hospital_Clinic_Type2
            & Activity %in% input$Activity_Measure2
            & Geography %in% input$Geography2
            & Substance %in% input$Substances2
            & Measure %in% input$Measure2
          )
      })
      
      #then we can plot the actual graph, with labels
      output$substances_plot <- renderPlotly({
        #first the tooltip label
                tooltip_substances <- paste0(
                  "Financial year: ",
                  substances_new()$Year,
                  "<br>",
                  "Location: ",
                  substances_new()$Geography,
                  "<br>",
                  "Clinical Type: ",
                  substances_new()$Hospital.Clinical.Type2,
                  "<br>",
                  substances_new()$Measure,": ",
                  substances_new()$Values
                )
        
        #Create the main body of the chart.
        
        plot_ly(
          data = substances_new(),
          #plot- we wont bother at this point with tailored colour
          x = ~  Years,
          y = ~  Values,
          color = ~  Substance,
          #tooltip
                    text = tooltip_substances,
                    hoverinfo = "text",
          #type
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(size = 8),
          width = 1000,
          height = 600
        ) %>%
          
          #add in title to chart
          
          
          layout(
            #Title
            title =
              paste0( input$Hospital_Clinic_Type2,  ", "  ,
                      input$Activity_Measure2 , " ",input$Measure2,
                      " in ", input$Geography2),
            separators = ".",
            
            #y=axis formatting       
            yaxis = list(
              
              exponentformat = "none",
              
              separatethousands = TRUE,
              
              range = c(0, max(substances_new()$Values, na.rm = TRUE) +
                          (max(substances_new()$Values, na.rm = TRUE)
                           * 10 / 100)),
              
              title = paste0(c(
                rep("&nbsp;", 20),
                input$Measure2,
                rep("&nbsp;", 20),
                rep("\n&nbsp;", 3)
              ),
              collapse = ""),
              showline = TRUE,
              ticks = "outside"
              
            ),
            
            #Set the tick angle to minus 45. It's the only way for the x...
            #axis tick labels (fin. years) to display without overlapping...
            #with each other.
            #Wrap the x axis title in blank spaces so that it doesn't...
            #overlap with the x axis tick labels.
            
            xaxis = list(tickangle = -45,
                         title = paste0(c(rep("&nbsp;", 20),
                                          "<br>",
                                          "Financial year",
                                          rep("&nbsp;", 20),
                                          rep("\n&nbsp;", 3)),
                                        collapse = ""),
                         showline = TRUE,
                         ticks = "outside"),
            
            #        #Fix the margins so that the graph and axis titles have enough...
            #       #room to display nicely.
            #      #Set the font sizes.
            #
            margin = list(l = 90, r = 60, b = 120, t = 90),
            font = list(size = 13),
            titlefont = list(size = 15),
            
            #insert legend
            showlegend = TRUE,
            legend = list(orientation = 'h',
                          x = 0,
                          y = -0.5,
                          bgcolor = 'rgba(255, 255, 255, 0)',
                          bordercolor = 'rgba(255, 255, 255, 0)')) %>%
          
          #Remove unnecessary buttons from the modebar.
          
          config(displayModeBar = TRUE,
                 modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                               'zoomOut2d', 'autoScale2d',
                                               'toggleSpikelines',
                                               'hoverCompareCartesian',
                                               'hoverClosestCartesian'),
                 displaylogo = F, collaborate = F, editable = F)
        
      })
      
      substances_new_table<-reactive({time_trend %>%
          filter(
            Hospital.Clinical.Type %in% input$Hospital_Clinic_Type2
            & Activity %in% input$Activity_Measure2
            & Geography %in% input$Geography2
            & Substance %in% input$Substances2
            & Measure %in% input$Measure2
          )%>%
          select(Years, Hospital.Clinical.Type,Activity,Geography,
                 Substance,Values)
      })
      
      #Insert table
      output$substances_table <- renderDataTable({
        datatable(substances_new_table(),
                  colnames = c("Financial Year",
                               "Hospital - Clinical Type",
                               "Activity Measure",
                               "Location",
                               "Substance",
                               input$Measure2),
                  rownames = FALSE,
                  style = "Bootstrap")
      })
        
        output$download_substances <- downloadHandler(
          filename = 'time_trend_Substance_data.csv',
          content = function(file) {
            write.table(substances_new_table(), 
                        file,
                        #Remove row numbers as the CSV file already has row numbers.
                        
                        row.names = FALSE,
                        col.names = c("Financial year", "Hospital-Clinical Type", 
                                      "Activity Measure" ,"Location", 
                                      "Substance", 
                                      input$Measure2), 
                        sep = ",")
          }
        )

##############################################.
############## Age/Sex tab ----
##############################################.

        
        output$age_sex_clinical_type <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Hospital_Clinic_Type3",
            label = "Select Hospital-Clinical Type",
            choices = clinical_types2,
            selected = "Combined- Combined "
          )
        })
        
        output$age_sex_substance <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Substances3",
            label = "Select Substance Category",
            choices = (if (str_detect(input$Hospital_Clinic_Type3, "Overdoses"))
              drug_types1_1
              else
                drug_types2_2),
            selected = "Opiods"
          )
        })
        
        #So here we need to create two graphs
        #1) Time-Trend - a line chart showing the change over time by chosen age/sex factors.
        #2) Year only- a tornado chart that shows all age/sex by a year
        
        
        #1) Time trend
        
        #Filter it by options for time trend
        age_sex_time_new <- reactive({
          age_sex %>%
            filter(
              Hospital.Clinical.Type %in% input$Hospital_Clinic_Type3
              & Activity %in% input$Activity_Measure3
              & Substances %in% input$Substances3
              & Measure %in% input$Measure3
              #and the age/sex options
              & Age %in% input$Age
              & Sex %in% input$Sex
            )
        })

        
        #Create the main body of the chart.
        output$age_sex_time_plot <- renderPlotly({
          #Add in a tooltip
          tooltip_age_sex_time <- paste0(
            "Financial year: ",
            age_sex_time_new()$Years,
            "<br>",
            "Hospital - Clinical: ",
            age_sex_time_new()$Hospital.Clinical.Type,
            "<br>",
            "Substance: ",
            age_sex_time_new()$Substances,
            "<br>",
            input$Measure3,
            ": ",
            abs(age_sex_time_new()$Values)
          )
          
          plot_ly(
            data = age_sex_time_new(),
            #plot- we wont bother at this point with tailored colour
            x = ~  Years,
            y = ~  Values,
            color = ~  Age,
            #so we can try different symbols as well as different linetypes to
            #distinguish between sex.
            linetype = ~ Sex,
            symbol = ~ Sex,
            #tooltip
            text = tooltip_age_sex_time,
            hoverinfo = "text",
            #type
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(size = 8),
            width = 1000,
            height = 600
          ) %>%
            
            #Make the graph title reactive.
            
            layout(title = 
                     paste0(input$Hospital_Clinic_Type3,  ", "  ,
                            input$Activity_Measure3 , " ",input$Measure3,
                            " by ", input$Substances3),
                   
                   separators = ".",
                   
                   #We need to fix the range of the y axis, as R refuses to set...
                   #the lower end of this axis to zero.
                   #The following "range" command fixes the lower end to...
                   #zero, and calculates the upper end as the maximum...
                   #number visualised in the graph + 10% of this number.
                   #Also, wrap the y axis title in blank spaces so it doesn't...
                   #overlap with the y axis tick labels.
                   #Finally, make the y axis title reactive.
                   
                   yaxis = list(
                     
                     exponentformat = "none",
                     
                     separatethousands = TRUE,
                     
                     range = c(0, max(age_sex_time_new()$Values, na.rm = TRUE) + 
                                 (max(age_sex_time_new()$Values, na.rm = TRUE) 
                                  * 10 / 100)), 
                     
                     title = input$Measure3,
                     showline = TRUE, 
                     ticks = "outside"
                     
                   ),
                   
                   #Set the tick angle to minus 45. It's the only way for the x...
                   #axis tick labels (fin. years) to display without overlapping...
                   #with each other.
                   #Wrap the x axis title in blank spaces so that it doesn't...
                   #overlap with the x axis tick labels.
                   
                   xaxis = list(tickangle = -45, 
                                title = paste0(c(rep("&nbsp;", 20),
                                                 "<br>", 
                                                 "Financial year",
                                                 rep("&nbsp;", 20),
                                                 rep("\n&nbsp;", 3)),
                                               collapse = ""),
                                showline = TRUE, 
                                ticks = "outside"),
                   
                   #Fix the margins so that the graph and axis titles have enough...
                   #room to display nicely.
                   #Set the font sizes.
                   
                   margin = list(l = 90, r = 60, b = 120, t = 90),
                   font = list(size = 13),
                   titlefont = list(size = 15),
                   
                   #Insert a legend so that the user knows which colour...
                   #corresponds to which location of treatment.
                   #Make the legend background and legend border white.              
                   
                   showlegend = TRUE,
                   legend = list(orientation = 'h',
                                 x = 0, 
                                 y = -0.5,
                                 bgcolor = 'rgba(255, 255, 255, 0)', 
                                 bordercolor = 'rgba(255, 255, 255, 0)')) %>%
            
            #Remove unnecessary buttons from the modebar.
            
            config(displayModeBar = TRUE,
                   modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                                 'zoomOut2d', 'autoScale2d', 
                                                 'toggleSpikelines', 
                                                 'hoverCompareCartesian', 
                                                 'hoverClosestCartesian'), 
                   displaylogo = F, collaborate = F, editable = F)
          
        })
        
        
        
        
        #Hmmm- this does not look great.
        #Let's try to see if it is better with subplot
        
        output$age_sex_time_plot2 <- renderPlotly({
          subplot(
            plot_ly(
              data = age_sex_time_new_male(),
              #plot- we wont bother at this point with tailored colour
              x = ~  Years,
              y = ~  Values,
              color = ~  Age,
              #tooltip
              #text = tooltip_substances,
              #hoverinfo = "text",
              #type
              type = 'scatter',
              mode = 'lines+markers',
              marker = list(size = 8),
              width = 1000,
              height = 600
            ),
            plot_ly(
              data = age_sex_time_new_female(),
              #plot- we wont bother at this point with tailored colour
              x = ~  Years,
              y = ~  Values,
              color = ~  Age,
              #tooltip
              #text = tooltip_substances,
              #hoverinfo = "text",
              #type
              type = 'scatter',
              mode = 'lines+markers',
              marker = list(size = 8),
              width = 1000,
              height = 600
            )
          )
        })
        
        
        #we can now add in the table for the bar chart- values
        #have to be reassigned to positive values
        age_sex_trend_table <- reactive({
          age_sex %>%
            filter(
              Hospital.Clinical.Type %in% input$Hospital_Clinic_Type3
              & Activity %in% input$Activity_Measure3
              & Substances %in% input$Substances3
              & Measure %in% input$Measure3
              & Age %in% input$Age
              & Sex %in% input$Sex
            ) %>%
            select(-Measure)
          
        })
        
        #Table
        output$age_sex_trend_table <- renderDataTable({
          datatable(
            age_sex_trend_table(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial Year",
              "Hospital - Clinical Type",
              "Activity Measure",
              "Substance",
              "Age",
              "Sex",
              input$Measure3
            )
          )
        })
        
        #Download button
        
        output$download_age_sex_trend <- downloadHandler(
          filename = 'age_sex_trend_data.csv',
          content = function(file) {
            write.table(
              age_sex_trend_table(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial Year",
                "Hospital - Clinical Type",
                "Activity Measure",
                "Substance",
                "Age",
                "Sex",
                input$Measure3
              ),
              sep = ","
            )
          }
        )
        
        
        #Still not hugely keen on the subplots either- a bit small.
        #For now we will move onto the tornado charts (with a slider)
        #called age_sex_year_plot
        
        #first create the data for it
        
        age_sex_year_new <- reactive({
          age_sex_tornado %>%
            filter(
              Years %in% input$Financial_Year
              & Hospital.Clinical.Type %in% input$Hospital_Clinic_Type3
              & Activity %in% input$Activity_Measure3
              & Substances %in% input$Substances3
              & Measure %in% input$Measure3
            ) %>%
            droplevels()
        })
        
        
        #then plot it
        output$age_sex_year_plot <- renderPlotly({
          #add in tooltip
          tooltip_age_sex_year <- paste0(
            "Financial year: ",
            age_sex_year_new()$Years,
            "<br>",
            "Hospital - Clinical: ",
            age_sex_year_new()$Hospital.Clinical.Type,
            "<br>",
            "Substance: ",
            age_sex_year_new()$Substances,
            "<br>",
            input$Measure3,
            ": ",
            abs(age_sex_year_new()$Values)
          )
          
          plot_ly(
            data = age_sex_year_new(),
            
            x = ~ Values,
            y = ~ Age,
            color = ~ Sex,
            
            #Colour palette:
            #Dark blue for males and light blue for females.
            
            colors = c("#0072B2", "#ADD8E6"),
            
            text = tooltip_age_sex_year,
            hoverinfo = "text",
            
            #Select the type of chart you want, in this case a bar chart,...
            #and set the orientation to horizontal to achieve the...
            #"pyramid" look.
            
            type = 'bar',
            width = 1000,
            height = 400
          ) %>%
            
            layout(
              title = paste0(
                "<b>",
                input$Activity_Measure3,
                " in financial year ",
                input$Financial_Year,
                ",",
                "<br>",
                "by age group and sex, by ",
                input$Substances3,
                "</b>"
              ),
              
              bargap = 0.2,
              barmode = "overlay",
              yaxis = list(
                title = paste0(c(
                  rep("&nbsp;", 20),
                  "Age group",
                  rep("&nbsp;", 20),
                  rep("\n&nbsp;", 3)
                ),
                collapse = ""),
                showline = TRUE,
                ticks = "outside"
              ),
              xaxis = list(
                exponentformat = "none",
                separatethousands = TRUE,
                tickmode = 'array',
                range = c(-round(max(
                  abs(age_sex_year_new()$Values)
                )
                * 110 / 100),
                round(max(
                  abs(age_sex_year_new()$Values)
                )
                * 110 / 100)),
                tickangle = 0,
                tickvals = c(
                  -round(max(abs(
                    age_sex_year_new()$Values
                  ))),-round(max(abs(
                    age_sex_year_new()$Values
                  ))
                  * 66 / 100),-round(max(abs(
                    age_sex_year_new()$Values
                  ))
                  * 33 / 100),
                  0,
                  round(max(abs(
                    age_sex_year_new()$Values
                  ))
                  * 33 / 100),
                  round(max(abs(
                    age_sex_year_new()$Values
                  ))
                  * 66 / 100),
                  round(max(abs(
                    age_sex_year_new()$Values
                  )))
                ),
                ticktext = paste0(as.character(
                  c(
                    round(max(abs(
                      age_sex_year_new()$Values
                    ))),
                    round(max(abs(
                      age_sex_year_new()$Values
                    ))
                    * 66 / 100),
                    round(max(abs(
                      age_sex_year_new()$Values
                    ))
                    * 33 / 100),
                    0,
                    round(max(abs(
                      age_sex_year_new()$Values
                    ))
                    * 33 / 100),
                    round(max(abs(
                      age_sex_year_new()$Values
                    ))
                    * 66 / 100),
                    round(max(abs(
                      age_sex_year_new()$Values
                    )))
                  )
                )),
                
                #Make the x axis title reactive.
                
                title =  input$Measure3,
                
                showline = TRUE,
                ticks = "outside"
                
              ),
              
              #Fix the margins so that the graph and axis titles have...
              #enough room to display nicely.
              #Set the font sizes.
              
              margin = list(
                l = 140,
                r = 10,
                b = 70,
                t = 90
              ),
              font = list(size = 13),
              titlefont = list(size = 15),
              
              #Insert a legend so that the user knows which colour...
              #corresponds to which sex.
              #Make the legend background and legend border white.
              
              showlegend = TRUE,
              legend = list(
                x = 1,
                y = 1,
                bgcolor = 'rgba(255, 255, 255, 0)',
                bordercolor = 'rgba(255, 255, 255, 0)'
              )
            ) %>%
            
            #Remove unnecessary buttons from the modebar.
            
            config(
              displayModeBar = TRUE,
              modeBarButtonsToRemove = list(
                'select2d',
                'lasso2d',
                'zoomIn2d',
                'zoomOut2d',
                'autoScale2d',
                'toggleSpikelines',
                'hoverCompareCartesian',
                'hoverClosestCartesian'
              ),
              displaylogo = F,
              collaborate = F,
              editable = F
            )
          
        })
        
        #we can now add in the table for the bar chart- values
        #have to be reassigned to positive values
        age_sex_year_table <- reactive({
          age_sex_tornado %>%
            filter(
              Years %in% input$Financial_Year
              & Hospital.Clinical.Type %in% input$Hospital_Clinic_Type3
              & Activity %in% input$Activity_Measure3
              & Substances %in% input$Substances3
              & Measure %in% input$Measure3
            ) %>%
            select(-Measure) %>%
            mutate(Values = abs(Values))
          
        })
        
        #Table
        output$age_sex_year_table <- renderDataTable({
          datatable(
            age_sex_year_table(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial Year",
              "Hospital - Clinical Type",
              "Activity Measure",
              "Substance",
              "Age",
              "Sex",
              input$Measure3
            )
          )
        })
        
        #Download button
        
        output$download_age_sex_year <- downloadHandler(
          filename = 'age_sex_year_data.csv',
          content = function(file) {
            write.table(
              age_sex_year_table(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial Year",
                "Hospital - Clinical Type",
                "Activity Measure",
                "Substance",
                "Age",
                "Sex",
                input$Measure3
              ),
              sep = ","
            )
          }
        )
        
      
      
##############################################.
############## Deprivation tab ----
##############################################.
      
      
      
##############################################.
############## Table tab ----
##############################################.
      
      
      #End of server
    }
  
  #End of script
}

shinyApp(ui = ui, server = server)
