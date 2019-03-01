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


#So initially we need to read in the data. Currently saved as an rds file.
All_Data<- readRDS("\\\\nssstats01\\SubstanceMisuse1\\Topics\\DrugRelatedHospitalStats\\Publications\\DRHS\\20181218\\Temp\\Dashboard_Data_Temp\\Data_explorer_data.rds")

#NOTE- the following is temporary and is conditional on 
#1) Where we decided to eventually store the final data set 
#2) The final format of the data set
time_trend <- All_Data %>% 
  filter(ï..output == 1.01) %>% 
  select(-c(ï..output,age_group,sex,simd))
  
age_sex <- All_Data %>% 
  filter(ï..output != 1.04, 
          geography =="Scotland") %>% 
  select(-c(ï..output,simd))

deprivation <- All_Data %>% 
  filter(ï..output != 1.05, 
          geography =="Scotland", 
          simd != "All") %>% 
  select(-c(ï..output,age_group,sex))


#We then create the options for users to choose from in the drop down menus. 
#Drug Types are created as list to allow different options dependent on the 
#Hospital admission types
clinical_types <- as.character(unique(All_Data$hos_clin_type))
activity_measure <- as.character(unique(All_Data$activity_type))
location_types <- as.character(unique(All_Data$geography_type))
locations<- as.character(unique(All_Data$geography))
Scotland<-locations[1:3]
Health_Board<-locations[4:17]
ADP<- locations[18:48]

geography_list<-list("Scotland" = locations[1:3],
                     "NHS Board" = locations[4:17],
                     "ADP" = locations[18:48])
geography_list["ADP"]

drug_types<- as.character(unique(All_Data$drug_type))
drug_types1<- list("Main Categories" = as.character(unique(All_Data$drug_type)[1:7]),
                  "Opioids Sub Categories" = as.character(unique(All_Data$drug_type)[8:10]))
drug_types2<- as.character(unique(All_Data$drug_type)[1:7])
measures<- as.character(unique(All_Data$measure))


#Add in age, sex, SIMD and financial years options for demographic tabs
age <- as.character(unique(All_Data$age_group))
sex <- as.character(unique(All_Data$sex))
financial_years <- as.character(unique(All_Data$year))
SIMD<- as.character(unique(All_Data$simd))


#we need to look at altering the data for the tornado chart so that male values
#negative to allow it to work 
#Convert males to negative (and remove all)
age_sex_male <- age_sex %>%
  filter(sex == "Male"
         & age_group != "All") %>%
  mutate(value = value * -1)
#Then remove females
age_sex_female <- age_sex %>%
  filter(sex == "Female"
         & age_group != "All")

#recombine them into one chart
age_sex_tornado <- rbind(age_sex_male, age_sex_female)


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
    "Introduction",
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
            "link_to_geography", "Time trend (location comparison)"
          )),
          icon("line-chart"),
          " - shows data on specific DRHS activty over time, by comparing by location."
      ),
      
        tags$li(
          tags$b(actionLink(
            "link_to_substances", "Time trend (drug type comparison)"
          )),
          icon("line-chart"),
          " - shows data on specific DRHS activty over time, by comparing by substances."
        ),
      tags$li(
        tags$b(actionLink(
          "link_to_age_sex", "Age/sex"
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
          tags$a(href = "mailto:isdsubstancemisuse@nhs.net",
          "isdsubstancemisuse@nhs.net.")
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
    "Time trend (location comparison)",
    icon = icon("line-chart"),
    style = "height: 95%; width: 95%; background-color: #FFFFFF;
    border: 0px solid #FFFFFF;",
    h3("Time Trend"),
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
        uiOutput("time_trend_clinical_type"), 
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
        4,
        shinyWidgets::pickerInput(
          inputId = "Activity_Measure",
          label = "Select activity type",
          choices = activity_measure,
          selected = "Stays"
        ),
        uiOutput("time_trend_locations")
        ),
        
        column(
          4,
          
          uiOutput("time_trend_substance1"),
          shinyWidgets::pickerInput(
            inputId = "Measure",
            label = "Select measure",
            choices = measures,
            selected = "Rates"
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
    "Time trend (drug type comparison)",
    icon = icon("line-chart"),
    style = "height: 95%; width: 95%; background-color: #FFFFFF;
    border: 0px solid #FFFFFF;",
    h3("Time trend"),
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
      # 4 - Geography 
      # 5 - Substance (Multiple)
      # 6 - Measure
      
      column(
        4,
        
        uiOutput("time_trend_clinical_type2"), 
        uiOutput("time_trend_location_types2"),
       
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
        4,
        shinyWidgets::pickerInput(
          inputId = "Activity_Measure2",
          label = "Select activity type",
          choices = activity_measure,
          selected = "Stays"
        ), 
        uiOutput("time_trend_locations2")
      ),
      

     
      
      column(
        4,
        uiOutput("time_trend_substance2"),

        shinyWidgets::pickerInput(
          inputId = "Measure2",
          label = "Select measure",
          choices = measures,
          selected = "Rates"
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
  "Age/sex",
  icon = icon("child"),
  style = "height: 95%; width: 95%; background-color: #FFFFFF;
  border: 0px solid #FFFFFF;",
  h3("Age/sex"),
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
           uiOutput("age_sex_clinical_type"), 
           uiOutput("age_sex_substance")),
    
    column(
      6,
      shinyWidgets::pickerInput(
        inputId = "Activity_Measure3",
        label = "Select activity type",
        choices = activity_measure,
        selected = "Stays"
      ),
      shinyWidgets::pickerInput(
        inputId = "Measure3",
        label = "Select measure",
        choices = measures,
        selected = "Rates"
      )
    )
  ),
  p(
    br(),
    HTML(
      "Please choose between the following options -"), 
      tags$ul(
        tags$li("Time trend to see data over time for particular
                age and sex catetegories"),
        tags$li("Bar chart for full breakdown by individual year")
      )
    ),
  
  #In the main panel of the tab, insert the time trend plot
  mainPanel(width = 12, 
            tabsetPanel(
    type = "pills",
    tabPanel(
      "Time Trend",
      icon = icon("line-chart"),
      style = "height: 95%; width: 95%; background-color: #FFFFFF;
      border: 0px solid #FFFFFF;",
      br(),
      br(),
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Age",
          label = "Select age",
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
        4,
        shinyWidgets::pickerInput(
          inputId = "Sex",
          label = "Select sex",
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
        8,
        shinyWidgets::sliderTextInput(
          inputId = "Financial_Year",
          label = "Select financial year",
          choices = financial_years,
          selected = "2017/18",
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
        dataTableOutput("age_sex_year_table"),
        HTML("</div>"),
        br(),
        br()
      )
    )
    )
    )
  
  #End of tab panel
    ),

##############################################.
############## Deprivation tab ----
##############################################.

tabPanel(
  "Deprivation",
  icon = icon("bar-chart"),
  style = "height: 95%; width: 95%; background-color: #FFFFFF;
  border: 0px solid #FFFFFF;",
  h3("Deprivation"),
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
    #We have Five filters at this point
    # 1 - Hospital/Clinical type
    # 2 - Activity Measure
    # 3 - Substance (dependent on Hospital/Clinic Type)
    # 4 - Financial Year
    # 5 - Measure
    
    column(4,
           uiOutput("SIMD_clinical_type"), 
           
           shinyWidgets::pickerInput(
             inputId = "Financial_Year2",
             label = "Select financial year",
             choices =  rev(financial_years),
             selected = "2017/18"), 
           downloadButton(
             outputId = "download_SIMD",
             label = "Download data",
             class = "mySIMDtrendbutton"
           ),
           tags$head(
             tags$style(
               ".mySIMDtrendbutton { background-color:
               #0072B2; }
               .mySIMDtrendbutton { color: #FFFFFF; }"
             )
           )
    ),
    
    column(
      4,
      shinyWidgets::pickerInput(
        inputId = "Activity_Measure4",
        label = "Select activity type",
        choices = activity_measure,
        selected = "Stays"
      ), 
      shinyWidgets::pickerInput(
        inputId = "Measure4",
        label = "Select measure",
        choices = measures,
        selected = "Rates"
      )
    ),
    
    column(4,
           uiOutput("SIMD_substance"))
  ),
  
  
  #In the main panel of the tab, insert the SIMD plot
  
  mainPanel(
    width =12,
    plotlyOutput("SIMD_plot",
                 width = "1090px",
                 height = "600px"),
    
    
    HTML(
      "<button data-toggle = 'collapse' href = '#SIMDchart'
      class = 'btn btn-primary' id = 'SIMD_link'>
      <strong>Show/hide table</strong></button>"
    ),
    HTML("<div id = 'SIMDchart' class = 'collapse'>"),
    br(),
    dataTableOutput("SIMD_table"),
    HTML("</div>"),
    br(),
    br()
    
    
    )
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
          updateTabsetPanel(session, "Panels", 
                            selected = "Time trend (location comparison)")
        })
      observeEvent(
        input$link_to_substances,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Time trend (drug type comparison)")
        })
      observeEvent(
        input$link_to_age_sex,
        {
          updateTabsetPanel(session, "Panels",
                            selected = "Age/sex")
        })
      observeEvent(
        input$link_to_deprivation,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Deprivation")
        })
      observeEvent(
        input$link_to_table,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Table")
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
                                  label = "Select location type (multiple selection)",
                                  choices = location_types, 
                                  selected = "Scotland", 
                                  multiple = TRUE)
      })

      output$time_trend_locations <- renderUI({
        shinyWidgets::pickerInput(inputId = "Geography",
                                  label = "Select location (multiple selection)",  
                                  choices = geography_list[input$Geography_type],
                                  multiple = TRUE,
                                  selected = geography_list[input$Geography_type][[1]][1]
        )
      }) 
      
      output$time_trend_clinical_type <- renderUI({
        shinyWidgets::pickerInput(inputId = "Hospital_Clinic_Type", 
                                  label = "Select hospital clinical type",
                                  choices = clinical_types, 
                                  selected = "Combined (General acute/Psychiatric) - Combined (Mental and Behavioural/Overdose)")
      })

      output$time_trend_substance1 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Substances",
                                  label = "Select drug type",  
                                  choices = (if(str_detect(input$Hospital_Clinic_Type, " Overdose"))
                                    drug_types1
                                    else
                                      drug_types2), 
                                  selected = "All"
        )
      }) 
      
         
      #we can then plot the graph based on the user input.
      #First we create a subset based on user input
      
      geography_new <- reactive({
        time_trend %>%
          filter(
            hos_clin_type %in% input$Hospital_Clinic_Type
            & activity_type %in% input$Activity_Measure
            & geography %in% input$Geography
            & drug_type %in% input$Substances
            & measure %in% input$Measure 
          )
      })
      
      #then we can plot the actual graph, with labels
      output$geography_plot <- renderPlotly({
        #first the tooltip label
        tooltip_geography <- paste0(
          "Financial year: ",
          geography_new()$year,
          "<br>",
          "Location: ",
          geography_new()$geography,
          "<br>",
          "Drug type: ",
          geography_new()$drug_type,
          "<br>",
          geography_new()$measure,": ",
          geography_new()$value
        )
        
        #Create the main body of the chart.
        
        plot_ly(
          data = geography_new(),
          #plot- we wont bother at this point with tailored colour
          x = ~  year,
          y = ~  value,
          color = ~  geography,
            
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
                   
                   range = c(0, max(geography_new()$value, na.rm = TRUE) +
                               (max(geography_new()$value, na.rm = TRUE)
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
            hos_clin_type %in% input$Hospital_Clinic_Type
            & activity_type %in% input$Activity_Measure
            & geography %in% input$Geography
            & drug_type %in% input$Substances
            & measure %in% input$Measure
          )%>%
          select(year, hos_clin_type,activity_type,geography_type,
                 geography, drug_type,value)
      })
      
      
      #Insert table
      output$geography_table <- renderDataTable({
        datatable(geography_new_table(),
                  colnames = c("Financial year",
                               "Hospital clinical type",
                               "Activity type",
                               "Location type",
                               "Location",
                               "Drug type",
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
                      col.names = c("Financial year", "Hospital clinical type", 
                                    "Activity type" ,"Location type","Location", 
                                    "Drug type", 
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
                                  label = "Select location type",
                                  choices = location_types, 
                                  selected = "Scotland")
      })
      
      output$time_trend_locations2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Geography2",
                                  label = "Select location",  
                                  choices = 
                                    unique(
                                      as.character(
                                        time_trend$geography
                                        [time_trend$geography_type %in% input$Geography_type2]
                                      )
                                    ),
                                  selected = "Scotland"
        )
      }) 
      
      
      output$time_trend_clinical_type2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Hospital_Clinic_Type2", 
                                  label = "Select hospital clinical type",
                                  choices = clinical_types, 
                                  selected = "Combined (General acute/Psychiatric) - Combined (Mental and Behavioural/Overdose)")
      })
      
      output$time_trend_substance2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Substances2",
                                  label = "Select drug type (multiple selection)",  
                                  choices = (if(str_detect(input$Hospital_Clinic_Type2, " Overdose"))
                                  drug_types1
                                  else
                                  drug_types2),
                                  multiple = TRUE, 
                                  selected = "All"
        )
      }) 

      #we can then plot the graph based on the user input.
      #First we create a subset based on user input
    
      substances_new <- reactive({
        time_trend %>%
          filter(
            hos_clin_type %in% input$Hospital_Clinic_Type2
            & activity_type %in% input$Activity_Measure2
            & geography %in% input$Geography2
            & drug_type %in% input$Substances2
            & measure %in% input$Measure2
          )
      })
      
      #then we can plot the actual graph, with labels
      output$substances_plot <- renderPlotly({
        #first the tooltip label
                tooltip_substances <- paste0(
                  "Financial year: ",
                  substances_new()$year,
                  "<br>",
                  "Location: ",
                  substances_new()$geography,
                  "<br>",
                  "Drug type: ",
                  substances_new()$drug_type,
                  "<br>",
                  substances_new()$measure,": ",
                  substances_new()$value
                )
        
        #Create the main body of the chart.
        
        plot_ly(
          data = substances_new(),
          #plot- we wont bother at this point with tailored colour
          x = ~  year,
          y = ~  value,
          color = ~  drug_type,
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
              
              range = c(0, max(substances_new()$value, na.rm = TRUE) +
                          (max(substances_new()$value, na.rm = TRUE)
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
            hos_clin_type %in% input$Hospital_Clinic_Type2
            & activity_type %in% input$Activity_Measure2
            & geography %in% input$Geography2
            & drug_type %in% input$Substances2
            & measure %in% input$Measure2
          )%>%
          select(year, hos_clin_type,activity_type,geography,
                 drug_type,value)
      })
      
      #Insert table
      output$substances_table <- renderDataTable({
        datatable(substances_new_table(),
                  colnames = c("Financial year",
                               "Hospital clinical type",
                               "Activity type",
                               "Location",
                               "Drug type",
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
                        col.names = c("Financial year", "Hospital clinical type", 
                                      "Activity type" ,"Location", 
                                      "Drug type", 
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
            label = "Select hospital clinical type",
            choices = clinical_types,
            selected = "Combined (General acute/Psychiatric) - Combined (Mental and Behavioural/Overdose)"
          )
        })
        
        output$age_sex_substance <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Substances3",
            label = "Select drug type",
            choices = (if (str_detect(input$Hospital_Clinic_Type3, " Overdose"))
              drug_types1
              else
                drug_types2),
            selected = "All"
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
              hos_clin_type %in% input$Hospital_Clinic_Type3
              & activity_type %in% input$Activity_Measure3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
              #and the age/sex options
              & age_group %in% input$Age
              & sex %in% input$Sex
            )
        })

        
        #Create the main body of the chart.
        output$age_sex_time_plot <- renderPlotly({
          #Add in a tooltip
          tooltip_age_sex_time <- paste0(
            "Financial year: ",
            age_sex_time_new()$year,
            "<br>",
            "Age: ",
            age_sex_time_new()$age_group,
            "<br>",
            "Sex: ",
            age_sex_time_new()$sex,
            "<br>",
            input$Measure3,
            ": ",
            abs(age_sex_time_new()$value)
          )
          
          plot_ly(
            data = age_sex_time_new(),
            #plot- we wont bother at this point with tailored colour
            x = ~  year,
            y = ~  value,
            color = ~  age_group,
            #so we can try different symbols as well as different linetypes to
            #distinguish between sex.
            linetype = ~ sex,
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
                     
                     range = c(0, max(age_sex_time_new()$value, na.rm = TRUE) + 
                                 (max(age_sex_time_new()$value, na.rm = TRUE) 
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
        
   
        #we can now add in the table for the time trend
        age_sex_trend_table <- reactive({
          age_sex %>%
            filter(
              hos_clin_type %in% input$Hospital_Clinic_Type3
              & activity_type %in% input$Activity_Measure3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
              & age_group %in% input$Age
              & sex %in% input$Sex
            ) %>%
            select(-c(measure,geography,geography_type))
        })
        
        #Table
        output$age_sex_trend_table <- renderDataTable({
          datatable(
            age_sex_trend_table(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Hospital clinical type",
              "Activity type",
              "Drug type",
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
                "Financial year",
                "Hospital clinical type",
                "Activity type",
                "Drug type",
                "Age",
                "Sex",
                input$Measure3
              ),
              sep = ","
            )
          }
        )
        
        
        ###
        ###Tornado Chart
        ###
        
        age_sex_year_new <- reactive({
          age_sex_tornado %>%
            filter(
              year %in% input$Financial_Year
              & hos_clin_type %in% input$Hospital_Clinic_Type3
              & activity_type %in% input$Activity_Measure3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
            ) %>%
            droplevels()
        })
        
        
        #then plot it
        output$age_sex_year_plot <- renderPlotly({
          #add in tooltip
          tooltip_age_sex_year <- paste0(
            "Financial year: ",
            age_sex_year_new()$year,
            "<br>",
            "Age: ",
            age_sex_year_new()$age_group,
            "<br>",
            "Sex: ",
            age_sex_year_new()$sex,
            "<br>",
            input$Measure3,
            ": ",
            abs(age_sex_year_new()$value)
          )
          
          plot_ly(
            data = age_sex_year_new(),
            
            x = ~ value,
            y = ~ age_group,
            color = ~ sex,
            
            #Colour palette:
            #Dark blue for females and light blue for males.
            
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
                  abs(age_sex_year_new()$value)
                )
                * 110 / 100),
                round(max(
                  abs(age_sex_year_new()$value)
                )
                * 110 / 100)),
                tickangle = 0,
                tickvals = c(
                  -round(max(abs(
                    age_sex_year_new()$value
                  ))),-round(max(abs(
                    age_sex_year_new()$value
                  ))
                  * 66 / 100),-round(max(abs(
                    age_sex_year_new()$value
                  ))
                  * 33 / 100),
                  0,
                  round(max(abs(
                    age_sex_year_new()$value
                  ))
                  * 33 / 100),
                  round(max(abs(
                    age_sex_year_new()$value
                  ))
                  * 66 / 100),
                  round(max(abs(
                    age_sex_year_new()$value
                  )))
                ),
                ticktext = paste0(as.character(
                  c(
                    round(max(abs(
                      age_sex_year_new()$value
                    ))),
                    round(max(abs(
                      age_sex_year_new()$value
                    ))
                    * 66 / 100),
                    round(max(abs(
                      age_sex_year_new()$value
                    ))
                    * 33 / 100),
                    0,
                    round(max(abs(
                      age_sex_year_new()$value
                    ))
                    * 33 / 100),
                    round(max(abs(
                      age_sex_year_new()$value
                    ))
                    * 66 / 100),
                    round(max(abs(
                      age_sex_year_new()$value
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
              year %in% input$Financial_Year
              & hos_clin_type %in% input$Hospital_Clinic_Type3
              & activity_type %in% input$Activity_Measure3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
            ) %>%
            select(-c(measure,geography,geography_type)) %>%
            mutate(value = abs(value))
          
        })
        
        #Table
        output$age_sex_year_table <- renderDataTable({
          datatable(
            age_sex_year_table(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Hospital clinical type",
              "Activity type",
              "Drug type",
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
                "Financial year",
                "Hospital clinical type",
                "Activity type",
                "Drug type",
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
      
        
        output$SIMD_clinical_type <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Hospital_Clinic_Type4",
            label = "Select hospital clinical type",
            choices = clinical_types,
            selected = "Combined (General acute/Psychiatric) - Combined (Mental and Behavioural/Overdose)"
          )
        })
        
        output$SIMD_substance <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Substances4",
            label = "Select drug type",
            choices = (if (str_detect(input$Hospital_Clinic_Type4, " Overdose"))
              drug_types1
              else
                drug_types2),
            selected = "All"
          )
        })
        
        
        #Filter it by options for time trend
        SIMD_new <- reactive({
          deprivation %>%
            filter(
              hos_clin_type %in% input$Hospital_Clinic_Type4
              & activity_type %in% input$Activity_Measure4
              & drug_type %in% input$Substances4
              & measure %in% input$Measure4
              #and the year options
              & year %in% input$Financial_Year2
            )
        })
        
        
        
        #Create the main body of the chart.
        output$SIMD_plot <- renderPlotly({
          #Add in a tooltip
          tooltip_SIMD <- paste0(
            "Financial year: ",
            SIMD_new()$year,
            "<br>",
            "Deprivation index: ",
            SIMD_new()$simd,
            "<br>",
            "Substance: ",
            SIMD_new()$drug_type,
            "<br>",
            input$Measure4,
            ": ",
            SIMD_new()$value
          )
          
          plot_ly(
            data = SIMD_new(),
            #plot- we wont bother at this point with tailored colour
            x = ~  simd,
            y = ~  value,
            #tooltip
            text = tooltip_SIMD,
            hoverinfo = "text",
            #type
            type = 'bar',
            width = 1000,
            height = 600
          ) %>%
            
            #Make the graph title reactive.
            
            layout(title = 
                     paste0(input$Hospital_Clinic_Type4,  ", "  ,
                            input$Activity_Measure4 , " ",input$Measure4,
                            " by ", input$Substances4),
                   
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
                     
                     range = c(0, max(SIMD_new()$value, na.rm = TRUE) + 
                                 (max(SIMD_new()$value, na.rm = TRUE) 
                                  * 10 / 100)), 
                     
                     title = input$Measure4,
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
                                                 "SIMD",
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
                   
                   ##REMOVE LEGEND FOR NOW- until we have discussed whether 
                   #to have multiple options for any categories
                   showlegend = FALSE,
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
        
        
        
        
        
        #we can now add in the table for the bar chart- values
        #have to be reassigned to positive values
        SIMD_table <- reactive({
          deprivation %>%
            filter(
              hos_clin_type %in% input$Hospital_Clinic_Type4
              & activity_type %in% input$Activity_Measure4
              & drug_type %in% input$Substances4
              & measure %in% input$Measure4
              & year %in% input$Financial_Year2
            )%>%
            select(-c(measure,geography_type))
        })
        
        #Table
        output$SIMD_table <- renderDataTable({
          datatable(
            SIMD_table(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Hospital clinical type",
              "Activity type",
              "Location",
              "Drug type",
              "Deprivation index",
              input$Measure4
            )
          )
        })
        
        #Download button
        
        output$download_SIMD <- downloadHandler(
          filename = 'deprivation.csv',
          content = function(file) {
            write.table(
              SIMD_table(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial year",
                "Hospital clinical type",
                "Activity type",
                "Location",
                "Drug type",
                "Deprivation index",
                input$Measure4
              ),
              sep = ","
            )
          }
        )
      
##############################################.
############## Table tab ----
##############################################.
      
      
      #End of server
    }
  
  #End of script
}

shinyApp(ui = ui, server = server)
