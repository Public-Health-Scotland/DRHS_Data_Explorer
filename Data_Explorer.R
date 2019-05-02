#Name: Drug Related Hospital Statistics (DRHS) Data explorer page
#Author: Mike Smith
#Created: 24/01/2019
#Type: Data visualisation
#Written on: RStudio
#Written for: R version 3.5.1 
#Output: Shiny application

#This is the full version of the data explorer for the new DRHS dashboard. 

#Currently there are tabs for 
# Tab 1) A Home Page
# Tab 2) Time Trend (Geography)
# Tab 3) Time Trend (Substances)
# Tab 4) Age/Sex 
# Tab 5) Deprivation
# Tab 6) Table


#libraries
library(shiny)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(stringr)
library(DT)
library(RColorBrewer)
library(forcats)

##############################################.
############## Reading In Data ----
##############################################.

#So initially we need to read in the data. Currently saved as an rds file.
#NOTE- the following is temporary and is conditional on 
#where we decided to eventually store the final data set 

path<- "\\\\nssstats01\\SubstanceMisuse1\\Topics\\DrugRelatedHospitalStats\\Publications\\DRHS\\20181218\\Temp\\"


#Data to be used for explorer and trend pages
#Following is rounded data
all_data<- readRDS(paste0(path,"s06-temp09_num_rate_perc_R-SHINY_rounded.rds"))
#need to rename the final column as value
all_data<-all_data %>% 
  rename("value" = value_Round)

#Round to two decimal places. 
all_data<-all_data%>% 
  mutate(value = round(value, 2))


#We will manually change the names of factors in R until we have an agreed 
#terminology for the hospital type and clinical type, as well age and sex. 

all_data<-all_data %>% 
  mutate(hospital_type= fct_recode(hospital_type, 
                                   "General acute"= "General acute (SMR01)",
                                   "Psychiatric" ="Psychiatric (SMR04)",
                                   "Combined gen acute/psych" = "Combined (General acute/Psychiatric)"),
         clinical_type= fct_recode(clinical_type, 
                                   "Mental & behavioural (M&B)" = "Mental and Behavioural",
                                   "Overdose (OD)" = "Overdose",
                                   "Combined M&B/OD" = "Combined (Mental and Behavioural/Overdose)"),
         age_group = fct_recode(age_group, "All age groups" = "All"),
         sex = fct_recode(sex, "Both sexes" = "All"))


#Data that is not visualized  
length_of_stay <- readRDS(paste0(path,"s07-temp08_lsty_R-SHINY_ROUNDED.rds"))
length_of_stay<-length_of_stay %>% 
  rename("perc_less_1week" = perc_less_1week_round, 
         "perc_more_1week" = perc_more_1week_round,
         "total" = total_rounded)

emergency_admissions<- readRDS(paste0(path,"s08-temp08_emerAdm_R-SHINY_ROUNDED.rds"))
emergency_admissions <-emergency_admissions %>% 
  rename("perc_adm_emer" = perc_adm_emer_round, 
         "perc_adm_other" = perc_adm_other_round,
         "total" = total_rounded) 

drug_type_by_hospital<-readRDS (paste0(path,"s09-temp05_dist_hospit_R-SHINY_ROUNDED.rds"))

drug_type_by_hospital<-drug_type_by_hospital %>% 
  rename("total" = total_rounded) 

#filter data set for data for each tab

time_trend <- all_data %>% 
  filter(age_group == "All age groups",
         sex == "Both sexes", 
         simd == "All") %>% 
  select(-c(age_group,sex,simd))

age_sex <- all_data %>% 
  filter(geography =="Scotland", 
         simd== "All"
         ) %>% 
  select(-c(simd, geography_type,geography)) 

deprivation <- all_data %>% 
  filter(geography =="Scotland", 
          simd != "All") %>% 
  select(-c(age_group,sex, geography_type,geography))


#We then create the options for users to choose from in the drop down menus. 
#Drug Types are created as list to allow different options dependent on the 
#Hospital admission types
hospital_types <- as.character(unique(all_data$hospital_type))
clinical_types <- as.character(unique(all_data$clinical_type))
activity_type <- as.character(unique(all_data$activity_type))
location_types <- as.character(unique(all_data$geography_type))
locations<- as.character(unique(all_data$geography))

geography_list<-list("Scotland" = locations[1:3],
                     "NHS Board of residence" = locations[4:17],
                     "ADP of residence" = locations[18:48])

drug_types<- as.character(unique(all_data$drug_type))
drug_types1<- list("Main Categories" = as.character(unique(all_data$drug_type)[1:7]),
                  "Opioids Sub Categories" = as.character(unique(all_data$drug_type)[8:10]))
drug_types2<- as.character(unique(all_data$drug_type)[1:7])
measures<- as.character(unique(all_data$measure))


#Add in age, sex, SIMD and financial years options for demographic tabs
age <- as.character(unique(all_data$age_group))
sex <- as.character(unique(all_data$sex))
financial_years <- as.character(unique(all_data$year))
SIMD<- as.character(unique(all_data$simd))


#we need to look at altering the data for the tornado chart so that male values
#negative to allow it to work 
#Convert males to negative (and remove all)
age_sex_male <- age_sex %>%
  filter(sex == "Male"
         & age_group != "All age groups") %>%
  mutate(value = value * -1)
#Then remove females
age_sex_female <- age_sex %>%
  filter(sex == "Female"
         & age_group != "All age groups")

#recombine them into one chart
age_sex_tornado <- rbind(age_sex_male, age_sex_female)

#we can now set up the data for that from the data trend page

activity_summary<-all_data %>% 
  filter(drug_type == "All", 
        age_group == "All age groups",
        sex == "Both sexes",
        simd == "All", 
        measure == "Rate")

drug_summary<- all_data %>% 
  filter(activity_type == "Stays",
         drug_type %in% drug_types2,
         drug_type != "All",
         age_group == "All age groups",
         sex == "Both sexes",
         simd == "All", 
         measure == "Rate")


demographic_summary<- all_data  %>% 
  filter(drug_type == "All",
         activity_type =="Patients",
         ((age_group != "All age groups" & sex == "Both sexes" & simd =="All")|
            (age_group == "All age groups" & sex != "Both sexes" & simd =="All")|
            (age_group == "All age groups" & sex == "Both sexes" & simd !="All")), 
         measure == "Rate") 


#Keep only those columns that are necessary
activity_summary<-activity_summary %>% 
  select(year,hospital_type,clinical_type,activity_type,
         geography_type,geography,value)

drug_summary<-drug_summary %>% 
  select(year,hospital_type,clinical_type,drug_type,
         geography_type,geography,value)

demographic_summary<-demographic_summary %>% 
  select(year,hospital_type,clinical_type,
         geography_type,geography,
         age_group,sex,simd,
         value)

#We can then drop unnecessary columns from these tables

length_of_stay <- length_of_stay %>% 
  select(-activity_type)%>% 
  mutate(perc_less_1week = round(perc_less_1week, 2), 
         perc_more_1week = round(perc_more_1week, 2),
         hospital_type= fct_recode(hospital_type, 
                                   "General acute"= "General acute (SMR01)",
                                   "Psychiatric" ="Psychiatric (SMR04)",
                                   "Combined gen acute/psych" = "Combined (General acute/Psychiatric)"),
         clinical_type= fct_recode(clinical_type, 
                                   "Mental & behavioural (M&B)" = "Mental and Behavioural",
                                   "Overdose (OD)" = "Overdose",
                                   "Combined M&B/OD" = "Combined (Mental and Behavioural/Overdose)"),
         drug_type = fct_recode (drug_type, 
                                 "Sedatives/ Hypnotics" = "Sedatives/Hypnotics"),
         age_group = fct_recode(age_group, "All age groups" = "All"),
         sex = fct_recode(sex, "Both sexes" = "All"))

emergency_admissions <- emergency_admissions %>% 
  select(-activity_type)%>% 
  mutate(perc_adm_emer = round(perc_adm_emer, 2), 
         perc_adm_other = round(perc_adm_other, 2),
         hospital_type= fct_recode(hospital_type, 
                                   "General acute"= "General acute (SMR01)",
                                   "Psychiatric" ="Psychiatric (SMR04)",
                                   "Combined gen acute/psych" = "Combined (General acute/Psychiatric)"),
         clinical_type= fct_recode(clinical_type, 
                                   "Mental & behavioural (M&B)" = "Mental and Behavioural",
                                   "Overdose (OD)" = "Overdose",
                                   "Combined M&B/OD" = "Combined (Mental and Behavioural/Overdose)"),
                 drug_type = fct_recode (drug_type, 
                                 "Sedatives/ Hypnotics" = "Sedatives/Hypnotics"),
         age_group = fct_recode(age_group, "All age groups" = "All"),
         sex = fct_recode(sex, "Both sexes" = "All"))

drug_type_by_hospital <- drug_type_by_hospital %>% 
  select(-c(geography_type,geography,
             age_group,sex,simd))%>% 
  mutate(perc_source01 = round(perc_source01, 2), 
         perc_source04 = round(perc_source04, 2), 
         perc_sourceBOTH = round(perc_sourceBOTH, 2),
         hospital_type= fct_recode(hospital_type, 
                                   "Combined gen acute/psych" = "Combined (General acute/Psychiatric)"),
         clinical_type= fct_recode(clinical_type, 
                                   "Mental & behavioural (M&B)" = "Mental and Behavioural",
                                   "Overdose (OD)" = "Overdose",
                                   "Combined M&B/OD" = "Combined (Mental and Behavioural/Overdose)"))


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
        "The data explorer allows you to visualise drug realted hospital stay
        (DRHS) data in a variety of ways."
      ),
      tags$ul(
        tags$li(
          tags$b(actionLink(
            "link_to_geography", "Time trend (location comparison)"
          )),
          icon("line-chart"),
          " - shows DRHS activity over time, allowing comparisons by location."
      ),
      
        tags$li(
          tags$b(actionLink(
            "link_to_substances", "Time trend (drug type comparison)"
          )),
          icon("line-chart"),
          " - shows DRHS activity over time, allowing comparisons by drug type."
        ),
      tags$li(
        tags$b(actionLink(
          "link_to_age_sex", "Age/sex"
        )),
        icon("child"),
        " - shows DRHS activity by age group and sex.."
      ),
      tags$li(
        tags$b(actionLink(
          "link_to_deprivation", "Deprivation"
        )),
        icon("bar-chart"),
        " - shows DRHS activity by deprivation group."
      ),
      tags$li(
        tags$b(actionLink(
          "link_to_table", "Table"
        )),
        icon("table"),
        " - data tables related to content of the Data Explorer and Data Trends
        pages, plus additional tables not visualised."
      )
      ),
      
      p(
        "When using the data explorer, please take the following 
        factors into consideration:"
      ),
      tags$ul( 
        tags$li(
          "The data explorer visualises information recorded in the SMR01 and SMR04
          datasets. The SMR01 dataset records general acute hospital inpatient and day
          case activity and SMR04 records psychiatric hospital inpatient and day case 
          activity."
        ),
        tags$li(
          "Data completeness may vary slightly from year to year. As a result,
          data are provisional and subject to change. For more information, visit 
          the SMR Completeness webpage. ", 
          tags$a(
            href = "http://www.isdscotland.org/products-and-Services/Data-Support-and-Monitoring/SMR-Completeness/", 
            "SMR Completeness"
          ),
          " webpage."
        ), 
        
        tags$li(
          "Diagnostic information is recorded using the International Statistical
          Classification of Diseases and Related Health Problems, 10th Edition
          (ICD-10). ICD-10 codes used to classify drug-related hospital stays 
          are listed in the Glossary. Note that patients may have more than one
          drug-related diagnosis per stay." 
          
        ),
        tags$li(
          "Statistical disclosure control has been applied to protect patient
          confidentiality. Therefore, the figures presented in this dashboard may 
          not be additive and may differ to previous sources of information.  
          For more information, please refer to the  ",
          tags$a(
            href = "http://www.isdscotland.org/About-ISD/Confidentiality/disclosure_protocol_v3.pdf", 
            "NSS Statistical Disclosure Control Protocol."
          ), 
          
          ""
        )
        
      ),
      
      p(
        "To help users understand the information visualised in the Data Explorer,
        we have created a glossary of commonly used terms. 
        Click the button below to download the glossary:"
      ),
      
      
      downloadButton(outputId = "download_glossary", 
                     label = "Download glossary", 
                     class = "glossary"),
      tags$head(
        tags$style(".glossary { background-color: #0072B2; } 
                   .glossary { color: #FFFFFF; }")
        ),
      
      p(
        br(),
        "If you experience any problems using this dashboard or have further
        questions relating to the data, please contact us at:",
        tags$b(
          tags$a(href = "mailto:NSS.isdsubstancemisuse@nhs.net",
          "NSS.isdsubstancemisuse@nhs.net.")
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
    h3("Time trend (location comparison)"),
    p(
      HTML(
        "This section allows users to visualise changes in drug related hospital
         activity over time and to make comparisons between different locations. 
         Information is available for financial years 1996/97 to 2017/18."),
        br(),
      br(),
      HTML("Use the filters to visualise the data you are interested in.
          You can visualise multiple locations at the same time to allow 
          comparisons between Scotland, NHS Boards and Alcohol and Drug 
          Partnerships (ADPs). Individual trend lines can be hidden by clicking 
          on the labels shown in the chart legend. Opioid sub categories are 
          available if overdoses are selected as Clinical type. ADP information 
          is available from 1997/98 and new patient trends are available from 2006/07."),
      br(),
      br(),
      HTML("To view 
        your data selection in a table, use the <a href = '#geography_link'> 
       'Show/hide table' </a>  button at the
        bottom of the page. To download your data selection as a CSV file, use the
        'Download data' button under the filters. At the top-right corner of the 
        graph, you will see a toolbar with four buttons:"
      )
      ),
    
    tags$ul(
      tags$li(
        icon("camera"),
        tags$b("Download plot as a png"),
        " - click this button to save the graph as an image
        (please note that Internet Explorer does not support this
        function)."
      ),
      tags$li(
        icon("search"),
        tags$b("Zoom"),
        " - zoom into the graph by clicking this button and then
        clicking and dragging your mouse over the area of the
        graph you are interested in."
      ),
      tags$li(
        icon("move", lib = "glyphicon"),
        tags$b("Pan"),
        " - adjust the axes of the graph by clicking this button
        and then clicking and moving your mouse in any direction
        you want."
      ),
      tags$li(
        icon("home"),
        tags$b("Reset axes"),
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
      # 2 - Activity Type
      # 3 - Geography Type
      # 4 - Geography (Multiple)
      # 5 - Substance
      # 6 - Measure
      
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Hospital_Type",
          label = "Select hospital type",
          choices = hospital_types
        ), 
        shinyWidgets::pickerInput(
          inputId = "Location",
          label = "Select location (multiple selection)",
          choices = geography_list,
          multiple = TRUE,
          selected = "Scotland",
          options = list(size=10, 
                         `live-search`=TRUE, 
                         `selected-text-format` = "count > 1", 
                         `count-selected-text` = "{0} locations chosen (8 Max)",
                         "max-options" = 8,
                         "max-options-text" = "Only 8 options can be chosen")
        ),
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
        uiOutput("time_trend_clinical_type"), 
        uiOutput("time_trend_substance1")
        ),
        
        column(
          4,
          shinyWidgets::pickerInput(
            inputId = "Activity_Type",
            label = "Select activity type",
            choices = activity_type
          ),
          shinyWidgets::pickerInput(
            inputId = "Measure",
            label = "Select measure",
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
                   height = "500px"),
      br(),
                  HTML("<button data-toggle = 'collapse' href = '#geography'
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
    h3("Time trend (drug type comparison)"),
    p(
      HTML(
        "This section allows users to visualise changes in drug related hospital
        activity over time and to make comparisons between different drug types.
        Information is available for financial years 1996/97 to 2017/18. "
      ),
      br(),
      br(),
      HTML("Use the filters to visualise the data you are interested in. You can
           visualise multiple drug types at the same time to allow comparisons
           between drug types within a single location (Scotland, 
           an NHS Board or an Alcohol and Drug Partnership (ADP)). Individual 
           trend lines can be hidden by clicking on the labels shown in 
           the chart legend. Opioid sub categories are available if overdoses 
           are selected as Clinical type. ADP information is available from 
           1997/98 and new patient trends are available from 2006/07."),
      br(),
      br(),
      HTML("To view your data selection in a table,
           use the <a href = '#substances_link'> 'Show/hide table' </a>  button at the
           bottom of the page. To download your data selection as a CSV file, use the
           'Download data' button under the filters. At the top-right corner of the 
           graph, you will see a toolbar with four buttons:")
      ),
    
    tags$ul(
      tags$li(
        icon("camera"),
        tags$b("Download plot as a png"),
        " - click this button to save the graph as an image
        (please note that Internet Explorer does not support this
        function)."
      ),
      tags$li(
        icon("search"),
        tags$b("Zoom"),
        " - zoom into the graph by clicking this button and then
        clicking and dragging your mouse over the area of the
        graph you are interested in."
      ),
      tags$li(
        icon("move", lib = "glyphicon"),
        tags$b("Pan"),
        " - adjust the axes of the graph by clicking this button
        and then clicking and moving your mouse in any direction
        you want."
      ),
      tags$li(
        icon("home"),
        tags$b("Reset axes"),
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
      # 2 - Activity Type
      # 3 - Geography Type
      # 4 - Geography 
      # 5 - Substance (Multiple)
      # 6 - Measure
      
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Hospital_Type2",
          label = "Select hospital type",
          choices = hospital_types
        ), 
        shinyWidgets::pickerInput(
          inputId = "Location2",
          label = "Select location",
          choices = geography_list,
          selected = "Scotland",
          options = list(size=10, 
                         `live-search`=TRUE)
        ),
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
        uiOutput("time_trend_clinical_type2"), 
        uiOutput("time_trend_substance2")
      ),
      
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Activity_Type2",
          label = "Select activity type",
          choices = activity_type
        ),
        shinyWidgets::pickerInput(
          inputId = "Measure2",
          label = "Select measure",
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
                   height = "500px"), 
      br(),
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
    HTML("This section allows users to visualise drug-related hospital 
         activity on the basis of the age and sex of patients. Information 
         is available for Scotland level and for financial years 1996/97 to 2017/18."),
    br(),
    br(),
    HTML("Use the filters to visualise the data you are interested in. 
         Opioid sub categories are available if overdoses are selected as 
         clinical type. New patient trends are available from 2006/07. The toggle
         buttons allow the data to be visualised in two ways:")
    
    ),

  tags$ul(
    tags$li(
      tags$b("Line chart"),
      icon("line-chart"),
      " - displays drug-related hospital patient trends for specific age and 
      sex groups over time. Select sex or age groups from the drop down menus.
      Multiple groups can be shown on the chart simultaneously. Individual trend 
      lines can be hidden by clicking on the labels shown in the chart legend."
    ),
    tags$li(
      tags$b("Bar Chart "),
      icon("bar-chart"),
      " - shows annual breakdowns of drug-related hospital patients by age and sex.
      Use the slider to control which year to look at, or use the play button to 
      see changes over time."
    )),
p(HTML("To view your data selection in a table, use the 
         <a href = '#age_and_sex_link'> 'Show/hide table' </a> button at 
         the bottom of the page. To download your data selection as a CSV 
         file, use the 'Download data' button under the filters. At the 
         top-right corner of the graph, you will see a toolbar with four 
         buttons:")),
  
  tags$ul(
    tags$li(
      icon("camera"),
      tags$b("Download plot as a png"),
      " - click this button to save the graph as an image
      (please note that Internet Explorer does not support this
      function)."
    ),
    
    tags$li(
      icon("search"),
      tags$b("Zoom"),
      " - zoom into the graph by clicking this button and then
      clicking and dragging your mouse over the area of the
      graph you are interested in."
    ),
    
    tags$li(
      icon("move", lib = "glyphicon"),
      tags$b("Pan"),
      " - adjust the axes of the graph by clicking this button
      and then clicking and moving your mouse in any direction
      you want."
    ),
    
    tags$li(
      icon("home"),
      tags$b("Reset axes"),
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
    # 2 - Activity Type
    # 3 - Substance (dependent on Hospital/Clinic Type)
    # 4 - Measure
    
    column(4,
           shinyWidgets::pickerInput(
             inputId = "Hospital_Type3",
             label = "Select hospital type",
             choices = hospital_types
           ),
           uiOutput("age_sex_substance")),
    
    column(
      4,
      uiOutput("age_sex_clinical_type"),
      shinyWidgets::pickerInput(
        inputId = "Measure3",
        label = "Select measure",
        choices = measures,
        selected = "Rate"
      )
    ), 
    
    column(4,
           shinyWidgets::pickerInput(
             inputId = "Activity_Type3",
             label = "Select activity type",
             choices = activity_type,
             selected = "Stays"
           ))
  ),

  
  #In the main panel of the tab, insert the time trend plot
  mainPanel(width = 12, 
            tabsetPanel(
    type = "pills",
    tabPanel(
      "Time Trend",
      tags$style(
        HTML("
           .tabbable > .nav > li > a[data-value = 'Bar Chart'] {background-color: #D3D3D3; color: #000000;}
           .tabbable > .nav > li > a[data-value = 'Time Trend'] {background-color: #D3D3D3; color: #000000;}
           .tabbable > .nav > li[class = active] > a {background-color: #0072B2;color: #FFFFFF;} 
           ") 
      ),
      icon = icon("line-chart"),
      style = "height: 95%; width: 95%; background-color: #FFFFFF;
      border: 0px solid #FFFFFF;",
      br(),
      br(),
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Age",
          label = "Select age group (multiple selection)",
          choices = age,
          multiple = TRUE,
          selected = "All age groups"
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
          label = "Select sex (multiple selection)",
          choices = sex,
          multiple = TRUE,
          selected = "Both sexes"
        )
      ),
      column (4,
              br(),
              
              downloadButton(
                outputId = "download_age_sex_trend",
                label = "Download data",
                class = "myagesextrendbutton"
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
                   height = "500px"),
      br(),
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
        chooseSliderSkin("HTML5"),
        shinyWidgets::sliderTextInput(
          inputId = "Financial_Year",
          label = "Select financial year",
          choices = financial_years,
          selected = "2017/18",
          grid = T,
          animate = animationOptions(playButton =icon('play', 
                                                      "fa fa-play-circle fa-2x"),
                                     pauseButton = icon('pause', 
                                                        "fa fa-pause-circle fa-2x")),
          width = "1090px"
          
        )
        ),
      column(1),
      column(3,
             br(),
             br(),
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
      )
      ,
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
   
        plotlyOutput("age_sex_year_plot",
                     width = "1090px",
                     height = "500px"),
      br(),
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
      "This section allows users to visualise drug-related hospital activity 
      on the basis of the deprivation group of patients. Information is available 
      for Scotland level and for financial years 1996/97 to 2017/18. "
    ),
    br(),
    br(),
    HTML("Use the filters to visualise the data you are interested in. Opioid sub
         categories are available if overdoses are selected as Clinical type. New 
         patient trends are available from 2006/07."),
    br(),
    br(),
    HTML("To view your data selection in a table,
      use the <a href = '#SIMD_link'> 'Show/hide table' </a>  button at the
      bottom of the page. To download your data selection as a CSV file, use the
      'Download data' button under the filters. At the top-right corner of the 
      graph, you will see a toolbar with four buttons:")
    ),
  
  tags$ul(
    tags$li(
      icon("camera"),
      tags$b("Download plot as a png"),
      " - click this button to save the graph as an image
      (please note that Internet Explorer does not support this
      function)."
    ),
    tags$li(
      icon("search"),
      tags$b("Zoom"),
      " - zoom into the graph by clicking this button and then
      clicking and dragging your mouse over the area of the
      graph you are interested in."
    ),
    tags$li(
      icon("move", lib = "glyphicon"),
      tags$b("Pan"),
      " - adjust the axes of the graph by clicking this button
      and then clicking and moving your mouse in any direction
      you want."
    ),
    tags$li(
      icon("home"),
      tags$b("Reset axes"),
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
    # 2 - Activity Type
    # 3 - Substance (dependent on Hospital/Clinic Type)
    # 4 - Financial Year
    # 5 - Measure
    
    column(4,
           shinyWidgets::pickerInput(
             inputId = "Hospital_Type4",
             label = "Select hospital type",
             choices = hospital_types
           ),
           uiOutput("SIMD_substance"),

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
      
      uiOutput("SIMD_clinical_type"), 
      
      shinyWidgets::pickerInput(
        inputId = "Financial_Year2",
        label = "Select financial year",
        choices =  rev(financial_years),
        selected = "2017/18")
      

    ),
    
    column(4,
           shinyWidgets::pickerInput(
             inputId = "Activity_Type4",
             label = "Select activity type",
             choices = activity_type,
             selected = "Stays"
           ), 
           shinyWidgets::pickerInput(
             inputId = "Measure4",
             label = "Select measure",
             choices = measures,
             selected = "Rate"
           )
    )  
  ),
  
  
  #In the main panel of the tab, insert the SIMD plot
  
  mainPanel(
    width =12,
    plotlyOutput("SIMD_plot",
                 width = "1090px",
                 height = "500px"),
    br(),
    
    
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
  icon = icon("table"), 
  style = "float: top; height: 95%; width: 95%; background-color: #FFFFFF; 
  border: 0px solid #FFFFFF;", 
  h3("Table"), 
  p(
    
  ),
  
  p(
    HTML(
      "This section allows users to view the data in table format. 
       Use the filter below to access tabular information for the dataset you are
       interested in. The list includes datasets used in both the 'Data trends' 
       and ‘Data Explorer’ pages. The list also includes data not visualised in 
       this dashboard (Length of stay, Emergency admissions and Drug type by hospital).
      " ), 
   br(),
   br(),
    HTML("You can then use the 
    filters situated below the column names of the table to modify the data table. 
    To download your data selection as a CSV file, use the 
    'Download data' button.")
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
  
  wellPanel(tags$style(".well { background-color: #FFFFFF; 
                       border: 0px solid #336699; }"),
            
            #We are only using one filter here, which contains the...
            #names of the files.
            
            column(6,
                   
                   shinyWidgets::pickerInput(
                     inputId = "table_filenames", 
                     label = "Select data file",  
                     choices = c("Time trend (Data explorer)", 
                                 "Age/sex (Data explorer)", 
                                 "Deprivation (Data explorer)", 
                                 "Activity summary (Trend data)",
                                 "Drug summary (Trend data)",
                                 "Demographic summary (Trend data)",
                                 "Length of stay",
                                 "Emergency admissions",
                                 "Drug type by hospital"), 
                     width = "95%"
                   )
                   
            ), 
            
            #We also insert the 'Download data' button.
            
            column(4,
                   
                   downloadButton(outputId = 'download_table', 
                                  label = 'Download data', 
                                  class = "mytablebutton", 
                                  style = "margin: 25px 10px 25px 10px")
                   
            )
  ),
  
  tags$head(
    tags$style(".mytablebutton { background-color: #0072B2; } 
                    .mytablebutton { color: #FFFFFF; }")
  ),
  
  
  #Finally, insert the actual table.
  
  mainPanel(width = 12, 
            dataTableOutput("table_tab")) 
  
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
      

      output$time_trend_clinical_type <- renderUI({
        shinyWidgets::pickerInput(inputId = "Clinical_Type", 
                                  label = "Select clinical type",
                                  choices = clinical_types)
      })

      output$time_trend_substance1 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Substances",
                                  label = "Select drug type",  
                                  choices = (if(input$Clinical_Type == "Overdose (OD)")
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
            hospital_type %in% input$Hospital_Type
            & clinical_type %in% input$Clinical_Type
            & activity_type %in% input$Activity_Type
            & geography %in% input$Location
            & drug_type %in% input$Substances
            & measure %in% input$Measure 
          )%>%
          select(year, hospital_type, clinical_type, activity_type,
                 geography_type, geography, drug_type,value) %>% 
          droplevels(except= c(1,5))
      })
      
      #then we can plot the actual graph, with labels
      output$geography_plot <- renderPlotly({
        
        if ((input$Location == 
            "Outside Scotland"|
             input$Location == 
             "Other/Not Known")
          & input$Measure== 
             "Rate"
          )
          
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Rates are not available for locations outside Scotland or
            unknown", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, collaborate = F, editable = F) 
          
        }
        
        #Now let's create alt message.

        else if (input$Substances == 
                  "All"
                  & input$Measure== 
                  "Percentage")
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Percentages are not available for all drugs", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, collaborate = F, editable = F) 
          
        }
        
        
        else if (is.null(input$Location))
                 
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Please make a selection from the drop down menus", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, collaborate = F, editable = F) 
          
        }
        
        

        else {
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
          input$Measure,": ",
          formatC(geography_new()$value, big.mark = ",")
        )
        
        #Create the main body of the chart.
        
        plot_ly(
          data = geography_new(),
          #plot
          x = ~  year,
          y = ~  value,
          color = ~  geography,
          colors = 
            #We use 8 colours that are considered to be 
            #blind friendly
            c('#006ddb','#db6d00','#920000',
              '#ffb6db','#490092','#6db6ff',
              '#000000','#004949'
              ),
       
          symbol = ~ geography_type,
          symbols = ~c(17,15,16),
          name = ~  str_wrap(geography,10),
          #tooltip
          text = tooltip_geography,
          hoverinfo = "text",
          #type
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(size = 7),
          width = 1000,
          height = 500
        ) %>%
   
          #add in title to chart
          
          
          layout(
            #Title
          title = (paste0("<b>",paste0(str_sub(input$Activity_Type,1,-2),
                                          " ", 
                                       str_to_lower(input$Measure),
                                       " for ",
                                       str_to_lower(input$Hospital_Type),
                                       " hospitals with","<br>", "clinical type ",
                                      str_to_lower(str_sub(input$Clinical_Type,start = 1,end = 1)),
                                       str_sub(input$Clinical_Type,start = 2),
                                       " due to","<br>", "drug type ", 
                                       str_to_lower(input$Substances),
                                       " by location"),"<b>")),
            

          
          
                 separators = ".,",
          
          annotations = 
            list(x = 0.99, y = -0.27, 
                 text = paste0("Source: Drug-Related","<br>",
                               "Hospital Statistics,","<br>",
                               "ISD Scotland (",format(Sys.Date(), "%Y"),")"), 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='left', yanchor='auto', xshift=0, yshift=0,
                 font=list(family = "arial", size=12, color="#7f7f7f")),
          
          
          #y=axis formatting       
           yaxis = list(
                                      
                   exponentformat = "none",
                   
                   separatethousands = TRUE,
                   
                   range = c(0, max(geography_new()$value, na.rm = TRUE) +
                               (max(geography_new()$value, na.rm = TRUE)
                                * 10 / 100)),
                   
                   title = 
                     ifelse(input$Measure == "Rate",
                            paste0(c(
                              rep("&nbsp;", 20),
                              "EASR per 100,000 population",
                              rep("&nbsp;", 20),
                              rep("\n&nbsp;", 3)
                            ),
                            collapse = ""),
                            paste0(c(
                              rep("&nbsp;", 20),
                              input$Measure,
                              rep("&nbsp;", 20),
                              rep("\n&nbsp;", 3)
                            ),
                            collapse = "")
                     ),
                   showline = TRUE,
                   ticks = "outside"
                   
                 ),
               
                 #Set the tick angle to minus 45. It's the only way for the x...
                 #axis tick labels (fin. years) to display without overlapping...
                 #with each other.
                 #Wrap the x axis title in blank spaces so that it doesn't...
                 #overlap with the x axis tick labels.
                 
                 xaxis = list(range = c(-1,22),
                              tickangle = -45,
                              title = paste0("<br>",
                                               "<br>",
                                               "Financial year"),
                              showline = TRUE,
                              ticks = "outside"),
        
                      
                 #        #Fix the margins so that the graph and axis titles have enough...
                 #       #room to display nicely.
                 #      #Set the font sizes.
                 #
                 margin = list(l = 90, r = 60, b = 70, t = 90),
                 font = list(size = 13),
                 titlefont = list(size = 15),
                 
                 #insert legend
                 showlegend = TRUE,
                 legend = list(
                               bgcolor = 'rgba(255, 255, 255, 0)',
                               bordercolor = 'rgba(255, 255, 255, 0)')
          ) %>%
          
          
          #Remove unnecessary buttons from the modebar.
          
          config(displayModeBar = TRUE,
                 modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                               'zoomOut2d', 'autoScale2d',
                                               'toggleSpikelines',
                                               'hoverCompareCartesian',
                                               'hoverClosestCartesian'),
                 displaylogo = F, collaborate = F, editable = F)
        }
      })
      

      #Insert table
      output$geography_table <- renderDataTable({
        datatable(geography_new(),
                  colnames = c("Financial year",
                               "Hospital type",
                               "Clinical type",
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
          write.table(geography_new(), 
                      file,
                      #Remove row numbers as the CSV file already has row numbers.
                      
                      row.names = FALSE,
                      col.names = c("Financial year", "Hospital type", "Clinical type",
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
      
      
      
      
      output$time_trend_clinical_type2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Clinical_Type2", 
                                  label = "Select clinical type",
                                  choices = clinical_types)
      })
      
      output$time_trend_substance2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Substances2",
                                  label = "Select drug type (multiple selection)",  
                                  choices = (if(input$Clinical_Type2 == "Overdose (OD)")
                                  drug_types1
                                  else
                                  drug_types2),
                                  multiple = TRUE, 
                                  options = list (`selected-text-format` = "count > 1", 
                                                  `count-selected-text` = "{0} drug types chosen"),
                                  selected = "All"
        )
      }) 

      #we can then plot the graph based on the user input.
      #First we create a subset based on user input
    
      substances_new <- reactive({
        time_trend %>%
          filter(
            hospital_type %in% input$Hospital_Type2
            & clinical_type %in% input$Clinical_Type2
            & activity_type %in% input$Activity_Type2
            & geography %in% input$Location2
            & drug_type %in% input$Substances2
            & measure %in% input$Measure2
          )%>%
          select(year, hospital_type, clinical_type, activity_type,
                 geography_type, geography, drug_type,value)
      })
      
      #then we can plot the actual graph, with labels
      output$substances_plot <- renderPlotly({
        
        if ((input$Location2 == 
             "Outside Scotland"|
             input$Location2 == 
             "Other/Not Known")
            & input$Measure2 == 
            "Rate"
        )
          
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Rates are not available for locations outside Scotland or
            unknown", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, collaborate = F, editable = F) 
          
        }
        
        #Now let's create alt message.
        
        else if (input$Substances2 == 
                 "All"
                 & length(input$Substances2) ==1
                 & input$Measure2 == 
                 "Percentage")
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Percentages are not available for all drugs", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, collaborate = F, editable = F) 
          
        }
        
        
        else if (is.null(input$Substances2))
        
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Please make a selection from the drop down menus", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, collaborate = F, editable = F) 
          
        }
        
        
        
        else {
        
        
        
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
                  input$Measure2,": ",
                  formatC(substances_new()$value, big.mark = ",")
                )
                
        #Create the main body of the chart.
        
        plot_ly(
          data = substances_new(),
          #plot
          x = ~  year,
          y = ~  value,
          color = ~  drug_type,
          colors = 
            #Colors are assigned to each drug type
          c('#000000','#b66dff','#004949',
            '#b6dbff','#490092','#920000',
            '#006ddb','#6db6ff','#db6d00',
            '#ffb6db'
          ),
          name = ~ str_wrap(drug_type,10),
          #tooltip
                    text = tooltip_substances,
                    hoverinfo = "text",
          #type
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(size = 7),
          width = 1000,
          height = 500
        ) %>%
          
          #add in title to chart
          
          
          layout(
            #Title
            title = (paste0("<b>",paste0(str_sub(input$Activity_Type2,1,-2),
                                         " ", 
                                         str_to_lower(input$Measure2),
                                         " for ",
                                         str_to_lower(input$Hospital_Type2),
                                         " hospitals with","<br>", "clinical type ",
                                         str_to_lower(str_sub(input$Clinical_Type2,start = 1,end = 1)),
                                         str_sub(input$Clinical_Type2,start = 2),
                                         " in","<br>", 
                                         input$Location2,
                                         " by drug type"),"<b>")),
 
            separators = ".,",
            annotations = 
              list(x = 0.96, y = -0.27, 
                   text = paste0("Source: Drug-Related","<br>",
                                 "Hospital Statistics,","<br>",
                                 "ISD Scotland (",format(Sys.Date(), "%Y"),")"), 
                   showarrow = F, xref='paper', yref='paper', 
                   xanchor='left', yanchor='auto', xshift=0, yshift=0,
                   font=list(family = "arial", size=12, color="#7f7f7f")),
            
            #y=axis formatting       
            yaxis = list(
              
              exponentformat = "none",
              
              separatethousands = TRUE,
              
              range = c(0, max(substances_new()$value, na.rm = TRUE) +
                          (max(substances_new()$value, na.rm = TRUE)
                           * 10 / 100)),
              
              title = ifelse(input$Measure2 == "Rate",
                             paste0(c(
                               rep("&nbsp;", 20),
                               "EASR per 100,000 population",
                               rep("&nbsp;", 20),
                               rep("\n&nbsp;", 3)
                             ),
                             collapse = ""),
                             paste0(c(
                               rep("&nbsp;", 20),
                               input$Measure2,
                               rep("&nbsp;", 20),
                               rep("\n&nbsp;", 3)
                             ),
                             collapse = "")
              ),
              showline = TRUE,
              ticks = "outside"
              
            ),
            
            #Set the tick angle to minus 45. It's the only way for the x...
            #axis tick labels (fin. years) to display without overlapping...
            #with each other.
            #Wrap the x axis title in blank spaces so that it doesn't...
            #overlap with the x axis tick labels.
            
            xaxis = list(range = c(-1,22),
                         tickangle = -45,
                         title = paste0("<br>",
                                        "<br>",
                                        "Financial year"),
                         showline = TRUE,
                         ticks = "outside"),
            
            #        #Fix the margins so that the graph and axis titles have enough...
            #       #room to display nicely.
            #      #Set the font sizes.
            #
            margin = list(l = 90, r = 60, b = 70, t = 90),
            font = list(size = 13),
            titlefont = list(size = 15),
            
            #insert legend
            showlegend = TRUE,
            legend = list(
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
        }  
      })
      

      
      #Insert table
      output$substances_table <- renderDataTable({
        datatable(substances_new(),
                  colnames = c("Financial year",
                               "Hospital type",
                               "Clinical type",
                               "Activity type",
                               "Location type",
                               "Location",
                               "Drug type",
                               input$Measure2),
                  rownames = FALSE,
                  style = "Bootstrap")
      })
        
        output$download_substances <- downloadHandler(
          filename = 'time_trend_Substance_data.csv',
          content = function(file) {
            write.table(substances_new(), 
                        file,
                        #Remove row numbers as the CSV file already has row numbers.
                        
                        row.names = FALSE,
                        col.names = c("Financial year", 
                                      "Hospital type", 
                                      "Clinical type", 
                                      "Activity type" ,
                                      "Location type",
                                      "Location", 
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
            inputId = "Clinical_Type3",
            label = "Select clinical type",
            choices = clinical_types
          )
        })
        
        output$age_sex_substance <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Substances3",
            label = "Select drug type",
            choices = (if (input$Clinical_Type3 == "Overdose (OD)")
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
              hospital_type %in% input$Hospital_Type3
              & clinical_type %in% input$Clinical_Type3
              & activity_type %in% input$Activity_Type3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
              #and the age/sex options
              & age_group %in% input$Age
              & sex %in% input$Sex
            ) %>%
            select(year, hospital_type, clinical_type, activity_type,
                   drug_type, age_group,sex,value)
        })

        
        #Create the main body of the chart.
        output$age_sex_time_plot <- renderPlotly({
          
          
          if (input$Substances3 == 
             "All"
             & input$Measure3 == 
             "Percentage")
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Percentages are not available for all drugs", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, collaborate = F, editable = F) 
            
          }
          
          #Now let's create alt message.

          else if (is.null(input$Age)|
            is.null(input$Sex))
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Please make a selection from the drop down menus", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, collaborate = F, editable = F) 
            
          }
          
          else {
          
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
            formatC(abs(age_sex_time_new()$value), big.mark = ",")
          )
          
          plot_ly(
            data = age_sex_time_new(),
            #plot
            x = ~  year,
            y = ~  value,
            color = ~  age_group,
            colors = #Colors are assigned 
            c('#b66dff','#db6d00','#920000','#006ddb',
              '#490092','#6db6ff',
              '#b6dbff', '#000000'
            ),
            #so we will use different linetypes to
            #distinguish between sex.
            linetype = ~ sex,
            #tooltip
            text = tooltip_age_sex_time,
            hoverinfo = "text",
            #type
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(size = 7),
            width = 1000,
            height = 500
          ) %>%
            
            #Make the graph title reactive.
            
            layout(title = (paste0("<b>",paste0(str_sub(input$Activity_Type3,1,-2),
                                                " ", 
                                                str_to_lower(input$Measure3),
                                                " for ",
                                                str_to_lower(input$Hospital_Type3),
                                                " hospitals with","<br>", "clinical type ",
                                                str_to_lower(str_sub(input$Clinical_Type3,start = 1,end = 1)),
                                                str_sub(input$Clinical_Type3,start = 2),
                                                " by drug type ",
                                                str_to_lower(input$Substances3),"<br>",
                                                " by age group and sex in Scotland"),"<b>")),
                   

                   separators = ".,",
                   annotations = 
                     list(x = 0.98, y = -0.27, 
                          text = paste0("Source: Drug-Related","<br>",
                                        "Hospital Statistics,","<br>",
                                        "ISD Scotland (",format(Sys.Date(), "%Y"),")"), 
                          showarrow = F, xref='paper', yref='paper', 
                          xanchor='left', yanchor='auto', xshift=0, yshift=0,
                          font=list(family = "arial", size=12, color="#7f7f7f")),
                   
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
                     
                     title = ifelse(input$Measure3 == "Rate",
                                    paste0(c(
                                      rep("&nbsp;", 20),
                                      "EASR per 100,000 population",
                                      rep("&nbsp;", 20),
                                      rep("\n&nbsp;", 3)
                                    ),
                                    collapse = ""),
                                    paste0(c(
                                      rep("&nbsp;", 20),
                                      input$Measure3,
                                      rep("&nbsp;", 20),
                                      rep("\n&nbsp;", 3)
                                    ),
                                    collapse = "")
                     ),
                     showline = TRUE, 
                     ticks = "outside"
                     
                   ),
                   
                   #Set the tick angle to minus 45. It's the only way for the x...
                   #axis tick labels (fin. years) to display without overlapping...
                   #with each other.
                   #Wrap the x axis title in blank spaces so that it doesn't...
                   #overlap with the x axis tick labels.
                   
                   xaxis = list(range = c(-1,22),
                     tickangle = -45, 
                                title = paste0(
                                                 "<br>",
                                                 "<br>",
                                                 "Financial year"
                                                 ),
                                showline = TRUE, 
                                ticks = "outside"),
                   
                   #Fix the margins so that the graph and axis titles have enough...
                   #room to display nicely.
                   #Set the font sizes.
                   
                   margin = list(l = 90, r = 60, b = 70, t = 90),
                   font = list(size = 13),
                   titlefont = list(size = 15),
                   
                   #Insert a legend so that the user knows which colour...
                   #corresponds to which location of treatment.
                   #Make the legend background and legend border white.              
                   
                   showlegend = TRUE,
                   legend = list(bgcolor = 'rgba(255, 255, 255, 0)', 
                                 bordercolor = 'rgba(255, 255, 255, 0)')) %>%
            
            #Remove unnecessary buttons from the modebar.
            
            config(displayModeBar = TRUE,
                   modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                                 'zoomOut2d', 'autoScale2d', 
                                                 'toggleSpikelines', 
                                                 'hoverCompareCartesian', 
                                                 'hoverClosestCartesian'), 
                   displaylogo = F, collaborate = F, editable = F)
          }
        })
        

        
        #Table
        output$age_sex_trend_table <- renderDataTable({
          datatable(
            age_sex_time_new(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Hospital  type",
              "Clinical type",
              "Activity type",
              "Drug type",
              "Age group",
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
              age_sex_time_new(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial year",
                "Hospital type",
                "Clinical type",
                "Activity type",
                "Drug type",
                "Age group",
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
              & hospital_type %in% input$Hospital_Type3
              & clinical_type %in% input$Clinical_Type3
              & activity_type %in% input$Activity_Type3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
            ) %>%
            droplevels()
        })
        
        age_sex_year_new_axis <- reactive({
          age_sex_tornado %>%
            filter(hospital_type %in% input$Hospital_Type3
                   & clinical_type %in% input$Clinical_Type3
              & activity_type %in% input$Activity_Type3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
            ) %>%
            droplevels()
        })
        
        
        #then plot it
        output$age_sex_year_plot <- renderPlotly({
          
          
          if (input$Substances3 == 
              "All"
              & input$Measure3 == 
              "Percentage")
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Percentages are not available for all drugs", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, collaborate = F, editable = F) 
            
          }
          
          #Now let's create alt message.
          
          else if (input$Financial_Year %in% financial_years[1:10]
                   & input$Activity_Type3 == "New patients")
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Data are not available for new patients before 
              2006/07", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, collaborate = F, editable = F) 
            
          }
          
          else {

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
            formatC(abs(age_sex_year_new()$value), big.mark = ",")
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
            height = 500
          ) %>%
            
            layout(title = (paste0("<b>",paste0(str_sub(input$Activity_Type3,1,-2),
                                           " ", 
                                           str_to_lower(input$Measure3),
                                           " for ",
                                           str_to_lower(input$Hospital_Type3),
                                           " hospitals with","<br>", "clinical type ",
                                           str_to_lower(str_sub(input$Clinical_Type3,start = 1,end = 1)),
                                           str_sub(input$Clinical_Type3,start = 2),
                                           " by drug type ",
                                           str_to_lower(input$Substances3),"<br>",
                                           " by age group and sex in Scotland for ",
                                           input$Financial_Year),"<b>")),
              
              
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
                  abs(age_sex_year_new_axis()$value)
                )
                * 110 / 100),
                round(max(
                  abs(age_sex_year_new_axis()$value)
                )
                * 110 / 100)
                ),
                tickangle = 0,
                tickvals = c(
                  -round(max(abs(
                    age_sex_year_new_axis()$value
                  )),
                  ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1)), 
                  
                  -round(max(abs(
                    age_sex_year_new_axis()$value
                  )),
                  ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))/2
                  ,
                  
                  0,
                  round(max(abs(
                    age_sex_year_new_axis()$value
                  )),
                  ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))/2,
                  
                  round(max(abs(
                    age_sex_year_new_axis()$value
                  )),
                  ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))
                ),
                ticktext = paste0(
                  formatC(
                  c(
                    round(max(abs(
                      age_sex_year_new_axis()$value
                    )),
                    ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1)),
                    
                    round(max(abs(
                      age_sex_year_new_axis()$value
                    )),
                    ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))/2,
                    
                    0,
                    
                    round(max(abs(
                      age_sex_year_new_axis()$value
                    )),
                    ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))/2,
                    
                    round(max(abs(
                      age_sex_year_new_axis()$value
                    )),
                    ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))
               
                    )
                  ,
                  big.mark=",")
                  )
                  
                ,
                
                #Make the x axis title reactive.
                
                title =  ifelse(input$Measure3 == "Rate",
                                  "EASR per 100,000 population",
                                  input$Measure3
                ),
                
                showline = TRUE,
                ticks = "outside"
                
              ),
              annotations = 
                list(x = 0.99, y = -0.21, 
                     text = paste0("Source: Drug-Related","<br>",
                                   "Hospital Statistics,","<br>",
                                   "ISD Scotland (",format(Sys.Date(), "%Y"),")"), 
                     showarrow = F, xref='paper', yref='paper', 
                     xanchor='left', yanchor='auto', xshift=0, yshift=0,
                     font=list(family = "arial", size=12, color="#7f7f7f")),
      
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
          }
        })
        
        #we can now add in the table for the bar chart- values
        #have to be reassigned to positive values
        age_sex_year_table <- reactive({
          age_sex_tornado %>%
            filter(
              year %in% input$Financial_Year
              & hospital_type %in% input$Hospital_Type3
              & clinical_type %in% input$Clinical_Type3
              & activity_type %in% input$Activity_Type3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
            ) %>%
            select(-measure) %>%
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
              "Hospital type",
              "Clinical type",
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
                "Hospital type",
                "Clinical type",
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
            inputId = "Clinical_Type4",
            label = "Select clinical type",
            choices = clinical_types
          )
        })
        
        output$SIMD_substance <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Substances4",
            label = "Select drug type",
            choices = (if (input$Clinical_Type4 == "Overdose (OD)")
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
              hospital_type %in% input$Hospital_Type4
              & clinical_type %in% input$Clinical_Type4
              & activity_type %in% input$Activity_Type4
              & drug_type %in% input$Substances4
              & measure %in% input$Measure4
              #and the year options
              & year %in% input$Financial_Year2
            )%>%
            select(year, hospital_type, clinical_type, activity_type,
                    drug_type,simd, value)
        })
        
        
        
        #Create the main body of the chart.
        output$SIMD_plot <- renderPlotly({
          
          
          if (input$Financial_Year2 %in% financial_years[1:10]
              & input$Activity_Type4 == "New patients")
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Data are not available for new patients before 
              2006/07", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, collaborate = F, editable = F) 
            
          }
          
          else {
          
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
            formatC(SIMD_new()$value, big.mark = ",")
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
            height = 500
          ) %>%
            
            #Make the graph title reactive.
            
            layout(title = (paste0("<b>",paste0(str_sub(input$Activity_Type4,1,-2),
                                    " ", 
                                    str_to_lower(input$Measure4),
                                    " for ",
                                    str_to_lower(input$Hospital_Type4),
                                    " hospitals with","<br>", "clinical type ",
                                    str_to_lower(str_sub(input$Clinical_Type4,start = 1,end = 1)),
                                    str_sub(input$Clinical_Type4,start = 2),
                                    " by drug type ",
                                    str_to_lower(input$Substances4),"<br>",
                                    " by deprivation quintile in Scotland for ",
                                    input$Financial_Year2),"<b>")),
                   
                   
                   
                   
                   separators = ".,",
                   annotations = 
                     list(x = 0.97, y = -0.21, 
                          text = paste0("Source: Drug-Related","<br>",
                                        "Hospital Statistics,","<br>",
                                        "ISD Scotland (",format(Sys.Date(), "%Y"),")"), 
                          showarrow = F, xref='paper', yref='paper', 
                          xanchor='left', yanchor='auto', xshift=0, yshift=0,
                          font=list(family = "arial", size=12, color="#7f7f7f")),    #Also, wrap the y axis title in blank spaces so it doesn't...
                   #overlap with the y axis tick labels.
                   #Finally, make the y axis title reactive.
                   
                   yaxis = list(
                     
                     exponentformat = "none",
                     
                     separatethousands = TRUE,
                     
                     range = c(0, max(SIMD_new()$value, na.rm = TRUE) + 
                                 (max(SIMD_new()$value, na.rm = TRUE) 
                                  * 10 / 100)), 
                     
                     title = ifelse(input$Measure4 == "Rate",
                                    paste0(c(
                                      rep("&nbsp;", 20),
                                      "EASR per 100,000 population",
                                      rep("&nbsp;", 20),
                                      rep("\n&nbsp;", 3)
                                    ),
                                    collapse = ""),
                                    paste0(c(
                                      rep("&nbsp;", 20),
                                      input$Measure4,
                                      rep("&nbsp;", 20),
                                      rep("\n&nbsp;", 3)
                                    ),
                                    collapse = "")
                     ),
                     showline = TRUE, 
                     ticks = "outside"
                     
                   ),
                   
                   #Set the tick angle to minus 45. It's the only way for the x...
                   #axis tick labels (fin. years) to display without overlapping...
                   #with each other.
                   #Wrap the x axis title in blank spaces so that it doesn't...
                   #overlap with the x axis tick labels.
                   
                   xaxis = list( 
                                title = paste0("<br>",
                                               "<br>",
                                               "Deprivation quintile"),
                                showline = TRUE, 
                                ticks = "outside"),
                   
                   #Fix the margins so that the graph and axis titles have enough...
                   #room to display nicely.
                   #Set the font sizes.
                   
                   margin = list(l = 90, r = 100, b = 70, t = 90),
                   font = list(size = 13),
                   titlefont = list(size = 15),
                   
                   #Insert a legend so that the user knows which colour...
                   #corresponds to which location of treatment.
                   #Make the legend background and legend border white.              
                   
                   ##REMOVE LEGEND FOR NOW- until we have discussed whether 
                   #to have multiple options for any categories
                   showlegend = FALSE,
                   legend = list(
                                 bgcolor = 'rgba(0, 0, 0, 0)', 
                                 bordercolor = 'rgba(255, 255, 255, 0)')) %>%
            
            #Remove unnecessary buttons from the modebar.
            
            config(displayModeBar = TRUE,
                   modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                                 'zoomOut2d', 'autoScale2d', 
                                                 'toggleSpikelines', 
                                                 'hoverCompareCartesian', 
                                                 'hoverClosestCartesian'), 
                   displaylogo = F, collaborate = F, editable = F)
          }
        })
        
        
        #Table
        output$SIMD_table <- renderDataTable({
          datatable(
            SIMD_new(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Hospital type",
              "Clinical type",
              "Activity type",
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
              SIMD_new(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial year",
                "Hospital type",
                "Clinical type",
                "Activity type",
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
      
        #On to the final tab, which is the Table tab.
        #The following piece of syntax tells R to switch between files...
        #according to the user's input in the filter SELECT DATA FILE.
        #The files below are the ones we read into R at the very beginning.
        #However, they require a few transformations before they can be displayed...
        #as a table.
        
        data_table <- reactive({
          switch(input$table_filenames,
                 "Time trend (Data explorer)" = time_trend %>%
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Clinical type" = clinical_type,
                          "Activity type" = activity_type,
                          "Location type" = geography_type, 
                          "Location" = geography, 
                          "Drug type" = drug_type,
                          "Measure" = measure,
                          "Value" = value) ,
                 "Age/sex (Data explorer)" = age_sex %>%
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Clinical type" = clinical_type,
                          "Activity type" = activity_type,
                          "Drug type" = drug_type,
                          "Age group" = age_group,
                          "Sex" = sex,
                          "Measure" = measure,
                          "Value" = value),
                 "Deprivation (Data explorer)" = deprivation %>%
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Clinical type" = clinical_type,
                          "Activity type" = activity_type,
                          "Drug type" = drug_type,
                          "Deprivation" = simd,
                          "Measure" = measure,
                          "Value" = value),
                 "Activity summary (Trend data)" = activity_summary %>% 
                 rename("Financial year" = year, 
                        "Hospital type" = hospital_type,
                        "Clinical type" = clinical_type,
                        "Activity type" = activity_type,
                        "Location type" = geography_type, 
                        "Location" = geography, 
                        "Rate" = value),
                 "Drug summary (Trend data)" = drug_summary %>% 
                 rename("Financial year" = year, 
                        "Hospital type" = hospital_type,
                        "Clinical type" = clinical_type,
                        "Location type" = geography_type, 
                        "Location" = geography, 
                        "Drug type" = drug_type,
                        "Rate" = value),
                 "Demographic summary (Trend data)" = demographic_summary %>% 
                 rename("Financial year" = year, 
                        "Hospital type" = hospital_type,
                        "Clinical type" = clinical_type,
                        "Location type" = geography_type, 
                        "Location" = geography, 
                        "Age group" = age_group,
                        "Sex" = sex,
                        "Deprivation index" = simd,
                        "Rate" = value),
                 "Length of stay" = length_of_stay %>% 
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Clinical type" = clinical_type,
                          "Location type" = geography_type, 
                          "Location" = geography, 
                          "Drug type" = drug_type,
                          "Age group" = age_group,
                          "Sex" = sex,
                          "Deprivation index" = simd,
                          "Number of stays" = total, 
                          "<1 week (%)" = perc_less_1week,
                          ">1 week (%)" = perc_more_1week,
                          "Median length of stay" = med_los
                   ),
                 "Emergency admissions" = emergency_admissions %>% 
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Clinical type" = clinical_type,
                          "Location type" = geography_type, 
                          "Location" = geography, 
                          "Drug type" = drug_type,
                          "Age group" = age_group,
                          "Sex" = sex,
                          "Deprivation index" = simd,
                          "Number of stays" = total,
                          "Emergency admissions (%)" = perc_adm_emer,
                          "Non-emergency admissions (%)" = perc_adm_other
                          
                     
                   ),
                 "Drug type by hospital" = drug_type_by_hospital %>% 
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Clinical type" = clinical_type,
                          "Activity type" = activity_type, 
                          "Drug type" = drug_type,
                          "SMR01 (%)" = perc_source01,
                          "SMR04 (%)" = perc_source04,
                          "SMR01 and SMR04 (%)" = perc_sourceBOTH, 
                          "Number" = total
                   )
                 
                 
                 
          )
        })
        
        
        #Create the actual table for the Table tab.
        
        output$table_tab <- renderDataTable({
          datatable(data_table(), 
                    style = 'bootstrap', 
                    class = 'table-bordered table-condensed',
                    rownames = FALSE, 
                    options = list(
                      pageLength = 20, 
                      autoWidth = TRUE, 
                      dom = 'tip'),
                    
                    #Insert filters at the top of each column.
                    
                    filter = list(position = 'top'))
        }) 
        
        #We also create a download button for the table tab.
        
        output$download_table <- downloadHandler(
          filename = 'table_data.csv', 
          content = function(file) { 
            write.csv(
              
              #The command "input[["table_tab_rows_all"]]" tells R to create a CSV...
              #file that takes into account the user's input in the filters below...
              #the column names.
              
              data_table()[input[["table_tab_rows_all"]], ], 
              file, 
              row.names = FALSE
            )
          } 
        )
        
        
        #glossary link for introduction page
        
        output$download_glossary <- downloadHandler(
          filename = 'glossary.pdf',
          content = function(file) {
            file.copy(paste0(path, "www\\glossary.pdf"), file)
          }
        )
      
      #End of server
    }
  
  #End of script
}

shinyApp(ui = ui, server = server)



