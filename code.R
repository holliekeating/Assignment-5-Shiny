library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(DT)
library(plotly)

dig.df <- read_csv("data/DIG.csv")%>%
  mutate(TRTMT = recode(TRTMT, '0' = "Placebo", `1` = "Treatment"),
         WHF = recode(WHF, '0' = "Healthy", '1'= "Worsening Heart Failure"),
         HOSP = recode(HOSP, '0' = "No Hospitalisation", '1' = "Hospitalised"),
         DEATH = recode(DEATH, '0' = "Alive", '1' = "Deceased"))%>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)
dig.df

trt_choices  <- sort(unique(dig.df$TRTMT))
whf_choices  <- sort(unique(dig.df$WHF))
hosp_choices <- sort(unique(dig.df$HOSP))
death_choices <- sort(unique(dig.df$DEATH))


ui <- dashboardPage(
  
  dashboardHeader(title = "DIG Trial Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About DIG Study", tabName = "about"),
      menuItem("Baseline Characteristics", tabName = "baseline"),
      menuItem("Patient Outcomes", tabName = "outcomes"),
      menuItem("Trial Outcomes", tabName = "trial")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "about", 
        # To be developed
        # Box: What the trial is about
        # no of patients in each treatment group
      ),
      tabItem(
        tabName = "baseline"
        # To be developed
      ),
      tabItem(
        tabName = "trial"
        # To be developed
        # Risk of mortality over time
      ),
      
      tabItem(
        tabName = "outcomes",
        # Rate of mortality (treatment groups, WHF, CVD)
        # Rate of hospitalizations (treatment groups, WHF, CVD)
        
        
        fluidRow(
          box(
            title = "Patient Outcomes",
            status = "primary",
            width = 10,
            solidHeader = TRUE,
            p("This section explores patient outcomes in the Digitalis Investigation Group (DIG) Trial. 
              Use the filters to explore the rate of mortality and hospitalisations between groups")
          )
        ),
      
      br(),
      
      fluidRow(
        valueBoxOutput("mortality_vb", width = 3),
        valueBoxOutput("mortality_placebo_vb", width = 3),
        valueBoxOutput("mortality_treatment_vb", width = 3)
      ),
      
      br(),
          
# Rate of mortality between groups
      fluidRow(
        box(
        title = "Filter Box - Rate of Mortality between Groups",
        status = "primary", solidHeader = TRUE,
        width = 4,
        selectInput(
          inputId  = "TRTMT_mortality",
          label    = "Select Treatment Group:",
          choices  = trt_choices,
          selected = trt_choices, 
          multiple = TRUE
        ),
        checkboxGroupInput(
          inputId  = "DEATH",
          label    = "Select Patient Status:",
          choices  = death_choices,
          selected = death_choices
      )
),

      box(
        title = "Rate of Mortality between Groups",
        status = "primary", solidHeader = TRUE,
        width = 6,
        plotlyOutput("plot_outcomes_mortality", height = 500),
        br()
      ),
          
 # Worsening heart failure          
      box(
        title = "Filter Box - Worsening Heart Failure and Hospitalisation Status",
        status = "primary", solidHeader = TRUE,
        width = 4,
      selectInput(
        inputId  = "TRTMT_whf",
        label    = "Select Treatment Group:",
        choices  = trt_choices,
        selected = trt_choices, 
        multiple = TRUE
      ),
      checkboxGroupInput(
        inputId  = "WHF",
        label    = "Select Patient Status:",
        choices  = whf_choices,
        selected = whf_choices
      ),
      checkboxGroupInput(
        inputId  = "HOSP",
        label    = "Select Hospitalisation Status:",
        choices  = hosp_choices,
        selected = hosp_choices
      )
),
      box(
        title = "Worsening Heart Failure and Hospitalisation Status",
        status = "primary", solidHeader = TRUE,
        width = 6,
        plotlyOutput("plot_outcomes_whf", height = 500)
      )
) 
)
)
)
)

server <- function(input, output) {

# Patient outcomes: Rate of mortality between groups
  
  output$mortality_vb <- renderValueBox({
    valueBox(
      value    = sum(dig.df$DEATH == "Deceased", na.rm = TRUE),
      subtitle = "Total Deaths",
      icon     = icon("users"),
      color    = "blue"
    )
  })
  
  output$mortality_placebo_vb <- renderValueBox({
    valueBox(
      value    = sum(dig.df$TRTMT == "Placebo" & dig.df$DEATH == "Deceased", na.rm = TRUE),
      subtitle = "Mortality Rate in Placebo Group",
      icon     = icon("capsules"),
      color    = "green"
    )
  })
  
  output$mortality_treatment_vb <- renderValueBox({
    valueBox(
      value    = sum(dig.df$TRTMT == "Treatment" & dig.df$DEATH == "Deceased", na.rm = TRUE),
      subtitle = "Mortality Rate in Treatment Group",
      icon     = icon("capsules"),
      color    = "yellow"
    )
  })
  
  mortality_sub <- reactive({
    req(input$TRTMT_mortality, input$DEATH)
    
    dig.df %>%
      filter(TRTMT %in% input$TRTMT_mortality) %>%
      filter(DEATH   %in% input$DEATH)
  })
  
  output$plot_outcomes_mortality <- renderPlotly({ 
    dat <- mortality_sub()
    req(nrow(dat) > 0)  
    
    plot_outcomes_mortality1 <- 
     dat %>%
      ggplot(aes(
        x = TRTMT,
        y = after_stat(100 * count / sum(count)),
        fill = DEATH,
        text = paste0(
          "Treatment Group: ", TRTMT, "<br>",
          "Patient Status: ", DEATH
      ))) +
      geom_bar(position = "dodge", colour = "black") +
      scale_fill_manual(values = c("Alive"= "lightyellow", "Deceased" = "darkblue"))+
      labs(
        fill = "Patient Status",
        x    = "Patient Mortality Status",
        y    = "Percentage (%)")+
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text =  element_text(face = "bold", size = 10)
      )
    ggplotly(plot_outcomes_mortality1, tooltip = "text")
  })
  

# Patient outcomes: rate of hospitalisations and WHF between groups   
  patients_sub <- reactive({
    req(input$TRTMT_whf, input$WHF, input$HOSP)
    
    dig.df %>%
      filter(TRTMT %in% input$TRTMT_whf) %>%
      filter(WHF   %in% input$WHF)   %>%
      filter(HOSP  %in% input$HOSP)
  })
  output$plot_outcomes_whf <- renderPlotly({ 
    dat <- patients_sub()
    req(nrow(dat) > 0)  
    
  plot_outcomes_whf1 <- 
    dat %>%
      ggplot(aes(
        x = HOSP,
        y = after_stat(100 * count / sum(count)),
        fill = WHF,
        text = paste0(
          "Hospitalisation: ", HOSP, "<br>",
          "Patient Status: ", WHF
      ))) +
      geom_bar(position = "dodge", colour = "black") +
      scale_fill_manual(values = c("Healthy"= "lightyellow", "Worsening Heart Failure" = "lightblue"))+
      labs(
        fill = "Patient Status",
        x    = "Patient Hospitalisation Status",
        y    = "Percentage (%)")+
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text =  element_text(face = "bold", size = 10)
      )
    ggplotly(plot_outcomes_whf1, tooltip = "text")
  })
}

shinyApp(ui, server)