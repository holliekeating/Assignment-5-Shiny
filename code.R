library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(DT)

dig.df <- read_csv("data/DIG.csv")%>%
  mutate(TRTMT = recode(TRTMT, '0' = "Placebo", `1` = "Treatment"),
         WHF = recode(WHF, '0' = "Healthy", '1'= "Worsening Heart Failure"),
         HOSP = recode(HOSP, '0' = "No Hospitalisation", '1' = "Hospitalised"))%>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)
dig.df

trt_choices  <- sort(unique(dig.df$TRTMT))
whf_choices  <- sort(unique(dig.df$WHF))
hosp_choices <- sort(unique(dig.df$HOSP))


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
            title = "Worsening Heart Failure and Patient Status",
      selectInput(
        inputId  = "TRTMT",
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
        title = "Hospitalisation by Patient Status",
        status = "primary", solidHeader = TRUE,
        width = 8,
        plotOutput("plot1", height = 300),
        br(),
        DTOutput("table1")
      )
) 
)
)
)
)
server <- function(input, output) {
  
  patients_sub <- reactive({
    req(input$TRTMT, input$WHF, input$HOSP)
    
    dig.df %>%
      filter(TRTMT %in% input$TRTMT) %>%
      filter(WHF   %in% input$WHF)   %>%
      filter(HOSP  %in% input$HOSP)
  })
  
  output$plot1 <- renderPlot({ 
    dat <- patients_sub()
    req(nrow(dat) > 0)  
    
    dat %>%
      ggplot(aes(
        x = HOSP,
        y = after_stat(100 * count / sum(count)),
        fill = WHF
      )) +
      geom_bar(position = "stack", colour = "black") +
      scale_fill_manual(values = c("Healthy"= "lightyellow", "Worsening Heart Failure" = "lightblue"))+
      labs(
        fill = "Patient Status",
        x    = "Patient Hospitalisation Status",
        y    = "Percentage (%)",
        title = "Effect of Worsening Heart Failure on Hospitalisation Levels")+
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text =  element_text(face = "bold", size = 10)
      )
  })
  
  output$table1 <- renderDT({ 
    patients_sub()
  })
}

shinyApp(ui, server)