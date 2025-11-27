library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)

dig.df <- read_csv("data/DIG.csv")%>%
  mutate(TRTMT = recode(TRTMT, '0' = "Placebo", `1` = "Treatment"),
         WHF = recode(WHF, '0' = "Healthy", '1'= "Worsening Heart Failure"),
         HOSP = recode(HOSP, '0' = "No Hospitalisation", '1' = "Hospitalised"))
dig.df

trt_choices  <- sort(unique(dig.df$TRTMT))
whf_choices  <- sort(unique(dig.df$WHF))
hosp_choices <- sort(unique(dig.df$HOSP))

# To determine if treatment group and WHF have an effect on hospitalizations

ui <- fluidPage(
  titlePanel("Digitalis Investigation Group (DIG) Trial Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId  = "TRTMT",
        label    = "Select Treatment Group:",
        choices  = trt_choices,
        selected = trt_choices, 
        multiple = TRUE
      ),
      selectInput(
        inputId  = "WHF",
        label    = "Select Patient Status (WHF):",
        choices  = whf_choices,
        selected = whf_choices,
        multiple = TRUE
      ),
      selectInput(
        inputId  = "HOSP",
        label    = "Select Hospitalisation Status:",
        choices  = hosp_choices,
        selected = hosp_choices,
        multiple = TRUE
      )
    ),
    
    mainPanel(
      plotOutput("plot1"),
      dataTableOutput("table1")
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
      geom_bar(position = "stack") +
      labs(
        fill = "Patient Status",
        x    = "Patient Hospitalisation Status",
        y    = "Percentage (%)"
      )
  })
  
  output$table1 <- renderDataTable({ 
    patients_sub()
  })
}

shinyApp(ui, server)