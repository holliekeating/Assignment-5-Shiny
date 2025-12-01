library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(DT)
library(plotly)

dig.df <- read_csv("data/DIG.csv") %>%
  mutate(
    TRTMT = recode(TRTMT, "0" = "Placebo", "1" = "Treatment"),
    WHF   = recode(WHF,   "0" = "Healthy", "1" = "Worsening Heart Failure"),
    HOSP  = recode(HOSP,  "0" = "No Hospitalisation", "1" = "Hospitalised"),
    DEATH = recode(DEATH, "0" = "Alive", "1" = "Deceased")
  ) %>%
  select(
    ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT,
    DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG,
    HOSP, HOSPDAYS, DEATH, DEATHDAY
  )

dig.df

trt_choices   <- sort(unique(dig.df$TRTMT))
whf_choices   <- sort(unique(dig.df$WHF))
hosp_choices  <- sort(unique(dig.df$HOSP))
death_choices <- sort(unique(dig.df$DEATH))

ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About DIG Study",          tabName = "about"),
      menuItem("Baseline Characteristics", tabName = "baseline"),
      menuItem("Patient Outcomes",         tabName = "outcomes"),
      menuItem("Trial Outcomes",           tabName = "trial")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "about",
        
        fluidRow(
          box(
            title = "About the DIG Study",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("The Digitalis Investigation Group (DIG) trial evaluated the effect of digoxin compared with placebo in patients with heart failure. This tab summarises the study purpose, treatment groups, and basic features of the analysis dataset.")
          )
        ),
        
        br(),
        
        fluidRow(
          valueBoxOutput("vb_total_patients", width = 3),
          valueBoxOutput("vb_digoxin",        width = 3),
          valueBoxOutput("vb_placebo",        width = 3),
          valueBoxOutput("vb_avg_age",        width = 3)
        ),
        
        br(),
        
        fluidRow(
          box(
            title = "Dataset Information",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            p(paste0(
              "The analysis dataset contains ", nrow(dig.df),
              " patients and ", ncol(dig.df),
              " variables derived from the DIG trial case report forms."
            )),
            p("Variables include demographics (age, sex), clinical measurements (BMI, blood pressure, creatinine, potassium), comorbidities (hypertension, cardiovascular disease), and outcomes (worsening heart failure, hospitalisation, death).")
          )
        ),
        
        br(),
        
        fluidRow(
          box(
            title = "Key Baseline Characteristics by Treatment",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("about_baseline_table")
          )
        ),
        
        br(),
        
        fluidRow(
          box(
            title = "Treatment Group Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("about_treatment_plot", height = 300)
          ),
          box(
            title = "Sex Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("about_sex_plot", height = 300)
          )
        ),
        
        br(),
        
        fluidRow(
          box(
            title = "Age Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("about_age_hist", height = 300)
          ),
          box(
            title = "Data Summary and Preview",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tabsetPanel(
              tabPanel("Summary", verbatimTextOutput("about_summary")),
              tabPanel("Data Preview", DTOutput("about_preview"))
            )
          )
        )
      ),
      
      tabItem(
        tabName = "baseline"
      ),
      
      tabItem(
        tabName = "trial"
      ),
      
      tabItem(
        tabName = "outcomes",
        
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
        
        fluidRow(
          box(
            title = "Filter Box - Rate of Mortality between Groups",
            status = "primary", solidHeader = TRUE,
            width = 4,
            selectInput(
              inputId  = "TRTMT",
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
          )
        ),
        
        fluidRow(
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
  
  output$vb_total_patients <- renderValueBox({
    valueBox(
      value    = nrow(dig.df),
      subtitle = "Total Patients",
      icon     = icon("users"),
      color    = "blue"
    )
  })
  
  output$vb_digoxin <- renderValueBox({
    valueBox(
      value    = sum(dig.df$TRTMT == "Treatment", na.rm = TRUE),
      subtitle = "Digoxin Group",
      icon     = icon("capsules"),
      color    = "green"
    )
  })
  
  output$vb_placebo <- renderValueBox({
    valueBox(
      value    = sum(dig.df$TRTMT == "Placebo", na.rm = TRUE),
      subtitle = "Placebo Group",
      icon     = icon("vial"),
      color    = "yellow"
    )
  })
  
  output$vb_avg_age <- renderValueBox({
    valueBox(
      value    = round(mean(dig.df$AGE, na.rm = TRUE), 1),
      subtitle = "Average Age (years)",
      icon     = icon("user-clock"),
      color    = "purple"
    )
  })
  
  output$about_treatment_plot <- renderPlotly({
    plt <- dig.df %>%
      count(TRTMT) %>%
      ggplot(aes(
        x    = TRTMT,
        y    = n,
        text = paste0("Group: ", TRTMT, "<br>Patients: ", n)
      )) +
      geom_col(fill = "steelblue") +
      labs(x = "Treatment Group", y = "Number of Patients") +
      theme_minimal()
    ggplotly(plt, tooltip = "text")
  })
  
  output$about_sex_plot <- renderPlotly({
    plt <- dig.df %>%
      count(SEX) %>%
      ggplot(aes(
        x    = SEX,
        y    = n,
        text = paste0("Sex: ", SEX, "<br>Patients: ", n)
      )) +
      geom_col(fill = "darkorange") +
      labs(x = "Sex", y = "Number of Patients") +
      theme_minimal()
    ggplotly(plt, tooltip = "text")
  })
  
  output$about_baseline_table <- renderDT({
    dig.df %>%
      group_by(TRTMT) %>%
      summarise(
        n        = n(),
        mean_age = round(mean(AGE, na.rm = TRUE), 1),
        mean_bmi = round(mean(BMI, na.rm = TRUE), 1),
        .groups  = "drop"
      )
  })
  
  output$about_age_hist <- renderPlotly({
    plt <- ggplot(dig.df, aes(x = AGE)) +
      geom_histogram(binwidth = 5, fill = "steelblue", colour = "white") +
      labs(x = "Age (years)", y = "Count", title = "Age Distribution of Patients") +
      theme_minimal()
    ggplotly(plt)
  })
  
  output$about_summary <- renderPrint({
    summary(select_if(dig.df, is.numeric))
  })
  
  output$about_preview <- renderDT({
    head(dig.df, 20)
  })
  
  mortality_sub <- reactive({
    req(input$TRTMT, input$DEATH)
    dig.df %>%
      filter(TRTMT %in% input$TRTMT) %>%
      filter(DEATH  %in% input$DEATH)
  })
  
  output$plot_outcomes_mortality <- renderPlotly({
    dat <- mortality_sub()
    req(nrow(dat) > 0)
    
    plot_outcomes_mortality1 <-
      dat %>%
      ggplot(aes(
        x    = TRTMT,
        y    = after_stat(100 * count / sum(count)),
        fill = DEATH,
        text = paste0(
          "Treatment Group: ", TRTMT, "",
          "Patient Status: ", DEATH
        )
      )) +
      geom_bar(position = "dodge", colour = "black") +
      scale_fill_manual(values = c("Alive" = "lightyellow",
                                   "Deceased" = "darkblue")) +
      labs(
        fill = "Patient Status",
        x    = "Patient Mortality Status",
        y    = "Percentage (%)"
      ) +
      theme(
        plot.title  = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text    = element_text(face = "bold", size = 10)
      )
    
    ggplotly(plot_outcomes_mortality1, tooltip = "text")
  })
  
  output$table_outcomes_mortality <- renderDT({
    mortality_sub()
  })
  
  patients_sub <- reactive({
    req(input$TRTMT_whf, input$WHF, input$HOSP)
    dig.df %>%
      filter(TRTMT %in% input$TRTMT_whf) %>%
      filter(WHF   %in% input$WHF) %>%
      filter(HOSP  %in% input$HOSP)
  })
  
  output$plot_outcomes_whf <- renderPlotly({
    dat <- patients_sub()
    req(nrow(dat) > 0)
    
    plot_outcomes_whf1 <-
      dat %>%
      ggplot(aes(
        x    = HOSP,
        y    = after_stat(100 * count / sum(count)),
        fill = WHF,
        text = paste0(
          "Hospitalisation: ", HOSP, "",
          "Patient Status: ", WHF
        )
      )) +
      geom_bar(position = "dodge", colour = "black") +
      scale_fill_manual(values = c("Healthy" = "lightyellow",
                                   "Worsening Heart Failure" = "lightblue")) +
      labs(
        fill = "Patient Status",
        x    = "Patient Hospitalisation Status",
        y    = "Percentage (%)"
      ) +
      theme(
        plot.title  = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text    = element_text(face = "bold", size = 10)
      )
    
    ggplotly(plot_outcomes_whf1, tooltip = "text")
  })
}

shinyApp(ui, server)

