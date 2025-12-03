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
      # ---------------- TAB 1: ABOUT ----------------
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
            title = "Filters for Treatment Group Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            checkboxGroupInput(
              "about_trtplot_sex",
              "Select sex group(s):",
              choices  = sort(unique(dig.df$SEX)),
              selected = sort(unique(dig.df$SEX))
            )
          ),
          box(
            title = "Filters for Sex Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            checkboxGroupInput(
              "about_sexplot_trt",
              "Select treatment group(s):",
              choices  = trt_choices,
              selected = trt_choices
            )
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
        ),
        br(),
        fluidRow(
          box(
            title = "Age Distribution by Treatment",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("about_age_boxplot", height = 300)
          ),
          box(
            title = "BMI Distribution by Treatment",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("about_bmi_boxplot", height = 300)
          )
        )
      ),
      
      # ---------------- TAB 2: BASELINE ----------------
      tabItem(
        tabName = "baseline",
        fluidRow(
          box(
            title = "Baseline Characteristics",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("Use the filters to explore baseline age and BMI distributions by treatment and sex.")
          )
        ),
        br(),
        fluidRow(
          box(
            title = "Filters",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            checkboxGroupInput(
              "base_trt",
              "Treatment group:",
              choices  = trt_choices,
              selected = trt_choices
            ),
            checkboxGroupInput(
              "base_sex",
              "Sex (code):",
              choices  = sort(unique(dig.df$SEX)),
              selected = sort(unique(dig.df$SEX))
            ),
            sliderInput(
              "base_age",
              "Age range:",
              min   = floor(min(dig.df$AGE, na.rm = TRUE)),
              max   = ceiling(max(dig.df$AGE, na.rm = TRUE)),
              value = c(
                floor(min(dig.df$AGE, na.rm = TRUE)),
                ceiling(max(dig.df$AGE, na.rm = TRUE))
              )
            )
          ),
          box(
            title = "Age Histogram",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("base_age_hist", height = 300)
          )
        ),
        br(),
        fluidRow(
          box(
            title = "BMI by Treatment",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("base_bmi_box", height = 300)
          ),
          box(
            title = "Interpretation",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            htmlOutput("base_text")
          )
        )
      ),
      
      # ---------------- TAB 3: TRIAL (empty) ----------------
      tabItem(tabName = "trial"),
      
      # ---------------- TAB 4: OUTCOMES ----------------
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
        br(),
        fluidRow(
          valueBoxOutput("mortality_vb",           width = 3),
          valueBoxOutput("mortality_placebo_vb",   width = 3),
          valueBoxOutput("mortality_treatment_vb", width = 3)
        ),
        br(),
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
          )
        ),
        br(),
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
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_outcomes_whf", height = 500)
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  # ------------ ABOUT TAB ------------
  about_trtplot_data <- reactive({
    req(input$about_trtplot_sex)      # no selection -> no plot
    dig.df %>%
      filter(SEX %in% input$about_trtplot_sex)
  })
  
  about_sexplot_data <- reactive({
    req(input$about_sexplot_trt)
    dig.df %>%
      filter(TRTMT %in% input$about_sexplot_trt)
  })
  
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
    dat <- about_trtplot_data()
    req(nrow(dat) > 0)
    
    plt <- dat %>%
      count(TRTMT) %>%
      ggplot(aes(
        x    = TRTMT,
        y    = n,
        fill = TRTMT,
        text = paste0("Group: ", TRTMT, "<br>Patients: ", n)
      )) +
      geom_col(colour = "black") +
      scale_fill_manual(
        values = c("Placebo" = "darkblue", "Treatment" = "lightblue")
      ) +
      labs(x = "Treatment Group", y = "Number of Patients", fill = "Treatment") +
      theme_minimal()
    
    ggplotly(plt, tooltip = "text")
  })
  
  output$about_sex_plot <- renderPlotly({
    dat <- about_sexplot_data()
    req(nrow(dat) > 0)
    
    plt <- dat %>%
      count(SEX) %>%
      ggplot(aes(
        x    = as.factor(SEX),
        y    = n,
        fill = as.factor(SEX),
        text = paste0(
          "Sex code: ", SEX,
          ifelse(SEX == 1, " (Male)", " (Female)"),
          "<br>Patients: ", n
        )
      )) +
      geom_col(colour = "black") +
      scale_x_discrete(
        labels = c("1" = "1 = Male", "2" = "2 = Female")
      ) +
      scale_fill_manual(
        values = c("1" = "lightblue", "2" = "pink"),
        labels = c("1" = "Male", "2" = "Female")
      ) +
      labs(x = "Sex", y = "Number of Patients", fill = "Sex") +
      theme_minimal()
    
    ggplotly(plt, tooltip = "text")
  })
  
  output$about_baseline_table <- renderDT({
    dig.df %>%
      group_by(TRTMT) %>%
      summarise(
        n        = n(),
        mean_age = round(mean(AGE, na.rm = TRUE), 2),
        sd_age   = round(sd(AGE, na.rm = TRUE), 2),
        mean_bmi = round(mean(BMI, na.rm = TRUE), 2),
        sd_bmi   = round(sd(BMI, na.rm = TRUE), 2),
        male_n   = sum(SEX == 1, na.rm = TRUE),
        female_n = sum(SEX == 2, na.rm = TRUE),
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
  
  output$about_age_boxplot <- renderPlotly({
    p <- ggplot(dig.df, aes(x = TRTMT, y = AGE, fill = TRTMT)) +
      geom_boxplot(color = "black") +
      scale_fill_manual(values = c("Placebo" = "blue", "Treatment" = "green")) +
      labs(x = "Treatment Group", y = "Age (years)") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$about_bmi_boxplot <- renderPlotly({
    p <- ggplot(dig.df, aes(x = TRTMT, y = BMI, fill = TRTMT)) +
      geom_boxplot(color = "black") +
      scale_fill_manual(values = c("Placebo" = "orange", "Treatment" = "purple")) +
      labs(x = "Treatment Group", y = "BMI") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # ------------ BASELINE TAB ------------
  base_data <- reactive({
    dat <- dig.df
    if (!is.null(input$base_trt) && length(input$base_trt) > 0) {
      dat <- dat %>% filter(TRTMT %in% input$base_trt)
    }
    if (!is.null(input$base_sex) && length(input$base_sex) > 0) {
      dat <- dat %>% filter(SEX %in% input$base_sex)
    }
    dat <- dat %>%
      filter(AGE >= input$base_age[1],
             AGE <= input$base_age[2])
    dat
  })
  
  output$base_age_hist <- renderPlotly({
    dat <- base_data()
    req(nrow(dat) > 0)
    
    p <- ggplot(dat, aes(x = AGE, fill = TRTMT)) +
      geom_histogram(binwidth = 5, colour = "black", alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("Placebo" = "darkblue", "Treatment" = "lightblue")) +
      labs(
        x = "Age (years)",
        y = "Number of patients",
        fill = "Treatment"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$base_bmi_box <- renderPlotly({
    dat <- base_data()
    req(nrow(dat) > 0)
    
    p <- ggplot(dat, aes(x = TRTMT, y = BMI, fill = TRTMT)) +
      geom_boxplot(colour = "black") +
      scale_fill_manual(values = c("Placebo" = "darkblue", "Treatment" = "lightblue")) +
      labs(
        x = "Treatment group",
        y = "BMI (kg/m^2)",
        fill = "Treatment"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$base_text <- renderUI({
    dat <- base_data()
    req(nrow(dat) > 0)
    
    n_pat    <- nrow(dat)
    mean_age <- round(mean(dat$AGE, na.rm = TRUE), 1)
    mean_bmi <- round(mean(dat$BMI, na.rm = TRUE), 1)
    
    by_trt <- dat %>%
      group_by(TRTMT) %>%
      summarise(
        n        = n(),
        mean_bmi = round(mean(BMI, na.rm = TRUE), 1),
        .groups  = "drop"
      )
    
    txt_trt <- paste(
      by_trt$TRTMT, ": n = ", by_trt$n,
      ", mean BMI = ", by_trt$mean_bmi,
      collapse = "<br>"
    )
    
    HTML(paste0(
      "<b>Current selection:</b><br>",
      "Patients: ", n_pat, "<br>",
      "Mean age: ", mean_age, " years<br>",
      "Overall mean BMI: ", mean_bmi, "<br><br>",
      "<b>BMI by treatment:</b><br>",
      txt_trt
    ))
  })
  
  # ------------ OUTCOMES TAB ------------
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
      subtitle = "Deaths in Placebo Group",
      icon     = icon("capsules"),
      color    = "green"
    )
  })
  
  output$mortality_treatment_vb <- renderValueBox({
    valueBox(
      value    = sum(dig.df$TRTMT == "Treatment" & dig.df$DEATH == "Deceased", na.rm = TRUE),
      subtitle = "Deaths in Treatment Group",
      icon     = icon("capsules"),
      color    = "yellow"
    )
  })
  
  mortality_sub <- reactive({
    req(input$TRTMT_mortality, input$DEATH)
    dig.df %>%
      filter(TRTMT %in% input$TRTMT_mortality) %>%
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
          "Treatment Group: ", TRTMT, "<br>",
          "Patient Status: ", DEATH
        )
      )) +
      geom_bar(position = "dodge", colour = "black") +
      scale_fill_manual(values = c("Alive" = "lightyellow", "Deceased" = "darkblue")) +
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
          "Hospitalisation: ", HOSP, "<br>",
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
