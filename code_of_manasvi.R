library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

## ===== 1. Data import and preprocessing =====
dig_raw <- read.csv("data/DIG.csv")

dig <- dig_raw %>%
  mutate(
    treatment   = factor(TRTMT, levels = c(0, 1),
                         labels = c("Placebo", "Digoxin")),
    sex         = factor(SEX,  levels = c(1, 2),
                         labels = c("Male", "Female")),
    race        = factor(RACE, levels = c(1, 2),
                         labels = c("White", "Nonwhite")),
    age         = AGE,
    bmi         = BMI,
    ejf_per     = EJF_PER,
    klevel      = KLEVEL,
    creatinine  = CREAT,
    sbp         = SYSBP,
    dbp         = DIABP,
    death       = DEATH,
    deathday    = DEATHDAY,
    hosp        = HOSP,
    hospdays    = HOSPDAYS,
    nhosp       = NHOSP
  )

validate_data <- function(d) {
  validate(need(nrow(d) > 0, "No data available for current filters. Please adjust."))
}

## ===== 2. UI =====

ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("About",      tabName = "about",    icon = icon("info-circle")),
      menuItem("Baseline",   tabName = "baseline", icon = icon("chart-bar")),
      menuItem("Outcomes",   tabName = "clinical", icon = icon("heartbeat")),
      menuItem("Variable explorer", tabName = "relations", icon = icon("project-diagram")),
      menuItem("Patient explorer",  tabName = "patients",  icon = icon("users")),
      menuItem("Documentation",     tabName = "docs",      icon = icon("book"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      ## ======= Tab 1: About ======
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title  = "DIG Trial Overview",
            p("The DIG (Digitalis Investigation Group) trial was a randomized, placebo-controlled study evaluating digoxin in patients with heart failure."),
            p("This tab summarises the study purpose, treatment groups, and basic features of the analysis dataset.")
          )
        ),
        br(),
        fluidRow(
          valueBoxOutput("vb_total",   width = 4),
          valueBoxOutput("vb_digoxin", width = 4),
          valueBoxOutput("vb_placebo", width = 4)
        ),
        br(),
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Dataset Information",
            p(paste0(
              "The analysis dataset contains ", nrow(dig),
              " patients and ", ncol(dig_raw),
              " variables derived from the DIG trial case report forms."
            )),
            p("Variables include demographics (age, sex, race), clinical measurements (BMI, blood pressure, creatinine, potassium, ejection fraction), and outcomes (hospitalisation and death).")
          )
        ),
        br(),
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Key Baseline Characteristics by Treatment",
            DTOutput("about_baseline_table")
          )
        ),
        br(),
        fluidRow(
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Treatment Group Distribution",
            checkboxGroupInput(
              "about_trtplot_sex",
              "Include sex group(s):",
              choices  = levels(dig$sex),
              selected = levels(dig$sex)
            ),
            plotOutput("about_treatment_plot", height = 300)
          ),
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Sex Distribution",
            checkboxGroupInput(
              "about_sexplot_trt",
              "Include treatment group(s):",
              choices  = levels(dig$treatment),
              selected = levels(dig$treatment)
            ),
            plotOutput("about_sex_plot", height = 300)
          )
        ),
        br(),
        fluidRow(
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Age Distribution",
            plotOutput("about_age_hist", height = 300)
          ),
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Data Summary and Preview",
            tabsetPanel(
              tabPanel("Summary", verbatimTextOutput("about_summary")),
              tabPanel("Data Preview", DTOutput("about_preview"))
            )
          )
        ),
        br(),
        fluidRow(
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Age Distribution by Treatment",
            plotOutput("about_age_boxplot", height = 300)
          ),
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "BMI Distribution by Treatment",
            plotOutput("about_bmi_boxplot", height = 300)
          )
        ),
        br(),
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Guide to Other Tabs",
            HTML("<b>Baseline tab:</b> Explore baseline variables by treatment with filters and interactive plots."),
            tags$br(), tags$br(),
            HTML("<b>Outcomes tab:</b> View mortality and hospitalisation patterns with filters for age, sex and EF."),
            tags$br(), tags$br(),
            HTML("<b>Variable explorer:</b> Interactive scatterplots with X/Y variable selection, grouping and correlation."),
            tags$br(), tags$br(),
            HTML("<b>Patient explorer:</b> Patient-level table with filters for treatment, sex, age and status."),
            tags$br(), tags$br(),
            HTML("<b>Documentation:</b> Detailed explanation of the app, tabs and team contributions.")
          )
        )
      ),
      
      ## ======== Tab 2: Baseline ======
      tabItem(
        tabName = "baseline",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Baseline Characteristics",
            p("Use the filters to explore baseline age, BMI and other variables by treatment, sex and race. All plots and tables update based on your selection.")
          )
        ),
        br(),
        fluidRow(
          box(
            width = 3, status = "primary", solidHeader = TRUE,
            title = "Filters",
            selectInput("b_sex", "Sex",
                        choices = c("All", levels(dig$sex))),
            selectInput("b_race", "Race",
                        choices = c("All", levels(dig$race))),
            sliderInput("b_age", "Age range",
                        min = floor(min(dig$age, na.rm = TRUE)),
                        max = ceiling(max(dig$age, na.rm = TRUE)),
                        value = c(40, 80), step = 1),
            checkboxGroupInput(
              "b_trt", "Treatment",
              choices  = levels(dig$treatment),
              selected = levels(dig$treatment)
            )
          ),
          
          box(
            width = 9, status = "primary", solidHeader = TRUE,
            title = "Baseline graphs",
            tabsetPanel(
              tabPanel(
                "Age histogram",
                plotOutput("plot_baseline_age")
              ),
              tabPanel(
                "BMI by treatment",
                plotOutput("plot_baseline_bmi"),
                br(),
                htmlOutput("base_text")
              ),
              tabPanel(
                "Sex proportion by treatment",
                plotOutput("base_sex_prop")
              ),
              tabPanel(
                "Custom variable",
                fluidRow(
                  column(
                    6,
                    selectInput(
                      "b_var", "Variable",
                      choices = c("Age" = "age",
                                  "BMI" = "bmi",
                                  "Ejection fraction (%)" = "ejf_per",
                                  "Potassium (KLEVEL)" = "klevel",
                                  "Creatinine" = "creatinine",
                                  "Systolic BP" = "sbp",
                                  "Diastolic BP" = "dbp")
                    )
                  ),
                  column(
                    6,
                    radioButtons("b_plot_type", "Plot type",
                                 choices = c("Histogram" = "hist",
                                             "Boxplot"   = "box"),
                                 inline = TRUE)
                  )
                ),
                plotOutput("plot_baseline_var")
              )
            )
          )
        ),
        br(),
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "Age–BMI correlation",
            htmlOutput("base_corr_text")
          ),
          box(
            width = 8, status = "primary", solidHeader = TRUE,
            title = "Baseline summary table (filtered)",
            DTOutput("tbl_baseline")
          )
        )
      ),
      
      ##=========Tab3: Clinical Outcomes (unchanged)=========
      tabItem(
        tabName = "clinical",
        fluidRow(
          box(
            width =3, status = "primary", solidHeader = TRUE,
            title = "Filters",
            selectInput("c_sex", "Sex",
                        choices = c("All", levels(dig$sex))),
            sliderInput("c_age", "Age range",
                        min = floor(min(dig$age, na.rm = TRUE)),
                        max = ceiling(max(dig$age, na.rm = TRUE)),
                        value = c(40,80), step = 1),
            selectInput("c_ef", "EF category",
                        choices = c("All", "Low EF", "Mid-range EF", "Preserved EF"))
          ),
          
          box(
            width = 9, status = "primary", solidHeader = TRUE, 
            title = "Clinical outcomes",
            tabsetPanel(
              tabPanel("Mortality by treatment",
                       DTOutput("tbl_mort"),
                       plotOutput("plot_mort")),
              tabPanel("Hospitalisation",
                       DTOutput("tbl_hosp"),
                       plotOutput("plot_hosp")),
              tabPanel("Days to death (histogram)",
                       plotOutput("plot_deathday"))
            )
          )
        )
      ),
      
      ##========== Tab4: Variable explorer (unchanged)=======
      tabItem(
        tabName = "relations",
        fluidRow(
          box(
            width = 3, status = "primary", solidHeader = TRUE,
            title = "Controls (Xiangqi)",
            
            selectInput(
              inputId = "r_x",
              label   = "X variable",
              choices = c("Age"               = "age",
                          "BMI"               = "bmi",
                          "Ejection fraction" = "ejf_per",
                          "Potassium"         = "klevel",
                          "Creatinine"        = "creatinine",
                          "Systolic BP"       = "sbp",
                          "Diastolic BP"      = "dbp")
            ),
            
            selectInput(
              inputId = "r_y",
              label   = "Y variable",
              choices = c("Ejection fraction" = "ejf_per",
                          "BMI"               = "bmi",
                          "Potassium"         = "klevel",
                          "Creatinine"        = "creatinine",
                          "Systolic BP"       = "sbp",
                          "Diastolic BP"      = "dbp")
            ),
            
            selectInput(
              inputId = "r_col",
              label   = "Colour / Group by",
              choices = c("None"      = "none",
                          "Treatment" = "treatment",
                          "Sex"       = "sex",
                          "Race"      = "race")
            ),
            
            checkboxInput(
              inputId = "r_smooth",
              label   = "Add linear trend line",
              value   = TRUE
            )
          ),
          
          box(
            width = 9, status = "primary", solidHeader = TRUE,
            title = "Scatterplot & Correlation",
            plotOutput("plot_rel"),
            br(),
            strong("Pearson correlation: "),
            textOutput("txt_cor")
          )
        )
      ),
      
      ##========= Tab5: Patient explorer (unchanged) ========
      tabItem(
        tabName = "patients",
        fluidRow(
          box(
            width = 3, status = "primary", solidHeader = TRUE,
            title = "Filters (Xiangqi)",
            
            selectInput(
              inputId = "p_trt",
              label   = "Treatment",
              choices = c("All", levels(dig$treatment))
            ),
            
            selectInput(
              inputId = "p_sex",
              label   = "Sex",
              choices = c("All", levels(dig$sex))
            ),
            
            sliderInput(
              inputId = "p_age",
              label   = "Age range",
              min     = floor(min(dig$age, na.rm = TRUE)),
              max     = ceiling(max(dig$age, na.rm = TRUE)),
              value   = c(40, 80),
              step    = 1
            ),
            
            selectInput(
              inputId = "p_status",
              label   = "Status",
              choices = c("All",
                          "Died",
                          "Alive",
                          "Hospitalized",
                          "Never hospitalized")
            )
          ),
          
          box(
            width = 9, status = "primary", solidHeader = TRUE,
            title = "Patient-level data",
            DTOutput("tbl_patients")
          )
        )
      ),
      
      ##======== Tab 6: Documentation (unchanged)=======
      tabItem(
        tabName = "docs",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Documentation and Help",
            h3("Purpose"),
            p("This Shiny dashboard provides an interactive overview of the DIG trial dataset,"),
            p("focusing on baseline characteristics, treatment differences, clinical outcomes,"),
            p("and relationships between key clinical variables."),
            h3("Tab overview"),
            tags$ul(
              tags$li(strong("Tab 1 – About:"), " overall description of the DIG study and high-level summaries."),
              tags$li(strong("Tab 2 – Baseline characteristics:"), 
                      " summary statistics and visualisations of baseline variables by treatment with filters."),
              tags$li(strong("Tab 3 – Clinical outcomes:"), 
                      " mortality and hospitalisation summaries with dynamic filters."),
              tags$li(strong("Tab 4 – Variable explorer:"), 
                      " interactive scatterplots with X/Y variable selection, grouping and correlation."),
              tags$li(strong("Tab 5 – Patient explorer:"), 
                      " patient-level table with filters for age, sex, treatment and status."),
              tags$li(strong("Tab 6 – Documentation & help:"), 
                      " explanation of the app, workflow and team contributions.")
            )
          )
        )
      )
    )
  )
)

##========== 3. Server=========

server <- function(input, output, session) {
  
  ## === About tab ===
  about_trtplot_data <- reactive({
    d <- dig
    if (!is.null(input$about_trtplot_sex) && length(input$about_trtplot_sex) > 0) {
      d <- d %>% filter(sex %in% input$about_trtplot_sex)
    } else {
      d <- d[0, ]
    }
    d
  })
  
  about_sexplot_data <- reactive({
    d <- dig
    if (!is.null(input$about_sexplot_trt) && length(input$about_sexplot_trt) > 0) {
      d <- d %>% filter(treatment %in% input$about_sexplot_trt)
    } else {
      d <- d[0, ]
    }
    d
  })
  
  output$vb_total <- renderValueBox({
    valueBox(nrow(dig), "Total patients", icon = icon("users"), color = "teal")
  })
  output$vb_digoxin <- renderValueBox({
    valueBox(sum(dig$treatment=="Digoxin"), "Digoxin group", icon = icon("capsules"), color = "blue")
  })
  output$vb_placebo <- renderValueBox({
    valueBox(sum(dig$treatment=="Placebo"), "Placebo group", icon = icon("prescription-bottle"), color = "purple")
  })
  
  output$about_treatment_plot <- renderPlot({
    d <- about_trtplot_data()
    if (nrow(d) == 0) {
      ggplot() +
        labs(x = "Treatment", y = "Count") +
        theme_minimal()
    } else {
      ggplot(d, aes(x = treatment, fill = treatment)) +
        geom_bar(colour = "black") +
        labs(x = "Treatment", y = "Count") +
        theme_minimal()
    }
  })
  
  output$about_sex_plot <- renderPlot({
    d <- about_sexplot_data()
    if (nrow(d) == 0) {
      ggplot() +
        labs(x = "Sex", y = "Count") +
        theme_minimal()
    } else {
      ggplot(d, aes(x = sex, fill = sex)) +
        geom_bar(colour = "black") +
        labs(x = "Sex", y = "Count") +
        theme_minimal()
    }
  })
  
  # small, paginated baseline table
  output$about_baseline_table <- renderDT({
    dig %>%
      group_by(treatment) %>%
      summarise(
        n        = n(),
        mean_age = round(mean(age, na.rm = TRUE), 1),
        mean_bmi = round(mean(bmi, na.rm = TRUE), 1),
        male_pct = round(100 * mean(sex == "Male", na.rm = TRUE), 1),
        .groups  = "drop"
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 5,
          dom = "tip"
        )
      )
  })
  
  # SHORT summary: only selected variables
  output$about_summary <- renderPrint({
    vars <- c("age", "bmi", "ejf_per", "klevel", "creatinine", "sbp", "dbp")
    summary(dig[, vars, drop = FALSE])
  })
  
  output$about_preview <- renderDT({
    datatable(head(dig, 20))
  })
  
  output$about_age_hist <- renderPlot({
    ggplot(dig, aes(x = age)) +
      geom_histogram(binwidth = 5, fill = "steelblue", colour = "white") +
      labs(x = "Age (years)", y = "Count", title = "Age distribution of patients") +
      theme_minimal()
  })
  
  output$about_age_boxplot <- renderPlot({
    ggplot(dig, aes(x = treatment, y = age, fill = treatment)) +
      geom_boxplot(colour = "black") +
      labs(x = "Treatment", y = "Age (years)") +
      theme_minimal()
  })
  
  output$about_bmi_boxplot <- renderPlot({
    ggplot(dig, aes(x = treatment, y = bmi, fill = treatment)) +
      geom_boxplot(colour = "black") +
      labs(x = "Treatment", y = "BMI") +
      theme_minimal()
  })
  
  ##==== Baseline tab ====
  b_data <- reactive({
    d <- dig
    if (input$b_sex != "All")  d <- d %>% filter(sex == input$b_sex)
    if (input$b_race != "All") d <- d %>% filter(race == input$b_race)
    d <- d %>% filter(age >= input$b_age[1], age <= input$b_age[2])
    if (!is.null(input$b_trt) && length(input$b_trt) > 0) {
      d <- d %>% filter(treatment %in% input$b_trt)
    } else {
      d <- d[0, ]
    }
    d
  })
  
  output$plot_baseline_age <- renderPlot({
    d <- b_data()
    validate_data(d)
    ggplot(d, aes(x = age, fill = treatment)) +
      geom_histogram(binwidth = 5, colour = "black", alpha = 0.7, position = "identity") +
      labs(x = "Age (years)", y = "Count", fill = "Treatment") +
      theme_minimal()
  })
  
  output$plot_baseline_bmi <- renderPlot({
    d <- b_data()
    validate_data(d)
    ggplot(d, aes(x = treatment, y = bmi, fill = treatment)) +
      geom_boxplot(colour = "black") +
      labs(x = "Treatment", y = "BMI") +
      theme_minimal()
  })
  
  output$base_text <- renderUI({
    d <- b_data()
    validate_data(d)
    
    n_pat    <- nrow(d)
    mean_age <- round(mean(d$age, na.rm = TRUE), 1)
    mean_bmi <- round(mean(d$bmi, na.rm = TRUE), 1)
    
    by_trt <- d %>%
      group_by(treatment) %>%
      summarise(
        n        = n(),
        mean_bmi = round(mean(bmi, na.rm = TRUE), 1),
        .groups  = "drop"
      )
    
    txt_trt <- paste(
      by_trt$treatment, ": n = ", by_trt$n,
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
  
  output$base_sex_prop <- renderPlot({
    d <- b_data()
    validate_data(d)
    
    d_sum <- d %>%
      count(treatment, sex) %>%
      group_by(treatment) %>%
      mutate(prop = n / sum(n))
    
    ggplot(d_sum, aes(x = treatment, y = prop, fill = sex)) +
      geom_col(colour = "black", position = "fill") +
      scale_y_continuous(labels = percent) +
      labs(x = "Treatment", y = "Proportion", fill = "Sex") +
      theme_minimal()
  })
  
  output$plot_baseline_var <- renderPlot({
    d <- b_data()
    validate_data(d)
    var <- input$b_var
    if (input$b_plot_type == "hist") {
      ggplot(d, aes_string(x = var, fill = "treatment")) +
        geom_histogram(alpha = 0.6, position = "identity") +
        labs(x = var, y = "Count",
             title = paste("Histogram of", var, "by treatment"))
    } else {
      ggplot(d, aes_string(x = "treatment", y = var, fill = "treatment")) +
        geom_boxplot(alpha = 0.7) +
        labs(x = "Treatment", y = var,
             title = paste("Boxplot of", var, "by treatment"))
    }
  })
  
  output$tbl_baseline <- renderDT({
    d <- b_data()
    validate_data(d)
    summary_df <- d %>%
      group_by(treatment) %>%
      summarise(
        n         = n(),
        mean_age  = round(mean(age, na.rm = TRUE), 1),
        mean_bmi  = round(mean(bmi, na.rm = TRUE), 1),
        mean_ef   = round(mean(ejf_per, na.rm = TRUE), 1),
        mean_k    = round(mean(klevel, na.rm = TRUE), 2),
        mean_cre  = round(mean(creatinine, na.rm = TRUE), 2),
        mean_sbp  = round(mean(sbp, na.rm = TRUE), 1),
        mean_dbp  = round(mean(dbp, na.rm = TRUE), 1),
        .groups   = "drop"
      )
    datatable(summary_df, rownames = FALSE, options = list(pageLength = 5))
  })
  
  output$base_corr_text <- renderUI({
    d <- b_data()
    if (nrow(d) < 3) {
      return(HTML("Not enough data to calculate correlation between age and BMI for the current selection."))
    }
    r <- suppressWarnings(cor(d$age, d$bmi, use = "complete.obs"))
    if (is.na(r)) {
      return(HTML("Correlation not available for age and BMI in the current selection."))
    }
    strength <- case_when(
      abs(r) < 0.2 ~ "very weak",
      abs(r) < 0.4 ~ "weak",
      abs(r) < 0.6 ~ "moderate",
      abs(r) < 0.8 ~ "strong",
      TRUE         ~ "very strong"
    )
    direction <- ifelse(r >= 0, "positive", "negative")
    HTML(paste0(
      "<b>Correlation summary:</b><br>",
      "Pearson correlation between age and BMI in the filtered patients is ",
      round(r, 2), " (", strength, " ", direction, " association)."
    ))
  })
  
  ##===== Outcomes, relations, patients tabs unchanged =====
  c_data <- reactive({
    d <- dig
    if (input$c_sex != "All") d <- d %>% filter(sex == input$c_sex)
    d <- d %>% filter(age >= input$c_age[1], age <= input$c_age[2])
    d <- d %>%
      mutate(
        ef_cat = case_when(
          ejf_per < 40                  ~ "Low EF",
          ejf_per >= 40 & ejf_per < 50  ~ "Mid-range EF",
          ejf_per >= 50                 ~ "Preserved EF",
          TRUE ~ NA_character_
        )
      )
    if (input$c_ef != "All") d <- d %>% filter(ef_cat == input$c_ef)
    d
  })
  
  output$tbl_mort <- renderDT({
    d <- c_data()
    validate_data(d)
    mort <- d %>%
      group_by(treatment) %>%
      summarise(
        n         = n(),
        deaths    = sum(death == 1, na.rm = TRUE),
        death_pct = 100 * deaths / n,
        .groups   = "drop"
      )
    datatable(mort, rownames = FALSE, options = list(pageLength = 5))
  })
  
  output$plot_mort <- renderPlot({
    d <- c_data()
    validate_data(d)
    mort <- d %>%
      group_by(treatment) %>%
      summarise(
        deaths = mean(death == 1, na.rm = TRUE),
        .groups = "drop"
      )
    ggplot(mort, aes(x = treatment, y = deaths)) +
      geom_col() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "Treatment", y = "Mortality (%)",
           title = "Mortality by treatment")
  })
  
  output$tbl_hosp <- renderDT({
    d <- c_data()
    validate_data(d)
    hosp <- d %>%
      group_by(treatment) %>%
      summarise(
        n          = n(),
        any_hosp   = mean(hosp == 1, na.rm = TRUE),
        mean_days  = mean(hospdays, na.rm = TRUE),
        mean_nhosp = mean(nhosp, na.rm = TRUE),
        .groups    = "drop"
      )
    datatable(hosp, rownames = FALSE, options = list(pageLength = 5))
  })
  
  output$plot_hosp <- renderPlot({
    d <- c_data()
    validate_data(d)
    hosp <- d %>%
      group_by(treatment) %>%
      summarise(
        any_hosp = mean(hosp == 1, na.rm = TRUE),
        .groups  = "drop"
      )
    ggplot(hosp, aes(x = treatment, y = any_hosp)) +
      geom_col() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "Treatment", y = "Hospitalized (%)",
           title = "Hospitalization by treatment")
  })
  
  output$plot_deathday <- renderPlot({
    d <- c_data()
    validate_data(d)
    ggplot(d %>% filter(death == 1), aes(x = deathday)) +
      geom_histogram(binwidth = 100) +
      labs(x = "Days to death", y = "Count",
           title = "Distribution of days to death")
  })
  
  output$plot_rel <- renderPlot({
    d <- dig
    validate_data(d)
    
    xvar <- input$r_x
    yvar <- input$r_y
    col  <- input$r_col
    
    if (col == "none") {
      p <- ggplot(d, aes_string(x = xvar, y = yvar))
    } else {
      p <- ggplot(d, aes_string(x = xvar, y = yvar, color = col))
    }
    
    p <- p + geom_point(alpha = 0.6)
    
    if (isTRUE(input$r_smooth)) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    
    p + labs(
      x = xvar,
      y = yvar,
      title = "Relationship between selected variables"
    )
  })
  
  output$txt_cor <- renderText({
    x <- dig[[input$r_x]]
    y <- dig[[input$r_y]]
    r <- suppressWarnings(cor(x, y, use = "complete.obs"))
    if (is.na(r)) {
      "Correlation not available"
    } else {
      round(r, 3)
    }
  })
  
  patient_filtered <- reactive({
    d <- dig
    if (input$p_trt != "All")  d <- d %>% filter(treatment == input$p_trt)
    if (input$p_sex != "All")  d <- d %>% filter(sex == input$p_sex)
    
    d <- d %>% filter(age >= input$p_age[1], age <= input$p_age[2])
    
    if (input$p_status == "Died") {
      d <- d %>% filter(death == 1)
    } else if (input$p_status == "Alive") {
      d <- d %>% filter(death == 0)
    } else if (input$p_status == "Hospitalized") {
      d <- d %>% filter(hosp == 1)
    } else if (input$p_status == "Never hospitalized") {
      d <- d %>% filter(hosp == 0)
    }
    d
  })
  
  output$tbl_patients <- renderDT({
    d <- patient_filtered()
    validate_data(d)
    cols_to_show <- intersect(
      c("ID","treatment","sex","race","age","bmi","ejf_per",
        "klevel","creatinine","sbp","dbp","death","hosp","nhosp","deathday"),
      names(d)
    )
    datatable(
      d[, cols_to_show, drop = FALSE],
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
