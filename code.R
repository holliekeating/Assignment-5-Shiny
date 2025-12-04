library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(DT)
library(plotly)
library(survival)

dig.df <- read_csv("data/DIG.csv")%>%
  mutate(TRTMT = recode(TRTMT, '0' = "Placebo", `1` = "Treatment"),
         WHF = recode(WHF, '0' = "Healthy", '1'= "Worsening Heart Failure"),
         HOSP = recode(HOSP, '0' = "No Hospitalisation", '1' = "Hospitalised"),
         DEATH = recode(DEATH, '0' = "Alive", '1' = "Deceased"),
         CVD = recode(CVD, '0' = "Healthy", '1' = "Cardiovascular Disease"))%>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)

  dig.df <- dig.df%>%
  mutate(MONTH = round(DEATHDAY/30), MONTH= as.numeric(MONTH))

trt_choices  <- sort(unique(dig.df$TRTMT))
whf_choices  <- sort(unique(dig.df$WHF))
hosp_choices <- sort(unique(dig.df$HOSP))
death_choices <- sort(unique(dig.df$DEATH))
cvd_choices <- sort(unique(dig.df$CVD))

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
        tabName = "outcomes",
        # Rate of mortality (treatment groups, WHF, CVD)
        # Rate of hospitalizations (treatment groups, WHF, CVD)
        
        
        fluidRow(
          box(
            title = "Patient Outcomes",
            status = "primary",
            width = 3,
            solidHeader = TRUE,
            p("This section explores patient outcomes in the Digitalis Investigation Group (DIG) Trial.   
              Use the filters to explore the rate of mortality, rate of cardiovascular disease and rate of hospitalisations between groups.
              Explore the interactive plots for information on patient outcomes.")
        ),
# Value boxes    
        valueBoxOutput("mortality_vb", width = 3),
        valueBoxOutput("mortality_placebo_vb", width = 3),
        valueBoxOutput("mortality_treatment_vb", width = 3),
        valueBoxOutput("cvd_treatment_vb", width = 4),
        valueBoxOutput("cvd_placebo_vb", width = 4)
      ),
      
      br(),
          
# Rate of mortality between groups
      fluidRow(
        box(
        title = "Rate of Mortality between Groups",
        status = "primary", solidHeader = TRUE,
        width = 6,
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
          selected = death_choices),
        
        br(),

        plotlyOutput("plot_outcomes_mortality", height = 400),
        
        br()
      ),
        
 # Cardiovascular disease
 box(
   title = "Exploring the Relationship between Cardiovascular Disease and Mortality between Groups",
   status = "primary", solidHeader = TRUE,
   width = 6,
   selectInput(
     inputId  = "TRTMT_cvd",
     label    = "Select Treatment Group:",
     choices  = trt_choices,
     selected = trt_choices, 
     multiple = TRUE
   ),
   checkboxGroupInput(
     inputId  = "CVD",
     label    = "Select Patient Status:",
     choices  = cvd_choices,
     selected = cvd_choices
   ),
   
   br(),
   
   plotlyOutput("plot_outcomes_cvd", height = 400)
   )
 ),   
br(),          
          
 # Worsening heart failure          
      fluidRow(
        column(
          width = 8,
          offset = 2,
          box(
            title = "Worsening Heart Failure and Hospitalisation Status",
            status = "primary", solidHeader = TRUE,
            width = 12,
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
      ),
      
      br(),
      
        plotlyOutput("plot_outcomes_whf", height = 500)
      )
))),

# Trial tab - probabilities of death/survival over time
    tabItem(
      tabName = "trial",
      
      fluidRow(
        box(
          title = "Trial Outcomes",
          status = "primary",
          width = 12,
          solidHeader = TRUE,
          p("This section explores the probability of survival in the Digitalis Investigation Group (DIG) Trial.
            As shown in the graphs there is a steady decreasing probability of survival in both the treatment and placebo groups. 
            By the end of the trial, in month 56 the probability of survival in the placebo group was 57.10% and in the treatment group it was 58.40%.
            Explore the interactive plots and survival tables for information on the trial outcomes.")
        )
      ),
      
      br(),
      
      fluidRow(
        box(
          title = "Placebo Group - Probability of Survival over Time",
          status = "primary", solidHeader = TRUE,
          width = 6,
            plotlyOutput("plot_surv_placebo", height = 400)),
          box(
            title = "Treatment Group - Probability of Survival over Time",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_surv_treatment", height = 400),
            ),
          br(),
            
        fluidRow(
          box(
            title = "Placebo - Survival Probabilities Table",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tableOutput("placebo_table_surv")
          ),
          
          br(),
          
          fluidRow(
          box(
            title = "Treatment - Survival Table",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tableOutput("treatment_table_surv"))
           ) 
        )
      )
    )
)))


server <- function(input, output) {

# Patient outcomes: Rate of mortality between groups
  # Value boxes
  output$mortality_vb <- renderValueBox({
    
    total_patients <- nrow(dig.df)
    total_deaths <- sum(dig.df$DEATH == "Deceased", na.rm = TRUE)
    perc_deaths <- round(100*total_deaths/total_patients, 1)

    
    valueBox(
      value    = paste0(perc_deaths, "%"),
      subtitle = "Overall Mortality",
      color    = "green"
    )
  })
  
  
  output$mortality_placebo_vb <- renderValueBox({
    
    placebo_total <- sum(dig.df$TRTMT == "Placebo", na.rm = TRUE)
    placebo_deaths <- sum(dig.df$TRTMT == "Placebo" & dig.df$DEATH == "Deceased")
    perc_deaths_placebo <- round(100*placebo_deaths/placebo_total, 1)

    
    valueBox(
      value    = paste0(perc_deaths_placebo, "%"),
      subtitle = "Rate of Mortality in Placebo Group",
      color    = "olive"
    )
  })
  
  output$mortality_treatment_vb <- renderValueBox({
    
    trtmt_total <- sum(dig.df$TRTMT == "Treatment", na.rm = TRUE)
    trtmt_deaths <- sum(dig.df$TRTMT == "Treatment" & dig.df$DEATH == "Deceased")
    perc_deaths_trtmt <- round(100*trtmt_deaths/trtmt_total, 1)
    
    valueBox(
      value    = paste0(perc_deaths_trtmt, "%"),
      subtitle = "Rate of Mortality in Treatment Group",
      color    = "teal"
    )
  })
  
  # Valueboxes cvd rates between groups
  output$cvd_placebo_vb <- renderValueBox({
    
    placebo_total <- sum(dig.df$TRTMT == "Placebo", na.rm = TRUE)
    placebo_cvd <- sum(dig.df$TRTMT == "Placebo" & dig.df$CVD == "Cardiovascular Disease")
    perc_cvd_placebo <- round(100*placebo_cvd/placebo_total, 1)
    
    
    valueBox(
      value    = paste0(perc_cvd_placebo, "%"),
      subtitle = "Rate of Cardiovascular Disease in Placebo Group",
      color    = "blue"
    )
  })
  
  output$cvd_treatment_vb <- renderValueBox({
    
    trtmt_total <- sum(dig.df$TRTMT == "Treatment", na.rm = TRUE)
    trtmt_cvd <- sum(dig.df$TRTMT == "Treatment" & dig.df$CVD == "Cardiovascular Disease")
    perc_cvd_trtmt <- round(100*trtmt_cvd/trtmt_total, 1)
    
    valueBox(
      value    = paste0(perc_cvd_trtmt, "%"),
      subtitle = "Rate of Cardiovascular Disease in Treatment Group",
      color    = "navy"
    )
  })
  
  #Plots: Rate of Mortality between groups
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
      scale_fill_manual(values = c("Alive"= "lightyellow", "Deceased" = "darkgreen"))+
      labs(
        fill = "Patient Status",
        x    = "Patient Mortality Status",
        y    = "Percentage (%)")+
      theme_minimal()+
      theme(
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text =  element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 8)
      )
    ggplotly(plot_outcomes_mortality1, tooltip = "text")
  })

  
# Patient outcomes: CVD and mortality
  cvd_sub <- reactive({
    req(input$TRTMT_cvd, input$CVD)
    
    dig.df %>%
      filter(TRTMT %in% input$TRTMT_cvd) %>%
      filter(CVD %in% input$CVD)
  })
  output$plot_outcomes_cvd <- renderPlotly({ 
    dat <- cvd_sub()
    req(nrow(dat) > 0)  
    
    plot_outcomes_cvd1 <- 
      dat %>%
      ggplot(aes(
        x = TRTMT,
        y = after_stat(100 * count / sum(count)),
        fill = CVD,
        text = paste0(
          "Treatment Group: ", TRTMT, "<br>",
          "Patient Status: ", CVD
        ))) +
      geom_bar(position = "dodge", colour = "black") +
      scale_fill_manual(values = c("Healthy"= "lightyellow", "Cardiovascular Disease" = "darkblue"))+
      labs(
        fill = "Patient Status",
        x    = "Treatment Group",
        y    = "Percentage (%)")+
      theme_minimal()+
      theme(
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text =  element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 8)
      )
    ggplotly(plot_outcomes_cvd1, tooltip = "text")
  })


  

# Patient outcomes: rate of hospitalizations and WHF between groups   
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
    theme_minimal()+
      theme(
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text =  element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 8)
      )
    ggplotly(plot_outcomes_whf1, tooltip = "text")
  })
  
  
  
  
  
  
  
  
  # Trial Outcomes
  # Get the probability of survival for each group
  # Placebo 
  sf_placebo <- survfit(Surv(MONTH, DEATH == "Deceased") ~ 1,
                        data = dig.df %>% filter(TRTMT == "Placebo"))
  sum_placebo <- summary(sf_placebo)
  
  placebo_df <- data.frame(
    month    = sum_placebo$time,
    survival = sum_placebo$surv,
    n_risk   = sum_placebo$n.risk,
    deaths   = sum_placebo$n.event
  )
  
# Treatment 
  sf_treatment <- survfit(Surv(MONTH, DEATH == "Deceased") ~ 1,
                          data = dig.df %>% filter(TRTMT == "Treatment"))
  sum_treatment <- summary(sf_treatment)
  
  treatment_df <- data.frame(
    month    = sum_treatment$time,
    survival = sum_treatment$surv,
    n_risk   = sum_treatment$n.risk,
    deaths   = sum_treatment$n.event
  )
  
  placebo_df$survival_perc <- placebo_df$survival*100
  treatment_df$survival_perc <- treatment_df$survival*100
  
  # Plot: Placebo
  output$plot_surv_placebo <- renderPlotly({
    plot_ly(
      data = placebo_df,
      x    = ~month,
      y    = ~survival_perc,
      type = "scatter",
      mode = "lines",
      line = list(width = 8, color = "darkgreen"),
      hoverinfo = "text",
      text = ~paste0(
        "Group: Placebo", "<br>",
        "Month: ", month, "<br>",
        "Survival: ", round(survival, 3), "<br>",
        "At risk: ", n_risk, "<br>",
        "Deaths: ", deaths
      )
    ) %>%
      layout(
        xaxis = list(title = "Time (months)"),
        yaxis = list(title = "Probability of Survival %", range = c(50, 100))
      )
  })
  
  
  # Plot: Treatment
  output$plot_surv_treatment <- renderPlotly({
    plot_ly(
      data = treatment_df,
      x    = ~month,
      y    = ~survival_perc,
      type = "scatter",
      mode = "lines",
      line = list(width = 8, color = "darkgreen"),
      hoverinfo = "text",
      text = ~paste0(
        "Group: Treatment", "<br>",
        "Month: ", month, "<br>",
        "Survival: ", round(survival, 3), "<br>",
        "At risk: ", n_risk, "<br>",
        "Deaths: ", deaths
      )
    ) %>%
      layout(
        xaxis = list(title = "Time (months)"),
        yaxis = list(title = "Probability of Survival %", range = c(50, 100))
      )
  })

# Trial outcomes: Tables probability of survival over time 
  output$placebo_table_surv <- renderTable({
    placebo_df %>%
      transmute(
        "Month" = month,
        "Survival (%)" = round(survival_perc,2),
        "Number at risk" = n_risk,
        "Deaths"= deaths)
  })
  
  output$treatment_table_surv <- renderTable({
    treatment_df %>%
      transmute(
        "Month"= month,
        "Survival (%)" = round(survival_perc,2),
        "Number at risk"= n_risk,
        "Deaths" = deaths)
  })
  

  
}


shinyApp(ui, server)





