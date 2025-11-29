# DIG Trial Dashboard – Project Plan

## 1. Overview
This dashboard will explore the DIG Trial dataset interactively. It will help users understand baseline characteristics, treatment differences, clinical outcomes, and relationships between variables. The app will be built using Shiny or Shinydashboard and deployed on shinyapps.io.

## 2. Dashboard Structure

### Tab 1 — About the DIG Study
- Short description of the DIG Trial.
- Summary statistics: total patients, treatment counts, age, BMI, sex distribution.
- Simple visualisations: treatment bar chart, sex distribution pie chart.
- Optional variable descriptions.

### Tab 2 — Baseline Characteristics
- Compare digoxin and placebo groups.
- Summary table of baseline variables.
- Visualisations: histograms, boxplots, bar plots.
- Filters: sex, age range, race.
- Variables: AGE, SEX, BMI, EJF_PER, KLEVEL, BP measures.

### Tab 3 — Clinical Outcomes
- Mortality (DEATH, DEATHDAY).
- Hospitalisation (HOSP, HOSPDAYS, NHOSP).
- Mortality % by treatment.
- Distribution of death days.
- Hospitalisation plots and summaries.
- Filters: age, sex, EF category.

### Tab 4 — Explore Relationships
- Users select X and Y variables.
- Colour/grouping by treatment, sex, race.
- Scatterplots with optional smoothing.
- Display correlation.
- Interesting pairs: Age vs EF, BMI vs EF, KLEVEL vs Death, Creatinine vs Death, SBP vs DBP.

### Tab 5 — Patient Explorer
- Filters: treatment, sex, age range, BMI, KLEVEL, death/hospitalisation indicators.
- Interactive table of filtered patients.
- Optional data download.

### Tab 6 — Documentation (Optional)
- Instructions for using the app.
- Explanation of tabs.
- Variable definitions.
- Team credits.

## 3. Design Recommendations
- Clear layout and tab names.
- Consistent colour theme.
- Proper axis labels and informative titles.
- Avoid overcrowded plots.
- Minimal, readable interface.


## 4. Goal
To create a clean, interactive Shiny dashboard that effectively summarises and visualises the DIG Trial dataset while following project guidelines and collaboration standards.
