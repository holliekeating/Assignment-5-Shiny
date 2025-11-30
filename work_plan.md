1. Project Goal

Build a clean, interactive Shiny or Shinydashboard app that helps users explore the DIG Trial dataset. It should provide:

Overview of the study

Baseline characteristics

Outcomes (mortality & hospitalisation)

Relationships between variables

Filtering and personalised exploration

Clear documentation

Deployed on shinyapps.io and version-controlled via GitHub.

2. App Structure (Final agreed structure)
TAB 1 — About the DIG Study

Study summary

Dataset info

Summary statistics

Basic visualisations

TAB 2 — Baseline Characteristics

Compare digoxin vs placebo

Tables & plots

Filters (age, sex, race)

TAB 3 — Clinical Outcomes

Mortality

Death-day distribution

Hospitalisation measures

Comparisons by treatment

TAB 4 — Variable Explorer (Interactive)

Select X and Y variables

Filter by treatment, sex, age

Scatterplots with optional smoothing

Correlation value

TAB 5 — Patient Explorer

Data table that updates from filters

Optional: download table

TAB 6 — Documentation / Help Page

Instructions

Variable definitions

Notes on app

Credits

 3. Work Allocation Between Three Members

To show balanced collaboration, split the work into:

Manasvi — Technical Structure + Baseline Analysis
Main Responsibilities:

Set up Shiny project structure

Build:

TAB 1 (Overview)

TAB 2 (Baseline Characteristics)

Prepare:

Summary statistics

Baseline plots (age, BMI, EF, sex)

Create clean UI layout

Organise the GitHub repo

Write part of documentation text

Why this role fits you:

You already understand the dataset well and built baseline analysis in Assignment 4.

Hollie — Outcomes + Deployment
Main Responsibilities:

Build:

TAB 3 (Clinical Outcomes)

Prepare:

Mortality plots

Hospitalisation summaries

Outcome tables

Add dynamic filters (age, sex, EF categories)

Deploy the finished app to shinyapps.io

Write interpretation text for outcomes tab

Set up GitHub branch protection & QA checks

Why this role fits:

Hollie owns the GitHub repo and can manage deployment smoothly.

Xiangqi — Interactive Exploration + Patient Filter
Main Responsibilities:

Build:

TAB 4 (Variable Explorer)

TAB 5 (Patient Explorer)

Create UI controls:

X variable selector

Y variable selector

Color/grouping selector

Age, sex, treatment filters

Build dynamic data table

Add correlation calculation

Write documentation/help tab (Tab 6)

Why this role fits:

These tabs require controlled UI elements, which are easier to implement gradually.

4. Timeline (5–7 Days)
Day 1

Team meeting

Confirm plan

Assign tasks

Create GitHub repo branches:

overview-tab

baseline-tab

outcomes-tab

explorer-tab

patient-tab

Day 2–3

Each member builds their assigned tabs locally

Commit small updates every day

Push to branches

Open Pull Requests for review by teammates

Day 4

Combine finished tabs into main branch

Finalise UI layout

Fix bugs

Ensure tabs work smoothly

Day 5

Deploy to shinyapps.io

Final visual polish

Add Help tab

Team testing

Day 6

Write contribution statements

Final review on GitHub

Submit project links

5. GitHub Workflow (Required for marks)

Your team should use:

✔ Branches
✔ Pull Requests
✔ Reviews
✔ Commit messages

Recommended workflow:

Each person works on their own branch

Commits daily with meaningful messages

Opens a Pull Request

Another team member reviews

Merge after fixes

This is necessary for collaboration marks.

6. Contribution Statement Guidance

Each person should describe:

What tabs they built

Code and UI they wrote

How they tested

How they reviewed partner’s PRs

What documentation they contributed

Collaboration examples (meetings, reviews)