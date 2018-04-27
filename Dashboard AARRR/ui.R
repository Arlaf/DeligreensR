library(shinydashboard)
library(plotly)
library(lubridate)

dashboardPage(
  
  ################### HEADER ###################
  dashboardHeader(title = "KPI AARRR"),
  
  ################### SIDEBAR ###################
  dashboardSidebar(
    uiOutput("sidebar")
  ),
  
  ################### BODY ###################
  dashboardBody(
    uiOutput("body")
  )
)