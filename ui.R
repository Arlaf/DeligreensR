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
    uiOutput("body"),
    textOutput("keepAlive")
  ),
  
  ######### Pour eviter la connexion time out ######
  tags$head(
    HTML(
      "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
    )
  )
)