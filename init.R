my_packages = c("shiny", 
                "shinydashboard",
                "shinyjs",
                "dplyr",
                "plotly",
                "lubridate",
                "RPostgreSQL",
                "sqldf"
                )

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))