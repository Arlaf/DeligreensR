my_packages = c("shinydashboard",
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

invisble(install.packages("shiny"))

invisible(sapply(my_packages, install_if_missing))