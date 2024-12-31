# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# 
# Trying GSscraper for pulling the references lists from our papers
# by Justin 
# 30 August 2024
# 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Run the shiny app
install.packages(c("shiny", "RCurl", "purrr", "dplyr", "shinyjs", "shinyWidgets", "textclean", "stringi", "magrittr", "zen4R", "shinyalert", "shinybusy", "strex"))
devtools::install_github("hrbrmstr/cfhttr")
library(stringr)
shiny::runGitHub("GSscraper", "nealhaddaway", subdir = "inst/shiny-examples/GSscraper")


