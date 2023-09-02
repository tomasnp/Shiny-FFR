#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd


if(!require("readr")){install.packages("readr"); library("readr")}
if(!require("dplyr")){install.packages("dplyr"); library("dplyr")}
if(!require("readxl")){install.packages("readxl"); library("readxl")}
if(!require("ggplot2")){install.packages("ggplot2"); library("ggplot2")}
if(!require("shinydashboard")){install.packages("shinydashboard"); library("shinydashboard")}
if(!require("DT")){install.packages("DT"); library("DT")}
if(!require("shinyWidgets")){install.packages("shinyWidgets"); library("shinyWidgets")}
if(!require("shinyjs")){install.packages("shinyjs"); library("shinyjs")}
if(!require("plotly")){install.packages("plotly"); library("plotly")}
if(!require("tidyr")){install.packages("tidyr"); library("tidyr")}
if(!require("shinymaterial")){install.packages("shinymaterial"); library("shinymaterial")}
if(!require("shinyjs")){install.packages("shinyjs"); library("shinyjs")}
if(!require("shiny")){install.packages("shiny"); library("shiny")}

app_sys <- function(...) {
  system.file(..., package = "exemple")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}
