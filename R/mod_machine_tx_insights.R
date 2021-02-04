#' machine_tx_insights UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_machine_tx_insights_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' machine_tx_insights Server Function
#'
#' @noRd 
mod_machine_tx_insights_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_machine_tx_insights_ui("machine_tx_insights_ui_1")
    
## To be copied in the server
# callModule(mod_machine_tx_insights_server, "machine_tx_insights_ui_1")
 
