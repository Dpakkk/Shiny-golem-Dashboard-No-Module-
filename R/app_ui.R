#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @import DT
#' @import plotly
#' @import janitor
#' @import highcharter
#' @import shinymanager
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),

    # Dashboard home
    bs4DashPage(
      bs4DashNavbar(),
      bs4DashSidebar(
        skin = "light",
        status = "primary",
        title = "Reduct Dashboard",
        brandColor = "primary",

        bs4SidebarMenu(
          bs4SidebarMenuItem("Dashboard", tabName = "dashboard", icon = "dashboard"),
          bs4SidebarMenuItem("Summary Insights", tabName = "spotaward", icon = "image"),
          bs4SidebarMenuItem("Trend Analysis", tabName = "trendanalysis", icon = "bar-chart-o"),
          bs4SidebarMenuItem("Transcriber Volume", tabName = "transcribervolume", icon = "briefcase"),
          bs4SidebarMenuItem("Company Volume", tabName = "companyvolume", icon = "hdd")
        )
      ),
      bs4DashBody(
        bs4TabItems(
          bs4TabItem(
            tabName = "dashboard",
            h2("Dashboard")
          ),
          bs4TabItem(
            tabName = "spotaward",

            ## for uploading latest data in csv form
            fluidRow(
              column(4, fileInput("main_df", "Upload Transcription CSV",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              )),

              ## for filtering data based on the date
              column(3, wellPanel(
                dateRangeInput("dateRange",
                  label = "Filter by date",

                  start = as.Date(Sys.Date()) - 10, end = as.Date(Sys.Date()) - 2
                )
              )),

              ## for filtering data with date and based on which data will be compared
              column(3, wellPanel(
                dateRangeInput("compareRange",
                  label = "Compare with:",

                  start = as.Date(Sys.Date()) - 14, end = as.Date(Sys.Date()) - 7
                )
              )),
              column(2, wellPanel(actionButton("summary_update", "Update", icon("refresh"),
                class = "btn btn-primary"
              )))
            ),
            
            # this is for hover informatino in below valuebox
            tags$script(HTML("setInterval(function(){ $('[title]').tooltip(); }, 1000)")),

            ## Valuebox for summary values
            fluidRow(
              valueBoxOutput("audio_volume", width = 2),
              valueBoxOutput("avg_audio_volume", width = 2),
              valueBoxOutput("avg_accuracy", width = 2),
              valueBoxOutput("avg_audio_length", width = 2),
              valueBoxOutput("avg_word_count", width = 2)
            ),


            fluidRow(
              bs4Card(
                title = "Machine VS Manual Transcription",
                width = 6,
                collapsible = TRUE,
                maximizable = TRUE,
                closable = FALSE,

                plotlyOutput("tx_distr_plot")
              ),
              bs4TabCard(
                id = "machine_tx_insights",
                title = "Machine Tx Insight",
                width = 6,
                collapsible = TRUE,
                maximizable = TRUE,
                closable = FALSE,

                bs4TabPanel(
                  tabName = "Time Taken",
                  active = TRUE,
                  bs4Table(
                    cardWrap = FALSE,
                    bordered = TRUE,
                    striped = TRUE,
                    headTitles = c(
                      "INSIGHTS",
                      "Time taken per Mins",
                      #"Compared Time (Minutes)",
                      "Changed Time"
                    ),
                    bs4TableItems(
                      bs4TableItem("With Machine Tx:"),
                      bs4TableItem(textOutput("machine_tx_time_taken")),
                      #bs4TableItem(textOutput("compared_machine_tx_time_taken")),
                      bs4TableItem(uiOutput("changed_machine_tx_time_taken"))
                    ),
                    bs4TableItems(
                      bs4TableItem("Without Machine Tx:"),
                      bs4TableItem(textOutput("manual_tx_time_taken")),
                     # bs4TableItem(textOutput("compared_manual_tx_time_taken")),
                      bs4TableItem(uiOutput("changed_manual_tx_time_taken"))
                    ),
                    bs4TableItems(
                      bs4TableItem("Overall Average:"),
                      bs4TableItem(textOutput("overall_tx_time_taken")),
                    #  bs4TableItem(textOutput("compared_overall_tx_time_taken")),
                      bs4TableItem(uiOutput("changed_overall_tx_time_taken"))
                    ),
                    bs4TableItems(
                      bs4TableItem("Average Audio Review Time:"),
                      bs4TableItem(textOutput("avg_ar_time_taken")),
                     # bs4TableItem(textOutput("compared_avg_ar_time_taken")),
                      bs4TableItem(uiOutput("changed_avg_ar_time_taken"))
                    )
                  )
                ),
                bs4TabPanel(
                  tabName = "Accuracy",
                  active = FALSE,
                  bs4Table(
                    cardWrap = FALSE,
                    bordered = TRUE,
                    striped = TRUE,
                    headTitles = c(
                      "INSIGHTS",
                      "Accuracy",
                      #"Compared Accuracy",
                      "Changed Accuracy"
                    ),
                    bs4TableItems(
                      bs4TableItem("With Machine Tx:"),
                      bs4TableItem(textOutput("machine_tx_accuracy")),
                      #bs4TableItem(textOutput("compared_machine_tx_accuracy")),
                      bs4TableItem(uiOutput("changed_machine_tx_accuracy"))
                    ),
                    bs4TableItems(
                      bs4TableItem("Without Machine Tx:"),
                      bs4TableItem(textOutput("manual_tx_accuracy")),
                      #bs4TableItem(textOutput("compared_manual_tx_accuracy")),
                      bs4TableItem(uiOutput("changed_manual_tx_accuracy"))
                    ),
                    bs4TableItems(
                      bs4TableItem("Overall Average:"),
                      bs4TableItem(textOutput("overall_avg_accuracy")),
                     # bs4TableItem(textOutput("compared_overall_avg_accuracy")),
                      bs4TableItem(uiOutput("changed_overall_avg_accuracy"))
                    ),
                    bs4TableItems(
                      bs4TableItem("Machine TX Error Rate:"),
                      bs4TableItem(textOutput("machine_tx_error_rate")),
                      #bs4TableItem(textOutput("compared_machine_tx_error_rate")),
                      bs4TableItem(uiOutput("changed_machine_tx_error_rate"))
                    )
                  )
                )
              )
            ),

            fluidRow(
              bs4TabCard(
                id = "spot_awards",
                title = "Spot Awards",
                width = 12,
                collapsible = TRUE,
                maximizable = TRUE,
                closable = FALSE,

                bs4TabPanel(
                  tabName = "QF Volume",
                  active = TRUE,
                  highchartOutput("most_qf_volume")
                ),
                bs4TabPanel(
                  tabName = "QF Accuracy",
                  DTOutput("best_accuracy")
                ),
                bs4TabPanel(
                  tabName = "AR Volume",
                  highchartOutput("most_ar_volume")
                ),
                bs4TabPanel(
                  tabName = "AR Accuracy",
                  DTOutput("audio_review_accuracy")
                  
                ),
                bs4TabPanel(
                  tabName = "Machine Transcribers",
                  highchartOutput("fast_machine_tx")
                ),
                bs4TabPanel(
                  tabName = "Manual Transcribers",
                  highchartOutput("fast_manual_tx")
                )

              )
            )
          ),
          bs4TabItem(
            tabName = "trendanalysis",
            fluidRow(
              bs4Card(
                title = "Pre Order Vs Workdone",
                width = 12,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                status = "primary",
                highchartOutput("preorder_workdone")
              )
            ),
            fluidRow(bs4Card(
              title = "Percentage of Pre Order Fulfilled",
              width = 12,
              collapsible = TRUE,
              closable = FALSE,
              maximizable = TRUE,
              status = "primary",
              highchartOutput("preorder_fulfilled")
            )),
            fluidRow(bs4Card(
              title = "WeekDays Distribution of Work",
              width = 12,
              collapsible = TRUE,
              maximizable = TRUE,
              closable = FALSE,
              status = "primary",
              fluidRow(column(4, wellPanel(
                dateRangeInput("weekdayRange",
                  label = "",

                  start = as.Date(Sys.Date()) - 30, end = as.Date(Sys.Date()) - 2
                )
              ))),
              fluidRow(
                column(12, highchartOutput("workdone_weekdays_volume"))
              )
            )),
            fluidRow(
              bs4Card(
                title = "Time Load Transcription",
                width = 12,
                collapsible = TRUE,
                closable = FALSE,
                maximizable = TRUE,
                status = "primary",
                fluidRow(column(4, wellPanel(
                  dateRangeInput("time_load_date",
                                 label = "",
                                 
                                 start = as.Date(Sys.Date()) - 30, end = as.Date(Sys.Date()) - 2
                  )
                ))),
                fluidRow(
                  column(12, plotlyOutput("time_load_graph"))
                )
              )
            )
          ),
          bs4TabItem(
            tabName = "transcribervolume",
            fluidRow(
              column(4, wellPanel(
                dateRangeInput("inputdateuser",
                               label = "Select Date",
                               start = as.Date("2020-01-01"), end = as.Date("2020-11-27")
                ),
              )),
              column(2, wellPanel(actionButton("file_update", "Update", icon("refresh"),
                                               class = "btn btn-primary"
              )))
            ),
            bs4Card(title = "Transcriber Volume",
                    width = 12,
                    collapsible = TRUE,
                    maximizable = TRUE,
                    closable = FALSE,
                    status = "primary",
                    fluidRow(
                      column(12,
                             highchartOutput("bubble_chart")    
                             
                      )
                    )
            ),
            bs4Card(title = "Transcriber Performance",
                    width = 12,
                    collapsible = TRUE,
                    maximizable = TRUE,
                    closable = FALSE,
                    status = "primary",
                    fluidRow(
              column(12,
                     DTOutput("transcriber_volume"))
              )
              )
            ),
          
          bs4TabItem(
            tabName="companyvolume",
            h2("Company Volume"),tags$script(HTML("setInterval(function(){ $('[title]').tooltip(); }, 1000)"))
          )
        )
      )
    )
  )
}


## checking inactivity on login page for 2 minutes

inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 120000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
  window.close();  //close the window
  }

  function resetTimer() {
  clearTimeout(t);
  t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
  }
  }
  idleTimer();"




## for secure login system
#app_ui <- shinymanager::secure_app(head_auth = tags$script(inactivity),theme = shinythemes::shinytheme("flatly"),app_ui,enable_admin = TRUE)


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "dashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
