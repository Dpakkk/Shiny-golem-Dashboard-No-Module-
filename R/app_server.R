#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @import data.table
#' @import dplyr
#' @import tidyr
#' @import plotly
#' @import janitor
#' @import ggplot2
#' @import DT
#' @import highcharter
#' @import shinymanager
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here


  # Observing the first series of inputs
  # Whenever the user clicks on the first validate button,
  # the value of choice1 will be printed to the console
  "observeEvent(input$validate1, {
    print(input$choice1)
  }) 

  

  # Same as the first observeEvent, except that we are
  # observing the second series
  observeEvent(input$validate2, {
    print(input$choice2)
  })

"
  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials("credentials/credentials.sqlite")
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })


  # By default, the file size limit is 5MB
  # Here we'll raise limit to 100MB.
  options(shiny.maxRequestSize = 100 * 1024^2)

  ## upload data
  main_df <- reactive({
    infile <- input$main_df
    if (is.null(infile)) {
      return(NULL)
    }
    data.table::fread(infile$datapath)
  })





  ## data
  df <- data.table::fread("data/Transcript-Data.csv") %>% as.data.frame()
  transcriber_name <- data.table::fread("data/trancriber_name.csv") %>% as.data.frame()
  pre_order_df <- data.table::fread("data/Preorder-Data.csv")%>%
  as.data.frame()%>%
  clean_names()

  new_df <- reactive({
    main_df() %>%
      as.data.frame() %>%
      janitor::clean_names()
  })


  ## Reactive data using the global date filter
  trial_df <- reactive({
    input$summary_update

    infile <- input$main_df
    if (is.null(infile)) {
      data <- isolate(df %>%
        as.data.frame() %>%
        janitor::clean_names() %>%
        mutate(created_on_date = lubridate::as_datetime(workflow_completed_ended_on / 1000)) %>%
        filter(lubridate::date(created_on_date) >= as.character(input$dateRange[1]) & lubridate::date(created_on_date) <= as.character(input$dateRange[2])))
    }
    else {
      data <- isolate(
        new_df() %>%
          janitor::clean_names() %>%
          mutate(created_on_date = lubridate::as_datetime(workflow_completed_ended_on / 1000)) %>%
          filter(lubridate::date(created_on_date) >= as.character(input$dateRange[1]) & lubridate::date(created_on_date) <= as.character(input$dateRange[2]))
      )
    }
  })

  ## Reactive comparing data  using the filter
  compare_df <- reactive({
    input$summary_update
    infile <- input$main_df
    if (is.null(infile)) {
      data <- isolate(
        df %>%
          as.data.frame() %>%
          janitor::clean_names() %>%
          mutate(created_on_date = lubridate::as_datetime(workflow_completed_ended_on / 1000)) %>%
          filter(lubridate::date(created_on_date) >= as.character(input$compareRange[1]) & lubridate::date(created_on_date) <= as.character(input$compareRange[2]))
      )
    }
    else {
      data <- isolate(
        new_df() %>%
          janitor::clean_names() %>%
          mutate(created_on_date = lubridate::as_datetime(workflow_completed_ended_on / 1000)) %>%
          filter(lubridate::date(created_on_date) >= as.character(input$compareRange[1]) & lubridate::date(created_on_date) <= as.character(input$compareRange[2]))
      )
    }
  })
  
## Reactive data for the machine tx insights table
  machine_df <- reactive({
    input$summary_update
    
    infile <- input$main_df
    if (is.null(infile)) {
      data <- isolate(df %>%
                        as.data.frame() %>%
                        janitor::clean_names() %>%
                        mutate(created_on_date = lubridate::as_datetime(workflow_completed_ended_on / 1000)) %>%
                        filter(lubridate::date(created_on_date) >= as.character(input$dateRange[1]) & lubridate::date(created_on_date) <= as.character(input$dateRange[2])))
    }
    else {
      data <- isolate(
        new_df() %>%
          janitor::clean_names() %>%
          mutate(created_on_date = lubridate::as_datetime(workflow_completed_ended_on / 1000)) %>%
          filter(lubridate::date(created_on_date) >= as.character(input$dateRange[1]) & lubridate::date(created_on_date) <= as.character(input$dateRange[2]))
      )
    }
  })
  
  ## Reactive data for the machine tx insights table
  compare_machine_df <- reactive({
    input$summary_update
    
    infile <- input$main_df
    if (is.null(infile)) {
      data <- isolate(df %>%
                        as.data.frame() %>%
                        janitor::clean_names() %>%
                        mutate(created_on_date = lubridate::as_datetime(workflow_completed_ended_on / 1000)) %>%
                        filter(lubridate::date(created_on_date) >= as.character(input$compareRange[1]) & lubridate::date(created_on_date) <= as.character(input$compareRange[2])))
    }
    else {
      data <- isolate(
        new_df() %>%
          janitor::clean_names() %>%
          mutate(created_on_date = lubridate::as_datetime(workflow_completed_ended_on / 1000)) %>%
          filter(lubridate::date(created_on_date) >= as.character(input$compareRange[1]) & lubridate::date(created_on_date) <= as.character(input$compareRange[2]))
      )
    }
  })


  ##  code for total audio volume
  audio_volume <- reactive({
    trial_df() %>%
      summarise(audio_length = round(sum(audio_length_seconds)))
  })


  audio_volume_change <- reactive({
    prev_volume <- as.double(compare_df() %>% summarise(audio_length = round(sum(audio_length_seconds))))
    round((audio_volume() - prev_volume) / prev_volume, 4) * 100
  })



  ## code for average daily audio volume
  ## as we are taking whole days from starting to ending date
  ## add 1 for correct number of days.
  avg_audio_volume <- reactive({
    input$summary_update

    no_days <- isolate(as.double(difftime(as.Date(input$dateRange[2]), as.Date(input$dateRange[1]))) + 1)
    trial_df() %>%
      summarise(avg_audio_volume_daily = sum(audio_length_seconds) / no_days)
  })

  avg_audio_change <- reactive({
    input$summary_update

    no_days <- isolate(as.double(difftime(as.Date(input$compareRange[2]), as.Date(input$compareRange[1]))) + 1)
    prev_avg <- compare_df() %>%
      summarise(avg_audio_volume_daily = sum(audio_length_seconds) / no_days) %>%
      as.double()

    round((avg_audio_volume() - prev_avg) / prev_avg, 4) * 100
  })



  ## code for average accuracy value
  avg_accuracy <- reactive({
    trial_df() %>%
      summarise(avg_accuracy = round(mean(accuracy, na.rm = TRUE) * 100, 2))
  })

  avg_accuracy_change <- reactive({
    prev_avg_accuracy <- compare_df() %>%
      summarise(avg_accuracy = round(mean(accuracy, na.rm = TRUE) * 100, 2)) %>%
      as.double()

    round((avg_accuracy() - prev_avg_accuracy) / prev_avg_accuracy, 4) * 100
  })

  ## code for average audio length
  avg_audio_length <- reactive({
    trial_df() %>%
      summarise(avg_audio_length = round(mean(audio_length_seconds), 2))
  })

  avg_audio_length_change <- reactive({
    prev_avg_audio_length <- compare_df() %>%
      summarise(avg_audio_length = round(mean(audio_length_seconds), 2))

    round((avg_audio_length() - prev_avg_audio_length) / prev_avg_audio_length, 4) * 100
  })

  ## code for average word count
  avg_word_count <- reactive({
    trial_df() %>%
      summarise(avg_word_count = round(mean(word_count, na.rm = TRUE)))
  })

  avg_word_count_change <- reactive({
    prev_word_count <- compare_df() %>%
      summarise(avg_word_count = round(mean(word_count, na.rm = TRUE)))

    round((avg_word_count() - prev_word_count) / prev_word_count, 4) * 100
  })



  ## create Value boxes

  ## Value box for audio volume
  output$audio_volume <- renderValueBox({
    customValueBox(
      value = paste0((audio_volume() %/% (3600)) %>% as.character(), "  HRS"),
      title = toupper("Total Audio"),
      subtitle = audio_volume_change(),
      color = "light-blue",
      info = NULL,
      icon = icon("headphones"),
      href = NULL
    )
  })

  ## Value box for average daily audio volume

  output$avg_audio_volume <- renderValueBox({
    customValueBox(
      value = paste0(avg_audio_volume() %>% lubridate::seconds_to_period() %>% round() %>% as.character()),
      title = toupper("Average Daily Audio"),
      subtitle = avg_audio_change(),
      color = "light-blue",
      info = NULL,
      icon = icon("clock"),
      href = NULL
    )
  })

  ## Value box for average accuracy
  output$avg_accuracy <- renderValueBox({
    customValueBox(
      value = paste0(avg_accuracy() %>% as.character(), " % "),
      title = toupper("Average Accuracy"),
      subtitle = avg_accuracy_change(),
      color = "light-blue",
      icon = icon("percent"),
      href = NULL
    )
  })

  ## Value box for average audio length
  output$avg_audio_length <- renderValueBox({
    customValueBox(
      value = paste0(avg_audio_length() %>%
        lubridate::seconds_to_period() %>% round() %>%
        as.character()),
      title = toupper("Average Audio Length"),
      subtitle = avg_audio_length_change(),
      color = "light-blue",
      info = NULL,
      icon = icon("clock"),
      href = NULL
    )
  })

  ## Value box for word count
  output$avg_word_count <- renderValueBox({
    customValueBox(
      value = paste0(avg_word_count() %>% as.character(), " Words"),
      title = toupper("Average Word Count"),
      subtitle = avg_word_count_change(),
      color = "light-blue",
      info = NULL,
      icon = icon("pencil"),
      href = NULL
    )
  })


  ## Transcription  distribution pie chart

  tx_distr_df <- reactive({
    trial_df() %>%
      dplyr::mutate(used_machine_tx = replace_na(used_machine_tx, FALSE)) %>%
      janitor::tabyl(used_machine_tx) %>%
      mutate(percent = percent * 100) %>%
      mutate(used_machine_tx = replace(used_machine_tx, used_machine_tx == TRUE, "Machine Transcription")) %>%
      mutate(used_machine_tx = replace(used_machine_tx, used_machine_tx == FALSE, "Manual Transcription"))
  })

  tx_distr_plot <- reactive({
    plotly::plot_ly(tx_distr_df(), labels = ~used_machine_tx, values = ~n, type = "pie") %>%
      layout(
        title = "Transcription Distribution",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      config(displayModeBar = F)
  })

  ## Display the pie chart
  output$tx_distr_plot <- renderPlotly({
    tx_distr_plot()
  })

  ## Code for Transcription Insights(Time Taken and Accuracy) based on types

  machine_tx_time_taken_per_min <- reactive({
    trial_df() %>%
      filter(status == "Completed") %>%
      filter(used_machine_tx == TRUE & qf_end_time_in_seconds >= 0 & qf_end_time_in_seconds < 7200) %>%
      summarise(round(sum(qf_end_time_in_seconds) / sum(audio_length_seconds), 2))
  })

  compared_machine_tx_time_taken_per_min <- reactive({
    compare_machine_df() %>%
      filter(status == "Completed") %>%
      filter(used_machine_tx == TRUE & qf_end_time_in_seconds >= 0 & qf_end_time_in_seconds < 7200) %>%
      summarise(round(sum(qf_end_time_in_seconds) / sum(audio_length_seconds), 2))
  })

  changed_machine_tx_time_taken_per_min <- reactive({
    change_value <- (machine_tx_time_taken_per_min() - compared_machine_tx_time_taken_per_min()) / compared_machine_tx_time_taken_per_min()
    change_value * 100
  })



  manual_tx_time_taken_per_min <- reactive({
    machine_df() %>%
      filter(status == "Completed") %>%
      filter(is.na(used_machine_tx) & qf_end_time_in_seconds >= 0 & qf_end_time_in_seconds < 7200) %>%
      summarise(round(sum(qf_end_time_in_seconds) / sum(audio_length_seconds), 2))
  })

  compared_manual_tx_time_taken_per_min <- reactive({
    compare_machine_df() %>%
      filter(status == "Completed") %>%
      filter(is.na(used_machine_tx) & qf_end_time_in_seconds >= 0 & qf_end_time_in_seconds < 7200) %>%
      summarise(round(sum(qf_end_time_in_seconds) / sum(audio_length_seconds), 2))
  })

  changed_manual_tx_time_taken_per_min <- reactive({
    change_value <- (manual_tx_time_taken_per_min() - compared_manual_tx_time_taken_per_min()) / compared_manual_tx_time_taken_per_min()
    change_value * 100
  })


  overall_tx_time_taken_per_min <- reactive({
    machine_df() %>%
      filter(status == "Completed") %>%
      filter(qf_end_time_in_seconds >= 0 & qf_end_time_in_seconds < 7200) %>%
      summarise(round(sum(qf_end_time_in_seconds) / sum(audio_length_seconds), 2))
  })

  compared_overall_tx_time_taken_per_min <- reactive({
    compare_machine_df() %>%
      filter(status == "Completed") %>%
      filter(qf_end_time_in_seconds >= 0 & qf_end_time_in_seconds < 7200) %>%
      summarise(round(sum(qf_end_time_in_seconds) / sum(audio_length_seconds), 2))
  })

  changed_overall_tx_time_taken_per_min <- reactive({
    change_value <- (overall_tx_time_taken_per_min() - compared_overall_tx_time_taken_per_min()) / compared_overall_tx_time_taken_per_min()
    change_value * 100
  })



  avg_ar_time_taken_per_min <- reactive({
    machine_df() %>%
      filter(audio_review_end_time_in_seconds >= 0 & audio_review_end_time_in_seconds < 3600) %>%
      summarise(round(sum(audio_review_end_time_in_seconds) / sum(audio_length_seconds), 2))
  })


  compared_avg_ar_time_taken_per_min <- reactive({
    compare_machine_df() %>%
      filter(audio_review_end_time_in_seconds >= 0 & audio_review_end_time_in_seconds < 3600) %>%
      summarise(round(sum(audio_review_end_time_in_seconds) / sum(audio_length_seconds), 2))
  })

  changed_avg_ar_time_taken_per_min <- reactive({
    change_value <- (avg_ar_time_taken_per_min() - compared_avg_ar_time_taken_per_min()) / compared_avg_ar_time_taken_per_min()
    change_value * 100
  })


  ## Accuracy Insights calculation

  machine_tx_accuracy <- reactive({
    machine_df() %>%
      filter(status == "Completed") %>%
      filter(!is.na(used_machine_tx) & accuracy > .85) %>%
      summarise(round(mean(accuracy, na.rm = TRUE) * 100, 2))
  })

  compared_machine_tx_accuracy <- reactive({
    compare_machine_df() %>%
      filter(status == "Completed") %>%
      filter(!is.na(used_machine_tx) & accuracy > .85) %>%
      summarise(round(mean(accuracy, na.rm = TRUE) * 100, 2))
  })

  changed_machine_tx_accuracy <- reactive({
    change_value <- (machine_tx_accuracy() - compared_machine_tx_accuracy()) / compared_machine_tx_accuracy()
    change_value * 100
  })



  manual_tx_accuracy <- reactive({
    machine_df() %>%
      filter(status == "Completed") %>%
      filter(is.na(used_machine_tx & accuracy > .85)) %>%
      summarise(round(mean(accuracy, na.rm = TRUE) * 100, 2))
  })

  compared_manual_tx_accuracy <- reactive({
    compare_machine_df() %>%
      filter(status == "Completed") %>%
      filter(is.na(used_machine_tx & accuracy > .85)) %>%
      summarise(round(mean(accuracy, na.rm = TRUE) * 100, 2))
  })

  changed_manual_tx_accuracy <- reactive({
    change_value <- (manual_tx_accuracy() - compared_manual_tx_accuracy()) / compared_manual_tx_accuracy()
    change_value * 100
  })

  overall_avg_accuracy <- reactive({
    machine_df() %>%
      filter(status == "Completed") %>%
      filter(accuracy > .85) %>%
      summarise(round(mean(accuracy, na.rm = TRUE) * 100, 2))
  })

  compared_overall_avg_accuracy <- reactive({
    compare_machine_df() %>%
      filter(status == "Completed") %>%
      filter(accuracy > .85) %>%
      summarise(round(mean(accuracy, na.rm = TRUE) * 100, 2))
  })

  changed_overall_avg_accuracy <- reactive({
    change_value <- (overall_avg_accuracy() - compared_overall_avg_accuracy()) / compared_overall_avg_accuracy()
    change_value * 100
  })



  machine_tx_error_rate <- reactive({
    machine_df() %>%
      summarise(round(mean(machine_and_manual_tx_error_rate, na.rm = TRUE) * 100, 2))
  })

  compared_machine_tx_error_rate <- reactive({
    compare_machine_df() %>%
      summarise(round(mean(machine_and_manual_tx_error_rate, na.rm = TRUE) * 100, 2))
  })

  changed_machine_tx_error_rate <- reactive({
    change_value <- (machine_tx_error_rate() - compared_machine_tx_error_rate()) / compared_machine_tx_error_rate()
    change_value * 100
  })

  ## Machine TX time taken insight

  output$machine_tx_time_taken <- renderText({
    paste0(machine_tx_time_taken_per_min() %>%
      as.character())
  })

  # output$compared_machine_tx_time_taken <- renderText({
  #   paste0(compared_machine_tx_time_taken_per_min() %>%
  #     as.character())
  # })

  output$changed_machine_tx_time_taken <- renderUI({
    change_checker(changed_machine_tx_time_taken_per_min())
  })

  output$manual_tx_time_taken <- renderText({
    paste0(manual_tx_time_taken_per_min() %>%
      as.character())
  })


  # output$compared_manual_tx_time_taken <- renderText({
  #   paste0(compared_manual_tx_time_taken_per_min() %>%
  #     as.character())
  # })

  output$changed_manual_tx_time_taken <- renderUI({
    change_checker(changed_manual_tx_time_taken_per_min())
  })


  output$overall_tx_time_taken <- renderText({
    paste0(overall_tx_time_taken_per_min() %>%
      as.character())
  })


  # output$compared_overall_tx_time_taken <- renderText({
  #   paste0(compared_overall_tx_time_taken_per_min() %>%
  #     as.character())
  # })


  output$changed_overall_tx_time_taken <- renderUI({
    change_checker(changed_overall_tx_time_taken_per_min())
  })

  output$avg_ar_time_taken <- renderText({
    paste0(avg_ar_time_taken_per_min() %>%
      as.character())
  })

  # output$compared_avg_ar_time_taken <- renderText({
  #   paste0(compared_avg_ar_time_taken_per_min() %>%
  #     as.character())
  # })

  output$changed_avg_ar_time_taken <- renderText({
    change_checker(changed_avg_ar_time_taken_per_min())
  })



  ## Machine TX accuracy insight

  output$machine_tx_accuracy <- renderText({
    paste0(machine_tx_accuracy() %>%
      as.character(), " %")
  })

  # output$compared_machine_tx_accuracy <- renderText({
  #   paste0(compared_machine_tx_accuracy() %>%
  #     as.character(), " %")
  # })

  output$changed_machine_tx_accuracy <- renderText({
    change_checker(changed_machine_tx_accuracy())
  })



  output$manual_tx_accuracy <- renderText({
    paste0(manual_tx_accuracy() %>%
      as.character(), " %")
  })

  # output$compared_manual_tx_accuracy <- renderText({
  #   paste0(compared_manual_tx_accuracy() %>%
  #     as.character(), " %")
  # })

  output$changed_manual_tx_accuracy <- renderText({
    change_checker(changed_manual_tx_accuracy())
  })



  output$overall_avg_accuracy <- renderText({
    paste0(overall_avg_accuracy() %>%
      as.character(), " %")
  })

  # output$compared_overall_avg_accuracy <- renderText({
  #   paste0(compared_overall_avg_accuracy() %>%
  #     as.character(), " %")
  # })

  output$changed_overall_avg_accuracy <- renderText({
    change_checker(changed_overall_avg_accuracy())
  })



  output$machine_tx_error_rate <- renderText({
    paste0(machine_tx_error_rate() %>%
      as.character(), " %")
  })

  # output$compared_machine_tx_error_rate <- renderText({
  #   paste0(compared_machine_tx_error_rate() %>%
  #     as.character(), " %")
  # })

  output$changed_machine_tx_error_rate <- renderText({
    change_checker(changed_machine_tx_error_rate())
  })




  ## spot award graph

  ## Quick Fire Volume
  output$most_qf_volume <- renderHighchart({
    most_qf_volume <- trial_df() %>%
      filter(accuracy > .85) %>%
      group_by(workflow_quickfire_user) %>%
      summarise(total_volume = round(sum(audio_length_seconds) / 3600, 2)) %>%
      arrange(-total_volume) %>%
      rename("transcriber" = workflow_quickfire_user) %>%
      filter(transcriber !="ujwalshrestha19@gmail.com")%>%
      head(10) %>%
      left_join(transcriber_name, by = "transcriber")


    hc <- hchart(most_qf_volume, "bar", hcaes(x = name, y = total_volume), color = "#7fc97f", name = "Audio Time (Hr) ",
                 dataLabels = list(
                   enabled = TRUE)) %>%
      hc_credits(enabled = FALSE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "Time in Hrs"))

    hc
  })


  ## Most AR Volume
  output$most_ar_volume <- renderHighchart({
    most_ar_volume <- trial_df() %>%
      filter(accuracy > .85) %>%
      group_by(workflow_peer_audio_review_user) %>%
      summarise(total_volume = round(sum(audio_length_seconds) / 3600, 2)) %>%
      arrange(-total_volume) %>%
      rename("transcriber" = workflow_peer_audio_review_user) %>%
      filter(transcriber !="ujwalshrestha19@gmail.com")%>%
      head(10) %>%
      left_join(transcriber_name, by = "transcriber")


    hc <- hchart(most_ar_volume, "bar", hcaes(x = name, y = total_volume), color = "#DD8D29", name = "Audio Time (Hr) ",
                 dataLabels = list(
                   enabled = TRUE)) %>%
      hc_credits(enabled = FALSE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "Time in Hrs"))

    hc
  })





  ## Fastest transcriber using machine TX
  output$fast_machine_tx <- renderHighchart({
    fast_machine_tx <- trial_df() %>%
      filter(qf_end_time_in_seconds < 7200 & word_count > 15 & used_machine_tx == TRUE) %>%
      group_by(workflow_quickfire_user) %>%
      summarise(tx_time = round(sum(qf_end_time_in_seconds) / sum(audio_length_seconds), 2), audio_user_time = sum(audio_length_seconds)) %>%
      filter(audio_user_time > 1800) %>%
      arrange(tx_time) %>%
      rename("transcriber" = workflow_quickfire_user)%>%
      filter(transcriber !="ujwalshrestha19@gmail.com")%>%
      head(10) %>%
      left_join(transcriber_name, by = "transcriber")

    hc <- hchart(fast_machine_tx, "bar", hcaes(x = reorder(name, -tx_time), y = tx_time), color = "#1f78b4", name = "Time taken per min of audio ",
                 dataLabels = list(
                   enabled = TRUE)) %>%
      hc_credits(enabled = FALSE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "Time taken per minute of audio"))
    hc
  })




  ## Fastest transcriber using manual TX
  output$fast_manual_tx <- renderHighchart({
    fast_manual_tx <- trial_df() %>%
      filter(qf_end_time_in_seconds < 7200 & word_count > 15 & is.na(used_machine_tx)) %>%
      group_by(workflow_quickfire_user) %>%
      summarise(tx_time = round(sum(qf_end_time_in_seconds) / sum(audio_length_seconds), 2), audio_user_time = sum(audio_length_seconds)) %>%
      arrange(tx_time) %>%
      rename("transcriber" = workflow_quickfire_user)%>%
      filter(transcriber !="ujwalshrestha19@gmail.com")%>%
      head(10) %>%
      left_join(transcriber_name, by = "transcriber")


    hc <- hchart(fast_manual_tx, "bar", hcaes(x = reorder(name, -tx_time), y = tx_time), color = "#fb8072", name = "Time taken per min of audio ",
                 dataLabels = list(
                   enabled = TRUE)) %>%
      hc_credits(enabled = FALSE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "Time taken per minute of audio"))
    hc
  })



  ## List of transcriber with best average accuracy
  output$best_accuracy <- renderDT({
    best_accuracy_df <- trial_df() %>%
      filter(accuracy > 0.85) %>%
      group_by(workflow_quickfire_user) %>%
      summarise(accuracy = round(mean(accuracy) * 100, 2), audio_sum = sum(audio_length_seconds)) %>%
      filter(audio_sum > 1800) %>%
      arrange(-accuracy) %>%
      rename("transcriber" = workflow_quickfire_user) %>%
      filter(transcriber !="ujwalshrestha19@gmail.com")%>%
      head(10) %>%
      select(-audio_sum) %>%
      left_join(transcriber_name, by = "transcriber") %>%
      select(name, accuracy) %>%
      rename("Transcriber" = name, "Accuracy" = accuracy)

    datatable(best_accuracy_df,
      options = list(dom = "t"),
      class = "cell-border stripe"
    )
  })

  ## List of transcriber with top average audio review accuracy
  output$audio_review_accuracy <- renderDT({
    best_audio_review_df <- trial_df() %>%
      group_by(workflow_peer_audio_review_user) %>%
      summarise(accuracy = round(mean(second_review_accuracy, na.rm = TRUE) * 100, 2)) %>%
      arrange(-accuracy) %>%
      rename("transcriber" = workflow_peer_audio_review_user) %>%
      filter(transcriber !="ujwalshrestha19@gmail.com")%>%
      head(10) %>%
      left_join(transcriber_name, by = "transcriber") %>%
      select(name, accuracy) %>%
      rename("Transcriber" = name, "Accuracy" = accuracy)
    
    datatable(best_audio_review_df,
              options = list(dom = "t"),
              class = "cell-border stripe"
    )
  })
  


  # Trend Analysis Section
  # preorder vs workdone
  test <- data.frame(
    month = c("Jan", "Feb", "March", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov"),
    Workdone = c(100, 138, 217, 232, 259, 289, 400, 605, 497, 441,403),
    Preorder = c(368, 425, 578, 552, 713, 783, 744, 855, 618, 832,700)
  )
  test2 <- reshape2::melt(test)

  test2$month <- factor(test2$month, levels = c("Jan", "Feb", "March", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov"))


  output$preorder_workdone <- renderHighchart({
    hc <- hchart(test2, "column", hcaes(x = month, y = value, group = variable), color = c("#7CB5EC", "#F7A35C"), name = c("Workdone", "Preorder")) %>%
      hc_credits(enabled = FALSE) %>%
      hc_xAxis(title = list(text = "Month")) %>%
      hc_yAxis(title = list(text = ""))
  })

  
  # preorder_fulfilled
  df_preorder_fulfilled <- data.table::fread("data/preorder_fulfilled.csv") %>% as.data.frame()
  df_preorder_fulfilled$Months <- factor(df_preorder_fulfilled$Month, levels = month.abb)


  output$preorder_fulfilled <- renderHighchart({
    hc <- hchart(df_preorder_fulfilled, "line", hcaes(x = Months, y = Percentage), color = "#377eb8", name = "Percentage") %>%
      hc_credits(enabled = FALSE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "Percentage of Preorder"))
    hc
  })


  ## Reactive Data for the weekdays trend
  weekday_tx_df <- reactive({
    infile <- input$main_df
    if (is.null(infile)) {
      df %>%
        as.data.frame() %>%
        janitor::clean_names() %>%
        mutate(created_on_date = lubridate::as_datetime(created_on / 1000)) %>%
        filter(lubridate::date(created_on_date) >= as.character(input$weekdayRange[1]) & lubridate::date(created_on_date) <= as.character(input$weekdayRange[2]))
    }
    else {
      new_df() %>%
        janitor::clean_names() %>%
        mutate(created_on_date = lubridate::as_datetime(created_on / 1000)) %>%
        filter(lubridate::date(created_on_date) >= as.character(input$weekdayRange[1]) & lubridate::date(created_on_date) <= as.character(input$weekdayRange[2]))
    }
  })
  
  weekday_pre_df <- reactive({
    pre_order_df%>%
      mutate(created_on_date = lubridate::as_datetime(created_on))%>%
      filter(lubridate::date(created_on_date) >= as.character(input$weekdayRange[1]) & lubridate::date(created_on_date) <= as.character(input$weekdayRange[2]))
  })


  output$workdone_weekdays_volume <- renderHighchart({
    # weekday preorder / Taken order
    pre_data <- weekday_pre_df()%>%
      mutate(order_weekday = lubridate::wday(created_on_date,label = TRUE))
    
    total_order_data <- pre_data %>% group_by(order_weekday)%>%
      summarise(total_order = n())
    
    accepted_order_data <- pre_data %>% filter(order_id !="")%>%group_by(order_weekday)%>%
      summarise(accepted_order = n())
    
    final_order_data <- left_join(total_order_data,accepted_order_data, by = "order_weekday")
    
    final_order_data<- final_order_data%>%
      tidyr::pivot_longer(!order_weekday, names_to = "Type", values_to = "count")
    
    
    workdone_weekdays_df <- weekday_tx_df() %>%
      mutate(work_weekday = lubridate::wday(created_on_date, label = TRUE)) %>%
      group_by(work_weekday) %>%
      summarise(duration = round(sum(audio_length_seconds) / 3600))

    
    hc <- hchart(final_order_data, "column", hcaes(x = order_weekday, y = count , group = Type), 
                 color = c("#a6d854","#fc8d62"),
                 name = c("Accepted Order ", "Total Order "),
                 showInLegend = c(TRUE , TRUE)) %>%
      hc_add_series(workdone_weekdays_df, "line", color = "#8da0cb",
                    hcaes(x= work_weekday, y = duration),
                    name = "Duration (Hrs) ",
                    showInLegend = TRUE)
  
    
    hc
  })
  
  ## Reactive Data for the time load graph
  time_load_df <- reactive({
    infile <- input$main_df
    if (is.null(infile)) {
      df %>%
        as.data.frame() %>%
        janitor::clean_names() %>%
        mutate(created_on_date = lubridate::as_datetime(created_on / 1000)) %>%
        filter(lubridate::date(created_on_date) >= as.character(input$time_load_date[1]) & lubridate::date(created_on_date) <= as.character(input$time_load_date[2]))
    }
    else {
      new_df() %>%
        janitor::clean_names() %>%
        mutate(created_on_date = lubridate::as_datetime(created_on / 1000)) %>%
        filter(lubridate::date(created_on_date) >= as.character(input$time_load_date[1]) & lubridate::date(created_on_date) <= as.character(input$time_load_date[2]))
    }
  })
  
  time_load_df_table <- reactive({
    time_load_df() %>%
      mutate(created_on_time = hms::as_hms(lubridate::round_date(created_on_date,"hours"))) %>%
      group_by(created_on_time) %>%
      summarise(n = n())
  })
  
  # Time Load Transcription Graph
   output$time_load_graph <- renderPlotly({
      fig <- plot_ly(time_load_df_table(), x = ~created_on_time, y = ~n, type = 'scatter', mode = 'lines+markers') %>%
        layout(
          title = "",
          xaxis = list(title="Time Value"),
          yaxis = list(title="No. of Transcription Count")
        ) %>%
        config(displayModeBar = FALSE)
      fig
   })
   

  ## Reactive Data in Transcriber Section
  
  user_df <- reactive({
    input$file_update
    infile <- input$main_df
    if (is.null(infile)) {
      data <- isolate(df %>%
                        as.data.frame() %>%
                        janitor::clean_names() %>%
                        mutate(created_on_date = lubridate::as_datetime(created_on / 1000)) %>%
                        filter(lubridate::date(created_on_date) >= as.character(input$inputdateuser[1]) & lubridate::date(created_on_date) <= as.character(input$inputdateuser[2])))
    }
    else {
      data <- isolate(
        new_df() %>%
          janitor::clean_names() %>%
          mutate(created_on_date = lubridate::as_datetime(created_on / 1000)) %>%
          filter(lubridate::date(created_on_date) >= as.character(input$inputdateuser[1]) & lubridate::date(created_on_date) <= as.character(input$inputdateuser[2]))
      )
    }
  })
  
  # Section for Bubble Chart
  output$bubble_chart <- renderHighchart({
    bubble_chart <- df_grouping()
    hc <- hchart(bubble_chart, "packedbubble", hcaes(name = email, value = total_vol, group = group)) %>%
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "<b>Email:{point.name}:</b> Total_Volume:{point.value}"
      )%>%
      hc_title(
        text = "Total Audio Volume per Transcriber",
        margin = 15,
        align = "left",
        style = list(color = "#da86c5", useHTML = TRUE)
      ) %>%
      hc_plotOptions(
        packedbubble = list(
          minSize = "50%",
          maxSize = "300%",
          zMin = 0,
          zMax = 1000
        )
      )
    hc
  })  
  
  
  # Section for Transcriber performance Table
  
  transcriber_qf <- transcriber_name %>%
    rename(workflow_quickfire_user = transcriber)
  
  transcriber_tr <- transcriber_name %>%
    rename(workflow_peer_text_review_user = transcriber)
  
  transcriber_ar <- transcriber_name %>%
    rename(workflow_peer_audio_review_user = transcriber)
  
  transcriber_sr <- transcriber_name %>%
    rename(workflow_second_review_user = transcriber)
  
  #for quickfire
  user_qf <- reactive({ 
    left_join(transcriber_qf, user_df(), "workflow_quickfire_user") %>%
      group_by(workflow_quickfire_user) %>%
      summarize(volume = round(sum(audio_length_seconds, na.rm = TRUE)/3600,3)) %>%
      rename(email = workflow_quickfire_user) %>%
      as.data.frame()
  })
  #for text review
  user_tr <- reactive({ 
    left_join(transcriber_tr, user_df(), "workflow_peer_text_review_user") %>%
      group_by(workflow_peer_text_review_user) %>%
      summarize(volume = round(sum(audio_length_seconds, na.rm = TRUE)/3600,3)) %>%
      rename(email = workflow_peer_text_review_user)%>%
      as.data.frame()
  })
  
  #for audio review
  user_ar <- reactive({
    left_join(transcriber_ar, user_df(), "workflow_peer_audio_review_user") %>%
      group_by(workflow_peer_audio_review_user) %>%
      summarize(volume = round(sum(audio_length_seconds, na.rm = TRUE)/3600,3)) %>%
      rename(email = workflow_peer_audio_review_user)
  })
  
  #for second review
  user_sr <- reactive({
    left_join(transcriber_sr, user_df(), "workflow_second_review_user") %>%
      group_by(workflow_second_review_user) %>%
      summarize(volume = round(sum(audio_length_seconds, na.rm = TRUE)/3600,3)) %>%
      rename(email = workflow_second_review_user)
  })
  
  # for accuracy
  user_acc <- reactive({
    left_join(transcriber_qf, user_df(), "workflow_quickfire_user") %>%
      group_by(workflow_quickfire_user) %>%
      summarize(accuracy = round(mean(accuracy,na.rm = TRUE),3)*100) %>%
      rename(email = workflow_quickfire_user)
  })
  
  df_qf_tr <-reactive({ 
    left_join(user_qf(), user_tr(), "email", suffix = c("_quickfire", "_text_review")) %>%
      as.data.frame()
  })
  
  df_ar_sr <- reactive({
    left_join(user_ar(), user_sr(),"email",suffix = c("_audio_review", "_second_review")) %>%
      as.data.frame()
  })
  
  df_qf_tr_ar_sr <- reactive({
    left_join(df_qf_tr(), df_ar_sr(), "email") %>%
      as.data.frame()
  })
  
  df_all <- reactive({
    left_join(df_qf_tr_ar_sr(), user_acc(), "email") %>%
      as.data.frame()
  })
  
  # removing empty emails

  df_final_table <- reactive({
    df_all() %>%
      filter(!is.na(email) & email != "") %>%
      mutate(total_vol = volume_quickfire+volume_text_review+volume_audio_review+volume_second_review)# %>%
      #rename("Email" = email,"QuickFire Vol" = 'volume_quickfire', "Text Review Vol" = 'volume_text_review', "Audio Review Vol" = 'volume_audio_review',"Second Review Vol" = "volume_second_review", "Accuracy" = "accuracy")
  })
  
  df_grouping <- reactive({
    df_final_table() %>%
      mutate(group = case_when(
        total_vol < 100 ~ 'less_than_hundred',
        (total_vol >= 100 & total_vol < 500) ~ 'between_hundred_and_five_hundred',
        total_vol >= 500 ~ 'more_than_five_hundred'
      ))
  })
    
    output$transcriber_volume <- renderDT({
      
      df_final_table()
    })




  ## Reactive Data for the weekdays trend
  weekday_df <- reactive({
    infile <- input$main_df
    if (is.null(infile)) {
      df %>%
        as.data.frame() %>%
        janitor::clean_names() %>%
        mutate(created_on_date = lubridate::as_datetime(created_on / 1000)) %>%
        filter(lubridate::date(created_on_date) >= as.character(input$weekdayRange[1]) & lubridate::date(created_on_date) <= as.character(input$weekdayRange[2]))
    }
    else {
      new_df() %>%
        janitor::clean_names() %>%
        mutate(created_on_date = lubridate::as_datetime(created_on / 1000)) %>%
        filter(lubridate::date(created_on_date) >= as.character(input$weekdayRange[1]) & lubridate::date(created_on_date) <= as.character(input$weekdayRange[2]))
    }

  })
  
}
