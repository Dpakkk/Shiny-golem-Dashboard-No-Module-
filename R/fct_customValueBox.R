customValueBox <- function(value, title, subtitle=NULL, info = NULL,icon = NULL, color = "aqua", width = 4, href = NULL) 
  {
  shinydashboard:::validateColor(color)

  if (!is.null(icon)) {
    shinydashboard:::tagAssert(icon, type = "i")
  }

  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    # bs3 pull-right
    # bs4 float-right
    class = "pull-right float-right"
  )
  
  if(!is.null(subtitle)){
    if (subtitle >  0) {
      subtitle <- tagList(HTML("&uarr;"), abs(subtitle), "%")
      compare_style <- "color:green;"
    }
    else if(subtitle < 0){
      subtitle <- tagList(HTML("&darr;"), abs(subtitle), "%")
      compare_style <- "color:red;"
    }
    
  }
  else{
    subtitle <- tagList(HTML(""))
    compare_style <- "color:white;"
  }


  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title), info_icon,
      h3(value),
      p(subtitle, style = compare_style)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
  )

  if (!is.null(href)) {
    boxContent <- a(href = href, boxContent)
  }

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}
