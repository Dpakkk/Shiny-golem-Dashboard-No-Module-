
## Check the change between previous and current values and assign color to the value

change_checker <- function(change_value)
{
  if(change_value < 0)
  {
    return_value <- HTML(paste0('<p style= \"color: red; font-weight:600;">', "&darr; ",change_value%>%round(2)%>%abs()%>%as.character()," %",'</p>'))
    return(return_value)
  }
  else if(change_value > 0)
  {
    return_value <- HTML(paste0('<p style= \"color: green; font-weight:600;">', "&uarr; ",change_value%>%round(2)%>%abs()%>%as.character()," %",'</p>'))
    return(return_value)
  }
  else{
    return_value <- HTML(paste0('<p style= \"color: blue; font-weight:600;">',change_value%>%round(2)%>%abs()%>%as.character()," %",'</p>'))
    return(return_value)
  }
}