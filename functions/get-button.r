# Wrapper for the actionButton function
get_button <- function(input_id, label, icon = NULL, class = "pohem-viewer-button") {
  return(
    actionButton(
      inputId = input_id,
      label = label,
      icon = icon,
      class = class
    )
  )
}