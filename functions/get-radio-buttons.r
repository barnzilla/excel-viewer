# Wrapper for the awesomeRadio function
get_radio_buttons <- function(input_id, label, choices, selected = NULL, status = "info") {
  return(
    awesomeRadio(
      inputId = input_id,
      label = label,
      choices = choices,
      selected = ifelse(is.null(selected), choices[1], selected),
      status = status
    )
  )
}