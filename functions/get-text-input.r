# Wrapper for the textInput function
get_text_input <- function(input_id, label, value = NULL, width = "100%") {
  return(
    textInput(
      inputId = input_id,
      label = label,
      value = value,
      width = width
    )
  )
}