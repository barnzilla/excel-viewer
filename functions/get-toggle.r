# Wrapper for the prettySwitch function
get_toggle <- function(input_id, label, status = "info", value = FALSE) {
  return(
    prettySwitch(
      inputId = input_id,
      label = label,
      status = status,
      value = value,
      fill = TRUE,
      width = "100%"
    )
  )
}