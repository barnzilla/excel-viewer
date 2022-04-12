# Wrapper for the sliderTextInput function
get_slider <- function(input_id, label, min, max, value, separator = ",", width = "100%") {
  return(
    sliderInput(
      inputId = input_id,
      label = label,
      min = min,
      max = max,
      value = value,
      sep = separator,
      width = width
    )
  )
}