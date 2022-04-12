# Wrapper for the selectInput function
get_select_input <- function(input_id, label, choices, selected = "", multiple = TRUE) {
  # Wrangle the choices and selected vectors
  choices <- unname(unlist(unique(choices)))
  selected <- unname(unlist(unique(selected)))
  
  # If there are choices, render the select input, else return NULL 
  if(length(choices) > 0) {
    return(
      selectInput(
        inputId = input_id,
        label = label,
        choices = choices,
        selected = selected,
        selectize = FALSE,
        multiple = multiple,
        size = ifelse(length(choices) > 7, 7, length(choices))
      )
    )
  } else {
    return(NULL)
  }
}