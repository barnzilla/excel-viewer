# Wrapper for the pickerInput function
get_drop_down <- function(input_id, label, choices, selected = "", actions_box = TRUE, live_search = TRUE, multiple = TRUE, max_options = FALSE, width = "100%") {
  # If there are choices, render the drop-down menu, else return NULL 
  if(length(choices) > 0) {
    return(
      pickerInput(
        inputId = input_id,
        label = label,
        choices = choices,
        selected = unlist(selected),
        options = pickerOptions(
          actionsBox = actions_box,
          liveSearch = live_search,
          noneSelectedText = "Please select...",
          maxOptions = max_options
        ),
        multiple = multiple,
        width = width
      )
    )
  } else {
    return(NULL)
  }
}