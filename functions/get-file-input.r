# Wrapper for the fileInput function
get_file_input <- function() {
  return(
    fileInput(
      inputId = "import_scenario", 
      label = "Scenario data",
      multiple = FALSE,
      accept = allowed_file_extensions,
      buttonLabel = "Import...",
      placeholder = ".xls(x) files allowed"
    )
  )
}