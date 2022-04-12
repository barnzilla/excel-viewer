# Get scenario (Excel) file names and remove the file name extensions
get_scenario_names <- function(pattern = allowed_file_extensions, limit = NULL) {
  # Get scenario files
  scenario_files <- sort(
    list.files("scenarios/", pattern = pattern)
  )
  
  # If files exists, strip file extensions, else return nothing
  if(length(scenario_files) > 0) {
    if(is.numeric(limit)) {
      return(
        remove_file_extension(scenario_files)[1:limit]
      )
    } else {
      return(
        remove_file_extension(scenario_files)
      )
    }
  } else {
    return(NULL)
  }
}