# Get file name that corresponds to scenario name
match_scenario_name_to_file_name <- function(scenario) {
  # Return results
  return(
    unlist(
      sapply(
        scenario, 
        function(x) {
          list.files(
            "scenarios", 
            pattern = allowed_file_extensions
          )[which(str_detect(remove_file_extension(list.files("scenarios", pattern = allowed_file_extensions)), paste0("^", gsub("([()])", "\\\\\\1", x), "$")))]
        }
      )
    )
  )
}
