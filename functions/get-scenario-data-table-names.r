# Get scenario (Excel) output data table names
get_scenario_data_table_names <- function(scenario_files) {
  # If there are multiple scenarios, iterate through one at a time and get the data table names
  if(length(scenario_files) > 1) {
    # Bind data table names from each scenario
    data_tables <- bind_rows(
      lapply(
        scenario_files, 
        as.data.frame(excel_sheets))
    )
    
    # Get data table name counts
    counts <- as.data.frame(
      table(data_tables),
      stringsAsFactors = FALSE
    )
    
    # Get common data table names and sort them 
    data_tables <- sort(
      as.vector(
        counts$data_tables[counts$Freq == length(scenario_files)]
      )
    )
  } else {
    # Sort data table names
    data_tables <- sort(excel_sheets(scenario_files))
  }
  
  # Return the results
  return(data_tables)
}