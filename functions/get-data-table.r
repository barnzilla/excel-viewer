# Get data table from scenario (Excel) output
get_data_table <- function(data_path, sheet_name, rounding_digits = 3) {
  # Import data table
  d <- bind_rows(
    lapply(
      data_path,
      function(x) {
        if(sheet_name %in% excel_sheets(x)) {
          bind_cols(
            Scenario = remove_file_extension(gsub("scenarios/", "", x)),
            read_xlsx(
              path = x,
              sheet = sheet_name
            )
          )
        }
      }
    )
  )
  
  # Round any vectors that are numeric
  d <- bind_cols(
    lapply(
      d, 
      function(x) {
        if(class(x) == "numeric") {
          round(x, rounding_digits)
        } else {
          x
        }
      }
    )
  )
  
  # Coerce any character vectors to factors
  d <- bind_cols(
    lapply(
      d, 
      function(x) {
        if(class(x) == "character") {
          factor(x)
        } else {
          x
        }
      }
    )
  )
  
  # Update vector names
  names(d) <- trimws(tolower(names(d)))
  names(d) <- gsub(" |[-]", "_", names(d))
  names(d) <- gsub("[=]", "_equals_", names(d))
  names(d) <- gsub("[,]", "_to_", names(d))
  names(d) <- gsub("[(]|[)]|[[]|[]]| ", "", names(d))
  names(d) <- gsub("<=", "less_than_equal_to_", names(d))
  names(d) <- gsub("<", "less_than_", names(d))
  names(d) <- gsub(">=", "greater_than_equal_to_", names(d))
  names(d) <- gsub(">", "greater_than_", names(d))
  names(d) <- gsub("__", "_", names(d))
  names(d) <- gsub("_$", "", names(d))
  
  # Return results
  return(d)
}