# Remove the file extension from a file name
remove_file_extension <- function(file_name) {
  # Remove the last period and everything to the right of it
  output <- unname(
    sapply(
      file_name, 
      function(x) {
        strsplit(x, "[.]")[[1]][-length(strsplit(x, "[.]")[[1]])]
      }
    )
  )
  
  # Return the results
  return(output)
}
