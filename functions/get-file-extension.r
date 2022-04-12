# Get the file extension (without the leading period) of a file name
get_file_extension <- function(file_name) {
  return(
    strsplit(
      file_name, 
      "[.]"
    )[[1]][length(strsplit(file_name, "[.]")[[1]])]
  )
}
