# Get timestamp of current time in format yyyy-mm-dd hh-mm-ss
get_current_timestamp <- function() {
  return(
    paste0(
      Sys.Date(), 
      " ", 
      gsub(
        ":", 
        "-", 
        strsplit(
          as.character(Sys.time()), 
          " "
        )[[1]][2]
      )
    )
  )
}
