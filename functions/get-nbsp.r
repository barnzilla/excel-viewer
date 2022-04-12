# Return the non-breaking space HTML entity x number of times
get_nbsp <- function(x, trailing_text = NULL) {
  return(
    paste0(
      paste0(
        rep("&nbsp;", x), 
        collapse = ""
      ), 
      trailing_text
    )
  )
}
