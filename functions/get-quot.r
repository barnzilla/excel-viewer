# Wrap a text in the HTML entity version of quotes
get_quot <- function(text, trailing_text = NULL) {
  return(
    paste0("&quot;", text, "&quot;", trailing_text)
  )
}
