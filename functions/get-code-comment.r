# Wrap a text in a span tag that will style the text as a code comment
get_code_comment <- function(text) {
  return(
    paste0("<span class='code-comment'># ", text, "</span>")
  )
}
