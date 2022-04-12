# Get number of plot rows
# Credit: https://stackoverflow.com/questions/44107119/extract-number-of-rows-from-faceted-ggplot
get_plot_rows <- function(p) {
  return(
    p %>%
      ggplot2::ggplot_build() %>%
        magrittr::extract2('layout') %>% 
          magrittr::extract2('layout') %>%
            magrittr::extract2('ROW') %>%
              unique() %>%
                length()
  )
}