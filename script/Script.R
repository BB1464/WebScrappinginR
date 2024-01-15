library(tidyverse)
library(rvest)

get_cg <- function(pages) {
  
  cat("Scraping page", pages, "\n")
  
  page <-
    str_c("https://cgspace.cgiar.org/discover?%20%20rpp=10&etal=0&query=cassava&scope=10568/35697&group_by=none&page=", pages) %>%
    read_html()
  
  
  tibble(
    title = page %>%
      html_elements(".ds-artifact-item") %>%
      html_element(".description-info") %>%
      html_text2(),
    link = page %>%
      html_elements(".ds-artifact-item") %>%
      html_element(".description-info") %>%
      html_attr("href") %>%
      str_c("https://cgspace.cgiar.org", .)
  )
  
}


StartTime <- Sys.time()

map_dfr(1:5, get_cg) %>% 
  as.data.frame()

StopTime <- Sys.time()

print(StopTime - StartTime)