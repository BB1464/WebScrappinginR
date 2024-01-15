## Load the Required Packages

library(rvest) # install.packages('rvest')
library(xml2) # install.packages('xml2')
library(httr) # install.packages('httr')
library(tidyverse) # install.packages('tidyverse')

get_cg <- function(pages) {
  cat("Scraping page", pages, "\n")
  page <-
    str_c("https://cgspace.cgiar.org/discover?%20%20rpp=10&etal=0&query=cassava&scope=10568/35697&group_by=none&page=", pages) |> # structure link 
    read_html()
  tibble(
    title = page %>%
      html_elements(".ds-artifact-item") %>%
      html_element(".description-info") %>%
      html_text2(),
    year = page |> 
      html_elements(".ds-artifact-item") |> 
      html_element("span.date") |> 
      html_text2(),
    type = page |> 
      html_elements(".ds-artifact-item") |> 
      html_element("div.artifact-type") |> 
      html_text2(),
    status = page |> 
      html_elements(".ds-artifact-item") |> 
      html_element(".artifact-description") |> 
      html_text2(),
    link = page |>
      html_elements(".ds-artifact-item") |>
      html_element(".description-info") |>
      html_attr("href") |>
      str_c("https://cgspace.cgiar.org", .)
  )
}

# For get the time of running
startTime <- Sys.time() 
datas <- data.frame(map_dfr(1:20, get_cg))# only 20 page, try you for more
endTime <- Sys.time()

print(endTime - startTime)

## Analyze the result

datas |> count(type) |> 
  arrange(-n) |> 
  ggplot(aes(x=fct_reorder(type,-n), y=n, fill=type)) +
  geom_col() +
  geom_text(aes(y=n, label=n), vjust=-0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        legend.position = 'none') +
  labs(title='Counting by type of information',
       x='')


datas |>  count(year) |> 
  mutate(year = substr(year, 1, 4)) |> 
  filter(grepl("^\\d{4}$", year)) |> 
  group_by(year) |> 
  summarise(total = sum(n)) |> 
  ggplot(aes(x=year, y=total, fill=year)) +
  geom_col() +
  geom_text(aes(y=total, label=total), vjust=-0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        legend.position = 'none') +
  labs(title='Counting by year of publication',
       x='')

datas |>  count(year,type) |> 
  mutate(year = substr(year, 1, 4)) |> 
  filter(grepl("^\\d{4}$", year)) |> 
  group_by(year,type) |> 
  summarise(total = sum(n)) |> 
  ggplot(aes(x=reorder(year, as.numeric(year)), y=total, fill=year)) +
  geom_col() +
  geom_text(aes(y=total, label=total), vjust=0.9) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = 'none') +
  labs(title='Counting by type of information and year of publication',
       x='') +
  facet_wrap(~ type)

## For more details check out this stackoverflow link:

# https://stackoverflow.com/questions/74209731/web-scraping-loop-for-pages-and-node-in-r/74209836#74209836



