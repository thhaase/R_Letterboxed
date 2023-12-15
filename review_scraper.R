library(rvest)
library(tidyverse)
library(stringr)

scrape_page <- function(page_number) {
  url <- paste0("https://letterboxd.com/film/barbie/reviews/page/", page_number, "/")
  page <- read_html(url)
  
  name_data <- page %>% html_elements(".name") %>% html_text(trim = TRUE)
  comment_data <- page %>% html_elements(".collapsible-text p") %>% html_text(trim = TRUE)
  date_data <- page %>% html_elements("._nobr") %>% html_text(trim = TRUE)
  rating_data <- page %>% html_elements(".-green") %>% html_text(trim = TRUE)
  
  # Ensure all vectors have the same length by using NA for missing elements
  max_length <- max(length(name_data), length(comment_data), length(date_data), length(rating_data))
  name_data <- rep(name_data, length.out = max_length)
  comment_data <- rep(comment_data, length.out = max_length)
  date_data <- rep(date_data, length.out = max_length)
  rating_data <- rep(rating_data, length.out = max_length)
  
  data.frame(name_data, comment_data, date_data, rating_data)
}
extract_and_replace_stars <- function(rating_text) {
  stars_count <- nchar(gsub("[^★]", "", rating_text))
  stars_count_text <- paste0(stars_count)
  return(stars_count_text)
}

# how many pages should be scraped?
num_pages <- 2

df <- data.frame()

# scraping pages
for (page_number in 1:num_pages) {
  cat("Scraping page:", page_number, "\n")
  scraped_data <- scrape_page(page_number)
  df <- rbind(df, scraped_data)
}

# cleaning
df <- df %>%
  rename(name = name_data,comment = comment_data,
         date = date_data,rating = rating_data)

# replace stars
df$rating <- as.numeric(lapply(df$rating, extract_and_replace_stars))

write.csv(df, file = "./barbievscode.csv", row.names = FALSE)

