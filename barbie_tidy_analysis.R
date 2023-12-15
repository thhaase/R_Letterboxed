library(tidyverse)
library(tidytext)
library(textdata)
library(gridExtra)
library(widyr)
library(ggraph)
library(igraph)

df <- read_csv("./barbie1.csv")


# first look
plot(table(df$date))
sort(table(df$date))

plot(table(df$rating))


# subsetting by rating
df_gr <- subset(df, rating >= 4)
df_nr <- subset(df, rating == 3)
df_br <- subset(df, rating > 0 & rating < 3)


text_df_gr <- tibble(line=1:dim(df_gr)[1], text=df_gr$comment)
text_df_nr <- tibble(line=1:dim(df_nr)[1], text=df_nr$comment)
text_df_br <- tibble(line=1:dim(df_br)[1], text=df_br$comment)


tidy_df_gr <- text_df_gr %>% unnest_tokens(word, text)
tidy_df_nr <- text_df_nr %>% unnest_tokens(word, text)
tidy_df_br <- text_df_br %>% unnest_tokens(word, text)


# clean data
data(stop_words)
my_stopwords <-tibble(word = c("barbie", "film", "movie", "greta", "ken", "margot", "ryan",
                               "gosling", "robbie", "gerwig", "kenough", "la", "de", "el",
                               "it's", "es", "é", "i'm"))

tidy_df_gr <- tidy_df_gr %>% anti_join(stop_words) %>% anti_join(my_stopwords)
tidy_df_nr <- tidy_df_nr %>% anti_join(stop_words) %>% anti_join(my_stopwords)
tidy_df_br <- tidy_df_br %>% anti_join(stop_words) %>% anti_join(my_stopwords)


# top words
top_gr <- tidy_df_gr %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)+ 
  ggtitle("topwords - good rating")

top_nr <- tidy_df_nr %>%
  count(word, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)+ 
  ggtitle("topwords - neutral rating")

top_br <- tidy_df_br %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) + 
  ggtitle("topwords - bad rating")

grid.arrange(top_gr,top_nr,top_br)



# correlations with interesting words e.g. woman

#wich words occur in the same comments
pairs_gr <- tidy_df_gr %>% pairwise_count(word, line, sort = TRUE) %>% 
  filter(item1 == "woman")

pairs_nr <- tidy_df_nr %>% pairwise_count(word, line, sort = TRUE) %>% 
  filter(item1 == "woman")

pairs_br <- tidy_df_br %>% pairwise_count(word, line, sort = TRUE) %>% 
  filter(item1 == "woman")
#pairs_br try: world

pairs_gr
pairs_nr
pairs_br




cors_gr <- tidy_df_gr %>% group_by(word) %>% filter(n() >= 15) %>%
  pairwise_cor(word, line, sort = TRUE)
cors_nr <- tidy_df_nr %>% group_by(word) %>% filter(n() >= 5) %>%
  pairwise_cor(word, line, sort = TRUE)
cors_br <- tidy_df_br %>% group_by(word) %>% filter(n() >= 3) %>%
  pairwise_cor(word, line, sort = TRUE)

cors_gr %>% filter(item1=="woman")
cors_nr %>% filter(item1=="woman")
cors_br %>% filter(item1=="woman")


set.seed(2023)

cors_br %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + 
  ggtitle("correlations bad rating")

cors_nr %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + 
  ggtitle("correlations neutral rating")

cors_gr %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + 
  ggtitle("correlations good rating")
