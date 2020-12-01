library(tidytext)
library(tidyverse)
library(ggplot2)
library(igraph)
library(ggraph)



#Input Text
text <- read.csv("~/Desktop/result_clean.csv")
text <- mutate(text, text = as.character(word))
text <- text %>%
  select(-word)




#Count bigrams
bigram_words <- text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)




#Check bigram counts
bigram_words %>%
  count(bigram, sort = TRUE)





bigrams_separated <- bigram_words %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_counts <- bigrams_separated %>% 
  count(word1, word2, sort = TRUE)
bigram_counts




bigram_graph <- bigram_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()
bigram_graph

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(color = "light blue") +
  geom_node_point( size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, vjust = 1.5, hjust = .5, color ="black")
