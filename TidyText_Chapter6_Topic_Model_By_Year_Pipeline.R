library(tidytext)
library(tidyverse)
library(ggplot2)
library(quanteda)
library(stm)
library(ggthemes)



#Input Text
text <- read.csv("~/Desktop/result_clean.csv")
text <- mutate(text, text = as.character(word))
text <- text %>%
  select(-word)
text <- as.data.frame(text)
text <- text %>%
  mutate(linenumber = row_number())



single_words <- text %>%
  unnest_tokens(word, text)

#Make matrix
text_dfm <- single_words %>%
  count(year, word, sort = TRUE) %>%
  cast_dfm(year, word, n)




#Topic model, K = number of topics or number of years in this case
topic_model <- stm(text_dfm, K = 6, 
                   verbose = FALSE, init.type = "Spectral")
td_beta <- tidy(topic_model)




#Visualize
td_beta %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col( show.legend = FALSE) +
  theme_bw() +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.001)) +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different topics are associated with different years")



#See which years the topics fit in
td_gamma <- tidy(topic_model, matrix = "gamma",                    
                 document_names = rownames(text_dfm))
ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram( show.legend = FALSE) +
  theme_bw() +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of time segment probabilities for each topic",
       subtitle = "Each topic is associated with 1 time segment",
       y = "Number of segments", x = expression(gamma))





