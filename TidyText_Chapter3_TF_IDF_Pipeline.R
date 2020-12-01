library(tidytext)
library(tidyverse)
library(ggthemes)
library(ggplot2)




#Input Text
text <- read.csv("~/Desktop/result_clean.csv")
text <- mutate(text, text = as.character(word))
text <- text %>%
  select(-word)


text <- as.data.frame(text)
text <- text %>%
  mutate(linenumber = row_number())


#Separate into single words 
single_words <- text %>%
  unnest_tokens(word, text)



#Filter Words 
Total_Corpus <- single_words %>%
  add_count(word) %>%
  filter(n > 300) %>%
  select(-n)



#Make TF_IDF table 
#use year if you have dates or use linenumber if not
#better to use by year if possible 
Total_Corpus_tf_idf <- Total_Corpus %>%
  count(year, word, sort = TRUE) %>%
  bind_tf_idf(word, year, n) %>%
  arrange(-tf_idf) %>%
  group_by(year) %>%
  top_n(10) %>%
  ungroup




Total_Corpus_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, year)) %>%
  ggplot(aes(word, tf_idf, fill = year)) +
  geom_col(show.legend = FALSE) +
  theme_bw() +
  facet_wrap(~ year, scales = "free_y", ncol = 3) +
  scale_x_reordered() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.001)) +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in Gene Drive Corpus",
       subtitle = "Separated by each time segment, totaling in 6 segments")





