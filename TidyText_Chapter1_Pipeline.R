library(tidytext)
library(tidyverse)




#Input Text
text <- read.csv("~/Desktop/result_clean.csv")
text <- mutate(text, text = as.character(word))
text <- text %>%
  select(-word)



#Separate into single words 
single_words <- text %>%
  unnest_tokens(word, text)
 
#Separate into single words and count overall word counts
single_words_count <- text %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)

#Separate into single words and count based on document
#this way keeps the number of times the word is used in the document
single_words_doc <- text %>%
  unnest_tokens(word, text) %>%
  count(document, word, sort = TRUE)




#Plot of Most Common Words
single_words_count %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) 
  






