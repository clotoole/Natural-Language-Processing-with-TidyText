#Sentiment Analysis
#The AFINN lexicon assigns words with a score that runs between -5 and 5
#with negative scores indicating negative sentiment 
#and positive scores indicating positive sentiment

#a ungiram approach only takes into account single words, not the context involved 

library(tidyverse)
library(tidytext)
library(ggplot2)




#have to download textdata package
#then download afinn lexicon
#check lexicon
get_sentiments("afinn")




#Input Text
text <- read.csv("~/Desktop/result_clean.csv")
text <- mutate(text, text = as.character(word))
text <- text %>%
  select(-word)


#if your dataset does not have dates or years use this in place
text <- as.data.frame(text)
text <- text %>%
  mutate(linenumber = row_number())



#Count and normalize word counts to compare across documents or years
single_words <- text %>%
  unnest_tokens(word, text) %>%
  count(linenumber, word, sort = TRUE) %>% #use year instead if you have dates in dataset
  mutate(n = (n*10000)/sum(n)) #normalize data




#Set up sentmient analysis the absolutes
#since it is normalized values this will be fine 
single_words_sentiment <- single_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(doc = linenumber) %>% #or use Date = year
  mutate(value = n * value) %>%
  summarise(Sentiment = sum(value))
  




#Visualize
single_words_sentiment %>%
  ggplot(aes(doc, Sentiment, fill = method)) + #then use Date instead of doc
  geom_col(show.legend = FALSE) 
  facet_wrap(~method, ncol = 1, scales = "free_y")
  










