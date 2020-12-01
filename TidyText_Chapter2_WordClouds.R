library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)




#Input Text
text <- read.csv("~/Desktop/result_clean.csv")
text <- mutate(text, text = as.character(word))
text <- text %>%
  select(-word)



#Count words
single_words <- text %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) 




#WordCloud
single_words %>%
  with(wordcloud(word, n, max.words = 100, random.order = FALSE, 
                 rot.per = 0.2, scale = c(4,.5), colors = brewer.pal(8, "Dark2")))


#OR
single_words %>%
  wordcloud2(size = 1.6, color = 'random-dark')



