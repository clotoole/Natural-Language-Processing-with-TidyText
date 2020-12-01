library(tidytext)
library(tidyverse)



#Load folder of text into R
tbl <- "~/Desktop/Research_Data/GeneDrive_Data/Time Segments/2018"
load_text <- tibble(file = dir(tbl, full.names = TRUE)) %>%
  mutate(text = map(file, read_lines)) %>%
  transmute(document = basename(file), text)

#Separating the lines of text
raw_text <- load_text %>%
  unnest(text)

#Unnesting into single words
word <- raw_text %>%
  unnest_tokens(word, text)




#Load csv of stop words, use column name "word" in csv
stop_list <- read_csv("~/Desktop/Research_Data/Lemma_Stop/Oxitec_Stop_List.csv", col_names = TRUE)



#Lemmatize words and remove stop words
word$word <- textstem::lemmatize_words(word$word) 
word <- word %>%
  anti_join(stop_list, by = "word") #stop_words or stop_list




#Rejoin  the data into whole texts
bag_words <- word %>%
  group_by(document) %>% 
  summarise(word =
  paste(word, collapse=", "))




#Optional steps: remove numbers, punctuation and other unneeded characters
bag_words$word <- str_replace_all(string = bag_words$word,
                                  pattern = "[0-9]+",
                                  replacement = "")
bag_words$word <- str_replace_all(string = bag_words$word,
                                  pattern = "\\s*\\([^\\)]+\\)",
                                  replacement = "")
bag_words$word <- str_replace_all(string = bag_words$word,
                                  pattern = "[:punct:]",
                                  replacement = "")



#Optional: if you have multiple years of folders then add a year column,
#then reassign bag_words to a different variable
#join all using rbind()
bag_words$year <- "2018"

gd <- bag_words

gd <- rbind(gd1, gd2, gd3, gd4, gd5, gd6)




#finally create csv
write_csv(bag_words, "~/Desktop/result_clean.csv")

#if you used option step then
write_csv(gd, "~/Desktop/result_clean.csv")

