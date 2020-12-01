library(tidytext)
library(tidyverse)
library(ggplot2)
library(furrr)
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


#Single words
single_words <- text %>%
  unnest_tokens(word, text)


#filter words
single_words <- single_words %>%
  add_count(word) %>%
  filter(n > 100) %>%
  select(-n)


#cast sparse matrix
text_sparse <- single_words %>%
  count(linenumber, word) %>%
  cast_sparse(linenumber, word, n)


#Multiprocess core
plan(multiprocess)



#test model to see which # of topics are most appropriate
many_models <- data_frame(K = c(10, 20, 40, 50, 60, 80)) %>%
  mutate(topic_model = future_map(K, ~stm(text_sparse, K = .,
                                          verbose = FALSE)))

heldout <- make.heldout(text_sparse)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, text_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, text_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result


#Plot, try to maximize heald-out while minimizing residuals
k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  theme_bw() +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around 60")



#Topic model with 50 topics, can only be the number of topics you chose to 
#train on
topic_model <- k_result %>% 
  filter(K == 50) %>% 
  pull(topic_model) %>% 
  .[[1]]


#Check model
topic_model


#Beta and gamma values
td_beta <- tidy(topic_model)

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(text_sparse))


#Top terms for the beta values, in this case top 7 for each topic
top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()



gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))



#Plot top 20 topics based on their gamma values along with
#top 7 words based on their beta values
gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "serif") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09)) +
  theme_tufte(base_family = "serif", ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family="serif"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence in the Gene Drive Corpus",
       subtitle = "With the top words that contribute to each topic")

gamma_terms_table <- gamma_terms%>%
  select(topic, gamma, terms) 







