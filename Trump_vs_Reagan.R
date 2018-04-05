library(tidyverse)
library(tidytext)
library(wordcloud)
# ================
# tidying speech |
# ================
text_Trump <- read_file("h_data/trump first state of the union.txt")
text_Reagan <- read_file("h_data/reagan first state of the union.txt")

tidy_Trump <- data_frame(paragraph = 1:length(str_split(text_Trump, '\r\n\r\n')[[1]]), text = str_split(text_Trump, '\r\n\r\n')[[1]]) %>% 
  unnest_tokens(word, text)

tidy_Reagan <- data_frame(paragraph = 1:length(str_split(text_Reagan, '\r\n\r\n')[[1]]), text = str_split(text_Reagan, '\r\n\r\n')[[1]]) %>% 
  unnest_tokens(word, text)



# =========================
# get lexicon from nrc     |
# joy, anger, anticipation |
# =========================

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")

nrc_anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")

# ==================================
# Trump joy,anger,anticipation top 1000|
# =================================

tidy_Trump %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)%>% 
  with(wordcloud(word, n, max.words = 1000))

tidy_Trump %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, max.words = 1000))

tidy_Trump %>%
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, max.words = 1000))
  
# ==================================
# Reagan joy,anger,anticipation top 1000|
# =================================

tidy_Reagan %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)%>% 
  with(wordcloud(word, n, max.words = 1000))

tidy_Reagan %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, max.words = 1000))

tidy_Reagan %>%
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, max.words = 1000))


# ===================================
# Trump sentiment by three method   |
# ===================================
afinn <- tidy_Trump %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = paragraph) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(tidy_Trump %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tidy_Trump %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = paragraph, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


bind_rows(afinn, 
          bing_and_nrc) %>%
    ggplot(aes(index, sentiment, fill = method)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~method, ncol = 1, scales = "free_y")

# ===================================
# Reagan sentiment by three method   |
# ===================================
afinn <- tidy_Reagan %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = paragraph) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(tidy_Reagan %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tidy_Reagan %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = paragraph, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") 



