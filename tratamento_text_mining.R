library(rtweet)
library(tidyverse)

#Opção por pegar o robô deve-se ao fato de retroceder para além de 9 dias e também é um atalho para pegar vários idiomas e a expressão Datnte Aliggieri
df_divina<- rtweet::get_timeline(user = "@DivinaCommediaQ", n=3200)

df_divina%>%
  mutate(idioma = lang) %>%
  mutate(idioma= ifelse(idioma=='pl',"it", idioma)) %>%
  mutate(idioma= ifelse(status_id =='1210173128976031744','pl',idioma)) %>%
  group_by(idioma) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  mutate(idioma= reorder(idioma, quantidade)) %>%
  ggplot() +
  geom_col(aes(x=idioma, y= quantidade),fill = "#8E1E15") + 
  coord_flip() +
  theme_light() 


####Textmining  

library(tidytext)
library(tidyr)
library(widyr)
library(igraph)
library(ggraph)
library(wordcloud)

##Italiano
df_divina_it <- 
  df_divina %>%
  filter(lang=='it') %>%
  select(status_id, text)


stop_words_grupo<- c(stopwords::stopwords("it"), c("https","t.co","dante","alighieri","divina","commedia","dantealighieri","divinacommedia"))

analise_mensagem <- tibble(text=df_divina_it$text) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()


analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 30, colors=brewer.pal(6,"Dark2"),random.order=FALSE))


analise_twitter_secoes <- tibble(texto= df_divina_it$text)%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stop_words_grupo)

word_cors <- analise_twitter_secoes %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, section, sort = TRUE)

set.seed(2016)

word_cors %>%
  filter(correlation > .65) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


###Português
df_divina_pt <- 
df_divina %>%
  filter(lang=='pt') %>%
  select(status_id, text)


stop_words_grupo<- c(stopwords::stopwords("pt"), c("https","t.co","dante","alighieri","é","divina","comédia","q" ))

analise_mensagem <- tibble(text=df_divina_pt$text) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()


analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 30, colors=brewer.pal(6,"Dark2"),random.order=FALSE))


analise_twitter_secoes <- tibble(texto= df_divina_pt$text)%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stopwords::stopwords("pt"))

word_cors <- analise_twitter_secoes %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, section, sort = TRUE)

set.seed(2016)

word_cors %>%
  filter(correlation > .55) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


###Espanhol
df_divina_es <- 
  df_divina %>%
  filter(lang=='es') %>%
  select(status_id, text)


stop_words_grupo<- c(stopwords::stopwords("es"), c("https","t.co","dante","alighieri","dantealighieri","divina","comedia","divinacommedia" ))


analise_mensagem <- tibble(text=df_divina_es$text) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()


analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 30, colors=brewer.pal(6,"Dark2"),random.order=FALSE))


analise_twitter_secoes <- tibble(texto= df_divina_es$text)%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stop_words_grupo)

word_cors <- analise_twitter_secoes %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, section, sort = TRUE)

set.seed(2016)

word_cors %>%
  filter(correlation > .65) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
