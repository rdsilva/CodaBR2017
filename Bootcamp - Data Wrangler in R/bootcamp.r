# importando as bibliotecas
library(tidyverse)
library(ggplot2movies)
library(nycflights13)

# importando os dados que serão trabalhados
data("movies")
data("flights")

# carregando as bases
# no r-studio basta digitar o nome das bases para ele carregar de fato em memória os dados
movies
flights

head(movies)
str(movies)

head(flights)
str(flights)

# 5 principais operações para serem utilizadas no R
# baseadas no SQL

# 1) Escolher ou Remover colunas

movies_title_year <- select(movies, title, year)
movies_wo_title <- select(movies, -title)
movies_subset <- select(movies, title:rating)
movies_subset_title <- select(movies, -(title:rating))
movies_rs <- select(movies, num_range("r", 1:5))
movies_tit <- select(movies, starts_with("tit"))

flights_time <- select(flights, contains("time"))
flights_subset <- select(flights, year:dep_delay)

# 2) Ordenar a base

flights_arranged <- arrange(flights, time_hour)
flights_arranged_desc <- arrange(flights, desc(time_hour))

movies_arranged_desc <- arrange(movies, desc(length))
longest_movie <- slice(movies_arranged_desc, 1)
longest_movie

# 3) Criar subsets com base nas linhas

movies_long_than_90 <- filter(movies, length > 90)
movies_equal_to_90 <- filter(movies, length == 90)
movies_diff_to_90 <- filter(movies, length != 90)

flights_from_jfk <- filter(flights, origin == "JFK")

# 4) Alterar o conteúdo das colunas / Criar novas colunas

movies <- mutate(movies, duracao_hora = length/60)
str(movies)

flights_ny_december <- arrange(filter(flights, month == 12, origin == "EWR"), day)

# ou

flights_ny_december_piped <- flights %>%
  arrange(day) %>%
  filter(month == 12, origin == "EWR")

# 5) Sumarizar/Resumir colunas

flights_means <- flights %>%
  summarise(media_atraso_partida = mean(dep_delay, na.rm = T),
            media_atraso_chegada = mean(arr_delay, na.rm = T))
  
flights_means_grouped <- flights %>%
  group_by(origin) %>%
  summarise(media_atraso_partida = mean(dep_delay, na.rm = T),
            media_atraso_chegada = mean(arr_delay, na.rm = T))

movies <- movies %>%
  mutate(grande_pequeno = ifelse(length > 90, "Grande", "Pequeno"))

movies_budget_mean <- movies %>%
  group_by(grande_pequeno) %>%
  summarise(budget_mean = mean(budget, na.rm = T))

flights_means_month <- flights %>%
  group_by(month) %>%
  summarise(mean_dep = mean(dep_delay, na.rm = T),
            mean_arr = mean(arr_delay, na.rm = T))
 


# Verticalizando o dataset

options(scipen=99999999) # forçando o R a processar o valor em numerico e não em cientifico

movies_gender <- movies %>%
  gather(genero, valor, Action:Short) %>% # verticalizando o dataset
  filter(valor == 1) %>% # filtrando os valores não desejados
  select(-valor) %>% # dropando a coluna não desejada
  group_by(genero) %>% # agrupando os dados
  summarise(mean_budget = mean(budget, na.rm = T),
            mean_length = mean(length, na.rm = T),
            median_budget = median(budget, na.rm = T),
            median_length = median(length, na.rm = T)) %>% # calculando as medias
  arrange(desc(mean_budget)) # ordenando os resultados 

movies_tmp <- movies %>%
  gather(genero, valor, Action:Short) %>% # verticalizando o dataset
  filter(valor == 1) %>% # filtrando os valores não desejados
  select(-valor) # dropando a coluna não desejada

# horizontalizando o dataset

movies_horizon <- movies_tmp %>%
  mutate(valor = 1) %>% # adicionando uma nova coluna com valores 1
  spread(genero, valor, fill = 0) # horizontalizando a tabela

# quebrando valores de uma coluna em multiplas entradas
# função : unnest


# ------
# dataset do campeanato brasileiro

corinthians_means <- brasileiro %>%
  filter(time_casa == "corinthians" | time_fora == "corinthians") %>%
  mutate(saldo = gols_casa - gols_fora) %>%
  mutate(casa_visitante = ifelse(time_fora == "corinthians", "visitante", "casa")) %>%
  mutate(saldo = ifelse(time_fora == "corinthians", saldo*(-1), saldo)) %>%
  group_by(casa_visitante) %>%
  summarise(media_saldo = mean(saldo))
