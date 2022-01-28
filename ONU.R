library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())
library(ggthemes)
library(tidytext)

tt <- tt_load("2021-03-23")

unvotes <- tt$unvotes %>%
  mutate(vote_number = match(vote, c("no", "abstain", "yes")) - 2) %>%
  left_join(tt$roll_calls %>%
              select(rcid, date, amend), by = "rcid")
unvotes %>%
  count(country, sort = TRUE)

summarize_votes <- function(tbl, min_votes = 10) {
  tbl %>%
    summarize(n_votes = n(),
              n_yes = sum(vote == "yes"),
              pct_yes = n_yes / n_votes,
              .groups = "drop") %>%
    filter(n_votes >= min_votes) %>%
    arrange(desc(pct_yes))
}

plot_by <- function(tbl, category) {
  tbl %>%
    filter(!is.na({{ category }})) %>%
    mutate(category = fct_reorder({{ category }}, pct_yes)) %>%
    ggplot(aes(year, pct_yes)) +
    geom_line(aes(color = category)) +
    scale_y_continuous(labels = percent) +
    scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
    expand_limits(y = 0) +
    labs(y = "% yes votes",
         x = "Year")
}
library(lubridate)
unvote
by_year <- unvotes %>%
  group_by(year = year(date)) %>%
  summarize_votes()
by_year %>%
  ggplot(aes(year, pct_yes)) +
  geom_line() +
  expand_limits(y = 0)

library(WDI)
country_incomes <- WDI(indicator = c(gdp_per_capita = "NY.GDP.PCAP.PP.KD",
                                     pop = "SP.POP.TOTL"),
                       start = 2005, end = 2005, extra = TRUE) %>%
  as_tibble() %>%
  select(country_code = iso2c, income, gdp_per_capita, pop) %>%
  filter(!is.na(income)) %>%
  mutate(income = fct_relevel(income, "Low income", "Lower middle income", "Upper middle income"))
unvotes %>%
  inner_join(country_incomes, by = "country_code") %>%
  group_by(income,
           year = year(date)) %>%
  summarize_votes() %>%
  plot_by(income)

by_country_year <- unvotes %>%
  bind_rows(unvotes %>% mutate(country = "Overall")) %>%
  group_by(year = year(date), country, country_code) %>%
  summarize_votes()


by_country_year %>%
  filter(country %in% c("United States", "Brazil")) %>%
  mutate(country = fct_reorder(country, pct_yes)) %>%
  ggplot(aes(year, pct_yes, color = country)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  expand_limits(y = 0) +
  theme(legend.position = "none") +
  labs(y = "% yes votes")+
  geom_vline(xintercept=c(2016,2018), linetype="dotted")

rc_words <- tt$roll_calls %>%
  filter(!is.na(short)) %>%
  unnest_tokens(word, short) %>%
  anti_join(stop_words, by = "word") %>%
  distinct(rcid, word) %>%
  add_count(word, name = "word_count") %>%
  filter(word_count >= 100)


unvotes %>%
  inner_join(rc_words, by = "rcid") %>%
  filter(country %in% c("Brazil", "United States")) %>%
  group_by(word, country, date) %>%
  summarize_votes(min_votes = 100) %>% 
  mutate(word = fct_reorder(word, n_yes)) %>%view()
  ggplot(aes(pct_yes, word)) +
  geom_point(aes(size = n_votes, color = country)) +
  expand_limits(x = 0) +
  scale_x_continuous(labels = percent) +
  labs(x = "% yes",
       y = "")

  date <- as.Date("2016-12-05", format="%Y-%m-%d")
  date1 <- as.Date("2003-01-01", format="%Y-%m-%d")
  date2 <- as.Date("1985-03-15", format="%Y-%m-%d")
  
  
  
unvotes %>%
  inner_join(rc_words, by = "rcid") %>%
  filter(word %in% c("human", "rights", "weapons", "nuclear"))%>%
  filter(country %in% c("Brazil")) %>%
  group_by(word, country, date) %>%
  summarize_votes(min_votes = 5) %>%
  mutate(word = fct_reorder(word, pct_yes)) %>%
  ggplot( aes(x = date, y = pct_yes, fill = word))+
  geom_area()+
  geom_vline(xintercept= c(as.numeric(date), as.numeric(date1), as.numeric(date2)), linetype="dotted", colour = "black", size = 1)+
  facet_wrap(~word)+
  labs(title = "Como o Brasil vota na ONU de acordo com \n a presença de algumas palavras chaves?",
       subtitle = "Porcentagem de votos sim:",
       x= "Ano",
       y= "Porcentagem",
       fill = "Palavra")+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  scale_color_brewer(palette = "Set1")


  geom_text(aes(x=as.numeric(date), label="\nInício Governo Temer", y=0.5), colour="blue", angle=90, text=element_text(size=11))+
  

+

  expand_limits(x = 0) +
  scale_x_continuous(labels = percent) +
  labs(x = "% yes",
       y = "")
