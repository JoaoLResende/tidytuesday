library(plyr)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(openxlsx)
library(dygraphs)
library(scales)
library(formattable)
library(scales)
library(zoo)
library(DT)
library(plotly)
library(shinyjs)
library(ggflags)


setwd("D:/Desktop/Consultorias/TidyTuesday")

tt <- tt_load("2020-08-04")

energy_tipes <- tt$energy_types%>%
  filter(level	== "Level 1")

country_totals <- tt$country_totals


country_incomes <- WDI(indicator = c(gdp_per_capita = "NY.GDP.PCAP.PP.KD",
                                     pop = "SP.POP.TOTL"),
                       start = 2005, end = 2005, extra = TRUE) %>%
  as_tibble() %>%
  select(country_code = iso2c, income, gdp_per_capita, pop) %>%
  filter(!is.na(income))

energy_tipes <- energy_tipes %>%
  gather(year,gigawatt_hours, starts_with("2"))%>%
  rename( country_code = "country")%>%
  inner_join(country_incomes )

country_totals <- country_totals %>%
  gather(year,gigawatt_hours, starts_with("2"))%>%
  rename( country_code = "country")%>%
  inner_join(country_incomes)

country_totals <- country_totals %>%
  filter(type == "Total net production")

energy_tipes%>%
  group_by(country_name, year, pop)%>%
  summarise(gigawatt_hours = sum(gigawatt_hours))%>%
  mutate(percapita_gigawatt_hours = gigawatt_hours/pop*1000)%>%
  ungroup()%>%
  mutate(country_name = fct_reorder(country_name,percapita_gigawatt_hours ))%>%
  ggplot(aes( x = percapita_gigawatt_hours, y = country_name, color = year))+
  geom_point(size = 2)+
  labs(title = "Percapita Gigawaat hours consumption (per 1000 people)",
       x = "Gigawaats hours",
       y = "Country",
       color = "Year")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  scale_color_brewer(palette = "Set1")

country_totals%>%
  group_by(country_name, year, pop)%>%
  summarise(gigawatt_hours = sum(gigawatt_hours))%>%
  mutate(percapita_gigawatt_hours = gigawatt_hours/pop*1000)%>%
  ungroup()%>%
  mutate(country_name = fct_reorder(country_name,percapita_gigawatt_hours ))%>%
  ggplot(aes( x = percapita_gigawatt_hours, y = country_name, color = year))+
  geom_point(size = 2)+
  labs(title = "Percapita Gigawaat hours production (per 1000 people)",
       x = "Gigawaats hours",
       y = "Country",
       color = "Year")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  scale_color_brewer(palette = "Set1")

energy_tipes%>%
  group_by(country_name, type, year)%>%
  filter( gigawatt_hours >= 26000,
          country_name != "Finland")%>%
  ungroup()%>%
  mutate(country_name = fct_reorder(country_name,gigawatt_hours, sum ))%>%
  ggplot(aes(x = gigawatt_hours, y = country_name, fill= type))+
  geom_col()+
  scale_x_continuous( labels = comma)+
  labs(title = "Total power production",
                                          x = "Gigawaats hours",
                                          y = "Country",
                                          color = "Type of production")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~year)




