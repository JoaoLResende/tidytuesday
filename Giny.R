library(tidyverse)
library(here)
library(wesanderson)
library(fuzzyjoin)
library(ggthemes)
library(countrycode)
library(WDI)
library(gridExtra)
library(forcats)

load("swiid9_0.rda")
setwd("D:/Documents")

emission <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")


country_incomes <- WDI(indicator = c(gdp_per_capita = "NY.GDP.PCAP.PP.KD",
                                     pop = "SP.POP.TOTL"),
                       start = 2017, end = 201, extra = TRUE) %>%
  as_tibble() %>%
  select(country_code = iso3c, income, gdp_per_capita, pop) %>%
  filter(!is.na(income)) %>%
  mutate(income = fct_relevel(income, "Low income", "Lower middle income", "Upper middle income"))

emission_per_income <-emission %>%
select(iso_code, year, co2, cumulative_co2,share_global_cumulative_co2, country, share_global_co2 )%>%
  rename(country_code = "iso_code")%>%
  inner_join(country_incomes)

emissio_by_year <-emission_per_income%>%
  group_by(income, year)%>%
  filter(!is.na(co2))%>%
  summarise(total_co2 = sum(co2))%>%
  ggplot(aes(x= year, y = total_co2, color= income))+
  geom_line(size = 1.5, alpha = 0.8)+
  labs(title = "Emission of CO₂:",
       subtitle = "Per quintile of income.",
       x= "Year",
       y= "Total of CO2(in millions of ton)",
       color = "Quintile")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  scale_color_brewer(palette = "Set1")+
  scale_x_continuous(breaks=seq(1750, 2015, 25))


percent <- emission_per_income %>% 
  group_by(year, income) %>% 
  filter(!is.na(share_global_co2))%>%
  summarise(Count = sum(share_global_co2))%>% 
  arrange(desc(Count))%>%
  arrange(year, income)%>%
  ggplot(aes(year, Count, color= income))+
  geom_line()+
  geom_line(size = 1.5)+
  labs(title = "Percentage of emission of CO2 per year:",
       subtitle = "Per quintile of income.",
       x= "Year",
       y= "Percentage",
       color = "Quintile")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  scale_color_brewer(palette = "Set1")+
  scale_x_continuous(breaks=seq(1750, 2015, 25))+ 
  scale_y_continuous(breaks=seq(0, 100, 20),labels = function(x) paste0(x, "%"))

  
  
LINKEDIN_CO2grid.arrange(emissio_by_year, percent, ncol=2)

    
emissio_culmulative_year <-emission_per_income%>%
  group_by(income, year)%>%
  filter(!is.na(cumulative_co2))%>%
  summarise(total_co2 = sum(cumulative_co2))%>%
  ggplot(aes(x= year, y = total_co2, color= income))+
  geom_line(size = 1.5)+ scale_y_continuous(trans = log2_trans())


emission%>%
  gather(key = "key", value= "value", oil_co2, coal_co2, cement_co2, gas_co2, other_industry_co2, flaring_co2)%>%
  rename(country_code = "iso_code")%>%
  inner_join(country_incomes)%>%
  filter(!is.na(value))%>%
  group_by(key)%>%
  ggplot(aes( x = key, y = value, fill = income ))+
  geom_col(position = "dodge")

emission_per_income%>%
  select(year, country, share_global_co2)%>%
  filter(!is.na(share_global_co2),
         share_global_co2 >=1)%>%
  group_by(year)%>%
  mutate(country = fct_lump(country, 5, w = share_global_co2))%>%
  group_by(country, year)%>%
  summarise(share_global_co2 = sum(share_global_co2))%>%
  ggplot(aes( x = year, y = share_global_co2, color = country))+
  geom_line()
