library(gapminder)
library(tidyverse)
library(funModeling)
library(plotly)
library(ggthemr)

gapminder <- gapminder
glimpse(gapminder) # non dice la classe dell'oggetto
colnames(gapminder)
class(gapminder)
str(gapminder)

gapminder %>% 
  distinct(year)

gapminder %>%
  filter(country == "China", year == 1957)

gapminder %>%
  arrange(desc(lifeExp))

gapminder %>%
  filter(year == 1957) %>% 
  arrange(desc(lifeExp))

# EX 1. Vedere quale e' la max lifeExp per year

gapminder %>%
  filter(year == 1957) %>% 
  arrange(desc(pop))

gapminder %>%
  mutate(lifeExp = lifeExp * 12)  # lifeExp in months - change the variable itself

gapminder %>%
  mutate(lifeExpMonths = lifeExp * 12)  # lifeExp in months - new variable

gapminder %>% 
  mutate(lifeExp_months = lifeExp * 12) %>% 
  relocate(lifeExpMonths, .after = lifeExp) # lifeExp in months - new variable relocated

gapminder %>%
  filter(year == 2007) %>% 
  mutate(lifeExp_months = lifeExp * 12) %>% 
  arrange(desc(lifeExp_months))


