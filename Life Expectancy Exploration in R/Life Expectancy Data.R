gm_five <- gapminder %>% filter(country %in% 
  c("France","Norway","Spain","Sweden","Denmark")) %>%
  select(country,year,lifeExp,gdpPercap)
#Which country = lowest/highest per capita GDP in 1952? In 2007?
  #Spain has the lowest in both 1952 and 2007
  #Norway has the highest in both 1952 and 2007
gm_lowest1952 <- gm_five %>%
  filter(year==1952) %>%
  arrange(gdpPercap)
gm_lowest2007 <- gm_five %>%
  filter(year==2007) %>%
  arrange(gdpPercap)

gdp <- ggplot(gm_five) + geom_line(aes(x = year, y = gdpPercap, color = country)) +
  ylab("per capita GDP") + ggtitle("GDP in Five Countries")
grid.arrange(life_exp, gdp, ncol = 1)

life_exp <- ggplot(gm_five) + geom_line(aes(x = year, y = lifeExp, color = country))+
  ylab("Life Expectancy") + ggtitle("Life Expectancy in Five Countries")
#Starting in 1952, Spain shows the lowest life expectancy out of the five countries
#that were chosen, probably as a consequence of the civil war that some believe didn't, 
#in fact, end in 1939 but in 1952. Norway shows the highest life expectancy in 1952 out
#of the five countries chosen. Ending in 2007, Spain's life expectancy has dramatically
#increased and has nearly taken over as the highest along with Sweden. 
#Denmark's life expectancy has not had as much of a dramatic increase from 1952-2007, thus
#is the lowest out of the five countries in 2007.

life <- gapminder %>%
  group_by(year) %>%
  summarize(life.med = median(lifeExp))
gm_fivemedians <- gm_five %>%
  group_by(year) %>%
  summarize(life_med = median(lifeExp))
View(gm_fivemedians)

life.boxplot <- ggplot(life, aes(x = factor(year), y = life.med)) + geom_boxplot() + ylab("Median Life Expectancy") +
  ggtitle("All Countries Median Life Expectancy")
gm_fivemedians.boxplot <- ggplot(gm_fivemedians, aes(x = factor(year), y = life_med)) + geom_boxplot() +
  ylab("Median Life Expectancy") + ggtitle("Five Countries Median Life Expectancy")
grid.arrange(life.boxplot, gm_fivemedians.boxplot, ncol = 1)
#Without even looking at each of the years from 1952-2007 and just looking at the ages
#that the y-axis is organized with, one can see that the overall median ages for 
#the five countries selected have a dramatic higher average than the overall
#medians of the countries when calculated all together.


