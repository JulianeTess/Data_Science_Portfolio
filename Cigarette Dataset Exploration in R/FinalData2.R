# Adjust the price of a pack of cigarettes for inflation
  #re-do scatter plot and linear regression
NewCigInfl <- Cigarette %>% mutate(PriceInfl = avgprs/cpi)

CigInflScatter <- ggplot(NewCigInfl, aes(x = PriceInfl, y = packpc)) + geom_point() +
  geom_smooth(method = lm) + 
  xlab("Average Inflation Price (in cents)") + ylab("Packs per Capita") + 
  ggtitle("Average Inflation Price vs Packs per Capita")

# Create a scatter plot with years as different colors
CigInflScatterYear <- ggplot(NewCigInfl, aes(x = PriceInfl, y = packpc, color = year)) + geom_point() +
  geom_smooth(method = lm) + 
  xlab("Average Inflation Price (in cents)") + ylab("Packs per Capita") + 
  ggtitle("Average Inflation Price vs Packs per Capita")

# Arrange year color plots next to each other for comparison
grid.arrange(CigInflScatterYear, CigScatterYear, ncol = 1)

CigInflRegression <- lm(packpc ~ PriceInfl, NewCigInfl)
summary(CigInflRegression)
# How much variability does the line explain?
# -> 38% of the variability
# p-value < .05, thus the price still shows a significant impact on the packs of 
  #cigarettes sold per capita. When comparing the two graphs, both have a negative 
  #correlation and both can be said that the higher the price, the less amount of
  #cigarettes sell.
