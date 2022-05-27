View(Cigarette)
head(Cigarette)
# Load in libraries
library("ggplot2")
library("dplyr")
# Box plot of avg. number of packs per capita ("packpc") by "state"
Cig.boxplot <- ggplot(Cigarette,aes(x = state, y = packpc)) + geom_boxplot() +
  xlab("State") + ylab("Packs Per Capita") + 
  ggtitle("Average Number of Packs Per Capita by State")
# Box plot of mean avg. number of packs per capita by state
ggplot(Cig.boxplotL, aes(x = state, y = Mean, color = state)) + geom_boxplot()
# Organize the info. for lowest and hights data, to see easier
Cig.boxplotL <- Cigarette %>% group_by(state) %>% summarise(Mean = mean(packpc)) %>% arrange(Mean)
Cig.boxplotH <- Cigarette %>% group_by(state) %>% summarise(Mean = mean(packpc)) %>% arrange(desc(Mean))
# Highest? KY followed by NH; Lowest? UT followed by NM
  #Both should be noted that the lowest and two highest states are far below/above all
  #other states and the closest ones to their avg. were at least 15 counts away.


# Median over all the "state"s of the number of packs per capita ("packpc") for each year
CigMedian <- Cigarette %>% group_by(year) %>% summarise(Median = median(packpc))
# Plot median value for 1985-1995
CigMedYear <- ggplot(CigMedian, aes(x = year, y = Median)) + geom_point() +
  xlab("Year") + 
  ylab("Median Packs per Capita for All States") +
  ggtitle("Median Packs per Capita for All States from 1985-1995")
# From 1985-1992 there is a steady decline of packs per capita bought each year,
  #but once you reach 1993 you can see sales have about leveled off and continue at 
  #a similar rate for the last two years within the Cigarette data set

# Create a scatter plot of "avgprs" (in cents) vs number of packs per capita for all states and all years
CigScatter <- ggplot(Cigarette, aes(x = avgprs, y = packpc)) + geom_point() +
  geom_smooth(method = lm) + 
  xlab("Average Price (in cents)") + ylab("Packs per Capita") + 
  ggtitle("Average Price vs Packs per Capita")
cor.test(Cigarette$avgprs, Cigarette$packpc, method = "pearson", use = "complete.obs")
# The avg. price and the per capita are negatively correlated; this is expected
  #as one would assume that as the price increases over the years, the packs 
  #bought would decrease

# Create a scatter plot with years as different colors
CigScatterYear <- ggplot(Cigarette, aes(x = avgprs, y = packpc, color = year)) + geom_point() +
  geom_smooth(method = lm) + 
  xlab("Average Price (in cents)") + ylab("Packs per Capita") + 
  ggtitle("Average Price vs Packs per Capita")

# Calculate the linear regression
CigRegression <- lm(packpc ~ avgprs, Cigarette)
summary(CigRegression)
# How much variability does the line explain?
  #-> 34% of the variability
# Packs per Capita are going to decrease by -0.41 for every average one unit price increase.
  #Average price per pack accounts for 34% of everything that influences the packs per capita.
# The p-value is < .05 so the overall model is significant. Price per pack
  #is a significant predictor of the packs per capita sold. The higher 
  #the price, the lower number of packs of cigarettes are sold.