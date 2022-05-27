# Create a data frame with just the rows from 1985
Cig1985 <- Cigarette %>% filter(year == "1985")
# Create a data frame with just the rows from 1995
Cig1995 <- Cigarette %>% filter(year == "1995")

# Get a vector of the number of packpc from each of these data frames
# Use paired t-test to see if number of packpc in 1995 was sig. different than in 1985
t.test(Cig1985$packpc, Cig1995$packpc, paired = TRUE)
# p < .05; there is a significant difference between packs per capita in 1985 to 1995