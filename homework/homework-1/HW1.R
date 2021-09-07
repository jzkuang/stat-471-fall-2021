library(tidyverse) # tidyverse
library(ggrepel) # for scatter plot point labels
library(kableExtra) # for printing tables
library(cowplot) # for side by side plots

#### 1 ####
## 1.1
mlb_raw <- read_csv(file = "stat-471-fall-2021/data/MLPayData_Total.csv") #Importing a csv
# 54 variables and 30 observations
# Yes
## 1.2
mlb_aggregate <- mlb_raw %>% 
  select("Team.name.2014", "payroll", "avgwin") %>% #Selecting the columns in the new tibble
  rename("team" = "Team.name.2014", "payroll_aggregate" = "payroll", #Renaming the columns
         "pct_wins_aggregate" = "avgwin")
mlb_yearly <- mlb_raw %>% 
  select(!c("payroll", "avgwin")) %>% #Selecting the columns in the new tibble
  rename("team" = "Team.name.2014") %>% #Renaming the columns
  pivot_longer(!"team", names_to = "col_name", values_to = "values") %>% 
  separate("col_name", c("prefix", "year", "suffix"), c(1, 5)) %>% #Separating the columns
  mutate("tidy_col_name" = 
          recode(paste0(prefix,suffix), p = "payroll", X = "num_wins", X.pct = "pct_wins"), 
         .keep = "unused") %>%  #Keep only the unused columns
  pivot_wider(names_from = "tidy_col_name", values_from = "values")
# Contain 5 510 and 30 rows
## 1.3
mlb_aggregate_computed <- mlb_yearly %>% #Creating the computed values' tibble
  group_by(team) %>% 
  summarise(payroll_aggregate_computed = sum(payroll)/1000, #Adding up the payrolls 
            pct_wins_aggregate_computed = sum(num_wins)/sum(num_wins/pct_wins)) #Computing win percentages

mlb_aggregate_joined <- mlb_aggregate %>% # Creating a merged tibble
  merge(mlb_aggregate_computed)

payroll_comparison <- ggplot(mlb_aggregate_joined, # Plotting the computed vs actual payroll values
                             aes(x= payroll_aggregate_computed, y = payroll_aggregate)) +
  geom_point() +
  geom_abline(slope=1, intercept= 0) #Adding the 45 degree line
pct_wins_comparison <- ggplot(mlb_aggregate_joined, # Plotting the computed vs actual pctwin values
                             aes(x= pct_wins_aggregate_computed, y = pct_wins_aggregate)) +
  geom_point() +
  geom_abline(slope=1, intercept= 0) #Adding the 45 degree line
payroll_comparison
#### 2 ####
## 2.1 ##
mlb_aggregate_computed <- mlb_aggregate_computed %>% # Calculate the average payroll
  mutate(average_payroll = payroll_aggregate_computed/17 * 1000)
ggplot(mlb_yearly, aes(x = year, y = payroll)) + # Plot the payroll by year
  geom_point() +
  facet_wrap(team ~ .) + # Facet this graph by teams
  geom_hline(mlb_aggregate_computed, # Add the dashed, red lines of the mean payroll to each graph
             mapping = aes(yintercept = average_payroll), linetype='dashed', col = 'red') +
  theme(axis.text.x = element_text(
    angle = 90, size = 7.5, vjust = 0.5, hjust=.9)) # A little theme working to make it better

greatest_aggregate_payroll <- mlb_aggregate_computed %>% #Getting the three highest payroll teams
  arrange(desc(payroll_aggregate_computed)) %>%  # Arrange them from highest to lowest aggreggate payroll
  slice(1:3) # Get the top 3
greatest_payroll_increase <- mlb_yearly %>% 
  select(c(team, year, payroll)) %>% # Get only the columns needed to find the greatest increase
  pivot_wider(names_from = year, values_from = c(payroll), 
          names_prefix = "payroll_") %>% # Pivoting it wider to make it easier to calculate the increase
  mutate(payroll_increase = 
           (payroll_2004 - payroll_1998)/payroll_1998) %>% #Calculating the increase
  arrange(desc(payroll_increase)) %>% # Arranging the data from highest to lowest payroll increase
  slice(1:3) # Getting the top 3
## 2.2 ##
mlb_average_winrate <- mlb_yearly %>% # Calculating the average win rate for each team
  group_by(team) %>% 
  summarise(avg_winrate = mean(pct_wins), # The average win rate
            pct_wins_sd = sd(pct_wins)) # The sd of the win rates
ggplot(mlb_yearly, aes(x = year, y = pct_wins)) + # Plot the win rate by year
  geom_point() +
  facet_wrap(team ~ .) + # Facet this graph by teams
  geom_hline(mlb_average_winrate, # Add the dashed, red lines of the mean win rate to each graph
             mapping = aes(yintercept = avg_winrate), linetype='dashed', col = 'red') +
  theme(axis.text.x = element_text(
    angle = 90, size = 7.5, vjust = 0.5, hjust=.9)) # A little theme working to make it better
greatest_aggregate_winrate <- mlb_aggregate %>% #Getting the three highest teams
  arrange(desc(pct_wins_aggregate)) %>%  # Arrange them from highest to lowest aggregate payroll
  select(c("team", "pct_wins_aggregate")) %>% 
  slice(1:3) # Get the top 3
greatest_aggregate_sd <- mlb_average_winrate %>% #Getting the three highest win rate teams
  arrange(desc(pct_wins_sd)) %>%  # Arrange them from highest to lowest aggregate payroll
  select(c("team", "pct_wins_sd")) %>% 
  slice(1:3) # Get the top 3
#answer q
## 2.3 ##
ggplot(data = mlb_aggregate, aes(x=payroll_aggregate, y = pct_wins_aggregate)) +
  geom_point() +
  geom_text_repel(aes(label = team)) + 
  geom_smooth(method='lm')
## 2.4 ##
mlb_aggregate <- mlb_aggregate %>% 
  mutate(efficiency = pct_wins_aggregate/payroll_aggregate)
greatest_aggregate_efficiency <- mlb_aggregate %>% #Getting the three highest efficiency teams
  arrange(desc(efficiency)) %>%  # Arrange them from highest to lowest aggregate payroll
  select(c("team", "efficiency")) %>% 
  slice(1:3) # Get the top 3



#### 3 ####
## 3.1 ##
lm_pctwins <- lm(mlb_aggregate, formula = pct_wins_aggregate~payroll_aggregate) # 
summary(lm_pctwins)
## 3.2 ##
lm_payroll <- lm(formula = payroll_aggregate~pct_wins_aggregate, data =mlb_aggregate)
summary(lm_payroll)
  