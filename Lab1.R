library(tidyverse)
library(DataExplorer)

data <- read_csv("oz_analysis.csv")

data <- data |> 
  mutate(DesignatedOZ = 
           ifelse(is.na(DesignatedOZ), 
                  "Not Designated", "Designated"))

data |> 
  count(DesignatedOZ)

colSums(is.na(data))

plot_missing(data)

data |>
  summarise(state_value = median(medhhincome, na.rm = TRUE))

data |> 
  group_by(Type) |> 
  summarise(group_value = median(medhhincome, na.rm = TRUE))

ozs <- data |> 
  filter(Type != "Not Eligible")

ozs |> 
  filter(!is.na(medhhincome)) |> 
  group_by(DesignatedOZ) |> 
  summarise(mean = mean(medhhincome), 
            sd = sd(medhhincome), 
            min = min(medhhincome), 
            max = max(medhhincome))

ozs |> 
  ggplot (aes(x = medhhincome, fill = DesignatedOZ)) +
  geom_density(alpha = 0.5) + 
  scale_x_continuous(labels = scales::dollar) + 
  labs(x = "Median Household Income", fill = "Tracts")

ozs |> 
  ggplot(aes(x = DesignatedOZ, y = medhhincome, fill = DesignatedOZ)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(x = "Opportunity Zone Eligible Tracts", 
       y = "Median Household Income", 
       fill = "Tracts")

ozs |> ggplot() +   
  geom_violin(aes(x = DesignatedOZ, y = medhhincome, fill = DesignatedOZ), trim = FALSE, alpha = 0.5) + 
  geom_boxplot(aes(x = DesignatedOZ, y = medhhincome),                 color = "black", width = .15, alpha = 0.8) +   
  scale_y_continuous(labels = scales::dollar) +   
  labs(x = "Opportunity Zone Eligible Tracts",     
       y = "Median Household Income",     
       title = "Distribution of Median Household Income") +
  coord_flip() +   
  theme(legend.position = "none")

ozs |>    
  mutate(pctnonwhite = 1 - pctwhite) |> 
  ggplot(aes(x = pctnonwhite, y = medhhincome,
             color = DesignatedOZ)) +   
  geom_point() +   
  geom_smooth(method = "lm", se = FALSE) +   
  labs(x = "Proportion of Non-White Population",        
       y = "Median Household Income",        
       title = "Median Household Income vs. Proportion of Non-White Population in Opportunity Zone Eligible Tracts", 
       subtitle = "State of Massachusetts",        
       caption = "Source: Urban Institute (2021)") + 
  theme_bw()
