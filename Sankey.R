# Install and load necessary packages
install.packages(c("dplyr", "ggplot2", "ggalluvial", "networkD3"))
install.packages("ggalluvial")
installed.packages("networkD3")
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(networkD3)

# 1. Create a Random Dataset for Sankey Plot
set.seed(123)  # For reproducibility
n <- 100  # Number of observations

data <- data.frame(
  Hospital = sample(c("A", "B", "C"), n, replace = TRUE),
  Gender = sample(c("Male", "Female"), n, replace = TRUE),
  AgeGroup = sample(c("Young", "Adult", "Elderly"), n, replace = TRUE),
  Outcome = sample(c("Recovered", "Deceased"), n, replace = TRUE),
  Dischargeto = sample(c("Home", "Rehab", "Other"), n, replace = TRUE)
)

# 2. Sankey Diagram using networkD3 (Interactive)
# Prepare Data for Sankey
library(tidyr)
long_data <- data %>% 
  pivot_longer(cols = everything(), names_to = "Stage", values_to = "Value")

nodes <- data.frame(name = unique(long_data$Value))

links <- long_data %>%
  group_by(Stage, Value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(
    source = match(Value, nodes$name) - 1,
    target = match(lead(Value, default = NA), nodes$name) - 1
  ) %>%
  na.omit()

sankey_plot <- sankeyNetwork(Links = links, Nodes = nodes, 
                             Source = "source", Target = "target", Value = "count",
                             NodeID = "name", fontSize = 14, nodeWidth = 30)

# Show the Sankey Plot
sankey_plot


# 3. Fancy Static Sankey Plot using ggalluvial
ggplot(data = data, aes(axis1 = Hospital, axis2 = Gender, axis3 = AgeGroup, axis4 = Outcome, axis5 = Dischargeto)) +
  geom_alluvium(aes(fill = Outcome), width = 1/12) +
  geom_stratum(aes(fill = Hospital), width = 1/12, color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_void() +
  ggtitle("Zafs_sankey_Linkedin") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
