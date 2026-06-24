# Analysis script
library(dplyr)
df %>% filter(hiv == 'positive') %>% summarise(n = n())
