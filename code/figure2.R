library("tidyverse")

source("emissions.R")

d = data.frame(N = seq(0,240, by=40)) %>% 
  mutate(emissions = emissions(N),
         hw = emissions_hw(N),
         lower = emissions - qnorm(.975) * hw,
         upper = emissions + qnorm(.975) * hw)

ggplot(d, aes(N)) + 
  geom_line(aes(y = emissions), linetype = "dotted") + 
  geom_line(aes(y = lower)) + 
  geom_line(aes(y = upper)) +
  theme_bw()
