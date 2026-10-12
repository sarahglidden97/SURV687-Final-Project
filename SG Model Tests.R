## SG's rough modeling notes.
## Please feel free to change or add more to the bottom!

library(rfars)
library(tidyverse)
library(lme4)
library(nlme)

fars <- get_fars(proceed = TRUE)

flat <- as.data.frame(fars$flat)

code_book <- as.data.frame(test$codebook)

states <- as.vector(unique(flat$state))

set.seed(1234)

random_states <- sample(states, size = 10, replace = FALSE)

df <- flat %>%
  filter(state %in% c("Arkansas",
                      "Wyoming",
                      "Maryland",
                      "Rhode Island",
                      "Maine",
                      "Arizona",
                      "Ohio",
                      "New Jersey",
                      "Nevada",
                      "California")) %>%
  filter(!(is.na(deaths)))

df_vis <- df %>%
  group_by(state,
           year) %>%
  summarise(death_tot = sum(deaths, na.rm = T))

p2 <- ggplot(df_vis, aes(x = year, y = death_tot, group = state, color = as.factor(state)))
p2 + geom_point() + geom_line()

mod1 <- lmer(deaths ~ year + fatals + speedrel + (1|state) + (state|year), REML = TRUE, data = df)
summary(mod1)