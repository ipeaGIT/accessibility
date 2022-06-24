
##### Decay functions ------------------------

library(accessibility)
library(data.table)
library(ggplot2)

vec <- 0:100
decay_value <- 0.2
cutoff <- 50

step        <- decay_binary(cutoff=cutoff)
linear      <- decay_linear(cutoff=cutoff)
exponential <- decay_exponential(decay_value=decay_value)
power       <- decay_power(decay_value = decay_value)

df <- data.table(
  minutes = vec,
  binary =          step(vec),
  linear =          linear(vec),
  exponential =     exponential(vec),
  inverse_power =   power(vec)
)

df2 <- data.table::melt.data.table(data = df, id.vars = 'minutes', variable.name = 'decay_function', value.name = 'impedance_factor')

ggplot() +
  geom_line(data=df2, aes(x=minutes, y=impedance_factor, color=decay_function), show.legend = FALSE) +
  facet_wrap(.~decay_function, ncol = 2) +
  theme_minimal()


ggsave('./man/figures/decay_functions.png', width = 12, height = 10, units = 'cm')
