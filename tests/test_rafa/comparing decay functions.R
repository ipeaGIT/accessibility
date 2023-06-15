
### setup ------------------------

library(accessibility)
library(data.table)
library(ggplot2)
library(sf)
library(interp)
library(sfheaders)

library(dplyr)

### prepate input data ---------------------------------
ttm_path <- system.file("extdata/travel_matrix.rds", package = "accessibility")
grid_path <- system.file("extdata/grid_bho.rds", package = "accessibility")
landuse_path <- system.file("extdata/land_use_data.rds", package = "accessibility")
ttm <- readRDS(ttm_path)
grid <- readRDS(grid_path)
landuse <- readRDS(landuse_path)


a <- left_join(landuse, grid)
a <- st_sf(a)
class(a)
ggplot() +
  geom_sf(data=a, aes(fill=schools), color=NA, show.legend = F) +
  scale_fill_viridis_c(direction = -1) +
  theme_void() +
  theme(panel.grid.major=element_line(colour="transparent"))

### prepate decay functions ---------------------------------

binary <- decay_binary(cutoff = 30)
linear <- decay_linear(cutoff = 60)
inverse_power <- decay_power(decay_value = 0.8)




# Active accessibility: number of schools accessible from each origin

df_tmi <- accessibility::cost_to_closest(travel_matrix = ttm,
                                 land_use_data = landuse,
                                 travel_cost = 'travel_time',
                                 opportunity = 'schools')

df_cum <- accessibility::gravity(travel_matrix = ttm,
                                 land_use_data = landuse,
                                 travel_cost = 'travel_time',
                                 opportunity = 'schools',
                                 decay_function = binary)

df_lin <- accessibility::gravity(travel_matrix = ttm,
                                 land_use_data = landuse,
                                 travel_cost = 'travel_time',
                                 opportunity = 'schools',
                                 decay_function = linear)

df_gra <- accessibility::gravity(travel_matrix = ttm,
                                           land_use_data = landuse,
                                           travel_cost = 'travel_time',
                                           opportunity = 'schools',
                                            decay_function = inverse_power)
df_cum$decay <- 'binary_30'
df_lin$decay <- 'linear_60'
df_gra$decay <- 'negexp_.1'

df <- rbind(df_cum, df_lin, df_gra)

# access
df2 <- df_lin[setDT(grid), on=c('id'='id'), geom := i.geom]

df2 <- st_sf(df2)



ggplot() +
  geom_sf(data=df2, aes(fill=schools), color=NA, show.legend = F) +
  scale_fill_viridis_c(option = 'inferno') +
  # facet_wrap(.~decay) +
  theme_void() +
  theme(panel.grid.major=element_line(colour="transparent"))
ggsave('aaa.png', dpi=300, width = 10, height = 10, units = 'cm')
