library(hexSticker) # https://github.com/GuangchuangYu/hexSticker
library(ggplot2)
library(sf)
library(accessibility)
library(sysfonts)
library(data.table)

# add special text font
sysfonts::font_add_google(name = "Roboto", family = "Roboto")

### setup ------------------------

library(accessibility)
library(data.table)
library(ggplot2)
library(sf)


ttm_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
grid_path <- system.file("extdata/grid_bho.rds", package = "accessibility")
ttm <- readRDS(ttm_path)
grid <- readRDS(grid_path)

setdiff(ttm$from_id, grid$id)

unique(ttm$from_id) |> length()
unique(grid$id) |> length()

# Active accessibility: number of schools accessible from each origin
df <- cumulative_time_cutoff(data = ttm,
                             opportunity_colname = 'jobs',
                             cutoff = 30,
                             by_colname = 'from_id')
#
# df <- accessibility::gravity_access(data = ttm,
#                                     opportunity_colname = 'jobs',
#                                     decay_function = 'inverse_power',
#                                     decay_value = .5,
#                                     by_colname = 'from_id')

# access
df2 <- df[setDT(grid), on=c('from_id'='id'), geom := i.geom]

df2 <- st_sf(df2)



### network plot  ------------------------

# plot results
fig <- ggplot() +
  geom_sf(data=df2, aes(fill=access), color=NA, show.legend = F) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(panel.grid.major=element_line(colour="transparent"))

fig


### save sticker  ------------------------

# big
sticker(fig,

        # package name
        package= expression( italic(paste("R"^5,"R"))),  p_size=10, p_y = 1.5, p_color = "gray95", p_family="Roboto",

        # ggplot image size and position
        s_x=1, s_y=.85, s_width=1.4, s_height=1.4,

        # blue hexagon
        h_fill="#0d8bb1", h_color="white", h_size=1.3,

        ## blackhexagon
        # h_fill="gray20", h_color="gray80", h_size=1.3,

        # url
        url = "github.com/ipeaGIT/accessibility", u_color= "gray95", u_family = "Roboto", u_size = 1.8,

        # save output name and resolution
        filename="./man/figures/r5r_biagaa.png", dpi=300 #
)

