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
library(interp)
library(sfheaders)



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

########### spatial interpolation --------------------


# interpolate estimates to get spatially smooth result
temp_xy <-  df2 |>
            st_centroid() |>
            sfheaders::sf_to_df(fill = T)

accss_interp <- interp(temp_xy$x, temp_xy$y, temp_xy$access) |>
                with(cbind(access=as.vector(z),  # Column-major order
                           x=rep(x, times=length(y)),
                           y=rep(y, each=length(x)))) |>
                as.data.frame() %>% na.omit()

# find results' bounding box to crop the map
bb_x <- c(min(accss_interp$x), max(accss_interp$x))
bb_y <- c(min(accss_interp$y), max(accss_interp$y))

fig_interp <- ggplot(data=accss_interp) +
              geom_contour_filled(aes(x=x, y=y, z=access), alpha=.8, show.legend = F) +
              # scale_fill_viridis_c() +
              theme_void() +
              theme(panel.grid.major=element_line(colour="transparent")) +
              coord_sf(xlim = bb_x, ylim = bb_y, datum = NA) +
              scale_x_continuous(expand=c(0,0)) +
              scale_y_continuous(expand=c(0,0))



### save sticker  ------------------------

# big
sticker(fig,

        # package name
        package= expression( italic('accessibility')),  p_size= 4, p_y = 1.6, p_color = "gray95", p_family="Roboto",

        # ggplot image size and position
        s_x=1, s_y=.85, s_width=1.4, s_height=1.4,

        # blue hexagon
        h_fill="#440154", h_color="#440154", h_size=1.3,

        ## blackhexagon
        # h_fill="gray20", h_color="gray80", h_size=1.3,

        # url
        url = "github.com/ipeaGIT/accessibility", u_color= "gray95", u_family = "Roboto", u_size = 1.2,

        # save output name and resolution
        filename="./man/figures/logo.png", dpi=300 #
)

