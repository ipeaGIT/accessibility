library(data.table)
library(sf)
library(ggplot2)


sigla <- 'bho'

########## select radius  --------------------------------------------------
# acces <- aopdata::read_access(city=sigla, year=2019, geometry = T)
# acces <- readRDS('L:/Proj_acess_oport/data/acesso_oport/output_base_final/2019/dados2019_AcessOport_access_tpcar_v1.0.rds')
acces_for <- subset(acces, sigla_muni ==sigla & pico==1 & modo =='tp')

center <- acces_for$id_hex[which.max(acces_for$CMATT30)]

buff <- subset(acces_for, id_hex == center) |>
          st_centroid()  |>
           st_transform(crs = '+proj=utm +zone23 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0') |>
          st_buffer(dist = 7000) |>
          st_transform(crs = st_crs(acces_for))

plot(buff['id_hex'])

ggplot() +
  geom_sf(data=acces_for, aes(fill=CMATT30), color=NA) +
  geom_sf(data=buff, color='red', fill='red', alpha=.5)

buff$CMATT30 <- NULL
inter <- st_intersects(acces_for, buff, sparse = FALSE )

inter <- acces_for[inter, ]

ggplot() +
  geom_sf(data=inter, aes(fill=CMATT30), color=NA) # +geom_sf(data=buff, color='red', fill='red', alpha=.5)




########## travel time matrix --------------------------------------------------
# read ttm
ttm_files <- list.files('E:/data/ttmatrix_fixed/2019', full.names = T)
ttm_for <- ttm_files[ ttm_files %like% sigla]


dt <- readRDS(ttm_for)
head(dt)

ttm <- subset(dt,
              mode =='transit' &
              pico == 1 &
              ano == 2019 &
              city  == sigla &
              travel_time <= 120)


ttm[, c('pico', 'city', 'ano', 'mode') := NULL]
names(ttm) <- c('from_id', 'to_id', 'travel_time')
head(ttm)

ttm <- subset(ttm, from_id %in% unique(inter$id_hex) )
ttm <- subset(ttm, to_id %in% unique(inter$id_hex) )



########## land use  --------------------------------------------------
land_for <- aopdata::read_landuse(city=sigla, year=2019, geometry = F)
land_for <- land_for[, .(id_hex, P001, T001, E001)]
names(land_for) <- c('id', 'population', 'jobs', 'schools')


# merge opportunities data
ttm[land_for, on=c('from_id'='id'), population   := i.population  ]
ttm[land_for, on=c('to_id'='id'), jobs  := i.jobs ]
ttm[land_for, on=c('to_id'='id'), schools  := i.schools ]


# ttm2 <- subset(ttm, population > 0 | jobs >0 | schools >0 )
head(ttm)
summary(ttm)



########## grid  --------------------------------------------------
grid <- aopdata::read_grid(city = sigla)

grid$abbrev_muni <- NULL
grid$name_muni  <- NULL
grid$code_muni <- NULL
names(grid)[1] <- 'id'

grid2 <- subset(grid, id %in% unique(inter$id_hex) )
grid2 <- unique(grid2)
class(grid2)
plot(grid2)
head(grid2)

# check
setdiff(ttm$from_id, grid2$id)
setdiff(ttm$to_id, grid2$id)
setdiff(grid2$id, ttm$to_id)

unique(ttm$to_id) |> length()
unique(ttm$from_id) |> length()
unique(grid2$id) |> length()


# save files
saveRDS(ttm, './inst/extdata/ttm_bho.rds', compress = TRUE)
saveRDS(grid2, './inst/extdata/grid_bho.rds', compress = T)
