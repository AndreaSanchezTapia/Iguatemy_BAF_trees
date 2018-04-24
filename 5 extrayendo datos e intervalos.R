library(readxl)
library(dplyr)
library(tidyr)
#esta fue creada con codigo viejo encontrado en itnernet para detectar cualquier duplicado - como ahora es especie por especie no lo estoy usando pero se podrían extraer mas vetted coordinates?

dupls <- read_excel("./output/com_duplicados.xlsx",sheet = )
dupls <- dupls %>% filter(is.na(cleaning))
dim(dupls)
names(dupls)
#quiero comparar estas dimensiones
ocurrencias.clean2 <- list.files("./data/occs_clean_vetted/",full.names = T) %>%
    purrr::map(., read.csv, row.names = 1)
all.clean.vetted <- data.table::rbindlist(ocurrencias.clean2)
dim(all.clean.vetted)#ok, perdí menos cosas? tengo más especies? ni idea.

nombres_occs <- list.files("./data/occs_clean_vetted/") %>%
    stringr::str_split(".csv",simplify = T) %>%
    data.frame() %>% dplyr::select(1)


table(dupls$name)[which.min(table(dupls$name))]
table(all.clean.vetted$name)[which.min(table(all.clean.vetted$name))]
table(dupls$name)[which.max(table(dupls$name))]
table(all.clean.vetted$name)[which.max(table(all.clean.vetted$name))]

#ahora todo esto es con all.clean.vetted
#podría ser para cada una pero bof

#Recordando a worldclim
# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (* 100)
# BIO4 = Temperature Seasonality (standard deviation *100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter

###La tentativa va a ser basada en esto
#https://www.painblogr.org/2017-10-18-purrring-through-bootstraps.html#fnref2




all.v <- all.clean.vetted %>%
    dplyr::select(-c(1,3,6,7)) %>% group_by(name)
names(all.v)
data_spp <- summarise(all.v,
                      n = n(),
                      #minlat = min(lat),
                      lat.05 = quantile(lat, 0.05),
                      lat.50 = quantile(lat, 0.5),
                      lat.95 = quantile(lat, 0.95),
                      #maxlat = max(lat),
                      #minlon = min(lon),
                      lon.05 = quantile(lon, 0.05),
                      lon.50 = quantile(lon, 0.5),
                      lon.95 = quantile(lon, 0.95),
                      #maxlon = max(lon),
                      #mintemp = min(bio1),
                      temp.05 = quantile(bio1, 0.05),
                      temp.50 = quantile(bio1, 0.5),
                      temp.95 = quantile(bio1, 0.95),
                      #maxtemp = max(bio1),
                      #trangemin = min(bio7),
                      trange.05 = quantile(bio7, 0.05),
                      trange.50 = quantile(bio7, 0.5),
                      trange.95 = quantile(bio7, 0.95),
                      #trangemax = max(bio7),
                      #minp = min(bio12),
                      p.05 = quantile(bio12, 0.05),
                      p.50 = quantile(bio12, 0.5),
                      p.95 = quantile(bio12, 0.95),
                      #maxp = max(bio12),
                      #seasonpmin = min(bio15),
                      seasonp.05 = quantile(bio15, 0.05),
                      seasonp.50 = quantile(bio15, 0.5),
                      seasonp.95 = quantile(bio15, 0.95),
                      #seasonpmax = max(bio15)
                      delta.lat = lat.95 - lat.05,
                      delta.lon = lon.95 - lon.05,
                      delta.temp = temp.95 - temp.05,
                      delta.trange = trange.95 - trange.05,
                      delta.p = p.95 - p.05,
                      delta.pseason = seasonp.95 - seasonp.05,
                      ) %>% select(name, n, contains(".50"), starts_with("delta"))
data_spp
write.csv(data_spp, "./output/resumospp.csv")

