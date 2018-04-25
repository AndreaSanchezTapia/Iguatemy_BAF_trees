# 7 a partir de las ocurrencias limpias - ya existe la tabla.
ocurrencias.clean2 <- list.files("./data/occs_clean_vetted/",full.names = T) %>%
    purrr::map(., read.csv, row.names = 1)
all.clean.vetted <- data.table::rbindlist(ocurrencias.clean2)

nombres_occs <- list.files("./data/occs_clean_vetted/") %>%
    stringr::str_split(".csv",simplify = T) %>%
    data.frame() %>% dplyr::select(1)


# ahora queremos areas
library(raster)
w <-  getData('worldclim', var = 'bio', res = 2.5)
w <- w[[1]] #solo la primera capa

i <- 1
pixel.sum <- list()
for (i in seq_along(ocurrencias.clean2)) {
    (sp <- nombres_occs$X1[i])
    a <- ocurrencias.clean2[[i]]
    if (nrow(a) != 0) {
    coord <- cbind(a$lon, a$lat)
    presences <- raster::rasterize(coord, w, field = 1)
    pa <- getValues(presences)
    pixel.sum[[i]] <- data.frame(name = sp, pixelsum = sum(pa, na.rm = T))
     } else {
    pixel.sum[[i]] <- data.frame(name = sp, pixelsum = 0)
    }
}
pixel.sum <- data.table::rbindlist(pixel.sum)

areas_tudo <- dplyr::left_join(data_spp, pixel.sum)
areas_tudo
write.csv(areas_tudo, "./output/resumospp.csv")


#zonal para el shape de biomas
biomas <- rgdal::readOGR("./data/biomas","BR_BIOMAS_IBGE")
extent(biomas)
biomas@data$CD_LEGENDA
plot(biomas)
biomas.r <- rasterize(biomas, w, field = biomas@data$CD_LEGENDA)
biomas.r <- crop(biomas.r,extent(biomas))
plot(biomas.r)

vals <- getValues(biomas.r)
hist(vals, na.rm = T)

zonal.data <- list()
for (i in seq_along(ocurrencias.clean2)) {
    sp <- nombres_occs$X1[i]
    a <- ocurrencias.clean2[[i]]
    if (nrow(a) != 0) {
        coord <- cbind(a$lon, a$lat)
        png(paste("./zonal/zonal_",sp,".png"))
        plot(biomas.r, main = sp, legend = F)
        points(coord, cex = 0.5, pch = 19)
        maps::map(, , , add = T)
        dev.off()
        presences <- raster::rasterize(coord, biomas.r, field = 1)
        #presences <- crop(presences, extent(biomas.r))
        #presences <- mask(presences, biomas.r)
        #plot(presences, col = "red", add = F, legend = F)
        #maps::map(, , ,add = T)
        #pa <- getValues(presences)
        #sum(pa, na.rm = T)
        zonal.data[[i]] <- zonal(x = presences, z = biomas.r, fun = "sum", na.rm = T) %>%
            t(.) %>% tibble::as.tibble() %>% rename(Amazonia = V1,
                                                                    Cerrado = V2,
                                                                    Caatinga = V3,
                                                                    MataAtlantica = V4,
                                                                    Pampa = V5,
                                                                    Pantanal = V6) %>%
            dplyr::slice(-1) %>% mutate(name = sp)

        } else {
        #zonal.data[[i]] <- matrix(data = c(1:6,rep(0,6)), ncol = 2, dimnames = list(NULL, c("zone", "sum")))
        zonal.data[[i]] <- NULL
    }
}

zonal.data[[i]] %>% t(.) %>% tibble::as.tibble() %>% rename(Amazonia = V1,
                                                            Cerrado = V2,
                                                            Caatinga = V3,
                                                            MataAtlantica = V4,
                                                            Pampa = V5,
                                                            Pantanal = V6) %>%
    dplyr::slice(-1) %>% mutate(name = sp)
datos_biomas <- bind_rows(zonal.data)
datos_biomas

areas_tudo <- dplyr::left_join(data_spp, pixel.sum)
areas_tudo
datos_todo <- left_join(areas_tudo, datos_biomas)
write.csv(datos_todo, "./output/especies_final.csv")
