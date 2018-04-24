# esto está bien desordenado...
library(purrr)
library(maps)
# occurrencias sin limpiar----
ocurrencias <- list.files("./data/occs",full.names = T)[-1] %>%
    purrr::map(., read.csv)
nombres_occs <- list.files("./data/occs")[-1] %>%
    stringr::str_split(".csv",simplify = T) %>%
    data.frame() %>% dplyr::select(1)
write.table(x = nombres_occs, "nombres_occs.txt")
nombres_occs$X1
length(ocurrencias)
names(ocurrencias[[1]])

#crea los mapas sin limpiar pero solo una vez----
# dir.create("./data/maps")
# #i <- 1
# for (i in seq_along(ocurrencias)) {
#     sp <- unique(ocurrencias[[i]]$name)
#     pdf(paste0("./data/maps/", sp, ".pdf"))
#     maps::map(, "Brazil", )
#     maps::map(add = T)
#     points(ocurrencias[[i]]$decimalLongitude,
#            ocurrencias[[i]]$decimalLatitude,
#            col = "red")
#     title(sp)
# dev.off()
# }

# crea una tabla completa pero yano la usa----
library(data.table)
gbif_all <- rbindlist(l = ocurrencias,
                       use.names = T,
                       fill = T
                       #idcol = "id"
                       )

head(gbif_all)
dim(gbif_all)
write.csv(gbif_all, "./data/gbif_all.csv")
#
# ya no limpia datos a partir de la tabla grande
#gbif_all <- read.csv("./data/gbif_all.csv",row.names = 1)
head(gbif_all)
dim(gbif_all)
#
# # limpia los datos
# # duplicados
# library(dplyr)
occs_clean <- gbif_all %>%
     select(name, decimalLatitude, decimalLongitude) %>%
     distinct()
 dim(occs_clean)
 write.csv(occs_clean, "./data/occs_clean.csv")
#occs_clean <- read.csv( "./data/occs_clean.csv", row.names = 1)

#bajar wordlclim----
library(raster)
w <-  getData('worldclim', var = 'bio', res = 2.5)
#no toca leer esto de nuevo, w ya tiene todo, es una lista

#cosas que caen en el mar----
    for (i in seq_along(ocurrencias)) {
        df <- ocurrencias[[i]]
        df %>% dplyr::select(decimalLongitude, decimalLatitude) %>%
            SpatialPoints() %>%
            raster::extract(w,.,cellnumber = T) %>%
    data.frame() %>% cbind(df,.) %>%
            distinct(cells, .keep_all = T) %>%
    filter(!is.na(bio1)) %>%
    write.csv(file = paste0("./data/occs_clean/",nombres_occs$X1[i],".csv"))
}
#una lista de puntos sin NA.s

# no hay repetidos para cada especie, no hay nada en el mar pero hay coordenadas sospechosas
# lee todas las ocurrencias limpias
library(dplyr)

ocurrencias.clean <- list.files("./data/occs_clean",full.names = T) %>%
    purrr::map(., read.csv, row.names = 1)
nombres_occs <- list.files("./data/occs_clean/") %>%
    stringr::str_split(".csv",simplify = T) %>%
    data.frame() %>% dplyr::select(1)

#quitar ocurrencias extrañas----
vetted <- read.csv("data/vetted coordinates.csv", sep = ";", colClasses = c("numeric", "numeric"))
#vetted <- round(vetted, 4)
log <- list()

dir.create("./data/occs_clean_vetted")
for (i in seq_along(ocurrencias.clean)) {
    df <- ocurrencias.clean[[i]] %>%
        rename(lon = decimalLongitude, lat = decimalLatitude) %>%
        mutate(lon = round(lon, 5), lat = round(lat, 5))
    log[[i]] <- semi_join(df,vetted)
    df.with.no.strange <- anti_join(df, vetted)
    write.csv(df.with.no.strange, file = paste0("./data/occs_clean_vetted/", nombres_occs$X1[i],".csv"))
    if (nrow(df) != nrow(df.with.no.strange)) cat(paste(nombres_occs$X1[i],nrow(df) - nrow(df.with.no.strange), "points deleted \n"))
}

# pintar mapas limpios----
ocurrencias.clean2 <- list.files("./data/occs_clean_vetted/",full.names = T) %>%
    purrr::map(., read.csv, row.names = 1)
nombres_occs <- list.files("./data/occs_clean_vetted/") %>%
    stringr::str_split(".csv",simplify = T) %>%
    data.frame() %>% dplyr::select(1)
#crea los mapas limpios-----
dir.create("./data/maps_clean")
for (i in seq_along(ocurrencias.clean2)) {
    sp <- nombres_occs$X1[i]
    pdf(paste0("./data/maps_clean/", sp, ".pdf"))
    maps::map(, c("Mexico","Argentina","Brazil"))
    maps::map(add = T)
    points(ocurrencias.clean2[[i]]$lon,
           ocurrencias.clean2[[i]]$lat,
           col = "red")
    title(sp)
    dev.off()
    }

#igual pero para todo el mundo (tum dünya için)
for (i in seq_along(ocurrencias.clean2)) {
    sp <- nombres_occs$X1[i]
    pdf(paste0("./data/maps_clean/", sp, "_world.pdf"))
    maps::map(, ,)
    points(ocurrencias.clean2[[i]]$lon,
           ocurrencias.clean2[[i]]$lat,
           col = "red")
    title(sp)
    dev.off()
}

###toca volver a ver vetted -- voy a revisar los mapas a mano y seguir completando la list
# anti join y en general r tienen problemas cuando se trata de reconocer numeros floating point ntonces tocaba redondear a 5 digitos todo.

library(sp)
maps::map(,,)
pts <- ocurrencias.clean2[[1]] %>% select("lon","lat") %>% SpatialPoints(.)
points(pts, col = "red")
selected <- raster::select(pts, use = 'rec')
selected@coords

#####ya acá se puede pasar al script siguiente....
###aqui habia una tentativa de comparar las consecuencias del data cleaning. antes y despues y boxplots.

