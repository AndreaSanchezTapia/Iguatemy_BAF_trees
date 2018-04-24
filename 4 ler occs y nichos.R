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
#no hay repetidos para cada especie, no hay nada en el mar pero hay coordenadas sospechosas
ocurrencias <- list.files("./data/occs",full.names = T)[-1] %>%
    purrr::map(., read.csv)

# pintar mapas limpios----
library(dplyr)
#crea los mapas limpios
dir.create("./data/maps_clean")

#sp <- unique(occs_land$name)[1]
for (sp in unique(occs_land$name)) {
    pdf(paste0("./data/maps_clean/", sp, ".pdf"))
    maps::map(, c("Mexico","Argentina","Brazil"))
    maps::map(add = T)
    occs_sp <- occs_land %>% as.tbl() %>%
        dplyr::filter(name == sp) %>%
        dplyr::select(decimalLongitude, decimalLatitude)
    points(occs_sp$decimalLongitude,
           occs_sp$decimalLatitude,
           col = "red")
    title(sp)
    dev.off()
}
#creo que no fue suficiente: quitó las cosas en el mar.

#igual pero para todo el mundo
for (sp in unique(occs_land$name)) {
    pdf(paste0("./data/maps_clean/", sp, "_world.pdf"))
    maps::map(, ,)
    occs_sp <- occs_land %>% as.tbl() %>%
        dplyr::filter(name == sp) %>%
        dplyr::select(decimalLongitude, decimalLatitude)
    points(occs_sp$decimalLongitude,
           occs_sp$decimalLatitude,
           col = "red")
    title(sp)
    dev.off()
}
### falta quitar esos puntos de los polígonos
maps::map(,,)
points(occs_land$decimalLongitude,
       occs_land$decimalLatitude,
       col = "red")


a <- maps::map(,,)
b <- map.text("world",)
str(b)
plot(b$x, b$y)

poli_dupl <- occs_land[duplicated(cbind(occs_land$decimalLongitude, occs_land$decimalLatitude)),]
dim(poli_dupl)
dupli <- cbind(occs_land$decimalLongitude, occs_land$decimalLatitude)[duplicated(cbind(occs_land$decimalLongitude, occs_land$decimalLatitude)),]
dim(dupli)
poli_nondupl <- occs_land[!duplicated(cbind(occs_land$decimalLongitude, occs_land$decimalLatitude)),]
dim(poli_nondupl)

# #vector a vr si hay true false - no dio en nada y se demoró horrores
# dupl.vector2 <- vector(length = nrow(occs_land))
# i <- 1
# for (i in 1:length(dupl.vector2)) {
# #dupl.vector[i] <- occs_land$decimalLongitude[i] %in% dupli[,1] & occs_land$decimalLatitude[i] %in% dupli[,2]
# dupl.vector2 <-
#     }
# table(dupl.vector)

####no rodar#########
# test.dplyr <- occs_land %>% select(decimalLongitude, decimalLatitude) %>% unique()
# dim(occs_land)
# dim(test.dplyr)
# dim(poli_dupl)
# dim(poli_nondupl)
# #mejor marcarlos que quitarlos...
# ####PAILA también, no rodar#####
# occs_land$duplicado_poli <- ifelse(duplicated(cbind(occs_land$decimalLongitude, occs_land$decimalLatitude)),"yes","no")
# head(occs_land)
# write.csv(occs_land, "./output/limpiar.csv")
####PAILA también, no rodar#####


### La solucion_ encotnrado en internet
dat1 <- data.frame(occs_land$decimalLongitude, occs_land$decimalLatitude)
idx <- sapply(dat1,function(x) !is.na(match(x,x[duplicated(x)])))
dat1$dup <- apply(idx, 1, function(x) ifelse(all(x) == TRUE, TRUE, FALSE))
table(dat1$dup)
dim(dat1)
head(dat1)
dim(occs_land)
head(occs_land)
names(dat1)
names(occs_land)
names(dat1) <- c("decimalLongitude", "decimalLatitude","dup")
duplicates_checked <- left_join(occs_land[,-24], dat1)
head(duplicates_checked)
duplicates_checked2 <- duplicates_checked[!duplicated(duplicates_checked),]
head(duplicates_checked2)
dim(duplicates_checked2)
write.csv(duplicates_checked2, "com_duplicados.csv")
####NUNCAFILTRÊ POR DUP__F
####abrir en excel para ver...

#sin dupl de poli?
#aqui para detectar coordenadas raras y marcarlas en el excel
#length(unique(poli_nondupl$name))
#length(unique(occs_land$name))
for (sp in unique(duplicates_checked2$name)[33]) {
    #pdf(paste0("./data/maps_clean/", sp, "_world_poli_dupl.pdf"))
    maps::map(, ,)
    occs_sp <- duplicates_checked2 %>% as.tbl() %>%
        dplyr::filter(name == sp) %>%
        dplyr::select(decimalLongitude, decimalLatitude, dup)
    points(occs_sp$decimalLongitude,
           occs_sp$decimalLatitude,
           col = ifelse(occs_sp$dup == T, "red", "green"))
    title(sp)
    #dev.off()
}

aa <- occs_sp %>% select("decimalLongitude","decimalLatitude") %>% SpatialPoints()
points(aa)
raster::select(aa, use = 'rec')
library(readxl)
dupls <- read_excel("./output/com_duplicados.xlsx",sheet = )
dupls <- dupls %>% filter(is.na(cleaning))
dim(dupls)


#creo que se queda así

for (sp in unique(dupls$name)) {
    pdf(paste0("./output/maps_clean/", sp, "_final.pdf"))
    maps::map(, ,)
    occs_sp <- dupls %>% as.tbl() %>%
        dplyr::filter(name == sp) %>%
        dplyr::select(decimalLongitude, decimalLatitude, dup)
    points(occs_sp$decimalLongitude,
           occs_sp$decimalLatitude,
           col = ifelse(occs_sp$dup == T, "red", "green"))
    title(sp)
    dev.off()
}

#####ahora los intervalos pero ya acá se puede àsar al script siguiente....
###aqui una tentativa de comparar las consecuencias del data cleaning. hacer después de limpiar mesmo.
library(tidyr)
library(ggplot2)
names(occs_land)
names(dupls)
#tidy_occs <- gather(occs_land, key = "VARIABLE",,5:23)
tidy_land <- occs_land %>% select(-24) %>% mutate(Data = "clean") %>% gather(key = "VARIABLE",,5:23)

tidy_poli <- dupls %>% select(-c(1,25,26)) %>% mutate(Data = "Poli") %>% gather(key = "VARIABLE",,5:23)

tidy_data <- rbind(tidy_land,tidy_poli)

tidy_data %>%
    filter(name == unique(tidy_data$name)[28]) %>%
    ggplot(aes(x = VARIABLE,y = value)) +
    geom_boxplot(aes(col = Data))
### no voy a quedarme comparando


