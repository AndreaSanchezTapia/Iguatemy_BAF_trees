library(dplyr)
spp <- read.csv("./output/gettaxaresults.csv", row.names = 1)
library(rgbif)

spp <- spp %>% filter(!is.na(id)) %>% filter(no_authors != "?")
dim(spp)
nombres <- sort(unique(droplevels(spp$no_authors)))

key <- vector(length = length(nombres))

occs <- list()
write.table(nombres, "./data/nombres.txt")
#aqui fiz a mão para tirar famílias, gëneros, sp. aff. e cf.
nombres_processed <- read.table("./data/nombres_processed.txt")
head(nombres_processed)
nombres <- sort(unique(nombres_processed$V2))

length(nombres)
#aqui deve ser 1 a 742 mas fiz por levas
for (i in c(1:742)) {
    print(nombres[i])
    key <- name_suggest(q = nombres[i], rank = 'species')$key[1]
    print(key)
    if (!is.null(key)) {
        occs[[i]] <- occ_search(
            taxonKey = key,
            limit = 100000,
            hasCoordinate = TRUE,
            basisOfRecord = "PRESERVED_SPECIMEN",
            hasGeospatialIssue = F,
            return = 'data',
            fields = "minimal"
        )
        print(dim(occs[[i]]))
        if (!is.null(dim(occs[[i]]))) {
            write.csv(
                x = data.frame(occs[[i]]),
                file = paste0("./data/occs/", nombres[i], ".csv")
            )
            cat(paste(nombres[i], "DONE", "\n"))
        }
    }
}


##busqueda uno por uno de cosas que faltaron
nombre <- "Xylosma glaberrima"

#
    print(nombre)
    key <- name_suggest(q = nombre, rank = 'species')$key
        #key <- name_suggest(q = nombre, rank = 'species')$key[2]
        #key <- name_suggest(q = nombre, rank = 'species')$key[3]
    print(key)
    occs <- list()
    for (l in 1:length(key)) {

        occs[[l]] <- occ_search(
            taxonKey = key[l],
            limit = 100000,
            hasCoordinate = TRUE,
            basisOfRecord = "PRESERVED_SPECIMEN",
            hasGeospatialIssue = F,
            return = 'data',
            fields = "minimal"
        )
    }

        lapply(occs,dim)
occs.f <- occs[[1]]
#occs.f <- occs[[2]]
#occs.f <- occs[[3]]
 #       occs.f <- bind_rows(occs[[2]],occs[[3]])
  #      occs.f <- bind_rows(occs[[3]],occs[[4]])
   #     occs.f <- bind_rows(occs[[1]],occs[[3]],occs[[4]], occs[[5]])
    occs.f <- occs.f[!duplicated(occs.f[,c(1,3,4)]),]
dim(occs.f)
            if (!is.null(dim(occs.f))) {
            write.csv(
                x = data.frame(occs.f),
                file = paste0("./data/occs/", nombre, ".csv"))
            cat(paste(nombre, "DONE", "\n"))
        }

