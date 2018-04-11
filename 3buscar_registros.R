library(dplyr)
library(readxl)
library(rgbif)
spp <- read_excel("./data/nombres/org_names_final.xlsx",
                  sheet = "final")

spp <- spp %>% select(1)
dim(spp)
length(unique(spp$`Lista final com sub espécies e variações`))
nombres <- sort(unique(spp$`Lista final com sub espécies e variações`))
write.table(nombres, "./data/nombres.txt")


#antes:
#aqui fiz a mão para tirar famílias, gëneros, sp. aff. e cf.
#nombres_processed <- read.table("./data/nombres/nombres_processed.txt")
#head(nombres_processed)
#nombres <- sort(unique(nombres_processed$V2))
#agora:
#mariana já limpou isto
#tem que tirar os autores mesmo assim
library(flora)
library(purrr)
rem_authors <- map(nombres, .f = remove.authors)
rem_authors2 <- purrr::flatten_chr(rem_authors)
# try 1, tuve que editar nombres[449] "Myroxylon peruiferumL.f."
which(spp$X__1 == "revisar")
length(rem_authors2)
nombres <- rem_authors2
#aqui deve ser 1 a 753 mas fiz por levas - modifico para que cate todos os keys e elimine duplicados:
key <- vector(length = length(nombres))
nombres[which(spp$X__1 == "revisar")]
nombres[365] <- "Marlierea silvatica"
which(nombres == "Rinorea guianensis")
nombres[518] <- "Phyllostylon brasiliense"
#checando subspecies
#grep(pattern = "var", nombres)
#nombres[grep(pattern = "var", nombres)]
#nombres[67]

for (i in 671) {
    print(nombres[i])
    key <- name_suggest(q = nombres[i], rank = 'variety')$key
    print(key)
    if (!is.null(key)) {
        occs <- list()
        for (k in 1:length(key)) {
            occs[[k]] <- occ_search(
                taxonKey = key[k],
                limit = 100000,
                hasCoordinate = TRUE,
                basisOfRecord = "PRESERVED_SPECIMEN",
                hasGeospatialIssue = F,
                return = 'data',
                fields = "minimal"
            )
        }

        print(lapply(occs,dim))
        if (any(!is.null(lapply(occs,dim)))) {
            dim.null <- lapply(occs, function(x) {!is.null(dim(x))})
            occs.f <- subset(occs, dim.null == T)
            occs.f <- bind_rows(occs.f)
            occs.f <- occs.f[!duplicated(occs.f[,c(1,3,4)]),]
            print(dim(occs.f))
            write.csv(
                x = data.frame(occs.f),
                file = paste0("./data/occs/", nombres[i], ".csv")
            )
    }
    } else {

            cat(paste("No key found for", nombres[i], "\n"))
        }
            cat(paste(nombres[i], "DONE", "\n"))
    }

#214 no records found
nombres[214]
library(stringr)
arquivos <- list.files("./data/occs")[-1] %>% str_split(pattern = "[:punct:]csv", simplify = T) %>% as.data.frame() %>% select(V1)
setdiff(arquivos$V1, data.frame(nombres)$nombres)
NORECORDS <- setdiff(data.frame(nombres)$nombres, arquivos$V1)
which(nombres %in% NORECORDS)
nombres[79]
nombres[285] <-  "Homalolepis subcymosa"
which(nombres == "Ocotea brachybothia")
nombres[470] <- "Ocotea brachybotrya"
nombres[89] <- "Capparis brasiliana"
nombres[115] <- "Chrysochlamys saldanhae"

#for (i in 115) {
#for (i in which(nombres %in% NORECORDS)) {
#for (i in which(nombres %in% NORECORDS)[10]) {
    print(nombres[i])
    key <- name_suggest(q = nombres[i], rank = 'species')$key
    print(key)
    if (!is.null(key)) {
        occs <- list()
        for (k in 1:length(key)) {
            occs[[k]] <- occ_search(
                taxonKey = key[k],
                limit = 100000,
                hasCoordinate = TRUE,
                basisOfRecord = "PRESERVED_SPECIMEN",
                hasGeospatialIssue = F,
                return = 'data',
                fields = "minimal"
            )
         }

        print(lapply(occs,dim))
        if (any(!is.null(lapply(occs,dim)))) {
            dim.null <- lapply(occs, function(x) {!is.null(dim(x))})
            occs.f <- subset(occs, dim.null == T)
            occs.f <- bind_rows(occs.f)
            occs.f <- occs.f[!duplicated(occs.f[,c(1,3,4)]),]
            print(dim(occs.f))
            write.csv(
                x = data.frame(occs.f),
                file = paste0("./data/occs/", nombres[i], ".csv")
            )
        }
    } else {

        cat(paste("No key found for", nombres[i], "\n"))
    }
    cat(paste(nombres[i], "DONE", "\n"))
}


##corrigiendo busquedas raras
n <- "Psychotria vellosiana"
    print(n)
    key <- name_suggest(q = n, rank = 'species')$key
    print(key)
        occs <- list()
        for (k in 1:length(key)) {
            occs[[k]] <- occ_search(
                taxonKey = key[k],
                limit = 100000,
                hasCoordinate = TRUE,
                basisOfRecord = "PRESERVED_SPECIMEN",
                hasGeospatialIssue = F,
                return = 'data',
                fields = "minimal"
            )
        }

        occs2 <- list()
            occs2 <- occ_search(
                #taxonKey = key[k],
                scientificName = n,
                limit = 100000,
                hasCoordinate = TRUE,
                basisOfRecord = "PRESERVED_SPECIMEN",
                hasGeospatialIssue = F,
                return = 'data',
                fields = "minimal"
            )


        print(lapply(occs,dim))
        if (any(!is.null(lapply(occs,dim)))) {
            dim.null <- lapply(occs, function(x) {!is.null(dim(x))})
            occs.f <- subset(occs, dim.null == T)
            occs.f <- bind_rows(occs.f)
            occs.f <- occs.f[!duplicated(occs.f[,c(1,3,4)]),]
            print(dim(occs.f))
            write.csv(
                x = data.frame(occs.f),
                file = paste0("./data/occs/", n, ".csv")
            )
        }
    } else {

        cat(paste("No key found for", nombres[i], "\n"))
    }
    cat(paste(nombres[i], "DONE", "\n"))
}
