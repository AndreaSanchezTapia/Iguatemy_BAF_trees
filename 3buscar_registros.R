library(dplyr)
library(readxl)
library(rgbif)
spp <- read_excel("./data/nombres/org_names_final.xlsx",
                  sheet = "final")

spp <- spp %>% select(1)
dim(spp)
length(unique(spp$`Lista final com sub espécies e variações`))
nombres <- sort(unique(spp$`Lista final com sub espécies e variações`))

key <- vector(length = length(nombres))

write.table(nombres, "./data/nombres.txt")
#antes:
#aqui fiz a mão para tirar famílias, gëneros, sp. aff. e cf.
#nombres_processed <- read.table("./data/nombres/nombres_processed.txt")
#head(nombres_processed)
#nombres <- sort(unique(nombres_processed$V2))
#agora:
#mariana já limpou isto


length(nombres)
#aqui deve ser 1 a 753 mas fiz por levas - modifico para que cate todos os keys e elimine duplicados:
for (i in c(1:10)) {
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
            cat(paste(nombres[i], "DONE", "\n"))
        }
    }

