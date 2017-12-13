library(dplyr)
spp <- read.csv("./output/gettaxaresults.csv", row.names = 1)
#hay bastante especies no encontradas
dim(spp)
head(spp)
sum(spp$accepted.name == NA, na.rm = T)
table(spp$taxon.rank)
spp %>% dplyr::filter(taxon.rank =="species") %>% select(taxon.status) %>% table()


gen_epi <- str_split(spp$no_authors,pattern = " ",simplify = T) %>%
    as.data.frame() %>% rename(genus = V1, epithet = V2) %>%
    select(genus, epithet)
dim(gen_epi)
dim(spp)
head(spp)
head(gen_epi)
spp2 <- cbind(spp, gen_epi)
write.csv(spp2, "./output/gettaxaresults.csv")
