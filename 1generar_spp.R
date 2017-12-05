library(readxl)
library(dplyr)
library(flora)
library(purrr)

rj <- read_excel("./data/traits_comm_rj_all2.xlsx") %>%
    select(c(1,2)) %>%
    dplyr::slice(-c(1:2))
rj
rem_authors <- lapply(rj$Espécie,FUN = remove.authors)
rem_authors2 <- purrr::flatten_chr(rem_authors)
spp <- get.taxa(rem_authors2, habitat = T, states = T)
head(spp)
spp$no_authors <- rem_authors2

write.csv(spp, "./output/gettaxaresults.csv")
