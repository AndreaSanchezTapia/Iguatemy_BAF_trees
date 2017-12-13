library(purrr)
ocurrencias <- map(list.files("./data/occs",full.names = T), read.csv)
nombres_occs <- list.files("./data/occs") %>% stringr::str_split(".csv",simplify = T) %>% data.frame() %>% dplyr::select(1) %>% write.table("nombres_occs.txt")

length(ocurrencias)
names(ocurrencias[[1]])
map(ocurrencias, plot(decimalLongitude))
dir.create("./data/maps")
i <- 1
for (i in seq_along(ocurrencias)) {
    sp <- unique(ocurrencias[[i]]$name)
    pdf(paste0("./data/maps/", sp, ".pdf"))
    maps::map(, "Brazil", )
    maps::map(add = T)
    points(ocurrencias[[i]]$decimalLongitude,
           ocurrencias[[i]]$decimalLatitude,
           col = "red")
    title(sp)
dev.off()
}

library(data.table)
gbif_all <- rbindlist(l = ocurrencias,
                      use.names = T,
                      fill = T
                      #idcol = "id"
                      )

head(gbif_all)
dim(gbif_all)
write.csv(gbif_all, "./data/gbif_all.csv")
