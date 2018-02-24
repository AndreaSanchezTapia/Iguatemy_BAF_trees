# selecionar especies para medicao de traits.
library(readxl)
library(dplyr)
library(tibble)
#le a tabela
rm(com)
com <- read_excel("./data/all_community 3.xlsx", sheet = "din", skip = 3)
head(com)
tail(com)
dim(com)
com[,10]
#substitui por zeros
com[is.na(com)] <- 0

com <- com %>% data.frame(row.names = 1)
com <- t(com)#para que fique locais nas linhas especies nas colunas
head(com)[,1:5]
numerodeindividuos <- colSums(com)
sort(numerodeindividuos, decreasing = T)[1:21]
sum(numerodeindividuos > 99)
frequencia <- colSums(com > 0)
sort(frequencia, decreasing = T)[22:30]
write.table(frequencia, "frequentes.txt")
write.table(numerodeindividuos, "abundantes.txt")

local.n <- list()
par(mfcol = c(3, 3), mar = c(4, 5, 1, 1))
i <- 1
dim(com)
for (i in c(1:nrow(com))) {
    a <- sort(com[i,], decreasing = T)
    b  <- as.matrix(a)
    c <- cumsum(b)
    plot(c, pch = 19, main = rownames(com)[i])
    abline(h = rowSums(com)[i] * 0.8,col = "red")
    d <-  which(c > ((rowSums(com)[i]) * 0.80))[1]
    local.n[[i]] <- names(a[1:d])
}

local.n[[76]]
com2 <- vegan::decostand(com, "pa")
head(com2)[,1:5]

for (i in 1:nrow(com2)) {
    com2[i,] <- ifelse(names(com2[i,]) %in% local.n[[i]], yes = 1, no = 0)
}

dim(com2)
write.table(com2, "com2.txt")
total.list <- rbindlist(local.n, fill = T)
sum(colSums(com2) == 0)
head(com2[,1:5])
com3 <- com2[,colSums(com2) != 0]
write.csv(com3,file = "com3.csv",quote = T)
library(vegan)
aa <- vegan::rda(decostand(com3, "hellinger"))
par(mfrow = c(1,1))
plot(aa)

bloques <- vector(length = nrow(com))
for (i in 1:length(local.n)) bloques[i] <- rownames(local.n[[i]])
library(data.table)
local.selected.80 <- rbindlist(local.n,fill = T,use.names = F)
local.selected.80 <- data.frame(local.selected.80)
row.names(local.selected.80) <- bloques
head(local.selected.80)



resumo <- read_excel("./output/resumospp.xlsx")
setdiff(resumo$name, colnames(com3))

library(flora)
nomes.sem.autor <- vector(length = ncol(com3))
for (i in 1:ncol(com3)) nomes.sem.autor[i] <- remove.authors(colnames(com3)[i])

setdiff(nomes.sem.autor, resumo$name)
setdiff(resumo$name, nomes.sem.autor)
