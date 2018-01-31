library(readxl)
library(dplyr)
library(tidyr)

dupls <- read_excel("./output/com_duplicados.xlsx",sheet = )
dupls <- dupls %>% filter(is.na(cleaning))
dim(dupls)
names(dupls)
knitr::kable(table(dupls$name))
hist(table(dupls$name), xlab = "")
table(dupls$name)[which.min(table(dupls$name))]
table(dupls$name)[which.max(table(dupls$name))]
tidy_poli <- dupls %>%
    dplyr::select(-c(1,25,26)) %>%
    mutate(Data = "Poli") %>%
    gather(key = "VARIABLE", , 5:23)

dplyr::glimpse(tidy_poli)
#Recordando a worldclim
# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (* 100)
# BIO4 = Temperature Seasonality (standard deviation *100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter

###La tentativa va a ser basada en esto
#https://www.painblogr.org/2017-10-18-purrring-through-bootstraps.html#fnref2

data_nest <- tidy_poli %>%
    #First I filter the variables that I want
    dplyr::filter(VARIABLE %in% c("bio1", "bio7", "bio12", "bio15")) %>%
    # First I group the data by species
    dplyr::group_by(name, VARIABLE)

data_nest %>% filter(VARIABLE)
summarise(data_nest, max = max())
    # Then I nest the dataframe
    tidyr::nest()
data_nest
boot_max <- function(d, i) {
    max(d[i])
}




dupls2 <- dupls %>%
    dplyr::select(-c(1,25,26)) %>% group_by(name)
names(dupls2)
data_spp <- summarise(dupls2,
                      n = n(),
          minlat = min(decimalLatitude),
          lat.05 = quantile(decimalLatitude, 0.05),
          lat.50 = quantile(decimalLatitude, 0.5),
          lat.95 = quantile(decimalLatitude, 0.95),
          maxlat = max(decimalLatitude),
          minlon = min(decimalLongitude),
          lon.05 = quantile(decimalLongitude, 0.05),
          lon.50 = quantile(decimalLongitude, 0.5),
          lon.95 = quantile(decimalLongitude, 0.95),
          maxlon = max(decimalLongitude),
          mintemp = min(bio1),
          temp.05 = quantile(bio1, 0.05),
          temp.50 = quantile(bio1, 0.5),
          temp.95 = quantile(bio1, 0.95),
          maxtemp = max(bio1),
          trangemin = min(bio7),
          trange.05 = quantile(bio7, 0.05),
          trange.50 = quantile(bio7, 0.5),
          trange.95 = quantile(bio7, 0.95),
          trangemax = max(bio7),
          minp = min(bio12),
          p.05 = quantile(bio12, 0.05),
          p.50 = quantile(bio12, 0.5),
          p.95 = quantile(bio12, 0.95),
          maxp = max(bio12),
          seasonpmin = min(bio15),
          seasonp.05 = quantile(bio15, 0.05),
          seasonp.50 = quantile(bio15, 0.5),
          seasonp.95 = quantile(bio15, 0.95),
          seasonpmax = max(bio15))
write.csv(data_spp, "./output/resumospp.csv")
####falta chequear
