# Testing codes ----------------------------------------------------------------------------------

# Created on 22nd October 2022

# Last updated on 25 October 2022

# This is a repo for testing simple R codes

# Looping through raster files -------------------------------------------------------------------

library(terra)            # Version 1.6.41
library(tidyverse)        # Version 1.3.2
library(sf)               # Version 1.0.9
library(WDI)              # Version 2.7.8
library(Data4Ecologists)  # Version 0.0.0.9000
library(lubridate)        # Version 1.9.0
library(doParallel)       # Version 1.0.17
library(parallel)         # Version 4.2.2
library(iterators)        # Version 1.0.14
library(foreach)          # Version 1.5.2
library(tmap)             # Version 3.3.3

pks <- c("terra", "tidyverse", "sf", "WDI", "data4Ecologists", "lubridate", 
         "doParallel", "parallel", "iterators", "foreach", "tmap"
         )

foreach(i = pks, .combine = c) %do% {
  paste(i, packageVersion(i))
}

files <- list.files(system.file("external", package = "sdm"),
                    pattern = ".asc$",
                    full.names = TRUE
                    )

b <- list()

# Simple form of for loop

for (i in 1:length(files)) {
  b[[i]] <- terra::rast(files[[i]])
  plot(b[[i]])
}

# END --------------------------------------------------------------------------------------------

nc <- system.file("gpkg/nc.gpkg", package = "sf") |> 
  read_sf()

nc.32119 <- st_transform(nc, 'EPSG:32119')
nc.32119 |>
  select(BIR74) |>
  plot(graticule = TRUE, axes = TRUE)

b <- raster::stack(list.files(
  system.file("external",
              package = "sdm"),
  pattern = ".asc$",
  full.names = T
))

crs(b) <- 23030

data(World)

mapview::mapview(raster::projectRaster(b, crs = crs(World)))

d <- terra::rast(list.files(system.file("external",
                                          package = "sdm"),
                              pattern = ".asc$",
                              full.names = T))

raster_files <- list.files(system.file("external", package = "sdm"),
                           pattern = ".asc$",
                           full.names = T)

ras <- list()
for (i in 1:length(raster_files)) {
  ras[[i]] <- raster::raster(raster_files[[i]])
  ras[[i]] <-
    raster::crop(ras[[i]], raster::extent(3.6e+05, 3.8e+05,
                                          4.06e+6, 4.1e+6))
  ras[[i]] <- raster::plot(ras[[i]])
}

f <- terra::aggregate(terra::as.polygons(d[[1]]))
plot(d[[1]])
plot(f, lwd = 5, border = "purple", add = T)

# World Bank Data --------------------------------------------------------------------------------

example <-
  WDI(indicator = c("EN.ATM.CO2E.PC", "NY.GDP.PCAP.CD"),
      extra = T)
example |> select(year, NY.GDP.PCAP.CD) |>
  ggplot(aes(factor(year),
             log(NY.GDP.PCAP.CD))) +
  geom_boxplot()

example |> select(year, EN.ATM.CO2E.PC) |> drop_na() |>
  ggplot(aes(factor(year),
             log(EN.ATM.CO2E.PC))) +
  geom_boxplot()

example |> select(year, EN.ATM.CO2E.PC) |> nrow()
example |> select(year, EN.ATM.CO2E.PC) |> drop_na() |> nrow()

WDI::WDI_data$country

dat = WDI(
  indicator = 'NY.GDP.PCAP.KD',
  country = c('KEN', 'NGA'),
  start = 1960,
  end = 2022
)

dat = WDI(
  indicator = 'NY.GDP.PCAP.CD',
  country = c('KEN', 'NGA'),
  start = 1960,
  end = 2022
)

dat |> ggplot(aes(year, NY.GDP.PCAP.CD, col = country)) +
  geom_line() +
  theme_minimal() +
  theme(legend.background = element_rect(
    fill = "purple",
    size = 0.5,
    linetype = "solid",
    color = "lightblue"
  ))

ggplot(dat, aes(year, NY.GDP.PCAP.KD, color = country)) +
  geom_line() +
  xlab('Year') +
  ylab('GDP per capita')

# Access to electricity % of population ----------------------------------------------------------

dat <-
  WDI(indicator = "EG.ELC.ACCS.ZS", country = c("KEN", "UGA", "TZA"))

dat |> drop_na() |>  ggplot(aes(year, EG.ELC.ACCS.ZS, col = country)) +
  geom_line() +
  theme(legend.background = element_rect(
    fill = "purple",
    size = 0.5,
    colour = "red"
  ))


dat <-
  WDI(indicator = "EN.MAM.THRD.NO", country = c("MDG", "ECU", "MYS"))

# Threatened mammal species ----------------------------------------------------------------------

dat |> drop_na() |>  ggplot(aes(year, EN.MAM.THRD.NO, col = country)) +
  geom_line() +
  theme(legend.background = element_rect(
    fill = "purple",
    size = 0.5,
    colour = "red"
  ))


# Data wrangling ---------------------------------------------------------------------------------

df <-
  data.frame(date = as_date(c(
    "2021-01-21", "2021-01-21", NA, "2021-03-01", "2021-01-12"
  )),
  value = rnorm(5))

as_tibble(df) |> mutate(new = if_else(is.na(date), 0, value))

# Add space --------------------------------------------------------------------------------------

df <- tibble(cha = c("Carson Lake,NV", "Eagle Lake,CA"))

df$cha <- gsub("\\,", ", ", df$cha)
df

# ggresidplot ------------------------------------------------------------------------------------

lmfit.R <- lm(Richness ~ ., data = RIKZdat)
summary(lmfit.R)

ggResidpanel::resid_panel(lmfit.R)

# doParallel -------------------------------------------------------------------------------------

cores = 4

registerDoParallel(cores = cores)

times(3) %do% rnorm(1)

foreach(i = 1:3) %do%
  sqrt(i)

foreach(i = 1:4, j = 1:10) %do%
  sqrt(i + j)

m <- matrix(rnorm(9), 3, 3)

foreach(i = 1:nrow(m), .combine = c) %dopar%
  mean(m[, i])

foreach(i = 1:nrow(m), .combine = rbind) %dopar%
  (m[, i] / mean(m[, i]))

a <- matrix(1:16, 4, 4)
b <- t(a)

foreach(b = iter(b, by = "col"), .combine = cbind) %dopar%
  (a %*% b)

d <- data.frame(x = 1:10, y = rnorm(10))

s <- foreach(d = iter(d, by = "row"), .combine = rbind) %dopar% d

identical(s, d)

qsort <- function(x) {
  n <- length(x)
  if (n == 0) {
    x
  } else {
    p <- sample(n, 1)
    smaller <- foreach(y = x[-p], .combine = c) %:% when(y <= x[p]) %do% y
    larger <- foreach(y = x[-p], .combine = c) %:% when(y > x[p]) %do% y
    c(qsort(smaller), x[p], qsort(larger))
  }
}

qsort(iris$Sepal.Width)


# Loading raster layers --------------------------------------------------------------------------

files <- list.files(system.file("external", package = "sdm"), 
                    pattern = ".asc$", full.names = T)

foreach(f = files) %do% {
  raster::plot(raster::raster(f))
}


# Parallel downloading species occurrence data ---------------------------------------------------

spec <- read_csv("D:/spec.csv")
m <- spec |> separate(col = species, 
                      into = c("one", "two", "three", "four", 
                               "five", "six", "seven", "eight"),
                      sep = " ") |> select(one, two) |> unite(col = combi, sep = " ")
m$combi
b <- m$combi
registerDoParallel(cores = 6)
m <- foreach(sp = b) %dopar% {rgbif::occ_search(scientificName = sp)$data}

stopImplicitCluster()