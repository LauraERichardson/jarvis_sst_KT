library(dplyr)
library(THE)
library(raster)

rm(list = ls())

select = dplyr::select

source("script/function.R")

sst_source = c("data/Jarvis_Sea_Surface_Temperature_CRW_Daily_1985-04-01_2023-07-31.nc",
               "data/Jarvis_Sea_Surface_Temperature_jplMUR_Daily_2002-06-01_2023-07-31.nc")[1]

# Load the SST nc file and process it
sst <- stack(sst_source) %>% 
  rasterToPoints() %>% 
  t() %>%
  as.data.frame() %>%
  mutate(
    date = row.names(.),
    year = as.numeric(substring(date, 2, 5)),
    month = substring(date, 7, 8),
    day = substring(date, 10, 11),
    sst = rowMeans(select(., starts_with("V")), na.rm = T)
  ) 

sst_i = sst %>% 
  slice(-c(1, 2)) %>% 
  filter(year %in% c(2014:2017)) %>%
  select(year, month, day, starts_with("V")) %>% 
  mutate(V = rowMeans(select(., starts_with("V")), na.rm = TRUE),
         date = as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d"),
         sst = V) %>% 
  select(year, month, day, date, sst)

test = compute.dhw(x = sst_i$date, sst_i$sst, z = "week"); plot.dhw(test)
test = compute.dhw(x = sst_i$date, sst_i$sst, z = "month"); plot.dhw(test)
test = compute.snap(x = sst_i$date, sst_i$sst, z = "week"); plot.snap(test)
test = compute.snap(x = sst_i$date, sst_i$sst, z = "month"); plot.snap(test)
