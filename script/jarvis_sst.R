library(raster)
library(dplyr)
library(ggplot2)
library(colorRamps)
library(patchwork)

rm(list = ls())

# https://upwell.pfeg.noaa.gov/erddap/info/pibhmc_bathy_5m_jarvis/index.html
bathy = raster("data/pibhmc_bathy_5m_jarvis_ea1e_3b5a_8f11.nc")

shp

# two gridded SST data

# CRW: CoralTemp Version 1.0, a daily global 5-km sea surface temperature dataset combined from: (1.) NOAA/NESDIS operational near-real-time daily global 5-km geostationary-polar-orbiting (geo-polar) blended night-only SST analysis, (2.) NOAA/NESDIS 2002-2016 reprocessed daily global 5-km geo-polar blended night-only SST analysis, and (3.) United Kingdom Met Office 1985-2002 daily global 5-km night-only SST reanalysis of Operational SST and Sea Ice Analysis (OSTIA).

# jplMUR: a merged, multi-sensor L4 Foundation Sea Surface Temperature (SST) analysis product from Jet Propulsion Laboratory (JPL). This daily, global, Multi-scale, Ultra-high Resolution (MUR) Sea Surface Temperature (SST) 1-km data set, Version 4.1, is produced at JPL under the NASA MEaSUREs program. 

sst_source = c("data/Jarvis_Sea_Surface_Temperature_CRW_Daily_1985-04-01_2023-07-31.nc",
               "data/Jarvis_Sea_Surface_Temperature_jplMUR_Daily_2002-06-01_2023-07-31.nc")[2]

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

# 1. the long-term average summer (and winter) temps across as long a period as possible.
monthly_mean_sd = sst %>%
  select(year, month, day, sst) %>%
  slice(-c(1, 2)) %>% 
  # filter(year %in% c(1985:2004)) %>% 
  group_by(year, month) %>% 
  summarise(sst = mean(sst)) %>%
  group_by(month) %>%
  summarise(
    mean = mean(sst),
    sd = sd(sst))

# 2. pixel-by-pixel summer mean temperature by averaging all SST values from the three climatologically warmest months
warmest_months <- monthly_mean_sd %>%
  top_n(3, mean)

grid = sst[1:2, ] %>% select(starts_with("V"))

df = sst %>% 
  slice(-c(1, 2)) %>% 
  select(year, month, day, starts_with("V")) %>%
  filter(month %in% warmest_months$month) %>% 
  select(starts_with("V"))

df = rbind(grid, df) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(
   sst = rowMeans(select(., starts_with("X")), na.rm = T)
  ) %>% 
  select(x, y, sst)

p1 = df %>% 
  ggplot(aes(x, y, fill = sst)) + 
  geom_raster() + 
  scale_fill_gradientn("Summer Temperature Mean (deg C)", colours = matlab.like(100))
  
# 3. summer standard deviation for each pixel to define "Hot Snaps"
df = sst %>% 
  slice(-c(1, 2)) %>% 
  select(year, month, day, starts_with("V")) %>%
  filter(month %in% warmest_months$month) %>% 
  select(starts_with("V"))

df = rbind(grid, df) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(sd = apply(select(., -x, -y), 1, sd)) %>% 
  select(x, y, sd)

p2 = df %>% 
  ggplot(aes(x, y, fill = sd)) + 
  geom_raster() + 
  scale_fill_gradientn("Summer Temperature SD (deg C)", colours = matlab.like(100))

p1 + p2

