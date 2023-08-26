library(raster)
library(dplyr)
library(ggplot2)
library(colorRamps)
library(patchwork)

rm(list = ls())

# https://upwell.pfeg.noaa.gov/erddap/info/pibhmc_bathy_5m_jarvis/index.html
bathy = raster("data/pibhmc_bathy_5m_jarvis_ea1e_3b5a_8f11.nc")

# two gridded SST data

# CRW: CoralTemp Version 1.0, a daily global 5-km sea surface temperature dataset combined from: (1.) NOAA/NESDIS operational near-real-time daily global 5-km geostationary-polar-orbiting (geo-polar) blended night-only SST analysis, (2.) NOAA/NESDIS 2002-2016 reprocessed daily global 5-km geo-polar blended night-only SST analysis, and (3.) United Kingdom Met Office 1985-2002 daily global 5-km night-only SST reanalysis of Operational SST and Sea Ice Analysis (OSTIA).

# jplMUR: a merged, multi-sensor L4 Foundation Sea Surface Temperature (SST) analysis product from Jet Propulsion Laboratory (JPL). This daily, global, Multi-scale, Ultra-high Resolution (MUR) Sea Surface Temperature (SST) 1-km data set, Version 4.1, is produced at JPL under the NASA MEaSUREs program. 

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

# 1. the long-term average summer (and winter) temps across as long a period as possible.
monthly_mean_sd = sst %>%
  select(year, month, day, sst) %>%
  slice(-c(1, 2)) %>% 
  filter(year %in% c(1985:2018)) %>%
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
  filter(year %in% c(1985:2018)) %>%
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

p1 / p2
ggsave(last_plot(), filename = "output/jarvis_mean_sd.png", width = 10, height = 5, units = "in")

# Hot Snaps occurred when the temperature exceeded the baseline, defined for Hot Snaps as one standard deviation above the summer mean. 
hot_snaps <- monthly_mean_sd %>%
  top_n(3, mean) %>% 
  mutate(hot_snaps = mean + sd)

month = unique(warmest_months$month)

snap_record = NULL

for (m in 1:length(month)) {
  
  # m = 1
  
  hot_snap = hot_snaps %>% filter(month == month[m])
  threshold <- hot_snap$hot_snaps
  
  df_i = sst %>% 
    slice(-c(1, 2)) %>% 
    filter(year %in% c(1985:2018)) %>%
    filter(month %in% hot_snap$month) %>% 
    select(year, month, day, starts_with("V"))
  
  df_i <- df_i %>%
    mutate(V = rowMeans(select(., starts_with("V")), na.rm = TRUE)) %>% 
    select(year, month, day, V)
  
  df_i = df_i %>% 
    mutate(snap = ifelse(V > threshold, 1, 0))
  
  snap_record = rbind(snap_record, df_i)
  
}

snap_record <- snap_record %>%
  rename(sst = !!colnames(.)[4]) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")))

rownames(snap_record) = NULL

set.seed(2023)  # for reproducibility

snap_record <- snap_record %>%
  mutate(survey_date = ifelse(runif(n()) < 0.5, 1, 0))

snap_record %>%
  filter(year %in% c(2010:2018)) %>%
  ggplot(aes(x = date, y = sst, color = factor(snap))) +
  geom_point() +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), "")

df <- as.data.frame(list(date = x, estimate = na.interpolation(y)))
df1 <- df %>% group_by(grp = eval(parse(text = paste0(z, 
                                                      "(date)")))) %>% mutate(climatology = mean(estimate)) %>% 
  ungroup()
df1$SSTA = df1$estimate - df1$climatology
df1$TSA = df1$estimate - max(df1$climatology)
df1$TSA = ifelse(df1$TSA > 0, df1$TSA, 0)
compute.dhw <- function(x, y) {
  dhw.ts <- as.data.frame(x)
  dhw.ts$event_id <- seq(1, length(dhw.ts$x))
  colnames(dhw.ts)[1] <- c("date")
  dhw.ts$week <- week(dhw.ts$date)
  dhw.ts$year <- year(dhw.ts$date)
  dhw.ts$TSA <- y
  dhw.ts$tsa_dhw <- ifelse(dhw.ts$TSA >= 1, dhw.ts$TSA, 
                           0)
  nz <- ifelse(z == "month", 89, ifelse(z == "week", 11, 
                                        "NA"))
  dhw <- function(event_id) {
    sum(dhw.ts$tsa_dhw[dhw.ts$event_id >= event_id - 
                         nz & dhw.ts$event_id <= event_id])
  }
  for (i in unique(dhw.ts$event_id)) {
    x <- dhw(i)
    y <- c(event_id = i, DHW = x)
    assign(paste0("DHW_", i), as.data.frame(t(y)))
  }
  dhw_list <- mget(ls(pattern = "DHW_"))
  dhw.climatology <- bind_rows(dhw_list)
  dhw.summary <- merge(dhw.ts, dhw.climatology, by = "event_id")
  (dhw.summary$DHW)
}
df1$DHW = compute.dhw(df1$date, df1$TSA)
colnames(df1)[2] <- "SST"
df2 <- dplyr::select(df1, date, SST, climatology, SSTA, TSA, 
                     DHW)

snap_record %>%
  arrange(date) %>%
  mutate(DHW = rollsum(x = HS/7, 84, align = "right", fill = NA)/7) #rolling sum for 84 days of hotspot value / 7 (to get from days to weeks)

ggsave(last_plot(), filename = "output/jarvis_hotsnap_ts.png", width = 7, height = 5, units = "in")

