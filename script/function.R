compute.snap = function (x, y) {
  
  # x = sst_i$date
  # y = sst_i$sst
  
  if (!require(tidyverse)) {
    install.packages("tidyverse")
    library(tidyverse)
  }
  if (!require(imputeTS)) {
    install.packages("imputeTS")
    library(imputeTS)
  }
  if (!require(lubridate)) {
    install.packages("lubridate")
    library(lubridate)
  }
  if (!require(progress)) {
    install.packages("progress")
    library(progress)
  }
  
  df <- as.data.frame(list(date = x, estimate = na_interpolation(y)))
  
  # mean and sd from three warmest months
  warmest_months <- df %>%
    mutate(date = as.Date(date), month = format(date, "%m")) %>% 
    # muatte(month = month.name[as.numeric(format(date, "%m"))]) %>% 
    group_by(month) %>%
    summarise(climatology = mean(estimate), sd = sd(estimate)) %>%
    arrange(desc(climatology)) %>%
    top_n(3, climatology) %>%
    print()
  
  summer_months = warmest_months$month
  summer_months <- summer_months[order(as.integer(summer_months))]
  
  # mean and sd from three coldest months
  coldest_months <- df %>%
    mutate(date = as.Date(date), month = format(date, "%m")) %>% 
    # muatte(month = month.name[as.numeric(format(date, "%m"))]) %>% 
    group_by(month) %>%
    summarise(climatology = mean(estimate), sd = sd(estimate)) %>%
    arrange(desc(climatology)) %>%
    top_n(-3, climatology) %>%
    print()
  
  winter_months = coldest_months$month
  winter_months <- winter_months[order(as.integer(winter_months))]
  
  # mean and sd across three warmest and coldest months (i.e., summer and winter months)
  warmest_months = df %>%
    mutate(date = as.Date(date), month = format(date, "%m")) %>%
    group_by(month) %>%
    summarise(climatology = mean(estimate), sd = sd(estimate)) %>%
    arrange(desc(climatology)) %>%
    top_n(3, climatology) %>%
    ungroup() %>%
    summarise(climatology = mean(climatology), sd = mean(sd)) %>%
    print()
  
  coldest_months <- df %>%
    mutate(date = as.Date(date), month = format(date, "%m")) %>%
    group_by(month) %>%
    summarise(climatology = mean(estimate), sd = sd(estimate)) %>%
    arrange(desc(climatology)) %>%
    top_n(-3, climatology) %>%
    ungroup() %>%
    summarise(climatology = mean(climatology), sd = mean(sd)) %>%
    print()
  
  # #1 Calculate Hot Snaps (temperature exceeds baseline)
  # Baseline: One standard deviation above the summer mean
  df$summer_mean = warmest_months$climatology
  df$summer_sd = warmest_months$sd
  df$hot_snap = df$estimate - (df$summer_mean + df$summer_sd)
  df$hot_snap = ifelse(df$hot_snap >= 0, df$hot_snap, 0) # Ensure non-negative values
  
  # #2 Calculate Cold Snaps (temperature drops below baseline)
  # Baseline: Winter mean less one winter standard deviation
  df$winter_mean = coldest_months$climatology
  df$winter_sd = coldest_months$sd
  df$cold_snap = df$estimate - (df$winter_mean - df$winter_sd)
  df$cold_snap = ifelse(df$cold_snap <= 0, df$cold_snap, 0)  # Ensure negative or zero values
  
  # Convert 'date' column to Date type
  df$date <- as.Date(df$date)
  
  ########################################
  #### Calculate accumulated hot snap ####
  ########################################
  
  # Initialize an empty vector to store accumulation sums
  accumulation_sums <- numeric(nrow(df))
  
  pb <- progress_bar$new(total = nrow(df))
  
  print("Calculating Hot Snap through period of accumulation...")
  
  # Iterate through the dates
  for (i in 1:nrow(df)) {
    
    pb$tick()
    
    # i = sample(1:10000, 1)
    
    date_i <- df$date[i]
    # print(date_i)
    
    # Find the beginning of most recent summer
    most_recent_summer <- as.Date(paste(year(date_i), summer_months[1], "01", sep = "-"))
    
    # If date_i is before summer, subtract one year for the most recent summer
    if (date_i < most_recent_summer)  most_recent_summer <- most_recent_summer - years(1)
    
    most_recent_summer
    
    # Check if the most recent summer is not missing ("-Inf")
    if (!is.infinite(most_recent_summer)) {
      
      # Calculate the start date of the accumulation period
      # hotsnap = three months before the most recent summer
      start_date <- most_recent_summer - months(3)
      
      # Filter the dataframe for dates within the accumulation period
      accumulation_period <- df %>%
        filter(date >= start_date & date <= date_i)
      
      # Calculate the sum of 'HSNAP' for the accumulation period and store it in the vector
      accumulation_sums[i] <- sum(accumulation_period$hot_snap)
      
    } else {
      
      # If most_recent_summer is "-Inf," set the accumulation sum to NA
      accumulation_sums[i] <- NA
      
    }
  }
  
  pb$terminate()
  
  df$accumulated_hot_snap <- accumulation_sums
  
  #########################################
  #### Calculate accumulated cold snap ####
  #########################################
  
  # Initialize an empty vector to store accumulation sums
  accumulation_sums <- numeric(nrow(df))
  
  pb <- progress_bar$new(total = nrow(df))
  
  print("Calculating Cold Snap through period of accumulation...")
  
  # Iterate through the dates
  for (i in 1:nrow(df)) {
    
    pb$tick()
    
    # i = sample(1:10000, 1)
    
    date_i <- df$date[i]
    # print(date_i)
    
    # Find the beginning of most recent summer
    most_recent_summer <- as.Date(paste(year(date_i), summer_months[1], "01", sep = "-"))
    
    # If date_i is before summer, subtract one year for the most recent summer
    if (date_i < most_recent_summer)  most_recent_summer <- most_recent_summer - years(1)
    
    most_recent_summer
    
    # Check if the most recent summer is not missing ("-Inf")
    if (!is.infinite(most_recent_summer)) {
      
      # Calculate the start date of the accumulation period
      # coldsnap = nine months before the most recent summer
      start_date <- most_recent_summer - months(9)
      
      # Filter the dataframe for dates within the accumulation period
      accumulation_period <- df %>%
        filter(date >= start_date & date <= date_i)
      
      # Calculate the sum of 'HSNAP' for the accumulation period and store it in the vector
      accumulation_sums[i] <- sum(accumulation_period$cold_snap)
      
    } else {
      
      # If most_recent_summer is "-Inf," set the accumulation sum to NA
      accumulation_sums[i] <- NA
      
    }
  }
  
  pb$terminate()
  
  df$accumulated_cold_snap <- accumulation_sums
  
  colnames(df)[2] <- "sst"
  
  # Convert accumulation sum to per week
  df <- df %>%
    mutate(unique_week_id = as.numeric(format(date, "%U")) + (year(date) - min(year(date))) * 52) %>% 
    group_by(unique_week_id) %>% 
    mutate(accumulated_hot_snap_week = mean(accumulated_hot_snap, na.rm = T),
           accumulated_cold_snap_week = mean(accumulated_cold_snap, na.rm = T),) %>% 
    ungroup()
  
  df = df %>% 
    dplyr::select(date, sst, summer_mean, winter_mean, summer_sd, winter_sd, 
                  hot_snap, cold_snap, 
                  accumulated_hot_snap, accumulated_cold_snap,
                  accumulated_hot_snap_week, accumulated_cold_snap_week)
  
  sv = readr::read_csv("data/2010-2018 Jarvis site info for KiseiTanaka.csv")
  sv$date = mdy(sv$DATE_)
  sv = sv %>% dplyr::select(date, SITEVISITID, SITE, ISLAND)
  
  df = full_join(sv, df)
  
  write_csv(df, file = "output/jarvis_hot_cold_snaps_ts.csv")
  
}

plot.snap = function (df, start = NULL, end = NULL, destfile = NULL, width = 8, height = 4, var = "snap") {
  
  df = snap
  start = NULL
  end = NULL
  
  if (!require(tidyverse)) {
    install.packages("tidyverse")
    library(tidyverse)
  }
  if (!require(lubridate)) {
    install.packages("lubridate")
    library(lubridate)
  }
  if (!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (!require(ggthemes)) {
    install.packages("ggthemes")
    library(ggthemes)
  }
  
  df_i <- df %>% 
    # na.omit() %>% 
    group_by(week = week(date)) %>%
    mutate(climatology = mean(SST),
           sd = sd(SST)) %>% 
    # filter(date >= ifelse(is.null(start), first(date), start) & 
    #          date <= ifelse(is.null(start), last(date), end)) %>%
    ungroup() 
  
  if (var == "snap") {
    
    sst.offset <- max(df_i$SST)/max(df_i$HSNAP)
    sec.axis.offset <- max(df_i$HSNAP)/max(df_i$SST)
    
    df_i$var = df$HSNAP
    
    xlab_name = expression(Hot_Snap ~ (degree ~ C ~ "-" ~ weeks))
    
  }
  
  if (var == "snapsum") {
    
    sst.offset <- max(df_i$SST)/max(df_i$HSNAP_accumulation)
    sec.axis.offset <- max(df_i$HSNAP_accumulation_per_week)/max(df_i$SST)
    
    df_i$var = df$HSNAP_accumulation_per_week
    
    xlab_name = expression(Hot_Snap_accumulation ~ (degree ~ C ~ "-" ~ weeks))
    
  }
  
  p <- ggplot(df_i) + 
    geom_line(aes(x = date, y = SST), color = "blue") + 
    geom_point(aes(x = date, y = climatology), color = "black", size = 0.5) + 
    geom_hline(yintercept = df$climatology + df$sd, color = "blue", lty = 3) + 
    geom_hline(yintercept = df$climatology, color = "blue", lty = 2) +
    geom_line(aes(x = date, y = var * sst.offset), color = "red") +
    xlab(expression("Date")) + 
    ylab(expression(SST ~ (degree ~ C))) +
    # scale_y_continuous(sec.axis = sec_axis(~. * sec.axis.offset, name = expression(Hot_Snap ~ (degree ~ C ~ "-" ~ weeks)))) +
    scale_y_continuous(sec.axis = sec_axis(~., name = xlab_name)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", "") +
    theme_classic() +
    theme(axis.ticks.y.right = element_line(color = "red"),
          axis.text.y.right = element_text(color = "red"),
          axis.title.y.right = element_text(color = "red")) +
    theme(axis.ticks.y.left = element_line(color = "blue"),
          axis.text.y.left = element_text(color = "blue"),
          axis.title.y.left = element_text(color = "blue")) +
    theme(axis.line.y.right = element_line(color = "red"),
          axis.line.y.left = element_line(color = "blue"))
  
  print.plot <- function(destfile, width, height) {
    pdf(destfile, width, height)
    print(p)
    graphics.off()
  }
  
  invisible(if (!is.null(destfile)) {
    print.plot(destfile, width, height)
  })
  
  print(p)
  
  if (var == "snap") ggsave(last_plot(), filename = "output/jarvis_snap_ts.png", height = 8, width = 16)
  if (var == "snapsum") ggsave(last_plot(), filename = "output/jarvis_snapsum_ts.png", height = 8, width = 16)
  
}
