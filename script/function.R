compute.dhw = function (x, y, z = "week") {
  
  # x = sst_i$date
  # y = sst_i$sst
  # z = "month"
  
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
  
  df <- as.data.frame(list(date = x, estimate = na_interpolation(y)))
  
  df_i <- df %>%
    group_by(grp = eval(parse(text = paste0(z, "(date)")))) %>% 
    mutate(climatology = mean(estimate),
           sd = sd(estimate)) %>% 
    ungroup()
  
  df_i$SSTA = df_i$estimate - df_i$climatology
  df_i$TSA = df_i$estimate - max(df_i$climatology)
  df_i$TSA = ifelse(df_i$TSA > 0, df_i$TSA, 0)
  
  compute.dhw.snap <- function(x, y) {
    
    dhw.ts <- as.data.frame(x)
    dhw.ts$event_id <- seq(1, length(dhw.ts$x))
    colnames(dhw.ts)[1] <- c("date")
    dhw.ts$week <- week(dhw.ts$date)
    dhw.ts$year <- year(dhw.ts$date)
    dhw.ts$TSA <- y
    dhw.ts$tsa_dhw <- ifelse(dhw.ts$TSA >= 1, dhw.ts$TSA, 0)
    
    nz <- ifelse(z == "month", 89, ifelse(z == "week", 11, "NA"))
    
    dhw <- function(event_id) {
      
      sum(dhw.ts$tsa_dhw[dhw.ts$event_id >= event_id - nz & dhw.ts$event_id <= event_id])
      
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
  
  df_i$DHW_cml = compute.dhw.snap(df_i$date, df_i$TSA)
  colnames(df_i)[2] <- "SST"
  df2 <- dplyr::select(df_i, date, SST, climatology, SSTA, TSA, DHW_cml)
  write_csv(df2, file = "output/jarvis_dhw_ts.csv")
  
}

compute.snap = function (x, y, z = "week") {
  
  x = sst_i$date
  y = sst_i$sst
  z = "week"
  
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
  
  df <- as.data.frame(list(date = x, estimate = na_interpolation(y)))
  
  df_i <- df %>%
    group_by(grp = eval(parse(text = paste0(z, "(date)")))) %>% 
    mutate(climatology = mean(estimate),
           sd = sd(estimate)) %>% 
    ungroup()
  
  # mean and sd from three warmest months
  warmest_months <- df %>%
    mutate(date = as.Date(date), month = format(date, "%m")) %>%
    group_by(month) %>%
    summarise(climatology = mean(estimate), sd = sd(estimate)) %>%
    arrange(desc(climatology)) %>%
    top_n(3, climatology) %>%
    print()
  
  # mean and sd from three coldest months
  coldest_months <- df %>%
    mutate(date = as.Date(date), month = format(date, "%m")) %>%
    group_by(month) %>%
    summarise(climatology = mean(estimate), sd = sd(estimate)) %>%
    arrange(desc(climatology)) %>%
    top_n(-3, climatology) %>%
    print()
  
  # mean and sd across three warmest months
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
  
  df_i$climatology = warmest_months$climatology
  df_i$sd = warmest_months$sd
  
  df_i$SSTA = df_i$estimate - df_i$climatology
  df_i$HSNAP = df_i$estimate - (df_i$climatology + df_i$sd)
  df_i$HSNAP = ifelse(df_i$HSNAP >= 0, df_i$HSNAP, 0)
  
  # Convert 'date' column to Date type
  df_i$date <- as.Date(df_i$date)
  
  # Initialize an empty vector to store accumulation sums
  accumulation_sums <- numeric(nrow(df_i))
  
  # Iterate through the dates using a for loop
  for (i in 1:nrow(df_i)) {
    
    # i = 1000
    
    date_i <- df_i$date[i]; print(date_i)
    
    # Find the most recent summer
    most_recent_summer <- max(df_i$date[df_i$date < date_i & months(df_i$date) %in% c("June", "July", "August")])
    
    # Check if the most recent summer is not missing ("-Inf")
    if (!is.infinite(most_recent_summer)) {
      
      # Calculate the start date of the accumulation period (three months before the most recent summer)
      start_date <- most_recent_summer - months(3)
      
      # Filter the dataframe for dates within the accumulation period
      accumulation_period <- df_i %>%
        filter(date >= start_date & date <= date_i)
      
      # Calculate the sum of 'HSNAP' for the accumulation period and store it in the vector
      accumulation_sums[i] <- sum(accumulation_period$HSNAP)
      
    } else {
      
      # If most_recent_summer is "-Inf," set the accumulation sum to NA
      accumulation_sums[i] <- NA
      
    }
  }
  
  # Add the accumulation sums to the dataframe
  df_i$HSNAP_accumulation <- accumulation_sums
  
  # Convert accumulation sum to per week
  df_i <- df_i %>%
    mutate(unique_week_id = as.numeric(format(date, "%U")) + (year(date) - min(year(date))) * 52) %>% 
    group_by(unique_week_id) %>% 
    mutate(HSNAP_accumulation_per_week = mean(HSNAP_accumulation, na.rm = T))
  
  colnames(df_i)[2] <- "SST"
  
  df2 <- dplyr::select(df_i, date, SST, climatology, sd, SSTA, HSNAP, HSNAP_accumulation, HSNAP_accumulation_per_week)
  write_csv(df2, file = "output/jarvis_hotsnap_ts.csv")
}

plot.dhw = function (df, start = NULL, end = NULL, destfile = NULL, width = 8, height = 4) {
  
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
  
  df_i <- na.omit(df) %>% 
    group_by(week = week(date)) %>%
    mutate(climatology = mean(SST)) %>% 
    ungroup() %>%
    filter(date >= ifelse(is.null(start), first(date), start) & date <= ifelse(is.null(start), last(date), end))
  
  sst.offset <- max(df_i$SST)/max(df_i$DHW)
  sec.axis.offset <- max(df_i$DHW)/max(df_i$SST)
  
  p <- ggplot(df_i) + 
    geom_line(aes(x = date, y = SST), color = "blue") + 
    geom_point(aes(x = date, y = climatology), color = "black", size = 0.1) + 
    geom_hline(yintercept = max(df_i$climatology) + 1, color = "blue", lty = 3) + 
    geom_hline(yintercept = max(df_i$climatology), color = "blue", lty = 2, size = 0.25) +
    geom_line(aes(x = date, y = DHW * sst.offset), color = "red") +
    geom_hline(yintercept = 4 * sst.offset, color = "red", lty = 2, size = 0.25) + 
    geom_hline(yintercept = 8 * sst.offset, color = "red", lty = 3) +
    xlab(expression("Date")) + 
    ylab(expression(SST ~ (degree ~ C))) +
    scale_y_continuous(sec.axis = sec_axis(~. * sec.axis.offset, name = expression(DHW ~ (degree ~ C ~ "-" ~ weeks)))) +
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
}

plot.snap = function (df, start = NULL, end = NULL, destfile = NULL, width = 8, height = 4) {
  
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
  
  df_i <- na.omit(df) %>% 
    group_by(week = week(date)) %>%
    mutate(climatology = mean(SST),
           sd = sd(SST)) %>% 
    ungroup() %>%
    filter(date >= ifelse(is.null(start), first(date), start) & date <= ifelse(is.null(start), last(date), end))
  
  sst.offset <- max(df_i$SST)/max(df_i$HSNAP_accumulation)
  sec.axis.offset <- max(df_i$HSNAP_accumulation_per_week)/max(df_i$SST)
  
  p <- ggplot(df_i) + 
    geom_line(aes(x = date, y = SST), color = "blue") + 
    geom_point(aes(x = date, y = climatology), color = "black", size = 0.1) + 
    geom_hline(yintercept = df$climatology + df$sd, color = "blue", lty = 3) + 
    geom_hline(yintercept = df$climatology, color = "blue", lty = 2) +
    geom_line(aes(x = date, y = HSNAP_accumulation_per_week * sst.offset), color = "red") +
    xlab(expression("Date")) + 
    ylab(expression(SST ~ (degree ~ C))) +
    # scale_y_continuous(sec.axis = sec_axis(~. * sec.axis.offset, name = expression(Hot_Snap ~ (degree ~ C ~ "-" ~ weeks)))) +
    scale_y_continuous(sec.axis = sec_axis(~., name = expression(Hot_Snap_accumulation ~ (degree ~ C ~ "per" ~ week)))) +
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
}
