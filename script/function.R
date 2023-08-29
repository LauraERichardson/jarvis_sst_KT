compute.the = function (x, y, z = "week") {
  
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
  
  df <- as.data.frame(list(date = x, estimate = na.interpolation(y)))
  
  df1 <- df %>%
    group_by(grp = eval(parse(text = paste0(z, "(date)")))) %>% 
    mutate(climatology = mean(estimate)) %>% 
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
  
  df1$DHW = compute.dhw(df1$date, df1$TSA)
  colnames(df1)[2] <- "SST"
  df2 <- dplyr::select(df1, date, SST, climatology, SSTA, TSA, 
                       DHW)
}

plot.the = function (df, start = NULL, end = NULL, destfile = NULL, width = 8, height = 4) {
  
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
  
  df1 <- na.omit(df) %>% 
    group_by(week = week(date)) %>%
    mutate(climatology = mean(SST)) %>% 
    ungroup() %>%
    filter(date >= ifelse(is.null(start), first(date), start) & date <= ifelse(is.null(start), last(date), end))
  
  sst.offset <- max(df1$SST)/max(df1$DHW)
  sec.axis.offset <- max(df1$DHW)/max(df1$SST)
  
  p <- ggplot(df1) + 
    geom_line(aes(x = date, y = SST), color = "navyblue") + 
    geom_point(aes(x = date, y = climatology), color = "black", size = 0.1) + 
    geom_hline(yintercept = max(df1$climatology) + 1, color = "navyblue", lty = 3) + 
    geom_hline(yintercept = max(df1$climatology), color = "navyblue", lty = 2, size = 0.25) +
    geom_line(aes(x = date, y = DHW * sst.offset), color = "darkred") +
    geom_hline(yintercept = 4 * sst.offset, color = "darkred", lty = 2, size = 0.25) + 
    geom_hline(yintercept = 8 * sst.offset, color = "darkred", lty = 3) +
    xlab(expression("Date")) + 
    ylab(expression(SST ~ (degree ~ C))) +
    scale_y_continuous(sec.axis = sec_axis(~. * sec.axis.offset, name = expression(DHW ~ (degree ~ C ~ "-" ~ weeks)))) +
    theme_classic() +
    theme(axis.ticks.y.right = element_line(color = "darkred"),
          axis.text.y.right = element_text(color = "darkred"),
          axis.title.y.right = element_text(color = "darkred")) +
    theme(axis.ticks.y.left = element_line(color = "navyblue"),
          axis.text.y.left = element_text(color = "navyblue"),
          axis.title.y.left = element_text(color = "navyblue")) +
    theme(axis.line.y.right = element_line(color = "darkred"),
          axis.line.y.left = element_line(color = "navyblue"))
  
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
