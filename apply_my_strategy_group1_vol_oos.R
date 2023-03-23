setwd("/Users/yuriisemenov/Desktop/Project/asset_g_1/data")

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB.R")

library(xts)
library(chron)
library(TTR)
library(tseries)
library(knitr) 
library(kableExtra) 
library(quantmod) 
library(roll)
library(caTools)

Sys.setlocale("LC_TIME", "English")

mySR <- function(x, scale) {
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)
} 

myCalmarRatio <- function(x, scale) {
  scale * mean(coredata(x), na.rm = TRUE) / 
    maxdrawdown(cumsum(x))$maxdrawdown
  
} 

Sys.setenv(TZ = 'America/New_York')

for (selected_quarter in c("2020_Q1", "2020_Q3", "2020_Q4", 
                           "2021_Q2", "2021_Q4", 
                           "2022_Q1", "2022_Q2","2020_Q2", "2021_Q1", "2021_Q3", 
                           "2022_Q3", "2022_Q4")) {
  message(selected_quarter)
  filename_ <- paste0("data1_", selected_quarter, ".RData")
  load(filename_)
  data.group1 <- get(paste0("data1_", selected_quarter))
  times_ <- substr(index(data.group1), 12, 19)
  
  data.group1["T09:31/T09:45",] <- NA 
  data.group1["T15:46/T16:00",] <-NA
  
  roll_sd <- 100
  multiplier <- 3.5
  
  data.group1$SMA_SP <- roll_mean(na.locf(data.group1$SP,na.rm = FALSE),
                                  width = 120,
                                  na_restore = TRUE)
  
  data.group1$SMA_NQ <- roll_mean(na.locf(data.group1$NQ,na.rm = FALSE),
                                  width = 100,
                                  na_restore = TRUE)
  
  # data.group1$SMA_SP[is.na(data.group1$SMA_SP)] <- NA
  
  data.group1$rollsd_SP <- runsd(data.group1$SP, 
                              roll_sd, 
                              endrule = "NA",
                              align = "right")
  data.group1$rollsd_SP[is.na(data.group1$SP)] <- NA
  
  data.group1$rollsd_NQ <- runsd(data.group1$NQ, 
                              roll_sd, 
                              endrule = "NA",
                              align = "right")
  data.group1$rollsd_NQ[is.na(data.group1$NQ)] <- NA
  
  # position 
  data.group1$positionSP.mom <- 
    positionVB(signal = data.group1$SP,
               upper = data.group1$SMA_SP + multiplier * data.group1$rollsd_SP,
               lower = data.group1$SMA_SP - multiplier * data.group1$rollsd_SP, 
               times_data = times_, # column with times 
               time_lower = "09:45:00", # when start trading
               time_upper = "15:46:00", # when exit all positions
               strategy = "mom")
  
  data.group1$positionNQ.mom <- 
    positionVB(signal = data.group1$NQ,
               upper = data.group1$SMA_NQ + multiplier * data.group1$rollsd_NQ,
               lower = data.group1$SMA_NQ - multiplier * data.group1$rollsd_NQ, 
               times_data = times_, # column with times 
               time_lower = "09:45:00", # when start trading
               time_upper = "15:46:00", # when exit all positions
               strategy = "mom")
  
  data.group1$positioSP.mom[times(times_) <= times("10:00:00") | 
                              times(times_) > times("15:40:00")] <- 0
  
  data.group1$positionNQ.mom[times(times_) <= times("10:00:00") | 
                               times(times_) > times("15:40:00")] <- 0
  
  # calculating gross pnl
  data.group1$pnl_grossSP.mom <- data.group1$positionSP.mom * diff.xts(data.group1$SP) * 50
  data.group1$pnl_grossNQ.mom <- data.group1$positionNQ.mom * diff.xts(data.group1$NQ) * 20
  
  # number of transactions
  data.group1$ntransSP.mom <- abs(diff.xts(data.group1$positionSP.mom))
  data.group1$ntransSP.mom[1] <- 0
  
  data.group1$ntransNQ.mom <- abs(diff.xts(data.group1$positionNQ.mom))
  data.group1$ntransNQ.mom[1] <- 0
  
  
  #net pnl
  data.group1$pnl_netSP.mom <- data.group1$pnl_grossSP.mom  -
    data.group1$ntransSP.mom * 10
  data.group1$pnl_netNQ.mom <- data.group1$pnl_grossNQ.mom  -
    data.group1$ntransNQ.mom * 10
  
  # total
  data.group1$pnl_gross.mom <- data.group1$pnl_grossNQ.mom + data.group1$pnl_grossSP.mom
  data.group1$pnl_net.mom <- data.group1$pnl_netNQ.mom + data.group1$pnl_netSP.mom
  
  # aggregate pnls and number of transactions to daily
  my.endpoints <- endpoints(data.group1, "days")
  
  data.group1.daily <- period.apply(data.group1[,c(grep("pnl", names(data.group1)),
                                                   grep("ntrans", names(data.group1)))],
                                    INDEX = my.endpoints, 
                                    FUN = function(x) colSums(x, na.rm = TRUE))
  
  # summarize the strategy for this quarter
  
  # SR
  grossSR = mySR(x = data.group1.daily$pnl_gross.mom, scale = 252)
  netSR = mySR(x = data.group1.daily$pnl_net.mom, scale = 252)
  # CR
  grossCR = myCalmarRatio(x = data.group1.daily$pnl_gross.mom, scale = 252)
  netCR = myCalmarRatio(x = data.group1.daily$pnl_net.mom, scale = 252)
  
  # average number of transactions
  av.daily.ntrades = mean(data.group1.daily$ntransSP.mom + 
                            data.group1.daily$ntransNQ.mom, na.rm = TRUE)
  # PnL
  grossPnL = sum(data.group1.daily$pnl_gross.mom)
  netPnL = sum(data.group1.daily$pnl_net.mom)
  # stat
  stat = netCR * max(0, log(abs(netPnL/1000)))
  
  # collecting all statistics for a particular quarter
  
  quarter_stats <- data.frame(quarter = selected_quarter,
                              rollsd = roll_sd,
                              multiplier = multiplier,
                              assets.group = 1,
                              grossSR,
                              netSR,
                              grossCR,
                              netCR,
                              av.daily.ntrades,
                              grossPnL,
                              netPnL,
                              stat,
                              stringsAsFactors = FALSE)
  # collect summaries for all quarters
  if(!exists("quarter_stats.all.group1")) quarter_stats.all.group1 <- quarter_stats else
    quarter_stats.all.group1 <- rbind(quarter_stats.all.group1, quarter_stats)
  
  
  # create a plot of gros and net pnl and save it to png file
  png(filename = paste0("pnl_group1_", selected_quarter, ".png"),
      width = 1000, height = 600)
  
  print( # when plotting in a loop you have to use print()
    plot(cbind(cumsum(data.group1.daily$pnl_gross.mom),
               cumsum(data.group1.daily$pnl_net.mom)),
         multi.panel = FALSE,
         main = paste0("Gross and net PnL for asset group 1 \n quarter ", selected_quarter), 
         col = c("#377EB8", "#E41A1C"),
         major.ticks = "weeks", 
         grid.ticks.on = "weeks",
         grid.ticks.lty = 3,
         legend.loc = "topleft",
         cex = 1)
  )
  # closing the png device (and file)
  dev.off()
  
  #remove all unneeded objects for group 1
  rm(data.group1, my.endpoints, grossSR, netSR, av.daily.ntrades,
  grossPnL, netPnL, stat, quarter_stats, data.group1.daily)
  
  gc()
  
}

calculus_1 <- aggregate(quarter_stats.all.group1$netPnL, list(quarter_stats.all.group1$rollsd), sum)


write.csv(quarter_stats.all.group1, 
          "quarter_stats.all.group1.csv",
          row.names = FALSE)