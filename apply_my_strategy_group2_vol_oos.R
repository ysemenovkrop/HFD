setwd("/Users/yuriisemenov/Desktop/Project/asset_g_2/data")

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
                           "2022_Q1", "2022_Q2", "2020_Q2", "2021_Q1", "2021_Q3", 
                           "2022_Q3", "2022_Q4")) {
  message(selected_quarter)
  filename_ <- paste0("data2_", selected_quarter, ".RData")
  load(filename_)
  data.group2 <- get(paste0("data2_", selected_quarter))
  times_ <- substr(index(data.group2), 12, 19)
  
  data.group2["T09:31/T09:45",] <- NA 
  data.group2["T15:46/T16:00",] <-NA
  
  # SMA 
  
  data.group2$SMA_AUD <- roll_mean(na.locf(data.group2$AUD,na.rm = FALSE),
                                   width = 100,
                                   na_restore = TRUE)
  
  data.group2$SMA_CAD <- roll_mean(na.locf(data.group2$CAD,na.rm = FALSE),
                                   width = 100,
                                   na_restore = TRUE)
  
  # rollsd
  
  data.group2$rollsd_AUD <- runsd(data.group2$AUD, 
                                  110, 
                                  endrule = "NA",
                                  align = "right")
  data.group2$rollsd_CAD <- runsd(data.group2$CAD, 
                                  110, 
                                  endrule = "NA",
                                  align = "right")
  
  
  data.group2$rollsd_AUD[is.na(data.group2$AUD)] <- NA
  data.group2$rollsd_CAD[is.na(data.group2$CAD)] <- NA
  
  # position 
  data.group2$positionAUD.mom <- 
    positionVB(signal = data.group2$AUD,
               upper = data.group2$SMA_AUD + 3.5 * data.group2$rollsd_AUD,
               lower = data.group2$SMA_CAD - 3.5 * data.group2$rollsd_AUD, 
               times_data = times_, # column with times 
               time_lower = "09:45:00", # when start trading
               time_upper = "15:46:00", # when exit all positions
               strategy = "mom")
  
  data.group2$positionCAD.mom <- 
    positionVB(signal = data.group2$CAD,
               upper = data.group2$SMA_CAD + 3.5 * data.group2$rollsd_CAD,
               lower = data.group2$SMA_CAD - 3.5 * data.group2$rollsd_CAD, 
               times_data = times_, # column with times 
               time_lower = "09:45:00", # when start trading
               time_upper = "15:46:00", # when exit all positions
               strategy = "mom")
  
  data.group2$positioAUD.mom[times(times_) <= times("10:00:00") | 
                               times(times_) > times("15:40:00")] <- 0
  
  data.group2$positionCAD.mom[times(times_) <= times("10:00:00") | 
                                times(times_) > times("15:40:00")] <- 0
  
  # calculating gross pnl
  data.group2$pnl_gross.AUD.mom <- data.group2$positionAUD.mom * diff.xts(data.group2$AUD) * 100000
  data.group2$pnl_gross.CAD.mom <- data.group2$positionCAD.mom * diff.xts(data.group2$CAD) * 100000
  
  # number of transactions
  data.group2$ntrans.AUD.mom <- abs(diff.xts(data.group2$positionAUD.mom))
  data.group2$ntrans.AUD.mom[1] <- 0
  
  data.group2$ntrans.CAD.mom <- abs(diff.xts(data.group2$positionCAD.mom))
  data.group2$ntrans.CAD.mom[1] <- 0
  
  
  #net pnl
  data.group2$pnl_net.AUD.mom <- data.group2$pnl_gross.AUD.mom  -
    data.group2$ntrans.AUD.mom * 7 
  
  data.group2$pnl_net.CAD.mom <- data.group2$pnl_gross.CAD.mom  -
    data.group2$ntrans.CAD.mom * 7
  
  # aggregate pnls and number of transactions to daily
  my.endpoints <- endpoints(data.group2, "days")
  
  data.group2.daily <- period.apply(data.group2[,c(grep("pnl", names(data.group2)),
                                                   grep("ntrans", names(data.group2)))],
                                    INDEX = my.endpoints, 
                                    FUN = function(x) colSums(x, na.rm = TRUE))
  
  # lets SUM gross and net pnls
  
  data.group2.daily$pnl_gross.mom <- 
    data.group2.daily$pnl_gross.AUD.mom +
    data.group2.daily$pnl_gross.CAD.mom
  
  data.group2.daily$pnl_net.mom <- 
    data.group2.daily$pnl_net.AUD.mom +
    data.group2.daily$pnl_net.CAD.mom
  
  # lets SUM number of transactions (with the same weights)
  
  data.group2.daily$ntrans.mom <- 
    data.group2.daily$ntrans.AUD.mom +
    data.group2.daily$ntrans.CAD.mom
  
  
  # summarize the strategy for this quarter
  
  # SR
  grossSR = mySR(x = data.group2.daily$pnl_gross.mom, scale = 252)
  netSR = mySR(x = data.group2.daily$pnl_net.mom, scale = 252)
  # CR
  grossCR = myCalmarRatio(x = data.group2.daily$pnl_gross.mom, scale = 252)
  netCR = myCalmarRatio(x = data.group2.daily$pnl_net.mom, scale = 252)
  
  # average number of transactions
  av.daily.ntrades = mean(data.group2.daily$ntrans.mom, 
                          na.rm = TRUE)
  # PnL
  grossPnL = sum(data.group2.daily$pnl_gross.mom)
  netPnL = sum(data.group2.daily$pnl_net.mom)
  
  # stat
  stat = netCR * max(0, log(abs(netPnL/1000)))
  
  # collecting all statistics for a particular quarter
  
  quarter_stats <- data.frame(quarter = selected_quarter,
                              assets.group = 2,
                              grossSR,
                              netSR,
                              grossCR,
                              netCR,
                              av.daily.ntrades,
                              grossPnL,
                              netPnL,
                              stat,
                              stringsAsFactors = FALSE
  )
  
  # collect summaries for all quarters
  if(!exists("quarter_stats.all.group2")) quarter_stats.all.group2 <- quarter_stats else
    quarter_stats.all.group2 <- rbind(quarter_stats.all.group2, quarter_stats)
  
  # create a plot of gros and net pnl and save it to png file
  
  png(filename = paste0("pnl_group2_", selected_quarter, ".png"),
      width = 1000, height = 600)
  print( # when plotting in a loop you have to use print()
    plot(cbind(cumsum(data.group2.daily$pnl_gross.mom),
               cumsum(data.group2.daily$pnl_net.mom)),
         multi.panel = FALSE,
         main = paste0("Gross and net PnL for asset group 2 \n quarter ", selected_quarter), 
         col = c("#377EB8", "#E41A1C"),
         major.ticks = "weeks", 
         grid.ticks.on = "weeks",
         grid.ticks.lty = 3,
         legend.loc = "topleft",
         cex = 1)
  )
  dev.off()
  
  # remove all unneeded objects for group 2
  rm(data.group2, my.endpoints, grossSR, netSR, av.daily.ntrades,
     grossPnL, netPnL, stat, quarter_stats, data.group2.daily)
  
  gc()
  
}

calculus_2 <- aggregate(quarter_stats.all.group2$netPnL, list(quarter_stats.all.group2$assets.group), sum)


write.csv(quarter_stats.all.group2, 
          "quarter_stats.all.group2.csv",
          row.names = FALSE)
