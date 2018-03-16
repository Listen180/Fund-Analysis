# **********************************************************
# * Author        : LEI Sen
# * Email         : 
# * Create time   : 2018-01-27 14:12
# * Last modified : 2018-03-16 14:13
# * Filename      : TianRui_LEISen.R
# * Description   : 
# * *******************************************************


##### Set Up #####
setwd("/Users/leisen/Box Sync/Work/TianRui")

library(lubridate)
library(plyr)
library(dplyr)
library(tibble)
library(reshape2)

##### Data Reading and Modification #####
raw <- read.csv("data/rawdata.csv", header=T)
(org_colnames <- colnames(raw))
str(raw)

## Data RE-Format
raw$基金账号 <- as.factor(raw$基金账号)
raw$基金代码 <- as.factor(raw$基金代码)
raw$确认日期 <- as.Date(raw$确认日期, format="%Y-%m-%d")
raw$申请日期 <- as.Date(raw$申请日期, format="%Y-%m-%d")
#data_org$赎回日 <- as.Date(data_org$赎回日)

## Using a (sub)dataset
data <- raw[,c("基金账号", "确认日期", "基金代码", "基金名称", 
                    "销售商", "业务类型", "确认金额", "客户类型")]
(data_colnames <- colnames(data))

str(data)

## Filter & Sort
data_comp <- data[complete.cases(data$确认日期),]
data_comp <- data_comp[order(data_comp$确认日期), ]

str(data_comp)


##### Data RE-Construction #####
## Mount the data set (in use)
dt <- subset(data_comp, 业务类型 %in% c("系统确认", "申购", "赎回"))

dt$业务类型 <- as.character(dt$业务类型)
dt$业务类型[dt$业务类型=="系统确认"] <- "认购"
dt$业务类型 <- factor(dt$业务类型, levels=c("认购", "申购", "赎回"))

dt$销售商 <- as.character(dt$销售商)
dt$销售商[!(dt$销售商 %in% c("招商银行", "中信银行"))] <- "其他"
dt$销售商 <- as.factor(dt$销售商)

dt$账户流水 <- dt[, "确认金额"]
dt$购买金额 <- dt[, "确认金额"]
dt$赎回金额 <- dt[, "确认金额"]
dt$账户流水[dt$业务类型=="赎回"] <- - dt$账户流水[dt$业务类型=="赎回"]
dt$购买金额[dt$业务类型=="赎回"] <- 0
dt$赎回金额[dt$业务类型!="赎回"] <- 0

dt$额度区间[dt$业务类型!="赎回" & dt$购买金额<=1e6] <- "100万以内(含)"
dt$额度区间[dt$业务类型!="赎回" & dt$购买金额>1e6 & dt$购买金额<=5e6] <- "100万(不含)-500万(含)"
dt$额度区间[dt$业务类型!="赎回" & dt$购买金额>5e6] <- "500万以上(不含)"
dt$额度区间[dt$业务类型=="赎回" & dt$赎回金额<=1e6] <- "100万以内(含)"
dt$额度区间[dt$业务类型=="赎回" & dt$赎回金额>1e6 & dt$赎回金额<=5e6] <- "100万(不含)-500万(含)"
dt$额度区间[dt$业务类型=="赎回" & dt$赎回金额>5e6] <- "500万以上(不含)"
dt$额度区间 <- factor(dt$额度区间, levels=c(
  "100万以内(含)", "100万(不含)-500万(含)", "500万以上(不含)"), 
  ordered=TRUE)

gp_buy <- group_by(dt[(dt$业务类型=="认购" | dt$业务类型=="申购"),], 额度区间)
sum_buy <- dplyr::summarise(gp_buy, "认购申购交易笔数"=n(), "总认购申购额"=sum(购买金额), "平均认购申购额"=mean(购买金额))
sum_buy
rm(gp_buy)

gp_redeem <- group_by(dt[dt$业务类型=="赎回",], 额度区间)
sum_redeem <- dplyr::summarise(gp_redeem, "赎回交易笔数"=n(), "总赎回额"=sum(赎回金额), "平均赎回额"=mean(赎回金额))
sum_redeem
rm(gp_redeem)

out1 <- data.frame(sum_buy[-1], sum_redeem[,-1])
rownames(out1) <- c("100万以内(含)", "100万(不含)-500万(含)", "500万以上(不含)")
out1

write.csv(dt, "newdata.csv")
write.csv(out1, "summary_journal.csv")






##### 按照[基金账号]重组数据 #####
gp_by_account <- group_by(dt, 基金账号)
dt_by_account <- dplyr::summarise(gp_by_account, 
                                  "购买金额"=sum(购买金额), 
                                  "赎回金额"=sum(赎回金额), 
                                  "账户余额"=sum(账户流水), 
                                  "额度类别"=sum(as.numeric(额度区间)))
rm(gp_by_account)
dt_by_account <- as.data.frame(dt_by_account)

dt_by_account$额度区间[dt_by_account$购买金额<=1e6] <- "100万以内(含)"
dt_by_account$额度区间[dt_by_account$购买金额>1e6 & dt_by_account$购买金额<=5e6] <- "100万(不含)-500万(含)"
dt_by_account$额度区间[dt_by_account$购买金额>5e6] <- "500万以上(不含)"
dt_by_account$额度区间 <- factor(dt_by_account$额度区间, levels=c(
  "100万以内(含)", "100万(不含)-500万(含)", "500万以上(不含)"), 
  ordered=TRUE)

dt_by_account$赎回情况[dt_by_account$账户余额<0] <- "全部赎回"
dt_by_account$赎回情况[dt_by_account$账户余额>=0] <- "部分赎回"
dt_by_account$赎回情况[dt_by_account$账户余额==dt_by_account$购买金额] <- "全部未赎回"
dt_by_account$赎回情况[dt_by_account$购买金额==1 & dt_by_account$账户余额==1] <- "仅开户"
dt_by_account$赎回情况 <- as.factor(dt_by_account$赎回情况)

summary(dt_by_account)
gp2_by_amount <- group_by(dt_by_account, 额度区间)
gp2_by_amount_sum <- summarise(gp2_by_amount, "基金账户数"=n(), "总购买额"=sum(购买金额), "平均购买额"=mean(购买金额))
gp2_by_amount_sum

gp2_by_amount_sub1 <- subset(gp2_by_amount, 赎回情况=="全部赎回")
gp2_by_amount_sub1_sum <- summarise(gp2_by_amount_sub1, "账户全部赎回数"=n(), "总赎回额"=sum(赎回金额))
gp2_by_amount_sub2 <- subset(gp2_by_amount, 赎回情况=="全部未赎回" | 赎回情况=="仅开户" | 赎回情况=="部分赎回")
gp2_by_amount_sub2_sum <- summarise(gp2_by_amount_sub2, "账户未全部赎回数"=n(), "未赎回额"=sum(购买金额))

redeem_sum <- data.frame(gp2_by_amount_sum[-1], 
                         gp2_by_amount_sub1_sum[,-1], 
                         gp2_by_amount_sub2_sum[,-1])
rownames(redeem_sum) <- c("100万以内(含)", "100万(不含)-500万(含)", "500万以上(不含)")
redeem_sum
(redeem_sum_sum <- data.frame(t(apply(redeem_sum, 2, sum))))
(test <- redeem_sum_sum$总赎回额 + redeem_sum_sum$未赎回额 - redeem_sum_sum$总购买额 )

write.csv(redeem_sum, "redeem_summary_by_account.csv")

write.csv(dt_by_account, "newdata_by_account.csv")




##### Self-Defined Function #####
## General func.: windows.generate
windows.generate <- function (dt_dates, window_length) {
  dt_dates <- data.frame(dt_dates)
  t <- data.frame(dt_dates[1,])
  time_windows <- t
  while (t < data.frame(dt_dates[dim(dt_dates)[1],])) {
    t <- t + window_length
    time_windows <- rbind(time_windows, t)
  }
  colnames(time_windows) <- paste0("time_match", "_", window_length)
  
  return(time_windows)
}

## General func.: windows.check
windows.check <- function (dt, windows, col_time, col_value) {
  windows <- windows
  sum_windows <- data.frame(matrix(NA, dim(dt)[1], dim(windows)[1]))
  int_names <- matrix(NA, dim(windows)[1], 1)
  
  for (j in 1:dim(windows)[1]) {
    for (i in 1:dim(dt)[1]) {
      if (dt[i, col_time] >= windows[j,] & dt[i, col_time] < windows[j+1,]) {
        sum_windows[i,j] <- dt[i, col_value]
      }
      else {
        sum_windows[i,j] <- 0
      }
    }
    int_names[j] <- paste0("[", windows[j,], ", ", windows[j+1,], ")")
  }
  
  sum_windows <- data.frame(sum_windows)
  sum_windows_sum <- data.frame(apply(sum_windows, 2, sum))
  sum_windows_count <- data.frame(apply(sum_windows>0, 2, sum))
  
  colnames(sum_windows_sum) <- colnames(dt)[col_value]
  rownames(sum_windows_sum) <- int_names
  colnames(sum_windows_count) <- "交易笔数"
  
  output <- data.frame(sum_windows_sum, sum_windows_count)
  return(output)
}







##### Windows Analysis 1 #####
table(dt$业务类型)
dt_window <- dt[dt$业务类型=="认购", ]
dt_window <- dt[dt$业务类型=="申购", ]
dt_window <- dt[dt$业务类型=="赎回", ]



## Build the windows
(time_windows_10d <- windows.generate(dt_window$确认日期, 10))
(time_windows_14d <- windows.generate(dt_window$确认日期, 14))

## Analyse data using windows
t(colnames(dt_window))
(output_10d <- windows.check(dt=dt_window, windows=time_windows_10d, col_time=2, col_value=7))
(output_14d <- windows.check(dt_window, time_windows_14d, col_time=2, col_value=7))

check <- c(951, 413, 532,321,382,261,450,179)
sum(check)


write.csv(output_10d, "10d.csv")
write.csv(output_14d, "14d.csv")




##### Data RE-Construction 2 #####
dt$是否认购交易 <- dt$业务类型=="认购"
dt$是否申购交易 <- dt$业务类型=="申购"
dt$是否赎回交易 <- dt$业务类型=="赎回"

dt_new <- dt[order(dt$基金账号),]

dt_new$账户操作批次 <- 1
dt_new$当前余额 <- dt_new$账户流水


for (i in 2:dim(dt_new)[1]) {
  if (dt_new$基金账号[i] == dt_new$基金账号[i-1]) {
    dt_new$账户操作批次[i] <- dt_new$账户操作批次[i-1] + 1
    dt_new$当前余额[i] <- dt_new$当前余额[i-1] + dt_new$当前余额[i]
  }
}

write.csv(dt_new, "newdata_v2.csv")




