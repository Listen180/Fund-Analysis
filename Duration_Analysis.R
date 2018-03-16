# **********************************************************
# * Author        : LEI Sen
# * Email         : 
# * Create time   : 2018-01-27 14:09
# * Last modified : 2018-03-16 14:11
# * Filename      : Duration_Analysis.R
# * Description   : 
# * *******************************************************


##### Duration Analysis 1 #####
dt_new <- read.csv("newdata_v2.csv", header=T)

dt_new <- dt_new[,-1]
colnames(dt_new)
dt_new$基金账号 <- as.factor(dt_new$基金账号)
dt_new$基金代码 <- as.factor(dt_new$基金代码)
dt_new$确认日期 <- as.Date(dt_new$确认日期, format="%Y-%m-%d")
dt_new$额度区间 <- factor(dt_new$额度区间, levels=c(
  "100万以内(含)", "100万(不含)-500万(含)", "500万以上(不含)"), 
  ordered=TRUE)
str(dt_new)

#dt_new_sub <- subset(dt_new, 业务类型=="赎回" | 业务类型=="认购")
#dt_new_sub <- subset(dt_new, 业务类型=="赎回" | 业务类型=="申购")


dt_new$持有时间 <- NA
for (i in 1:dim(dt_new)[1]) {
  if (dt_new$业务类型[i]=="赎回" & dt_new$账户操作批次[i] > dt_new$账户操作批次[i+1]) {
    dt_new$持有时间[i] <- as.numeric(dt_new$确认日期[i] - dt_new$确认日期[i-dt_new$账户操作批次[i]+1])
  }
}
mean(dt_new$持有时间, na.rm=TRUE)

dt_new$持有时间 <- NA
for (i in 1:dim(dt_new)[1]) {
  if (dt_new$当前余额[i]<0 & dt_new$账户操作批次[i] > dt_new$账户操作批次[i+1]) {
    dt_new$持有时间[i] <- as.numeric(dt_new$确认日期[i] - dt_new$确认日期[i-dt_new$账户操作批次[i]+1])
  }
}
mean(dt_new$持有时间, na.rm=TRUE)



dt_new$基金类型识别码[dt_new$基金名称=="金鹰添瑞中短债A"] <- 0
dt_new$基金类型识别码[dt_new$基金名称=="金鹰添瑞中短债C"] <- 1

write.csv(dt_new, "newdata_v3_duration.csv")




##### 按照[基金账号]重组数据 #####
gp_by_account <- group_by(dt_new, 基金账号)
dt_by_account <- summarise(gp_by_account, 
                               "购买金额"=sum(购买金额), 
                               "赎回金额"=sum(赎回金额), 
                               "账户余额"=sum(账户流水), 
                               "基金类型"=sum(基金类型识别码), 
                               "认购交易"=sum(是否认购交易), 
                               "持有时间"=sum(持有时间, na.rm=TRUE))
rm(gp_by_account)
dt_by_account <- as.data.frame(dt_by_account)

dt_by_account$基金类型 <- ifelse(dt_by_account$基金类型==0, "金鹰添瑞中短债A", "金鹰添瑞中短债C")
dt_by_account$基金类型 <- as.factor(dt_by_account$基金类型)

dt_by_account$额度区间[dt_by_account$购买金额<=1e6] <- "100万以内(含)"
dt_by_account$额度区间[dt_by_account$购买金额>1e6 & dt_by_account$购买金额<=5e6] <- "100万(不含)-500万(含)"
dt_by_account$额度区间[dt_by_account$购买金额>5e6] <- "500万以上(不含)"
dt_by_account$额度区间 <- factor(dt_by_account$额度区间, levels=c(
  "100万以内(含)", "100万(不含)-500万(含)", "500万以上(不含)"), 
  ordered=TRUE)

dt_by_account$认购交易[dt_by_account$认购交易>=1] <- TRUE
dt_by_account$认购交易[dt_by_account$认购交易==0] <- FALSE

dt_by_account$持有时间[dt_by_account$持有时间==0] <- NA


write.csv(dt_by_account, "data for duration analysis.csv")
str(dt_by_account)

## Results: 
gp2_by_amount <- group_by(dt_by_account[dt_by_account$认购交易>=1,], 额度区间)
r1 <- summarise(gp2_by_amount, "持有时间"=mean(持有时间, na.rm=TRUE))
gp2_by_amount <- group_by(dt_by_account[dt_by_account$认购交易==0,], 额度区间)
r2 <- summarise(gp2_by_amount, "持有时间"=mean(持有时间, na.rm=TRUE))
result <- data.frame(r1[-1],r2[-1])
rm(r1, r2)
rownames(result) <- c("100万以内(含)", "100万(不含)-500万(含)", "500万以上(不含)")
colnames(result) <- c("认购交易", "非认购交易")
result
write.csv(result, "持有时间分析_是否认购.csv")


## Results: 
gp2_by_amount <- group_by(dt_by_account[dt_by_account$基金类型=="金鹰添瑞中短债A",], 额度区间)
r1 <- summarise(gp2_by_amount, "持有时间"=mean(持有时间, na.rm=TRUE))
gp2_by_amount <- group_by(dt_by_account[dt_by_account$基金类型=="金鹰添瑞中短债C",], 额度区间)
r2 <- summarise(gp2_by_amount, "持有时间"=mean(持有时间, na.rm=TRUE))
result <- data.frame(r1[-1],r2[-1])
rm(r1, r2)
rownames(result) <- c("100万以内(含)", "100万(不含)-500万(含)", "500万以上(不含)")
colnames(result) <- c("金鹰添瑞中短债A", "金鹰添瑞中短债C")
result
write.csv(result, "持有时间分析_基金类型.csv")






##### 其他分析 #####
dt_new_sub <- subset(dt_new, 持有时间>0)
dt_new_sub


Liste.duration <- function (dt, type) {
  dt_sub <- subset(dt, 销售商 == type)
  
  d <- mean(dt_sub$持有时间)
  out <- data.frame("class"=type, "avg_duration"=d)
  return(out)
}

d1 <- Liste.duration(dt=dt_new_sub, type="中信银行")
d2 <- Liste.duration(dt=dt_new_sub, type="招商银行")
d3 <- Liste.duration(dt=dt_new_sub, type="其他")
(out1 <- data.frame(rbind(d1, d2, d3)))
rm(d1, d2, d3)

write.csv(out1, "持有时间分析1.csv")


Liste.duration <- function (dt, type) {
  dt_sub <- subset(dt, 基金名称 == type)
  
  d <- mean(dt_sub$持有时间)
  out <- data.frame("class"=type, "avg_duration"=d)
  return(out)
}

d1 <- Liste.duration(dt=dt_new_sub, type="金鹰添瑞中短债A")
d2 <- Liste.duration(dt=dt_new_sub, type="金鹰添瑞中短债C")
(out2 <- data.frame(rbind(d1, d2)))
rm(d1, d2)

write.csv(out2, "持有时间分析2.csv")






