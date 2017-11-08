#1
rm(list=ls())
dt<-read.csv('C:/Users/Gaurav Nand/Desktop/amazon_compare.csv')
library(data.table)
dt<-data.table(dt)
#2
dt1<-dt[which(!is.na(dt$price)&!is.na(dt$price_online)),]
dt2<-dt1[price<=quantile(price,prob=0.99)&price_online<=quantile(price_online,prob=0.99),]
set.seed(31)
dt2$id_rand<-rnorm(nrow(dt2),mean=0,sd=1)
dt3<-dt2[order(dt2$id_rand,decreasing = FALSE)]
dt4<-dt3[1:1000]
#3
dt2[,.(mean_price=mean(price),sd_price=sd(price)),]

dt2[,quantile(price,probs=c(0,0.25,0.5,0.75,1))]
dt2[,quantile(price,probs = seq(0,1,0.1))]

dt2[,.(mean_priceonlne=mean(price_online),sd_priconline=sd(price_online)),]

dt2[,quantile(price_online,probs=c(0,0.25,0.5,0.75,1))]
dt2[,quantile(price_online,probs = seq(0,1,0.1))]

#4
dt2[,t.test(price==price_online,alternative = c("two.sided"))]


sd_price<-dt2[,sd(price)^2/nrow(dt2)]
sd_price_online<-dt2[,sd(price_online)^2/nrow(dt2)]
sd_total<-sqrt(sd_price+sd_price_online)
mean_total<-dt2[,mean(price)-mean(price_online)]

t<-mean_total/sd_total
#5
x<-mean(dt2$price_online)
dt2[,t.test(price,mu=x)]
y<-mean(dt2$price)
dt2[,t.test(price,mu=y)]


#6
dt5<-dt[price<quantile(price,0.95),]

#7
dt5[is.na(price_online),missing_online:=1]
dt5[!is.na(price_online),missing_online:=0]
library(ggplot2)
ggplot(dt5,aes(factor(missing_online),price))+geom_boxplot()
ggplot(dt5,aes(factor(missing_online),price_amazon))+geom_boxplot()

ggplot(dt5,aes(price))+geom_histogram()+facet_wrap(~missing_online)
ggplot(dt5,aes(price_amazon))+geom_histogram()+facet_wrap(~missing_online)

#8
dt_compare<-list(price_1=double(),price_amazon_1=double(),price_0=double(),price_amazon_0=double())
dt_compare[[1]][1]<-dt5[missing_online==1,mean(price)]
dt_compare[[1]][2]<-dt5[missing_online==1,mean(price_amazon)]

dt_compare[[1]][3]<-dt5[missing_online==0,mean(price)]
dt_compare[[1]][4]<-dt5[missing_online==0,mean(price_amazon)]

#9

dt5[, lapply(.SD, mean), by = category, .SDcols = c("price", "price_amazon")]
dt5[, lapply(.SD, mean), by = retailer_s, .SDcols = c("price", "price_amazon")]
dt5[, lapply(.SD, mean), by = list(category, retailer_s), .SDcols = c("price", "price_amazon")]
#10
dt_ps<-dt5[is.na(price_online),]
dt_ps[, lapply(.SD, mean), by = category, .SDcols = c("price", "price_amazon")]
dt_ps[, lapply(.SD, mean), by = retailer_s, .SDcols = c("price", "price_amazon")]
dt_ps[, lapply(.SD, mean), by = list(category, retailer_s), .SDcols = c("price", "price_amazon")]

dt_ps_95<-dt_ps[price<quantile(price,0.95),]
dt_ps_95[, lapply(.SD, mean), by = category, .SDcols = c("price", "price_amazon")]
dt_ps_95[, lapply(.SD, mean), by = retailer_s, .SDcols = c("price", "price_amazon")]
dt_ps_95[, lapply(.SD, mean), by = list(category, retailer_s), .SDcols = c("price", "price_amazon")]

#11
set.seed(30)
dt5[,id_rand:=runif(nrow(dt5),min=0,max=1)]

#12
x<-1
y<-100
Sample<-y-x+1
dt6<-dt5[order(dt5$id_rand),]
dt7<-dt6[x:y]
CI_95<-data.frame(sample=double(),mean=double(),sd=double(),lowest_value=double(),highest_value=double())
CI_95[1,1]<-Sample

CI_95[1,2]<-dt7[,mean(price)]
CI_95[1,3]<-dt7[,sd(price)]
CI_95[1,4]<-dt7[,mean(price)]-qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95[1,5]<-dt7[,mean(price)]+qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95


#13
x<-1
y<-200
Sample<-y-x+1
dt8<-dt6[x:y]
CI_95[2,1]<-Sample
CI_95[2,2]<-dt8[,mean(price)]
CI_95[2,3]<-dt8[,sd(price)]
CI_95[2,4]<-dt8[,mean(price)]-qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95[2,5]<-dt8[,mean(price)]+qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]

#14

x<-1
y<-300
Sample<-y-x+1

dt9<-dt6[x:y]
CI_95[3,1]<-Sample
CI_95[3,2]<-dt9[,mean(price)]
CI_95[3,3]<-dt9[,sd(price)]
CI_95[3,4]<-dt9[,mean(price)]-qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95[3,5]<-dt9[,mean(price)]+qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]

#15
x<-1
y<-1000
Sample<-y-x+1

dt10<-dt6[x:y]
CI_95[4,1]<-Sample
CI_95[4,2]<-dt10[,mean(price)]
CI_95[4,3]<-dt10[,sd(price)]
CI_95[4,4]<-dt10[,mean(price)]-qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95[4,5]<-dt10[,mean(price)]+qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]

#16
set.seed(301)
dt5[,id_rand:=runif(nrow(dt5),min=0,max=1)]


dt6<-dt5[order(dt5$id_rand),]
x<-1
y<-100
dt7<-dt6[x:y]

Sample<-y-x+1
CI_95<-data.frame(sample=double(),mean=double(),sd=double(),lowest_value=double(),highest_value=double())
CI_95[1,1]<-Sample

CI_95[1,2]<-dt7[,mean(price)]
CI_95[1,3]<-dt7[,sd(price)]
CI_95[1,4]<-dt7[,mean(price)]-qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95[1,5]<-dt7[,mean(price)]+qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95

x<-1
y<-200
Sample<-y-x+1
dt8<-dt6[x:y]
CI_95[2,1]<-Sample
CI_95[2,2]<-dt8[,mean(price)]
CI_95[2,3]<-dt8[,sd(price)]
CI_95[2,4]<-dt8[,mean(price)]-qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95[2,5]<-dt8[,mean(price)]+qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]



x<-1
y<-300
Sample<-y-x+1

dt9<-dt6[x:y]
CI_95[3,1]<-Sample
CI_95[3,2]<-dt9[,mean(price)]
CI_95[3,3]<-dt9[,sd(price)]
CI_95[3,4]<-dt9[,mean(price)]-qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95[3,5]<-dt9[,mean(price)]+qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]


x<-1
y<-1000
Sample<-y-x+1

dt10<-dt6[x:y]
CI_95[4,1]<-Sample
CI_95[4,2]<-dt10[,mean(price)]
CI_95[4,3]<-dt10[,sd(price)]
CI_95[4,4]<-dt10[,mean(price)]-qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95[4,5]<-dt10[,mean(price)]+qnorm(0.975)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]
#17
set.seed(30)
dt5[,id_rand:=runif(nrow(dt5),min=0,max=1)]


dt6<-dt5[order(dt5$id_rand),]
#90% CI:
x<-1
y<-100
dt7<-dt6[x:y]

Sample<-y-x+1
CI_95<-data.frame(sample=double(),mean=double(),sd=double(),lowest_value=double(),highest_value=double())
CI_95[1,1]<-Sample

CI_95[1,2]<-dt7[,mean(price)]
CI_95[1,3]<-dt7[,sd(price)]
CI_95[1,4]<-dt7[,mean(price)]-qnorm(0.95)*dt5[,sd(price)/sqrt(Sample)]
CI_95[1,5]<-dt7[,mean(price)]+qnorm(0.95)*dt5[,sd(price)/sqrt(Sample)]
CI_95

x<-1
y<-200
Sample<-y-x+1
dt8<-dt6[x:y]
CI_95[2,1]<-Sample
CI_95[2,2]<-dt8[,mean(price)]
CI_95[2,3]<-dt8[,sd(price)]
CI_95[2,4]<-dt8[,mean(price)]-qnorm(0.95)*dt5[,sd(price)/sqrt(Sample)]
CI_95[2,5]<-dt8[,mean(price)]+qnorm(0.95)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]



x<-1
y<-300
Sample<-y-x+1

dt9<-dt6[x:y]
CI_95[3,1]<-Sample
CI_95[3,2]<-dt9[,mean(price)]
CI_95[3,3]<-dt9[,sd(price)]
CI_95[3,4]<-dt9[,mean(price)]-qnorm(0.95)*dt5[,sd(price)/sqrt(Sample)]
CI_95[3,5]<-dt9[,mean(price)]+qnorm(0.95)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]


x<-1
y<-1000
Sample<-y-x+1

dt10<-dt6[x:y]
CI_95[4,1]<-Sample
CI_95[4,2]<-dt10[,mean(price)]
CI_95[4,3]<-dt10[,sd(price)]
CI_95[4,4]<-dt10[,mean(price)]-qnorm(0.95)*dt5[,sd(price)/sqrt(Sample)]
CI_95[4,5]<-dt10[,mean(price)]+qnorm(0.95)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]

#99% CI:
set.seed(30)

x<-1
y<-100
dt7<-dt6[x:y]

Sample<-y-x+1
CI_95<-data.frame(sample=double(),mean=double(),sd=double(),lowest_value=double(),highest_value=double())
CI_95[1,1]<-Sample

CI_95[1,2]<-dt7[,mean(price)]
CI_95[1,3]<-dt7[,sd(price)]
CI_95[1,4]<-dt7[,mean(price)]-qnorm(0.995)*dt5[,sd(price)/sqrt(Sample)]
CI_95[1,5]<-dt7[,mean(price)]+qnorm(0.995)*dt5[,sd(price)/sqrt(Sample)]
CI_95

x<-1
y<-200
Sample<-y-x+1
dt8<-dt6[x:y]
CI_95[2,1]<-Sample
CI_95[2,2]<-dt8[,mean(price)]
CI_95[2,3]<-dt8[,sd(price)]
CI_95[2,4]<-dt8[,mean(price)]-qnorm(0.995)*dt5[,sd(price)/sqrt(Sample)]
CI_95[2,5]<-dt8[,mean(price)]+qnorm(0.995)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]



x<-1
y<-300
Sample<-y-x+1

dt9<-dt6[x:y]
CI_95[3,1]<-Sample
CI_95[3,2]<-dt9[,mean(price)]
CI_95[3,3]<-dt9[,sd(price)]
CI_95[3,4]<-dt9[,mean(price)]-qnorm(0.995)*dt5[,sd(price)/sqrt(Sample)]
CI_95[3,5]<-dt9[,mean(price)]+qnorm(0.995)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]


x<-1
y<-1000
Sample<-y-x+1

dt10<-dt6[x:y]
CI_95[4,1]<-Sample
CI_95[4,2]<-dt10[,mean(price)]
CI_95[4,3]<-dt10[,sd(price)]
CI_95[4,4]<-dt10[,mean(price)]-qnorm(0.995)*dt5[,sd(price)/sqrt(Sample)]
CI_95[4,5]<-dt10[,mean(price)]+qnorm(0.995)*dt5[,sd(price)/sqrt(Sample)]
CI_95
dt5[,mean(price)]
#18
dt[,times_date:=.N,by=date]
dt_norm<-dt[,.(normalization=times_date-min(times_date)/max(times_date)-min(times_date))]
ggplot(dt_norm,aes(normalization))+geom_histogram()

dt_standard<-dt[,.(standardization=times_date-mean(times_date)/sd(times_date))]
ggplot(dt_standard,aes(standardization))+geom_histogram()

dt_log<-dt[,.(log=log(times_date))]
ggplot(dt_log,aes(log))+geom_histogram()
