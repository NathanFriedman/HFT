tr <- 0
tr = `20151204trades`
names(tr) = c("Ticker","Date","Time","Price","Volume","Buyer","Seller","What")
tr$Ticker = as.character(tr$Ticker)
tr$Time = as.character(tr$Time)
tr = data.frame(tr,0)
tr$X0 = matrix(as.numeric(unlist(strsplit(tr$Time,":"))), nrow = length(tr$Time),byrow = T)[,1]*60 + matrix(as.numeric(unlist(strsplit(tr$Time,":"))), nrow = length(tr$Time),byrow = T)[,2] +matrix(as.numeric(unlist(strsplit(tr$Time,":"))), nrow = length(tr$Time),byrow = T)[,3]/60 
names(tr) = c("Ticker","Date","Time","Price","Volume","Buyer","Seller","What","MOD")  
tr$MOD = tr$MOD - 570
tr[,"Time_P"] = as.POSIXct(strptime(tr[,"Time"],"%H:%M:%OS"))
hist(tr$Time_P, breaks = 110, main = "Distribution of transaction times, 15/12/04")

# TD Firm analysis
tr_TD = subset(tr, tr$Buyer == 1 | tr$Seller == 1)
TD_Buy = subset(tr,tr$Buyer == 1)
TD_Sell = subset(tr,tr$Seller ==1)

head(tr_TD)
hist(tr_TD$Time_P, breaks = 100, main = "TD Trades throughout Day")
hist(TD_Buy$Time_P, breaks = 100, main = "TD Buys throughout Day")
hist(TD_Sell$Time_P, breaks = 100, main = "TD Sells throughout Day")



#frequent <-0
#for (q in c(1:length(unique(tr$Ticker))))
#{
# frequent[q] = sum(tr$Ticker == unique(tr$Ticker)[q])
#}

freq_data = data.frame(ftable(tr$Ticker))
new_freq = freq_data[order(-freq_data$Freq),]
############################################################
CPG = tr[grep("CPG", tr$Ticker),]
hist(CPG$Time_P, breaks = 90, main = "Transactions of CPG")

TD = tr[tr$Ticker == "TD      ",]
hist(TD$Time_P,breaks = 90,main = "Transactions of TD")

AC = tr[tr$Ticker == "AC      ",]
hist(AC$Time_P,breaks = 90,main = "Transactions of AC")


CEE = tr[tr$Ticker == "CEE     ",]
hist(CEE$Time_P,breaks = 50, "Transactions of CEE")

ER = tr[tr$Ticker == "ER      ",]
hist(ER$Time_P,breaks = 50,main = "Transactions of ER")
############################################################
TD_TD = subset(TD, TD$Time_P <= day_end_end +1 & TD$Time_P >= day_end_end - 30*60)

############################################################

medata <-0
for(j in c(0:17))
{
  if(!j)
  {
    medata = subset(tr, tr$Ticker == as.character(med$unique.tr.Ticker.[j+1]))
  } else
  {
    medata = rbind(medata, subset(tr, tr$Ticker == med$unique.tr.Ticker.[j+1]))
  }
}

hist(medata$Time_P, breaks = 70, main = "18 lightly traded Stocks")



############################

bin <- 0
mod_start = min(tr_s$Time_P)
incriment = 10*60
k = 1
while (mod_start < end)
{
  bin[k] = sum(tr_s$Volume[tr_s$Time_P >= mod_start & tr_s$Time_P < (mod_start+incriment)])
  mod_start = mod_start + incriment
  k = k +1 
}
plot(min(tr_s$Time_P) + 600*c(0:39),bin, xlab = "Time", ylab = "Total Volume", main = "Total Volume vs. Time")
############################
############################

newbin <- 0
mod_start = min(tr_s$Time_P)
incriment = 10*60
k = 1
while (mod_start < end)
{
  newbin[k] = sum(tr_s$Mkt_cap[tr_s$Time_P >= mod_start & tr_s$Time_P < (mod_start+incriment)])
  mod_start = mod_start + incriment
  k = k +1 
}
plot(min(tr_s$Time_P) + 600*c(0:39),newbin, xlab = "Time", ylab = "Total $", main = "Time vs total price x Volume")
############################
for(j in c(0: dim(small_freq)[1]-1))
{
  stock_small = 
}

vol = 50

voltrad <- matrix(0,nrow = 24,ncol = 2)

for(q in c(1:24))
{
  
  voltrad[q,1] = sum(volumptious$Volume == vol)
  voltrad[q,2] = vol
  vol = vol + 50
}
##########################
bottom = subset(tr_s,tr_s$Time_P < median(tr_s$Time_P))
top = subset(tr_s,tr_s$Time_P >= median(tr_s$Time_P))
mean(bottom$Volume)
mean(top$Volume)
mean(bottom$Price)
mean(top$Price)
mean(bottom$Mkt_cap)
mean(top$Mkt_cap)

mean(bottom$Volume)*mean(bottom$Price)
mean(top$Volume)*mean(top$Price)


sum(bottom$Volume) - sum(top$Volume)
sum(top$Volume)
mean(bottom$Price)
mean(top$Price)
mean(bottom$Mkt_cap)
mean(top$Mkt_cap)


mean(bottom$Mkt_cap)
mean(top$Mkt_cap)

Buyer4 = subset(tr, tr$Buyer == 4 | tr$Seller == 4)
if(2 == 1:2)
{
  print("HI")
}
