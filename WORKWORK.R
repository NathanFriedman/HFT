Kanye <- 0
zzz = median(data.frame(ftable(tr$Ticker))$Freq)
yyy = unique(tr$Ticker)
for (j in c(1:length(yyy)))
{
  if (sum(tr$Ticker == yyy[j]) <= zzz)
  {   
     Kanye = append(Kanye,yyy[j])
  }
}

Kanye = Kanye[-1]

Jay_Z = subset(tr, tr$Ticker %in% Kanye)
hist(Jay_Z$Time_P, breaks = 90, main = "Smallest 943 stocks, 15/12/04")
################# TIme Variables ###########

day_start = min(tr$Time_P)
day_start_end = min(tr$Time_P) + 60
day_end_start = max(tr$Time_P) - (19 + 57*60) -1
day_end_end = max(tr$Time_P) - (52 + 52*60)

sum((TD$Time_P >= day_end_start) & (TD$Time_P < day_end_end))


############ TD ########################
Interval = day_start
step_inc = 1
holder <- 0
for (j in c(1:23401))
{
  holder[j] = sum((TD$Time_P >= Interval) & (TD$Time_P < Interval + step_inc))
  Interval = Interval + step_inc
}

plot(day_end_end +(-25:1),holder[(length(holder)-100 + 74):length(holder)], xlab = "3:59 Seconds", ylab = "Number of transactions", main = "TD - Last 25 Seconds")

holder[length(holder)]/mean(holder)
holder[length(holder)]/mean(holder[holder != 0])

#############################FM ##################################################


FM = subset(tr, tr$Ticker == "FM      ")
hist(FM$Time_P, breaks = 90, main = "Distribution of FM transactions")
Interval = day_start
step_inc = 1
holder_F <- 0
for (j in c(1:23401))
{
  holder_F[j] = sum((FM$Time_P >= Interval) & (FM$Time_P < Interval + step_inc))
  Interval = Interval + step_inc
}
tail(holder_F,120)
plot(day_end_end +(-25:1),holder_F[(length(holder_F)-100 +74):length(holder_F)], xlab = "3:59 Seconds", ylab = "Number of transactions", main = "FM - Last 25 Seconds")
holder_F[7200:7300]

holder_F[length(holder_F)]/mean(holder_F)
holder_F[length(holder_F)]/mean(holder_F[holder_F != 0])

#############################  BTO ##################################################

BTO = subset(tr, tr$Ticker == "BTO     ")
hist(BTO$Time_P, breaks = 90)
Interval = day_start
step_inc = 1
holder_B <- 0
for (j in c(1:23401))
{
  holder_B[j] = sum((BTO$Time_P >= Interval) & (BTO$Time_P < Interval + step_inc))
  Interval = Interval + step_inc
}
tail(holder_B,120)
plot(day_end_end +(-25:1),holder_B[(length(holder_B)-100 +74):length(holder_B)],xlab = "3:59 Seconds", ylab = "Number of transactions", main = "BTO - Last 25 Seconds")
holder_A[7200:7300]

holder_B[length(holder_B)]/mean(holder_B)
holder_B[length(holder_B)]/mean(holder_B[holder_B != 0])

#############################  RY ##################################################

RY = subset(tr, tr$Ticker == as.character(new_freq[2,1]))
hist(RY$Time_P, breaks = 90, main = "RY distribution of transactions")
Interval = day_start
step_inc = 1
holder_R <- 0
for (j in c(1:23401))
{
  holder_R[j] = sum((RY$Time_P >= Interval) & (RY$Time_P < Interval + step_inc))
  Interval = Interval + step_inc
}
tail(holder_R,120)
plot(day_end_end +(-25:1),holder_R[(length(holder_R)-100 +74):length(holder_R)],xlab = "3:59 Seconds", ylab = "Number of transactions", main = "RY - Last 25 Seconds")
holder_A[7200:7300]

holder_R[length(holder_R)]/mean(holder_R)
holder_R[length(holder_R)]/mean(holder_R[holder_R != 0])

########################  Small stocks #######################
Interval = day_start
step_inc = 1
holder_Z <- 0
for (j in c(1:23401))
{
  holder_Z[j] = sum((Jay_Z$Time_P >= Interval) & (Jay_Z$Time_P < Interval + step_inc))
  Interval = Interval + step_inc
}
plot(day_end_end +(-25:1),holder_Z[(length(holder_Z)-100 +74):length(holder_Z)], ylim = c(0,25),xlab = "3:59 Seconds", ylab = "Number of transactions", main = "Smallest 943 - Last 25 Seconds")
holder_Z[length(holder_Z)]/mean(holder_Z)
holder_Z[length(holder_Z)]/mean(holder_Z[holder_Z != 0])

##########################   ENB  #######################################
ENB = subset(tr, tr$Ticker == as.character(new_freq[1,1]))
hist(CNQ$Time_P, breaks = 3*3*30, main = "ENB distribution of transactions")
Interval = day_start
step_inc = 1
holder_N <- 0
for (j in c(1:23401))
{
  holder_N[j] = sum((ENB$Time_P >= Interval) & (ENB$Time_P < Interval + step_inc))
  Interval = Interval + step_inc
}
tail(holder_N,120)
plot(day_end_end +(-25:1),holder_N[(length(holder_N)-100 +74):length(holder_N)],xlab = "3:59 Seconds", ylab = "Number of transactions", main = "CNQ - Last 25 Seconds")

holder_N[length(holder_N)]/mean(holder_N)
holder_N[length(holder_N)]/mean(holder_N[holder_N != 0])
