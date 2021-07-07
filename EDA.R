# DATA IS FROM 2008 TO 2019

data <- read.csv("R Codes/deliveries.csv",na.strings=c(""," ","NA"))
View(data)
data1 <- read.csv("R Codes/matches.csv")
View(data1)
data_re <- data1[-1]
View(data_re)
str(data_re)
sum(is.na(data_re))

barplot(table(data_re$winner),las =2, cex.names = 0.5,cex.axis = 1.5)
#Most successful team is MI followed by CSK

barplot(table(data_re$player_of_match),las =2, cex.names = 0.5,cex.axis = 1.5)
#Most successful player is CH Gayle followed by ABD

barplot(table(data_re$venue),las =2, cex.names = 0.5,cex.axis = 1.5)
#Most matches are played in Eden garden > Chinaswamy > Wankhade stadium

#----Mosaic plot for toss_winner & winner----
library(dplyr)
data_mo <- select(data_re, toss_winner,winner)
dftable <- xtabs(  ~ toss_winner + winner, data=data_re)
library(vcd)
mosaic(dftable, shade=TRUE, legend=TRUE, direction = "v", rot_labels=c(90,90,0,0))

#----Multiple bar plot----
library(ggplot2)
g <- ggplot(data_mo, aes(toss_winner, ..count..)) + geom_bar(aes(fill = winner), position = "dodge")
g+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Winning toss and winning match CSK > MI > KKR

g <- ggplot(data_re, aes(winner, ..count..)) + geom_bar(aes(fill = toss_decision), position = "dodge")
g+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Most wins while fielding first : MI > KXIP > RCB > KKR > CSK > RR > DD > SRH
#Most wins while batting first  : CSK > MI > KKR > RR > DD > RCB > SRH > KXIP 

data_re$dl_applied <- ifelse(data_re$dl_applied == 1, "Yes", "NO")
g <- ggplot(data_re, aes(winner, ..count..)) + geom_bar(aes(fill = dl_applied), position = "dodge")
g+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Below graph is for stacked bar plot
g <- ggplot(data_re, aes(winner, fill=dl_applied)) + 
   geom_bar( position='fill') +
    geom_text(aes(label=after_stat(count)), stat='count', position='fill')
g+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
#Most win while dl is applied : RPS > KTK > KKR

g <- ggplot(data_re, aes(venue, ..count..)) + geom_bar(aes(fill = winner), position = "dodge")
g+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
g + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Most wins on 
# 1)Eden Garden : KKR
# 2)Chinaswamy : CSK

#----HIGHEST RUNS SCORER----
batsman <- aggregate(batsman_runs ~ batsman, data=data, FUN= sum)
View(batsman)
top_run <-batsman[order(-batsman$batsman_runs),]
View(top_run)

g <- ggplot(top_run[1:10,], aes(x=batsman, y=batsman_runs)) + 
  geom_bar(stat = "identity") + ggtitle("Highest run scorer")
g+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
#  kohli > Raina > Rohit > Warner > Dhawan > Gayle

#----MOST MATCHES PLAYED----
batsman_match <- sqldf("select batsman,count(distinct(match_id)) as matches from data group by batsman")
batsman_match1 <- batsman_match[order(-batsman_match$matches),]
View(batsman_match1)
# RAINA > R SHARMA > DHONI > UTHAPPA

#----AVERAGE RUNS----
join <- inner_join(batsman_match1, top_run, by ='batsman')
View(join)
for(i in 1:nrow(join))
  join$avg_runs <- (join$batsman_runs/join$matches)
join <-join[order(-join$avg_run),]
View(join)

#Batsman who played matches more than 100
join1 <- join[join$matches >= 100,]
View(join1)

g <- ggplot(join1[1:10,], aes(x=batsman,y=avg_runs)) + 
  geom_bar(stat = "identity") + ggtitle("HIGHEST AVERAGE RUNS")
g+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
g + geom_bar(stat = "identity") +
  geom_text(aes(label = avg_runs), vjust = 0) # to display count of barplot,where label should be the name of column to count
g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) # to have xlab names vertical
# AVG RUNS FOR MORE THAN 100 MATCHES : WARNER > GAYLE > KOHLI > ABD

#----Not right below code----
batsman_played <- sqldf("select distinct(match_id),batsman from data")
View(batsman_played)
batsman_played <-batsman_played[order(-batsman_played$obs),]
View(batsman_played)
g <- ggplot(batsman_played[1:10,], aes(x=batsman, y=obs)) + 
  geom_bar(stat = "identity") + ggtitle("Highest match played")
g+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
# Highest match played : kohli > Raina > Rohit > Dhawan > Gambhir > Uthappa

merge <- merge(batsman_played,top_run)
View(merge)
for(i in 1:nrow(merge))
  merge$mean_run <- (merge$batsman_runs/merge$obs)
merge <-merge[order(-merge$mean_run),]
View(merge)

#----HIGHEST WICKET TAKERS----
library(sqldf)
wicket <- sqldf("select bowler,count(player_dismissed) as count from data group by bowler")
View(wicket)
High_wick <- wicket[order(-wicket$count),]
View(High_wick)
library(ggplot2)
g <- ggplot(High_wick[1:10,], aes(x=bowler,y=count)) + 
  geom_bar(stat = "identity") + ggtitle("Highest Wicket Takers")
g+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
g + geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = 0) # to display count of barplot
g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) # to have xlab names vertical
#MALINGA > BRAVO > A MISHRA > HARBHAJAN > P CHAWLA



