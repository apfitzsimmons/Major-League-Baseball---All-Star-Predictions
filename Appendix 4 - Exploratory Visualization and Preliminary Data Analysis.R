data = read.csv("~/Depaul/Fall 2022/Programming ML Applications/Final Project/Stats_AS_Target.csv")
library(tidyverse)
library(ggplot2)
library(ggforce)
library(beeswarm)
library(ggbeeswarm)
library(reshape2)
library(RColorBrewer)
library(corrplot)


data_AS = data%>% 
  filter(AS_Roster =='1')

#histogram of age of All stars
ggplot(data_AS,aes(x=Age))+geom_histogram(binwidth = 2)

#histogram of Plate appearances of All stars
ggplot(data_AS%>% 
         filter(AS_Roster =='1'),aes(x=PA))+geom_histogram(binwidth = 5)+
  xlim(0,450)
#set minimum threshold for plate appearances for all star consideration at 100
z = (mean(data_AS$PA)-100)/sd(data_AS$PA)
z
pnorm(z)
#100 PA represents 4.68 std dev from the mean which should account for a % of the population greater than 99.99 pct (nearing 100%)

data_reducedPA = data%>%
  filter(PA >=100)
summary(data_reducedPA)

#2022 data

#2021 data

#2019 data

#2018 data

#2017 data


data_reducedPA_num = select(data_reducedPA,-bbref_id,-season,-Name,-Age,-Level,-Team,-yearName,-AS_Roster)
standardized_data = as.data.frame(scale(data_reducedPA_num, center = TRUE, scale = TRUE))
summary(standardized_data)
standardized_data$AS_Roster = as.factor(data_reducedPA$AS_Roster)

#games, PA, BA
standardized_data%>%
  melt(id.var = c('AS_Roster'),
               variable.name = 'Feature')%>%
  filter(Feature=='G'|Feature=='PA'|Feature=='AB')%>%
  ggplot(aes(x=Feature,y=value)) + geom_quasirandom(aes(colour=AS_Roster,alpha=AS_Roster))+
  ylim(-3,3)+ylab('Std Dev')+theme_bw()+
  scale_color_manual(values = c("0" = "grey",
                                "1" = 'red'))

#R, H, x1B, x2B, x3B, HR
standardized_data%>%
  melt(id.var = c('AS_Roster'),
       variable.name = 'Feature')%>%
  filter(Feature=='R'|Feature=='H'|Feature=='X1B'|Feature=='X2B'|Feature=='X3B'|Feature=='HR')%>%
  ggplot(aes(x=Feature,y=value)) + geom_quasirandom(aes(colour=AS_Roster,alpha=AS_Roster))+
  ylim(-3,3)+ylab('Std Dev')+theme_bw()+
  scale_color_manual(values = c("0" = "grey",
                                "1" = 'red'))

#RBI, BB, IBB, uBB, SO, HBP
standardized_data%>%
  melt(id.var = c('AS_Roster'),
       variable.name = 'Feature')%>%
  filter(Feature=='RBI'|Feature=='BB'|Feature=='IBB'|Feature=='uBB'|Feature=='SO'|Feature=='HBP')%>%
  ggplot(aes(x=Feature,y=value)) + geom_quasirandom(aes(colour=AS_Roster,alpha=AS_Roster))+
  ylim(-3,3)+ylab('Std Dev')+theme_bw()+
  scale_color_manual(values = c("0" = "grey",
                                "1" = 'red'))

#SH, SF, GDP, SB, CS
standardized_data%>%
  melt(id.var = c('AS_Roster'),
       variable.name = 'Feature')%>%
  filter(Feature=='SH'|Feature=='SF'|Feature=='GDP'|Feature=='SB'|Feature=='CS')%>%
  ggplot(aes(x=Feature,y=value)) + geom_quasirandom(aes(colour=AS_Roster,alpha=AS_Roster))+
  ylim(-3,3)+ylab('Std Dev')+theme_bw()+
  scale_color_manual(values = c("0" = "grey",
                                "1" = 'red'))

#BA, OBP, SLG, OPS
standardized_data%>%
  melt(id.var = c('AS_Roster'),
       variable.name = 'Feature')%>%
  filter(Feature=='BA'|Feature=='OBP'|Feature=='SLG'|Feature=='OPS')%>%
  ggplot(aes(x=Feature,y=value)) + geom_quasirandom(aes(colour=AS_Roster,alpha=AS_Roster))+
  ylim(-3,3)+ylab('Std Dev')+theme_bw()+
  scale_color_manual(values = c("0" = "grey",
                                "1" = 'red'))
                                                    