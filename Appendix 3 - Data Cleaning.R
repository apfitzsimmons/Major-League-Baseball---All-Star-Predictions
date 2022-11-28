#install.packages("devtools")
devtools::install_github("BillPetti/baseballr")


library(baseballr)
library(ggplot2)
library(lubridate)
library(dplyr)

#Season Start and AS Game Dates
#2022 start: 2022-04-07
#2022 as game: 2022-07-19
#2021 start: 2021-04-01
#2021 as game: 2021-07-13
#2019 start: 2019-03-20
#2019 as game: 2019-07-09
#2018 start: 2018-03-29
#2018 as game: 2018-07-17
#2017 start: 2017-04-02
#2017 as game: 2017-07-11


#batting data from start of season to AS game (each season)
stats2017 = daily_batter_bref("2017-04-02", "2017-07-11")
stats2018 = daily_batter_bref("2018-03-29", "2018-07-17")
stats2019 = daily_batter_bref("2019-03-20", "2019-07-09")
stats2021 = daily_batter_bref("2021-04-01", "2021-07-13")
stats2022 = daily_batter_bref("2022-04-07", "2022-07-19")

#bind all years together
statsALL = rbind(stats2022,stats2021,stats2019,stats2018,stats2017)

#read All star Rosters
AS_Rosters = read.csv("~/Depaul/Fall 2022/Programming ML Applications/Final Project/DSC478_AS_Rosters - AS Rosters.csv")
AS_Rosters = rename(AS_Rosters,Name=Player)
AS_Rosters$Position = as.factor(AS_Rosters$Position)
summary(AS_Rosters$Position)

#set Shohei Ohtani to DH to keep his batting stats
filter(AS_Rosters,Position == 'DH/P')
AS_Rosters$Position[AS_Rosters$Position == 'DH/P'] = 'DH'
summary(AS_Rosters$Position)

#filter to remove Pitchers
AS_Rosters_Batters = filter(AS_Rosters, Position != 'P')

#remove some dataframes
ls()
rm("stats2017","stats2018","stats2019","stats2021","stats2022")
rm("DSC478_AS_Rosters...AS.Rosters","data","AS_Rosters",'test','AS_ID_list')




#generate keys with names and ID
bbrefID_name = statsALL %>% select('bbref_id','season','Name')
bbrefID_name$yearName = paste(bbrefID_name$season,bbrefID_name$Name)

#convert for latin names
AS_Rosters_Batters$Name = iconv(AS_Rosters_Batters$Name,from="UTF-8",to="ASCII//TRANSLIT")

#assign bbref_id to AS Roster
AS_Rosters_Batters$yearName = paste(AS_Rosters_Batters$Year,AS_Rosters_Batters$Name)

#create list of IDs to appear in AS games
AS_YearName_list = as.list(AS_Rosters_Batters$yearName)

#create new dataframe to add target variable to
statsALL_AS = statsALL
statsALL_AS$yearName = paste(statsALL_AS$season,statsALL_AS$Name)
statsALL_AS$AS_Roster = ifelse(statsALL_AS$yearName %in% AS_YearName_list,1,0)
filter(statsALL_AS,AS_Roster=='1')

#find entries that worked
statsAS_only = filter(statsALL_AS,AS_Roster=='1')
successfully_input_target = as.list(statsAS_only$yearName)
for (entry in AS_YearName_list) {if(entry %in% successfully_input_target){}else{print(entry)}}

#add rownames
statsALL_AS%>%
  add_rownames()%>%
  filter(,grepl('2017 DJ',yearName))%>%
  select('rowname','yearName','AS_Roster')

#update all star status on outstanding entries
statsALL_AS[172,32]=1
statsALL_AS[205,32]=1
statsALL_AS[684,32]=1
statsALL_AS[650,32]=1
statsALL_AS[1532,32]=1
statsALL_AS[1533,32]=1
statsALL_AS[1498,32]=1
statsALL_AS[2353,32]=1
statsALL_AS[3237,32]=1
statsALL_AS[3191,32]=1

#final data
sum(statsALL_AS$AS_Roster)

#modify name text
statsALL_AS$Name = iconv(statsALL_AS$Name,from="UTF-8",to="ASCII//TRANSLIT")
statsALL_AS$yearName = paste(statsALL_AS$season,statsALL_AS$Name)

#filter for at least 1 at bat and check for NA
statsALL_AS = statsALL_AS %>%
  filter(,AB >='1')
statsALL_AS[rowSums(is.na(statsALL_AS)) > 0,]


#write to csv
write.csv(statsALL_AS,file="~/Depaul/Fall 2022/Programming ML Applications/Final Project//Stats_AS_Target.csv", row.names = FALSE)
