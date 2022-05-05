rm(list=ls())

#getwd()
#setwd("C:/Users/theRa/OneDrive/Documents")

#Original dataframe gotten from Kaggle
#Contains all players from 1996-97 to 2020-21 NBA seasons (Has 11,700 rows)
Seasons<-read.csv("C:/Users/humbl/OneDrive/Documents/Data Wrangling/Final Project/all_seasons_project.csv",sep = ",",header = T)
Seasons$ï..<-NULL
str(Seasons)

#Subset dataframe for all players in 2019-20 Season
#If we need a larger amount of rows then we will subset more than one season
Seasons2<-subset(Seasons, season=='2019-20')

#Web scraped data from basketball-reference website (free to use)
#[1] gets the player name
#[8] gets the field goals made per game
#If we need a larger amount of rows then we will scrape again from basketball-reference website
#basketball-reference keeps stats from multiple seasons so that won't be a worry if we need more
library(xml2)
read_html("https://www.basketball-reference.com/leagues/NBA_2020_per_game.html")
page<-read_html("https://www.basketball-reference.com/leagues/NBA_2020_per_game.html")
class(page)
page

links<-xml_attr(xml_find_all(page,"//tr[@class='full_table']/td[1]"),"href")
player_name<-xml_text(xml_find_all(page,"//tr[@class='full_table']/td[1]"),"player_name")
player_name_df<-data.frame(player_name)

links2<-xml_attr(xml_find_all(page,"//tr[@class='full_table']/td[8]"),"href")
FGM<-xml_text(xml_find_all(page,"//tr[@class='full_table']/td[8]"),"FGM")
FGM_df<-data.frame(FGM)
#If we need more rows, we will scrape again using a similar process with a different season

#Vertical and Horizontal integration to put into one merged dataframe
Players<-rbind(player_name,FGM)
Players<-t(Players)
merged_data<-merge(Seasons2, Players, by="player_name")
str(merged_data)
merged_data$FGM<-as.numeric(merged_data$FGM)
str(merged_data)
merged_data<-merged_data[order(-merged_data$FGM),]

#null info that we are not using in questions
merged_data$pts<-NULL
merged_data$oreb_pct<-NULL
merged_data$dreb_pct<-NULL
merged_data$ast_pct<-NULL
merged_data$season<-NULL

#We are checking how certain basketball statistics, measurements, 
#and background information like where they are from relate
#with field goals made per game (FGM)
#Players with more FGM tend to be the best players
#If you sort the merged_data by FGM, you'll see all the
#best NBA players like Lebron, Giannis, and Harden near the top



#1)Is there a significant correlation between the amount of games played, 
#assists, and rebounds per game by each player and the field goals made per game that they have?

#Get correlation value for games played and field goals made
cor(merged_data$gp, merged_data$FGM)
#Get correlation value for assists and field goals made
cor(merged_data$ast, merged_data$FGM)
#Get correlation value for rebounds and field goals made
cor(merged_data$reb, merged_data$FGM)
library(ggplot2)
#plot a scatter plot of the three vairables with FGM
ggplot(merged_data, aes(x=FGM,y=gp))+geom_point()+geom_smooth(method=lm)
ggplot(merged_data, aes(x=FGM,y=ast))+geom_point()+geom_smooth(method=lm)
ggplot(merged_data, aes(x=FGM,y=reb))+geom_point()+geom_smooth(method=lm)
# Hypothesis test for significant correlation
# H0=no correlation, H1=correlation!=0
ctGp<-cor.test(merged_data$gp, merged_data$FGM)
ctGp$p.value
#Interpretation: p < 0.05 so we can reject the null hypothesis that the correlation is 0
ctAst<-cor.test(merged_data$ast, merged_data$FGM)
ctAst$p.value
#Interpretation: p < 0.05 so we can reject the null hypothesis that the correlation is 0
ctReb<-cor.test(merged_data$reb, merged_data$FGM)
ctReb$p.value
#Interpretation: p < 0.05 so we can reject the null hypothesis that the correlation is 0

#3)Do Americans or Non-Americans tend to have higher field goals made per game?
library(dplyr)
#Make a new column to show whether or not the player is American
merged_data$american<-ifelse(merged_data$country=="USA","American","Non-American")
#Group by the american column
americanGroup<-group_by(merged_data,american)
#Plot the average field goals made for american and non-american
boxplot(FGM~american, data=americanGroup,xlab="Nationality",ylab="Average Field Goals Made",col=c("powderblue","mistyrose"))
#H0 = means are equal
american<-merged_data[merged_data$american=="American","FGM"]
nonamerican<-merged_data[merged_data$american=="Non-American","FGM"]
t.test(american,nonamerican)
#Interpretation: p = 0.9272, thus we can fail to reject
#the null hypothesis that the average FGM for 
#Americans and Non-Americans are equal



















