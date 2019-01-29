library(reshape2)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(stats)

##renaming 
epl_table = EPL_Table_fixed

## merging 2 dataframe to Transfer spending df with league position df
merged_table = merge(x = EPL_TM1, y = epl_table, by = c("Club", "Season"), all.x = TRUE)

merged_table


write.csv(merged_table, "Merged_table.csv", quote = FALSE, row.names = FALSE)


##adding column for previous season league position
Merged_table %>%
  arrange(Club, Season) %>%
  group_by(Club) %>%
  mutate(Pre_Pos = lag(Position)) -> Merged_table1

Merged_table %>%
  arrange(Club, Season) %>%
  group_by(Club) %>%
  mutate(Pre_Pos = lag(Position)) -> Merged_table2

Merged_table1

## changing league position to factor
Merged_table1$Position = factor(Merged_table1$Position)
Merged_table1$Pre_Pos = factor(Merged_table1$Pre_Pos)

Merged_table1

Merged_table1 %>%
  mutate(PPG = Points/38) %>%
  select(Season, Club, Transfer_Expenditure, League_Spent, Goal_Difference, Points, PPG, Position, Pre_Pos) %>%
  arrange(Season, Position) -> df1998to2018

df1998to2018

## correlation between expenditure and position
cor(Merged_table2$Transfer_Expenditure, Merged_table2$Position)

## correlation between expenditure and points
cor(Merged_table2$Transfer_Expenditure, Merged_table2$Points)

## correlation between expenditure and goal difference
cor(Merged_table2$Transfer_Expenditure, Merged_table2$Goal_Difference)


## running multiple correlation position 
fit <- lm(Position ~ Transfer_Expenditure + Net_Spent, data=Merged_table2)
fit

Merged_table1 %>%
  filter(Season == 2018 ) %>%
  mutate(PPG = Points/23) %>% 
  select(Season, Club, Transfer_Expenditure, League_Spent, Goal_Difference, Points, PPG, Position, Pre_Pos) %>%
  arrange(Season, Position) -> df2018

df2018

Merged_table1 %>%
  mutate(PPG = Points/38) -> epl_df_complete
epl_df_complete

df1998to2018 %>%
  filter(as.numeric(as.character(Position)) <= 4) -> top4_20_years

top4_20_years
top4_20_years$Pre_Pos[top4_20_years$Season == 1998 & top4_20_years$Club == "Leeds United"] = epl_table$Position[epl_table$Season == "97/98" & epl_table$Club == "Leeds United"]
top4_20_years$Pre_Pos[top4_20_years$Season == 1998 & top4_20_years$Club == "Manchester United"] = epl_table$Position[epl_table$Season == "97/98" & epl_table$Club == "Manchester United"]
top4_20_years$Pre_Pos[top4_20_years$Season == 1998 & top4_20_years$Club == "Arsenal FC"] = epl_table$Position[epl_table$Season == "97/98" & epl_table$Club == "Arsenal FC"]
top4_20_years$Pre_Pos[top4_20_years$Season == 1998 & top4_20_years$Club == "Chelsea FC"] = epl_table$Position[epl_table$Season == "97/98" & epl_table$Club == "Chelsea FC"]
top4_20_years



# top4_20_years$Position = factor(top4_20_years$Position)
# top4_20_years$Pre_Pos = factor(top4_20_years$Pre_Pos)

# ##graphing point tally of the champions
top4_20_years %>%
  filter(Position == 1, Season <2018)%>%
  ggplot(aes(x = Season, y= Points)) + 
  geom_line(stat = "identity", color = "blue" ) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Points Tally For Champions (1998 - 2017)") -> Point_tally_champions_graph
Point_tally_champions_graph
# ##graphing point tally of the top 4 teams by position
# top4_20_years %>%
#   ggplot(aes(x = Season, y= Points)) + geom_line(aes(color = Position), 
#                                                  stat = "identity", show.legend = TRUE ) + facet_wrap(.~ Position) 


##graphing point tally of the top teams 1998-2007
top4_20_years %>%
  filter(Season <2008) %>%
  ggplot(aes(x = Season, y= Points)) + geom_line(aes(color = Position), 
                                                 stat = "identity", show.legend = TRUE ) + 
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Points Tally For EPL Top 4 (1998 - 2007)")+ 
  scale_x_continuous(breaks = c(1998,2000, 2002, 2004, 2006)) -> Point_tally_98_07
Point_tally_98_07

##graphing point tally of the top teams 2008-2017
top4_20_years %>%
  filter(Season > 2007 & Season < 2018) %>%
  ggplot(aes(x = Season, y= Points)) + geom_line(aes(color = Position), 
                                                 stat = "identity", show.legend = TRUE ) + 
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Points Tally For EPL Top 4 (2008 - 2017)") + 
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) ->Point_tally_08_17
Point_tally_08_17

##graphing point tally of the top teams 1998-2017
top4_20_years %>%
  filter(Season <2018) %>%
  ggplot(aes(x = Season, y= Points)) + geom_line(aes(color = Position), 
                                                 stat = "identity", show.legend = TRUE ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Points Tally For EPL Top 4 (1998 - 2017)")+ 
  scale_x_continuous(breaks = c(2001, 2005, 2009, 2013, 2017)) ->Point_tally_top4_20y
Point_tally_top4_20y


## Graphing the increase of transfer spending by EPL clubs
df1998to2018 %>%
  select(Season, League_Spent)%>%
  group_by(Season) %>%
  summarize(Spent = mean(League_Spent)) %>%
  ggplot(aes(x= Season, y = Spent)) + geom_bar(aes(fill = Spent),stat= "identity") +geom_line(stat = "identity", show.legend = TRUE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("EPL Transfer Spending (1998 - 2017)") + xlab(label = "Season") + ylab(label = "£ (Millions)") +
  scale_x_continuous(breaks = c(2001, 2005, 2009, 2013, 2017)) ->League_spending_graph

League_spending_graph

## mean transfer expenditure by league 1998-2012
df1998to2018 %>%
  group_by(Season) %>%
  summarize(sum(Transfer_Expenditure)) %>%
  filter(Season <= 2013) %>%
  summarise_all(funs(mean)) -> mean_expenditure_98_13
mean_expenditure_98_13

## mean transfer expenditure by league 2014-2018
df1998to2018 %>%
  group_by(Season) %>%
  summarize(sum(Transfer_Expenditure)) %>%
  filter(Season >2013) %>%
  summarise_all(funs(mean)) -> mean_expenditure_14_18
mean_expenditure_14_18

## making expenditure df
df1998to2018 %>%
  mutate(perc_of_league_spent = Transfer_Expenditure/League_Spent) %>%
  group_by(Club, Season) %>%
  select(perc_of_league_spent, Transfer_Expenditure, Goal_Difference, Points, Position) -> expenditiure_df

expenditiure_df


## graphing the league position of clubs vs their transfer expenditure (outliers 2003(40%) and 2004(33%))
expenditiure_df %>%
  filter(Season<2007) %>%
  group_by(Season) %>%
  ggplot(aes(x = perc_of_league_spent*100, y = as.numeric(as.character(Position)))) +
  geom_point(aes(color = Goal_Difference)) + facet_wrap(. ~ Season) +
  theme(legend.position="bottom", legend.text = element_text(size = 6), 
        legend.title = element_text(size = 10), legend.key.size = unit(.5, "cm")) + 
  xlab(label = "Club transfer expenditure as % of total league expenditure") + 
  ylab(label = "League Position")+ scale_y_reverse( lim=c(20,1)) +
  scale_x_continuous(limits = c(0, 20), breaks = c(0, 10, 20)) +
  ggtitle("Expenditure vs League Position (1998 - 2006)") -> spending_vs_position98_06

spending_vs_position98_06
# Exception 2003 and 2004 (.4 and .33)

expenditiure_df %>%
  filter(Season > 2006 & Season < 2013) %>%
  group_by(Season) %>%
  ggplot(aes(x = perc_of_league_spent*100, y = as.numeric(as.character(Position)))) +
  geom_point(aes(color = Goal_Difference)) + facet_wrap(. ~ Season) +
  theme(legend.position="bottom", legend.text = element_text(size = 6), 
        legend.title = element_text(size = 10), legend.key.size = unit(.5, "cm")) + 
  xlab(label = "Club transfer expenditure as % of total league expenditure") + 
  ylab(label = "League Position")+
  ggtitle("Expenditure vs League Position (2007 - 2012)") +
  scale_y_reverse( lim=c(20,1)) +
  scale_x_continuous(limits = c(0, 20), breaks = c(0, 10, 20))-> spending_vs_position07_12

spending_vs_position07_12
# Outlier 2009 and 2010 (25 and 26 )

expenditiure_df %>%
  filter(Season > 2012) %>%
  group_by(Season) %>%
  ggplot(aes(x = perc_of_league_spent*100, y = as.numeric(as.character(Position)))) +
  geom_point(aes(color = Goal_Difference))  + 
  facet_wrap(. ~ Season) +
  theme(legend.position="bottom", legend.text = element_text(size = 6), 
        legend.title = element_text(size = 10), legend.key.size = unit(.5, "cm")) + 
  xlab(label = "Club transfer expenditure as % of total league expenditure") + 
  ylab(label = "League Position")+
  ggtitle("Expenditure vs League Position (2013 - 2018)") +
  scale_y_reverse( lim=c(20,1))+
  scale_x_continuous(breaks = c(0, 7.5, 15))-> spending_vs_position13_18

spending_vs_position13_18

##linear model fit line for season to see link between spending and league position
spending_vs_position98_06+ geom_smooth(method = 'lm') -> spending_vs_position98_06_lm
spending_vs_position07_12+ geom_smooth(method = 'lm') -> spending_vs_position07_12_lm
spending_vs_position13_18+ geom_smooth(method = 'lm') -> spending_vs_position13_18_lm

spending_vs_position98_06_lm
spending_vs_position07_12_lm
spending_vs_position13_18_lm

Merged_table2 %>%
  mutate(PPG = Points/38) -> epl_df_complete


## mean data for clubs for different time spans

## 1998-2018
epl_df_complete %>%
  group_by(Club) %>%
  summarise_all(funs(mean)) %>%
  select(Club, Season, Net_Spent, Transfer_Expenditure, Transfer_Income, 
         Goal_Difference, Points, Position, PPG) %>%
  arrange(Position) -> mean_20y
mean_20y

## 2008-2018
epl_df_complete %>%
  group_by(Club) %>%
  filter(Season< 2008) %>%
  summarise_all(funs(mean)) %>%
  select(Club, Season, Net_Spent, Transfer_Expenditure, Transfer_Income, 
         Goal_Difference, Points, Position, PPG) %>%
  arrange(Position) -> mean_98_07
mean_98_07

## 2008-2018
epl_df_complete %>%
  group_by(Club) %>%
  filter(Season > 2007) %>%
  summarise_all(funs(mean)) %>%
  select(Club, Season, Net_Spent, Transfer_Expenditure, Transfer_Income, 
         Goal_Difference, Points, Position, PPG) %>%
  arrange(Position) -> mean_10y
mean_10y

## 2013-2018
epl_df_complete %>%
  group_by(Club) %>%
  filter(Season >= 2013) %>%
  summarise_all(funs(mean)) %>%
  select(Club, Season, Net_Spent, Transfer_Expenditure, Transfer_Income, 
         Goal_Difference, Points, Position, PPG) %>%
  arrange(Position) -> mean_5y
mean_5y


## season averages for data

## mean per season
epl_df_complete %>%
  group_by(Season) %>%
  select(Season, Net_Spent, Transfer_Expenditure) %>%
  summarise_all(funs(mean)) %>%
  arrange(Season) -> mean_by_season
mean_by_season

## median per season
epl_df_complete %>%
  group_by(Season) %>%
  select(Season, Net_Spent, Transfer_Expenditure) %>%
  summarise_all(funs(median)) %>%
  arrange(Season) -> median_by_season
median_by_season

## mean of champions
epl_df_complete %>%
  group_by(Season) %>%
  filter(Position == 1)%>%
  mutate(League_avg = League_Spent/20)%>%
  select(Season, Net_Spent, Transfer_Expenditure, League_avg) -> first_placed_clubs
first_placed_clubs

## merging df to compare mean and median values per season
merged_means = merge(x = mean_by_season, y = median_by_season, by = "Season", quote = FALSE, all = TRUE )
merged_means %>%
  rename(Mean_Net_Spent = Net_Spent.x, Mean_Expenditure = Transfer_Expenditure.x, 
         Median_Net_Spent = Net_Spent.y, Median_Expenditure = Transfer_Expenditure.y) ->merged_means
merged_means
merged_means %>%
  melt(Season)

## plotting spending for champions vs avg

expenditure_winner_vs_avg = ggplot(first_placed_clubs, aes(x = Season, y  = League_avg)) + 
  geom_line(color = "blue") + geom_line(aes(y = Transfer_Expenditure), color = "red") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Expenditure: Winners vs League Avg") + 
  xlab(label = "Season") + ylab(label = "£ (Millions)")
expenditure_winner_vs_avg

## top spending teams 

## over 20 years
epl_df_complete %>%
  group_by(Club) %>%
  select(Net_Spent, Transfer_Expenditure, Transfer_Income) %>%
  summarise(Expenditure = sum(Transfer_Expenditure)) %>%
  arrange(desc(Expenditure))-> club_spent_20y
club_spent_20y

## 1998 - 2007
epl_df_complete %>%
  filter(Season <2008) %>%
  group_by(Club) %>%
  select(Net_Spent, Transfer_Expenditure, Transfer_Income) %>%
  summarise(Expenditure = sum(Transfer_Expenditure)) %>%
  arrange(desc(Expenditure))-> club_spent_98_07
club_spent_98_07

## 2008-2018
epl_df_complete %>%
  filter(Season >=2008) %>%
  group_by(Club) %>%
  select(Net_Spent, Transfer_Expenditure, Transfer_Income) %>%
  summarise(Expenditure = sum(Transfer_Expenditure)) %>%
  arrange(desc(Expenditure))-> club_spent_10y
club_spent_10y

## 2013 - 2018
epl_df_complete %>%
  filter(Season >=2013) %>%
  group_by(Club) %>%
  select(Net_Spent, Transfer_Expenditure, Transfer_Income) %>%
  summarise(Expenditure = sum(Transfer_Expenditure)) %>%
  arrange(desc(Expenditure))-> club_spent_5y
club_spent_5y

## plotting money spent

## over 20 years
head(club_spent_20y, n = 8) %>%
  ggplot(aes(x = reorder(Club, -Expenditure), y = Expenditure)) + 
  geom_bar(aes(fill = Club), stat="identity") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none")+
  scale_fill_brewer(palette="Dark2")+
  ggtitle("Clubs with highest transfer expenditure (1998-2018)") + xlab(label = "Club") + 
  ylab(label = "£ (Millions)")->club_spent_20y_graph
club_spent_20y_graph

# 1998-2007
head(club_spent_98_07, n = 8) %>%
  ggplot(aes(x = reorder(Club, -Expenditure), y = Expenditure)) + 
  geom_bar(aes(fill = Club), stat="identity") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none")+
  scale_fill_brewer(palette="Dark2")+
  ggtitle("Clubs with highest transfer expenditure (1998-2007)") + xlab(label = "Club") + 
  ylab(label = "£ (Millions)")->club_spent_98_07_graph
club_spent_98_07_graph

## 2008 - 2018
head(club_spent_10y, n = 8) %>%
  ggplot(aes(x = reorder(Club, -Expenditure), y = Expenditure)) + 
  geom_bar(aes(fill = Club), stat="identity") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none")+
  scale_fill_brewer(palette="Dark2")+
  ggtitle("Clubs with highest transfer expenditure (2008-2018)") + xlab(label = "Club") + 
  ylab(label = "£ (Millions)")->club_spent_10y_graph
club_spent_10y_graph

## 2013- 2018
head(club_spent_5y, n = 8) %>%
  ggplot(aes(x = reorder(Club, -Expenditure), y = Expenditure)) + 
  geom_bar(aes(fill = Club), stat="identity") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none")+
  scale_fill_brewer(palette="Dark2")+
  ggtitle("Clubs with highest transfer expenditure (2013-2018)") + xlab(label = "Club") + 
  ylab(label = "£ (Millions)")->club_spent_5y_graph
club_spent_5y_graph

## Teams that finish first 

## over 20 years
epl_df_complete %>%
  filter(Position == 1) %>%
  select(Position) %>%
  group_by(Club) %>%
  count(Club) %>%
  arrange(desc(n))-> club_1st_20y

club_1st_20y

## 1998 - 2007
epl_df_complete %>%
  filter(Position == 1, Season <2008) %>%
  select(Position) %>%
  group_by(Club) %>%
  count(Club) %>%
  arrange(desc(n))-> club_1st_98_07

club_1st_98_07

## 2008 - 2018
epl_df_complete %>%
  filter(Position == 1, Season >= 2008) %>%
  select(Position) %>%
  group_by(Club) %>%
  count(Club) %>%
  arrange(desc(n))-> club_1st_10y

club_1st_10y

## 2013 - 2018
epl_df_complete %>%
  filter(Position == 1, Season > 2012) %>%
  select(Position) %>%
  group_by(Club) %>%
  count(Club) %>%
  arrange(desc(n))-> club_1st_5y

club_1st_5y

##plotting teams that finish top

## over 20 years
club_1st_20y %>%
  ggplot(aes(x = reorder(Club, -n), y = n)) +geom_bar(fill = "blue", stat="identity") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Clubs with most titles (1998-2018)") + xlab(label = "Club") + ylab(label = "# of Titles") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) ->club_1st_20y_graph
club_1st_20y_graph

## 1998 - 2007
club_1st_98_07 %>%
  ggplot(aes(x = reorder(Club, -n), y = n)) +geom_bar(fill = "blue", stat="identity") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Clubs with most titles (1998-2008)") + xlab(label = "Club") + ylab(label = "# of Titles") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) ->club_1st_98_07_graph
club_1st_98_07_graph

## 2008 - 2018

club_1st_10y %>%
  ggplot(aes(x = reorder(Club, -n), y = n)) + geom_bar(fill = "blue", stat="identity") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Clubs with most titles (2008-2018)") + xlab(label = "Club") + ylab(label = "# of Titles") +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4)) -> club_1st_10y_graph

club_1st_10y_graph

## 2013 - 2018
club_1st_5y %>%
  ggplot(aes(x = reorder(Club, -n), y = n)) +geom_bar(fill = "blue", stat="identity") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Clubs with most titles (2013-2018)") + xlab(label = "Club") + ylab(label = "# of Titles") +
  scale_y_continuous(breaks = c(0, 1, 2, 3)) -> club_1st_5y_graph
club_1st_5y_graph


##teams that finsh 2nd

## over 20 years
epl_df_complete %>%
  filter(Position == 2) %>%
  select(Position) %>%
  group_by(Club) %>%
  count(Club) %>%
  arrange(desc(n))-> club_2nd_20y

club_2nd_20y

## 1998 - 2007
epl_df_complete %>%
  filter(Position == 2, Season < 2008) %>%
  select(Position) %>%
  group_by(Club) %>%
  count(Club) %>%
  arrange(desc(n))-> club_2nd_98_07

club_2nd_98_07

## 2008 - 2018
epl_df_complete %>%
  filter(Position == 2, Season >= 2008) %>%
  select(Position) %>%
  group_by(Club) %>%
  count(Club) %>%
  arrange(desc(n))-> club_2nd_10y

club_2nd_10y

## 2013 - 2018
epl_df_complete %>%
  filter(Position == 2, Season > 2012) %>%
  select(Position) %>%
  group_by(Club) %>%
  count(Club) %>%
  arrange(desc(n))-> club_2nd_5y

club_2nd_5y

##merging 1st and second place data to graph together

## Championship and Runners Up count (1998-2018)
merge(x = club_1st_20y, y = club_2nd_20y, quotes = FALSE, by = "Club", all= TRUE) %>%
  rename(Champions = n.x, Runners_Up = n.y) %>%
  melt() %>%
  ggplot(aes(x = reorder(Club, -value), y = value)) + 
  geom_bar(aes(fill = variable), stat="identity", position = "dodge2") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Clubs with most titles & 2nd place finishes (1998-2018)") + xlab(label = "Club") + ylab(label = "Count") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) -> champ_sec_graph_20y
champ_sec_graph_20y

## Championship and Runners Up count (1998-2007)
merge(x = club_1st_98_07, y = club_2nd_98_07, quotes = FALSE, by = "Club", all= TRUE) %>%
  rename(Champions = n.x, Runners_Up = n.y) %>%
  melt() %>%
  ggplot(aes(x = reorder(Club, -value), y = value)) + 
  geom_bar(aes(fill = variable), stat="identity", position = "dodge2") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Clubs with most titles & 2nd place finishes (1998-2007)") + xlab(label = "Club") + ylab(label = "Count") +
  scale_y_continuous(breaks = c(0, 2, 4, 6,8)) -> champ_sec_graph_98_07
champ_sec_graph_98_07

## Championship and Runners Up count (2008-2018)
merge(x = club_1st_10y, y = club_2nd_10y, quotes = FALSE, by = "Club", all= TRUE) %>%
  rename(Champions = n.x, Runners_Up = n.y) %>%
  melt() %>%
  ggplot(aes(x = reorder(Club, -value), y = value)) + 
  geom_bar(aes(fill = variable), stat="identity", position = "dodge2") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Clubs with most titles & 2nd place finishes (2008-2018)") + xlab(label = "Club") + ylab(label = "Count") +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4)) -> champ_sec_graph_10y
champ_sec_graph_10y

## Championship and Runners Up count (2013-2018)
merge(x = club_1st_5y, y = club_2nd_5y, quotes = FALSE, by = "Club", all= TRUE) %>%
  rename(Champions = n.x, Runners_Up = n.y) %>%
  melt() %>%
  ggplot(aes(x = reorder(Club, -value), y = value)) + 
  geom_bar(aes(fill = variable), stat="identity", position = "dodge2") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Clubs with most titles & 2nd place finishes (2013-2018)") + xlab(label = "Club") + ylab(label = "Count") +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4)) -> champ_sec_graph_5y
champ_sec_graph_5y

## histogram showing distribution of Clubs spending as a fraction of total league spending. outliers taken out
Merged_table1 %>%
  group_by(Season) %>%
  mutate(Spent = mean(League_Spent)) %>%
  ggplot(aes(x = (Transfer_Expenditure/Spent)*100)) + geom_histogram() +
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_fill_brewer(palette="Dark2") + 
  xlab(label = "% of total league expenditure") + 
  ggtitle("Distribution of club spending as % of total league expenditure (1998 - 2018)") +
  scale_x_continuous(lim=c(0,20),breaks = c(0, 10, 20, 30)) -> spending_distribution_perc
spending_distribution_perc

## histogram showing distribution of Clubs spending as a fraction of total league spending
Merged_table1 %>%
  group_by(Season) %>%
  ggplot(aes(x = (Transfer_Expenditure))) + geom_histogram() +
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_fill_brewer(palette="Dark2") + 
  xlab(label = "£ (Millions)") + 
  ggtitle("Distribution of club spending per season (1998 - 2018)") +
  scale_x_continuous(lim=c(0,200)) -> spending_distribution
spending_distribution

Merged_table1%>%
  group_by(Club) %>%
  filter(Transfer_Expenditure > 200)
