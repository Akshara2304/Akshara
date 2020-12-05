library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(gridExtra)
library(extrafont)
library(RColorBrewer)
#setting the working directory
setwd("C:\\Users\\Admin\\Desktop\\FILES\\AKPROJECTS\\ELECINDPRJ")
#Structure of the data
national_election = read.table("C:\\Users\\Admin\\Desktop\\FILES\\AKPROJECTS\\ELECINDPRJ\\INDELEC.txt",sep="\t", header=TRUE)
national_election
# looking the structure of data
str(national_election)

# #In the election data set we have state name(st_name), constituency 
# name(pc_name), total electorates(electors), votes polled(totvotpoll),
# cadidate name(cand_name) etc.

# Adding new variables
# 
# Before we go into any analysis we are going to add some new 
# variables in the data. We are going to add a variable name 
# proportion which gives us the proportion of vote a candidate get. 
# the another variable is booleean variable which store the information 
# about the candidate's winning or loosing.

#Making a variable proportion to reflect the vote proportion of each candidate
national_election$proportion = national_election$totvotpoll / national_election$electors
head(national_election)
#making a variable with the winning vote--using new data winner_vote
national_election.winner_vote <- national_election %>%
  group_by(st_name, year,pc_no) %>%
  summarise(win_vote = max(totvotpoll))
#making the left join to the orignal data
Nat_election <- 
  merge(national_election,
        national_election.winner_vote,
        by = c('st_name','year','pc_no'),
        x.all = TRUE )
head(Nat_election)
#making our won variable to reflect the winning status of the candidate
Nat_election$won = ifelse(Nat_election$totvotpoll == Nat_election$win_vote, TRUE, FALSE)
summary(Nat_election)
#Cleaning the data

# Above we can see that in data we can see that the minimum winning vote is 0.
# which cant be true.These may not be the wrong values, because manier times
# candidate was cancelled after suspection of booth capturing, manier times result Whithheal By Courts etc.
# These observation are very few(5 to be exact) ,but this can effect out analysis so we have to remove these from our data set as a outliers.
#filtering the outliers
Nat_election <- subset(Nat_election, win_vote != 0)
sprintf('Nation election data has %s Observation after filtering out Outliars.',NROW(Nat_election))
#sprintf-A wrapper for the C function sprintf, that returns a character vector containing a formatted combination of text and variable values
#updated summary
summary(Nat_election$win_vote)
# Univariate Plots Section
#converting year variable into type factor
Nat_election$year <- as.factor(Nat_election$year)
head(Nat_election)
#plotting the no of candidates contested each year in election
ggplot(Nat_election, aes(x=year)) + 
  geom_histogram(stat = 'count',fill = '#C51162',color="black") +
  theme(axis.text.x=element_text(angle=90, size=9)) +
  coord_flip() + 
  theme_minimal() + 
  scale_y_continuous(breaks=seq(0, 15000, 1000))+
  ggtitle("No of Contestant each year (National)")
# Here we can see, there is a consistent increase in number of contestant
# participating in the elction and goes up to 14000 in 1996.But after that it comes to 
# less than 5000 in 1998 and it decreased even further in 1999.
# 
# The increase in 1996 could be because so many independent cadidate contested 
# as it was the first election after india became an open economy which
# give indian citzen too many opportunities and contesting election was one of that.
# 2.Number of contestant based on gender.  
#plotting the no of candidates contested by gender
ggplot(subset(Nat_election,cand_sex != 'NULL'), aes(x = cand_sex)) + 
  geom_bar(stat = 'count',fill = '#C51162',color = "black") +
  geom_text(stat = "count", 
            color = "black", 
            vjust = -0.1, 
            size = 3, 
            aes(y = ..count.., label = ..count..)) + 
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  theme_minimal() +
  ggtitle("Contestant by Gender (National)")
# We can see how men easily out numbered the women in politics.womens 
# had contested almost less than 5 percent as compared to mens. The other 
# sex(or LGBT) has a long way to go in the indian politics as far as the numbers are concern.
# It will be more intresting to investigate how many women out of the few 
# who contested did well in the elections. 

## 3.Number of contestant based on State.


#plots the histogram to see the no of contestant from each state
ggplot(Nat_election, aes(x = reorder(st_name, st_name, function(x) length(x)))) + 
  geom_histogram(stat = 'count',fill = '#C51162',color = "black") +
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  coord_flip() + 
  theme_minimal() +
  xlab("States") + 
  ylab('candidates count') +
  ggtitle("No of Contestant from each state (National)")
# The above chart has no surprises as the states with densely populated state Bihar has highest number of candidates. The second highest in population is Andhra Pradesh ,
# The data may look surprising but not too much for an Indian who is aware of 
# political system of the conuntry.
# 
# **Bihar** and **Uttar pradesh** have been for a long time a 
# hub for political revolution. which gives a mojority of polititians to the country.
# 


#First lets see the summary of total vote polled.

summary(Nat_election$totvotpoll)
summary(Nat_election$proportion)

# In the totvopoll we can see that the median is very small
# which gives us a rough idea that most of the candidate got a very few votes. 
# This is also reflecting in the proportion summary the median proportion is 
# 0.0066 percent which is insanely small as compared to the max value or the mean. 
# More than 75 percent of the candidate didn't even get a 10 percent of vote share.

# plot to see the distribution of votes polled
ggplot(Nat_election, aes(x = totvotpoll)) + 
  geom_bar(stat = 'count',fill = '#C51162',color = "#C51162") +
  theme(axis.text.x = element_text(angle = 90, size = 9)) + 
  theme_minimal() + 
  ggtitle("Distribution for votes polled (National)") + xlab("Votes polled")
# The data is highly skewed as most of the candidates lost due to few no of votes. 
# It will be good to see the data in log10 transformation.'
# plot to see the distribution of votes shared
ggplot(Nat_election, aes(x = totvotpoll)) + 
  geom_bar(stat = 'count',fill = '#C51162',color = "#C51162") +
  theme(axis.text.x = element_text(angle = 90, size = 9)) + scale_x_log10() + theme_minimal() + 
  ggtitle("Distribution for votes polled (National)") + xlab("Votes polled(log10)")
#log10 transformation gives a better picture of data. We almost got a normal distribution.
# 5. Which state loves women more
#filtering out the winning candidates
state_women_won = subset(Nat_election,won == TRUE & cand_sex == 'F')
#plotting winning candidates by states
ggplot(state_women_won, 
       aes(x = reorder(st_name, st_name, function(x) length(x)))) + 
  geom_bar(stat = 'count',fill = '#C51162',color = "white") +
  geom_text(stat = "count", 
            color = "black", 
            vjust = 0.4,
            hjust = -0.2, 
            size = 3,
            aes(y = ..count.., label = ..count..)) + 
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  theme_minimal() +
  ylab('contestant count') +
  ggtitle("Women Won From states")
#Bihar is at top followed by AP and Assam for women candidates who have won.
# 1. Proportion based on gender
#removing the observation with null values
Nat_election_gender <- subset(Nat_election,cand_sex != 'NULL')#summary total votes by gender
by(Nat_election_gender$proportion, Nat_election_gender$cand_sex, summary)
#plotting the proportion vs gender plot
ggplot(Nat_election_gender, 
       aes(x = cand_sex, y = proportion, fill = cand_sex)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0,0.01), breaks=seq(0, 0.01, .002)) +
  ggtitle('Proportion Boxplot By gender')
# From the above Boxplot and summary we can say that most women in percentage 
# has little bit better proportion of votes than men. 
# Though women candidates are few, We have to see the female contribution as compared to men a little bit closer.
#Density plot winning and loosing candidate based on gender
ggplot(subset(Nat_election,cand_sex != 'O' & cand_sex != '' & cand_sex != 'NULL'),
       aes(x = proportion, fill = won)) +
  geom_density(binwidth = 10, alpha = 0.5) + 
  scale_y_continuous(limits = c(0,10)) + 
  facet_wrap(~cand_sex) +
  ggtitle('Win/loose vs vote proportion Density chart')
# We can see that both men and women who won the election share same distribution of proportion.
# 3. Women candidate By party

#Grouping the Females candidates By party and their status
Nat_election_party_women <- Nat_election %>%
  filter(cand_sex == 'F') %>%
  group_by(partyabbre,won) %>%
  summarise(count_no = n())
#sortion the data by no. of candidates by each party
Nat_election_party_women_sorted = Nat_election_party_women[order(-Nat_election_party_women$count_no),]
#Plotting Female contestant vs Political Party
P1 <- ggplot(head(subset(Nat_election_party_women_sorted,won == FALSE),10),
             aes(x = reorder(partyabbre,count_no),y = count_no)) + 
  geom_histogram(stat = 'identity',fill = '#C51162') +
  geom_text(stat = "identity", 
            color = "white", 
            vjust = 0.35, 
            hjust = 1, 
            size = 3, 
            aes(y = count_no, label = count_no)) +
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  coord_flip() + 
  theme_minimal() +
  xlab("Political Parties") + 
  ylab("Female candidates count") +
  ggtitle("No. of Female Cadidates by political parties who looses")
P1

P2 <- ggplot(head(subset(Nat_election_party_women_sorted,won == TRUE),10),
             aes(x = reorder(partyabbre,count_no),y = count_no)) + 
  geom_histogram(stat = 'identity',fill = '#C51162') +
  geom_text(stat = "identity",
            color = "white",
            vjust = 0.35,
            hjust = 1.5,
            size = 3,
            aes(y = count_no, label = count_no)) +
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  coord_flip() + 
  theme_minimal() +
  scale_y_continuous(limits = c(0,200)) +
  xlab("Political Parties") + 
  ylab("Female candidates count") +
  ggtitle("No. of Female Cadidates by political parties who Won")
grid.arrange(P1, P2)
# 3. Male candidate By party

# Lets see which party fielded more women candidate through out the years and
# how being in a  party effect the winning of a men candidate
#Grouping the Male candidates By party and their status
Nat_election_party_men <- Nat_election %>%
  filter(cand_sex == 'M') %>%
  group_by(partyabbre,won) %>%
  summarise(count_no = n())
#sortion the data by no. of candidates by each party
Nat_election_party_men_sorted = Nat_election_party_men[order(-Nat_election_party_men$count_no),]

# 4.Rise And Fall Of Women.

# Here we are going to see how many female cadidate make it to the parliament in
# the election.



#filtering out the winning women candidates
Women_won <- subset(Nat_election, cand_sex == 'F' & won == TRUE)
ggplot(Women_won, aes(x = year)) + 
  geom_bar(stat = 'count',fill = '#C51162',color = "black") +
  geom_text(stat = "count",
            color = "black",
            vjust = -0.1,
            size = 3,
            aes(y = ..count.., label = ..count..)) + 
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  theme_minimal() +
  xlab('Election Years') + 
  ylab('candidates count') +
  ggtitle("winning Women candidates by year")


# As happy to see a slow but yet progressive trend for women taking more seats in parliament through out the years.

#PLotting Male Contestant vs State
P1 <- ggplot(head(subset(Nat_election_party_men_sorted,won == FALSE),10),
             aes(x = reorder(partyabbre,count_no),y = count_no)) + 
  geom_histogram(stat = 'identity',fill = '#C51162') +
  geom_text(stat = "identity", 
            color =  "black", 
            vjust = 0.35, 
            hjust = 0,
            size = 3,
            aes(y = count_no, label = count_no)) +
  theme(axis.text.x = element_text(angle = 90, size = 9)) + 
  scale_y_continuous(limits = c(0,43000)) +
  coord_flip() + theme_minimal() +
  xlab("Political Parties") + 
  ylab("Male candidates count") +
  ggtitle("No. of Male Cadidates by political parties who looses")
P2 <- ggplot(head(subset(Nat_election_party_men_sorted,won == TRUE),10),
             aes(x = reorder(partyabbre,count_no), y = count_no)) + 
  geom_histogram(stat = 'identity',fill = '#C51162') + 
  geom_text(stat = "identity",
            color = "black",
            vjust = 0.35,
            hjust = 0,
            size = 3,
            aes(y = count_no, label = count_no)) +
  theme(axis.text.x = element_text(angle = 90, size = 9)) + 
  scale_y_continuous(limits = c(0,1700)) +
  coord_flip() + theme_minimal() +
  xlab("Political Parties") +
  ylab("Male candidates count") +
  ggtitle("No. of Male Cadidates by political parties who Won") 

grid.arrange(P1, P2)
# 5.Rise and fall of women's charisma

# Here we are going to see how women candiate's vote proportion varies through out the years.


#plotting the density line chart of women vote share though out the years
ggplot(Women_won, aes(x = proportion,color = year)) + 
  geom_density(position = "dodge") + 
  ggtitle('Vote proportion density chart for women candidates')


# 2014 may be the year when maximum no of womens won the 
# election but their vote share decreases than the previous elections. 
# 6. Respond to Independent's Call

Here we see the the performance of independent candidate through out years.

```{r echo=FALSE,warning=FALSE, Bivariate_Plots_ind}
#getting the independent candidates
independent_cand <- subset(Nat_election,partyabbre == 'IND' & won == TRUE)
ggplot(independent_cand, aes(x = year)) + 
  geom_bar(stat = 'count',fill = '#C51162',color = "black") +
  geom_text(stat = "count",
            color = "black",
            vjust = -0.1,
            size = 3,
            aes(y = ..count.., label = ..count..)) + 
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  theme_minimal() +
  xlab('Election Years') + 
  ylab('candidates count') +
  ggtitle("winning Independent candidates by year")
# 6. Respond to Independent's Call

# Here we see the the performance of independent candidate through out years.


#getting the independent candidates
independent_cand <- subset(Nat_election,partyabbre == 'IND' & won == TRUE)
ggplot(independent_cand, aes(x = year)) + 
  geom_bar(stat = 'count',fill = '#C51162',color = "black") +
  geom_text(stat = "count",
            color = "black",
            vjust = -0.1,
            size = 3,
            aes(y = ..count.., label = ..count..)) + 
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  theme_minimal() +
  xlab('Election Years') + 
  ylab('candidates count') +
  ggtitle("winning Independent candidates by year")
# According to the above graph 1991 and 2014 were the worst years for 
# Independent candidates. It is followed by the 2004 election where 
# only 5 candidates won.

# To conclude this we can say that Elections in india is
# a tough job for an Independent candidate rather than a candidate with a 
# political party in his/her back.

# 7. States concious of democracy
# 
# Here we see the votepolled out of total electorats in each state
# 
#summing the  votes and electors by grouping them by state
state_grouped <- Nat_election %>%
  group_by(st_name) %>% 
  summarise(polled_vote = sum(totvotpoll),
            total_vote = sum(unique(electors)))
#making a proportion variable
state_grouped$proportion <- state_grouped$polled_vote/state_grouped$total_vote
#plotting the voting percentage by states 
ggplot(state_grouped, 
       aes(x = reorder(st_name,proportion),y = proportion,width=0.8)) + 
  geom_histogram(stat = 'identity',fill = '#C51162',color = 'black') + 
  scale_y_continuous(labels = percent, limits = c(0,1)) + 
  geom_text(stat = "identity",
            color = "black",
            vjust = 0.35,
            hjust = -0.2,
            size = 3,
            aes(y = proportion, 
                label = paste(round(proportion, 3)*100, "%", sep = ""))) + 
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  coord_flip() + 
  theme_minimal() +
  xlab("States") + 
  ylab("Percentage count of polled votes") +
  ggtitle("People who voted in each state")
#Filtering out the winning candidates
won_candidate_data <- subset(Nat_election,won == TRUE)
ggplot(won_candidate_data, 
       aes(x = proportion,y = electors)) + geom_jitter(alpha = 1/10) +
  scale_x_continuous(labels = percent,limits = c(0,0.8),breaks = seq(0,0.8,0.1)) +
  scale_y_continuous(labels = comma) + 
  xlab("votes percentage") + 
  ylab("total electors") + 
  ggtitle("electors vs Vote proportion of winning candidates")
P1 <- ggplot(subset(won_candidate_data,year == '1977'), 
             aes(x = proportion,y = electors)) + 
  geom_jitter(alpha = 1/10) +
  scale_x_continuous(labels = percent,limits = c(0,0.8),breaks = seq(0,0.8,0.2)) +
  scale_y_continuous(labels = comma) +
  xlab("votes percentage") +
  ylab("total electors")
P2 <- ggplot(subset(won_candidate_data,year == '2014'), aes(x = proportion,y = electors)) + 
  geom_jitter(alpha = 1/10) +
  scale_x_continuous(labels = percent,limits = c(0,0.8),breaks = seq(0,0.8,0.2)) +
  scale_y_continuous(labels = comma) + 
  xlab("votes percentage")
grid.arrange(P1,P2,ncol=2,top ="electors vs Vote share(%) winning candidates")

#filtering out the winning candidates and excluding candidate with no cast type
won_candidates = subset(Nat_election,won == TRUE & pc_type != '') 
#little bit more cleaning
won_candidates$pc_type[won_candidates$pc_type == 'SC '] <- 'SC'
won_candidates$pc_type <- factor(won_candidates$pc_type) 
#summary of the winning candidates by cast type
summary(won_candidates$pc_type)

#plotting cast vs count through out the yars
ggplot(won_candidates, aes(x = pc_type)) + 
  geom_bar(stat = 'count',fill = '#F44336',color = "white") +
  geom_text(stat = "count", 
            color = "white",
            vjust = 0.4,
            hjust = 1.1, 
            size = 3,
            aes(y = ..count.., label = ..count..)) +
  coord_flip()+
  theme(axis.text.x=element_text(angle = 90, size = 9)) +
  theme_minimal() + 
  ylab('contestant count') +
  xlab("Casts") +
  facet_wrap(~year) +
  ggtitle("Reserve Seats for different casts")

# # Bivariant Analysis
# 
# # Features relations observation and Surprices With that
# 
# * Though the women's candidate are lower in numbers but womens secured a good meadia in proportion then men, more women (in %) get good proportion of vote then men.
# 
# * There is a slow increase in women winning candidate through out the year but surprizingly their vote share drops down in the later elections.
# 
# * Independent candidates are may be the highest in contesting the elecctions but they are most likely ones to loose the election. On the other hand only few political parties gets the majority of seats. 
# 
# * The top states which are concious of democracy are either union teritories or the states which are dealing with some kind of a militant insurgency in their state.  
# Multivariate Plots Section

# 1. Electors vs proportion by year

#plotting the chart between Electors and vote polled percentage
ggplot(won_candidate_data, 
       aes(x = proportion,y = electors,color = year))+ 
  geom_jitter() + 
  scale_color_brewer(type = 'div')+
  scale_x_continuous(labels = percent,limits = c(0,0.8),breaks = seq(0,0.8,0.1)) +
  scale_y_continuous(labels = comma,limits = c(0,3000000),breaks = seq(0,3000000,500000)) +
  xlab("votes percentage") + 
  ylab("total electors") + 
  ggtitle("electors vs Vote proportion of winning candidates in years")
# 2.Electors vs Vote share By Gender
#filtering out the women who won the election
women_won <- subset(won_candidate_data,cand_sex == 'F')
#plooting the chart between Electors and vote polled percentage
ggplot(won_candidate_data, aes(x = electors,y = proportion,color = cand_sex))+ 
  geom_smooth(se = FALSE) +
  scale_y_continuous(labels = percent,limits = c(0,0.8),breaks = seq(0,0.8,0.1)) +
  scale_x_continuous(labels = comma,breaks = seq(0,4000000,500000)) + 
  ylab("votes percentage") +
  xlab("total electors") + 
  ggtitle("electors vs Vote proportion line chart based on gender")
ggplot(won_candidate_data, aes(x = electors,y = proportion,color = year))+ 
  geom_smooth(se = FALSE) +
  scale_y_continuous(labels = percent,limits = c(0,0.8),breaks = seq(0,0.8,0.1)) +
  scale_x_continuous(labels = comma) +
  xlab("votes percentage") + 
  ylab("total electors") +
  ggtitle("electors vs Vote proportion line chart of women candidate based on years")
# 3.Top Political Parties

#Now we are going to see how the vote share varies in top political parties through out the year.
#grouping the winning candidates by party
Nat_election_party <- won_candidate_data %>%
  group_by(partyabbre) %>%
  summarise(cand_count = n())
#soritng the party by candidates
Nat_election_party <- Nat_election_party[order(-Nat_election_party$cand_count),]
#extracting the top 5 party
top_party_name <- head(Nat_election_party,10)
#getting full top parties data
top_parties <- subset(won_candidate_data,partyabbre %in%  top_party_name$partyabbre)
top_parties$partyabbre <- factor(top_parties$partyabbre)
#Plotting top parties vs no of candidates 
ggplot(top_parties, aes(x = reorder(partyabbre, partyabbre, function(x) length(x)))) + 
  geom_histogram(stat = 'count',fill = '#C51162') +
  geom_text(stat = "count",
            color = "white",
            vjust = 0.35, 
            hjust = 1.2, 
            size = 5,
            aes(y = ..count.., label = ..count..)) +
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  coord_flip() + 
  theme_minimal() +
  xlab("Political Parties") + 
  ylab("candidates count") +
  ggtitle("Top Parties Winning the elections")
# We can see that out of top ten parties two parties have the largest share which are the **BJP** and **INC**.
#getting summary of parties by years
by(top_parties$partyabbre,top_parties$year,summary)
#plotting stacked bar graph with top parties by year
ggplot(data = top_parties, aes(x = year, fill = partyabbre)) +
  geom_bar(stat = 'count',color = "black") + 
  coord_flip() + 
  xlab('Years') + 
  ylab("Parliament seats won by political parties") + 
  ggtitle('Rise and Fall of top parties')

# 4. Independents Vote share analysis
#getting The independent candidates who won
independent_won <- subset(won_candidate_data, partyabbre == 'IND')
#womens growth through out election years
ggplot(data = independent_won, aes(x = proportion, y = electors,color = cand_sex)) + 
  geom_jitter() +
  scale_x_continuous(labels = percent,limits = c(0,0.8),breaks = seq(0,0.8,0.2)) +
  scale_y_continuous(labels = comma,limits = c(0,2000000),breaks = seq(0,2000000,500000)) +
  xlab('vote share (%)') + 
  ylab("Electors") + 
  ggtitle('Independent candidate vote share vs total electors') +
  facet_wrap(~year)
# 5.Top Parties Performance

# In this we are going to look at the performance of top three parties which we found above. They are:
#   
#   * India National Congress (INC)
# * BHartiya Janta Party (BJP)
# * Communist Party Of India-Marxist (CPM)


#filtering the top parties
Top_Parties <- subset(won_candidate_data, 
                      partyabbre == 'INC' |   partyabbre == 'BJP' | partyabbre == 'CPM')
#plotting scatter plot
ggplot(data = Top_Parties, aes(x = proportion, y = electors,color = partyabbre)) + 
  geom_jitter(alpha = 1/2) + 
  xlab('vote share (%)') +
  ylab("Electors") +
  scale_x_continuous(labels = percent,breaks = seq(0,1,0.1)) +
  scale_y_continuous(labels = comma,breaks = seq(0,3000000,500000)) +
  ggtitle('Proformance of top political parties')


# INC may be the longest running party to woo the voters but from the above graph we can say that BJP has an edge in higher electors constituncies.
#filtering the top parties
INC <- subset(won_candidate_data, partyabbre == 'INC')
BJP <- subset(won_candidate_data, partyabbre == 'BJP')
#plotting performance of INC
ggplot(data = INC, aes(x = proportion, y = electors)) + 
  geom_jitter(alpha = 1/2) +
  xlab('vote share (%)') + 
  ylab("Electors") + 
  scale_x_continuous(labels = percent,breaks = seq(0,1,0.1)) +
  scale_y_continuous(labels = comma,breaks = seq(0,3000000,500000)) +
  ggtitle('Proformance of INC') + 
  facet_wrap(~year)
#plotting performance of BJP
ggplot(data = BJP, aes(x = proportion, y = electors)) +
  geom_jitter(alpha = 1/2) + 
  xlab('vote share (%)') +
  ylab("Electors") +
  scale_x_continuous(labels = percent,breaks = seq(0,1,0.1)) +
  scale_y_continuous(labels = comma,breaks = seq(0,3000000,500000)) +
  ggtitle('Proformance of BJP') + 
  facet_wrap(~year)
#Here we are looking at the performance of top two political parties having a very opposite performance chart, One is BJP who went from nothing to majority in the parliment, and the Other is INC who went from Allmost all the seats to just 44 seats in the parliament.

# Party winning candidates through out the years by gender
#plotting the bar garph of INC by gender
ggplot(INC, aes(x = year, fill = cand_sex) ) +
  geom_histogram(stat = "count", position = "dodge", color = "black") +
  geom_text(stat = 'count',
            data = subset(INC, cand_sex == "M"), 
            color = "white",
            aes(y = ..count.., label = ..count..), 
            size = 3, 
            vjust = -0.5, 
            hjust = 1.3) +
  geom_text(stat = 'count',
            data = subset(INC,cand_sex == "F"),
            color = "black",
            aes(y = ..count.., label = ..count..), 
            size = 3,
            vjust = 1.5, 
            hjust = -0.3) +
  theme_minimal() +
  coord_flip() + 
  scale_fill_manual(values=c("goldenrod1","dodgerblue4" ), name="candidate Sex") +
  xlab("Years") + 
  ylab("Candidates") +
  ggtitle("Candidates by gender in INC")
#PLot of BJP
ggplot(BJP, aes(x = year, fill = cand_sex) ) +
  geom_histogram(stat = "count", position = "dodge", color = "black") +
  geom_text(stat = 'count',
            data = subset(BJP, cand_sex == "M"), 
            color = "white",
            aes(y = ..count.., label = ..count..), 
            size = 3,
            vjust = -0.5, 
            hjust = 1.3) +
  geom_text(stat = 'count',
            data = subset(BJP, cand_sex == "F"),
            color = "black",
            aes(y = ..count.., label = ..count..), 
            size=3,
            vjust = 1.5, 
            hjust=-0.3) +
  theme_minimal() +
  coord_flip() + 
  scale_fill_manual(values = c("goldenrod1","dodgerblue4" ), name="candidate Sex") +
  xlab("Years") + 
  ylab("Candidates") +
  ggtitle("Candidates by gender in BJP")
# FOr both the parties women share in each year is always less than 15 percent and in most of the cases its less than 10%. but we see a overall increase in women cadidates winnning in the recent years. 
# 
# For BJP 2014 is the first time that 30 women(highest ever in BJP) won the election.
# 
# Final Plots and Summary

### 1. Cast Divison In Parliament
# 
# India is a cast Based society, It thousands of casts which play an important role in election, there are some reserved seats in the parliament on which candidates from a particular cast can contest. Lets see how many cast reserve seats have been in the parliament through out the years of election.
# 
# There are two main cast who has reservation 

# **1)** Schedule cast(SC)
# **2)** Schedule cast(ST)
# 
# Every body else contest in **genral** category.
#plotting cast vs count through out the yars
ggplot(won_candidates, aes(x = pc_type)) + 
  geom_bar(stat = 'count',fill = '#F44336',color = "white") +
  geom_text(stat = "count", 
            color = "white",
            vjust = 0.4,
            hjust = 1.1, 
            size = 3,
            aes(y = ..count.., label = ..count..)) +
  coord_flip()+
  theme(axis.text.x=element_text(angle = 90, size = 9)) +
  theme_minimal() + 
  ylab('contestant count') +
  xlab("Casts") +
  facet_wrap(~year) +
  ggtitle("Reserve Seats for different casts")

# 2. States concious of democracy

# We will see in this graph how much citizen of each state take election seriously or went to poll their votes.


#plotting the voting percentage by states 
ggplot(state_grouped, 
       aes(x = reorder(st_name,proportion),y = proportion,width=0.8)) + 
  geom_histogram(stat = 'identity',fill = '#C51162',color = 'black') + 
  scale_y_continuous(labels = percent, limits = c(0,1)) + 
  geom_text(stat = "identity",
            color = "black",
            vjust = 0.35,
            hjust = -0.2,
            size = 3,
            aes(y = proportion, 
                label = paste(round(proportion, 3)*100, "%", sep = ""))) + 
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  coord_flip() + 
  theme_minimal() +
  xlab("States") + 
  ylab("Percentage count of polled votes") +
  ggtitle("People who voted in each state")
# 3. Political Shift in Parliament

#plooting stacked bar graph with top parties by year
ggplot(data = top_parties, aes(x = year, fill = partyabbre)) +
  geom_bar(stat = 'count',color = "black") + 
  coord_flip() + 
  xlab('Years') + 
  ylab("Parliament seats won by political parties") + 
  ggtitle('Rise and Fall of top parties')
# From the above chart we can say that **INC(Indian National congress)** has been the most dominating party through out the years, but we see a fascinating shift in 2014 where INC went down to 44 seats from 206 seats in 2009. And BJP which starts from 2 seats in 1984 went up to 282 seats in 2014.
# 
# But the most bitter truth of all is that only few political parities took majority of seats.
# 
# 
