library(XML)
library(dplyr)
library(tidyr)
library(textclean)


# load rda ----------------------------------------------------------------
load(file = 'remap.rda')
load(file = 'type_change.rda')


# Tournaments ------------------
url <- "http://www.fivb.org/vis2009/XmlRequest.asmx?Request=<Requests> <Request Type= 'GetBeachTournamentList' Fields='Type Season No Code Name Title StartDateQualification ENdDateMainDraw Gender NbTeamsMainDraw NbTeamsQualification'></Request></Requests>"
xml <- XML::xmlParse(url, encoding="UTF-8")
df <- XML:::xmlAttrsToDataFrame(XML::getNodeSet(xml, path ='//BeachTournament')) %>% 
  filter(NbTeamsMainDraw != 0) %>% 
  select(-Version) %>% 
  arrange(Season) %>% 
  mutate(Gender = ifelse(Gender == 0, 'M', 'W'),
         Month = substr(StartDateQualification, 6,7),
         Month = as.numeric(Month),
         StartDateQualification = as.Date(StartDateQualification),
         EndDateMainDraw = as.Date(EndDateMainDraw),
         Season = as.numeric(Season),
         NbTeamsMainDraw = as.numeric(NbTeamsMainDraw),
         NbTeamsQualification = as.numeric(NbTeamsQualification),
         Title = textclean::replace_non_ascii(Title),
         Name = textclean::replace_non_ascii(Name))

df <- remap(df, col = 'Type', schema = 'Beach Tournament Type')
save(df, file = 'tourn_df.rda')


# Matches -------------
match_url <- "http://www.fivb.org/vis2009/XmlRequest.asmx?Request=<Requests> <Request Type= 'GetBeachMatchList' Fields='Season TournamentCode NoPlayerB1 NoPlayerB2 NoPlayerA1 NoPlayerA2 TournamentType RoundName Court TournamentTitle LocalDate LocalTime TeamAFederationCode TeamAName MatchPointsA PointsTeamASet1 PointsTeamASet2 PointsTeamASet3 TeamBFederationCode TeamBName MatchPointsB PointsTeamBSet1 PointsTeamBSet2 PointsTeamBSet3 NbSpectators DurationSet1 DurationSet2 DurationSet3 No NoTournament'></Request></Requests>"
match_xml <- XML::xmlParse(match_url, encoding="UTF-8")
match_dfs <- XML:::xmlAttrsToDataFrame(XML::getNodeSet(match_xml, path ='//BeachMatch'))
#Clean the match data
match_df <- match_dfs %>% 
  mutate(TeamAName = replace_non_ascii(TeamAName),
         TeamBName = replace_non_ascii(TeamBName),
         TournamentTitle = replace_non_ascii(TournamentTitle),
         LocalDate = as.Date(LocalDate),
         Court = as.integer(Court),
         Hour = as.integer(substr(LocalTime, 1, 2)),
         Year = as.integer(substr(LocalDate,1,4)),
         DurationSet1 = as.integer(DurationSet1),
         DurationSet2 = as.integer(DurationSet2),
         DurationSet3 = as.integer(DurationSet3)) %>% 
  select(-Version)

match_df <- remap(match_df, col = 'TournamentType', schema = 'Beach Tournament Type')
#

# Team finishes -------------
player_url <- "http://www.fivb.org/vis2009/XmlRequest.asmx?Request=<Requests> <Request Type= 'GetBeachTeamList' Fields='NoPlayer1 NoPlayer2 Name Rank No NoTournament Gender'></Request></Requests>"
player_xml <- XML::xmlParse(player_url)
player_df <- XML:::xmlAttrsToDataFrame(XML::getNodeSet(player_xml, path ='//BeachTeam'))

# Attach team finishes to matches -------------------------------------------
get_gender <- df %>% select(Code, Gender)

team_finishes_pivoted <- player_df %>% pivot_longer(c(NoPlayer1:NoPlayer2))
match_df <- match_df %>% 
  pivot_longer(c(NoPlayerB1:NoPlayerA2)) %>% 
  left_join(team_finishes_pivoted %>% select(Rank, NoTournament, value)) %>% 
  filter(!is.na(Rank) & Rank != '' & PointsTeamASet1 != '' & PointsTeamBSet1 != '') %>% 
  pivot_wider(names_from = name, values_from = c('value', 'Rank')) %>% 
  select(-contains('value'), -c('Rank_NoPlayerB1', 'Rank_NoPlayerA1')) %>% 
  rename(TeamARank = Rank_NoPlayerA2,
         TeamBRank = Rank_NoPlayerB2) %>% 
  left_join(get_gender, by = c('TournamentCode' = 'Code'))
  

save(match_df, file = 'match_df.rda')
