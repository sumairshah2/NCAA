# Game logs script - pitching

#devtools::install_github("BillPetti/baseballr",force = T)
#install.packages('webshot')
#Load packages

library(baseballr)
library(webshot)
webshot::install_phantomjs()
library(tidyverse)
library(dplyr)
library(knitr)
#install.packages('kableExtra')
library(kableExtra)
library(gt)
library(lubridate)
#install.packages("downloadthis")
library(downloadthis)
#install.packages("googlesheets4")
library('googlesheets4')








ncaa_clients <- read_csv("Data/ClientsFinalx.csv") 

pitchers <- read.csv("pitchers.csv")

abel_logs <- read.csv("ab.csv") %>% select(-c(X))




#function / scrapper to get game logs for players

get_ncaa_game_logs_v2 <- function(player_id,
                                  year = 2019,
                                  type = "batting",
                                  span = 'game') {
  
  year_id <- subset(ncaa_season_id_lu, season == year, select = id)
  batting_id <- subset(ncaa_season_id_lu, season == year, select = batting_id)
  pitching_id <- subset(ncaa_season_id_lu, season == year, select = pitching_id)
  
  if (type == "batting") {
    
    batting_url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", batting_id)
    batting_payload <- xml2::read_html(batting_url)
  } else {
    
    pitching_url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", pitching_id)
    pitching_payload <- xml2::read_html(pitching_url)
  }
  
  if (span == 'game') {
    
    if (type == "batting") {
      
      payload_df <- batting_payload %>%
        rvest::html_nodes("table") %>%
        .[5] %>%
        rvest::html_table(fill = TRUE) %>%
        as.data.frame() %>%
        .[,c(1:23)]
      
      names(payload_df) <- payload_df[2,]
      
      payload_df <- payload_df[-c(1:3),]
      
      payload_df <- payload_df %>%
        mutate_at(vars(G:RBI2out), extract_numeric)
      
      if('OPP DP' %in% colnames(payload_df) == TRUE) {
        
        payload_df <- payload_df %>%
          dplyr::rename(DP = `OPP DP`)
      }
      
      cols_to_num <- c("G","R", "AB", "H", "2B", "3B", "TB", "HR", "RBI",
                       "BB", "HBP", "SF", "SH", "K", "DP", "CS", "Picked",
                       "SB", "IBB", "RBI2out")
      
      payload_df <- payload_df %>%
        dplyr::mutate_at(cols_to_num, as.numeric)
      
    } else {
      
      payload_df <- pitching_payload %>%
        rvest::html_nodes("table") %>%
        .[5] %>%
        rvest::html_table(fill = TRUE) %>%
        as.data.frame() %>%
        .[,c(1:35)]
      
      names(payload_df) <- payload_df[2,]
      
      payload_df <- payload_df[-c(1:3),]
      
      if('OPP DP' %in% colnames(payload_df) == TRUE) {
        
        payload_df <- payload_df %>%
          dplyr::rename(DP = `OPP DP`)
      }
      
      cols_to_num <- c("G", "App", "GS", "IP", "CG", "H", "R", "ER", "BB", "SO", "SHO", "BF", "P-OAB", "2B-A", "3B-A", "Bk", "HR-A", "WP", "HB", "IBB", "Inh Run", "Inh Run Score", "SHA", "SFA", "Pitches", "GO", "FO", "W", "L", "SV", "OrdAppeared", "KL")
      
      payload_df <- payload_df %>%
        dplyr::mutate_at(vars(-c("Date")),
                         list(~gsub("\\/", "", x = .))) %>%
        dplyr::mutate_at(cols_to_num, as.numeric)
    }
    
  } else {
    
    if(type == 'batting') {
      
      payload_df <- batting_payload %>%
        rvest::html_nodes('table') %>%
        .[3] %>%
        rvest::html_table(fill = T) %>%
        as.data.frame() %>%
        .[-1,]
      
      names(payload_df) <- payload_df[1,]
      
      payload_df <- payload_df[-1,]
      
      if('OPP DP' %in% colnames(payload_df) == TRUE) {
        
        payload_df <- payload_df %>%
          dplyr::rename(DP = `OPP DP`)
      }
      
      payload_df <- payload_df %>%
        dplyr::select(Year,Team,GP,G,BA,OBPct,SlgPct,R,AB,H,`2B`,`3B`,TB,HR,RBI,BB,HBP,SF,SH,K,DP,CS,Picked,SB,RBI2out)
      
      payload_df <- payload_df %>%
        dplyr::mutate(player_id = player_id) %>%
        dplyr::select(Year, player_id, everything())
      
    } else {
      
      payload_df <- pitching_payload %>%
        rvest::html_nodes('table') %>%
        .[3] %>%
        rvest::html_table(fill = T) %>%
        as.data.frame() %>%
        .[-1,]
      
      names(payload_df) <- payload_df[1,]
      
      payload_df <- payload_df[-1,]
      
      payload_df <- payload_df %>%
        dplyr::select(Year,Team,GP,G,App,GS,ERA,IP,CG,H,R,ER,BB,SO,SHO,BF,`P-OAB`,`2B-A`,`3B-A`,Bk,`HR-A`,WP,HB,IBB,`Inh Run`,`Inh Run Score`,SHA,SFA,Pitches,GO,FO,W,L,SV,KL)
      
      payload_df <- payload_df %>%
        dplyr::mutate(player_id = player_id) %>%
        dplyr::select(Year, player_id, everything())
    }
    
  }
  
  return(payload_df)
  
}



#Pitchers - Kevin Abel (ABEL) 1975377
Abel_logs <- get_ncaa_game_logs_v2(player_id = 1975377,span = "game",type = "pitching",year = 2021) %>% mutate(across(c("H":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(player_id = 1975377) %>% mutate(ERA = "",GP = G,Team = "Oregon St.",WHIP = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = "",`K/9` = "") %>% 
  select(-c("OrdAppeared"))


write.csv(Abel_logs,"ab.csv")


Abel_career <- get_ncaa_game_logs_v2(player_id = 1975377,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year)) 






# 2335485 Mack Anglin

Anglin_logs <- get_ncaa_game_logs_v2(player_id = 2335485,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2335485) %>% mutate(ERA = "",GP = G,Team = "Clemson",WHIP = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
select(-c("OrdAppeared"))


Anglin_career <- get_ncaa_game_logs_v2(player_id = 2335485,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))



# 2307074 Riley Cornelio
Cornelio_logs <- get_ncaa_game_logs_v2(player_id = 2307074,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2307074) %>% mutate(ERA = "",GP = G,Team = "TCU",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Cornelio_career <- get_ncaa_game_logs_v2(player_id = 2307074,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))





# 2137435 Becker fix this()
Becker_logs <- get_ncaa_game_logs_v2(player_id = 2137435,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2137435) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Becker_career <- get_ncaa_game_logs_v2(player_id = 2137435,year = 2021,span = "career",type = "pitching") %>%  mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>% mutate(Date = "",Opponent = "",Result = "",AB = (BF - SHA - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = ((BB + H )/ IP)) %>%  filter(Year == "2020-21") %>% select(-c(Year))


#2119824 Birdsell

Birdsell_logs <- get_ncaa_game_logs_v2(player_id = 2119824,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2119824) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Birdsell_career <- get_ncaa_game_logs_v2(player_id = 2119824,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>%  mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>% mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - SHA - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = ((BB + H )/ IP))  %>% select(-c(Year))


# 2475649 Cam Brown

Brown_logs <- get_ncaa_game_logs_v2(player_id = 2475649,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2475649) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Brown_career <- get_ncaa_game_logs_v2(player_id = 2475649,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>%  mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>% mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - SHA - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = ((BB + H )/ IP)) %>%  filter(Year == "2020-21") %>% select(-c(Year))


# 2123514 Mississippi St. Cerantola

Cerantola_logs <- get_ncaa_game_logs_v2(player_id = 2123514,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2123514) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))



Cerantola_career <- get_ncaa_game_logs_v2(player_id = 2123514,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


#Feeney NC State 1887214

Feeney_logs <- get_ncaa_game_logs_v2(player_id = 1887214,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 1887214) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>% mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))



Feeney_career <- get_ncaa_game_logs_v2(player_id = 1887214,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>%  mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>% mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - SHA - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = ((BB + H )/ IP)) %>%  filter(Year == "2020-21") %>% select(-c(Year))


# 2310000 Pete Hansen Texas

Hansen_logs <- get_ncaa_game_logs_v2(player_id = 2310000,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2310000) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))



Hansen_career <- get_ncaa_game_logs_v2(player_id = 2310000,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>%  mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>% mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - SHA - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = ((BB + H )/ IP)) %>%  filter(Year == "2020-21") %>% select(-c(Year))


#Jaden 2132620
Hill_logs <- get_ncaa_game_logs_v2(player_id = 2132620,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2132620) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))

Hill_career <- get_ncaa_game_logs_v2(player_id = 2132620,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>%  mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>% mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - SHA - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = ((BB + H )/ IP)) %>%  filter(Year == "2020-21") %>% select(-c(Year))



#Kindreich 2021 2119195 Biola

Kd_logs <- get_ncaa_game_logs_v2(player_id = 2119195,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2119195) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Kd_career <- get_ncaa_game_logs_v2(player_id = 2119195,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>%  mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>% mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - SHA - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = ((BB + H )/ IP)) %>%  filter(Year == "2020-21") %>% select(-c(Year))


#Austin Krob #2307088
Krob_logs <- get_ncaa_game_logs_v2(player_id = 2307088,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2307088) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Krob_career <- get_ncaa_game_logs_v2(player_id = 2307088,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>% mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - SHA - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = ((BB + H )/ IP)) %>%  filter(Year == "2020-21") %>% select(-c(Year))



#Jack Leftwich 1985230

Leftwich_logs <- get_ncaa_game_logs_v2(player_id = 1985230,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 1985230) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Leftwich_career <- get_ncaa_game_logs_v2(player_id = 1985230,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


#Seth Lonsway 1993441

Lonsway_logs <- get_ncaa_game_logs_v2(player_id = 1993441,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 1993441) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))

Lonsway_career <- get_ncaa_game_logs_v2(player_id = 1993441,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))

# JP Massey 2114015
Massey_logs <- get_ncaa_game_logs_v2(player_id = 2114015,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2114015) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))

Massey_career <- get_ncaa_game_logs_v2(player_id = 2114015,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


#  McFarlane 2310014

Mcfarlane_logs <- get_ncaa_game_logs_v2(player_id = 2310014,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2310014) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Mcfarlane_career <- get_ncaa_game_logs_v2(player_id = 2310014,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))



# Griff McGarry 1983417

McGarry_logs <- get_ncaa_game_logs_v2(player_id = 1983417,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 1983417) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


McGarry_career <- get_ncaa_game_logs_v2(player_id = 1983417,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


#Cy Nielson 2301728 BYU

Nielson_logs <- get_ncaa_game_logs_v2(player_id = 2301728,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2301728) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Nielson_career <- get_ncaa_game_logs_v2(player_id = 2301728,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))




# Connor Prielipp 2021 2303260 Alabama

Prielipp_logs <- get_ncaa_game_logs_v2(player_id = 2303260,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2303260) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Prielipp_career <- get_ncaa_game_logs_v2(player_id = 2303260,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))



#Kumar 2021 2137440

Rocker_logs <- get_ncaa_game_logs_v2(player_id = 2137440,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2137440) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Rocker_career <- get_ncaa_game_logs_v2(player_id = 2137440,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


# Miami 2470900 Alejandro Rosario
Rosario_logs <- get_ncaa_game_logs_v2(player_id = 2470900,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2470900) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))

Rosario_career <- get_ncaa_game_logs_v2(player_id = 2470900,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))



# Nate Savino 2306475 Virginia

Savino_logs <- get_ncaa_game_logs_v2(player_id = 2306475 ,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2306475) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))

Savino_career <- get_ncaa_game_logs_v2(player_id = 2306475,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


# Carson Seymour 1971873 Kansas St.

Seymour_logs <- get_ncaa_game_logs_v2(player_id = 1971873 ,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 1971873) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))

Seymour_career <- get_ncaa_game_logs_v2(player_id = 1971873,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))



# Brandon Sproat 2305251 Florida

Sproat_logs <- get_ncaa_game_logs_v2(player_id = 2305251 ,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2305251) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Sproat_career <- get_ncaa_game_logs_v2(player_id = 2305251,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


#Brett Thomas 2021 2304791 South Carolina
Thomas_logs <- get_ncaa_game_logs_v2(player_id = 2304791 ,span = "game",type = "pitching",year = 2021) %>% mutate(player_id =  2304791) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Thomas_career <- get_ncaa_game_logs_v2(player_id = 2304791,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


# Jack Washburn 2301960

Washburn_logs <- get_ncaa_game_logs_v2(player_id = 2301960 ,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2301960) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Washburn_career <- get_ncaa_game_logs_v2(player_id = 2301960,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


# Nate Wohlgemuth 2477432 Arkansas

Wohlgemuth_logs <- get_ncaa_game_logs_v2(player_id = 2477432 ,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2477432) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Wohlgemuth_career <- get_ncaa_game_logs_v2(player_id = 2477432,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


# Gabriel Hughes 2312826

GHughes_logs <- get_ncaa_game_logs_v2(player_id = 2312826 ,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2312826) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


GHughes_career <- get_ncaa_game_logs_v2(player_id = 2312826,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))



# 2304352 Carson ECU

Carson_logs <- get_ncaa_game_logs_v2(player_id = 2304352 ,span = "game",type = "pitching",year = 2021) %>% mutate(player_id = 2304352) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Carson_career <- get_ncaa_game_logs_v2(player_id = 2304352,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


# Carson Montgomery 2468073 FSU

Mongomery_logs <- get_ncaa_game_logs_v2(player_id =2468073,span = "game",type = "pitching",year = 2021) %>% mutate(player_id =  2468073) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Montgomery_career <- get_ncaa_game_logs_v2(player_id =  2468073,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


# Christian MacLeod 2123508


MacLeod_logs <- get_ncaa_game_logs_v2(player_id =2123508,span = "game",type = "pitching",year = 2021) %>% mutate(player_id =  2123508) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


MacLeoad_career <- get_ncaa_game_logs_v2(player_id =  2123508,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))



# Derek Diamond	2021	2331970		Ole Miss


Diamond_logs <- get_ncaa_game_logs_v2(player_id = 2331970	,span = "game",type = "pitching",year = 2021) %>% mutate(player_id =  2331970	) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Diamond_career <- get_ncaa_game_logs_v2(player_id = 2331970,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))


# Caswell Smith	2021	2345672		Charleston

Caswell_logs <- get_ncaa_game_logs_v2(player_id = 2345672,span = "game",type = "pitching",year = 2021) %>% mutate(player_id =  22345672) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = "",BAA = "",`K/9` = "") %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(AB = (BF - as.numeric(SHA) - SFA - BB - HB)) %>% 
  select(-c("OrdAppeared"))


Caswell_career <- get_ncaa_game_logs_v2(player_id = 2345672	,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))



# Jack Brannigan 2326787 Notre Dame

JB_logs <- get_ncaa_game_logs_v2(player_id = 2326787,span = "game",type = "pitching",year = 2021) %>% mutate(across(c("BF","SHA","SFA","BB","HB"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(player_id = 2326787) %>% mutate(ERA = "",GP = G,Team = "",WHIP = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = "",`K/9` = "") %>% 
  select(-c("OrdAppeared"))



JB_career <- get_ncaa_game_logs_v2(player_id = 2326787,year = 2021,span = "career",type = "pitching") %>% filter(Year == "2020-21") %>% mutate(across(c("GP":"KL"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  mutate(Date = "",Opponent = "Season YTD",Result = "",AB = (BF - as.numeric(SHA) - SFA - BB - HB),BAA = (H / AB),`K/9` = (SO/IP) * 9,WHIP = (BB + H )/ IP) %>% select(-c(Year))






# pitching logs


abel_logs <- abel_logs %>% rename("2B-A" = X2B.A,"3B-A" = X3B.A,"HR-A" = HR.A,`K/9` = K.9,"P-OAB" = P.OAB,`Inh Run` = Inh.Run,`Inh Run Score` = Inh.Run.Score)




pitching_logs <- rbind(abel_logs,Anglin_logs,Cornelio_logs,Becker_logs,Birdsell_logs,Brown_logs,Cerantola_logs,Feeney_logs,Hansen_logs,Hill_logs,Krob_logs,Leftwich_logs,Lonsway_logs,Massey_logs,Mcfarlane_logs,McGarry_logs,Nielson_logs,Prielipp_logs,Rocker_logs,Rosario_logs,Savino_logs,Seymour_logs,Sproat_logs,Thomas_logs,Washburn_logs,Wohlgemuth_logs,GHughes_logs,Kd_logs,JB_logs,Caswell_logs,Diamond_logs,MacLeod_logs,Mongomery_logs,Carson_logs)




#pitching_logs$Date <- mdy(pitching_logs$Date)
#pitchers$Date <- mdy(pitchers$Date)




#pitching_logs$ERA <- as.numeric(pitching_logs$ERA)


#pitching_logs$WHIP <- round(as.numeric(pitching_logs$WHIP),2)
#pitching_logs$BAA <- round(as.numeric(pitching_logs$BAA),3)
#pitching_logs$`K/9` <- round(as.numeric(pitching_logs$`K/9`),1)




pitching_logs <- unique(pitching_logs) %>% inner_join(ncaa_clients)


pitchers2 <- pitchers %>% rename("2B-A" = X2B.A,"3B-A" = X3B.A,"HR-A" = HR.A,`K/9` = K.9,"P-OAB" = P.OAB,`Inh Run` = Inh.Run,`Inh Run Score` = Inh.Run.Score,Bk = BK,App = APP) %>% mutate(E =1,X1 = 3, Var1 = 2, Var2 = 3,Team = 2)




pitching_logs$Date <- mdy(pitching_logs$Date)
pitchers2$Date <- mdy(pitchers2$Date)

pitchers_day <- rbind(pitchers2,pitching_logs)


career_logs <- rbind(Abel_career,Anglin_career,Cornelio_career,Becker_career,Birdsell_career,Brown_career,Cerantola_career,Feeney_career,Hansen_career,Hill_career,Krob_career,Leftwich_career,Lonsway_career,Massey_career,Mcfarlane_career,McGarry_career,Nielson_career,Prielipp_career,Rocker_career,Rosario_career,Savino_career,Seymour_career,Sproat_career,Thomas_career,Washburn_career,Wohlgemuth_career,GHughes_career,Kd_career,JB_career,Caswell_career,Diamond_career,MacLeoad_career,Montgomery_career,Carson_career) %>% inner_join(ncaa_clients)

career_logs$WHIP <- round(as.numeric(career_logs$WHIP),2)
career_logs$BAA <- round(as.numeric(career_logs$BAA),3)
career_logs$`K/9` <- round(as.numeric(career_logs$`K/9`),1)
career_logs$ERA <- round(as.numeric(career_logs$ERA),2)


## Daily ----- choose the date/ daily games


## all games today

# pitchers who pitched
pitchers_day2 <- pitchers_day %>% filter(Date == "2001-02-02" | (Date == today()  & App == 1))

#career_logs$WHIP <- round(as.numeric(career_logs$WHIP),2)
#career_logs$BAA <- round(as.numeric(career_logs$BAA),3)
#career_logs$`K/9` <- round(as.numeric(career_logs$`K/9`),1)
#career_logs$ERA <- round(as.numeric(career_logs$ERA),2)

PITCHERS_DAY2 <- rbind(pitchers_day2,career_logs)
# pitchers who team played

(pitchers_day3 <- pitchers_day %>% filter(Date == today()) %>%  filter(target != "Target") %>% select(Name,Result,school,Opponent,Date,App))


### Daily Report

PITCHERS_DAY2 <- PITCHERS_DAY2 %>% filter(LastName %in% c("Abel","Whisenhunt","Kindreich","Anglin"))


TODAY <- PITCHERS_DAY2 %>% mutate(Player = paste(PITCHERS_DAY2$Name,PITCHERS_DAY2$yr,"(",PITCHERS_DAY2$school,PITCHERS_DAY2$PT,")")) %>%  rename(HR = `HR-A`) %>% filter(target != "Target") %>%  select(Opponent,Result,Name,Team,ERA,IP,H,`2B` = `2B-A`,`3B` = `3B-A`,HR,R,ER,BB,SO,BF,AB,P = Pitches,AB,W,L,WHIP,`K/9`,BAA,APP = App,GS,target,LastName,yr,target,Player) %>%  arrange(target,yr,LastName)%>% gt(groupname_col ="Player")%>% tab_header(title = md("**Advisees**"),subtitle = md("**(4/9/21)**"))  %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "75%",grand_summary_row.background.color = "Navy",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2, row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% fmt_missing(columns = vars(ERA,IP,H,HR,R,ER,BB,SO,AB,W,L,WHIP,`K/9`,BAA,APP,GS,BF),missing_text = "") %>% cols_hide(vars(Name,Team,LastName,yr,target)) %>% tab_style(style = list(cell_fill(color = "Grey")),locations = cells_body(rows = Opponent == "Season YTD") 
) %>% tab_options(row_group.font.weight = 'bolder',row_group.font.size = 28,row_group.border.bottom.style = "bottom",row_group.border.top.color = "white",row_group.border.top.width = 38)  %>% tab_style(style = list(cell_fill(color = "Black")),locations = cells_body(rows = Opponent == "Opponent") 
)  %>% tab_style(style = list(cell_text(color = "White")),locations = cells_body(rows = Opponent == "Opponent") 
) 


TODAY  %>% 
  gtsave(
    "PitcherLogsNight.pdf", expand = 10000,
  )  

##################### weekly logs

WEEKLY <- pitchers_day %>% filter(Date == "2001-02-02" | (Date >= "2021-03-29" & App == 1))

WEEKLY2 <- rbind(WEEKLY,career_logs)

 ######Advisees

WEEKLY7 <- WEEKLY2 %>% mutate(Player = paste(WEEKLY2$Name,WEEKLY2$yr,"(",WEEKLY2$school,WEEKLY2$PT,")")) %>% filter(target != "Target") %>%  rename(HR = `HR-A`) %>% select(Opponent,Result,Name,Team,ERA,IP,H,`2B` = `2B-A`,`3B` = `3B-A`,HR,R,ER,BB,SO,BF,AB,P = Pitches,AB,W,L,WHIP,`K/9`,BAA,APP = App,GS,target,LastName,yr,target,Player) %>%  arrange(target,yr,LastName)%>% gt(groupname_col ="Player")%>% tab_header(title = md("**Advisees**"),)  %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "75%",grand_summary_row.background.color = "Navy",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2, row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% fmt_missing(columns = vars(ERA,IP,H,HR,R,ER,BB,SO,AB,W,L,WHIP,`K/9`,BAA,APP,GS,BF),missing_text = "") %>% cols_hide(vars(Name,Team,LastName,yr,target)) %>% tab_style(style = list(cell_fill(color = "Grey")),locations = cells_body(rows = Opponent == "Season YTD") 
) %>% tab_options(row_group.font.weight = 'bolder',row_group.font.size = 28,row_group.border.bottom.style = "bottom",row_group.border.top.color = "white",row_group.border.top.width = 38)  %>% tab_style(style = list(cell_fill(color = "Black")),locations = cells_body(rows = Opponent == "Opponent") 
)  %>% tab_style(style = list(cell_text(color = "White")),locations = cells_body(rows = Opponent == "Opponent") 
) 

##Advisees

WEEKLY7  %>% 
  gtsave(
    "PitcherLogs.pdf", expand = 10000,
  )  


## Targets



TARGET7 <- WEEKLY2 %>% mutate(Player = paste(WEEKLY2$Name,WEEKLY2$yr,"(",WEEKLY2$school,WEEKLY2$PT,")")) %>% filter(target == "Target") %>%  rename(HR = `HR-A`) %>% select(Opponent,Result,Name,Team,ERA,IP,H,`2B` = `2B-A`,`3B` = `3B-A`,HR,R,ER,BB,SO,BF,AB,P = Pitches,AB,W,L,WHIP,`K/9`,BAA,APP = App,GS,target,LastName,yr,target,Player) %>%  arrange(target,yr,LastName)%>% gt(groupname_col ="Player")%>% tab_header(title = md("**Targets**"),subtitle = md("**(04/01) - (04/09)**"))  %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "75%",grand_summary_row.background.color = "Navy",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2, row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% fmt_missing(columns = vars(ERA,IP,H,HR,R,ER,BB,SO,AB,W,L,WHIP,`K/9`,BAA,APP,GS,BF),missing_text = "") %>% cols_hide(vars(Name,Team,LastName,yr,target)) %>% tab_style(style = list(cell_fill(color = "Grey")),locations = cells_body(rows = Opponent == "Season YTD") 
) %>% tab_options(row_group.font.weight = 'bolder',row_group.font.size = 28,row_group.border.bottom.style = "bottom",row_group.border.top.color = "white",row_group.border.top.width = 38)  %>% tab_style(style = list(cell_fill(color = "Black")),locations = cells_body(rows = Opponent == "Opponent") 
)  %>% tab_style(style = list(cell_text(color = "White")),locations = cells_body(rows = Opponent == "Opponent") 
) 






Target7  %>% 
  gtsave(
    "PitcherTargetLogs.pdf", expand = 10000,
  )  




# YTD Report ###################


YTD <- career_logs

















ytd_final_target_adv <- YTD  %>% arrange(target,yr,LastName) %>% filter(target != "Target") %>% rename(HR = `HR-A`,`2B` = `2B-A`) %>% select(Name,Team,ERA,IP,H,`2B`,HR,R,ER,BB,SO,BF,AB,W,L,WHIP,`K/9`,BAA,APP = App,GS,BF,target,LastName) %>% gt(rowname_col = "Name",groupname_col = "target" )%>% tab_header(title = md("**Advisees**"),subtitle = md("*04-04-2021*")) %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "90%",grand_summary_row.background.color = "Navy",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% fmt_missing(columns = vars(ERA,IP,H,`2B`,HR,ER,BB,SO,AB,W,L,WHIP,`K/9`,BAA,APP,GS,BF)) %>% cols_hide(columns = vars(LastName)) %>% fmt_number(
  columns = "WHIP",
  decimals = 2,
  use_seps = FALSE
) %>% fmt_number(
  columns = "K/9",
  decimals = 1,
  use_seps = FALSE
) %>% fmt_number(
  columns = "BAA",
  decimals = 3,
  use_seps = FALSE) %>%  cols_align(
    align = c("auto", "left", "center", "right"),
    columns = everything()
  ) %>%  opt_table_lines() 


ytd_final_target_adv  %>% 
  gtsave(
    "YTDPitchers.pdf", expand = 10000,
  )  


###############################################################

ytd_final_target_tar <- YTD  %>% arrange(target,yr,LastName) %>% filter(target == "Target") %>% rename(HR = `HR-A`) %>% select(Name,Team,ERA,IP,H,R,ER,BB,SO,BF,AB,W,L,WHIP,`K/9`,BAA,APP = App,GS,BF,target,LastName,E = Errors) %>% gt(rowname_col = "Name",groupname_col = "target" )%>% tab_header(title = md("**Targets**"),subtitle = md("*04-04-2021*")) %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "90%",grand_summary_row.background.color = "Navy",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% fmt_missing(columns = vars(ERA,IP,H,R,ER,BB,SO,AB,W,L,WHIP,`K/9`,BAA,APP,GS,BF)) %>% cols_hide(columns = vars(LastName)) %>% fmt_number(
  columns = "WHIP",
  decimals = 2,
  use_seps = FALSE
) %>% fmt_number(
  columns = "K/9",
  decimals = 1,
  use_seps = FALSE
) %>% fmt_number(
  columns = "BAA",
  decimals = 3,
  use_seps = FALSE) %>%  cols_align(
    align = c("auto", "left", "center", "right"),
    columns = everything()
  )


ytd_final_target_tar  %>% 
  gtsave(
    "YTDTar3.pdf", expand = 10000,
  )  


















# Game Logs

# filter pitching logs data

logs <- pitching_logs %>% filter((Date >= "2021-03-29"  & Date <= today() - 1) & App == 1)

GL <- rbind(logs,career_logs)

GL <- GL %>% inner_join(ncaa_clients)






xo <- GL %>% select(Opponent,Result,Name,Team,ERA,IP,H,R,ER,BB,SO,BF,AB,W,L,WHIP,`K/9`,BAA,APP = App,GS,BF,target,LastName,yr,target) %>%  mutate(label = paste(GL$Name,"(",GL$school,GL$PT,paste0(yr,'`'),")"))  %>% arrange(target,yr,LastName)



write.csv(xo,"Gamelogs.csv")

Weekly_pitching <- GL %>% rename(HR = `HR-A`) %>% select(Opponent,Result,Name,Team,ERA,IP,H,HR,R,ER,BB,SO,BF,Pc = Pitches,AB,W,L,WHIP,`K/9`,BAA,APP = App,GS,BF,target,LastName,yr,target) %>%  mutate(label = paste(GL$Name,"(",GL$school,GL$PT,paste0(yr,'`'),")"))  %>% arrange(target,yr,LastName)%>% gt(groupname_col = "label" )%>% tab_header(title = md("**Pitchers (3/29- 4/4)**"))  %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "90%",grand_summary_row.background.color = "Navy",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% fmt_missing(columns = vars(ERA,IP,H,HR,R,ER,BB,SO,AB,W,L,WHIP,`K/9`,BAA,APP,GS,BF)) %>% cols_hide(vars(Name,Team,LastName,yr,target)) %>% tab_style(style = list(cell_fill(color = "Grey")),locations = cells_body(rows = Opponent == "Season YTD") 
) %>% fmt_number(
  columns = "WHIP",
  decimals = 2,
  use_seps = FALSE
) %>% fmt_number(
  columns = "K/9",
  decimals = 1,
  use_seps = FALSE
) %>% fmt_number(
  columns = "BAA",
  decimals = 3,
  use_seps = FALSE) %>%  tab_style(style = list(cell_text(color = "Red")),locations = cells_body(rows = target == "Target"))


Weekly_pitching %>% 
  gtsave(
    "WeeklyPitchers4.pdf", expand = 10000,
  )  

nightly_p


l <- career_logs %>% inner_join(ncaa_clients)
write.csv(l,"stack_logs.csv")


nightly <- pitching_logs %>% filter(Date == today() - 1,App == 1)

nightly_p <- rbind(nightly,career_logs) %>% inner_join(ncaa_clients)

y <- nightly_p %>%  rename(HR = `HR-A`) %>%  select(Opponent,Result,Name,Team,ERA,IP,H,HR,R,ER,BB,SO,BF,AB,PC = Pitches,W,L,WHIP,`K/9`,BAA,APP = App,GS,BF,target,LastName,yr) %>%  mutate(label = paste(nightly_p$Name, nightly_p$yr,"(",nightly_p$school,nightly_p$PT,")"))  %>% arrange(target,yr,LastName) %>% filter(target!= "Target" | LastName == "Hill") 
  
  
  gt(groupname_col = "label", )%>% tab_header(title = md("**Pitchers**"),subtitle = md("*BORAS CORP COLLEGE DRAFT ADVISEE & TARGET RESULTS – POS. PLAYERS (4/1) Sorted by Advisees/Targets, Draft Eligible Year, and then alphabetically*")) %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "90%",grand_summary_row.background.color = "Navy",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% fmt_missing(columns = vars(ERA,IP,H,HR,R,ER,BB,SO,AB,W,L,WHIP,`K/9`,BAA,APP,GS,BF)) %>% cols_hide(vars(Name,Team,LastName,yr,target)) %>% tab_style(style = list(cell_fill(color = "Grey")),locations = cells_body(rows = Opponent == "Season YTD") 
) %>% fmt_number(
  columns = "WHIP",
  decimals = 2,
  use_seps = FALSE
) %>% fmt_number(
  columns = "K/9",
  decimals = 1,
  use_seps = FALSE
) %>% fmt_number(
  columns = "BAA",
  decimals = 3,
  use_seps = FALSE) %>% tab_options(row_group.padding = 22,row_group.border.bottom.width = 'dashed',row_group.border.bottom.color = "Navy",row_group.border.top.color = "Navy",row_group.border.top.width = 'dashed')


y %>% 
  gtsave(
    "pitchersweekly.pdf", expand = 10000,
  )  















