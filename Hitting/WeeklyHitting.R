# Game Logs Hitting

# Game logs script

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

#Read in client list
ncaa_clients <- read_csv("Data/ClientsFinalx.csv") 


#pitchers <- ncaa_clients %>% filter(position == "P" | position == "UT")

#write.csv(pitchers,"pitchers.csv")

#hitters <- ncaa_clients %>% filter(position != "P")



# load in function

Errors <- read_sheet("https://docs.google.com/spreadsheets/d/1xRLnN7LaGOrrnmmRmTifnY_EwOaeFZES6-9y9_NZT2U/edit#gid=962710186")



hitters <- read.csv("hitters.csv") %>% rename(`3B` = X3B,`2B` = X2B)





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


#Corey Collins 2021 2471655 Georgia

Collins_logs <- get_ncaa_game_logs_v2(player_id =2471655,year = 2021,span = "game",type = "batting") %>% mutate(player_id = 2471655,Team = "Georgia", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")



Collins_career <- get_ncaa_game_logs_v2(player_id =2471655,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")


# Adrian Del Castillo 2021 2122652 Miami (FL)

Adrian_logs <- get_ncaa_game_logs_v2(player_id = 2122652 ,year = 2021,span = "game",type = "batting") %>% mutate(player_id = 2122652 ,Team = "Miami", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "") 


Adrian_career <- get_ncaa_game_logs_v2(player_id = 2122652,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21") 

# Kyle Teel 2021 2486493 Virginia

Teel_logs <-  get_ncaa_game_logs_v2(player_id =2486493,year = 2021,span = "game",type = "batting") %>% mutate(player_id = 2486493,Team = "Virginia", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "") 

Teel_career <- get_ncaa_game_logs_v2(player_id = 2486493,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21") 

#Kris Armstrong 2122294 Florida

Armstrong_logs <-get_ncaa_game_logs_v2(player_id = 2122294,year = 2021,span = "game",type = "batting") %>% mutate(player_id = 2122294,Team = "Florida", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "") 



Armstrong_career <- get_ncaa_game_logs_v2(player_id =2122294,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21") 


# Garrett Blaylock 1978818 Georgia

Blaylock_logs <- get_ncaa_game_logs_v2(player_id = 1978818,year = 2021,span = "game",type = "batting") %>% mutate(player_id = 1978818,Team = "Georgia", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "") 



Blaylock_career <- get_ncaa_game_logs_v2(player_id = 1978818,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21") 

#Zack Gelof 2127049 Virginia

Gelof_logs <- get_ncaa_game_logs_v2(player_id = 2127049,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2127049,Team = "Virginia", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "") 


Gelof_career <- get_ncaa_game_logs_v2(player_id = 2127049,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21") 


#Nick Goodwin 2471788 Fr Kansas St.

Goodwin_logs <- get_ncaa_game_logs_v2(player_id = 2471788,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2471788,Team = "Kansas St.", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")


Goodwin_career <- get_ncaa_game_logs_v2(player_id = 2471788,year = 2021,span = "career",type = "batting")  %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21") 



# Kalae Harrison 2480184 Texas A&M

Harrison_logs <-get_ncaa_game_logs_v2(player_id = 2480184,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2480184,Team = "Texas A&M.", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")




Harrison_career <- get_ncaa_game_logs_v2(player_id =2480184,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21") 

# Jace Jung 2346820 Texas Tech

Jung_logs <- get_ncaa_game_logs_v2(player_id = 2346820,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2346820,Team = "Texas Tech", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "") 

Jung_career <- get_ncaa_game_logs_v2(player_id =2346820,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")

# Josh Rivera 2021 2305256 Florida

Rivera_logs <-get_ncaa_game_logs_v2(player_id = 2305256,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2305256,Team = "Florida", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

Rivera_career <- get_ncaa_game_logs_v2(player_id =2305256,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")

# Alejandro Toral 2021 1982080 Miami (FL)

Toral_logs <-get_ncaa_game_logs_v2(player_id = 1982080,year = 2021,span = "game",type = "batting") %>% mutate(player_id =1982080,Team = "Miami (FL)", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

Toral_career <- get_ncaa_game_logs_v2(player_id =1982080,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")

#Trevor Werner 2309490 Texas A&M

Werner_logs <-get_ncaa_game_logs_v2(player_id = 2309490,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2309490,Team = "Texas A&M", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

Werner_career <- get_ncaa_game_logs_v2(player_id =2309490,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")

# Brock Jones 2311353 Stanford

Jones_logs <-get_ncaa_game_logs_v2(player_id = 2311353,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2311353,Team = "Stanford", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

Jones_career <- get_ncaa_game_logs_v2(player_id =2311353,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")


#Max Marusak 2133783 Texas Tech

Max_logs <-get_ncaa_game_logs_v2(player_id = 2133783,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2133783,Team = "Texas Tech", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

Max_career <- get_ncaa_game_logs_v2(player_id =2133783,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")

# Chris Newell 2306474 Virginia

Newell_logs <-get_ncaa_game_logs_v2(player_id = 2306474 ,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2306474 ,Team = "Virginia", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

Newell_career <- get_ncaa_game_logs_v2(player_id = 2306474,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")

# Carson Wells 2494065
#Southern California

Wells_logs <-get_ncaa_game_logs_v2(player_id = 2494065,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2494065,Team = "Southern California", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

Wells_career <- get_ncaa_game_logs_v2(player_id = 2494065,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")

#Gabriel Hughes 2312826 Gonzaga

#Hughes_logs <-get_ncaa_game_logs_v2(player_id = 2312826,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2312826,Team = "Gonzaga", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

#Hughes_career <- get_ncaa_game_logs_v2(player_id = 2312826,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")

# Luca Tresh 2021 2140156 NC State

Tresh_logs <-get_ncaa_game_logs_v2(player_id = 2140156,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2140156,Team = "NC State", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

Tresh_career <- get_ncaa_game_logs_v2(player_id = 2140156,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")


# Sean McLain ASU OF 2022 2349359

SMcLain_logs <- get_ncaa_game_logs_v2(player_id = 2349359,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2349359,Team = "Arizona St.", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

SMcLain_career <- get_ncaa_game_logs_v2(player_id = 2349359,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")


#write.csv(SMcLain_logs,"SMClain.csv")
#x <- read.csv("SMClain.csv")

# Peyton Graham Oklahoma 2330757 



Graham_logs <- get_ncaa_game_logs_v2(player_id = 2330757,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2330757,Team = "Oklahoma ", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

Graham_career <- get_ncaa_game_logs_v2(player_id = 2330757,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")


# Jack Brannigan 2326787 Notre Dame

Brannigan_logs <- get_ncaa_game_logs_v2(player_id = 2326787,year = 2021,span = "game",type = "batting") %>% mutate(player_id = 2326787,Team = "Notre Dame", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

Brannigan_career <- get_ncaa_game_logs_v2(player_id = 2326787,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")



#2131028

MattM_logs <- get_ncaa_game_logs_v2(player_id = 2131028,year = 2021,span = "game",type = "batting") %>% mutate(player_id = 2131028,Team = "UCLA", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")

MattM_careers <- get_ncaa_game_logs_v2(player_id = 2131028,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")

# Dylan Crews
crews_logs <- get_ncaa_game_logs_v2(player_id =  2486588,year = 2021,span = "game",type = "batting") %>% mutate(player_id =  2486588,Team = "LSU", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")


crews_career <-  get_ncaa_game_logs_v2(player_id = 2486588,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")


# 2485641 -- Enrique #Bradfield Jr. , Enrique 
#Fr
#2485641

Enrique_logs <- get_ncaa_game_logs_v2(player_id = 2485641,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2485641,Team = "Vanderbilt", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")


Enrique_career <-  get_ncaa_game_logs_v2(player_id = 2485641,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")





# Young, Carter
#So
#2309744

Carter_logs <- get_ncaa_game_logs_v2(player_id = 2309744,year = 2021,span = "game",type = "batting") %>% mutate(player_id =2309744,Team = "Vanderbilt", OBPct = "",SlgPct = "",BA = "",GP = "",Year = "")


Carter_career <-  get_ncaa_game_logs_v2(player_id = 2309744,year = 2021,span = "career",type = "batting") %>% mutate("Date" = "",Opponent = "Season YTD",IBB = "",Result = "") %>% filter(Year == "2020-21")




ytd_clogs <- rbind(Tresh_career,Wells_career,Newell_career,Max_career,Jones_career,Werner_career,Toral_career,Rivera_career,Jung_career,Harrison_career,Goodwin_career,Gelof_career,Blaylock_career,Armstrong_career,Teel_career,Adrian_career,Collins_career,SMcLain_career,Graham_career,Brannigan_career,MattM_careers,crews_career,Carter_career,Enrique_career)

#read.csv("day.csv")


ytd_clogs$SB <- sub("^$", 0, ytd_clogs$SB)
ytd_clogs$H <- sub("^$", 0, ytd_clogs$H)
ytd_clogs$R <- sub("^$", 0, ytd_clogs$R)
ytd_clogs$`2B` <- sub("^$", 0, ytd_clogs$`2B`)
ytd_clogs$`3B` <- sub("^$", 0, ytd_clogs$`3B`)
ytd_clogs$HR <- sub("^$", 0, ytd_clogs$HR)
ytd_clogs$RBI <- sub("^$", 0, ytd_clogs$RBI)
ytd_clogs$BB <- sub("^$", 0, ytd_clogs$BB)
ytd_clogs$K <- sub("^$", 0, ytd_clogs$K)
ytd_clogs$CS <- sub("^$", 0, ytd_clogs$CS)
ytd_clogs$G <- sub("^$", 0, ytd_clogs$G)


ytd_clogs <- ytd_clogs %>% mutate(OPS = as.numeric(OBPct) + as.numeric(SlgPct)) %>% inner_join(ncaa_clients)

ytd_clogs <- ytd_clogs%>% mutate(Player = paste(ytd_clogs$Name,ytd_clogs$Yr))


#### DAIlY

x <- x %>% rename(`2B` = X2B,`3B` = X3B) %>% select(-c(X))


hitters

gameday_logs <- rbind(Tresh_logs,Wells_logs,Newell_logs,Max_logs,Jones_logs,Werner_logs,Toral_logs,Rivera_logs,Jung_logs,Harrison_logs,Goodwin_logs,Gelof_logs,Blaylock_logs,Armstrong_logs,Teel_logs,Adrian_logs,Collins_logs,Brannigan_logs,SMcLain_logs,Graham_logs,MattM_logs,crews_logs,Enrique_logs,Carter_logs) %>% mutate(OPS = '') %>% inner_join(ncaa_clients) 

game_day <- gameday_logs %>% mutate(Player = paste(gameday_logs$Name,gameday_logs$Yr))


game_day$Date <- lubridate::mdy(gameday_logs$Date)

hitters$Date <- lubridate::mdy(hitters$Date)


# Weekly

week_hitters <- rbind(game_day,hitters)

week_hitters2 <- week_hitters %>% filter(Date == '2020-02-20' | (Date >= '2021-4-12')) %>% arrange(LastName,Date) %>% filter(G == 1 | G == 'G')

week_hitters3 <- rbind(week_hitters2,ytd_clogs) 


(hitters_day3 <- week_hitters %>% filter(Date == today()) %>% filter(target != "Target") %>% mutate(across(c("GP","AB","G"), ~ as.numeric(.x))) %>% replace(is.na(.), 0) %>%  select(Name,Result,school,Opponent,Date,G))


##Daily
day_hitters <- week_hitters %>% filter(Date == '2020-02-20'| (Date == today() )) %>% arrange(LastName,Date) %>% filter(G == 1 | G == 'G')

# Filter out guys who dnp  


day_hitters2 <- rbind(day_hitters,ytd_clogs) 



day_hitters <- day_hitters2 %>% arrange(target,LastName)

#write_csv(DAY,"dayh.csv")

Day2 <- day_hitters2 %>% filter(LastName != "Rivera") %>% filter(LastName!= "Armstrong") %>% filter(LastName!= "Crews")


day_hitters2 <- Day2




DAY <- read_csv("day.csv")


#Change date ----- DAILY!







DAY <- day_hitters2 %>% mutate(Player = paste(day_hitters2$Name,day_hitters2$yr,"(",day_hitters2$school,day_hitters2$position,")")) %>%  select (Player,Opponent,Result,AVG = BA,Team = school,Date,target,AB,R,H,`2B`,`3B`,HR,RBI,BB,SO = K,OBP = OBPct,SLG = SlgPct,OPS,SB,CS,position,LastName,school,Name,yr,target)  %>% arrange(target,LastName,Date,target) %>% filter(target != "Target")  %>%  gt(groupname_col = "Player") %>% tab_options(column_labels.background.color = "black") %>%   cols_hide(columns = c("Date","LastName","target","Team")) %>% tab_options(table.width = "70%",  row_group.background.color = "lightblue") %>% tab_style(style = list(cell_fill(color = "Black")),locations = cells_body(rows = Opponent == "Opponent"))  %>% tab_style(style = list(cell_text(color = "white")),locations = cells_body(rows = Opponent == "Opponent")) %>% cols_align(align = "center") %>% tab_options(data_row.padding = px(1/2)) %>% tab_options(row_group.padding = px(12)) %>% tab_style(style = list(cell_text(color = "black")),locations = cells_body(rows = Opponent == "Season YTD")) %>% tab_style(style = list(cell_fill(color = "grey")),locations = cells_body(rows = Opponent == "Season YTD")) %>% cols_hide(columns = c("Name","yr","position","Player")) %>% fmt_missing(c("R","2B","3B","HR","RBI","BB","SO","SB","CS","H","AB"), rows = NULL, missing_text = 0) %>%  tab_header(title = md("**Hitters**"),subtitle = md("*(4/18/2021)*")) %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "75%",grand_summary_row.background.color = "purple",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% tab_options(row_group.font.weight = 'bolder',row_group.font.size = 28,row_group.border.bottom.style = "all",row_group.border.top.color = "white",row_group.border.top.width = 38) %>% fmt_missing(c("AVG","OBP","SLG","OPS"),missing_text = " ")



DAY %>% 
  gtsave(
    "DaylogsH.pdf", expand = 10000,
  )  


### Advisees ## WEEKLY

GAME_LOGS <- week_hitters3 %>% mutate(Player = paste(week_hitters3$Name,week_hitters3$yr,"(",week_hitters3$school,week_hitters3$position,")")) %>%  select (Player,Opponent,Result,AVG = BA,Team = school,Date,target,AB,R,H,`2B`,`3B`,HR,RBI,BB,SO = K,OBP = OBPct,SLG = SlgPct,OPS,SB,CS,position,LastName,school,Name,yr,target)  %>% arrange(target,LastName,Date,target) %>% filter(target != "Target")  %>%  gt(groupname_col = "Player") %>% tab_options(column_labels.background.color = "black") %>%   cols_hide(columns = c("Date","LastName","target","Team")) %>% tab_options(table.width = "70%",  row_group.background.color = "lightblue") %>% tab_style(style = list(cell_fill(color = "Black")),locations = cells_body(rows = Opponent == "Opponent"))  %>% tab_style(style = list(cell_text(color = "white")),locations = cells_body(rows = Opponent == "Opponent")) %>% cols_align(align = "center") %>% tab_options(data_row.padding = px(1/2)) %>% tab_options(row_group.padding = px(12)) %>% tab_style(style = list(cell_text(color = "black")),locations = cells_body(rows = Opponent == "Season YTD")) %>% tab_style(style = list(cell_fill(color = "grey")),locations = cells_body(rows = Opponent == "Season YTD")) %>% cols_hide(columns = c("Name","yr","position","Player")) %>% fmt_missing(c("R","2B","3B","HR","RBI","BB","SO","SB","CS","H","AB"), rows = NULL, missing_text = 0) %>%  tab_header(title = md("**Advisees**"),subtitle = md("*4/12/2021 -- 4/20/2021*")) %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "75%",grand_summary_row.background.color = "purple",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% tab_options(row_group.font.weight = 'bolder',row_group.font.size = 28,row_group.border.bottom.style = "bottom",row_group.border.top.color = "white",row_group.border.top.width = 38)





GAME_LOGS %>% 
  gtsave(
    "gamelogs.pdf", expand = 10000,
  )  


#### Targets ### Weekly ### Change Date

TARGET_LOGS 

week_hitters3 %>% filter(LastName != "Werner")

l <- week_hitters3 %>% mutate(Player = paste(week_hitters3$Name,week_hitters3$yr,"(",week_hitters3$school,week_hitters3$position,")")) %>%   select (Player,Opponent,Result,AVG = BA,Team = school,Date,target,AB,R,H,`2B`,`3B`,HR,RBI,BB,SO = K,OBP = OBPct,SLG = SlgPct,OPS,SB,CS,position,LastName,school,Name,yr,target)  %>% arrange(yr,LastName,Date,target) %>% filter(target == "Target" & LastName != "Hughes")  %>%  gt(groupname_col = "Player") %>% tab_options(column_labels.background.color = "black") %>%   cols_hide(columns = c("Date","LastName","target","Team")) %>% tab_options(table.width = "70%",  row_group.background.color = "lightblue") %>% tab_style(style = list(cell_fill(color = "Black")),locations = cells_body(rows = Opponent == "Opponent"))  %>% tab_style(style = list(cell_text(color = "white")),locations = cells_body(rows = Opponent == "Opponent")) %>% cols_align(align = "center") %>% tab_options(data_row.padding = px(1/2)) %>% tab_options(row_group.padding = px(12)) %>% tab_style(style = list(cell_text(color = "black")),locations = cells_body(rows = Opponent == "Season YTD")) %>% tab_style(style = list(cell_fill(color = "grey")),locations = cells_body(rows = Opponent == "Season YTD")) %>% cols_hide(columns = c("Name","yr","position","Player")) %>% fmt_missing(c("R","2B","3B","HR","RBI","BB","SO","SB","CS","H","AB"), rows = NULL, missing_text = 0) %>%  tab_header(title = md("**Targets**"),subtitle = md("*4/12/2021 -- 4/20/2021*")) %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "75%",grand_summary_row.background.color = "purple",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% tab_options(row_group.font.weight = 'bolder',row_group.font.size = 28,row_group.border.bottom.style = "bottom",row_group.border.top.color = "white",row_group.border.top.width = 38)






l %>% 
  gtsave(
    "gamelogsTH.pdf", expand = 10000,
  )  






########### YTD





# YMD - game logs

ytd <- ytd_clogs %>% inner_join(ncaa_clients)

ytd <- ytd %>% mutate(across(c("GP":"SB"), ~ as.numeric(.x))) %>% inner_join(Errors)

YTD_adv <- ytd  %>% filter(target != "Target") %>%  mutate(OPS = OBPct + SlgPct) %>% arrange(target,LastName)  %>% select(Name,Team,AVG = BA,AB,R,H,'2B','3B',HR,RBI,BB,SO = K,OBP = OBPct,SLG = SlgPct,OPS,SB,CS,target,E = Errors) %>% gt(rowname_col = "Hitter",groupname_col = "target" )%>% tab_header(title = md("**Hitters**"),subtitle = "4/18/2021") %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "75%",grand_summary_row.background.color = "black",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold", column_labels.vlines.style = "dashed")  %>% fmt_number(columns = vars(AVG),decimals = 3)  %>% fmt_missing(columns = vars(AVG,AB,R,H,RBI,BB,SO,SB,CS,`2B`,`3B`,`HR`), missing_text = 0 ) %>% fmt_number(columns = vars(AVG),decimals = 3) %>% fmt_missing(columns = vars(AVG,OBP,SLG), missing_text = "-" ) %>% cols_align(align = "center") %>% opt_table_outline("solid",color = "black") %>%  opt_table_lines() 




YTD_adv %>% 
  gtsave(
    "ytdhitters.pdf", expand = 10000,
  )  



########################################################################################################################


YTD_tar <- ytd  %>% filter(target == "Target") %>%  mutate(OPS = OBPct + SlgPct) %>% arrange(target,LastName)  %>% select(Name,Team,AVG = BA,AB,R,H,'2B','3B',HR,RBI,BB,SO = K,OBP = OBPct,SLG = SlgPct,OPS,SB,CS,target) %>% gt(rowname_col = "Hitter",groupname_col = "target" )%>% tab_header(title = md("**Targets**"),subtitle = "4/04/2021") %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dotted",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "70%",grand_summary_row.background.color = "purple",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold")  %>% fmt_number(columns = vars(AVG),decimals = 3)  %>% fmt_missing(columns = vars(AVG,AB,R,H,RBI,BB,SO,SB,CS,`2B`,`3B`,`HR`), missing_text = 0 ) %>% fmt_number(columns = vars(AVG),decimals = 3) %>% fmt_missing(columns = vars(AVG,OBP,SLG), missing_text = "-" )  %>%  opt_table_lines() 




ss %>% 
  gtsave(
    "nightly.pdf", expand = 10000,
  )  















#####################3

# Weekly game logs for hitters

hitting <- hitting %>% mutate(across(c("GP":"SB"), ~ as.numeric(.x)))

Hitting_weekly <- hitting %>% mutate(OPS = OBPct + SlgPct ) %>% select(Opponent,Result,AVG = BA,Team = school,Date,target,AB,R,H,`2B`,`3B`,HR,RBI,BB,SO = K,OBP = OBPct,SLG = SlgPct,OPS,SB,CS,position,LastName,school,Name,yr,target)  %>%  mutate(label = paste(hitting$Name,"(",hitting$school,hitting$position,paste0(yr,'`'),")")) %>% mutate() %>% arrange(target,yr,LastName)%>% gt(groupname_col =  "label" ) %>% tab_header(title = md("**Hitters**"),subtitle = md("*BORAS CORP COLLEGE DRAFT ADVISEE & TARGET RESULTS – POS. PLAYERS (3/29 – 4/1) Sorted by Advisees/Targets, Draft Eligible Year, and then alphabetically - (Targets stats in red*)")) %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "70%",grand_summary_row.background.color = "purple",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>%fmt_missing(columns = vars(AVG,AB,R,H,RBI,BB,SO,SB,CS,`2B`,`3B`,`HR`), missing_text = 0) %>% fmt_number(columns = vars(AVG),decimals = 3)  %>% fmt_missing( 
  columns = vars(AVG,OBP,SLG,OPS),missing_text = "--" ) %>% cols_hide(columns = vars(LastName,Date,target,position,Name,yr,Team)) %>% tab_style(style = list(cell_fill(color = "lightblue")),locations = cells_body(rows = Opponent == "Season YTD") 
  ) %>% tab_style(style = list(cell_text(color = "Red")),locations = cells_body(rows = target == "Target")) 






# weekly hitters

test %>%
  gtsave(
    "Hittingnightly.pdf"
  )  

#######

YTD_adv %>% 
  gtsave(
    "YTDAdvHitters2.pdf",
  )  



#####


# filter ytd_clogs by date!!

gameday_logs$Date <- mdy(gameday_logs$Date)

logs <- gameday_logs %>% filter((Date >= "2021-04-03" & G == 1))

hitting <- rbind(logs,ytd_clogs)









######


night_logs <- rbind(Collins_logs_,Gelof_logs_) %>% mutate(OPS = " ")
carrer_logs <- rbind(Harrison_career,Goodwin_career,Gelof_career,Armstrong_career)

carrer_logs <- carrer_logs %>% mutate("OPS" = as.numeric(OBPct) + as.numeric(SlgPct))

night_logs <- night_logs %>%  filter(Date == today()-2 | Date == "2001-01-01" )


fchris <- unique(rbind(night_logs,carrer_logs))