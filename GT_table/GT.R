Hitting_weekly <- hitting %>% mutate(OPS = OBPct + SlgPct ) %>% select(Opponent,Result,AVG = BA,Team = school,Date,target,AB,R,H,`2B`,`3B`,HR,RBI,BB,SO = K,OBP = OBPct,SLG = SlgPct,OPS,SB,CS,position,LastName,school,Name,yr,target)  %>%  mutate(label = paste(hitting$Name,"(",hitting$school,hitting$position,paste0(yr,'`'),")")) %>% mutate() %>% arrange(target,yr,LastName)%>% gt(groupname_col =  "label" ) %>% tab_header(title = md("**Hitters**"),subtitle = md("*BORAS CORP COLLEGE DRAFT ADVISEE & TARGET RESULTS – POS. PLAYERS (3/29 – 4/1) Sorted by Advisees/Targets, Draft Eligible Year, and then alphabetically - (Targets stats in red*)")) %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "70%",grand_summary_row.background.color = "purple",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>%fmt_missing(columns = vars(AVG,AB,R,H,RBI,BB,SO,SB,CS,`2B`,`3B`,`HR`), missing_text = 0) %>% fmt_number(columns = vars(AVG),decimals = 3)  %>% fmt_missing( 
  columns = vars(AVG,OBP,SLG,OPS),missing_text = "--" ) %>% cols_hide(columns = vars(LastName,Date,target,position,Name,yr,Team)) %>% tab_style(style = list(cell_fill(color = "lightblue")),locations = cells_body(rows = Opponent == "Season YTD") 
  ) %>% tab_style(style = list(cell_text(color = "Red")),locations = cells_body(rows = target == "Target")) 






# weekly hitters

test %>%
  gtsave(
    "Hittingnightly.pdf"
  )  


# DAY
DAY <- day_hitters2 %>% mutate(Player = paste(day_hitters2$Name,day_hitters2$yr,"(",day_hitters2$school,day_hitters2$position,")")) %>%  select (Player,Opponent,Result,AVG = BA,Team = school,Date,target,AB,R,H,`2B`,`3B`,HR,RBI,BB,SO = K,OBP = OBPct,SLG = SlgPct,OPS,SB,CS,position,LastName,school,Name,yr,target)  %>% arrange(target,LastName,Date,target) %>% filter(target != "Target")  %>%  gt(groupname_col = "Player") %>% tab_options(column_labels.background.color = "black") %>%   cols_hide(columns = c("Date","LastName","target","Team")) %>% tab_options(table.width = "70%",  row_group.background.color = "lightblue",column_labels.hidden = T) %>% tab_style(style = list(cell_fill(color = "Black")),locations = cells_body(rows = Opponent == "Opponent"))  %>% tab_style(style = list(cell_text(color = "white")),locations = cells_body(rows = Opponent == "Opponent")) %>% cols_align(align = "center") %>% tab_options(data_row.padding = px(1/2)) %>% tab_options(row_group.padding = px(12)) %>% tab_style(style = list(cell_text(color = "black")),locations = cells_body(rows = Opponent == "Season YTD")) %>% tab_style(style = list(cell_fill(color = "grey")),locations = cells_body(rows = Opponent == "Season YTD")) %>% cols_hide(columns = c("Name","yr","position","Player")) %>% fmt_missing(c("R","2B","3B","HR","RBI","BB","SO","SB","CS","H","AB"), rows = NULL, missing_text = 0) %>%  tab_header(title = md("**Hitters**"),subtitle = md("*(4/27/2021)*")) %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "75%",grand_summary_row.background.color = "purple",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% tab_options(row_group.font.weight = 'bolder',row_group.font.size = 28,row_group.border.bottom.style = "all",row_group.border.top.color = "white",row_group.border.top.width = 38) %>% fmt_missing(c("AVG","OBP","SLG","OPS","Result"),missing_text = " ")




TODAY <- PITCHERS_DAY2 %>% mutate(Player = paste(PITCHERS_DAY2$Name,PITCHERS_DAY2$yr,"(",PITCHERS_DAY2$school,PITCHERS_DAY2$PT,")")) %>%  rename(HR = `HR-A`) %>% filter(target != "Target") %>% filter(IP == 'IP' | IP != 0 ) %>%  select(Opponent,Result,Name,Team,ERA,IP,H,`2B` = `2B-A`,`3B` = `3B-A`,HR,R,ER,BB,SO,BF,AB,P = Pitches,AB,W,L,WHIP,`K/9`,BAA,APP = App,GS,target,LastName,yr,target,Player) %>%  arrange(target,yr,LastName)%>% gt(groupname_col ="Player")%>% tab_header(title = md("**Pitchers**"),subtitle = md("*(4/27/21)*"))  %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "75%",grand_summary_row.background.color = "Navy",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2, row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold",column_labels.hidden = T) %>% fmt_missing(columns = vars(ERA,IP,H,HR,R,ER,BB,SO,AB,W,L,WHIP,`K/9`,BAA,APP,GS,BF,Result),missing_text = "") %>% cols_hide(vars(Name,Team,LastName,yr,target)) %>% tab_style(style = list(cell_fill(color = "Grey")),locations = cells_body(rows = Opponent == "Season YTD") 
) %>% tab_options(row_group.font.weight = 'bolder',row_group.font.size = 28,row_group.border.bottom.style = "bottom",row_group.border.top.color = "white",row_group.border.top.width = 38)  %>% tab_style(style = list(cell_fill(color = "Black")),locations = cells_body(rows = Opponent == "Opponent") 
)  %>% tab_style(style = list(cell_text(color = "White")),locations = cells_body(rows = Opponent == "Opponent") 
) 


TODAY  %>% 
  gtsave(
    "PitcherLogsNight.pdf", expand = 10000,
  )  

##################### weekly logs

WEEKLY <- pitchers_day %>% filter(Date == "2001-02-02" | (Date >= "2021-04-19" & App == 1))


WEEKLY2 <- rbind(WEEKLY,career_logs)

######Advisees

WEEKLY7 <- WEEKLY2 %>% mutate(Player = paste(WEEKLY2$Name,WEEKLY2$yr,"(",WEEKLY2$school,WEEKLY2$PT,")")) %>% filter(target != "Target") %>%  rename(HR = `HR-A`) %>% select(Opponent,Result,Name,Team,ERA,IP,H,`2B` = `2B-A`,`3B` = `3B-A`,HR,R,ER,BB,SO,BF,AB,P = Pitches,AB,W,L,WHIP,`K/9`,BAA,APP = App,GS,target,LastName,yr,target,Player) %>%  arrange(target,yr,LastName)%>% gt(groupname_col ="Player")%>% tab_header(title = md("**Advisees**"),subtitle = md("*4/19/2021 -- 4/25/2021*"))  %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "75%",grand_summary_row.background.color = "Navy",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2, row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% fmt_missing(columns = vars(ERA,IP,H,HR,R,ER,BB,SO,AB,W,L,WHIP,`K/9`,BAA,APP,GS,BF),missing_text = "") %>% cols_hide(vars(Name,Team,LastName,yr,target)) %>% tab_style(style = list(cell_fill(color = "Grey")),locations = cells_body(rows = Opponent == "Season YTD") 
) %>% tab_options(row_group.font.weight = 'bolder',row_group.font.size = 28,row_group.border.bottom.style = "bottom",column_labels.hidden = T,row_group.border.top.color = "white",row_group.border.top.width = 38)  %>% tab_style(style = list(cell_fill(color = "Black")),locations = cells_body(rows = Opponent == "Opponent") 
)  %>% tab_style(style = list(cell_text(color = "White")),locations = cells_body(rows = Opponent == "Opponent") 
) 

##Advisees

WEEKLY7  %>% 
  gtsave(
    "PitcherLogs.pdf", expand = 10000,
  )  


## Targets

WEEKLY2 <- WEEKLY2 %>% filter(LastName != "Hill")

TARGET7 <- WEEKLY2 %>% mutate(Player = paste(WEEKLY2$Name,WEEKLY2$yr,"(",WEEKLY2$school,WEEKLY2$PT,")")) %>% filter(target == "Target") %>%  rename(HR = `HR-A`) %>% select(Opponent,Result,Name,Team,ERA,IP,H,`2B` = `2B-A`,`3B` = `3B-A`,HR,R,ER,BB,SO,BF,AB,P = Pitches,AB,W,L,WHIP,`K/9`,BAA,APP = App,GS,target,LastName,yr,target,Player) %>%  arrange(target,yr,LastName)%>% gt(groupname_col ="Player")%>% tab_header(title = md("**Targets**"),subtitle = md("*4/19/2021 -- 4/25/2021*"))  %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "75%",grand_summary_row.background.color = "Navy",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2, row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% fmt_missing(columns = vars(ERA,IP,H,HR,R,ER,BB,SO,AB,W,L,WHIP,`K/9`,BAA,APP,GS,BF),missing_text = "") %>% cols_hide(vars(Name,Team,LastName,yr,target)) %>% tab_style(style = list(cell_fill(color = "Grey")),locations = cells_body(rows = Opponent == "Season YTD") 
) %>% tab_options(row_group.font.weight = 'bolder',row_group.font.size = 28,row_group.border.bottom.style = "bottom",row_group.border.top.color = "white",row_group.border.top.width = 38)  %>% tab_style(style = list(cell_fill(color = "Black")),locations = cells_body(rows = Opponent == "Opponent") 
)  %>% tab_style(style = list(cell_text(color = "White")),locations = cells_body(rows = Opponent == "Opponent") 
)  %>% tab_options(column_labels.hidden = T)






TARGET7  %>% 
  gtsave(
    "PitcherTargetLogs.pdf", expand = 10000,
  )  




# YTD Report ###################


YTD <- career_logs













ytd_final_target_adv <- YTD  %>% arrange(target,yr,LastName) %>% filter(target != "Target") %>% rename(HR = `HR-A`,`2B` = `2B-A`) %>% select(Name,Team,ERA,IP,H,`2B`,HR,R,ER,BB,SO,BF,AB,W,L,WHIP,`K/9`,BAA,APP = App,GS,BF,target,LastName) %>% gt(groupname_col = "target" )%>% tab_header(title = md("**Pitchers**"),subtitle = md("*04-25-2021*")) %>% tab_options(heading.background.color = "#EFFBFC",stub.border.style = "dashed",stub.border.color = "#989898",stub.border.width = "1px",summary_row.border.color = "#989898",table.width = "90%",grand_summary_row.background.color = "Navy",column_labels.background.color = "black",table.font.color = "black",row_group.border.bottom.color = "black",row_group.border.bottom.width = 2,row_group.padding = 10,row_group.background.color = "#EFFBFC",stub.font.weight = "bold") %>% fmt_missing(columns = vars(ERA,IP,H,`2B`,HR,ER,BB,SO,AB,W,L,WHIP,`K/9`,BAA,APP,GS,BF)) %>% cols_hide(columns = vars(LastName)) %>% fmt_number(
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
    align = c("center"),
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







