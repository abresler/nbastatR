all_players <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1774150094' %>% 
  read_csv()

dl_moves <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=918513047' %>% 
  read_csv()

nba_moves  <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=136633448' %>% 
  read_csv()

intl_moves <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=2103167934' %>% 
  read_csv()
player_rights <- 
  "https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=45887821" %>% 
  read_csv()

draft_ratings <-
  "https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=775526807" %>% 
  read_csv()

current_coaches <- 
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=tsv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=824162697' %>% 
  read_csv()

pick_protection <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1952783473' %>% 
  read_csv()

rights_held <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1952783473' %>% 
  read_csv()

trade_history <- 
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1952783473' %>% 
  read_csv(col_names = T)

player_tenure <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=2134829741' %>% 
  read_csv(col_names = T)

ncaa_players <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=308339921' %>% 
  read_csv(col_names = T)

vtb_players <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=850585129' %>% 
  read_csv(col_names = F)

turkey_players <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1170875830' %>% 
  read_csv(col_names = F)

spain_players <- 
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1856635138' %>% 
  read_csv(col_names = F)

poland_players <- 
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=347331898' %>% 
  read_csv(col_names = F)

lithuiana <- 
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=515759809'

italy <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1136495205'

israel <- 
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=401932258'

greece <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1591433225'

germany <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=12407622' %>% 
  read_csv(col_names = F)

france <- 
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=436957937' %>% 
  read_csv(col_names = F)

belgium <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1590552025'

aba <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1548176791' %>% 
  read_csv(col_names = F)

d_league <- 
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1997732214' %>% 
  read_csv(col_names = F)

## 
nba  <-
  'https://docs.google.com/spreadsheets/d/1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0/export?format=csv&id=1IyJaUOA_aiSinn0IrhEqW2Ka7WEJDRZAz3q8PBjL3i0&gid=1538599335' %>% 
  read_csv(col_names = F)
