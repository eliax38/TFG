library(worldfootballR)
library(readxl)

extract_season <- function(passing, aerial_duels, df_list, df_name, season, stats, stats_real, stats_imp, stats_ls, big_comps, new_row, last = FALSE, GK = FALSE){
  alt_stats = list("MP" = "MP_Time", "MP_Time" = "MP")
  
  if (dim(df_list[[df_name]])[1] > 0){  # Comprobamos que exista info del jugador para esa stat
    stats_season = df_list[[df_name]][df_list[[df_name]]$Season == season, ]  # Stats de cada tipo (GK, GK_adv, etc.)
    
    if(dim(stats_season)[1] == 0){  # Comprobamos que exista info para esa temporada, y elegimos el year si no existe
      stats_season = df_list[[df_name]][df_list[[df_name]]$Season == as.character(year), ]
    }
    
    if(last && is.null(new_row$Age)){
      new_row$Age = stats_season[1, "Age"] + 1
    }
    
    if(dim(stats_season)[1] != 0){  # Si hay info para la temporada la sumamos a los totales
      
      if(df_name == "passing"){
        for (row in 1:nrow(stats_season)){
          if (!(is.na(stats_season[row, "Att_Total"]) || is.na(stats_season[row, "Cmp_Total"]) || is.null(stats_season[row, "Att_Total"]) || is.null(stats_season[row, "Cmp_Total"]))){
            passing[1] = passing[1] + stats_season[row, "Att_Total"]
            passing[2] = passing[2] + stats_season[row, "Cmp_Total"]
          }
        }
      }
      
      if(df_name == "misc" && GK == FALSE){
        for (row in 1:nrow(stats_season)){
          if (!(is.null(stats_season[row, "Won_Aerial_Duels"]) || is.na(stats_season[row, "Won_Aerial_Duels"]) || is.na(stats_season[row, "Lost_Aerial_Duels"]) || is.null(stats_season[row, "Lost_Aerial_Duels"]))){
            aerial_duels[1] = aerial_duels[1] + stats_season[row, "Won_Aerial_Duels"] + stats_season[row, "Lost_Aerial_Duels"]
            aerial_duels[2] = aerial_duels[2] + stats_season[row, "Won_Aerial_Duels"]
          }
        }
      }
      
      for (comp in 1:nrow(stats_season)){
        comp_row = stats_season[comp, ]  # Fila de una competicion en la temporada anterior al traspaso
        bigComp = comp_row[["Comp"]] %in% big_comps
        
        for(possible_stat in stats[[df_name]]){
          
          stat = possible_stat
          
          if(is.null(comp_row[[stat]])){
            stat = alt_stats[[stat]]
          }
          
          if(is.null(stat)){
            print(paste0("Stat NULL for ", stats_season[1, "player_name"], ": ",possible_stat))
            next
          }
          
          if(is.null(comp_row[[stat]])){
            print(paste0("Stat not available for ", stats_season[1, "player_name"], ": ", possible_stat))
            next
          }
          
          if (!(is.na(comp_row[[stat]]))){
            if (stats_real[[stat]] %in% names(new_row)){
              new_row[[stats_real[[stat]]]] = new_row[[stats_real[[stat]]]] + comp_row[[stat]]
            }else{
              new_row[stats_real[[stat]]] = comp_row[[stat]]
            }
            
            
            if (stat %in% stats_ls && last){
              
              if (paste(stats_real[[stat]], "LS", sep="_") %in% names(new_row)){
                new_row[[paste(stats_real[[stat]], "LS", sep="_")]] = new_row[[paste(stats_real[[stat]], "LS", sep="_")]] + comp_row[[stat]]
              }else{
                new_row[paste(stats_real[[stat]], "LS", sep="_")] = comp_row[[stat]]
              }
            }
          }
          
          if (stat %in% stats_imp){
            if (!(paste(stats_real[[stat]], "BC", sep="_") %in% names(new_row))){
              new_row[paste(stats_real[[stat]], "BC", sep="_")] = 0
            }
            
            if (!(paste(stats_real[[stat]], "BC", "LS", sep="_") %in% names(new_row))){
              new_row[paste(stats_real[[stat]], "BC", "LS", sep="_")] = 0
            }
          }
          
          if (stat %in% stats_imp && bigComp){
            
            stat_value = comp_row[[stat]]
            if(is.na(stat_value)){
              stat_value = 0
            }
            
            if (paste(stats_real[[stat]], "BC", sep="_") %in% names(new_row)){
              new_row[[paste(stats_real[[stat]], "BC", sep="_")]] = new_row[[paste(stats_real[[stat]], "BC", sep="_")]] + stat_value
            }else{
              new_row[paste(stats_real[[stat]], "BC", sep="_")] = stat_value
            }
            
            if (last){
              if (paste(stats_real[[stat]], "BC", "LS", sep="_") %in% names(new_row)){
                new_row[[paste(stats_real[[stat]], "BC", "LS", sep="_")]] = new_row[[paste(stats_real[[stat]], "BC", "LS", sep="_")]] + stat_value
              }else{
                new_row[paste(stats_real[[stat]], "BC", "LS", sep="_")] = stat_value
              }
            }
            
          }
        }
      }
    }
  }
  
  return(list("row" = new_row, "passing" = passing, "aerial" = aerial_duels))
}


fbref_tm = read_excel("tm_fbref.xlsx")

big_comps = c("1. Champions Lg", "1. Premier League", "1. Serie A", "1. La Liga", "1. Ligue 1", "1. Bundesliga")

stats_gk = list("keeper" = c('MP_Time', 'Starts_Time', 'Min_Time', 'GA', 'SoTA', 'CS', 'PKA_Penalty_Kicks', 'PKsv_Penalty_Kicks'), 
                "keeper_adv" = c('Opp_Crosses', 'Stp_Crosses'), "standard" = c('Gls', 'Ast'), "passing" = c('Cmp_Total'), 
                "gca" = c('GCA_GCA'), "misc" = c('CrdY', 'CrdR'))

stats_gk_real = list("Gls" = "Goals", "Ast" = "Assists", "Cmp_Total" = "Succ_passes", 
                     "GCA_GCA" = "GCA", "CrdY" = "Yellow_cards", "CrdR" = "Red_cards", "MP_Time" = "Matches_pl",
                     "Starts_Time" = "Starts", "Min_Time" = "Minutes_pl", "GA" = "GA", "SoTA" = "SoTA", "CS" = "CS",
                     "PKA_Penalty_Kicks" = "PKA", "PKsv_Penalty_Kicks" = "PKSv", "Opp_Crosses" = "Cross_ag", 
                     "Stp_Crosses" = "Cross_stp", 'MP' = "Matches_pl")

stats_gk_imp = c("MP_Time", "Starts_Time", "Min_Time", "GA", "SoTA")

stats_gk_ls = stats_gk_imp

keeper_stats = c("keeper", "keeper_adv", "standard", "passing", "gca", "misc")

# % --> passing, won aerial duels

stats_pitch = list("standard" = c('MP', 'Starts_Time', 'Min_Time', 'G_minus_PK', 'PK', 'Ast'),
                   "shooting" = c('Sh_Standard', 'SoT_Standard'), "passing" = c('Cmp_Total'),
                   "defense" = c('TklW_Tackles', "Blocks_Blocks", "Int"),
                   "gca" = c('GCA_GCA'), "misc" = c('CrdY', 'CrdR', 'Won_Aerial_Duels'),
                   "possession" = c('Touches_Touches', 'Att Pen_Touches', 'Succ_Take_Ons', 'Dis_Carries'))

stats_pitch_real = list('MP' = "Matches_pl", "Starts_Time" = "Starts", "Min_Time" = "Minutes_pl", "G_minus_PK" = "NP_goals",
                        "PK" = "Pen_goals", "Ast" = "Assists", 'Sh_Standard' = "Shoots", 'SoT_Standard' = "Shoots_on_target",
                        "Cmp_Total" = "Succ_passes", 'TklW_Tackles' = "Tackles_won", "Blocks_Blocks" = "Blocks",
                        "Int" = "Interceptions","GCA_GCA" = "GCA", "CrdY" = "Yellow_cards", "CrdR" = "Red_cards",
                        "Won_Aerial_Duels" = "Won_aerial_duels", 'Touches_Touches' = "Touches",
                        'Att Pen_Touches' = "Att_pen_touches", 'Succ_Take_Ons' = "Succ_take_ons", 'Dis_Carries' = "Dis_Carries",
                        'MP_Time' = "Matches_pl")

stats_pitch_imp = c("MP", "Starts_Time", "Min_Time", "G_minus_PK", "PK", "Ast", "GCA_GCA")

stats_pitch_ls = c("MP", "Starts_Time", "Min_Time", "G_minus_PK", "PK", "Ast", "GCA_GCA", "TklW_Tackles", "Blocks_Blocks", "Int")

pitch_stats = c("standard", "shooting", "passing", "defense", "gca", "misc", "possession")

pitch_players = data.frame(
  Name = character(), Age = numeric(), Position = character(), Team_from = character(), Team_to = character(), Exp_contr = numeric(), Year = numeric(),
  Matches_pl = numeric(), Starts = numeric(), Minutes_pl = numeric(), NP_goals = numeric(), Pen_goals = numeric(), Assists = numeric(),
  Shoots = numeric(), Shoots_on_target = numeric(), Succ_passes = numeric(), Succ_passes_perc = numeric(), GCA = numeric(), 
  Tackles_won = numeric(), Blocks = numeric(), Interceptions = numeric(),
  Touches = numeric(), Att_pen_touches = numeric(), Succ_take_ons = numeric(), Dis_Carries = numeric(),
  Yellow_cards = numeric(), Red_cards = numeric(), Won_aerial_duels = numeric(), Won_aerial_duels_perc = numeric(),
  Matches_pl_BC = numeric(), Starts_BC = numeric(), Minutes_pl_BC = numeric(), NP_goals_BC = numeric(), Pen_goals_BC = numeric(),
  GCA_BC = numeric(), Assists_BC = numeric(),
  Matches_pl_LS = numeric(), Starts_LS = numeric(), Minutes_pl_LS = numeric(), NP_goals_LS = numeric(), Pen_goals_LS = numeric(), 
  GCA_LS = numeric(), Assists_LS = numeric(), Tackles_won_LS = numeric(), Blocks_LS = numeric(), Interceptions_LS = numeric(),
  Matches_pl_BC_LS = numeric(), Starts_BC_LS = numeric(), Minutes_pl_BC_LS = numeric(), NP_goals_BC_LS = numeric(), Pen_goals_BC_LS = numeric(),
  GCA_BC_LS = numeric(), Assists_BC_LS = numeric(),
  Transfer_value = numeric()
)

goalkeepers = data.frame(
  Name = character(), Age = numeric(), Position = character(), Team_from = character(), Team_to = character(), Exp_contr = numeric(), Year = numeric(), 
  Matches_pl = numeric(), Starts = numeric(), Minutes_pl = numeric(), GA = numeric(), SoTA = numeric(), CS = numeric(),
  PKA = numeric(), PKSv = numeric(), Cross_ag = numeric(), Cross_stp = numeric(),
  Goals = numeric(), Assists = numeric(), Succ_passes = numeric(), Succ_passes_perc = numeric(), GCA = numeric(), Yellow_cards = numeric(), Red_cards = numeric(),
  Matches_pl_BC = numeric(), Starts_BC = numeric(), Minutes_pl_BC = numeric(), GA_BC = numeric(), SoTA_BC = numeric(),
  Matches_pl_LS = numeric(), Starts_LS = numeric(), Minutes_pl_LS = numeric(), GA_LS = numeric(), SoTA_LS = numeric(),
  Matches_pl_BC_LS = numeric(), Starts_BC_LS = numeric(), Minutes_pl_BC_LS = numeric(), GA_BC_LS = numeric(), SoTA_BC_LS = numeric(),
  Transfer_value = numeric()
)

# nrow(fbref_tm), first:last

first = 6901
last = nrow(fbref_tm)

for(i in first:last){
  
  if(i%%5 == 0){
    print(i)
  }
  
  row = fbref_tm[i, ]
  
  name = row[["PlayerFBref"]]
  url_fbref = row[["UrlFBref"]]
  url_tm = row[["UrlTmarkt"]]
  position = row[["TmPos"]]
  
  # print(paste(name, position, sep=' -> '))
  
  transfers <- try(tm_player_transfer_history(url_tm), silent = TRUE)
  
  # Verificar si hubo un error
  # if (inherits(transfers, "try-error")) {
  #   next
  # }
  
  if (!("Transfer" %in% transfers[transfers$transfer_date>as.Date("2019-03-31"),]$transfer_type)){
    next
  }
  
  if(position == "Goalkeeper"){
     
    df_list_gk = list()
    
    for (k in keeper_stats){
      df_list_gk[[k]] = fb_player_season_stats(url_fbref, k)
      Sys.sleep(3)
    }
    
    for(t in 1:nrow(transfers)){
      passing = c(0, 0)
      transfer_row = transfers[t, ]
      
      date = transfer_row[["transfer_date"]]
      splitted_date = unlist(strsplit(as.character(date), '-'))
      
      month = as.numeric(splitted_date[2])
      year = as.numeric(splitted_date[1])
      
      transfer_type = transfer_row[["transfer_type"]]
      
      if(date>as.Date("2019-03-31") && month > 5 && month < 10 && !(is.na(transfer_type)) && transfer_type == "Transfer"){
        new_row <- list(Name = name)
        
        new_row$Exp_contr = as.numeric(transfer_row[["days_remaining"]])%/%30
        new_row$Team_from = transfer_row[["team_from"]]
        new_row$Team_to = transfer_row[["team_to"]]
        new_row$Transfer_value = transfer_row[["transfer_value"]]
        new_row$Position = position
        new_row$Name = name
        new_row$Year = year
        
        season = paste(as.character(year-1), as.character(year), sep = "-")
        previous_season = paste(as.character(year-2), as.character(year-1), sep = "-")
        pre_previous_season = paste(as.character(year-3), as.character(year-2), sep = "-")
        
        for(df_name in names(df_list_gk)){
          season_res = extract_season(passing, c(0, 0), df_list_gk, df_name, season, stats_gk, stats_gk_real, stats_gk_imp, stats_gk_ls, big_comps, new_row, last = TRUE, GK = TRUE)
          new_row = season_res[["row"]]
          passing = season_res[["passing"]]
          
          previous_season_res = extract_season(passing, c(0, 0), df_list_gk, df_name, previous_season, stats_gk, stats_gk_real, stats_gk_imp, stats_gk_ls, big_comps, new_row, last = FALSE, GK = TRUE)
          new_row = previous_season_res[["row"]]
          passing = previous_season_res[["passing"]]
          
          pre_previous_season = extract_season(passing, c(0, 0), df_list_gk, df_name, pre_previous_season, stats_gk, stats_gk_real, stats_gk_imp, stats_gk_ls, big_comps, new_row, last = FALSE, GK = TRUE)
          new_row = pre_previous_season[["row"]]
          passing = pre_previous_season[["passing"]]
        }
        
        if(!(is.na(passing[1])) && passing[1] > 0){
          new_row["Succ_passes_perc"] = round((passing[2]/passing[1])*100, digits=2)
        }
        
        for (n in names(goalkeepers)){
          if (!(n %in% names(new_row))){
            new_row[n] = NA
          }
        }
        
        df_row <- as.data.frame(new_row)
        goalkeepers <- rbind(goalkeepers, df_row)
      }
      
      
    }
    
    ###############################################################################################################
    
  }else{ 
    
    df_list = list()
    
    for (s in pitch_stats){
      df_list[[s]] = fb_player_season_stats(url_fbref, s)
      Sys.sleep(3)
    }
    
    for(t in 1:nrow(transfers)){
      passing = c(0, 0)
      aerial_duels = c(0, 0)
      transfer_row = transfers[t, ]
      
      date = transfer_row[["transfer_date"]]
      splitted_date = unlist(strsplit(as.character(date), '-'))
      
      month = as.numeric(splitted_date[2])
      year = as.numeric(splitted_date[1])
      
      transfer_type = transfer_row[["transfer_type"]]
      
      if(date>as.Date("2019-03-31") && month > 5 && month < 10 && !(is.na(transfer_type)) && transfer_type == "Transfer"){
        new_row <- list(Name = name)
        
        new_row$Exp_contr = as.numeric(transfer_row[["days_remaining"]])%/%30
        new_row$Team_from = transfer_row[["team_from"]]
        new_row$Team_to = transfer_row[["team_to"]]
        new_row$Transfer_value = transfer_row[["transfer_value"]]
        new_row$Position = position
        new_row$Name = name
        new_row$Year = year
        
        season = paste(as.character(year-1), as.character(year), sep = "-")
        previous_season = paste(as.character(year-2), as.character(year-1), sep = "-")
        pre_previous_season = paste(as.character(year-3), as.character(year-2), sep = "-")
        
        for(df_name in names(df_list)){
          season_res = extract_season(passing, aerial_duels, df_list, df_name, season, stats_pitch, stats_pitch_real, stats_pitch_imp, stats_pitch_ls, big_comps, new_row, last = TRUE)
          new_row = season_res[["row"]]
          passing = season_res[["passing"]]
          aerial_duels = season_res[["aerial"]]
          
          previous_season_res = extract_season(passing, aerial_duels, df_list, df_name, previous_season, stats_pitch, stats_pitch_real, stats_pitch_imp, stats_pitch_ls, big_comps, new_row, last = FALSE)
          new_row = previous_season_res[["row"]]
          passing = previous_season_res[["passing"]]
          aerial_duels = previous_season_res[["aerial"]]
          
          pre_previous_season_res = extract_season(passing, aerial_duels, df_list, df_name, pre_previous_season, stats_pitch, stats_pitch_real, stats_pitch_imp, stats_pitch_ls, big_comps, new_row, last = FALSE)
          new_row = pre_previous_season_res[["row"]]
          passing = pre_previous_season_res[["passing"]]
          aerial_duels = pre_previous_season_res[["aerial"]]
        }
        
        new_row["Succ_passes_perc"] = round((passing[2]/passing[1])*100, digits=2)
        
        if(!(is.na(aerial_duels[1])) && aerial_duels[1] > 0){
          new_row["Won_aerial_duels_perc"] = round((aerial_duels[2]/aerial_duels[1])*100, digits=2)
        }
        
        if(!(is.na(passing[1])) && passing[1] > 0){
          new_row["Succ_passes_perc"] = round((passing[2]/passing[1])*100, digits=2)
        }
        
        for (n in names(pitch_players)){
          if (!(n %in% names(new_row))){
            new_row[n] = NA
          }
        }
        
        df_row <- as.data.frame(new_row)
        pitch_players <- rbind(pitch_players, df_row)
      }
      
    }
  }
  
}


library(openxlsx)
# last = 6705
write.xlsx(pitch_players, paste0("./transfers_pitch/pitch_", as.character(first), "_", as.character(last), ".xlsx"))
write.xlsx(goalkeepers, paste0("./transfers_gk/gk_", as.character(first), "_", as.character(last), ".xlsx"))


# Unimos los excel

archivos <- list.files("./transfers_pitch", full.names = TRUE)

for (archivo in archivos) {
  datos <- read_excel(archivo)
  pitch_players = rbind(pitch_players, datos)
}

archivos <- list.files("./transfers_gk", full.names = TRUE)

for (archivo in archivos) {
  datos <- read_excel(archivo)
  goalkeepers = rbind(goalkeepers, datos)
}


write.xlsx(pitch_players, "df_pitch.xlsx")
write.xlsx(goalkeepers, "df_goalkeepers.xlsx")
