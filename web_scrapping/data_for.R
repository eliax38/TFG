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

contracts_gk = read_excel("contracts_gk.xlsx")

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

goalkeepers = data.frame(
  Name = character(), Age = numeric(), Exp_contr = numeric(),
  Matches_pl = numeric(), Starts = numeric(), Minutes_pl = numeric(), GA = numeric(), SoTA = numeric(), CS = numeric(),
  PKA = numeric(), PKSv = numeric(), Cross_ag = numeric(), Cross_stp = numeric(),
  Goals = numeric(), Assists = numeric(), Succ_passes = numeric(), Succ_passes_perc = numeric(), GCA = numeric(), Yellow_cards = numeric(), Red_cards = numeric(),
  Matches_pl_BC = numeric(), Starts_BC = numeric(), Minutes_pl_BC = numeric(), GA_BC = numeric(), SoTA_BC = numeric(),
  Matches_pl_LS = numeric(), Starts_LS = numeric(), Minutes_pl_LS = numeric(), GA_LS = numeric(), SoTA_LS = numeric(),
  Matches_pl_BC_LS = numeric(), Starts_BC_LS = numeric(), Minutes_pl_BC_LS = numeric(), GA_BC_LS = numeric(), SoTA_BC_LS = numeric()
)

# nrow(contracts_gk), first:last

first = 237
last = nrow(contracts_gk)

for(i in first:last){
  
  if(i%%5 == 0){
    print(i)
  }
  
  row = contracts_gk[i, ]
  
  name = row[["name"]]
  url_fbref = row[["url_fbref"]]
  url_tm = row[["url_tm"]]
    
  df_list_gk = list()
  
  for (k in keeper_stats){
    df_list_gk[[k]] = fb_player_season_stats(url_fbref, k)
    Sys.sleep(3)
  }
  
  passing = c(0, 0)
  
  new_row <- list(Name = name)
    
  new_row$Exp_contr = row[["months"]]
  new_row$Name = name
  
  year = 2024
  
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


library(openxlsx)
# last = 6705
write.xlsx(goalkeepers, "current_gk_2.xlsx")


