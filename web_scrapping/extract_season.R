extract_season <- function(passing, aerial_duels, df_list, df_name, season, stats, stats_real, stats_imp, stats_ls, big_comps, new_row, last = FALSE, GK = FALSE){
  alt_stats = list("MP" = "MP_Time", "MP_Time" = "MP")
  
  if (dim(df_list[[df_name]])[1] > 0){  # Comprobamos que exista info del jugador para esa stat
    stats_season = df_list[[df_name]][df_list[[df_name]]$Season == season, ]  # Stats de cada tipo (GK, GK_adv, etc.)
    
    if(dim(stats_season)[1] == 0){  # Comprobamos que exista info para esa temporada, y elegimos el year si no existe
      stats_season = df_list[[df_name]][df_list[[df_name]]$Season == as.character(year), ]
    }
    
    if(last && is.null(new_row$Age)){
      new_row$Age = stats_season[1, "Age"]
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

s = extract_season(c(0, 0), c(0, 0), df_list, df_name, season, stats_pitch, stats_pitch_real, stats_pitch_imp, stats_pitch_ls, big_comps, new_row, last = TRUE, GK = FALSE)
  
