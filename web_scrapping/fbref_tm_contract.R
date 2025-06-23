library(worldfootballR)
library(readxl)
library(openxlsx)

setwd("C:/Users/elias/OneDrive/Escritorio/TFG/scrapping")

contracts = read_excel("contracts.xlsx")
fbref_tm = read_excel("tm_fbref.xlsx")

contracts_gk = data.frame(name = character(), url_fbref = character(), url_tm = character(), months = numeric())
contracts_pitch = data.frame(name = character(), url_fbref = character(), url_tm = character(), position = character(), months = numeric())

for(i in 1:nrow(fbref_tm)){
  if (i%%500 == 0){
    print(i)
  }
  
  row = fbref_tm[i, ]
  
  url_tm = row[["UrlTmarkt"]]
  
  for(j in 1:nrow(contracts)){
    row_c = contracts[j, ]
    
    if(url_tm == row_c[["url"]]){
      months = row_c[["months"]]
      name = row[["PlayerFBref"]]
      url_fbref = row[["UrlFBref"]]
      position = row[["TmPos"]]
      
      if(position == "Goalkeeper"){
        new_row = list(name = name, url_fbref = url_fbref, url_tm = url_tm, months = months)
        contracts_gk = rbind(contracts_gk, new_row)
      }else{
        new_row = list(name = name, url_fbref = url_fbref, url_tm = url_tm, position = position, months = months)
        contracts_pitch = rbind(contracts_pitch, new_row)
      }
      
      break
    }
  }
}

write.xlsx(contracts_gk, "contracts_gk.xlsx")
write.xlsx(contracts_pitch, "contracts_pitch.xlsx")
