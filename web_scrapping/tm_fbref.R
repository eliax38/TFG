library(worldfootballR)
library(readxl)

setwd("C:/Users/elias/OneDrive/Escritorio/TFG/scrapping")

urls = player_dictionary_mapping()

# write.table(urls, file = "urls_tm.csv", sep = ";", row.names = FALSE, quote = FALSE)


# Obtenemos los jugadores que queremos analizar y no aparecen en el excel 

fbref = read_excel("pos_url.xlsx")
fbref_tm = read_excel("urls_tm.xlsx")

url_fbref = fbref$Url
url_fbref_tm = fbref_tm$UrlFBref

faltantes = c()

for (i in 1:length(url_fbref)){
  if (!(url_fbref[i] %in% url_fbref_tm)){
    faltantes = c(faltantes, url_fbref[i])
  }
}

############################################################


# Obtenemos datos de los jugadores deseados que militan fuera de las cinco grandes ligas

teams = c("https://www.transfermarkt.com/porto/startseite/verein/720", "https://www.transfermarkt.com/sl-benfica/startseite/verein/294", "https://www.transfermarkt.com/sporting-cp/startseite/verein/336",
          "https://www.transfermarkt.com/ajax-amsterdam/startseite/verein/610", "https://www.transfermarkt.com/psv-eindhoven/startseite/verein/383", "https://www.transfermarkt.com/feyenoord-rotterdam/startseite/verein/234",
          "https://www.transfermarkt.com/club-brugge-kv/startseite/verein/2282", "https://www.transfermarkt.com/celtic-glasgow/startseite/verein/371", "https://www.transfermarkt.com/gnk-dinamo-zagreb/startseite/verein/419",
          "https://www.transfermarkt.com/shakhtar-donetsk/startseite/verein/660", "https://www.transfermarkt.com/ac-sparta-prag/startseite/verein/197", "https://www.transfermarkt.com/sk-sturm-graz/startseite/verein/122",
          "https://www.transfermarkt.com/red-bull-salzburg/startseite/verein/409", "https://www.transfermarkt.com/red-star-belgrade/startseite/verein/159", "https://www.transfermarkt.com/slovan-bratislava/startseite/verein/540",
          "https://www.transfermarkt.com/bsc-young-boys/startseite/verein/452", "https://www.transfermarkt.com/botafogo-de-futebol-e-regatas/startseite/verein/537", "https://www.transfermarkt.com/sociedade-esportiva-palmeiras/startseite/verein/1023",
          "https://www.transfermarkt.com/cr-flamengo/startseite/verein/614", "https://www.transfermarkt.co.uk/ca-river-plate/startseite/verein/209", "https://www.transfermarkt.com/club-atletico-boca-juniors/startseite/verein/189"
          )

player_urls = c()
player_names = c()
player_pos = c()

for (team in teams){
  print(team)
  
  squad = tm_squad_stats(team)
  
  player_urls = c(player_urls, squad$player_url)
  player_names = c(player_names, squad$player_name)
  player_pos = c(player_pos, squad$player_pos)
}

##########################################################################


# Obtenemos un dataframe con la URL de los jugadores faltantes y el nombre con el que nos referimos a ellos

df_faltantes_fbref <- data.frame(
  nombre = c(),
  url = c()
)

for (url in faltantes){
  newRow = data.frame(
    nombre = tolower(unlist(strsplit(url, '/'))[7]),
    url = url
  )
  
  df_faltantes_fbref = rbind(df_faltantes_fbref, newRow)
}

################################################################################


#######################################################

faltantes_2 = c()
encontrados = c()

for (name in names_fbref){
  if (name %in% names_tm){
    encontrados = c(encontrados, name)
  }else{
    faltantes_2 = c(faltantes_2, name)
  }
}

####################################################

df <- data.frame(
  PlayerFBref = c(),
  UrlFBref = c(),
  UrlTmarkt = c(),
  TmPos = c()
)

for (i in 1:nrow(fbref_tm)){
  if (i%%1000 == 0){
    print(i)
  }
  
  row = fbref_tm[i, ]
  
  if (row$UrlFBref %in% url_fbref){
    df = rbind(df, row)
  }
}


for (url in 1:length(player_urls)){
  for (url2 in 1:nrow(df_faltantes_fbref)){
    if(unlist(strsplit(player_urls[url], '/'))[4] == df_faltantes_fbref[url2, "nombre"]){
      
      newRow = data.frame(
        PlayerFBref = player_names[url],
        UrlFBref = df_faltantes_fbref[url2, "url"],
        UrlTmarkt = player_urls[url],
        TmPos = player_pos[url]
      )
      
      df = rbind(df, newRow)
      break
    }
  }
}

write.xlsx(df, "tm_fbref.xlsx")

##############################################################################

# Metemos los faltantes a un dataframe

fbref = read_excel("pos_url.xlsx")
fbref_tm = read_excel("tm_fbref.xlsx")

fbref_url = fbref$Url
fbref_url_tm = fbref_tm$UrlFBref

faltantes = c()

for (player in fbref_url){
  if (!(player %in% fbref_url_tm)){
    faltantes = c(faltantes, player)
  }
}

df_faltantes = data.frame(
  url = faltantes
)

write.xlsx(df_faltantes, "faltantes_2.xlsx")

