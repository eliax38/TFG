library(readxl)

fbref_tm = read_excel("tm_fbref.xlsx")

characters = c()
positions = c()

for (i in 1:nrow(fbref_tm)){
  
  if(i%%1000==0){
    print(i)
  }
  
  row = fbref_tm[i, ]
  name = row[["PlayerFBref"]]
  
  name_chars = unlist(strsplit(name, split = ""))
  
  for (ch in 1:length(name_chars)){
    if (!(name_chars[ch] %in% characters)){
      characters = c(characters, name_chars[ch])
      positions = c(positions, name)
    }
  }
}

players = c()
url_fbref = c()
url_tm = c()

for (i in 1:nrow(fbref_tm)){
  
  if(i%%1000==0){
    print(i)
  }
  
  row = fbref_tm[i, ]
  
  name = row[["PlayerFBref"]]
  url_f = row[["UrlFBref"]]
  url_t = row[["UrlTmarkt"]]
  
  if (!(name %in% players)){
    players = c(players, name)
  }else{
    print(name)
  }
  
  if (!(url_f %in% url_fbref)){
    url_fbref = c(url_fbref, url_f)
  }else{
    print(url_f)
  }
  
  if (!(url_t %in% url_tm)){
    url_tm = c(url_tm, url_t)
  }else{
    print(url_t)
  }
}


positions = c()

for (i in 1:nrow(fbref_tm)){
  
  if(i%%1000==0){
    print(i)
  }
  
  row = fbref_tm[i, ]
  
  name = row[["TmPos"]]
  
  if (!(name %in% positions)){
    positions = c(positions, name)
  }
}

print(positions)


library(readxl)
initial_fbref_tm = read_excel("urls_tm.xlsx")

url_fbref = c()
url_tm = c()

for (i in 1:nrow(initial_fbref_tm)){
  
  if(i%%1000==0){
    print(i)
  }
  
  row = initial_fbref_tm[i, ]
  
  url_f = row[["UrlFBref"]]
  url_t = row[["UrlTmarkt"]]
  
  if (!(url_f %in% url_fbref)){
    url_fbref = c(url_fbref, url_f)
  }else{
    print(url_f)
  }
  
  if (!(url_t %in% url_tm)){
    url_tm = c(url_tm, url_t)
  }else{
    print(url_t)
  }
}

