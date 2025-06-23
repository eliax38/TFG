library(openxlsx)
library(readxl)

setwd("C:/Users/elias/OneDrive/Escritorio/TFG/models")

transfers_pitch = read_excel("./train_data/df_pitch_unique.xlsx")
transfers_gk = read_excel("./train_data/df_goalkeepers_unique.xlsx")

current_pitch = read_excel("./current/current_pitch.xlsx")
current_gk = read_excel("./current/current_gk.xlsx")

# TRANSFERS_PITCH

for (name in colnames(transfers_pitch)) {
  print(summary(transfers_pitch[name]))
}

current_gk = current_gk[!is.na(current_gk$Matches_pl),]

write.xlsx(transfers_pitch, "transfers_pitch.xlsx")
write.xlsx(transfers_gk, "transfers_gk.xlsx")
write.xlsx(current_pitch, "current_pitch.xlsx")
write.xlsx(current_gk, "current_gk.xlsx")

nas = transfers_pitch[is.na(transfers_pitch$Matches_pl_LS),]







