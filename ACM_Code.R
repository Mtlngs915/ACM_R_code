## Load library plotrix
install.packages("plotrix")
library(plotrix)

##################################################################################################
################# Data Preparation
Bonaventura <- read.csv("D:/PGDDA/ACM_Blogpost/Midfielders/ACM_16-17_05_Bonaventura.csv")
Honda <- read.csv("D:/PGDDA/ACM_Blogpost/Midfielders/ACM_16-17_10_Honda.csv")
Fernandez <- read.csv("D:/PGDDA/ACM_Blogpost/Midfielders/ACM_16-17_14_Fernandez.csv")
Poli <- read.csv("D:/PGDDA/ACM_Blogpost/Midfielders/ACM_16-17_16_Poli.csv")
Montolivo <- read.csv("D:/PGDDA/ACM_Blogpost/Midfielders/ACM_16-17_18_Montolivo.csv")
Sosa <- read.csv("D:/PGDDA/ACM_Blogpost/Midfielders/ACM_16-17_23_Sosa.csv")
Kucka <- read.csv("D:/PGDDA/ACM_Blogpost/Midfielders/ACM_16-17_33_Kucka.csv")
Locatelli <- read.csv("D:/PGDDA/ACM_Blogpost/Midfielders/ACM_16-17_73_Locatelli.csv")
Pasalic <- read.csv("D:/PGDDA/ACM_Blogpost/Midfielders/ACM_16-17_80_Pasalic.csv")
Bertolacci <- read.csv("D:/PGDDA/ACM_Blogpost/Midfielders/ACM_16-17_91_Bertolacci.csv")
ACM_Performancesheet <- read.csv("D:/PGDDA/ACM_Blogpost/ACM_16-17_Performancesheet.csv")
ACM_Scoresheet <- read.csv("D:/PGDDA/ACM_Blogpost/ACM_16-17_Scoresheet.csv")

str(Bertolacci)
str(Bonaventura)
str(Fernandez)
str(Honda)
str(Kucka)
str(Locatelli)
str(Montolivo)
str(Pasalic)
str(Poli)
str(Sosa)
str(ACM_Performancesheet)
str(ACM_Scoresheet)

################# Missing Data
sum(is.na(Bonaventura))
## 0 values
sum(is.na(Honda))
## 0 values
sum(is.na(Fernandez))
## 0 values
sum(is.na(Poli))
## 0 values
sum(is.na(Montolivo))
## 0 values
sum(is.na(Sosa))
## 0 values
sum(is.na(Kucka))
## 0 values
sum(is.na(Locatelli))
## 0 values
sum(is.na(Pasalic))
## 0 values
sum(is.na(Bertolacci))
## 0 values
sum(is.na(ACM_Performancesheet))
## 0 values
sum(is.na(ACM_Scoresheet))
## 0 values

################# EDA
boxplot_berto <- boxplot.stats(Bertolacci$Bertolacci_minutes_played)$out
boxplot_berto
boxplot_bona  <- boxplot.stats(Bonaventura$Bonaventura_minutes_played)$out
boxplot_bona
boxplot_frnndz<- boxplot.stats(Fernandez$Fernandez_minutes_played)$out
boxplot_frnndz
boxplot_honda <- boxplot.stats(Honda$Honda_minutes_played)$out
boxplot_honda
boxplot_kucka <- boxplot.stats(Kucka$Kucka_minutes_played)$out
boxplot_kucka
boxplot_loca  <- boxplot.stats(Locatelli$Locatelli_minutes_played)$out
boxplot_loca 
boxplot_monto <- boxplot.stats(Montolivo$Montolivo_minutes_played)$out
boxplot_monto
boxplot_pasa  <- boxplot.stats(Pasalic$Pasalic_minutes_played)$out
boxplot_pasa 
boxplot_poli  <- boxplot.stats(Poli$Poli_minutes_played)$out
boxplot_poli
boxplot_sosa  <- boxplot.stats(Sosa$Sosa_minutes_played)$out
boxplot_sosa

## Bertolacci, Fernandez, Honda, Montolivo and Poli were the lesser played
## midfielders

################# Variable Formation

Minutes_Played <- data.frame(Bertolacci$Bertolacci_minutes_played,
                             Bonaventura$Bonaventura_minutes_played,
                             Fernandez$Fernandez_minutes_played,
                             Honda$Honda_minutes_played,
                             Kucka$Kucka_minutes_played,
                             Locatelli$Locatelli_minutes_played,
                             Montolivo$Montolivo_minutes_played,
                             Pasalic$Pasalic_minutes_played,
                             Poli$Poli_minutes_played,
                             Sosa$Sosa_minutes_played)
colnames(Minutes_Played) <- c("Bertolacci_minutes_played",
                              "Bonaventura_minutes_played",
                              "Fernandez_minutes_played",
                              "Honda_minutes_played",
                              "Kucka_minutes_played",
                              "Locatelli_minutes_played",
                              "Montolivo_minutes_played",
                              "Pasalic_minutes_played",
                              "Poli_minutes_played",
                              "Sosa_minutes_played") 

Opening_Goal <- data.frame(Bertolacci$Bertolacci_scored_opening_goal,
                           Bonaventura$Bonaventura_scored_opening_goal,
                           Fernandez$Fernandez_scored_opening_goal,
                           Honda$Honda_scored_opening_goal,
                           Kucka$Kucka_scored_opening_goal,
                           Locatelli$Locatelli_scored_opening_goal,
                           Montolivo$Montolivo_scored_opening_goal,
                           Pasalic$Pasalic_scored_opening_goal,
                           Poli$Poli_scored_opening_goal,
                           Sosa$Sosa_scored_opening_goal)
colnames(Opening_Goal) <- c("Bertolacci",
                            "Bonaventura",
                            "Fernandez",
                            "Honda",
                            "Kucka",
                            "Locatelli",
                            "Montolivo",
                            "Pasalic",
                            "Poli",
                            "Sosa")

Equalizer <- data.frame(Bertolacci$Bertolacci_scored_equalizer,
                        Bonaventura$Bonaventura_scored_equalizer,
                        Fernandez$Fernandez_scored_equalizer,
                        Honda$Honda_scored_equalizer,
                        Kucka$Kucka_scored_equalizer,
                        Locatelli$Locatelli_scored_equalizer,
                        Montolivo$Montolivo_scored_equalizer,
                        Pasalic$Pasalic_scored_equalizer,
                        Poli$Poli_scored_equalizer,
                        Sosa$Sosa_scored_equalizer)
colnames(Equalizer) <-  c("Bertolacci",
                          "Bonaventura",
                          "Fernandez",
                          "Honda",
                          "Kucka",
                          "Locatelli",
                          "Montolivo",
                          "Pasalic",
                          "Poli",
                          "Sosa")

Winning_Goal <- data.frame(Bertolacci$Bertolacci_scored_winning_goal,
                           Bonaventura$Bonaventura_scored_winning_goal,
                           Fernandez$Fernandez_scored_winning_goal,
                           Honda$Honda_scored_winning_goal,
                           Kucka$Kucka_scored_winning_goal,
                           Locatelli$Locatelli_scored_winning_goal,
                           Montolivo$Montolivo_scored_winning_goal,
                           Pasalic$Pasalic_scored_winning_goal,
                           Poli$Poli_scored_winning_goal,
                           Sosa$Sosa_scored_winning_goal)                      
colnames(Winning_Goal) <-  c("Bertolacci",
                             "Bonaventura",
                             "Fernandez",
                             "Honda",
                             "Kucka",
                             "Locatelli",
                             "Montolivo",
                             "Pasalic",
                             "Poli",
                             "Sosa")

goals_00_15   <-  data.frame(Bertolacci$Bertolacci_goals_scored_00.15,
                             Bonaventura$Bonaventura_goals_scored_00.15,
                             Fernandez$Fernandez_goals_scored_00.15,
                             Honda$Honda_goals_scored_00.15,
                             Kucka$Kucka_goals_scored_00.15,
                             Locatelli$Locatelli_goals_scored_00.15,
                             Montolivo$Montolivo_goals_scored_00.15,
                             Pasalic$Pasalic_goals_scored_00.15,
                             Poli$Poli_goals_scored_00.15,
                             Sosa$Sosa_goals_scored_00.15)
colnames(goals_00_15) <-  c("Bertolacci",
                            "Bonaventura",
                            "Fernandez",
                            "Honda",
                            "Kucka",
                            "Locatelli",
                            "Montolivo",
                            "Pasalic",
                            "Poli",
                            "Sosa")

goals_16_30   <-  data.frame(Bertolacci$Bertolacci_goals_scored_16.30,
                             Bonaventura$Bonaventura_goals_scored_16.30,
                             Fernandez$Fernandez_goals_scored_16.30,
                             Honda$Honda_goals_scored_16.30,
                             Kucka$Kucka_goals_scored_16.30,
                             Locatelli$Locatelli_goals_scored_16.30,
                             Montolivo$Montolivo_goals_scored_16.30,
                             Pasalic$Pasalic_goals_scored_16.30,
                             Poli$Poli_goals_scored_16.30,
                             Sosa$Sosa_goals_scored_16.30)
colnames(goals_16_30) <-  c("Bertolacci",
                            "Bonaventura",
                            "Fernandez",
                            "Honda",
                            "Kucka",
                            "Locatelli",
                            "Montolivo",
                            "Pasalic",
                            "Poli",
                            "Sosa")

goals_31_45plus      <-   data.frame(Bertolacci$Bertolacci_goals_scored_31.45.,
                                     Bonaventura$Bonaventura_goals_scored_31.45.,
                                     Fernandez$Fernandez_goals_scored_31.45.,
                                     Honda$Honda_goals_scored_31.45.,
                                     Kucka$Kucka_goals_scored_31.45.,
                                     Locatelli$Locatelli_goals_scored_31.45.,
                                     Montolivo$Montolivo_goals_scored_31.45.,
                                     Pasalic$Pasalic_goals_scored_31.45.,
                                     Poli$Poli_goals_scored_31.45.,
                                     Sosa$Sosa_goals_scored_31.45.)
colnames(goals_31_45plus) <-  c("Bertolacci",
                                "Bonaventura",
                                "Fernandez",
                                "Honda",
                                "Kucka",
                                "Locatelli",
                                "Montolivo",
                                "Pasalic",
                                "Poli",
                                "Sosa")

goals_46_60   <-  data.frame(Bertolacci$Bertolacci_goals_scored_46.60,
                             Bonaventura$Bonaventura_goals_scored_46.60,
                             Fernandez$Fernandez_goals_scored_46.60,
                             Honda$Honda_goals_scored_46.60,
                             Kucka$Kucka_goals_scored_46.60,
                             Locatelli$Locatelli_goals_scored_46.60,
                             Montolivo$Montolivo_goals_scored_46.60,
                             Pasalic$Pasalic_goals_scored_46.60,
                             Poli$Poli_goals_scored_46.60,
                             Sosa$Sosa_goals_scored_46.60)              
colnames(goals_46_60)     <-  c("Bertolacci",
                                "Bonaventura",
                                "Fernandez",
                                "Honda",
                                "Kucka",
                                "Locatelli",
                                "Montolivo",
                                "Pasalic",
                                "Poli",
                                "Sosa")

goals_61_75   <-  data.frame(Bertolacci$Bertolacci_goals_scored_61.75,
                             Bonaventura$Bonaventura_goals_scored_61.75,
                             Fernandez$Fernandez_goals_scored_61.75,
                             Honda$Honda_goals_scored_61.75,
                             Kucka$Kucka_goals_scored_61.75,
                             Locatelli$Locatelli_goals_scored_61.75,
                             Montolivo$Montolivo_goals_scored_61.75,
                             Pasalic$Pasalic_goals_scored_61.75,
                             Poli$Poli_goals_scored_61.75,
                             Sosa$Sosa_goals_scored_61.75)                                
colnames(goals_61_75)     <-  c("Bertolacci",
                                "Bonaventura",
                                "Fernandez",
                                "Honda",
                                "Kucka",
                                "Locatelli",
                                "Montolivo",
                                "Pasalic",
                                "Poli",
                                "Sosa")

goals_76_90plus      <-   data.frame(Bertolacci$Bertolacci_goals_scored_76.90.,
                                     Bonaventura$Bonaventura_goals_scored_76.90.,
                                     Fernandez$Fernandez_goals_scored_76.90.,
                                     Honda$Honda_goals_scored_76.90.,
                                     Kucka$Kucka_goals_scored_76.90.,
                                     Locatelli$Locatelli_goals_scored_76.90.,
                                     Montolivo$Montolivo_goals_scored_76.90.,
                                     Pasalic$Pasalic_goals_scored_76.90.,
                                     Poli$Poli_goals_scored_76.90.,
                                     Sosa$Sosa_goals_scored_76.90.)
colnames(goals_76_90plus) <-  c("Bertolacci",
                                "Bonaventura",
                                "Fernandez",
                                "Honda",
                                "Kucka",
                                "Locatelli",
                                "Montolivo",
                                "Pasalic",
                                "Poli",
                                "Sosa")

assists_00_15   <-  data.frame(Bertolacci$Bertolacci_assists_00.15,
                               Bonaventura$Bonaventura_assists_00.15,
                               Fernandez$Fernandez_assists_00.15,
                               Honda$Honda_assists_00.15,
                               Kucka$Kucka_assists_00.15,
                               Locatelli$Locatelli_assists_00.15,
                               Montolivo$Montolivo_assists_00.15,
                               Pasalic$Pasalic_assists_00.15,
                               Poli$Poli_assists_00.15,
                               Sosa$Sosa_assists_00.15)
colnames(assists_00_15) <-  c("Bertolacci",
                              "Bonaventura",
                              "Fernandez",
                              "Honda",
                              "Kucka",
                              "Locatelli",
                              "Montolivo",
                              "Pasalic",
                              "Poli",
                              "Sosa")

assists_16_30   <-  data.frame(Bertolacci$Bertolacci_assists_16.30,
                               Bonaventura$Bonaventura_assists_16.30,
                               Fernandez$Fernandez_assists_16.30,
                               Honda$Honda_assists_16.30,
                               Kucka$Kucka_assists_16.30,
                               Locatelli$Locatelli_assists_16.30,
                               Montolivo$Montolivo_assists_16.30,
                               Pasalic$Pasalic_assists_16.30,
                               Poli$Poli_assists_16.30,
                               Sosa$Sosa_assists_16.30)
colnames(assists_16_30) <-  c("Bertolacci",
                              "Bonaventura",
                              "Fernandez",
                              "Honda",
                              "Kucka",
                              "Locatelli",
                              "Montolivo",
                              "Pasalic",
                              "Poli",
                              "Sosa")

assists_31_45plus      <-   data.frame(Bertolacci$Bertolacci_assists_31.45.,
                                       Bonaventura$Bonaventura_assists_31.45.,
                                       Fernandez$Fernandez_assists_31.45.,
                                       Honda$Honda_assists_31.45.,
                                       Kucka$Kucka_assists_31.45.,
                                       Locatelli$Locatelli_assists_31.45.,
                                       Montolivo$Montolivo_assists_31.45.,
                                       Pasalic$Pasalic_assists_31.45.,
                                       Poli$Poli_assists_31.45.,
                                       Sosa$Sosa_assists_31.45.)
colnames(assists_31_45plus) <-  c("Bertolacci",
                                  "Bonaventura",
                                  "Fernandez",
                                  "Honda",
                                  "Kucka",
                                  "Locatelli",
                                  "Montolivo",
                                  "Pasalic",
                                  "Poli",
                                  "Sosa")

assists_46_60   <-  data.frame(Bertolacci$Bertolacci_assists_46.60,
                               Bonaventura$Bonaventura_assists_46.60,
                               Fernandez$Fernandez_assists_46.60,
                               Honda$Honda_assists_46.60,
                               Kucka$Kucka_assists_46.60,
                               Locatelli$Locatelli_assists_46.60,
                               Montolivo$Montolivo_assists_46.60,
                               Pasalic$Pasalic_assists_46.60,
                               Poli$Poli_assists_46.60,
                               Sosa$Sosa_assists_46.60)              
colnames(assists_46_60)     <-  c("Bertolacci",
                                  "Bonaventura",
                                  "Fernandez",
                                  "Honda",
                                  "Kucka",
                                  "Locatelli",
                                  "Montolivo",
                                  "Pasalic",
                                  "Poli",
                                  "Sosa")

assists_61_75   <-  data.frame(Bertolacci$Bertolacci_assists_61.75,
                               Bonaventura$Bonaventura_assists_61.75,
                               Fernandez$Fernandez_assists_61.75,
                               Honda$Honda_assists_61.75,
                               Kucka$Kucka_assists_61.75,
                               Locatelli$Locatelli_assists_61.75,
                               Montolivo$Montolivo_assists_61.75,
                               Pasalic$Pasalic_assists_61.75,
                               Poli$Poli_assists_61.75,
                               Sosa$Sosa_assists_61.75)                                
colnames(assists_61_75)     <-  c("Bertolacci",
                                  "Bonaventura",
                                  "Fernandez",
                                  "Honda",
                                  "Kucka",
                                  "Locatelli",
                                  "Montolivo",
                                  "Pasalic",
                                  "Poli",
                                  "Sosa")

assists_76_90plus      <-   data.frame(Bertolacci$Bertolacci_assists_76.90.,
                                       Bonaventura$Bonaventura_assists_76.90.,
                                       Fernandez$Fernandez_assists_76.90.,
                                       Honda$Honda_assists_76.90.,
                                       Kucka$Kucka_assists_76.90.,
                                       Locatelli$Locatelli_assists_76.90.,
                                       Montolivo$Montolivo_assists_76.90.,
                                       Pasalic$Pasalic_assists_76.90.,
                                       Poli$Poli_assists_76.90.,
                                       Sosa$Sosa_assists_76.90.)
colnames(assists_76_90plus) <-  c("Bertolacci",
                                  "Bonaventura",
                                  "Fernandez",
                                  "Honda",
                                  "Kucka",
                                  "Locatelli",
                                  "Montolivo",
                                  "Pasalic",
                                  "Poli",
                                  "Sosa")

Yellow_cards <- data.frame(Bertolacci$Bertolacci_yellow_cards,
                           Bonaventura$Bonaventura_yellow_cards,
                           Fernandez$Fernandez_yellow_cards,
                           Honda$Honda_yellow_cards,
                           Kucka$Kucka_yellow_cards,
                           Locatelli$Locatelli_yellow_cards,
                           Montolivo$Montolivo_yellow_cards,
                           Pasalic$Pasalic_yellow_cards,
                           Poli$Poli_yellow_cards,
                           Sosa$Sosa_yellow_cards)                      
colnames(Yellow_cards) <-  c("Bertolacci",
                             "Bonaventura",
                             "Fernandez",
                             "Honda",
                             "Kucka",
                             "Locatelli",
                             "Montolivo",
                             "Pasalic",
                             "Poli",
                             "Sosa")

Red_cards <- data.frame(Bertolacci$Bertolacci_red_cards,
                        Bonaventura$Bonaventura_red_cards,
                        Fernandez$Fernandez_red_cards,
                        Honda$Honda_red_cards,
                        Kucka$Kucka_red_cards,
                        Locatelli$Locatelli_red_cards,
                        Montolivo$Montolivo_red_cards,
                        Pasalic$Pasalic_red_cards,
                        Poli$Poli_red_cards,
                        Sosa$Sosa_red_cards)                      
colnames(Red_cards) <-  c("Bertolacci",
                          "Bonaventura",
                          "Fernandez",
                          "Honda",
                          "Kucka",
                          "Locatelli",
                          "Montolivo",
                          "Pasalic",
                          "Poli",
                          "Sosa")

Penalties_converted <- data.frame(Bertolacci$Bertolacci_penalties_converted,
                                  Bonaventura$Bonaventura_penalties_converted,
                                  Fernandez$Fernandez_penalties_converted,
                                  Honda$Honda_penalties_converted,
                                  Kucka$Kucka_penalties_converted,
                                  Locatelli$Locatelli_penalties_converted,
                                  Montolivo$Montolivo_penalties_converted,
                                  Pasalic$Pasalic_penalties_converted,
                                  Poli$Poli_penalties_converted,
                                  Sosa$Sosa_penalties_converted)
colnames(Penalties_converted) <-  c("Bertolacci",
                                    "Bonaventura",
                                    "Fernandez",
                                    "Honda",
                                    "Kucka",
                                    "Locatelli",
                                    "Montolivo",
                                    "Pasalic",
                                    "Poli",
                                    "Sosa")

rm(Bertolacci,Bonaventura,Fernandez,Honda,Kucka,Locatelli,Montolivo,Pasalic,Poli,Sosa)

################# Data Visualisations
############ Goals
### Goals 00-15
# Make dataset
testdf1 <- data.frame(sum(goals_00_15$Bertolacci),sum(goals_00_15$Bonaventura),
                      sum(goals_00_15$Fernandez),sum(goals_00_15$Honda),sum(goals_00_15$Kucka),
                      sum(goals_00_15$Locatelli),sum(goals_00_15$Montolivo),sum(goals_00_15$Pasalic),
                      sum(goals_00_15$Poli),sum(goals_00_15$Sosa))
rownames(testdf1) <-  "Goals @00-15"
testdf2 <- data.frame(sum(ACM_Scoresheet$goals_scored_00_15))
colnames(testdf2) <-  "ACM scored @00-15"
row.names(testdf2)<-  "Goals"

# Bar Plot
barp(testdf1, main = "Goals @00-15", ylab = "Goals scored", names.arg = c("Bertolacci",
                                                                          "Bonaventura",
                                                                          "Fernandez",
                                                                          "Honda",
                                                                          "Kucka",
                                                                          "Locatelli",
                                                                          "Montolivo",
                                                                          "Pasalic",
                                                                          "Poli",
                                                                          "Sosa"), col = 2)

### Goals 16-30
# Make dataset
testdf1 <- data.frame(sum(goals_16_30$Bertolacci),sum(goals_16_30$Bonaventura),
                      sum(goals_16_30$Fernandez),sum(goals_16_30$Honda),sum(goals_16_30$Kucka),
                      sum(goals_16_30$Locatelli),sum(goals_16_30$Montolivo),sum(goals_16_30$Pasalic),
                      sum(goals_16_30$Poli),sum(goals_16_30$Sosa))
rownames(testdf1) <-  "Goals @16-30"
testdf2 <- data.frame(sum(ACM_Scoresheet$goals_scored_16_30))
colnames(testdf2) <-  "ACM scored @16-30"
row.names(testdf2)<-  "Goals"

# Bar Plot
barp(testdf1, main = "Goals @16-30", ylab = "Goals scored", names.arg = c("Bertolacci",
                                                                          "Bonaventura",
                                                                          "Fernandez",
                                                                          "Honda",
                                                                          "Kucka",
                                                                          "Locatelli",
                                                                          "Montolivo",
                                                                          "Pasalic",
                                                                          "Poli",
                                                                          "Sosa"), col = 2)

### Goals 31-45plus
# Make dataset
testdf1 <- data.frame(sum(goals_31_45plus$Bertolacci),sum(goals_31_45plus$Bonaventura),
                      sum(goals_31_45plus$Fernandez),sum(goals_31_45plus$Honda),sum(goals_31_45plus$Kucka),
                      sum(goals_31_45plus$Locatelli),sum(goals_31_45plus$Montolivo),sum(goals_31_45plus$Pasalic),
                      sum(goals_31_45plus$Poli),sum(goals_31_45plus$Sosa))
rownames(testdf1) <-  "Goals @31-45+"
testdf2 <- data.frame(sum(ACM_Scoresheet$goals_scored_31_45))
colnames(testdf2) <-  "ACM scored @31-45+"
row.names(testdf2)<-  "Goals"

# Bar Plot
barp(testdf1, main = "Goals @31-45+", ylab = "Goals scored", names.arg = c("Bertolacci",
                                                                           "Bonaventura",
                                                                           "Fernandez",
                                                                           "Honda",
                                                                           "Kucka",
                                                                           "Locatelli",
                                                                           "Montolivo",
                                                                           "Pasalic",
                                                                           "Poli",
                                                                           "Sosa"), col = 2)

### Goals 46-60
# Make dataset
testdf1 <- data.frame(sum(goals_46_60$Bertolacci),sum(goals_46_60$Bonaventura),
                      sum(goals_46_60$Fernandez),sum(goals_46_60$Honda),sum(goals_46_60$Kucka),
                      sum(goals_46_60$Locatelli),sum(goals_46_60$Montolivo),sum(goals_46_60$Pasalic),
                      sum(goals_46_60$Poli),sum(goals_46_60$Sosa))
rownames(testdf1) <-  "Goals @46-60"
testdf2 <- data.frame(sum(ACM_Scoresheet$goals_scored_16_30))
colnames(testdf2) <-  "ACM scored @46-60"
row.names(testdf2)<-  "Goals"

# Bar Plot
barp(testdf1, main = "Goals @46-60", ylab = "Goals scored", names.arg = c("Bertolacci",
                                                                          "Bonaventura",
                                                                          "Fernandez",
                                                                          "Honda",
                                                                          "Kucka",
                                                                          "Locatelli",
                                                                          "Montolivo",
                                                                          "Pasalic",
                                                                          "Poli",
                                                                          "Sosa"), col = 2)

### Goals 61-75
# Make dataset
testdf1 <- data.frame(sum(goals_61_75$Bertolacci),sum(goals_61_75$Bonaventura),
                      sum(goals_61_75$Fernandez),sum(goals_61_75$Honda),sum(goals_61_75$Kucka),
                      sum(goals_61_75$Locatelli),sum(goals_61_75$Montolivo),sum(goals_61_75$Pasalic),
                      sum(goals_61_75$Poli),sum(goals_61_75$Sosa))
rownames(testdf1) <-  "Goals @61-75"
testdf2 <- data.frame(sum(ACM_Scoresheet$goals_scored_61_75))
colnames(testdf2) <-  "ACM scored @61-75"
row.names(testdf2)<-  "Goals"

# Bar Plot
barp(testdf1, main = "Goals @61-75", ylab = "Goals scored", names.arg = c("Bertolacci",
                                                                          "Bonaventura",
                                                                          "Fernandez",
                                                                          "Honda",
                                                                          "Kucka",
                                                                          "Locatelli",
                                                                          "Montolivo",
                                                                          "Pasalic",
                                                                          "Poli",
                                                                          "Sosa"), col = 2)

### Goals 76-90plus
# Make dataset
testdf1 <- data.frame(sum(goals_76_90plus$Bertolacci),sum(goals_76_90plus$Bonaventura),
                      sum(goals_76_90plus$Fernandez),sum(goals_76_90plus$Honda),sum(goals_76_90plus$Kucka),
                      sum(goals_76_90plus$Locatelli),sum(goals_76_90plus$Montolivo),sum(goals_76_90plus$Pasalic),
                      sum(goals_76_90plus$Poli),sum(goals_76_90plus$Sosa))
rownames(testdf1) <-  "Goals @76-90+"
testdf2 <- data.frame(sum(ACM_Scoresheet$goals_scored_76_90plus))
colnames(testdf2) <-  "ACM scored @76-90+"
row.names(testdf2)<-  "Goals"

# Bar Plot
barp(testdf1, main = "Goals @76-90+", ylab = "Goals scored", names.arg = c("Bertolacci",
                                                                           "Bonaventura",
                                                                           "Fernandez",
                                                                           "Honda",
                                                                           "Kucka",
                                                                           "Locatelli",
                                                                           "Montolivo",
                                                                           "Pasalic",
                                                                           "Poli",
                                                                           "Sosa"), col = 2)

############ Assists
### Assists 00-15
# Make dataset
testdf1 <- data.frame(sum(assists_00_15$Bertolacci),sum(assists_00_15$Bonaventura),
                      sum(assists_00_15$Fernandez),sum(assists_00_15$Honda),sum(assists_00_15$Kucka),
                      sum(assists_00_15$Locatelli),sum(assists_00_15$Montolivo),sum(assists_00_15$Pasalic),
                      sum(assists_00_15$Poli),sum(assists_00_15$Sosa))
rownames(testdf1) <-  "Assists @00-15"

# Bar Plot
barp(testdf1, main = "Assists @00-15", ylab = "Assists", names.arg = c("Bertolacci",
                                                                       "Bonaventura",
                                                                       "Fernandez",
                                                                       "Honda",
                                                                       "Kucka",
                                                                       "Locatelli",
                                                                       "Montolivo",
                                                                       "Pasalic",
                                                                       "Poli",
                                                                       "Sosa"), col = 2)

### Assists 16-30
# Make dataset
testdf1 <- data.frame(sum(assists_16_30$Bertolacci),sum(assists_16_30$Bonaventura),
                      sum(assists_16_30$Fernandez),sum(assists_16_30$Honda),sum(assists_16_30$Kucka),
                      sum(assists_16_30$Locatelli),sum(assists_16_30$Montolivo),sum(assists_16_30$Pasalic),
                      sum(assists_16_30$Poli),sum(assists_16_30$Sosa))
rownames(testdf1) <-  "Assists @16-30"

# Bar Plot
barp(testdf1, main = "Assists @16-30", ylab = "Assists", names.arg = c("Bertolacci",
                                                                       "Bonaventura",
                                                                       "Fernandez",
                                                                       "Honda",
                                                                       "Kucka",
                                                                       "Locatelli",
                                                                       "Montolivo",
                                                                       "Pasalic",
                                                                       "Poli",
                                                                       "Sosa"), col = 2)

### Assists 31-45plus
# Make dataset
testdf1 <- data.frame(sum(assists_31_45plus$Bertolacci),sum(assists_31_45plus$Bonaventura),
                      sum(assists_31_45plus$Fernandez),sum(assists_31_45plus$Honda),sum(assists_31_45plus$Kucka),
                      sum(assists_31_45plus$Locatelli),sum(assists_31_45plus$Montolivo),sum(assists_31_45plus$Pasalic),
                      sum(assists_31_45plus$Poli),sum(assists_31_45plus$Sosa))
rownames(testdf1) <-  "Goals @31-45+"

# Bar Plot
barp(testdf1, main = "Assists @31-45+", ylab = "Assists", names.arg = c("Bertolacci",
                                                                        "Bonaventura",
                                                                        "Fernandez",
                                                                        "Honda",
                                                                        "Kucka",
                                                                        "Locatelli",
                                                                        "Montolivo",
                                                                        "Pasalic",
                                                                        "Poli",
                                                                        "Sosa"), col = 2)

### Assists 46-60
# Make dataset
testdf1 <- data.frame(sum(assists_46_60$Bertolacci),sum(assists_46_60$Bonaventura),
                      sum(assists_46_60$Fernandez),sum(assists_46_60$Honda),sum(assists_46_60$Kucka),
                      sum(assists_46_60$Locatelli),sum(assists_46_60$Montolivo),sum(assists_46_60$Pasalic),
                      sum(assists_46_60$Poli),sum(assists_46_60$Sosa))
rownames(testdf1) <-  "Assists @46-60"

# Bar Plot
barp(testdf1, main = "Assists @46-60", ylab = "Assists", names.arg = c("Bertolacci",
                                                                       "Bonaventura",
                                                                       "Fernandez",
                                                                       "Honda",
                                                                       "Kucka",
                                                                       "Locatelli",
                                                                       "Montolivo",
                                                                       "Pasalic",
                                                                       "Poli",
                                                                       "Sosa"), col = 2)

### Assists 61-75
# Make dataset
testdf1 <- data.frame(sum(assists_61_75$Bertolacci),sum(assists_61_75$Bonaventura),
                      sum(assists_61_75$Fernandez),sum(assists_61_75$Honda),sum(assists_61_75$Kucka),
                      sum(assists_61_75$Locatelli),sum(assists_61_75$Montolivo),sum(assists_61_75$Pasalic),
                      sum(assists_61_75$Poli),sum(assists_61_75$Sosa))
rownames(testdf1) <-  "Assists @61-75"

# Bar Plot
barp(testdf1, main = "Assists @61-75", ylab = "Assists", names.arg = c("Bertolacci",
                                                                       "Bonaventura",
                                                                       "Fernandez",
                                                                       "Honda",
                                                                       "Kucka",
                                                                       "Locatelli",
                                                                       "Montolivo",
                                                                       "Pasalic",
                                                                       "Poli",
                                                                       "Sosa"), col = 2)

### Assists 76-90plus
# Make dataset
testdf1 <- data.frame(sum(assists_76_90plus$Bertolacci),sum(assists_76_90plus$Bonaventura),
                      sum(assists_76_90plus$Fernandez),sum(assists_76_90plus$Honda),sum(assists_76_90plus$Kucka),
                      sum(assists_76_90plus$Locatelli),sum(assists_76_90plus$Montolivo),sum(assists_76_90plus$Pasalic),
                      sum(assists_76_90plus$Poli),sum(assists_76_90plus$Sosa))
rownames(testdf1) <-  "Assists @76-90+"

# Bar Plot
barp(testdf1, main = "Assists @76-90+", ylab = "Assists", names.arg = c("Bertolacci",
                                                                        "Bonaventura",
                                                                        "Fernandez",
                                                                        "Honda",
                                                                        "Kucka",
                                                                        "Locatelli",
                                                                        "Montolivo",
                                                                        "Pasalic",
                                                                        "Poli",
                                                                        "Sosa"), col = 2)

### Minutes Played
total_min_plyd <- data.frame(sum(Minutes_Played$Bertolacci_minutes_played),
                             sum(Minutes_Played$Bonaventura_minutes_played),
                             sum(Minutes_Played$Fernandez_minutes_played),
                             sum(Minutes_Played$Honda_minutes_played),
                             sum(Minutes_Played$Kucka_minutes_played),
                             sum(Minutes_Played$Locatelli_minutes_played),
                             sum(Minutes_Played$Montolivo_minutes_played),
                             sum(Minutes_Played$Pasalic_minutes_played),
                             sum(Minutes_Played$Poli_minutes_played),
                             sum(Minutes_Played$Sosa_minutes_played))
colnames(total_min_plyd) <- c("Bertolacci",
                              "Bonaventura",
                              "Fernandez",
                              "Honda",
                              "Kucka",
                              "Locatelli",
                              "Montolivo",
                              "Pasalic",
                              "Poli",
                              "Sosa")
testdf1_gauge <- gvisGauge(total_min_plyd, options = list(min = 200, max = 2300,
                                                          greenFrom = 1600, greenTo = 2300, yellowFrom = 900,
                                                          yellowTo = 1600, redFrom = 200, redTo = 900, width=800, height=600))

plot(testdf1_gauge)

### Did they scored First ?
# View state dataset
testdf1 = data.frame(ACM_Performancesheet$scored_first_goal)

# View region counts
section <- table(testdf1)
section

# Labels of region
lbls <- paste(names(section),"\n",section)

# 3D piechart
pie3D(section, radius = 2.00, labels = lbls, explode = 0.05,
      main = "ACM Scored First ?", col = c("red","green"))

### Did they scored Last ?
# View state dataset
testdf1 = data.frame(ACM_Performancesheet$scored_last_goal)

# View region counts
section <- table(testdf1)
section

# Labels of region
lbls <- paste(names(section),"\n",section)

# 3D piechart
pie3D(section, radius = 2.00, labels = lbls, explode = 0.05,
      main = "ACM Scored Last ?", col = c("red","green"))

#### Crosses - Left/Right
library(ggplot2)
library(rCharts)
testdf1 <- data.frame(ACM_Performancesheet$Round,
                      ACM_Performancesheet$left_crosses, ACM_Performancesheet$right_crosses)
colnames(testdf1) <- c("Round",
                       "Left_Crosses","Right_Crosses")
Combo <- gvisComboChart(testdf1, xvar = "Round", yvar = c("Left_Crosses","Right_Crosses"),
                        options = list(seriesType = "bars",
                                       series = '{1:{type:"line"}}'))
plot(Combo)

#### Crosses - Left/Right
testdf1 <- data.frame(ACM_Performancesheet$Round,
                      ACM_Performancesheet$Inside_18_yard_line_crosses,ACM_Performancesheet$Outside_18_yard_line_crosses)
colnames(testdf1) <- c("Round",
                       "Inside_18_yard_line_crosses","Outside_18_yard_line_crosses")
Combo <- gvisComboChart(testdf1, xvar = "Round", yvar = c("Inside_18_yard_line_crosses","Outside_18_yard_line_crosses"),
                        options = list(seriesType = "bars",
                                       series = '{1:{type:"line"}}'))
plot(Combo)

## Goals scored by team
goals_scored <- data.frame(sum(ACM_Scoresheet$goals_scored_00_15),
                           sum(ACM_Scoresheet$goals_scored_16_30),
                           sum(ACM_Scoresheet$goals_scored_31_45),
                           sum(ACM_Scoresheet$goals_scored_46_60),
                           sum(ACM_Scoresheet$goals_scored_61_75),
                           sum(ACM_Scoresheet$goals_scored_76_90plus))
colnames(goals_scored) <- c("00-15","16-30","31-45plus",
                            "46-60","61-75","76-90plus")
# Bar Plot
barp(goals_scored, main = "Goals Scored", ylab = "Goals", names.arg = c("00-15","16-30","31-45plus",
                                                                        "46-60","61-75","76-90plus"), col = 2)

### Defence
## Goals Conceded
goals_conceded <- data.frame(sum(ACM_Scoresheet$goals_conceded_00_15),
                             sum(ACM_Scoresheet$goals_conceded_16_30),
                             sum(ACM_Scoresheet$goals_conceded_31_45),
                             sum(ACM_Scoresheet$goals_conceded_46_60),
                             sum(ACM_Scoresheet$goals_conceded_61_75),
                             sum(ACM_Scoresheet$goals_conceded_76_90plus))
colnames(goals_conceded) <- c("00-15","16-30","31-45plus",
                              "46-60","61-75","76-90plus")
# Bar Plot
barp(goals_conceded, main = "Goals Conceded", ylab = "Goals", names.arg = c("00-15","16-30","31-45plus",
                                                                            "46-60","61-75","76-90plus"), col = 1)

## Defence recoveries, clearances and interceptions
testdf1 <- data.frame(ACM_Performancesheet$Round, ACM_Performancesheet$defence_clearances,
                      ACM_Performancesheet$defence_recoveries, ACM_Performancesheet$interceptions)
colnames(testdf1) <- c("Round","Defence_Clearances",
                       "Defence_Recoveries","Interceptions")
testdf2 <- data.frame("Round","Defence","Value")
colnames(testdf2) <- c("Round","Defence","Value")
View(testdf2)
testdf2$Round <- as.integer(as.character(testdf2$Round))
testdf2[1:38,1] <- testdf1$Round
testdf2$Defence <- as.character(testdf2$Defence)
testdf2[1:38,2] <- "Defence Clearances"
testdf2$Value <- as.numeric(as.character(testdf2$Value))
testdf2[1:38,3] <- testdf1$Defence_Clearances
testdf2[39:76,1] <- testdf1$Round
testdf2[39:76,2] <- "Defence Recoveries"
testdf2[39:76,3] <- testdf1$Defence_Recoveries
testdf2[77:114,1] <- testdf1$Round
testdf2[77:114,2] <- "Interceptions"
testdf2[77:114,3] <- testdf1$Interceptions

qplot(Round, Value, data=testdf2, geom="area", margins = "Defence", fill = Defence)

##################################################################################################
################# Facts & Stats

won_matches1 <- subset(ACM_Scoresheet,ACM_Scoresheet$match_result == "W", header = TRUE)
won_matches2 <- merge(x = won_matches1, y = ACM_Performancesheet, by.x = "Round", 
                      by.y = "Round", all.x = TRUE)
Opening_Goal["Round"] <- ACM_Scoresheet$Round
won_matches <- merge(x = won_matches2, y = Opening_Goal, by.x = "Round", 
                     by.y = "Round", all.x = TRUE)
won_matches <- won_matches[,-19]

sum(won_matches$goals_scored_00_15)
## 4 goals
sum(won_matches$goals_scored_16_30)
## 4 goals
sum(won_matches$goals_scored_31_45)
## 7 goals
sum(won_matches$goals_scored_46_60)
## 3 goals
sum(won_matches$goals_scored_61_75)
## 11 goals
sum(won_matches$goals_scored_76_90plus)
## 10 goals

mean(won_matches$goals_scored_61_75)
## 0.6111111
mean(won_matches$goals_scored_76_90plus)
## 0.5555556

length(which(won_matches$scored_first_goal=='Y'))
## 17 matches
length(which(won_matches$scored_last_goal=='Y'))
## 17 matches
length(which(won_matches$scored_first_goal=='Y' & won_matches$scored_last_goal=='Y'))
## 16 matches
length(which(won_matches$equalizer_scored != 0))
## 2 matches
length(which(won_matches$equalizer_conceded != 0))
## 6 matches

mean(won_matches$ball_possesion)
## 0.5233333
mean(won_matches$pass_accuracy)
## 0.82

sum(won_matches$goals_conceded_00_15)
## 1 goals
sum(won_matches$goals_conceded_16_30)
## 3 goals
sum(won_matches$goals_conceded_31_45)
## 1 goals
sum(won_matches$goals_conceded_46_60)
## 3 goals
sum(won_matches$goals_conceded_61_75)
## 1 goals
sum(won_matches$goals_conceded_76_90plus)
## 2 goals

sum(won_matches$yellow_cards)
## 34 YCs
sum(won_matches$red_cards)
## 3 RCs

sum(won_matches$Bertolacci)
## 0 goals
sum(won_matches$Bonaventura)
## 2 goals
sum(won_matches$Fernandez)
## 1 goal
sum(won_matches$Honda)
## 0 goals
sum(won_matches$Kucka)
## 1 Kucka
sum(won_matches$Locatelli)
## 1 Locatelli 
sum(won_matches$Montolivo)
## 0 goals
sum(won_matches$Pasalic)
## 2 goals
sum(won_matches$Poli)
## 0 goals
sum(won_matches$Sosa)
## 0 goals

Minutes_Played["Round"] <- ACM_Scoresheet$Round
won_matches <- merge(x = won_matches, y = Minutes_Played, by.x = "Round", 
                     by.y = "Round", all.x = TRUE)
sum(won_matches$Bertolacci_minutes_played)
## 299 minutes
sum(won_matches$Bonaventura_minutes_played)
## 788 minutes
sum(won_matches$Fernandez_minutes_played)
## 165 minutes
sum(won_matches$Honda_minutes_played)
## 60 minutes
sum(won_matches$Kucka_minutes_played)
## 1071 minutes
sum(won_matches$Locatelli_minutes_played)
## 990 minutes 
sum(won_matches$Montolivo_minutes_played)
## 443 minutes
sum(won_matches$Pasalic_minutes_played)
## 780 minutes
sum(won_matches$Poli_minutes_played)
## 120 minutes
sum(won_matches$Sosa_minutes_played)
## 545 minutes

lost_matches1 <- subset(ACM_Scoresheet,ACM_Scoresheet$match_result == "L", header = TRUE)
lost_matches2 <- merge(x = lost_matches1, y = ACM_Performancesheet, by.x = "Round", 
                       by.y = "Round", all.x = TRUE)
lost_matches <- merge(x = lost_matches2, y = Opening_Goal, by.x = "Round", 
                      by.y = "Round", all.x = TRUE)
lost_matches <- lost_matches[,-19]

sum(lost_matches$goals_scored_00_15)
## 1 goal
sum(lost_matches$goals_scored_16_30)
## 0 goal
sum(lost_matches$goals_scored_31_45)
## 2 goals
sum(lost_matches$goals_scored_46_60)
## 2 goals
sum(lost_matches$goals_scored_61_75)
## 2 goals
sum(lost_matches$goals_scored_76_90plus)
## 1 goals

mean(lost_matches$goals_scored_61_75)
## 0.1818182
mean(lost_matches$goals_scored_76_90plus)
## 0.5555556

length(which(lost_matches$scored_first_goal=='Y'))
## 1 match
length(which(lost_matches$scored_last_goal=='Y'))
## 2 matches
length(which(lost_matches$scored_first_goal=='Y' & lost_matches$scored_last_goal=='Y'))
## 16 matches
length(which(lost_matches$equalizer_scored != 0))
## 0 matches
length(which(lost_matches$equalizer_conceded != 0))
## 2 matches

mean(lost_matches$ball_possesion)
## 0.5627273
mean(lost_matches$pass_accuracy)
## 0.8327273

sum(lost_matches$goals_conceded_00_15)
## 4 goals
sum(lost_matches$goals_conceded_16_30)
## 4  goals
sum(lost_matches$goals_conceded_31_45)
## 3 goals
sum(lost_matches$goals_conceded_46_60)
## 0 goal
sum(lost_matches$goals_conceded_61_75)
## 5 goals
sum(lost_matches$goals_conceded_76_90plus)
## 7 goals

sum(lost_matches$yellow_cards)
## 25 YCs
sum(lost_matches$red_cards)
## 7 RCs

sum(lost_matches$Bertolacci)
## 0 goals
sum(lost_matches$Bonaventura)
## 1 goals
sum(lost_matches$Fernandez)
## 0 goal
sum(lost_matches$Honda)
## 0 goals
sum(lost_matches$Kucka)
## 0 goals
sum(lost_matches$Locatelli)
## 0 goals
sum(lost_matches$Montolivo)
## 0 goals
sum(lost_matches$Pasalic)
## 0 goals
sum(lost_matches$Poli)
## 0 goals
sum(lost_matches$Sosa)
## 0 goals

sum(Winning_Goal$Bertolacci)
## 0 goal
sum(Winning_Goal$Bonaventura)
## 1 goal
sum(Winning_Goal$Fernandez)
## 1 goal
sum(Winning_Goal$Honda)
## 0 goal
sum(Winning_Goal$Kucka)
## 0 goal
sum(Winning_Goal$Locatelli)
## 1 goal
sum(Winning_Goal$Montolivo)
## 0 goal
sum(Winning_Goal$Pasalic)
## 1 goal
sum(Winning_Goal$Poli)
## 0 goal
sum(Winning_Goal$Sosa)
## 0 goal

max(ACM_Performancesheet$interceptions)
which(ACM_Performancesheet$interceptions == 21)
ACM_Performancesheet[19,]
max(ACM_Performancesheet$defence_recoveries)
which(ACM_Performancesheet$defence_recoveries == 72)
ACM_Performancesheet[17,]
max(ACM_Performancesheet$defence_clearances)
which(ACM_Performancesheet$defence_clearances == 36)
ACM_Performancesheet[6,]
ACM_Performancesheet[26,]
