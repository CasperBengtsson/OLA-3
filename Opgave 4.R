library(dkstat)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# ==========================================
# 1. HENT OG KLARGØR FORBRUGERFORVENTNINGER (FORV1)
# ==========================================
forv1_meta <- dst_meta(table = "FORV1")
spoergsmaal <- forv1_meta$values$INDIKATOR$text

# Vælger alle spørgsmål undtagen (Forbrugertillidsindikatoren og 'Anser det som fornuftigt')
spoergsmaal <- spoergsmaal[!grepl("Forbrugertillidsindikatoren|Anser det som fornuftigt", spoergsmaal)]

forv1 <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = spoergsmaal, Tid = "*"),
  meta_data = forv1_meta,
  lang = "da",
  parse_dst_tid = FALSE
)

# Beholder januar 2000 og frem
forv1 <- forv1[forv1$TID >= "2000M01", ]

# Laver spørgsmålene til kolonner
forv1 <- pivot_wider(forv1, id_cols = TID, names_from = INDIKATOR, values_from = value)
forv1 <- as.data.frame(forv1)
forv1 <- forv1[order(forv1$TID), ]
rownames(forv1) <- NULL

# Laver måneder til kvartaler
maaned <- as.numeric(substr(forv1$TID, 6, 7))
kvartal <- paste0(substr(forv1$TID, 1, 4), "K", ceiling(maaned / 3))

# Beregner gennemsnit pr. kvartal
forv1_kvt <- aggregate(forv1[, -1], by = list(TID = kvartal), FUN = mean)

# ==========================================
# VENDER FORTEGN PÅ F6, F7 OG F8
# ==========================================
# Vender fortegnet så et fald i priser/arbejdsløshed. Dette trækker indikatoren op i stedet for ned.
forv1_kvt <- forv1_kvt %>%
  mutate(
    `F6 Priser i dag, sammenlignet med for et år siden` = `F6 Priser i dag, sammenlignet med for et år siden` * -1,
    `F7 Priser om et år, sammenlignet med i dag` = `F7 Priser om et år, sammenlignet med i dag` * -1,
    `F8 Arbejdsløsheden om et år, sammenlignet med i dag` = `F8 Arbejdsløsheden om et år, sammenlignet med i dag` * -1
  )

# ==========================================
# 2. KØR LOOP FOR ALLE 2.047 KOMBINATIONER PÅ RENE DATA
# ==========================================
# Vi bruger det oprindelige '- 1', da DI-indikatoren ikke er lagt ind endnu
antal_spm <- ncol(forv1_kvt) - 1 
alle_komb <- list()

for (k in 1:antal_spm) {
  n_comb <- choose(antal_spm, k)
  z <- matrix(0, ncol = n_comb, nrow = nrow(forv1_kvt))
  
  for(i in 1:nrow(forv1_kvt)) {
    # Regner på de 11 spørgsmål (kolonne 2 og frem)
    z[i,] <- colMeans(matrix(as.numeric(combn(forv1_kvt[i, 2:ncol(forv1_kvt)], k)), nrow = k))
  }
  alle_komb[[k]] <- z
}

z_komplet <- do.call(cbind, alle_komb)

# Gør matrixen til et dataframe og tilføjer TID
z_df <- as.data.frame(z_komplet)
z_df$TID_kvartal <- forv1_kvt$TID 

# ==========================================
# 3. BEREGN DI'S INDIKATOR
# ==========================================
# Tilføjes direkte til z_df, så det oprindelige datasæt ikke forstyrres
z_df <- z_df %>%
  mutate(DI_ind = (
    forv1_kvt$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden` + 
      forv1_kvt$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden` + 
      forv1_kvt$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` + 
      forv1_kvt$`F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`
  ) / 4)

# ==========================================
# 4. HENT OG BEREGN PRIVATFORBRUG (REALVÆKST)
# ==========================================
meta_NKN <- dst_meta("NKN1")
meta_NKN$variables
meta_forv1_DI$values$SÆSON

query_privatforbrug <- list(
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter", 
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*" 
)

df_forbrug <- dst_get_data(table = "NKN1", query = query_privatforbrug)

df_forbrug_vaekst <- df_forbrug %>%
  filter(TID >= as.Date("1999-01-01")) %>%
  arrange(TID) %>%
  mutate(Vaekst_YOY_procent = ((value - lag(value, n = 4)) / lag(value, n = 4)) * 100) %>%
  filter(TID >= as.Date("2000-01-01")) %>%
  mutate(
    Aar = year(TID),
    Kvartal = quarter(TID),
    TID_kvartal = paste0(Aar, "K", Kvartal)
  ) %>%
  select(TID_kvartal, Realforbrug = value, Vaekst_YOY_procent)

# ==========================================
# 5. FLET DATA OG FIND DEN BEDSTE INDIKATOR
# ==========================================
analyse_data <- inner_join(z_df, df_forbrug_vaekst, by = "TID_kvartal")

# Test alle indikator-kolonner (V1 til V2047) mod realvæksten
antal_kombinationer <- ncol(z_komplet)
r2_resultater <- numeric(antal_kombinationer)

for(j in 1:antal_kombinationer) {
  model <- lm(analyse_data$Vaekst_YOY_procent ~ analyse_data[[j]])
  r2_resultater[j] <- summary(model)$r.squared
}

# Udtræk R^2 for den bedste maskin-indikator og for DI's indikator
max_r2 <- max(r2_resultater)
vinder_index <- which.max(r2_resultater)

model_DI <- lm(Vaekst_YOY_procent ~ DI_ind, data = analyse_data)
di_r2 <- summary(model_DI)$r.squared

# ==========================================
# OPGAVE 4
# ==========================================

# ==========================================
# 4.1 STABILITETSANALYSE GÅR 7 ÅR TILBAGE
# ==========================================
# Sørg for at data er sorteret kronologisk
analyse_data <- analyse_data[order(analyse_data$TID_kvartal), ]

# Hvor mange kvartaler vil du teste bagud? (7 år tilbage fra 2026)
antal_skridt_tilbage <- 28 
antal_kombinationer <- 2047

# Opret en tom matrix til at gemme R^2 resultaterne. Rækker = V1 til V2047, Kolonner = de forskellige tidsperioder
r2_historik <- matrix(NA, nrow = antal_kombinationer, ncol = antal_skridt_tilbage + 1)
kvartal_navne <- c()

# Start the ydre loop: Går fra 0 (fjerner intet). Fjerner de 28 kvartaler
for (step in 0:antal_skridt_tilbage) {
  
  # Find rækkenummeret vi skal stoppe ved i dette loop
  slut_index <- nrow(analyse_data) - step
  
  # Lav et midlertidigt datasæt, hvor de nyeste kvartaler er klippet af
  temp_data <- analyse_data[1:slut_index, ]
  
  # Gem navnet på det kvartal, datasættet nu slutter med (f.eks. "2026K2")
  seneste_kvartal <- temp_data$TID_kvartal[slut_index]
  kvartal_navne <- c(kvartal_navne, seneste_kvartal)
  
  # Kør det indre loop: Beregn R^2 for alle 2.047 kombinationer på dette afkortede datasæt
  for (j in 1:antal_kombinationer) {
    # Vi bruger 'V1', 'V2' osv. som kolonnenavne for at være sikre på at ramme rigtigt
    indikator_navn <- paste0("V", j) 
    model <- lm(temp_data$Vaekst_YOY_procent ~ temp_data[[indikator_navn]])
    
    # Gem resultatet i vores matrix (række j, kolonne step + 1)
    r2_historik[j, step + 1] <- summary(model)$r.squared
  }
}

# Gør matrixen til en pæn dataframe og giv den kolonnenavne
r2_historik_tilbage <- as.data.frame(r2_historik)
colnames(r2_historik_tilbage) <- kvartal_navne
r2_historik_tilbage$ID <- paste0("V", 1:antal_kombinationer)
r2_historik_tilbage$Spoergsmaal <- komb_navne # Tilføj hvilke spørgsmål V-nummeret dækker over

# ==========================================
# 4.1 BEREGN GENNEMSNIT OG STANDARDAFVIGELSE
# ==========================================
# Beregn gennemsnittet af R^2 på tværs af alle testede perioder
r2_historik_tilbage$Gennemsnit_R2 <- rowMeans(r2_historik_tilbage[, 1:(antal_skridt_tilbage + 1)])

# Beregn standardafvigelsen (hvor meget R^2 svinger over tid)
r2_historik_tilbage$SD_R2 <- apply(r2_historik_tilbage[, 1:(antal_skridt_tilbage + 1)], 1, sd)

# Sortér listen, så de mest stabile og bedst forklarende indikatorer ligger øverst.
# Her sorteres primært efter det højeste gennemsnit, og derefter den laveste standardafvigelse.
stabilitet_top10 <- r2_historik_tilbage %>%
  select(ID, Spoergsmaal, Gennemsnit_R2, SD_R2, everything()) %>%
  arrange(desc(Gennemsnit_R2), SD_R2) %>%
  head(10)

cat("\n--- TOP 10 MEST STABILE INDIKATORER OVER TID ---\n")
print(stabilitet_top10[, 1:4], row.names = FALSE)

# ==========================================
# 4.2 STABILITETSANALYSE GÅR 7 ÅR FREM
# ==========================================
# Sørg for at data er sorteret kronologisk
analyse_data <- analyse_data[order(analyse_data$TID_kvartal), ]

# Hvor mange kvartaler vil du fjerne fra starten. Fjerner igen de første 7 år = 28 kvartaler
antal_skridt_frem <- 28 
antal_kombinationer <- 2047

# Opret en tom matrix til at gemme R^2 resultaterne
r2_historik <- matrix(NA, nrow = antal_kombinationer, ncol = antal_skridt_frem + 1)
kvartal_navne <- c()

# Start det ydre loop: Går fra 0 (fjerner intet). Fjerner de 28 kvartaler
for (step in 0:antal_skridt_frem) {
  
  # Definer start-indekset (rykker én række ned for hvert step)
  start_index <- 1 + step
  
  # Lav et midlertidigt datasæt, hvor de ældste kvartaler er klippet af
  temp_data <- analyse_data[start_index:nrow(analyse_data), ]
  
  # Gem navnet på det ældste kvartal, datasættet nu starter med (f.eks. "2000K2")
  tidligste_kvartal <- temp_data$TID_kvartal[1]
  kvartal_navne <- c(kvartal_navne, tidligste_kvartal)
  
  # Kør det indre loop: Beregn R^2 for alle kombinationer på det afkortede datasæt
  for (j in 1:antal_kombinationer) {
    indikator_navn <- paste0("V", j) 
    model <- lm(temp_data$Vaekst_YOY_procent ~ temp_data[[indikator_navn]])
    
    # Gem resultatet i vores matrix
    r2_historik[j, step + 1] <- summary(model)$r.squared
  }
}

# Gør matrixen til en pæn dataframe og giv den kolonnenavne (som nu er start-kvartalet)
r2_historik_frem <- as.data.frame(r2_historik)
colnames(r2_historik_frem) <- kvartal_navne
r2_historik_frem$ID <- paste0("V", 1:antal_kombinationer)
r2_historik_frem$Spoergsmaal <- komb_navne 

# ==========================================
# 4.2 BEREGN GENNEMSNIT OG STANDARDAFVIGELSE
# ==========================================
# Beregn gennemsnittet af R^2 på tværs af de forskellige starttidspunkter
r2_historik_frem$Gennemsnit_R2 <- rowMeans(r2_historik_frem[, 1:(antal_skridt_frem + 1)])

# Beregn standardafvigelsen (hvor meget R^2 svinger, når ældre data fjernes)
r2_historik_frem$SD_R2 <- apply(r2_historik_frem[, 1:(antal_skridt_frem + 1)], 1, sd)

# Sortér listen, så de mest stabile og bedst forklarende indikatorer ligger øverst.
stabilitet_top10 <- r2_historik_frem %>%
  select(ID, Spoergsmaal, Gennemsnit_R2, SD_R2, everything()) %>%
  arrange(desc(Gennemsnit_R2), SD_R2) %>%
  head(10)

cat("\n--- TOP 10 MEST STABILE INDIKATORER OVER TID (VED FJERNELSE AF ÆLDRE DATA) ---\n")
print(stabilitet_top10[, 1:4], row.names = FALSE)

# ===============================================
# 4.3 STABILITETSANALYSE GÅR Å ÅR FREM & TILBAGE
# ===============================================
# Sørg for at data er sorteret kronologisk
analyse_data <- analyse_data[order(analyse_data$TID_kvartal), ]

# Fjerner de første 28 OG de sidste 28 kvartaler
antal_skridt_indad <- 28 
antal_kombinationer <- 2047

# Opret en tom matrix til at gemme R^2 resultaterne
r2_historik <- matrix(NA, nrow = antal_kombinationer, ncol = antal_skridt_indad + 1)
periode_navne <- c()

# Start loopet: Går fra 0 (fjerner intet). Fjerne de 20 kvartaler for fra hver ende
for (step in 0:antal_skridt_indad) {
  
  # Definer både start- og slut-indeks
  start_index <- 1 + step
  slut_index <- nrow(analyse_data) - step
  
  # Lav et midlertidigt datasæt, der er klippet i begge ender
  temp_data <- analyse_data[start_index:slut_index, ]
  
  # Gem navnet på den aktuelle periode (f.eks. "2000K2 - 2026K2")
  aktuel_periode <- paste0(temp_data$TID_kvartal[1], " til ", temp_data$TID_kvartal[nrow(temp_data)])
  periode_navne <- c(periode_navne, aktuel_periode)
  
  # Kør det indre loop: Beregn R^2 for alle kombinationer
  for (j in 1:antal_kombinationer) {
    indikator_navn <- paste0("V", j) 
    model <- lm(temp_data$Vaekst_YOY_procent ~ temp_data[[indikator_navn]])
    
    # Gem resultatet
    r2_historik[j, step + 1] <- summary(model)$r.squared
  }
}

# Gør matrixen til en pæn dataframe
r2_historik_frem_tilbage <- as.data.frame(r2_historik)
colnames(r2_historik_frem_tilbage) <- periode_navne # Viser nu tidsspændet som kolonneoverskrift
r2_historik_frem_tilbage$ID <- paste0("V", 1:antal_kombinationer)
r2_historik_frem_tilbage$Spoergsmaal <- komb_navne 

# ==========================================
# 4.3 BEREGN GENNEMSNIT OG STANDARDAFVIGELSE
# ==========================================
# Beregn gennemsnit og standardafvigelse på tværs af de kortere perioder
r2_historik_frem_tilbage$Gennemsnit_R2 <- rowMeans(r2_historik_frem_tilbage[, 1:(antal_skridt_indad + 1)])
r2_historik_frem_tilbage$SD_R2 <- apply(r2_historik_frem_tilbage[, 1:(antal_skridt_indad + 1)], 1, sd)

# Sortér listen efter højeste stabilitet og forklaringsgrad
stabilitet_top10 <- r2_historik_frem_tilbage %>%
  select(ID, Spoergsmaal, Gennemsnit_R2, SD_R2, everything()) %>%
  arrange(desc(Gennemsnit_R2), SD_R2) %>%
  head(10)

cat("\n--- TOP 10 MEST STABILE INDIKATORER OVER TID (BESKÅRET I BEGGE ENDER) ---\n")
print(stabilitet_top10[, 1:4], row.names = FALSE)


# ==========================================
# 4.4 SAMLET STABILITETSANALYSE (ALLE 3 METODER)
# ==========================================
samlet_stabilitet <- data.frame(
  ID = r2_historik_tilbage$ID,
  Spoergsmaal = r2_historik_tilbage$Spoergsmaal,
  Gns_tilbage = r2_historik_tilbage$Gennemsnit_R2,
  Gns_frem = r2_historik_frem$Gennemsnit_R2,
  Gns_frem_tilbage = r2_historik_frem_tilbage$Gennemsnit_R2
)

# Beregn det overordnede gennemsnit
samlet_stabilitet$Samlet_Gns_R2 <- rowMeans(samlet_stabilitet[, c("Gns_tilbage", "Gns_frem", "Gns_frem_tilbage")])

# Sortér listen
top_10_ultimativ <- samlet_stabilitet %>%
  arrange(desc(Samlet_Gns_R2)) %>%
  head(10)

print(top_10_ultimativ, row.names = FALSE)
