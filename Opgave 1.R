# ==========================================
# 0. INDLÆS PAKKER
# ==========================================
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
meta_NKN$values$SÆSON

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
# 6. UDSKRIV RESULTATERNE TIL OPGAVE 1.2
# ==========================================
cat("--- RESULTATER TIL OPGAVE 1.2 ---\n")
cat("Højeste R^2 er:", round(max_r2, 4), "(findes i kolonne V", vinder_index, ")\n")
# 1. Træk navnene på de 11 spørgsmål ud fra dit datasæt
spm_navne <- colnames(forv1_kvt)[2:ncol(forv1_kvt)]

# 2. Genskab listen over alle kombinationer (med tekst i stedet for tal)
komb_navne <- c()
for (k in 1:length(spm_navne)) {
  aktuelle_komb <- combn(spm_navne, k, simplify = FALSE)
  komb_navne <- c(komb_navne, sapply(aktuelle_komb, paste, collapse = " + "))
}

# 3. Slå direkte op på den vindende plads for at se spørgsmålene
komb_navne[952]
cat("Dansk Industris R^2 er:", round(di_r2, 4), "\n\n")

sammenligningstab <- data.frame(
  Indikator = c("Optimal Indikator (Algoritme)", "DI's Indikator"),
  R_squared = c(round(max_r2, 4), round(di_r2, 4))
)

print(sammenligningstab)

# ==========================================
# 7. TOP 10 OVER BEDSTE INDIKATORER & KORRELATION
# ==========================================
# Saml alle resultater i én samlet dataframe
alle_resultater <- data.frame(
  ID = paste0("V", 1:length(r2_resultater)), # Viser præcis hvilken V-kolonne det er
  Antal_spm = sapply(strsplit(komb_navne, "\\+"), length), 
  Spørgsmål = komb_navne,
  R_squared = round(r2_resultater, 4),
  Korrelation = round(sqrt(r2_resultater), 4)
)

# Sortér efter højeste R^2 og gem de 10 bedste
top_10_liste <- alle_resultater %>%
  arrange(desc(R_squared)) %>%
  head(10)

# Udskriv resultatet pænt
cat("\n--- TOP 10 BEDSTE INDIKATORER (ALGORITME) ---\n")
print(top_10_liste, row.names = FALSE)

# ==========================================
# 10. MULTIPEL LINEÆR REGRESSION (ALLE 11 SPØRGSMÅL)
# ==========================================
# Flet de 11 rå spørgsmål sammen med data for forbrugsvækst
# (forv1_kvt bruger navnet "TID", mens df_forbrug_vaekst bruger "TID_kvartal")
data_multi <- inner_join(forv1_kvt, df_forbrug_vaekst, by = c("TID" = "TID_kvartal"))

# Fjern de kolonner, der ikke skal med i selve regressionsmodellen 
# (TID giver ikke mening i regressionen, og Realforbrug fjernes, da vi kigger på Vaekst_YOY)
data_til_model <- data_multi %>%
  select(-TID, -Realforbrug)

# Kør den multiple regression. 
# Punktumet betyder: "Forudsig Vaekst_YOY_procent ud fra resten af kolonnerne"
model_multipel <- lm(Vaekst_YOY_procent ~ ., data = data_til_model)

# Udskriv de samlede resultater og p-værdier for hvert enkelt spørgsmål
summary(model_multipel)

# ===============================================================
# OPGAVE 1.4 - KIGGER PÅ FAKTISKE VINDER OG VORES ANALYTIKSE BUD
# ===============================================================
# 1. Klargør begge indikatorer i datasættet
analyse_data$Algoritme_Vinder <- analyse_data[[vinder_index]] # Maskinens vinder (højeste R^2)
analyse_data$Kvalitativ_Vinder <- analyse_data[["V952"]]      # Jeres udvalgte vinder (>1 spm)

# 2. Byg to separate regressionsmodeller
model_algo <- lm(Vaekst_YOY_procent ~ Algoritme_Vinder, data = analyse_data)
model_kval <- lm(Vaekst_YOY_procent ~ Kvalitativ_Vinder, data = analyse_data)

# 3. Sortér data og træk de 2 absolut nyeste kvartaler ud
z_df <- z_df[order(z_df$TID_kvartal), ]
nyeste_to_kvartaler <- tail(z_df, 2)

kvartaler_navne <- nyeste_to_kvartaler$TID_kvartal
algo_tal <- nyeste_to_kvartaler[[vinder_index]]
kval_tal <- nyeste_to_kvartaler[["V952"]]

# 4. Gør data klar og kør forudsigelserne
nye_data_algo <- data.frame(Algoritme_Vinder = algo_tal)
nye_data_kval <- data.frame(Kvalitativ_Vinder = kval_tal)

forudsigelser_algo <- predict(model_algo, newdata = nye_data_algo)
forudsigelser_kval <- predict(model_kval, newdata = nye_data_kval)

# 5. Udskriv resultaterne flot og adskilt
cat("\n--- FORUDSIGELSE 1: Den matematiske bedste indikator (Kolonne V", vinder_index, ") ---\n", sep="")
for (i in 1:length(kvartaler_navne)) {
  cat(kvartaler_navne[i], "- Forventet realvækst:", round(forudsigelser_algo[i], 2), 
      "% (Forbrugertillid:", round(algo_tal[i], 2), ")\n")
}

cat("\n--- FORUDSIGELSE 2: VORES KVALITATIVE bedste indikator (Kolonne V952) ---\n")
for (i in 1:length(kvartaler_navne)) {
  cat(kvartaler_navne[i], "- Forventet realvækst:", round(forudsigelser_kval[i], 2), 
      "% (Forbrugertillid:", round(kval_tal[i], 2), ")\n")
}

# ==========================================
# OPGAVE 1.5: TOP 10 BEDSTE MIKRO-INDIKATORER
# ==========================================
# Vi filtrerer 'alle_resultater', så vi udelukker alle indikatorer, 
# der indeholder makro-spørgsmål (Danmarks økonomi, priser, arbejdsløshed).
# Det efterlader kun de indikatorer, der bygger på familiens egen økonomi.
mikro_resultater <- alle_resultater %>%
  filter(!grepl("Danmarks|Priser|Arbejdsløsheden", Spørgsmål))

# Sortér efter højeste R^2 og gem de 10 bedste mikro-indikatorer
top_10_mikro <- mikro_resultater %>%
  arrange(desc(R_squared)) %>%
  head(10)

# Udskriv resultatet som en pæn tabel i konsollen
cat("\n--- TOP 10 BEDSTE RENE MIKRO-INDIKATORER ---\n")
print(top_10_mikro, row.names = FALSE)


# ==========================================
# Samlet konklusion
# ==========================================
# 1. Konverter "2000K1" formatet til en rigtig dato
analyse_data$Dato <- yq(gsub("K", "Q", analyse_data$TID_kvartal))

# 2. Sørg for at vinder-indikatoren ligger klar som sin egen kolonne
analyse_data$Vinder_Indikator <- analyse_data[[vinder_index]]

# 3. Lav den professionelle graf
ggplot(analyse_data, aes(x = Dato)) +
  # Tegner linjerne lidt tykkere (linewidth = 1.2)
  geom_line(aes(y = scale(Vaekst_YOY_procent), color = "Realvækst i privatforbrug"), linewidth = 1.2) +
  geom_line(aes(y = scale(Vinder_Indikator), color = "Vores optimerede indikator"), linewidth = 1.2, linetype = "twodash") +
  
  # Tilføjer professionelle farver (Mørkeblå og Børsen-rød)
  scale_color_manual(values = c("Realvækst i privatforbrug" = "#2C3E50", 
                                "Vores optimerede indikator" = "#E74C3C")) +
  
  # Gør x-aksen pænere, så den kun viser årstal med passende mellemrum
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  
  # Overskrifter og tekster
  labs(
    title = "Forbrugertillid vs. Faktisk forbrugsvækst",
    subtitle = "Udviklingen fra 2000 til 2026 (Standardiserede værdier)",
    caption = "Kilde: Egen beregning på baggrund af data fra Danmarks Statistik",
    x = NULL, # Fjerner teksten "Dato", da årstallene taler for sig selv
    y = "Standardiserede udsving (Z-score)",
    color = NULL # Fjerner overskriften på legenden for et renere look
  ) +
  
  # Det visuelle tema
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#333333"),
    plot.subtitle = element_text(size = 14, color = "#666666", margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#999999", hjust = 1, margin = margin(t = 15)),
    legend.position = "top", # Rykker boksen med farver op over grafen
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#EAEAEA"), # Meget lysegrå streger
    panel.grid.minor = element_blank(), # Fjerner de små hjælpestreger for ro i billedet
    axis.text = element_text(color = "#444444")
  )
