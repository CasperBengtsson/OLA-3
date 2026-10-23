# ==========================================
# OPGAVE 2: PRINCIPAL COMPONENT REGRESSION (PCR)
# ==========================================
# Installér pakken, hvis du ikke har den: install.packages("pls")
library(pls)

# 1. Kør PCR-modellen baseret på metoden fra dit billede.
# Punktumet '.' betyder at vi forudsiger Vaekst_YOY ud fra ALLE de 11 spørgsmål i 'data_til_model'
pcr.fit <- pcr(Vaekst_YOY_procent ~ ., 
               data = data_til_model, 
               scale = TRUE, 
               validation = "CV")

# Vis et summary (her kigger du efter hvor meget varians, der forklares af '1 comps')
summary(pcr.fit)

# 2. Udtræk loadings præcis som vist i dit billede
loadings.pcr.fit <- pcr.fit$loadings

# 3. Beregn vægtene (kvadrerede loadings) for den FØRSTE principal component (Comp 1)
# På billedet bruges [1:5]^2 fordi de har 5 spørgsmål. 
# Vi bruger [, 1]^2 for at få alle 11 spørgsmål for den første komponent.
w.indicators.1 <- loadings.pcr.fit[, 1]^2

# Tjekker at summen er (tæt på) 1, ligesom i din vedhæftede formel
sum(w.indicators.1)

# ==========================================
# OPGAVE 2.2: IDENTIFICER DE VIGTIGSTE INDIKATORER
# ==========================================
# Sortér vægtene for at se, hvilke 3 spørgsmål der vægter højest i den vigtigste komponent
vigtigste_spm <- sort(w.indicators.1, decreasing = TRUE)[1:3]

cat("\n--- DE 3 VIGTIGSTE SPØRGSMÅL I PCR-MODELLEN ---\n")
print(vigtigste_spm)

# ==========================================
# OPGAVE 2.3: DYNAMISK FORUDSIGELSE MED PCR-MODELLEN
# ==========================================
# 1. Sortér forventningsdata kronologisk for at være sikker på, at det nyeste ligger i bunden
forv1_kvt <- forv1_kvt[order(forv1_kvt$TID), ]

# 2. Træk de 2 absolut nyeste kvartaler ud
nye_forventninger_pcr <- tail(forv1_kvt, 2)
aktuelle_kvartaler <- nye_forventninger_pcr$TID

cat("\n--- DYNAMISK FORUDSIGELSE MED PCR ---\n")
cat("Koden har fundet forventningsdata for:", paste(aktuelle_kvartaler, collapse = " og "), "\n\n")

# 3. Lav forudsigelsen for begge kvartaler
if(nrow(nye_forventninger_pcr) > 0) {
  # Forudsigelsen baseres udelukkende på 1. komponent (ncomp = 1)
  forudsigelse_pcr <- predict(pcr.fit, newdata = nye_forventninger_pcr, ncomp = 1)
  
  # Kør et loop, der udskriver en pæn sætning for hvert af de to kvartaler
  for (i in 1:nrow(nye_forventninger_pcr)) {
    kvartal <- nye_forventninger_pcr$TID[i]
    vaekst <- round(forudsigelse_pcr[i], 2)
    cat("PCR-modellens forudsigelse af realvækst for", kvartal, "(Comp 1):", vaekst, "%\n")
  }
} else {
  cat("Advarsel: Der skete en fejl. Fandt ingen forventningsdata i datasættet.\n")
}

# ==========================================
# OPGAVE 2.4: FAKTISK VÆKST I 2016 VS. DI's FORUDSIGELSE
# ==========================================
faktisk_vaekst_2016 <- df_forbrug_vaekst %>%
  # substr() udtrækker tegn 1 til 4 fra teksten (f.eks. "2016" fra "2016K1")
  filter(substr(TID_kvartal, 1, 4) == "2016") %>%
  summarise(
    Gns_Realvaekst_2016 = mean(Vaekst_YOY_procent, na.rm = TRUE)
  )

# Udskriv resultatet
cat("\n--- RESULTAT TIL OPGAVE 2.4 ---\n")
cat("Den gennemsnitlige faktiske YoY realvækst i forbruget i 2016 var:", 
    round(faktisk_vaekst_2016$Gns_Realvaekst_2016, 2), "%\n")
