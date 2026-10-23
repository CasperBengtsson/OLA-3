# =====================================================================
#  CONSUMER CONFIDENCE AND HOUSEHOLD CONSUMPTION
#
#  Part A  Data: survey questions, consumption growth, confidence indicator
#  Part B  PCA regression: which survey questions explain consumption growth?
#  Part C  Nowcast: consumption growth in the newest quarter
#  Part D  Christmas: will Q4 consumption beat last year's Q4?
#  Part E  PCA regression with the pls package (teacher's method, opgave 2.1-2.2)
#  Summary The key numbers from B-E in one place
#
#  Every setting you might want to change is in "0. SETTINGS".
# =====================================================================

library(dkstat)
library(tidyr)


# =====================================================================
# 0. SETTINGS
# =====================================================================

pca_start       <- "2000M01"                       # first survey month used in Part B and C
f_koder         <- paste0("F", c(2:10, 12:13))     # the 11 questions (F11 dropped: stops in 2025)
omvendt         <- c("F6", "F7", "F8")             # questions where a HIGH number is BAD news
nowcast_kvartal <- "2026K3"                        # quarter to nowcast in Part C
jul_start       <- "1992K1"                        # first quarter the Christmas model may learn from
jul_mål         <- "2026K4"                        # the Christmas quarter to predict
jul_startvindue <- 40                              # quarters used to train before the first test


# =====================================================================
# HELPERS (small functions used several times)
# =====================================================================

# Pick ONE value out of a DST variable by pattern; stop if 0 or 2+ hits
vaelg <- function(tekster, moenster) {
  hit <- tekster[grepl(moenster, tekster)]
  if (length(hit) != 1) {
    stop("Pattern '", moenster, "' matched ", length(hit), " values:\n",
         paste(hit, collapse = "\n"),
         "\nAll available values:\n", paste(tekster, collapse = "\n"))
  }
  hit
}

# "2026M08" -> "2026K3"
til_kvartal <- function(tid) {
  paste0(substr(tid, 1, 4), "K", ceiling(as.numeric(substr(tid, 6, 7)) / 3))
}

# "2026K3" -> "2025K3"
samme_kvartal_året_før <- function(kv) {
  paste0(as.numeric(substr(kv, 1, 4)) - 1, substr(kv, 5, 6))
}

# Move a column down k rows: each row then holds the value from k rows earlier
forskyd <- function(x, k) c(rep(NA, k), head(x, -k))

# Headings in the printed output
overskrift <- function(tekst) {
  cat("\n", strrep("=", 72), "\n ", tekst, "\n", strrep("=", 72), "\n", sep = "")
}
underoverskrift <- function(tekst) cat("\n--- ", tekst, " ---\n", sep = "")


# #####################################################################
#  PART A. DATA
# #####################################################################
overskrift("A. DATA")

# ---------------------------------------------------------------------
# A1. Survey questions (FORV1): monthly -> quarterly
# ---------------------------------------------------------------------
# FORV1 has 13 values: F1 = the finished confidence indicator, F2-F13 = the
# 12 questions. Here we keep the questions, minus F11 ("Anser det som
# fornuftigt at spare op ..."), which DST stopped publishing in 2025.

forv1_meta <- dst_meta(table = "FORV1", lang = "da")

spørgsmål <- forv1_meta$values$INDIKATOR$text
spørgsmål <- spørgsmål[!grepl("Forbrugertillidsindikatoren|Anser det som fornuftigt", spørgsmål)]
stopifnot(length(spørgsmål) == length(f_koder))

forv1 <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = spørgsmål, Tid = "*"),
  meta_data = forv1_meta,
  lang = "da",
  parse_dst_tid = FALSE
)
forv1 <- forv1[forv1$TID >= pca_start, ]
forv1 <- as.data.frame(pivot_wider(forv1, id_cols = TID,
                                   names_from = INDIKATOR, values_from = value))
forv1 <- forv1[order(forv1$TID), ]

# Quarterly value = mean of the 3 months. If a month is missing, the quarter
# becomes NA on purpose and is dropped later.
kvartal   <- til_kvartal(forv1$TID)
forv1_kvt <- aggregate(forv1[, -1], by = list(TID = kvartal), FUN = mean)

# Replace the long question texts with DST's codes (F2, F3, ...)
tekster  <- names(forv1_kvt)[-1]
har_kode <- grepl("^F[0-9]+ ", tekster)                 # text starts with "F10 ..."?
f_nr     <- sub("^(F[0-9]+) .*", "\\1", tekster)
f_nr[!har_kode] <- NA
if (anyNA(f_nr)) {                                       # otherwise look it up in the metadata
  rens <- function(x) gsub(" +", " ", trimws(x))
  f_nr <- forv1_meta$values$INDIKATOR$id[
    match(rens(tekster), rens(forv1_meta$values$INDIKATOR$text))]
}
if (anyNA(f_nr) || !setequal(f_nr, f_koder)) {
  print(tekster)
  print(forv1_meta$values$INDIKATOR)
  stop("Could not work out the F-codes - compare the two printouts above")
}
spørgsmål_tabel      <- data.frame(F_nr = f_nr, tekst = tekster)
names(forv1_kvt)[-1] <- f_nr
forv1_kvt            <- forv1_kvt[, c("TID", f_koder)]

# ---------------------------------------------------------------------
# A2. Household consumption (NKN1): real year-on-year growth
# ---------------------------------------------------------------------
# P.31 household consumption, 2020 prices (real), seasonally adjusted.

nkn1_meta <- dst_meta(table = "NKN1", lang = "da")
stopifnot(all(c("TRANSAKT", "PRISENHED", "SÆSON") %in% names(nkn1_meta$values)))

nkn1 <- dst_get_data(
  table = "NKN1",
  query = list(
    TRANSAKT  = vaelg(nkn1_meta$values$TRANSAKT$text, "^P\\.31 Husholdningernes forbrugsudgifter$"),
    PRISENHED = vaelg(nkn1_meta$values$PRISENHED$text, "^2020-priser"),
    SÆSON     = vaelg(nkn1_meta$values$SÆSON$text,     "^Sæsonkorrigeret$"),
    Tid       = "*"
  ),
  meta_data = nkn1_meta,
  lang = "da",
  parse_dst_tid = FALSE
)
nkn1 <- nkn1[order(nkn1$TID), c("TID", "value")]
names(nkn1)[2] <- "forbrug"
nkn1 <- nkn1[!is.na(nkn1$forbrug), ]                     # NKN1 is blank for 1990
stopifnot(!anyDuplicated(nkn1$TID))

# growth = (level now / level same quarter last year - 1) * 100
n <- nrow(nkn1)
nkn1$vækst <- c(rep(NA, 4), (nkn1$forbrug[5:n] / nkn1$forbrug[1:(n - 4)] - 1) * 100)

# ---------------------------------------------------------------------
# A3. DST's consumer confidence indicator (F1): monthly -> quarterly
# ---------------------------------------------------------------------
tillid_navn <- forv1_meta$values$INDIKATOR$text[
  grepl("Forbrugertillidsindikatoren", forv1_meta$values$INDIKATOR$text)]
stopifnot(length(tillid_navn) == 1)

tillid <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = tillid_navn, Tid = "*"),
  meta_data = forv1_meta,
  lang = "da",
  parse_dst_tid = FALSE
)
tillid <- tillid[tillid$TID >= "1990M01" & !is.na(tillid$value), ]
tillid$kvartal <- til_kvartal(tillid$TID)

tillid_kvt <- aggregate(value ~ kvartal, data = tillid, FUN = mean)
names(tillid_kvt) <- c("TID", "tillid")

# Before 1996 DST skipped the July survey, so those Q3s have 2 months.
# Keep quarters with at least 2 months; an unfinished quarter (1 month) is dropped.
mdr <- table(tillid$kvartal)
tillid_kvt <- tillid_kvt[tillid_kvt$TID %in% names(mdr)[mdr >= 2], ]

underoverskrift("What data do we have?")
cat(sprintf("  Survey questions (FORV1) : %d questions, %s to %s\n",
            length(f_koder), forv1$TID[1], forv1$TID[nrow(forv1)]))
cat(sprintf("  Consumption (NKN1)       : %s to %s (growth from %s)\n",
            nkn1$TID[1], nkn1$TID[n], nkn1$TID[5]))
cat(sprintf("  Confidence indicator F1  : %s to %s (quarters)\n",
            tillid_kvt$TID[1], tillid_kvt$TID[nrow(tillid_kvt)]))


# #####################################################################
#  PART B. PCA REGRESSION
# #####################################################################
overskrift("B. PCA REGRESSION: which survey questions explain consumption growth?")

# ---------------------------------------------------------------------
# B1. One table: questions (X) and growth (y) on the same quarters
# ---------------------------------------------------------------------
data <- merge(forv1_kvt, nkn1[, c("TID", "vækst")], by = "TID")
data <- data[complete.cases(data), ]

X <- as.matrix(data[, f_koder])
y <- data$vækst
p <- ncol(X)

# Flip F6, F7, F8 so that for every question: positive = optimism
X[, omvendt] <- -X[, omvendt]

cat(sprintf("  Quarters used: %d (%s to %s), questions: %d\n",
            nrow(X), data$TID[1], data$TID[nrow(data)], p))

# ---------------------------------------------------------------------
# B2. PCA: squeeze the 11 overlapping questions into summary scores
# ---------------------------------------------------------------------
# scale. = TRUE puts every question on the same scale first, so questions
# that swing a lot (F7) do not dominate questions that move little (F10).
pca         <- prcomp(X, center = TRUE, scale. = TRUE)
egenværdier <- pca$sdev^2              # information per component (total = p)
scores      <- pca$x                   # each quarter's value on each component

varians <- data.frame(
  PC         = paste0("PC", 1:p),
  egenværdi  = round(egenværdier, 2),
  andel      = round(100 * egenværdier / sum(egenværdier), 1),
  kumulativ  = round(100 * cumsum(egenværdier) / sum(egenværdier), 1)
)
underoverskrift("B2a. Information per component (eigenvalue > 1 = worth more than one question)")
print(varians, row.names = FALSE)

underoverskrift("B2b. What each component is made of (loadings, first 4 components)")
print(round(pca$rotation[, 1:4], 2))

# ---------------------------------------------------------------------
# B3. Regression of growth on the first k components
# ---------------------------------------------------------------------
pcr_fit <- function(kk) lm(y ~ ., data = as.data.frame(scores[, 1:kk, drop = FALSE]))

tabel_k <- data.frame(k = 1:p, r2 = NA, adj_r2 = NA, loocv_rmse = NA)
for (i in 1:p) {
  m <- pcr_fit(i)
  tabel_k$r2[i]         <- summary(m)$r.squared
  tabel_k$adj_r2[i]     <- summary(m)$adj.r.squared
  tabel_k$loocv_rmse[i] <- sqrt(mean((resid(m) / (1 - hatvalues(m)))^2))
}
underoverskrift("B3a. Fit for k = 1 ... p components (loocv_rmse = typical miss on an unseen quarter)")
print(round(tabel_k, 3), row.names = FALSE)

# Kaiser rule: keep components worth more than one question. Overwrite by hand if wanted.
k <- sum(egenværdier > 1)
pcr_model <- pcr_fit(k)

cat(sprintf("\n  Components kept (Kaiser rule): k = %d, together %.1f %% of the information\n",
            k, varians$kumulativ[k]))
underoverskrift("B3b. The chosen model: growth = a + g1*PC1 + ... + gk*PCk")
print(summary(pcr_model))

# ---------------------------------------------------------------------
# B4. Back to the questions: weight per question
# ---------------------------------------------------------------------
# weight of a question = sum over components of (loading x coefficient)
# Unit: pp of growth per 1 standard deviation change in the question.
pcr_vægte <- function(kk) {
  as.vector(pca$rotation[, 1:kk, drop = FALSE] %*% coef(pcr_fit(kk))[-1])
}
vægt <- pcr_vægte(k)
names(vægt) <- colnames(X)

# Check: the weights must reproduce the model's fitted values exactly
fit_fra_vægte <- as.numeric(scale(X) %*% vægt) + as.numeric(coef(pcr_model)[1])
stopifnot(isTRUE(all.equal(fit_fra_vægte, as.numeric(fitted(pcr_model)))))

resultat <- data.frame(
  F_nr        = colnames(X),
  vægt        = round(vægt, 3),
  korrelation = round(as.vector(cor(X, y)), 3),
  tekst       = substr(spørgsmål_tabel$tekst[match(colnames(X), spørgsmål_tabel$F_nr)], 1, 50)
)
resultat <- resultat[order(-abs(resultat$vægt)), ]
underoverskrift(paste0("B4. Weight per question (k = ", k, "), biggest first. ",
                       "korrelation = question alone vs growth"))
print(resultat, row.names = FALSE)

# ---------------------------------------------------------------------
# B5. Do the weights depend on k?
# ---------------------------------------------------------------------
k_valg <- unique(c(1, 2, 3, 4, p))
vægt_k <- sapply(k_valg, pcr_vægte)
dimnames(vægt_k) <- list(colnames(X), paste0("k=", k_valg))

underoverskrift("B5. Weights for different k")
print(round(vægt_k, 2))

største <- apply(abs(vægt_k), 2, function(x) rownames(vægt_k)[which.max(x)])
cat("\n  Biggest weight per k: ", paste(names(største), største, sep = " -> ", collapse = " | "), "\n")

# ---------------------------------------------------------------------
# B6. Pictures
# ---------------------------------------------------------------------
par(mfrow = c(1, 2))
plot(egenværdier, type = "b", xlab = "Component", ylab = "Eigenvalue", main = "Scree plot")
abline(h = 1, lty = 2)
barplot(sort(vægt), horiz = TRUE, las = 1,
        main = paste("Question weights, k =", k),
        xlab = "pp growth per 1 sd of question")
par(mfrow = c(1, 1))


# #####################################################################
#  PART C. NOWCAST
# #####################################################################
overskrift(paste("C. NOWCAST: yearly real consumption growth in", nowcast_kvartal))

# ---------------------------------------------------------------------
# C1. Predict the new quarter with the Part B model
# ---------------------------------------------------------------------
antal_mdr <- sum(kvartal == nowcast_kvartal)
stopifnot(antal_mdr == 3)                                 # all 3 survey months must exist

ny_x <- as.matrix(forv1_kvt[forv1_kvt$TID == nowcast_kvartal, f_koder])
ny_x[, omvendt] <- -ny_x[, omvendt]                       # flipped like the training data
stopifnot(nrow(ny_x) == 1, !anyNA(ny_x))

# Convert with the TRAINING data's centring, scaling and rotation, then predict
ny_scores <- as.data.frame(predict(pca, newdata = ny_x)[, 1:k, drop = FALSE])
nowcast   <- predict(pcr_model, newdata = ny_scores, interval = "prediction", level = 0.95)

cat(sprintf("  Nowcast %s: %.1f %% y/y  (95%% interval %.1f to %.1f)\n",
            nowcast_kvartal, nowcast[1, "fit"], nowcast[1, "lwr"], nowcast[1, "upr"]))

# ---------------------------------------------------------------------
# C2. Is it realistic?
# ---------------------------------------------------------------------
underoverskrift("C2a. Latest actual growth (NKN1)")
print(tail(data.frame(TID = nkn1$TID, vækst = round(nkn1$vækst, 2)), 5), row.names = FALSE)

sammenlign <- data.frame(TID       = data$TID,
                         faktisk   = round(y, 2),
                         model     = round(fitted(pcr_model), 2),
                         afvigelse = round(resid(pcr_model), 2))
underoverskrift("C2b. Model vs actual, last 8 quarters (afvigelse = actual - model)")
print(tail(sammenlign, 8), row.names = FALSE)
cat(sprintf("  Average miss, last 8 quarters: %.2f pp\n", mean(tail(resid(pcr_model), 8))))

# What the nowcast means for the LEVEL, compared with normal quarterly changes
niveau     <- setNames(nkn1$forbrug, nkn1$TID)
år_før     <- samme_kvartal_året_før(nowcast_kvartal)
seneste    <- nkn1$TID[nrow(nkn1)]
nyt_niveau <- niveau[[år_før]] * (1 + nowcast[1, c("fit", "lwr", "upr")] / 100)
qq         <- (nyt_niveau / niveau[[seneste]] - 1) * 100
qq_hist    <- diff(nkn1$forbrug) / head(nkn1$forbrug, -1) * 100

underoverskrift("C2c. What the nowcast implies for the level")
cat(sprintf("  Implied change from %s: %.2f %% (interval %.2f to %.2f)\n",
            seneste, qq["fit"], qq["lwr"], qq["upr"]))
cat(sprintf("  Normal quarterly change: 5%% = %.2f, median = %.2f, 95%% = %.2f\n",
            quantile(qq_hist, 0.05), quantile(qq_hist, 0.5), quantile(qq_hist, 0.95)))

nowcast_k <- sapply(1:p, function(kk) {
  predict(pcr_fit(kk), newdata = as.data.frame(predict(pca, ny_x)[, 1:kk, drop = FALSE]))
})
names(nowcast_k) <- paste0("k=", 1:p)
underoverskrift("C2d. The nowcast with 1 ... p components")
print(round(nowcast_k, 2))

sidste <- tail(sammenlign, 16)
x_akse <- 1:16
plot(x_akse, sidste$faktisk, type = "l", xaxt = "n", xlim = c(1, 17),
     ylim = range(c(sidste$faktisk, sidste$model, nowcast[1, c("lwr", "upr")])),
     xlab = "", ylab = "Real growth (% y/y)", main = paste("Nowcast", nowcast_kvartal))
lines(x_akse, sidste$model, lty = 2)
points(17, nowcast[1, "fit"], pch = 19, col = "red")
segments(17, nowcast[1, "lwr"], 17, nowcast[1, "upr"], col = "red")
axis(1, at = c(x_akse, 17), labels = c(sidste$TID, nowcast_kvartal), las = 2, cex.axis = 0.7)
legend("topleft", bty = "n", lty = c(1, 2, NA), pch = c(NA, NA, 19),
       col = c("black", "black", "red"),
       legend = c("Actual", "Model (fitted)", "Nowcast + 95% interval"))


# #####################################################################
#  PART D. CHRISTMAS
# #####################################################################
overskrift(paste("D. CHRISTMAS: will consumption in", jul_mål, "beat",
                 samme_kvartal_året_før(jul_mål), "?"))

# ---------------------------------------------------------------------
# D1. One row per quarter, with predictors known BEFORE the quarter
# ---------------------------------------------------------------------
# The calendar runs to the end of the target year, so the Christmas quarter
# has a row even though it has not happened yet.
aar   <- 1990:as.numeric(substr(jul_mål, 1, 4))
kal_q <- data.frame(TID = paste0(rep(aar, each = 4), "K", rep(1:4, times = length(aar))))
kal_q <- merge(kal_q, tillid_kvt, by = "TID", all.x = TRUE)
kal_q <- merge(kal_q, nkn1[, c("TID", "vækst")], by = "TID", all.x = TRUE)
kal_q <- kal_q[order(kal_q$TID), ]

kal_q$tillid_lag1    <- forskyd(kal_q$tillid, 1)                      # confidence, last quarter
kal_q$tillid_ændring <- kal_q$tillid_lag1 - forskyd(kal_q$tillid, 5)  # ... vs a year before that
kal_q$vækst_lag2     <- forskyd(kal_q$vækst, 2)                       # latest PUBLISHED growth
kal_q$højere         <- as.integer(kal_q$vækst > 0)                   # 1 = higher than last year

# Training data: every quarter with all inputs and a known outcome.
# All quarters, not only Q4: "higher than the same quarter last year" works
# the same way every quarter, and Q4 alone has too few "not higher" cases.
træn_q <- kal_q[complete.cases(kal_q[, c("højere", "tillid_lag1", "tillid_ændring", "vækst_lag2")]), ]
træn_q <- træn_q[træn_q$TID >= jul_start, ]
rownames(træn_q) <- NULL

underoverskrift("D1. Data the model learns from")
cat(sprintf("  Training quarters : %d (%s to %s)\n",
            nrow(træn_q), træn_q$TID[1], træn_q$TID[nrow(træn_q)]))
cat(sprintf("  Share 'higher'    : %.0f %%\n", 100 * mean(træn_q$højere)))
if (træn_q$TID[1] > jul_start) {
  cat(sprintf("  Note: you asked for %s, but the first usable quarter is %s.\n",
              jul_start, træn_q$TID[1]))
  cat(sprintf("        NKN1 starts in %s, so growth exists from %s, and the model\n",
              nkn1$TID[1], nkn1$TID[5]))
  cat("        also needs growth from 2 quarters earlier.\n")
}

# ---------------------------------------------------------------------
# D2. The model (logistic regression: gives a probability of 'higher')
# ---------------------------------------------------------------------
formel_q <- højere ~ tillid_lag1 + tillid_ændring + vækst_lag2
model_q  <- glm(formel_q, family = binomial, data = træn_q)

underoverskrift("D2. The model")
print(summary(model_q))

# ---------------------------------------------------------------------
# D3. Forecast for the Christmas quarter
# ---------------------------------------------------------------------
ny_q <- kal_q[kal_q$TID == jul_mål, c("tillid_lag1", "tillid_ændring", "vækst_lag2")]
stopifnot(nrow(ny_q) == 1, !anyNA(ny_q))

fit_q <- predict(model_q, newdata = ny_q, type = "link", se.fit = TRUE)
p_q   <- plogis(fit_q$fit + c(0, -1.96, 1.96) * fit_q$se.fit)
names(p_q) <- c("estimat", "nedre95", "øvre95")

underoverskrift(paste("D3. Inputs used for", jul_mål))
print(round(ny_q, 2), row.names = FALSE)

# ---------------------------------------------------------------------
# D4. Walk-forward test: predict each quarter using only earlier data
# ---------------------------------------------------------------------
# For test quarter j, train on rows 1..(j-2): last quarter's growth is not
# published yet. The first jul_startvindue + 1 quarters can only be used for
# training - there is nothing earlier to test them with.
test_q <- data.frame(TID = træn_q$TID, faktisk = træn_q$højere, p = NA)
for (j in (jul_startvindue + 2):nrow(træn_q)) {
  m <- suppressWarnings(glm(formel_q, family = binomial, data = træn_q[1:(j - 2), ]))
  test_q$p[j] <- predict(m, newdata = træn_q[j, ], type = "response")
}
test_q <- test_q[!is.na(test_q$p), ]
test_q$forudsagt <- as.integer(test_q$p > 0.5)

underoverskrift("D4. Walk-forward test")
cat(sprintf("  Tested quarters : %d (%s to %s)\n",
            nrow(test_q), test_q$TID[1], test_q$TID[nrow(test_q)]))
cat(sprintf("  Not tested      : the first %d training quarters (only used to train the first model)\n",
            jul_startvindue + 1))

# ---------------------------------------------------------------------
# D5. Confusion matrix and accuracy
# ---------------------------------------------------------------------
udfald     <- c("ikke højere", "højere")
forvirring <- table(forudsagt = factor(test_q$forudsagt, levels = 0:1, labels = udfald),
                    faktisk   = factor(test_q$faktisk,   levels = 0:1, labels = udfald))

træfsikkerhed <- sum(diag(forvirring)) / sum(forvirring)
sensitivitet  <- forvirring["højere", "højere"] / sum(forvirring[, "højere"])
specificitet  <- forvirring["ikke højere", "ikke højere"] / sum(forvirring[, "ikke højere"])
altid_højere  <- mean(test_q$faktisk == 1)
vedvarende    <- mean(as.integer(træn_q$vækst_lag2[match(test_q$TID, træn_q$TID)] > 0) == test_q$faktisk)

underoverskrift("D5. Confusion matrix (rows = predicted, columns = actual)")
print(forvirring)

jul_q   <- test_q[substr(test_q$TID, 5, 6) == "K4", ]
jul_q$p <- round(jul_q$p, 2)
underoverskrift("D6. The Christmas quarters in the test (p = predicted probability of 'higher')")
print(jul_q, row.names = FALSE)

underoverskrift("D7. Christmas result")
cat(sprintf("  P(%s higher than %s) : %.2f  (95%% CI %.2f - %.2f)\n",
            jul_mål, samme_kvartal_året_før(jul_mål), p_q["estimat"], p_q["nedre95"], p_q["øvre95"]))
cat(sprintf("  Accuracy (out-of-sample)      : %.1f %% over %d quarters\n", 100 * træfsikkerhed, nrow(test_q)))
cat(sprintf("  Sensitivity / specificity     : %.1f %% / %.1f %%\n", 100 * sensitivitet, 100 * specificitet))
cat(sprintf("  Benchmarks                    : always 'higher' %.1f %% | last published growth %.1f %%\n",
            100 * altid_højere, 100 * vedvarende))
cat(sprintf("  Christmas quarters correct    : %d of %d\n",
            sum(jul_q$forudsagt == jul_q$faktisk), nrow(jul_q)))


# #####################################################################
#  PART E. PCA REGRESSION WITH THE pls PACKAGE (teacher's method)
#          Opgave 2.1: Y = yearly real consumption growth, X = the survey questions
#          Opgave 2.2: which 3 questions have the highest weights?
#          Same 11 questions as Part A (F11 left out: DST stopped it in 2025)
# #####################################################################
overskrift("E. PCA REGRESSION WITH pls::pcr (opgave 2.1 and 2.2)")

library(pls)

# Cross-validation (validation = "CV") splits the data into RANDOM groups.
# Without a fixed seed the CV numbers change every run - and differ between
# group members. set.seed() makes everyone get exactly the same result.
set.seed(1)

# ---------------------------------------------------------------------
# E1. Data: the same 11 questions and quarters as Part B
# ---------------------------------------------------------------------
# Not flipped, exactly like the teacher's code (flipping does not change the
# weights in E3, because they are squared).
data_ml <- merge(forv1_kvt[, c("TID", f_koder)], nkn1[, c("TID", "vækst")], by = "TID")
data_ml <- data_ml[complete.cases(data_ml), ]

underoverskrift("E1. Data")
cat(sprintf("  Quarters: %d (%s to %s), questions: %d\n",
            nrow(data_ml), data_ml$TID[1], data_ml$TID[nrow(data_ml)], length(f_koder)))

# ---------------------------------------------------------------------
# E2. Opgave 2.1 - the PCA regression (same structure as the teacher's code)
# ---------------------------------------------------------------------
# vækst ~ .  means "growth explained by ALL other columns", i.e. the 11 questions.
# scale = TRUE standardises the questions, as prcomp(scale. = TRUE) in Part B.
pcr.fit <- pcr(vækst ~ ., data = data_ml[, c("vækst", f_koder)],
               scale = TRUE, validation = "CV")

underoverskrift("E2a. summary(pcr.fit): CV error and % variance explained per number of components")
summary(pcr.fit)

# The teacher's validation plot: prediction error (MSEP) for 0, 1, 2, ... components.
# Look for where the curve stops falling - adding components after that does not help.
validationplot(pcr.fit, val.type = "MSEP", main = "PCR: cross-validated error")

# ---------------------------------------------------------------------
# E3. Opgave 2.2 - weights: squared loadings on the FIRST component
# ---------------------------------------------------------------------
# A loading says how much a question contributes to a component. Squaring
# removes the sign and makes the weights add up to 1 (= 100 %).
# The teacher's  loadings.pcr.fit[1:5]^2  is the same as  [, 1]^2  with 5 questions.
loadings.pcr.fit <- pcr.fit$loadings
w.indicators.1   <- loadings.pcr.fit[, 1]^2
stopifnot(abs(sum(w.indicators.1) - 1) < 1e-8)

vægt_pc1 <- data.frame(
  F_nr      = names(w.indicators.1),
  loading   = round(loadings.pcr.fit[, 1], 3),
  vægt_pct  = round(100 * w.indicators.1, 1),
  tekst     = substr(spørgsmål_tabel$tekst[match(names(w.indicators.1), spørgsmål_tabel$F_nr)], 1, 50)
)
vægt_pc1 <- vægt_pc1[order(-vægt_pc1$vægt_pct), ]

underoverskrift("E3. Weight of each question in component 1 (squared loading, sums to 100 %)")
print(vægt_pc1, row.names = FALSE)

cat(sprintf("\n  The 3 highest weights: %s\n", paste(vægt_pc1$F_nr[1:3], collapse = ", ")))
cat(sprintf("  Component 1 explains %.1f %% of the variation in the questions\n",
            explvar(pcr.fit)[1]))

barplot(rev(vægt_pc1$vægt_pct), names.arg = rev(vægt_pc1$F_nr), horiz = TRUE, las = 1,
        main = "Weight in component 1 (squared loading)", xlab = "% of component 1")

# ---------------------------------------------------------------------
# E4. Three different "weights" side by side
# ---------------------------------------------------------------------
#   pc1_vægt_pct : teacher's weight - how much of the "mood" component the
#                  question makes up (does not look at consumption at all)
#   pcr_vægt     : Part B's weight - how much the k-component regression
#                  leans on the question when predicting consumption
#   korrelation  : the question on its own vs consumption growth
# (pcr_vægt and korrelation use the flipped F6-F8 from Part B.)
sammenlign_vægte <- data.frame(
  F_nr         = f_koder,
  pc1_vægt_pct = round(100 * w.indicators.1[f_koder], 1),
  pcr_vægt     = round(vægt[f_koder], 2),
  korrelation  = round(as.vector(cor(X, y)), 2)
)
sammenlign_vægte$rang_pc1 <- rank(-sammenlign_vægte$pc1_vægt_pct, ties.method = "min")
sammenlign_vægte$rang_pcr <- rank(-abs(sammenlign_vægte$pcr_vægt), ties.method = "min")
sammenlign_vægte$rang_kor <- rank(-abs(sammenlign_vægte$korrelation), ties.method = "min")
sammenlign_vægte <- sammenlign_vægte[order(sammenlign_vægte$rang_pc1), ]

underoverskrift(paste0("E4. Three kinds of weight (rank 1 = biggest; pcr_vægt uses k = ", k, ")"))
print(sammenlign_vægte, row.names = FALSE)


# #####################################################################
#  SUMMARY
# #####################################################################
overskrift("SUMMARY")
største_vægt <- names(vægt)[which.max(abs(vægt))]
cat(sprintf("  B. PCA regression : k = %d, R2 = %.2f, biggest weight %s (%.2f)\n",
            k, summary(pcr_model)$r.squared, største_vægt, vægt[største_vægt]))
cat(sprintf("  C. Nowcast %s : %.1f %% y/y (95%% interval %.1f to %.1f)\n",
            nowcast_kvartal, nowcast[1, "fit"], nowcast[1, "lwr"], nowcast[1, "upr"]))
cat(sprintf("  D. Christmas %s: P(higher) = %.2f, accuracy %.1f %% over %d tested quarters\n",
            jul_mål, p_q["estimat"], 100 * træfsikkerhed, nrow(test_q)))
cat(sprintf("  E. pls::pcr          : top 3 weights in component 1: %s\n",
            paste(paste0(vægt_pc1$F_nr[1:3], " (", vægt_pc1$vægt_pct[1:3], " %)"), collapse = ", ")))