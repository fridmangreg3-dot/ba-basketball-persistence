# ============================================================
# BINAERE TREND-REGRESSION - PERSISTENZ-TEST
# Bachelorarbeit: Einfluss vergangener Trefferquoten
# ============================================================
library(readxl)
library(dplyr)
library(ggplot2)
library(sandwich)
library(lmtest)
# ============================================================
# 1. EINGABEN
# ============================================================
excel_datei <- "BA_Datensatz_1.xlsx"
min_spiele <- 60  #55, 65
N <- 1            # 3, 5, 7, 9
sheet_name <- 1
spieler_spalte <- "Player"
spiele_spalte <- "G...7"
avg_spalte <- "3P%"
erste_spiel_spalte <- 9
options(OutDec = ".")
# ============================================================
# 2. DATEN EINLESEN & VORBEREITEN
# ============================================================

to_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  x <- as.character(x)
  x[x %in% c("", "NA", "N/A", "Not available", "not available", "Not Available")] <- NA
  as.numeric(x)
}

daten <- read_excel(excel_datei, sheet = sheet_name, .name_repair = "unique")
spiel_spalten <- names(daten)[erste_spiel_spalte:92]  # CN = Excel-Spalte 92

daten_gefiltert <- daten %>%
  mutate(
    "{spiele_spalte}" := as.numeric(.data[[spiele_spalte]]),
    "{avg_spalte}" := as.numeric(.data[[avg_spalte]])
  ) %>%
  filter(.data[[spiele_spalte]] >= min_spiele)

spielwerte_raw <- as.data.frame(lapply(daten_gefiltert[spiel_spalten], to_numeric))

options(OutDec = ",")

# ============================================================
# 3. HILFSFUNKTIONEN FUER PAUSENLOGIK UND REGRESSIONSDATEN
# ============================================================
finde_na_runs <- function(werte) {
  na_pos <- is.na(werte)
  runs <- rle(na_pos)
  ende <- cumsum(runs$lengths)
  start <- ende - runs$lengths + 1
  
  data.frame(
    start = start,
    ende = ende,
    laenge = runs$lengths,
    ist_na = runs$values
  )
}

berechne_spieler_beobachtungen <- function(spieler, werte, avg, N) {
  historie_werte <- numeric()
  historie_positionen <- integer()
  na_in_folge <- 0
  beobachtungen <- list()
  
  for (pos in seq_along(werte)) {
    aktueller_wert <- werte[pos]
    
    if (is.na(aktueller_wert)) {
      na_in_folge <- na_in_folge + 1
      
      if (na_in_folge >= 2) {
        historie_werte <- numeric()
        historie_positionen <- integer()
      }
      
      next
    }
    
    na_in_folge <- 0
    
    if (aktueller_wert == avg) {
      next
    }
    
    if (length(historie_werte) >= N) {
      letzte_n <- tail(historie_werte, N)
      letzte_n_positionen <- tail(historie_positionen, N)
      
      above <- sum(letzte_n > avg)
      below <- sum(letzte_n < avg)
      
      if (above != below) {
        trend <- ifelse(above > below, 1, -1)
        richtung <- ifelse(aktueller_wert > avg, 1, -1)
        
        beobachtungen[[length(beobachtungen) + 1]] <- data.frame(
          Player = spieler,
          Original_Position = pos,
          Trend_Start_Position = min(letzte_n_positionen),
          Trend_Ende_Position = max(letzte_n_positionen),
          Aktuelle_Tq = aktueller_wert,
          Avg = avg,
          Above_In_Trend = above,
          Below_In_Trend = below,
          Trend = trend,
          Richtung_Aktuelles_Spiel = richtung,
          Gleiche_Richtung = as.integer(trend == richtung),
          stringsAsFactors = FALSE
        )
      }
    }
    
    historie_werte <- c(historie_werte, aktueller_wert)
    historie_positionen <- c(historie_positionen, pos)
  }
  
  bind_rows(beobachtungen)
}

berechne_bereinigung <- function(werte, avg) {
  runs <- finde_na_runs(werte)
  na_runs <- runs[runs$ist_na, ]
  
  data.frame(
    Spiele_ohne_NA = sum(!is.na(werte)),
    Einzelne_NAs = sum(na_runs$laenge == 1),
    Pausen = sum(na_runs$laenge >= 2),
    NAs_in_Pausen = sum(na_runs$laenge[na_runs$laenge >= 2]),
    Durchschnittswerte = sum(!is.na(werte) & werte == avg),
    stringsAsFactors = FALSE
  )
}
# ============================================================
# 4. BINAEREN TREND-DATENSATZ ERSTELLEN
# ============================================================
reg_data <- bind_rows(lapply(seq_len(nrow(daten_gefiltert)), function(i) {
  berechne_spieler_beobachtungen(
    spieler = daten_gefiltert[[spieler_spalte]][i],
    werte = as.numeric(spielwerte_raw[i, ]),
    avg = daten_gefiltert[[avg_spalte]][i],
    N = N
  )
}))

bereinigung <- bind_rows(lapply(seq_len(nrow(daten_gefiltert)), function(i) {
  cbind(
    data.frame(
      Player = daten_gefiltert[[spieler_spalte]][i],
      stringsAsFactors = FALSE
    ),
    berechne_bereinigung(
      werte = as.numeric(spielwerte_raw[i, ]),
      avg = daten_gefiltert[[avg_spalte]][i]
    )
  )
}))

if (nrow(reg_data) == 0) {
  stop("Es wurden keine gueltigen Beobachtungen fuer die Regression gefunden.")
}

# ============================================================
# 5. DESKRIPTIVE STATISTIK
# ============================================================

cat("\n=== DATENUEBERSICHT ===\n")
cat("Beobachtungen:", nrow(reg_data), "\n")
cat("Spieler:", length(unique(reg_data$Player)), "\n\n")

cat("Verteilung des Trends:\n")
print(table(reg_data$Trend))

cat("\nAnteil gleicher Richtung nach Trend:\n")
for (t in c(-1, 1)) {
  anteil <- mean(reg_data$Gleiche_Richtung[reg_data$Trend == t], na.rm = TRUE)
  cat(ifelse(t == 1, "Positiver", "Negativer"), "Trend:", round(anteil * 100, 2), "%\n")
}

cat("\n--- ALLGEMEINE FORTSETZUNGSWAHRSCHEINLICHKEIT ---\n")
p_neg <- mean(reg_data$Gleiche_Richtung[reg_data$Trend == -1], na.rm = TRUE)
p_pos <- mean(reg_data$Gleiche_Richtung[reg_data$Trend == 1], na.rm = TRUE)
p_gesamt <- mean(reg_data$Gleiche_Richtung, na.rm = TRUE)


# ============================================================
# 6. PERSISTENZ-TEST (LEERMODELL OLS) - GEGEN 50%
# ============================================================
cat("\n\n=== PERSISTENZ-TEST (OLS Leermodell) ===\n")
cat("Testet: Weicht die allgemeine Fortsetzungswahrscheinlichkeit von 50% ab?\n\n")

persistenz_model <- lm(Gleiche_Richtung ~ 1, data = reg_data)
summary_persistenz <- summary(persistenz_model)

# Cluster-robuster Test für β₀ gegen 0,5 (Cluster: Spieler)
anzahl_cluster <- dplyr::n_distinct(reg_data$Player)
df_cluster <- anzahl_cluster - 1
vcov_persistenz_cluster <- sandwich::vcovCL(
  persistenz_model, cluster = ~Player, type = "HC1"
)
se_beta0_cluster <- sqrt(diag(vcov_persistenz_cluster))[1]
beta0_cluster <- coef(persistenz_model)[1]
t_beta0_cluster_gegen_50 <- (beta0_cluster - 0.5) / se_beta0_cluster
p_beta0_cluster_gegen_50 <- 2 * pt(
  -abs(t_beta0_cluster_gegen_50), df = df_cluster
)

# Standard-Ausgabe (testet gegen 0)
print(summary_persistenz)

# Manuelle Berechnung des t-Tests gegen 0.5
beta0 <- coef(summary_persistenz)[1, 1]      # Achsenabschnitt
beta0_se <- coef(summary_persistenz)[1, 2]   # Standardfehler
beta0_df <- summary_persistenz$df[2]         # Freiheitsgrade

# t-Statistik für Test gegen 0.5
t_gegen_50 <- (beta0 - 0.5) / beta0_se

# p-Wert (zweiseitig)
p_gegen_50 <- 2 * pt(-abs(t_gegen_50), df = beta0_df)


# ============================================================
# 7. PERSISTENZ-TEST (T-TEST)
# ============================================================
t_test_persistenz <- t.test(reg_data$Gleiche_Richtung, mu = 0.5)


# ============================================================
# 8. PERSISTENZ-TEST (LOGIT-LEERMODELL)
# ============================================================

cat("\n\n=== PERSISTENZ-TEST (Logit Leermodell) ===\n")
cat("Methodisch besser geeignet für binäre abhängige Variablen.\n\n")

logit_persistenz <- glm(Gleiche_Richtung ~ 1, 
                        data = reg_data, 
                        family = binomial())

summary_logit_persistenz <- summary(logit_persistenz)
print(summary_logit_persistenz)

# Log-Odds und Odds Ratio
logit_persistenz_beta0 <- coef(summary_logit_persistenz)[1, 1]
logit_persistenz_se <- coef(summary_logit_persistenz)[1, 2]
logit_persistenz_p <- coef(summary_logit_persistenz)[1, 4]
logit_persistenz_OR <- exp(logit_persistenz_beta0)

# Vorhergesagte Wahrscheinlichkeit
logit_persistenz_P <- predict(logit_persistenz, type = "response")[1]

  
# ============================================================
# 9. ASYMMETRIE-TEST (OLS) - ZUSAETZLICH
# ============================================================
cat("\n\n=== ASYMMETRIE-TEST (OLS) ===\n")
cat("Testet: Unterscheiden sich gute und schlechte Serien?\n\n")

model_asym <- lm(Gleiche_Richtung ~ Trend, data = reg_data)
summary_asym <- summary(model_asym)

beta1 <- coef(summary_asym)[2, 1]
beta1_p <- coef(summary_asym)[2, 4]

# Cluster-robuster Test für β₁ gegen 0 (keine Asymmetrie)
vcov_asym_cluster <- sandwich::vcovCL(
  model_asym, cluster = ~Player, type = "HC1"
)
se_beta1_cluster <- sqrt(diag(vcov_asym_cluster))[2]
t_beta1_cluster_gegen_0 <- beta1 / se_beta1_cluster
p_beta1_cluster_gegen_0 <- 2 * pt(
  -abs(t_beta1_cluster_gegen_0), df = df_cluster
)
p_neg_asym <- coef(model_asym)[1] + coef(model_asym)[2] * (-1)
p_pos_asym <- coef(model_asym)[1] + coef(model_asym)[2] * 1


# ============================================================
# 10. LOGIT-REGRESSION (ASYMMETRIE, ROBUST)
# ============================================================
cat("\n\n=== LOGIT-REGRESSION (ASYMMETRIE) ===\n")
logit_model <- glm(Gleiche_Richtung ~ Trend, data = reg_data, family = binomial())
summary_logit_model <- summary(logit_model)

logit_beta1 <- coef(summary_logit_model)[2, 1]
logit_beta1_p <- coef(summary_logit_model)[2, 4]
logit_odds_ratio <- exp(2 * coef(logit_model)[2])
cat("Odds Ratio (positiv vs. negativ):", round(logit_odds_ratio, 4), "\n")
cat("p-Wert:", round(logit_beta1_p, 4),
    ifelse(logit_beta1_p < 0.05, " signifikant", " nicht signifikant"), "\n")

# ============================================================
# 11. PUNKT-BISERIALE KORRELATION (TREND vs. TREFFERQUOTE)
# ============================================================

# Korrelation: Trend vs. Aktuelle_Tq (für die aktuelle Serienlänge N)
kor_trend_tq <- cor.test(
  reg_data$Trend,
  reg_data$Aktuelle_Tq,
  method = "pearson"
)


# ============================================================
# 12. VISUALISIERUNGEN
# ============================================================

format_prozent <- function(x, digits = 1) {
  paste0(formatC(x * 100, format = "f", digits = digits, decimal.mark = ","), "%")
}

# Verteilung der Richtung im Folgespiel
# Diese Werte werden in beiden Grafiken verwendet.
n_folge_gesamt <- nrow(reg_data)
n_folge_ueber <- sum(reg_data$Richtung_Aktuelles_Spiel == 1, na.rm = TRUE)
n_folge_unter <- sum(reg_data$Richtung_Aktuelles_Spiel == -1, na.rm = TRUE)
p_folge <- n_folge_ueber / n_folge_gesamt
p_folge_unter <- n_folge_unter / n_folge_gesamt
persistenz_erwartet <- p_folge^2 + p_folge_unter^2


cat("Folgespiele gesamt:", n_folge_gesamt, "\n")


# Kennzahlen für die allgemeine Persistenz
mittelwert <- mean(reg_data$Gleiche_Richtung, na.rm = TRUE)
n <- nrow(reg_data)
se <- sqrt(mittelwert * (1 - mittelwert) / n)
untergrenze <- mittelwert - 1.96 * se
obergrenze <- mittelwert + 1.96 * se
p_wert <- t_test_persistenz$p.value

# Grafik 1: Allgemeine Persistenz
# Die Referenzbeschriftungen liegen bewusst auf unterschiedlichen x-Positionen.
plot_persistenz <- ggplot(data.frame(x = 1, y = mittelwert), aes(x = x, y = y)) +
  geom_point(color = "#4A7B9D", size = 5) +
  geom_errorbar(aes(ymin = untergrenze, ymax = obergrenze),
                width = 0.08, color = "#4A7B9D", linewidth = 1.2) +
  geom_hline(yintercept = persistenz_erwartet, color = "#B84A4A", linewidth = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray45", linewidth = 0.8) +
  annotate("label", x = 0.73, y = 0.515, label = "Zufall (50%)",
           color = "gray35", fill = "white", label.size = 0, size = 3.4) +
  annotate("label", x = 1.27, y = persistenz_erwartet - 0.015,
           label = paste0("Erwartet: ", format_prozent(persistenz_erwartet, 2)),
           color = "#B84A4A", fill = "white", label.size = 0, size = 3.4) +
  scale_x_continuous(limits = c(0.5, 1.5), breaks = NULL) +
  scale_y_continuous(
    limits = c(0.35, 0.65),
    breaks = seq(0.35, 0.65, 0.05),
    labels = function(x) paste0(round(x * 100), "%")
  ) +
  labs(
    title = paste0("Allgemeine Persistenz mit Erwartungswert (N = ", N, ")"),
    subtitle = paste0(
      "P(Fortsetzung) = ", format_prozent(mittelwert, 2),
      " | p = ", formatC(p_wert, format = "f", digits = 4, decimal.mark = ","),
      ifelse(p_wert < 0.05, " *", "")
    ),
    x = NULL,
    y = "Fortsetzungswahrscheinlichkeit"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = paste0("Persistenz_mit_Erwartungswert_N", N, ".png"),
  plot = plot_persistenz,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Grafik 2: Asymmetrie mit beobachteten und erwarteten Anteilen
n_neg <- sum(reg_data$Trend == -1)
n_pos <- sum(reg_data$Trend == 1)
se_neg <- sqrt(p_neg * (1 - p_neg) / n_neg)
se_pos <- sqrt(p_pos * (1 - p_pos) / n_pos)

asym_plot_data <- data.frame(
  x = c(0.78, 1.22, 1.78, 2.22),
  Kennzahl = c(
    "Fortsetzung negativer Trend", "P(Folge unterdurchschnittlich)",
    "Fortsetzung positiver Trend", "P(Folge überdurchschnittlich)"
  ),
  Wert = c(p_neg, p_folge_unter, p_pos, p_folge),
  Untergrenze = c(pmax(0, p_neg - 1.96 * se_neg), NA, pmax(0, p_pos - 1.96 * se_pos), NA),
  Obergrenze = c(pmin(1, p_neg + 1.96 * se_neg), NA, pmin(1, p_pos + 1.96 * se_pos), NA)
)

asym_plot_data$Kennzahl <- factor(
  asym_plot_data$Kennzahl,
  levels = c(
    "Fortsetzung negativer Trend", "P(Folge unterdurchschnittlich)",
    "Fortsetzung positiver Trend", "P(Folge überdurchschnittlich)"
  )
)

plot_asymmetrie <- ggplot(asym_plot_data, aes(x = x, y = Wert, fill = Kennzahl)) +
  geom_col(width = 0.40, color = "gray25") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray", linewidth = 0.7) +
  geom_text(aes(label = format_prozent(Wert, 2)), vjust = -0.45, size = 3.5) +
  scale_fill_manual(values = c(
    "Fortsetzung negativer Trend" = "#B84A4A",
    "P(Folge unterdurchschnittlich)" = "#D78C8C",
    "Fortsetzung positiver Trend" = "#5A8A7A",
    "P(Folge überdurchschnittlich)" = "#94B9AD"
  )) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Negativer Trend", "Positiver Trend")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = function(x) paste0(round(x * 100), "%")
  ) +
  labs(
    title = paste0("Persistenz nach Trend und Folgespielrichtung (N = ", N, ")"),
    subtitle = paste0(
      "Differenz der Fortsetzung: ", format_prozent(p_pos - p_neg, 2),
      " | p = ", formatC(beta1_p, format = "f", digits = 4, decimal.mark = ","),
      ifelse(beta1_p < 0.05, " *", "")
    ),
    x = NULL,
    y = "Anteil",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = paste0("Asymmetrie_mit_Folgerichtung_N", N, ".png"),
  plot = plot_asymmetrie,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 13. KERNERGEBNISSE 
# ============================================================

# Daten für Teilgruppen-Tests
data_neg <- reg_data$Gleiche_Richtung[reg_data$Trend == -1]
data_pos <- reg_data$Gleiche_Richtung[reg_data$Trend == 1]

# t-Tests für Teilgruppen
t_test_neg <- t.test(data_neg, mu = 0.5)
t_test_pos <- t.test(data_pos, mu = 0.5)

# Beobachtungszahlen
n_neg <- length(data_neg)
n_pos <- length(data_pos)
n_gesamt <- nrow(reg_data)

kernergebnisse <- data.frame(
  Nr = 1:26,
  Kennzahl = c(
    "P(Fortsetzung allgemein)",
    "n (allgemein)",
    "p-Wert Persistenz (t-Test)",
    "OLS Achsenabschnitt (ß₀)",
    "p-Wert Persistenz (OLS Leermodell gegen 50%)",
    "t-Wert (OLS gegen 50%)",
    "P(Schlecht → Schlecht)",
    "n (schlecht)",
    "p-Wert Persistenz (t-Test schlecht)",
    "P(Gut → Gut)",
    "n (gut)",
    "p-Wert Persistenz (t-Test gut)",
    "p-Wert Persistenz (Logit Leermodell)",
    "Odds Ratio Persistenz (Logit)",
    "Differenz (Asymmetrie)",
    "p-Wert Asymmetrie (OLS)",
    "Odds Ratio Asymmetrie (Logit)",
    "p-Wert Asymmetrie (Logit)",
    "Pearson r (Trend vs. Trefferquote)",
    "p-Wert Pearson (Trend vs. Trefferquote)",
    "P(Folge überdurchschnittlich)",
    "Persistenz erwartet (aus P(Folge))",
    "Cluster-robuster SE β₀",
    "p-Wert cluster-robust (β₀ gegen 50%)",
    "Cluster-robuster SE β₁",
    "p-Wert cluster-robust (β₁ gegen 0)"
  ),
  Wert = c(
    p_gesamt,
    n_gesamt,
    t_test_persistenz$p.value,
    beta0,
    p_gegen_50,
    t_gegen_50,
    p_neg,
    n_neg,
    t_test_neg$p.value,
    p_pos,
    n_pos,
    t_test_pos$p.value,
    logit_persistenz_p,
    logit_persistenz_OR,
    p_pos_asym - p_neg_asym,
    beta1_p,
    logit_odds_ratio,
    logit_beta1_p,
    unname(kor_trend_tq$estimate),
    kor_trend_tq$p.value,
    p_folge,
    persistenz_erwartet,
    se_beta0_cluster,
    p_beta0_cluster_gegen_50,
    se_beta1_cluster,
    p_beta1_cluster_gegen_0
  ),
  Darstellung = c(
    paste0(round(p_gesamt * 100, 2), "%"),
    paste0(n_gesamt),
    round(t_test_persistenz$p.value, 4),
    round(beta0, 4),
    round(p_gegen_50, 4),
    round(t_gegen_50, 4),
    paste0(round(p_neg * 100, 2), "%"),
    paste0(n_neg),
    round(t_test_neg$p.value, 4),
    paste0(round(p_pos * 100, 2), "%"),
    paste0(n_pos),
    round(t_test_pos$p.value, 4),
    round(logit_persistenz_p, 4),
    round(logit_persistenz_OR, 4),
    paste0(round((p_pos_asym - p_neg_asym) * 100, 2), " Prozentpunkte"),
    round(beta1_p, 4),
    round(logit_odds_ratio, 4),
    round(logit_beta1_p, 4),
    round(unname(kor_trend_tq$estimate), 4),
    round(kor_trend_tq$p.value, 4),
    paste0(round(p_folge * 100, 2), "%"),
    paste0(round(persistenz_erwartet * 100, 2), "%"),
    round(se_beta0_cluster, 4),
    round(p_beta0_cluster_gegen_50, 4),
    round(se_beta1_cluster, 4),
    round(p_beta1_cluster_gegen_0, 4)
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 14. BERICHTERSTATTUNG
# ============================================================

cat("\n\n=== DATENBEREINIGUNG UND PAUSENLOGIK ===\n")
cat("Urspruengliche Spiele ohne NAs:", sum(bereinigung$Spiele_ohne_NA), "\n")
cat("Einzelne NAs:", sum(bereinigung$Einzelne_NAs), "\n")
cat("Pausen mit mindestens 2 NAs:", sum(bereinigung$Pausen), "\n")
cat("NAs innerhalb solcher Pausen:", sum(bereinigung$NAs_in_Pausen), "\n")
cat("Werte exakt auf dem Durchschnitt:", sum(bereinigung$Durchschnittswerte), "\n")
cat("Spiele in der Regression:", nrow(reg_data), "\n")

cat("\n\n=== KERNERGEBNISSE ===\n")
print(kernergebnisse, row.names = FALSE)
cat("\n\n=== FERTIG! ===\n")
