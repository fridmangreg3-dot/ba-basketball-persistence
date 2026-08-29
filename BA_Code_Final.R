#####Aus Spaß weil ich Bock und Zeit hab#####

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
excel_datei <- "/Users/georg/Library/Mobile Documents/com~apple~CloudDocs/Uni/Semester 6/BA/BA/BA_Datensatz_1.xlsx"
min_spiele <- 60  #55, 65
N <- 1           # 3, 5, 7, 9
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
# 5. ERWARTUNGSWERTE
# ============================================================
# p_folge ist die Wahrscheinlichkeit eines unterdurchschnittlichen
# Folgespiels. q ist der Anteil negativer Serien in der aktuellen Analyse.
p_folge <- mean(reg_data$Richtung_Aktuelles_Spiel == -1, na.rm = TRUE)
p_unter <- p_folge
q <- mean(reg_data$Trend == -1, na.rm = TRUE)

# Erwartete Persistenz unter Berücksichtigung des Anteils negativer Serien.
p_erwartet <- p_folge * q + (1 - p_folge) * (1 - q)
persistenz_erwartet <- p_erwartet
asymmetrie_erwartet <- 2 * (0.5 - p_unter)

# Für die Regression ist β0 die mittlere Persistenz und β1 die Differenz
# zwischen positivem und negativem Trend. Die Kodierung erhält diese direkt.
reg_data <- reg_data %>%
  mutate(Trend_asymmetrie = (Trend - mean(Trend, na.rm = TRUE)) / 2)

# ============================================================
# 6. ASYMMETRIE-REGRESSION (OLS) UND CLUSTER-ROBUSTE TESTS
# ============================================================
cat("\n\n=== ASYMMETRIE-REGRESSION (OLS) ===\n")
model_asym <- lm(Gleiche_Richtung ~ Trend_asymmetrie, data = reg_data)
vcov_asym_cluster <- sandwich::vcovCL(
  model_asym, cluster = ~Player, type = "HC1"
)
se_cluster <- sqrt(diag(vcov_asym_cluster))
beta0 <- coef(model_asym)[1]
beta1 <- coef(model_asym)[2]
se_beta0_cluster <- se_cluster[1]
se_beta1_cluster <- se_cluster[2]
anzahl_cluster <- dplyr::n_distinct(reg_data$Player)
df_cluster <- anzahl_cluster - 1

t_beta0_gegen_persistenz_erwartet <-
  (beta0 - persistenz_erwartet) / se_beta0_cluster
p_beta0_gegen_persistenz_erwartet <- 2 * pt(
  -abs(t_beta0_gegen_persistenz_erwartet), df = df_cluster
)
t_beta1_gegen_asymmetrie_erwartet <-
  (beta1 - asymmetrie_erwartet) / se_beta1_cluster
p_beta1_gegen_asymmetrie_erwartet <- 2 * pt(
  -abs(t_beta1_gegen_asymmetrie_erwartet), df = df_cluster
)

p_neg <- mean(reg_data$Gleiche_Richtung[reg_data$Trend == -1], na.rm = TRUE)
p_pos <- mean(reg_data$Gleiche_Richtung[reg_data$Trend == 1], na.rm = TRUE)
p_gesamt <- mean(reg_data$Gleiche_Richtung, na.rm = TRUE)

# ============================================================
# 7. LOGIT-REGRESSION DER ASYMMETRIE
# ============================================================
cat("\n\n=== LOGIT-REGRESSION (ASYMMETRIE) ===\n")
logit_model <- glm(
  Gleiche_Richtung ~ Trend_asymmetrie,
  data = reg_data,
  family = binomial()
)
logit_beta0 <- coef(logit_model)[1]
logit_beta1 <- coef(logit_model)[2]

# ============================================================
# 8. PUNKT-BISERIALE KORRELATION (TREND vs. TREFFERQUOTE)
# ============================================================
kor_trend_tq <- cor.test(reg_data$Trend, reg_data$Aktuelle_Tq, method = "pearson")


# ============================================================
# 12. VISUALISIERUNGEN
# ============================================================

format_prozent <- function(x, digits = 1) {
  paste0(formatC(x * 100, format = "f", digits = digits, decimal.mark = ","), "%")
}

# Verteilung der Richtung im Folgespiel. Die Erwartungswerte wurden bereits
# vor den Regressionen berechnet und werden hier nur für die Grafik verwendet.
n_folge_gesamt <- nrow(reg_data)
n_folge_ueber <- sum(reg_data$Richtung_Aktuelles_Spiel == 1, na.rm = TRUE)
n_folge_unter <- sum(reg_data$Richtung_Aktuelles_Spiel == -1, na.rm = TRUE)
p_folge_unter <- p_folge
p_folge_ueber <- 1 - p_folge


# Kennzahlen für die allgemeine Persistenz
mittelwert <- mean(reg_data$Gleiche_Richtung, na.rm = TRUE)
n <- nrow(reg_data)
se <- sqrt(mittelwert * (1 - mittelwert) / n)
untergrenze <- mittelwert - 1.96 * se
obergrenze <- mittelwert + 1.96 * se
p_wert <- p_beta0_gegen_persistenz_erwartet

# Grafik 1: Allgemeine Persistenz
# Die Referenzbeschriftungen liegen bewusst auf unterschiedlichen x-Positionen.
plot_persistenz <- ggplot(data.frame(x = 1, y = mittelwert), aes(x = x, y = y)) +
  geom_point(color = "#4A7B9D", size = 5) +
  geom_errorbar(aes(ymin = untergrenze, ymax = obergrenze),
                width = 0.08, color = "#4A7B9D", linewidth = 1.2) +
  geom_hline(yintercept = persistenz_erwartet, linetype = "dashed",
             color = "gray45", linewidth = 0.8) +
  annotate("label", x = 1.27, y = persistenz_erwartet - 0.015,
           label = paste0("Erwartet: ", format_prozent(persistenz_erwartet, 2)),
           color = "gray35", fill = "white", label.size = 0, size = 3.4) +
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
  Wert = c(p_neg, p_folge_unter, p_pos, p_folge_ueber),
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
      " | p = ", formatC(p_beta1_gegen_asymmetrie_erwartet, format = "f", digits = 4, decimal.mark = ","),
      ifelse(p_beta1_gegen_asymmetrie_erwartet < 0.05, " *", "")
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
# 10. KERNERGEBNISSE
# ============================================================
n_gesamt <- nrow(reg_data)
ausgeschlossene_beobachtungen <- sum(bereinigung$Spiele_ohne_NA) - n_gesamt

kernergebnisse <- data.frame(
  Kennzahl = c(
    "Zahl Beobachtungen", "Ausgeschlossene Beobachtungen", "p_unter",
    "Anteil_Negativer_Serien",
    "p_erwartet", "persistenz_erwartet", "asymmetrie_erwartet",
    "β0 (OLS)", "SE β0 (cluster-robust)",
    "β0 t-Test gegen persistenz_erwartet (t)",
    "β0 t-Test gegen persistenz_erwartet (p)",
    "β1 (OLS)", "SE β1 (cluster-robust)",
    "β1 t-Test gegen asymmetrie_erwartet (t)",
    "β1 t-Test gegen asymmetrie_erwartet (p)",
    "β0 (Logit)", "β1 (Logit)", "Korrelation", "p-Wert Korrelation"
  ),
  Wert = c(
    n_gesamt, ausgeschlossene_beobachtungen, p_unter, q, p_erwartet,
    persistenz_erwartet, asymmetrie_erwartet, beta0, se_beta0_cluster,
    t_beta0_gegen_persistenz_erwartet, p_beta0_gegen_persistenz_erwartet,
    beta1, se_beta1_cluster, t_beta1_gegen_asymmetrie_erwartet,
    p_beta1_gegen_asymmetrie_erwartet, logit_beta0, logit_beta1,
    unname(kor_trend_tq$estimate), kor_trend_tq$p.value
  ),
  stringsAsFactors = FALSE
)

cat("\n\n=== KERNERGEBNISSE ===\n")
kernergebnisse$Darstellung <- ifelse(
  kernergebnisse$Kennzahl %in% c("Zahl Beobachtungen", "Ausgeschlossene Beobachtungen"),
  formatC(kernergebnisse$Wert, format = "f", digits = 0),
  formatC(kernergebnisse$Wert, format = "f", digits = 6, decimal.mark = ",")
)
print(kernergebnisse[c("Kennzahl", "Darstellung")], row.names = FALSE)
cat("\n\n=== FERTIG! ===\n")
