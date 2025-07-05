library(readr)
library(dplyr)

# Caricamento del file
pn <- "C:\\Users\\danie\\Desktop\\analisi_scommesse_sportive-master\\dati\\quote.csv"
dati <- read_csv(pn)

# Calcolo quota vincente e quote perdenti
dati <- dati %>%
  mutate(
    Quota_vincente = case_when(
      Outcome == "1" ~ Odd_1,
      Outcome == "X" ~ Odd_X,
      Outcome == "2" ~ Odd_2,
      TRUE ~ NA_real_
    ),
    Quota_perdente_1 = case_when(
      Outcome == "1" ~ Odd_X,
      Outcome == "X" ~ Odd_1,
      Outcome == "2" ~ Odd_1,
      TRUE ~ NA_real_
    ),
    Quota_perdente_2 = case_when(
      Outcome == "1" ~ Odd_2,
      Outcome == "X" ~ Odd_2,
      Outcome == "2" ~ Odd_X,
      TRUE ~ NA_real_
    )
  )

quote_perdenti <- c(dati$Quota_perdente_1, dati$Quota_perdente_2)
quote_perdenti <- quote_perdenti[!is.na(quote_perdenti)]

calcola_ic_95 <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  media <- mean(x)
  sd_val <- sd(x)
  errore_standard <- sd_val / sqrt(n)
  t_critico <- qt(0.975, df = n - 1)
  margine <- t_critico * errore_standard
  list(
    media = media,
    lower = media - margine,
    upper = media + margine,
    n = n,
    errore_standard = errore_standard
  )
}

# Calcolo intervalli di confidenza
ic_vincenti <- calcola_ic_95(dati$Quota_vincente)
ic_perdenti <- calcola_ic_95(quote_perdenti)

cat(sprintf("Intervallo di confidenza al 95%% per la media delle quote vincenti: [%.3f , %.3f]\nMedia: %.3f | Errore standard: %.3f\n\n",
            ic_vincenti$lower, ic_vincenti$upper, ic_vincenti$media, ic_vincenti$errore_standard))

cat(sprintf("Intervallo di confidenza al 95%% per la media delle quote perdenti: [%.3f , %.3f]\nMedia: %.3f | Errore standard: %.3f\n",
            ic_perdenti$lower, ic_perdenti$upper, ic_perdenti$media, ic_perdenti$errore_standard))

