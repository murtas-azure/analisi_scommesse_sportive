library(readr)
library(dplyr)

# Caricamento del CSV
dati <- read_csv("C:/Users/danie/Desktop/analisi_scommesse_sportive-master/dati/quote.csv")

dati <- dati %>%
  mutate(anno = as.numeric(anno)) %>%
  filter(!is.na(anno), Odd_1 > 1, Odd_X > 1, Odd_2 > 1)

# Definizione periodo COVID
dati <- dati %>%
  mutate(periodo = ifelse(anno %in% c(2020, 2021), "covid", "noncovid"))

# Funzione per test Z
test_z <- function(colonna) {
  covid_vals <- dati %>% filter(periodo == "covid") %>% pull({{colonna}})
  noncovid_vals <- dati %>% filter(periodo == "noncovid") %>% pull({{colonna}})
  
  m <- length(covid_vals)
  n <- length(noncovid_vals)
  mean_covid <- mean(covid_vals)
  mean_noncovid <- mean(noncovid_vals)
  var_covid <- var(covid_vals)
  var_noncovid <- var(noncovid_vals)
  
  errore_standard <- sqrt(var_covid / m + var_noncovid / n)
  Z0 <- (mean_covid - mean_noncovid) / errore_standard
  p_value <- 2 * (1 - pnorm(abs(Z0)))
  
  ci_lower <- (mean_covid - mean_noncovid) - qnorm(0.975) * errore_standard
  ci_upper <- (mean_covid - mean_noncovid) + qnorm(0.975) * errore_standard
  
  cat(sprintf("\n--- Test Z su %s ---\n", deparse(substitute(colonna))))
  cat(sprintf("Media COVID = %.3f | Media non-COVID = %.3f\n", mean_covid, mean_noncovid))
  cat(sprintf("Varianza COVID = %.3f | Varianza non-COVID = %.3f\n", var_covid, var_noncovid))
  cat(sprintf("Z0 = %.4f | p-value = %.5f\n", Z0, p_value))
  cat(sprintf("IC al 95%%: [%.3f , %.3f]\n", ci_lower, ci_upper))
  cat(sprintf("Decisione (α = 0.05): %s\n", ifelse(p_value < 0.05, "Differenza significativa", "Nessuna evidenza di differenza")))
}

# Esecuzione dei tre test
test_z(Odd_1)  # Vittoria casa
test_z(Odd_X)  # Pareggio
test_z(Odd_2)  # Vittoria trasferta
