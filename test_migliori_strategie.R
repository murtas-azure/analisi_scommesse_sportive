library(dplyr)
library(readr)

# Caricamento dati
pn <- "C:\\Users\\danie\\Desktop\\analisi_scommesse_sportive-master\\dati\\quote.csv"
dati <- read_csv(pn)

# Calcolo probabilità implicite e bin per strategia 1
dati <- dati %>%
  mutate(
    p2_raw = 1 / Odd_2,
    overround = (1 / Odd_1) + (1 / Odd_X) + p2_raw,
    p2_imp = p2_raw / overround,
    p_bin = cut(p2_imp, breaks = seq(0.3, 0.8, 0.05), right = FALSE)
  )

bin_valore <- c("[0.45,0.5)", "[0.75,0.8)")

# Strategie: profitto per scommessa
strat1 <- dati %>%
  filter(p_bin %in% bin_valore) %>%
  mutate(
    bet_success = ifelse(Outcome == "2", 1, 0),
    profitto = ifelse(bet_success == 1, Odd_2 - 1, -1)
  ) %>%
  select(profitto)

strat2 <- dati %>%
  filter(Home == "Juventus" | Away == "Juventus") %>%
  mutate(
    segno_juve = ifelse(Home == "Juventus", "1", "2"),
    quota_juve = ifelse(segno_juve == "1", Odd_1, Odd_2),
    bet_success = ifelse(Outcome == segno_juve, 1, 0),
    profitto = ifelse(bet_success == 1, quota_juve - 1, -1)
  ) %>%
  select(profitto)

# Calcolo statistiche campionarie
m <- nrow(strat1)
n <- nrow(strat2)
mean_x <- mean(strat1$profitto)
mean_y <- mean(strat2$profitto)
var_x <- var(strat1$profitto)
var_y <- var(strat2$profitto)

# Statistica Z
delta0 <- 0 
numeratore <- mean_x - mean_y - delta0
denominatore <- sqrt(var_x / m + var_y / n)
Z0 <- numeratore / denominatore

p_value <- 1 - pnorm(Z0)

alpha <- 0.05
decision <- ifelse(p_value < alpha, "Rifiuta H0: strategia 1 migliore", "Non rifiuta H0")

# Output risultati
cat(sprintf(
  "=== Test Z su differenza di medie (campioni numerosi) ===\nMedia strat1: %.4f | Media strat2: %.4f\nVarianza strat1: %.4f | Varianza strat2: %.4f\nN strat1: %d | N strat2: %d\nZ0: %.4f | p-value: %.5f\nDecisione (α = 0.05): %s\n",
  mean_x, mean_y,
  var_x, var_y,
  m, n,
  Z0, p_value,
  decision
))
