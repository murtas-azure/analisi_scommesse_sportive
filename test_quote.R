library(dplyr)
library(ggplot2)
library(tidyr)
library(stats)

pn <- "C:/Users/danie/Desktop/analisi_scommesse_sportive-master/dati/quote_"
sy = 2015
quote_df <- read.csv(paste(pn, toString(sy), "_", toString(sy+1), ".csv", sep = ""))
for(x in (sy+1):2022) {
  df1 <- read.csv(paste(pn, toString(x), "_", toString(x+1), ".csv", sep = ""))
  quote_df <- rbind(quote_df, df1)
}

#Calcolo probabilità implicite normalizzate
quote_df <- quote_df %>%
  mutate(
    p1_raw = 1 / Odd_1,
    pX_raw = 1 / Odd_X,
    p2_raw = 1 / Odd_2,
    overround = p1_raw + pX_raw + p2_raw,
    p1_imp = p1_raw / overround,
    pX_imp = pX_raw / overround,
    p2_imp = p2_raw / overround
  )

analisi_segno <- function(df, col_prob, esito_atteso, label_col = "p_bin", breaks = seq(0.30, 0.80, 0.05)) {
  df <- df %>%
    mutate(
      !!label_col := cut(!!sym(col_prob), breaks = breaks, right = FALSE)
    )
  
  freq_vs_prob <- df %>%
    filter(!is.na(!!sym(label_col))) %>%
    group_by(!!sym(label_col)) %>%
    summarise(
      n_partite = n(),
      n_successi = sum(Outcome == esito_atteso),
      freq_empirica = n_successi / n_partite,
      p_medio = mean(!!sym(col_prob)),
      .groups = "drop"
    ) %>%
    mutate(
      z_score = (freq_empirica - p_medio) / sqrt(p_medio * (1 - p_medio) / n_partite),
      p_value = 2 * (1 - pnorm(abs(z_score)))
    )
  
  return(freq_vs_prob)
}

#Analisi per segno "1"
ris_1 <- analisi_segno(quote_df, col_prob = "p1_imp", esito_atteso = "1")

#Analisi per segno "2"
ris_2 <- analisi_segno(quote_df, col_prob = "p2_imp", esito_atteso = "2")

print(ris_1)
print(ris_2)

#Grafico per il segno "1"
ggplot(ris_1, aes(x = p_medio)) +
  geom_point(aes(y = freq_empirica), color = "blue", size = 3) +
  geom_line(aes(y = p_medio), color = "red", linetype = "dashed") +
  labs(title = "Frequenza osservata vs Probabilità implicita – Vittoria Casa",
       x = "Probabilità implicita media",
       y = "Frequenza osservata") +
  theme_minimal()

#Grafico per il segno "2"
ggplot(ris_2, aes(x = p_medio)) +
  geom_point(aes(y = freq_empirica), color = "blue", size = 3) +
  geom_line(aes(y = p_medio), color = "red", linetype = "dashed") +
  labs(title = "Frequenza osservata vs Probabilità implicita – Vittoria Trasferta",
       x = "Probabilità implicita media",
       y = "Frequenza osservata") +
  theme_minimal()

