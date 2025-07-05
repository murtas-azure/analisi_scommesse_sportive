pn <- "C:\\Users\\danie\\Desktop\\analisi_scommesse_sportive-master\\dati\\quote.csv"

# Caricamento pacchetti
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Lettura del file
dati <- read_csv(pn)

# Calcolo delle quote vincenti e perdenti
dati <- dati %>%
  mutate(
    Quota_vincente = case_when(
      Outcome == "1" ~ Odd_1,
      Outcome == "X" ~ Odd_X,
      Outcome == "2" ~ Odd_2
    ),
    Quota_perdente1 = case_when(
      Outcome == "1" ~ pmax(Odd_X, Odd_2),
      Outcome == "X" ~ pmax(Odd_1, Odd_2),
      Outcome == "2" ~ pmax(Odd_1, Odd_X)
    ),
    Quota_perdente2 = case_when(
      Outcome == "1" ~ pmin(Odd_X, Odd_2),
      Outcome == "X" ~ pmin(Odd_1, Odd_2),
      Outcome == "2" ~ pmin(Odd_1, Odd_X)
    )
  )

quote_perdenti <- c(dati$Quota_perdente1, dati$Quota_perdente2)
quote_perdenti <- quote_perdenti[!is.na(quote_perdenti)]

statistiche <- summary(quote_perdenti)
media <- mean(quote_perdenti)

boxplot(quote_perdenti, dati$Quota_vincente,
        main = "Distribuzione delle quote",
        ylab = "Quota",
        names = c("Perdente", "Vincente"))

info <- sprintf("Perdente: Min: %.2f | 1°Q: %.2f | Mediana: %.2f | Media: %.2f | 3°Q: %.2f | Max: %.2f",
                statistiche["Min."],
                statistiche["1st Qu."],
                statistiche["Median"],
                media,
                statistiche["3rd Qu."],
                statistiche["Max."])
mtext(info, side = 1, line = 2, cex = 0.8)

statistiche <- summary(dati$Quota_vincente)
media <- mean(dati$Quota_vincente, na.rm = TRUE)

info <- sprintf("Vincente: Min: %.2f | 1°Q: %.2f | Mediana: %.2f | Media: %.2f | 3°Q: %.2f | Max: %.2f",
                statistiche["Min."],
                statistiche["1st Qu."],
                statistiche["Median"],
                media,
                statistiche["3rd Qu."],
                statistiche["Max."])
mtext(info, side = 1, line = 3, cex = 0.8)

# Istogramma Quote Vincenti
ggplot(dati, aes(x = Quota_vincente)) +
  geom_histogram(bins = 30, color = "black", fill = "white") +
  coord_cartesian(xlim = c(1, NA)) +
  labs(title = "Istogramma - Quote Vincenti", x = "Quota Vincente", y = "Frequenza") +
  theme_minimal()

# Istogramma Quote Perdenti
df_quote <- data.frame(Quota_perdente = quote_perdenti)
ggplot(df_quote, aes(x = Quota_perdente)) +
  geom_histogram(bins = 30, color = "black", fill = "white") +
  coord_cartesian(xlim = c(1, NA)) +
  labs(title = "Istogramma - Quote Perdenti", 
       x = "Quota Perdente", y = "Frequenza") +
  theme_minimal()

# === QQ-PLOT QUOTE VINCENTI ===
quote_vincenti <- dati$Quota_vincente[!is.na(dati$Quota_vincente)]
n_vincenti <- length(quote_vincenti)

if (n_vincenti >= 3) {
  if (n_vincenti > 5000) {
    set.seed(123)
    quote_vincenti_test <- sample(quote_vincenti, 5000)
  } else {
    quote_vincenti_test <- quote_vincenti
  }
  
  shapiro_test_v <- shapiro.test(quote_vincenti_test)
  
  qqnorm(quote_vincenti_test, main = "QQ-plot delle Quote Vincenti")
  qqline(quote_vincenti_test, col = "red", lwd = 2)
  mtext(sprintf("Shapiro-Wilk p-value: %.4g", shapiro_test_v$p.value), side = 3, line = 0.5, cex = 0.9)
} else {
  message("Quote vincenti: troppo pochi dati per il test di Shapiro-Wilk")
}

# === QQ-PLOT QUOTE PERDENTI ===
n_perdenti <- length(quote_perdenti)

if (n_perdenti >= 3) {
  if (n_perdenti > 5000) {
    set.seed(123)
    quote_perdenti_test <- sample(quote_perdenti, 5000)
  } else {
    quote_perdenti_test <- quote_perdenti
  }
  
  shapiro_test_p <- shapiro.test(quote_perdenti_test)
  
  qqnorm(quote_perdenti_test, main = "QQ-plot delle Quote Perdenti")
  qqline(quote_perdenti_test, col = "red", lwd = 2)
  mtext(sprintf("Shapiro-Wilk p-value: %.4g", shapiro_test_p$p.value), side = 3, line = 0.5, cex = 0.9)
} else {
  message("Quote perdenti: troppo pochi dati per il test di Shapiro-Wilk")
}
