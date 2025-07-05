dir <- "C:\\Users\\danie\\Desktop\\analisi_scommesse_sportive-master\\dati\\"
set.seed(123)
library(nnet)
library(ggplot2)
rose_df <- read.csv(paste(dir, "rose.csv",sep = ""))
quote_df <- read.csv(paste(dir, "quote.csv", sep = ""))
library(dplyr)
quote_df <- quote_df %>%
  left_join(rose_df, by = c("Home" = "squadra", "anno" = "anno")) %>%
  rename(valore_home = valore_rosa) %>%
  left_join(rose_df, by = c("Away" = "squadra", "anno" = "anno")) %>%
  rename(valore_away = valore_rosa)

quote_df <- quote_df[sample(nrow(quote_df)), ]
rownames(quote_df) <- NULL
train_index <- sample(seq_len(nrow(quote_df)), size = 0.7 * nrow(quote_df))

quote_df <- quote_df %>%
  mutate(diff_valore = (valore_home - valore_away)/(valore_home+valore_away))

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

quote_df$Outcome <- factor(quote_df$Outcome, levels = c("1", "X", "2"))


library(dplyr)
library(zoo)

# 1. Crea 'giornata' in ordine crescente
quote_df <- quote_df %>%
  group_by(anno) %>%
  mutate(giornata = rev(seq_along(anno))) %>%
  ungroup()

# 2. Calcola forma_home
forma_home_df <- quote_df %>%
  arrange(anno, giornata) %>%
  group_by(Home, anno) %>%
  mutate(
    punti_home = case_when(
      Outcome == "1" ~ 3,
      Outcome == "X" ~ 1,
      TRUE ~ 0
    ),
    forma_home = rollapply(punti_home, width = 5, FUN = mean, fill = NA, align = "right")
  ) %>%
  select(Home, anno, giornata, forma_home)

# 3. Calcola forma_away
forma_away_df <- quote_df %>%
  arrange(anno, giornata) %>%
  group_by(Away, anno) %>%
  mutate(
    punti_away = case_when(
      Outcome == "2" ~ 3,
      Outcome == "X" ~ 1,
      TRUE ~ 0
    ),
    forma_away = rollapply(punti_away, width = 5, FUN = mean, fill = NA, align = "right")
  ) %>%
  select(Away, anno, giornata, forma_away)

quote_df <- quote_df %>%
  left_join(forma_home_df, by = c("Home", "anno", "giornata")) %>%
  left_join(forma_away_df, by = c("Away", "anno", "giornata"))

quote_df <- quote_df %>%
  mutate(diff_forma = forma_home - forma_away)


train_df <- quote_df[train_index, ]
test_df <- quote_df[-train_index, ]

soglia_quota<-100

train_df_clean <- train_df %>%
  filter(!is.na(diff_forma), !is.na(diff_valore))

train_df_low <- train_df_clean %>%
  filter(Odd_1 < soglia_quota)

test_df_low <- test_df %>%
  filter(Odd_1 < soglia_quota) %>%
  filter(!is.na(diff_forma), !is.na(diff_valore))

mod_log <- lm(log(Odd_1) ~ diff_valore * forma_away, data = train_df_low)

# Previsioni e residui sullo stesso set
train_df_low$pred <- predict(mod_log)
train_df_low$resid <- residuals(mod_log)

# Scatterplot residui vs predetti
ggplot(train_df_low, aes(pred, resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residui vs Valori Predetti", x = "Predetti", y = "Residui") +
  theme_minimal()

# Calcolo del test di Shapiro-Wilk
shapiro_test <- shapiro.test(train_df_low$resid)
pval <- shapiro_test$p.value

# Q–Q plot

ggplot(train_df_low, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(
    title = paste0("Q–Q Plot dei Residui (p = ", signif(pval, 4), ")"),
    x = "Quantili teorici (Normale)",
    y = "Quantili dei residui"
  ) +
  theme_minimal()

#predizione
valore_home <- 575.7
valore_away <- 438.2
forma_away_val <- mean(c(1, 3, 1, 3, 1))

diff_valore_val <- (valore_home - valore_away)/(valore_home + valore_away)

nuovo_match <- data.frame(
  diff_valore = diff_valore_val,
  forma_away = forma_away_val
)

log_quota_pred <- predict(mod_log, newdata = nuovo_match)
quota_pred <- exp(log_quota_pred)
print(quota_pred)


pred_pred <- predict(mod_log, newdata = nuovo_match, interval = "prediction", level = 0.95)
quota_pred <- exp(pred_pred)

cat(sprintf(
  "Quota stimata Juventus vs Milan: %.3f\nIntervallo di predizione 95%%: [%.3f, %.3f]",
  quota_pred[1], quota_pred[2], quota_pred[3]
))

