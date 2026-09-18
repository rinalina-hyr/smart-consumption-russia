install.packages("DBI")
install.packages("RSQLite")
install.packages("dplyr")
install.packages("ggplot2")
# ============================================
# H2 (углублённая): Рейтинг факторов
# Сила влияния разных признаков на умное потребление
# ============================================

library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

# --- 1. Подключаемся к базе ---
con <- dbConnect(SQLite(),
                 "C:/Users/irin/Documents/GitHub/smart-consumption-russia/data/smart_consumption.db")

segments <- dbGetQuery(con, "SELECT * FROM chizhik_segments")
dbDisconnect(con)

# --- 2. Оставляем только известные dimensions ---
# Убираем строку-источник и пустые значения
segments <- segments %>%
  filter(!is.na(smart_consumption_share_pct),
         dimension != "Источник") %>%
  mutate(smart_consumption_share_pct = as.numeric(smart_consumption_share_pct))

print("Что в данных:")
print(table(segments$dimension))

# --- 3. Считаем effect size для каждого фактора ---
# range — максимальный разброс
# sd   — стандартное отклонение
# cv   — коэффициент вариации (нормированный разброс)
# eta2 — доля объяснённой дисперсии (через ANOVA)

effect_sizes <- segments %>%
  group_by(dimension) %>%
  summarise(
    n_groups = n(),
    min_share = min(smart_consumption_share_pct),
    max_share = max(smart_consumption_share_pct),
    range_pp  = max(smart_consumption_share_pct) - min(smart_consumption_share_pct),
    sd_pp     = sd(smart_consumption_share_pct),
    cv        = sd(smart_consumption_share_pct) / mean(smart_consumption_share_pct),
    .groups = "drop"
  ) %>%
  arrange(desc(range_pp))

print("Рейтинг факторов по разбросу (range):")
print(effect_sizes)

# --- 4. ANOVA: насколько каждая dimension объясняет разброс ---
# Модель: smart_share ~ dimension
aov_model <- aov(smart_consumption_share_pct ~ dimension, data = segments)
print("ANOVA: объясняет ли dimension разброс?")
print(summary(aov_model))

# eta-squared = SS_between / SS_total
ss <- summary(aov_model)[[1]]
eta2 <- ss[["Sum Sq"]][1] / sum(ss[["Sum Sq"]])
print(paste("Eta-squared:", round(eta2, 3)))
print("(интерпретация: >0.14 — большой эффект, 0.06–0.14 — средний, <0.06 — малый)")

# --- 5. Упрощённая классификация факторов ---
effect_sizes <- effect_sizes %>%
  mutate(
    strength = case_when(
      range_pp >= 12 ~ "Сильный",
      range_pp >= 6  ~ "Средний",
      TRUE           ~ "Слабый"
    )
  )

print("Классификация:")
print(effect_sizes %>% select(dimension, range_pp, strength))

# --- 6. График: рейтинг факторов ---
# Убираем служебные строки и упорядочиваем
effect_sizes$dimension <- factor(effect_sizes$dimension,
                                  levels = effect_sizes$dimension[order(effect_sizes$range_pp)])

ggplot(effect_sizes, aes(x = dimension, y = range_pp, fill = strength)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(range_pp, " п.п.")),
            hjust = -0.2, size = 3.8) +
  coord_flip() +
  labs(
    title = "H1: Рейтинг факторов по силе влияния на умное потребление",
    subtitle = "Разброс доли умных потребителей между группами внутри каждого фактора",
    x = NULL,
    y = "Разброс доли умных, п.п.",
    fill = "Сила влияния"
  ) +
  scale_fill_manual(values = c("Сильный" = "#E63946",
                                "Средний" = "#F4A261",
                                "Слабый"  = "#A8DADC")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave("C:/Users/irin/Documents/GitHub/smart-consumption-russia/visualizations/H1_factor_ranking.png",
       width = 11, height = 7, dpi = 150)

print("График сохранён в visualizations/H1_factor_ranking.png")