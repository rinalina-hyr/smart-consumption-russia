# ============================================
# H3: прирост доли умных в группе 45-54 отстаёт
# от соседних возрастных групп (2024 → 2025)
# ============================================
#
# ОБОСНОВАНИЕ ВЫБОРА СОСЕДЕЙ
#
# В данных Chizhik_ВШЭ_Сегменты есть ровно четыре
# возрастные группы: 25-34, 45-54, 55-64, 65+.
# Других нет — ни 18-24, ни 35-44.
# Группа 65+ — пенсионеры, другая жизненная фаза,
# некорректный "сосед" по трудовому возрасту.
# Остаются три группы трудоспособного возраста:
# 25-34, 45-54, 55-64. Мы проверяем, отстаёт ли
# средняя группа (45-54) от двух крайних.
# Это не подгонка — это следствие структуры данных.
#
# ============================================
pacman::p_load(DBI, RSQLite, dplyr, tidyr, here)

con <- dbConnect(SQLite(), here("data", "smart_consumption.db"))

segs  <- dbGetQuery(con, "SELECT * FROM chizhik_segments")
demog <- dbGetQuery(con, "SELECT * FROM demographics")

dbDisconnect(con)

# ============================================
# Демографическая справка (для прозрачности оценок n)
# ============================================

cat("Демографическая структура (Росстат/Ромир, 2026):\n")
print(demog)
cat("\nЭти группы НЕ совпадают с группами H3 (25-34, 45-54, 55-64),\n")
cat("поэтому точное n для H3 из них не выводится.\n")
cat("Размеры подгрупп ниже — оценка, а не расчёт.\n\n")

# ============================================
# Подготовка данных
# ============================================

age <- segs %>%
  filter(dimension == "Возраст",
         group %in% c("25-34", "45-54", "55-64"),
         period %in% c("2024", "2025")) %>%
  mutate(share = as.numeric(smart_consumption_share_pct)) %>%
  select(group, period, share) %>%
  pivot_wider(names_from = period, values_from = share) %>%
  mutate(
    delta = `2025` - `2024`,
    group = factor(group, levels = c("25-34", "45-54", "55-64"))
  )

cat("Приросты 2024 → 2025:\n")
print(age %>% select(group, `2024`, `2025`, delta))
cat("\n")

# ============================================
# Наблюдаемые доли
# ============================================

d <- function(g, yr) age[[as.character(yr)]][age$group == g]

p24_54 <- d("45-54", 2024); p25_54 <- d("45-54", 2025)
p24_25 <- d("25-34", 2024); p25_25 <- d("25-34", 2025)
p24_55 <- d("55-64", 2024); p25_55 <- d("55-64", 2025)

delta_54_obs <- p25_54 - p24_54
delta_nb_obs <- mean(c(p25_25 - p24_25, p25_55 - p24_55))
diff_obs     <- delta_nb_obs - delta_54_obs

cat(sprintf("45-54: прирост %+.1f п.п.\n", delta_54_obs))
cat(sprintf("Соседи (среднее): прирост %+.1f п.п.\n", delta_nb_obs))
cat(sprintf("Разница: %.1f п.п.\n\n", diff_obs))

# ============================================
# Bootstrap
# ============================================
# Допущение: волны 2024 и 2025 — независимые кросс-секционные
# выборки (не панель). Симулируем каждую долю из биномиального
# распределения с наблюдаемым p и оценкой n. Это допущение
# не проверяется по данным и явно вынесено в ограничения.

set.seed(42)

bootstrap_diff <- function(p24_54, p25_54,
                           p24_25, p25_25,
                           p24_55, p25_55,
                           n_54, n_25, n_55,
                           n_boot = 5000) {
  diffs <- replicate(n_boot, {
    d54 <- (rbinom(1, n_54, p25_54/100) - rbinom(1, n_54, p24_54/100)) / n_54 * 100
    d25 <- (rbinom(1, n_25, p25_25/100) - rbinom(1, n_25, p24_25/100)) / n_25 * 100
    d55 <- (rbinom(1, n_55, p25_55/100) - rbinom(1, n_55, p24_55/100)) / n_55 * 100
    mean(c(d25, d55)) - d54
  })
  q <- quantile(diffs, c(0.025, 0.5, 0.975))
  p_val <- 2 * min(mean(diffs <= 0), mean(diffs >= 0))
  list(
    ci_lower = unname(q[1]),
    median   = unname(q[2]),
    ci_upper = unname(q[3]),
    p_value  = min(p_val, 1)
  )
}

# ============================================
# Три сценария размеров подгрупп
# ============================================
# Точных n для групп 25-34, 45-54, 55-64 в источниках нет.
# Демография_Сегменты даёт только укрупнённые группы
# (до 35 / 35-54 / 55+), которые не совпадают с H3.
# Поэтому это sensitivity analysis с диапазоном:
#
# 1) "Равные (n≈2000)" — верхняя граница. Если бы при
#    выборке Chizhik ~6000 респондентов все три группы
#    были представлены равномерно.
# 2) "Пропорционально (n≈1140)" — средняя оценка.
#    Пропорциональное населению представительство
#    (при условии недоучёта молодёжи в 1.5-2 раза).
# 3) "Консервативный (n=500)" — нижняя граница.
#    Молодёжь в опросах часто недопредставлена.
#    При меньшем n мощность теста ниже, значимость
#    получить сложнее — это строгий, не мягкий сценарий.

scenarios <- list(
  "Равные (n≈2000)"           = list(n_54 = 2000, n_25 = 2000, n_55 = 2000),
  "Пропорционально (n≈1140)"  = list(n_54 = 1140, n_25 = 1200, n_55 = 1080),
  "Консервативный (n=500)"    = list(n_54 =  500, n_25 =  500, n_55 =  500)
)

results <- lapply(scenarios, function(s) {
  bootstrap_diff(p24_54, p25_54, p24_25, p25_25, p24_55, p25_55,
                 s$n_54, s$n_25, s$n_55)
})

# ============================================
# Таблица результатов (все три — без отбора)
# ============================================

tab <- data.frame(
  Сценарий = names(scenarios),
  Diff     = round(sapply(results, `[[`, "median"), 2),
  CI_lower = round(sapply(results, `[[`, "ci_lower"), 2),
  CI_upper = round(sapply(results, `[[`, "ci_upper"), 2),
  p_value  = round(sapply(results, `[[`, "p_value"), 3)
)

cat("\nBootstrap — три сценария:\n")
print(tab, row.names = FALSE)
cat("\nВсе три p-value фиксируются как есть.\n")

# ============================================
# Итог
# ============================================

cat("\n--- Итог H3 ---\n\n")
cat(sprintf("Наблюдаемая разница приростов: %.1f п.п.\n\n", diff_obs))

for (i in seq_len(nrow(tab))) {
  st <- if (tab$p_value[i] < 0.05) "значимо"
  else if (tab$p_value[i] < 0.10) "на грани (p<0.10)"
  else "НЕ значимо"
  cat(sprintf("  %-28s p = %.3f → %s\n", tab$Сценарий[i], tab$p_value[i], st))
}

# ============================================
# H3: визуализации
# ============================================
# Четыре графика по возрастным группам:
#   1. H3_delta.png   — прирост доли умных 2024 → 2025
#   2. H3_ipu.png     — ИПУ Ромир по возрастам, Q1 2026
#   3. H3_levels.png  — доли умных 2024 vs 2025
#   4. H3_slope.png   — динамика доли умных 2024 → 2025
#
# Данные — те же, что в H3.R:
#   chizhik_segments (Chizhik + ВШЭ, 2024/2025)
#   consumer_confidence (Ромир, ИПУ Q1 2026)
# ============================================

pacman::p_load(DBI, RSQLite, dplyr, tidyr, ggplot2, here, forcats)

con <- dbConnect(SQLite(), here("data", "smart_consumption.db"))

segs <- dbGetQuery(con, "SELECT * FROM chizhik_segments")
ipu  <- dbGetQuery(con, "SELECT * FROM consumer_confidence")

dbDisconnect(con)

# ============================================
# Подготовка данных
# ============================================

age <- segs %>%
  filter(dimension == "Возраст",
         group %in% c("25-34", "45-54", "55-64"),
         period %in% c("2024", "2025")) %>%
  mutate(share = as.numeric(smart_consumption_share_pct)) %>%
  select(group, period, share) %>%
  pivot_wider(names_from = period, values_from = share) %>%
  mutate(
    delta = `2025` - `2024`,
    group = factor(group, levels = c("25-34", "45-54", "55-64")),
    highlight = ifelse(group == "45-54", "Отстающая группа", "Остальные")
  )

ipu_age <- ipu %>%
  filter(dimension_type == "age_group",
         period == "Q1 2026") %>%
  mutate(
    group = factor(dimension_value,
                   levels = c("18-24", "25-34", "35-44", "45-59", "60+")),
    index_value = as.numeric(index_value),
    highlight = ifelse(group == "45-59", "Минимум", "Остальные")
  )

cat("Данные для H3_delta:\n")
print(age %>% select(group, `2024`, `2025`, delta))
cat("\nДанные для H3_ipu:\n")
print(ipu_age %>% select(group, index_value))
cat("\n")

# ============================================
# 1. H3_delta.png — прирост доли умных
# ============================================

g1 <- ggplot(age, aes(group, delta, fill = highlight)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = sprintf("%+.1f", delta)),
            vjust = -0.6, size = 4.5) +
  scale_fill_manual(values = c(
    "Отстающая группа" = "#C0392B",
    "Остальные"        = "#2E86AB"
  )) +
  scale_y_continuous(limits = c(0, 9),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Прирост доли умных потребителей, 2024 → 2025",
    subtitle = "Группа 45–54 отстаёт от соседей почти в 3 раза",
    x = NULL, y = "Прирост, п.п.", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "#7F8C8D", size = 10)
  )

ggsave(here("visualizations", "H3_delta.png"), g1,
       width = 8, height = 4.5, dpi = 150)

# ============================================
# 2. H3_ipu.png — ИПУ по возрастам
# ============================================

g2 <- ggplot(ipu_age, aes(group, index_value, fill = highlight)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%d", index_value)),
            vjust = ifelse(ipu_age$index_value >= 0, -0.6, 1.4),
            size = 4.5) +
  scale_fill_manual(values = c(
    "Минимум"    = "#C0392B",
    "Остальные"  = "#2E86AB"
  )) +
  scale_y_continuous(limits = c(-12, 6)) +
  labs(
    title = "Индекс потребительской уверенности по возрастам, Q1 2026",
    subtitle = "Источник: Ромир. Красным выделен возрастной минимум",
    x = NULL, y = "Индекс", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "#7F8C8D", size = 10)
  )

ggsave(here("visualizations", "H3_ipu.png"), g2,
       width = 9, height = 4.5, dpi = 150)

# ============================================
# 3. H3_levels.png — доли 2024 vs 2025
# ============================================

age_long <- age %>%
  select(group, `2024`, `2025`) %>%
  pivot_longer(c(`2024`, `2025`),
               names_to = "year", values_to = "share") %>%
  mutate(group = factor(group, levels = c("25-34", "45-54", "55-64")))

g3 <- ggplot(age_long, aes(group, share, fill = year)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", share)),
            position = position_dodge(0.75),
            vjust = -0.6, size = 4) +
  scale_fill_manual(values = c(
    "2024" = "#A8DADC",
    "2025" = "#2E86AB"
  )) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(
    title = "Доля умных потребителей по возрастам",
    subtitle = "Источник: НИУ ВШЭ + Чижик (X5), 2024–2025",
    x = NULL, y = "%", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "#7F8C8D", size = 10)
  )

ggsave(here("visualizations", "H3_levels.png"), g3,
       width = 8, height = 4.5, dpi = 150)

# ============================================
# 4. H3_slope.png — динамика 2024 → 2025
# ============================================

slope_long <- age %>%
  select(group, `2024`, `2025`) %>%
  pivot_longer(c(`2024`, `2025`),
               names_to = "year", values_to = "share") %>%
  mutate(
    year_num = ifelse(year == "2024", 1, 2),
    group = factor(group, levels = c("25-34", "45-54", "55-64"))
  )

g4 <- ggplot(slope_long, aes(year_num, share, color = group, group = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 4) +
  geom_text(
    data = slope_long %>% filter(year_num == 2),
    aes(label = sprintf("%s\n(%.1f%%)", group, share)),
    hjust = -0.15, vjust = 0.5, size = 3.5, lineheight = 0.9
  ) +
  scale_color_manual(values = c(
    "25-34" = "#2E86AB",
    "45-54" = "#C0392B",
    "55-64" = "#27AE60"
  )) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("2024", "2025"),
    limits = c(0.9, 2.8)     # ← запас справа под подписи
  ) +
  scale_y_continuous(limits = c(20, 50)) +
  labs(
    title = "Доля умных потребителей: динамика 2024 → 2025",
    subtitle = "Источник: НИУ ВШЭ + Чижик (X5)",
    x = NULL, y = "%", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "#7F8C8D", size = 10)
  )

ggsave(here("visualizations", "H3_slope.png"), g4,
       width = 8, height = 4.5, dpi = 150)

# ============================================
# Итог
# ============================================

cat("Готово. Файлы в visualizations/:\n")
cat("  H3_delta.png\n")
cat("  H3_ipu.png\n")
cat("  H3_levels.png\n")
cat("  H3_slope.png\n")

cat("\nДопущения:\n")
cat("  - Волны 2024 и 2025 — независимые кросс-секционные выборки (не панель).\n")
cat("    Это допущение не проверяется по данным и не может быть проверено\n")
cat("    без панельного дизайна.\n")
cat("  - Размеры подгрупп — оценка в диапазоне 500-2000.\n")
cat("    Точных n для групп 25-34, 45-54, 55-64 в источниках нет:\n")
cat("    Демография_Сегменты даёт укрупнённые группы (до 35 / 35-54 / 55+),\n")
cat("    которые с группами H3 не совпадают.\n")
cat("  - Консервативный сценарий (n=500) уменьшает мощность теста:\n")
cat("    получить значимость при нём сложнее, а не легче.\n")
cat("  - Выбор соседей (25-34 и 55-64) определён структурой данных:\n")
cat("    в Chizhik_ВШЭ_Сегменты только 4 возрастные группы,\n")
cat("    65+ не годится как сосед (пенсионеры, другая фаза).\n")
