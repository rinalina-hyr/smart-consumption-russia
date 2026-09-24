# H2: ценовой компонент индекса умного потребления
# и давление на бюджеты домохозяйств


pacman::p_load(DBI, RSQLite, dplyr, tidyr, ggplot2, here, forcats, stringr)

con <- dbConnect(SQLite(), here("data", "smart_consumption.db"))

index    <- dbGetQuery(con, "SELECT * FROM chizhik_index")
savings  <- dbGetQuery(con, "SELECT * FROM hse_savings_credits")
fmcg_inf <- dbGetQuery(con, "SELECT * FROM fmcg_inflation")
ipu_dyn  <- dbGetQuery(con, "SELECT * FROM romir_confidence_dynamics")

dbDisconnect(con)


# 1. Компоненты индекса Chizhik: 2024 → 2025


short_names <- c(
  "Доля умных потребителей (совокупный индекс)" = "Доля умных (индекс)",
  "Индекс умного потребления (п.п.)"           = "Индекс (п.п.)",
  "Ориентируются на приемлемое качество"       = "Качество",
  "Ориентируются исключительно на низкую цену" = "Только цена",
  "Не готовы переплачивать за известный бренд" = "Не за бренд",
  "Планируют покупки заранее"                  = "Планируют",
  "Легко ориентируются среди множества предложений" = "Ориентируются",
  "Стремятся минимизировать время на покупки"  = "Экономят время"
)

df <- index %>%
  filter(!is.na(value_2024), !is.na(value_2025)) %>%
  mutate(
    metric_short = recode(metric, !!!short_names),
    delta = value_2025 - value_2024,
    is_price = grepl("низкую цену", metric),
    is_index = metric_short %in% c("Индекс (п.п.)", "Доля умных (индекс)")
  ) %>%
  select(metric, metric_short, value_2024, value_2025, delta, is_price, is_index)

cat("Компоненты индекса Chizhik, 2024 → 2025:\n")
print(df %>% select(metric_short, value_2024, value_2025, delta))
cat("\n")

mean_other <- df %>%
  filter(!is_price, !is_index) %>%
  pull(delta) %>%
  mean(na.rm = TRUE)

price_delta <- df %>% filter(is_price) %>% pull(delta) %>% as.numeric()

cat(sprintf("Ценовой компонент Δ:                    %+.1f п.п.\n", price_delta))
cat(sprintf("Остальные 5 компонентов Δ (среднее):    %+.1f п.п.\n\n", mean_other))


# 2. График: компоненты 2024 vs 2025

df_long <- df %>%
  filter(!is_index) %>%
  select(metric_short, value_2024, value_2025, is_price) %>%
  pivot_longer(c(value_2024, value_2025),
               names_to = "year", values_to = "value") %>%
  mutate(
    year = ifelse(year == "value_2024", "2024", "2025"),
    metric_short = fct_reorder(metric_short, value, .fun = mean)
  )

g1 <- ggplot(df_long, aes(value, metric_short, fill = year)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.0f", value)),
            position = position_dodge(0.75),
            hjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("2024" = "lightblue", "2025" = "blue")) +
  scale_x_continuous(limits = c(0, 80)) +
  labs(
    title = "Компоненты индекса Chizhik: 2024 vs 2025",
    subtitle = "Источник: НИУ ВШЭ + Чижик (X5), 2024–2025",
    x = "%", y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(here("visualizations", "H2_components.png"), g1,
       width = 9, height = 5.5, dpi = 150)


# 3. График: прирост каждого компонента, 2024 → 2025


df_delta <- df %>%
  filter(!is_index) %>%
  mutate(
    metric_short = fct_reorder(metric_short, delta),
    color = ifelse(is_price, "Ценовой компонент", "Остальные")
  )

g2 <- ggplot(df_delta, aes(delta, metric_short, fill = color)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.0f п.п.", delta)),
            hjust = ifelse(df_delta$delta >= 0, -0.3, 1.3),
            size = 4) +
  scale_fill_manual(values = c(
    "Ценовой компонент" = "red",
    "Остальные" = "blue"
  )) +
  scale_x_continuous(limits = c(-2, 8)) +
  labs(
    title = "Прирост компонентов индекса, 2024 → 2025",
    subtitle = "Ценовой компонент не сдвинулся, остальные выросли на +2…+5 п.п.",
    x = "Изменение, п.п.", y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray", size = 10)
  )

ggsave(here("visualizations", "H2_delta.png"), g2,
       width = 9, height = 5, dpi = 150)


# 4. Давление на бюджеты — три окна сравнения


sav <- savings %>%
  rename(
    savings_pct          = `есть_ликвидные_сбережения_pct`,
    credits_pct          = `есть_непогашенный_кредит_pct`,
    no_sav_no_credit_pct = `нет_сбережений_нет_кредитов_pct`
  ) %>%
  mutate(
    wave = factor(wave, levels = c(
      "Июнь-июль 2023",
      "Сентябрь-ноябрь 2023",
      "Декабрь2023-Февраль2024",
      "Апрель-май 2024",
      "Октябрь-ноябрь 2024",
      "Апрель-май 2025",
      "Октябрь-ноябрь 2025"
    )),
    wave_num = as.numeric(wave)
  )

a_first <- sav$no_sav_no_credit_pct[sav$wave_num == 1]
a_last  <- sav$no_sav_no_credit_pct[sav$wave_num == 7]

b_2024 <- sav$no_sav_no_credit_pct[sav$wave == "Апрель-май 2024"]
b_2025 <- sav$no_sav_no_credit_pct[sav$wave == "Апрель-май 2025"]

c_2024 <- sav$no_sav_no_credit_pct[sav$wave == "Октябрь-ноябрь 2024"]
c_2025 <- sav$no_sav_no_credit_pct[sav$wave == "Октябрь-ноябрь 2025"]

cat("Давление на бюджеты — три окна сравнения:\n")
cat(sprintf("  A. Июн-июл 2023 → окт-ноя 2025:  %.1f%% → %.1f%% (Δ %+.1f п.п.) — окно НЕ совпадает с индексом\n",
            a_first, a_last, a_last - a_first))
cat(sprintf("  B. Апр-май 2024 → апр-май 2025:  %.1f%% → %.1f%% (Δ %+.1f п.п.)\n",
            b_2024, b_2025, b_2025 - b_2024))
cat(sprintf("  C. Окт-ноя 2024 → окт-ноя 2025:  %.1f%% → %.1f%% (Δ %+.1f п.п.) — основное окно ниже\n\n",
            c_2024, c_2025, c_2025 - c_2024))

# Основной показатель для сопоставления с индексом — вариант C
delta_no_sav_credit <- c_2025 - c_2024
window_c_label <- "окт-ноя 2024 → окт-ноя 2025"


# 5. График: динамика индикаторов сбережений


sav_long <- sav %>%
  select(wave, wave_num, savings_pct, no_sav_no_credit_pct, credits_pct) %>%
  pivot_longer(-c(wave, wave_num),
               names_to = "metric", values_to = "value") %>%
  mutate(
    metric = recode(metric,
                    "savings_pct"          = "Есть ликвидные сбережения",
                    "no_sav_no_credit_pct" = "Нет сбережений и нет кредитов",
                    "credits_pct"          = "Есть непогашенный кредит"
    )
  )

g3 <- ggplot(sav_long, aes(wave_num, value, color = metric, group = metric)) +
  annotate("rect", xmin = 5, xmax = 7, ymin = -Inf, ymax = Inf,
           fill = "#F5F5F5", alpha = 0.6) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  annotate("text", x = 6, y = 58,
           label = "окно измерения\nиндекса Chizhik\n(2024–2025)",
           size = 3, color = "gray", lineheight = 0.9) +
  scale_color_manual(values = c(
    "Есть ликвидные сбережения"          = "green",
    "Нет сбережений и нет кредитов"       = "red",
    "Есть непогашенный кредит"            = "gray"
  )) +
  scale_x_continuous(
    breaks = 1:7,
    labels = c("Июн-июл\n2023", "Сен-ноя\n2023", "Дек 23–\nфев 24",
               "Апр-май\n2024", "Окт-ноя\n2024", "Апр-май\n2025",
               "Окт-ноя\n2025")
  ) +
  scale_y_continuous(limits = c(0, 62)) +
  labs(
    title = "Финансовое положение домохозяйств, 2023–2025",
    subtitle = "Затенённая область — окно, где обе точки сравнения C лежат внутри годов индекса",
    x = NULL, y = "Доля респондентов, %", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray", size = 10)
  )

ggsave(here("visualizations", "H2_savings_trend.png"), g3,
       width = 10, height = 5, dpi = 150)


# 6. График: сопоставление (обновлено на окно C)


comparison <- tibble(
  metric = c(
    sprintf("Давление на бюджеты:\nдоля без сбережений и без кредитов\n(%s)", window_c_label),
    "Остальные компоненты индекса\n(среднее, 2024 → 2025)",
    "Ценовой компонент:\n«только низкая цена»\n(2024 → 2025)"
  ),
  value = c(delta_no_sav_credit, mean_other, price_delta),
  type  = c("Давление", "Остальные компоненты", "Ценовой компонент")
)

cat("Сопоставление (окно C):\n")
print(comparison %>% select(-type))
cat("\n")

g4 <- ggplot(comparison,
             aes(value, fct_reorder(metric, value), fill = type)) +
  geom_col(width = 0.55) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f п.п.", value)),
            hjust = ifelse(comparison$value >= 0, -0.3, 1.3),
            size = 4.5) +
  scale_fill_manual(values = c(
    "Давление"              = "gray",
    "Остальные компоненты"  = "blue",
    "Ценовой компонент"     = "red"
  )) +
  scale_x_continuous(limits = c(-2, 6)) +
  labs(
    title = "Давление на бюджеты vs компоненты индекса",
    subtitle = sprintf("В окне измерения индекса (%s) давление на бюджеты не росло", window_c_label),
    x = "Изменение, п.п.", y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray", size = 10)
  )

ggsave(here("visualizations", "H2_gap_vs_price.png"), g4,
       width = 10, height = 5, dpi = 150)



# 8. 

cat("--- Итог H2 ---\n\n")

cat("Что показывают данные:\n\n")
cat(sprintf("  Давление на бюджеты, окно C (%s):\n", window_c_label))
cat(sprintf("    Доля без сбережений и без кредитов: %.1f%% → %.1f%% (Δ %+.1f п.п.)\n\n",
            c_2024, c_2025, delta_no_sav_credit))

cat("  Компоненты индекса Chizhik (2024 → 2025):\n")
cat(sprintf("    Ценовой компонент («только цена»):   43%% → 43%% (%+.1f п.п.)\n",
            price_delta))
cat(sprintf("    Остальные 5 компонентов (среднее):   %+.1f п.п.\n\n",
            mean_other))
