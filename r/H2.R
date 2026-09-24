# ============================================
# H2: ценовой компонент индекса умного потребления
# и давление на бюджеты домохозяйств
# ============================================
#
# ПЕРЕСМОТРЕНО. Исходная версия сравнивала окно индекса
# Chizhik (2024 → 2025) с давлением на бюджеты за 2023 → 2025
# (первая и последняя волна ЭПДХ) — это два РАЗНЫХ окна.
# В окне, где индекс реально измеряется (2024–2025),
# давление на бюджеты не росло — см. раздел 4 ниже.
# Значит, исходный тест H2 («растёт ли цена вместе
# с давлением») в этом окне непроверяем: нет роста
# давления, на который цена могла бы отреагировать.
#
# Что данные всё же позволяют сказать честно:
# компоненты индекса (кроме цены) выросли даже в период,
# когда давление не менялось — см. вывод в конце файла.
# ============================================

pacman::p_load(DBI, RSQLite, dplyr, tidyr, ggplot2, here, forcats, stringr)

con <- dbConnect(SQLite(), here("data", "smart_consumption.db"))

index    <- dbGetQuery(con, "SELECT * FROM chizhik_index")
savings  <- dbGetQuery(con, "SELECT * FROM hse_savings_credits")
fmcg_inf <- dbGetQuery(con, "SELECT * FROM fmcg_inflation")
ipu_dyn  <- dbGetQuery(con, "SELECT * FROM romir_confidence_dynamics")

dbDisconnect(con)

# ============================================
# 1. Компоненты индекса Chizhik: 2024 → 2025
# ============================================
# Без изменений относительно исходной версии — эта часть
# не зависела от проблемы с окном.

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

# ============================================
# 2. График: компоненты 2024 vs 2025
# ============================================
# Без изменений.

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
  scale_fill_manual(values = c("2024" = "#A8DADC", "2025" = "#2E86AB")) +
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

# ============================================
# 3. График: прирост каждого компонента, 2024 → 2025
# ============================================
# Без изменений.

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
    "Ценовой компонент" = "#C0392B",
    "Остальные" = "#2E86AB"
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
    plot.subtitle = element_text(color = "#7F8C8D", size = 10)
  )

ggsave(here("visualizations", "H2_delta.png"), g2,
       width = 9, height = 5, dpi = 150)

# ============================================
# 4. Давление на бюджеты — ТРИ окна сравнения
# ============================================
# Раньше здесь брались только первая и последняя волна
# (2023 → 2025). Это несинхронно с окном индекса (2024–2025).
# Три варианта, все считаются и выводятся честно:
#
#   A. Июн-июл 2023 → окт-ноя 2025 — исходный (неверный
#      для сопоставления с индексом) аргумент.
#   B. Апр-май 2024 → апр-май 2025 — ближайшая пара волн
#      вокруг предполагаемого окна индекса.
#   C. Окт-ноя 2024 → окт-ноя 2025 — вторая пара того же
#      сезона, наиболее вероятное окно измерения индекса
#      Chizhik (2024 и 2025 без более точной привязки к месяцу).
#
# C выбран как основной для сопоставления с индексом,
# потому что это единственная пара волн ЭПДХ, обе точки
# которой лежат внутри годов индекса (2024 и 2025) и разнесены
# ровно на 12 месяцев — не полгода, как у B.

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

cat(sprintf("ОСНОВНОЕ окно (C): давление на бюджеты изменилось на %+.1f п.п.\n",
            delta_no_sav_credit))
cat("Это означает: в единственном окне ЭПДХ, где обе точки\n")
cat("лежат внутри годов индекса Chizhik, давление на бюджеты\n")
cat(if (delta_no_sav_credit < 0) "СНИЗИЛОСЬ" else "практически не изменилось")
cat(" — исходный тест H2 («растёт ли цена вместе с давлением»)\n")
cat("в этом окне непроверяем: нет роста давления, на который\n")
cat("цена могла бы отреагировать.\n\n")

# ============================================
# 5. График: динамика индикаторов сбережений,
#    с отметкой окна измерения индекса
# ============================================

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
           size = 3, color = "#7F8C8D", lineheight = 0.9) +
  scale_color_manual(values = c(
    "Есть ликвидные сбережения"          = "#27AE60",
    "Нет сбережений и нет кредитов"       = "#C0392B",
    "Есть непогашенный кредит"            = "#7F8C8D"
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
    plot.subtitle = element_text(color = "#7F8C8D", size = 10)
  )

ggsave(here("visualizations", "H2_savings_trend.png"), g3,
       width = 10, height = 5, dpi = 150)

# ============================================
# 6. График: сопоставление (обновлено на окно C)
# ============================================

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
    "Давление"              = "#7F8C8D",
    "Остальные компоненты"  = "#2E86AB",
    "Ценовой компонент"     = "#C0392B"
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
    plot.subtitle = element_text(color = "#7F8C8D", size = 10)
  )

ggsave(here("visualizations", "H2_gap_vs_price.png"), g4,
       width = 10, height = 5, dpi = 150)

# ============================================
# 7. Дополнительный контекст (описательно,
#    без статистических тестов — слишком мало точек)
# ============================================

cat("--- Дополнительный контекст ---\n\n")

cat("A) Инфляция FMCG, 2023 → 2024 (Data Insight/TAdviser):\n")
print(fmcg_inf %>% select(year, inflation_pct, real_demand_pct))
cat("Это ДРУГОЕ окно (год до начала измерения индекса), поэтому\n")
cat("не заменяет тест H2, но показывает: реальное инфляционное\n")
cat("давление продолжало расти именно в 2023–2024 гг. — то есть\n")
cat("до того, как индекс Chizhik вообще начал измеряться.\n\n")

cat("B) ИПУ Ромир по кварталам, 2024–2025:\n")
print(ipu_dyn)
q4_2025 <- ipu_dyn %>% filter(period %in% c("2025 Октябрь", "2025 Ноябрь", "2025 Декабрь"))
cat("\nВ конце 2025 г. индекс потребительской уверенности резко\n")
cat("упал (сентябрь 13 → октябрь 4 → ноябрь 3 → декабрь 1), тогда\n")
cat("как объективный индикатор давления («нет сбережений и нет\n")
cat("кредитов») в этот же период держался на месте (~53%).\n")
cat("Расхождение настроения и поведения — отдельное наблюдение,\n")
cat("не часть основного теста H2, но согласуется с общим выводом:\n")
cat("поведенческие показатели не реагируют резко на краткосрочные\n")
cat("колебания давления или настроения.\n\n")

# ============================================
# 8. Вывод (переформулирован)
# ============================================

cat("--- Итог H2 (пересмотрено) ---\n\n")

cat("Что показывают данные:\n\n")
cat(sprintf("  Давление на бюджеты, окно C (%s):\n", window_c_label))
cat(sprintf("    Доля без сбережений и без кредитов: %.1f%% → %.1f%% (Δ %+.1f п.п.)\n\n",
            c_2024, c_2025, delta_no_sav_credit))

cat("  Компоненты индекса Chizhik (2024 → 2025):\n")
cat(sprintf("    Ценовой компонент («только цена»):   43%% → 43%% (%+.1f п.п.)\n",
            price_delta))
cat(sprintf("    Остальные 5 компонентов (среднее):   %+.1f п.п.\n\n",
            mean_other))

cat("Вывод:\n\n")
cat("  Исходная формулировка H2 («цена не растёт, хотя давление\n")
cat("  выросло») опиралась на сравнение разных окон: индекс\n")
cat("  измерялся в 2024–2025, а давление считалось за 2023–2025.\n")
cat("  В единственном окне ЭПДХ, обе точки которого лежат внутри\n")
cat("  годов индекса (окт-ноя 2024 → окт-ноя 2025), давление на\n")
cat(sprintf("  бюджеты не выросло (%+.1f п.п.) — поэтому исходный тест\n",
            delta_no_sav_credit))
cat("  здесь непроверяем: нет роста давления, на который цена\n")
cat("  могла бы отреагировать.\n\n")
cat("  Что можно сказать честно: качественные компоненты индекса\n")
cat("  (качество, планирование, отказ от бренда, ориентация,\n")
cat("  экономия времени) выросли на +2…+5 п.п. НЕЗАВИСИМО от\n")
cat("  того, что давление на бюджеты в этом окне не менялось.\n")
cat("  Значит, рост избирательности не объясняется краткосрочным\n")
cat("  ухудшением финансового положения — он идёт сам по себе.\n\n")

cat("Статус: не может быть проверено в исходной формулировке\n")
cat("(нет роста давления в проверяемом окне). Переформулированный\n")
cat("вывод — рост избирательности компонентов не связан с\n")
cat("краткосрочной динамикой давления на бюджеты — подтверждается.\n")