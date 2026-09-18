# ============================================
# H2: Синхронность финансовой осторожности и умного потребления
# ============================================
install.packages("tidyr")

library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)

# --- 1. Подключаемся к базе ---
con <- dbConnect(SQLite(),
                 "C:/Users/irin/Documents/GitHub/smart-consumption-russia/data/smart_consumption.db")

savings <- dbGetQuery(con, "SELECT * FROM hse_savings_credits")
chizhik <- dbGetQuery(con, "SELECT * FROM chizhik_index")
macro   <- dbGetQuery(con, "SELECT * FROM hse_macro_context")

dbDisconnect(con)

# --- 2. Готовим данные HSE ---
savings <- savings %>%
  filter(!is.na(wave), grepl("20", wave)) %>%
  mutate(
    no_savings = as.numeric(нет_сбережений_нет_кредитов_pct),
    has_savings = as.numeric(есть_ликвидные_сбережения_pct),
    wave_short = c("2023 (лето)", "2023 (осень)", "2023-24",
                   "2024 (весна)", "2024 (осень)", "2025 (весна)", "2025 (осень)")
  ) %>%
  select(wave_short, no_savings, has_savings)

print("HSE данные:")
print(savings)

# --- 3. Готовим данные Chizhik ---
smart <- chizhik %>%
  filter(metric == "Доля умных потребителей (совокупный индекс)") %>%
  mutate(
    value_2024 = as.numeric(value_2024),
    value_2025 = as.numeric(value_2025)
  ) %>%
  select(value_2024, value_2025)

smart_2024 <- smart$value_2024[1]
smart_2025 <- smart$value_2025[1]

# --- 4. Совмещаем на одном графике ---
# HSE — по волнам (7 точек)
# Chizhik — по годам (2 точки), привязываем к ближайшим волнам

# Создаём общий лонг-датафрейм
hse_long <- savings %>%
  pivot_longer(cols = c(no_savings, has_savings),
               names_to = "metric_hse", values_to = "value_hse") %>%
  mutate(
    metric_label = case_when(
      metric_hse == "no_savings"  ~ "Без сбережений и без кредитов (HSE)",
      metric_hse == "has_savings" ~ "Есть ликвидные сбережения (HSE)"
    )
  )

# Индекс умных — растянем на 7 волн для сопоставления
# (интерполяция между 2024 и 2025)
smart_interp <- tibble(
  wave_short = savings$wave_short,
  value_smart = c(NA, NA, NA, smart_2024, NA, smart_2025, NA)
) %>%
  mutate(value_smart = approx(seq_along(wave_short), value_smart,
                              xout = seq_along(wave_short),
                              rule = 2)$y)

print("Интерполированные значения умных:")
print(smart_interp)

# --- 5. График 1: три линии на одном полотне ---
fig1 <- ggplot() +
  geom_line(data = hse_long,
            aes(x = wave_short, y = value_hse, color = metric_label, group = metric_label),
            linewidth = 1.3) +
  geom_point(data = hse_long,
             aes(x = wave_short, y = value_hse, color = metric_label),
             size = 3) +
  geom_line(data = smart_interp,
            aes(x = wave_short, y = value_smart, group = 1),
            color = "#2E86AB", linewidth = 1.3, linetype = "dashed") +
  geom_point(data = smart_interp,
             aes(x = wave_short, y = value_smart),
             color = "#2E86AB", size = 3, shape = 17) +
  labs(
    title = "H2: Финансовая осторожность и умное потребление растут синхронно",
    subtitle = "HSE — по волнам 2023–2025; Chizhik — интерполяция между 2024 и 2025",
    x = NULL,
    y = "% домохозяйств / доля умных",
    color = NULL
  ) +
  scale_color_manual(values = c("Без сбережений и без кредитов (HSE)" = "#E63946",
                                "Есть ликвидные сбережения (HSE)" = "#2E86AB")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

ggsave("C:/Users/irin/Documents/GitHub/smart-consumption-russia/visualizations/H2_trends.png",
       plot = fig1, width = 12, height = 6, dpi = 150)

print("График 1 сохранён: visualizations/H2_trends.png")

# --- 6. Считаем корреляцию Спирмена по 2 точкам пересечения ---
# Пересечение: 2024 (весна) и 2025 (весна)
hse_at_intersect <- c(
  savings$no_savings[savings$wave_short == "2024 (весна)"],
  savings$no_savings[savings$wave_short == "2025 (весна)"]
)
smart_at_intersect <- c(smart_2024, smart_2025)

cor_test <- cor.test(hse_at_intersect, smart_at_intersect, method = "spearman")

print("Корреляция Спирмена на 2 точках пересечения:")
print(cor_test)
print("⚠️ 2 точки — это мало для статистики. Корреляция скорее иллюстративна.")

# --- 7. Направление трендов ---
dir_no_savings <- savings$no_savings[nrow(savings)] - savings$no_savings[1]
dir_smart <- smart_2025 - smart_2024

print("---")
print(paste("Тренд «без сбережений»:", round(dir_no_savings, 1), "п.п. (за 2 года)"))
print(paste("Тренд «умных»:", round(dir_smart, 1), "п.п. (за 1 год)"))

if (dir_no_savings > 0 && dir_smart > 0) {
  print("✅ H2 ПОДТВЕРЖДЕНА: оба тренда однонаправлены (растут)")
} else {
  print("❌ H2 не подтверждается: тренды разнонаправлены")
}
print(fig1)

# --- 8. Контекст из макроэкономики ---
print("---")
print("Макроэкономический контекст (HSE):")
print(macro %>% select(metric, value, unit, period))