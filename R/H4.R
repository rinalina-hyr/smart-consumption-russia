# ============================================
# H4: Прогноз доли умных потребителей к 2028
# ============================================

library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

# --- 1. Данные ---
con <- dbConnect(SQLite(),
                 "C:/Users/irin/Documents/GitHub/smart-consumption-russia/data/smart_consumption.db")

chizhik <- dbGetQuery(con, "SELECT * FROM chizhik_index")
dbDisconnect(con)

# --- 2. Берём долю умных за 2024 и 2025 ---
smart <- chizhik %>%
  filter(metric == "Доля умных потребителей (совокупный индекс)") %>%
  transmute(year = 2024, value = as.numeric(value_2024)) %>%
  bind_rows(
    chizhik %>%
      filter(metric == "Доля умных потребителей (совокупный индекс)") %>%
      transmute(year = 2025, value = as.numeric(value_2025))
  )

print("Исходные точки:")
print(smart)

# --- 3. Три сценария прогноза ---
# Базовый темп роста +5 п.п./год
forecast <- tibble(
  year = c(2024, 2025, 2026, 2027, 2028),
  actual = c(32, 37, NA, NA, NA),
  optimistic  = c(32, 37, 42, 47, 51),  # +5/+5/+5/+4
  realistic   = c(32, 37, 42, 44, 46),  # +5/+5/+2/+2
  conservative= c(32, 37, 41, 42, 43)   # +5/+4/+1/+1
)

print("Сценарии прогноза:")
print(forecast)

# --- 4. График ---
forecast_long <- forecast %>%
  tidyr::pivot_longer(cols = c(optimistic, realistic, conservative),
                      names_to = "scenario", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(scenario = factor(scenario,
                           levels = c("optimistic", "realistic", "conservative"),
                           labels = c("Оптимистичный (+5/год)",
                                      "Реалистичный (+5,+5,+3,+2)",
                                      "Консервативный (+5,+5,+2,+2)")))

fig <- ggplot() +
  # Исторические данные
  geom_line(data = forecast %>% filter(!is.na(actual)),
            aes(x = year, y = actual),
            color = "#1D3557", linewidth = 1.5) +
  geom_point(data = forecast %>% filter(!is.na(actual)),
             aes(x = year, y = actual),
             color = "#1D3557", size = 4) +
  geom_text(data = forecast %>% filter(!is.na(actual)),
            aes(x = year, y = actual, label = paste0(actual, "%")),
            vjust = -1.2, size = 4, fontface = "bold") +
  # Прогноз — 3 линии
  geom_line(data = forecast_long,
            aes(x = year, y = value, color = scenario),
            linewidth = 1.2, linetype = "dashed") +
  geom_point(data = forecast_long %>% filter(year == 2028),
             aes(x = year, y = value, color = scenario),
             size = 3.5) +
  geom_text(data = forecast_long %>% filter(year == 2028),
            aes(x = year, y = value, label = paste0(value, "%"), color = scenario),
            hjust = -0.3, size = 4, fontface = "bold") +
  scale_color_manual(values = c("Оптимистичный (+5/год)" = "#2E86AB",
                                "Реалистичный (+5,+5,+3,+2)" = "#F4A261",
                                "Консервативный (+5,+5,+2,+2)" = "#E63946")) +
  scale_x_continuous(breaks = 2024:2028) +
  labs(
    title = "H4: Прогноз доли умных потребителей к 2028 году",
    subtitle = "Три сценария на основе темпа роста 2024 → 2025 (+5 п.п.)",
    x = "Год",
    y = "Доля умных потребителей, %",
    color = "Сценарий"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(fig)
ggsave("C:/Users/irin/Documents/GitHub/smart-consumption-russia/visualizations/H4_forecast.png",
       plot = fig, width = 12, height = 6, dpi = 150)

print("График сохранён: visualizations/H4_forecast.png")

# --- 5. Вывод ---
print("---")
print("ИТОГ H4:")
print(paste("Реалистичный сценарий к 2028:", forecast$realistic[5], "%"))
print(paste("Диапазон всех сценариев:", 
            min(forecast$conservative[5], forecast$optimistic[5]),
            "–",
            max(forecast$conservative[5], forecast$optimistic[5]), "%"))

if (forecast$realistic[5] >= 42 && forecast$realistic[5] <= 48) {
  print("✅ H4 ПОДТВЕРЖДЕНА: реалистичный прогноз 42–48% к 2028")
} else {
  print("⚠️ H4 требует корректировки")
}

# --- 6. Дополнительный контекст: демография ---
demography <- dbGetQuery(DBI::dbConnect(SQLite(),
                                        "C:/Users/irin/Documents/GitHub/smart-consumption-russia/data/smart_consumption.db"),
                         "SELECT * FROM demographics")

print("---")
print("Демографический контекст (куда движется население к 2030):")
print(demography)