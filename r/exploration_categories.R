# ============================================
# Региональный разрез офлайн-торговли
# ДОПОЛНИТЕЛЬНОЕ НАБЛЮДЕНИЕ (вне основной рамки)
# ============================================
# Этот скрипт не проверяет гипотезы H1–H3 напрямую.
# Он показывает контекст: где именно падает офлайн-оборот.
# Разрез только по офлайну, 7 наблюдений, слабая корреляция.
# Выводы в статье должны формулироваться осторожно.
# ============================================

pacman::p_load(DBI, RSQLite, dplyr, tidyr, ggplot2, here, stringr)

con <- dbConnect(SQLite(), here("data", "smart_consumption.db"))

offline <- dbGetQuery(con, "SELECT * FROM sberindex_offline")

dbDisconnect(con)

# ============================================
# Подготовка данных
# ============================================

# --- Короткие названия городов ---

city_short <- c(
  "Москва"                = "Москва",
  "Санкт-Петербург"       = "СПб",
  "Миллионники"           = "Миллионники",
  "Города 500 тыс.-1 млн" = "500 тыс. – 1 млн",
  "Города 300-500 тыс."   = "300–500 тыс.",
  "Города 100-300 тыс."   = "100–300 тыс.",
  "Города менее 100 тыс." = "менее 100 тыс."
)

city_order <- c(
  "Москва", "СПб", "Миллионники",
  "500 тыс. – 1 млн", "300–500 тыс.",
  "100–300 тыс.", "менее 100 тыс."
)

df <- offline %>%
  mutate(
    city_short = recode(city, !!!city_short),
    city_short = factor(city_short, levels = city_order),
    growth = as.numeric(growth_pct_q1_2026_vs_q1_2025),
    channel = paste(category, "·", format)
  )

cat("Данные:\n")
print(df %>% select(city_short, channel, growth))
cat("\n")

# ============================================
# График 1: тепловая карта город × канал
# ============================================

# Порядок каналов: продовольствие ТЦ, street, непрод ТЦ, street
channel_order <- c(
  "Продукты · ТЦ",
  "Продукты · Street retail",
  "Непродовольственные · ТЦ",
  "Непродовольственные · Street retail"
)

df$channel <- factor(df$channel, levels = channel_order)

g1 <- ggplot(df, aes(city_short, channel, fill = growth)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = sprintf("%+.1f", growth)),
            color = "white", size = 4.2, fontface = "bold") +
  scale_fill_gradient2(
    low = "#8B0000", mid = "#F5F5F5", high = "#2E86AB",
    midpoint = 0, limits = c(-11, 2),
    name = "Изменение, %"
  ) +
  labs(
    title = "Офлайн-оборот по городам и форматам, Q1 2026 к Q1 2025",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

ggsave(here("visualizations", "regions_offline_heatmap.png"), g1,
       width = 11, height = 5.5, dpi = 150)

# ============================================
# График 2: сводный по размеру города
# ============================================

# Разбиваем: Москва / СПб отдельно, остальное — по размеру
df_summary <- df %>%
  mutate(
    city_group = case_when(
      city_short == "Москва" ~ "Москва",
      city_short == "СПб" ~ "СПб",
      TRUE ~ as.character(city_short)
    ),
    city_group = factor(city_group, levels = city_order),
    cat_short = ifelse(grepl("Продукты", category),
                       "Продукты", "Непродовольственные")
  ) %>%
  group_by(city_group, cat_short) %>%
  summarise(avg_growth = mean(growth), .groups = "drop")

g2 <- ggplot(df_summary, aes(city_group, avg_growth, fill = cat_short)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f", avg_growth)),
            position = position_dodge(0.75),
            vjust = ifelse(df_summary$avg_growth >= 0, -0.5, 1.5),
            size = 3.8) +
  scale_fill_manual(values = c(
    "Продукты" = "#27AE60",
    "Непродовольственные" = "#C0392B"
  )) +
  scale_y_continuous(limits = c(-9, 3)) +
  labs(
    title = "Средний рост офлайн-оборота по городам, Q1 2026 к Q1 2025",
    x = NULL, y = "Изменение, %", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 20, hjust = 1, size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(here("visualizations", "regions_offline_summary.png"), g2,
       width = 9, height = 4.5, dpi = 150)

# ============================================
# График 3: разрыв прод vs непрод по городам
# ============================================

df_gap <- df %>%
  mutate(cat_short = ifelse(grepl("Продукты", category),
                            "Продукты", "Непродовольственные")) %>%
  group_by(city_short, cat_short) %>%
  summarise(avg = mean(growth), .groups = "drop") %>%
  pivot_wider(names_from = cat_short, values_from = avg) %>%
  mutate(gap = Продукты - Непродовольственные)

g3 <- ggplot(df_gap, aes(city_short, gap)) +
  geom_col(width = 0.55, fill = "#E67E22") +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f", gap)),
            vjust = -0.6, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, 8)) +
  labs(
    title = "Разрыв между продовольствием и непродовольствием, Q1 2026",
    subtitle = "Насколько продовольствие растёт быстрее непродовольствия, п.п.",
    x = NULL, y = "Разрыв, п.п."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1, size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(here("visualizations", "regions_offline_gap.png"), g3,
       width = 9, height = 4.5, dpi = 150)

# ============================================
# Анализ
# ============================================

cat("\n--- Сводка по городам ---\n\n")

# Средний рост по категориям и городам
summary_by_cat <- df %>%
  group_by(city_short, category) %>%
  summarise(avg = mean(growth), .groups = "drop") %>%
  pivot_wider(names_from = category, values_from = avg)

print(summary_by_cat)
cat("\n")

# Непрод: от Москвы к малым городам
neprod <- df %>%
  filter(grepl("Непрод", category)) %>%
  group_by(city_short) %>%
  summarise(avg = mean(growth), .groups = "drop")

cat("Непродовольственный офлайн по городам (средний рост, %):\n")
for (i in 1:nrow(neprod)) {
  cat(sprintf("  %-22s %+.1f\n",
              as.character(neprod$city_short[i]), neprod$avg[i]))
}
cat("\n")

# Тренд: чем меньше город, тем сильнее падение?
neprod_num <- neprod %>%
  mutate(city_rank = as.numeric(city_short))

cor_val <- cor(neprod_num$city_rank, neprod_num$avg, method = "spearman")
cat(sprintf("Корреляция (размер города × рост непрода, Спирмен): %.2f\n", cor_val))
cat("Интерпретация: отрицательная = чем меньше город, тем сильнее падение.\n\n")

# Прод: стабильность
prod <- df %>%
  filter(grepl("Прод", category)) %>%
  group_by(city_short) %>%
  summarise(avg = mean(growth), .groups = "drop")

cat("Продовольственный офлайн по городам (средний рост, %):\n")
for (i in 1:nrow(prod)) {
  cat(sprintf("  %-22s %+.1f\n",
              as.character(prod$city_short[i]), prod$avg[i]))
}
cat("\n")

# ============================================
# Финальный вывод
# ============================================

cat("--- Итог ---\n\n")
cat("Что показывают данные:\n\n")

# --- 1. Прод ---
prod_moscow   <- prod$avg[prod$city_short == "Москва"]
prod_million  <- prod$avg[prod$city_short == "Миллионники"]
prod_min      <- min(prod$avg)
prod_max      <- max(prod$avg)
prod_range <- round(prod_max - prod_min, 1)

cat("1. Продовольственный офлайн стабилен по всем городам.\n")
cat(sprintf("   От %.1f до %.1f п.п. (разброс %.1f п.п.).\n",
            prod_min, prod_max, prod_range))
cat("   Это в пределах шума.\n\n")

# --- 2. Непрод ---
neprod_moscow  <- neprod$avg[neprod$city_short == "Москва"]
neprod_spb     <- neprod$avg[neprod$city_short == "СПб"]
neprod_others  <- neprod$avg[!neprod$city_short %in% c("Москва", "СПб")]
neprod_min_oth <- min(neprod_others)
neprod_max_oth <- max(neprod_others)

cat("2. Непродовольственный офлайн падает везде, но по-разному.\n")
cat(sprintf("   Москва:    %+.1f%%\n", neprod_moscow))
cat(sprintf("   СПб:       %+.1f%%\n", neprod_spb))
cat(sprintf("   Остальные: от %+.1f%% до %+.1f%%\n\n",
            neprod_min_oth, neprod_max_oth))

# --- 3. Разрыв ---
cat("3. Разрыв прод vs непрод:\n")
for (i in 1:nrow(df_gap)) {
  cat(sprintf("   %-22s %+.1f п.п.\n",
              as.character(df_gap$city_short[i]), df_gap$gap[i]))
}
cat("\n")

# --- 4. Паттерн ---
gap_moscow <- df_gap$gap[df_gap$city_short == "Москва"]
gap_spb    <- df_gap$gap[df_gap$city_short == "СПб"]
gap_others <- df_gap$gap[!df_gap$city_short %in% c("Москва", "СПб")]

cat("Паттерн:\n\n")
cat("  Данные согласуются не с градиентом «чем меньше город,\n")
cat("  тем сильнее падение», а с контрастом:\n")
cat(sprintf("  - Москва: разрыв %+.1f п.п.\n", gap_moscow))
cat(sprintf("  - СПб: %+.1f п.п.\n", gap_spb))
cat(sprintf("  - Все остальные города: от %+.1f до %+.1f п.п.\n\n",
            min(gap_others), max(gap_others)))

cat(sprintf("  Корреляция Спирмена: %.2f — слабая.\n",
            cor_val))
cat("  Это не градиент, а контраст между Москвой и остальными.\n\n")

# --- 5. Что это значит ---
diff_moscow_million <- neprod_moscow - neprod$avg[neprod$city_short == "Миллионники"]
diff_moscow_small   <- neprod_moscow - neprod$avg[neprod$city_short == "менее 100 тыс."]

cat("Что это значит:\n\n")
cat("  Глубина сжатия непродовольственного офлайна\n")
cat("  не определяется размером города линейно.\n")
cat(sprintf("  Разница Москва–миллионник: %.1f п.п.\n", diff_moscow_million))
cat(sprintf("  Разница Москва–малый город: %.1f п.п.\n\n", diff_moscow_small))

cat("  Данные согласуются с гипотезой о структурных различиях\n")
cat("  между московским рынком и рынками остальных городов,\n")
cat("  но выборка мала (7 городов) для строгих выводов.\n\n")

cat("Ограничения:\n")
cat("  - 7 наблюдений, корреляция слабая.\n")
cat("  - Разрез только по офлайну. Онлайн по городам не разбит.\n")
cat("  - Нельзя сказать, куда уходит непродовольственный спрос\n")
cat("    в малых городах (возможная версия — маркетплейсы).\n")