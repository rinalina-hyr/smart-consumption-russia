# ============================================
# H5: Кластеризация товарных категорий
# ============================================

library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)

# --- Загрузка данных ---
con <- dbConnect(SQLite(),
                 "C:/Users/irin/Documents/GitHub/smart-consumption-russia/data/smart_consumption.db")
categories <- dbGetQuery(con, "SELECT * FROM sberindex_categories")
dbDisconnect(con)

# --- Подготовка признаков ---
df <- categories %>%
  filter(category != "Все расходы") %>%
  mutate(
    growth_2025 = as.numeric(growth_pct_2025),
    growth_2026 = as.numeric(growth_pct_apr_may_2026),
    acceleration = growth_2026 - growth_2025,
    short_name = case_when(
      grepl("Продовольственные", category) ~ "Продовольствие",
      grepl("Непродовольственные", category) ~ "Непрод. всего",
      grepl("Маркетплейсы", category) ~ "Маркетплейсы",
      grepl("Топливо", category) ~ "Топливо",
      grepl("Лекарства", category) ~ "Лекарства",
      grepl("стройки", category) ~ "Стройка",
      grepl("Одежда", category) ~ "Одежда",
      grepl("Бытовая", category) ~ "Быт. техника",
      grepl("красоты", category) ~ "Красота",
      grepl("Мебель", category) ~ "Мебель",
      grepl("Услуги", category) ~ "Услуги всего",
      grepl("ЖКУ", category) ~ "ЖКУ",
      grepl("Медицинские услуги", category) ~ "Мед. услуги",
      grepl("Такси", category) ~ "Такси",
      grepl("Путешествия", category) ~ "Туризм",
      grepl("Авиабилеты", category) ~ "Авиабилеты",
      grepl("Общепит", category) ~ "Общепит",
      TRUE ~ category
    )
  ) %>%
  select(short_name, growth_2025, growth_2026, acceleration) %>%
  filter(!is.na(growth_2025), !is.na(growth_2026))

print(df)

# --- Кластеризация k-means ---
set.seed(42)
features <- df %>% select(growth_2025, growth_2026, acceleration)
features_scaled <- scale(features)

km <- kmeans(features_scaled, centers = 3, nstart = 25)
df$cluster <- factor(km$cluster)

print("Размеры кластеров:")
print(km$size)

# --- Профили кластеров ---
cluster_profile <- df %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    avg_2025 = round(mean(growth_2025), 1),
    avg_2026 = round(mean(growth_2026), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_2026))

print("Профиль кластеров:")
print(cluster_profile)

# --- Присвоение имён кластерам ---
cluster_profile <- cluster_profile %>%
  mutate(
    label = case_when(
      avg_2025 >= 20 ~ "Высокий рост",       # кластер 1: 27.3
      avg_2025 >= 5  ~ "Умеренный рост",     # кластер 2: 12.6
      TRUE           ~ "Снижающиеся"          # кластер 3: -1.2
    )
  )

df <- df %>% left_join(cluster_profile %>% select(cluster, label), by = "cluster")

print("Категории по кластерам:")
print(df %>% arrange(cluster, desc(growth_2026)) %>%
        select(short_name, cluster, label, growth_2025, growth_2026))

# --- PCA для визуализации ---
pca <- prcomp(features_scaled, center = FALSE, scale. = FALSE)
df$pc1 <- pca$x[, 1]
df$pc2 <- pca$x[, 2]

var_pc1 <- round(pca$sdev[1]^2 / sum(pca$sdev^2) * 100, 1)
var_pc2 <- round(pca$sdev[2]^2 / sum(pca$sdev^2) * 100, 1)

# --- График 1: PCA ---
fig1 <- ggplot(df, aes(x = pc1, y = pc2, color = label)) +
  geom_point(size = 5, alpha = 0.85) +
  geom_text(aes(label = short_name), vjust = -1, size = 3.2) +
  labs(
    title = "Кластеризация товарных категорий (k-means, k=3)",
    subtitle = "PCA-проекция по признакам: рост 2025, рост 2026, ускорение",
    x = paste0("PC1 (", var_pc1, "%)"),
    y = paste0("PC2 (", var_pc2, "%)"),
    color = "Кластер"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(fig1)
ggsave("C:/Users/irin/Documents/GitHub/smart-consumption-russia/visualizations/H5_clusters_pca.png",
       plot = fig1, width = 11, height = 7, dpi = 150)

# --- График 2: Scatter 2025 vs 2026 ---
fig2 <- ggplot(df, aes(x = growth_2025, y = growth_2026, color = label)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 5, alpha = 0.85) +
  geom_text(aes(label = short_name), vjust = -1, size = 3) +
  labs(
    title = "Рост категорий: 2025 vs апрель-май 2026",
    subtitle = "Диагональ — рост не изменился. Выше — ускорение, ниже — замедление",
    x = "Рост в 2025, %",
    y = "Рост в апреле-мае 2026, %",
    color = "Кластер"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(fig2)
ggsave("C:/Users/irin/Documents/GitHub/smart-consumption-russia/visualizations/H5_clusters_scatter.png",
       plot = fig2, width = 11, height = 7, dpi = 150)

# --- Итог ---
print("---")
print(paste("Число кластеров:", length(unique(df$cluster))))
print(paste("Объяснённая дисперсия PC1+PC2:", var_pc1 + var_pc2, "%"))
print("")
print("Состав кластеров:")
for (lbl in unique(df$label)) {
  cat("\n", lbl, ":\n")
  cats <- df %>% filter(label == lbl) %>% pull(short_name)
  cat(" ", paste(cats, collapse = ", "), "\n")
}