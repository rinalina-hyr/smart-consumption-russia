# H1: рост СТМ концентрируется в повседневных
# категориях, а не в дискреционных


pacman::p_load(DBI, RSQLite, dplyr, tidyr, ggplot2, here, stringr, forcats)

con <- dbConnect(SQLite(), here("data", "smart_consumption.db"))
categories <- dbGetQuery(con, "SELECT * FROM stm_categories")
categories_2022 <- dbGetQuery(con, "SELECT * FROM stm_categories_2022")
motivation <- dbGetQuery(con, "SELECT * FROM stm_motivation")
experience <- dbGetQuery(con, "SELECT * FROM stm_experience")
dbDisconnect(con)

# Топ категорий СТМ
cats <- categories %>%
  filter(!is.na(share_pct_june_2026)) %>%
  mutate(
    category_short = str_wrap(category, 25),
    category_short = fct_reorder(category_short, share_pct_june_2026)
  )

g1 <- ggplot(cats, aes(share_pct_june_2026, category_short)) +
  geom_col(fill = "lightblue", width = 0.7) +
  geom_text(aes(label = sprintf("%d%%", share_pct_june_2026)),
            hjust = -0.3, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Топ категорий покупок СТМ, июнь 2026",
    subtitle = "Источник: Ромир, выборка 758",
    x = "Доля покупателей, %", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(here("visualizations", "H1_stm_categories.png"), g1,
       width = 9, height = 6, dpi = 150)

# Сравнение с 2022 г. (Ромир, romir.ru/feed/romir-zafiksiroval-uvelichenie-doli-stm-na-rossiyskom-rynke-za-7-let)
# Ранг топ-категорий устойчив (молочные/бакалея — топ-2 в обеих точках),
# но уровни смешанные: большинство просело на -1..-4 п.п., гигиена выросла на +13.
category_match <- tribble(
  ~category_2022,          ~category_2026,                               ~match_note,
  "Молочные продукты",     "Молочная/кисло-молочная продукция, сыр",     "прямое",
  "Бакалея",                "Бакалея",                                    "прямое",
  "Товары личной гигиены", "Средства гигиены",                           "прямое",
  "Кондитерские изделия",  "Кондитерские изделия",                       "прямое",
  "Хозтовары",              "Средства для уборки дома",                  "приблизительное"
)

comparison_2022_2026 <- category_match %>%
  left_join(categories_2022 %>% select(category_2022 = category, share_2022 = share_pct),
            by = "category_2022") %>%
  left_join(categories %>% select(category_2026 = category, share_2026 = share_pct_june_2026),
            by = "category_2026") %>%
  mutate(delta_pp = share_2026 - share_2022)

print(comparison_2022_2026 %>% select(category_2022, share_2022, share_2026, delta_pp))

cmp_long <- comparison_2022_2026 %>%
  mutate(category_2022 = fct_reorder(category_2022, share_2026)) %>%
  select(category_2022, share_2022, share_2026) %>%
  pivot_longer(c(share_2022, share_2026), names_to = "year", values_to = "value") %>%
  mutate(year = ifelse(year == "share_2022", "2022", "2026"))

g_cmp <- ggplot(cmp_long, aes(value, category_2022, fill = year)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%d%%", value)),
            position = position_dodge(0.75), hjust = -0.3, size = 3.8) +
  scale_fill_manual(values = c("2022" = "lightblue", "2026" = "steelblue")) +
  scale_x_continuous(limits = c(0, 45)) +
  labs(
    title = "Доля покупателей СТМ по категориям: 2022 vs 2026",
    subtitle = "Сопоставимые категории, источник: Ромир",
    x = "%", y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(here("visualizations", "H1_stm_categories_2022_vs_2026.png"), g_cmp,
       width = 9, height = 5, dpi = 150)

# Мотивация выбора СТМ
mot <- motivation %>%
  filter(!is.na(share_pct)) %>%
  mutate(
    factor = fct_reorder(factor, share_pct),
    type = ifelse(factor_type == "Рациональный",
                  "Рациональный (цена, качество, состав)",
                  "Ценностный (семья, безопасность, надёжность)")
  )

g2 <- ggplot(mot, aes(share_pct, factor, fill = type)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%d%%", share_pct)),
            hjust = -0.3, size = 4) +
  scale_fill_manual(values = c(
    "Рациональный (цена, качество, состав)" = "lightblue",
    "Ценностный (семья, безопасность, надёжность)" = "red"
  )) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 100)) +
  labs(
    title = "Мотивация выбора СТМ",
    subtitle = "Источник: Ромир, исследование мотивации, 2026",
    x = "Доля респондентов, %", y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(here("visualizations", "H1_stm_motivation.png"), g2,
       width = 9, height = 5, dpi = 150)

# Классификация категорий: мягкая vs строгая
top10 <- categories %>%
  filter(!is.na(share_pct_june_2026)) %>%
  mutate(
    type_soft = case_when(
      grepl("Молочн|Бакалея|Мясо|Колбас|Кондитер|Консервы|Заморожен|Чай|гигиен|уборк",
            category, ignore.case = TRUE) ~ "Повседневные",
      TRUE ~ "Прочие"
    ),
    type_strict = case_when(
      grepl("Молочн|Бакалея|Мясо|Консервы|Заморожен|Чай|гигиен|уборк",
            category, ignore.case = TRUE) ~ "Повседневные",
      TRUE ~ "Спорные или прочие"
    )
  )

soft_n   <- sum(top10$type_soft == "Повседневные")
strict_n <- sum(top10$type_strict == "Повседневные")

cat(sprintf("Мягкая: %d из %d, строгая: %d из %d\n",
            soft_n, nrow(top10), strict_n, nrow(top10)))

# Динамика доли покупателей СТМ по волнам 2026 г.
trend <- experience %>%
  filter(!is.na(regular_or_sometimes_total_pct)) %>%
  filter(period != "Q1 2026") %>%
  mutate(
    period_label = recode(period,
                          "Январь 2026" = "Январь",
                          "Q2 2026"     = "Апрель (Q2)",
                          "Май 2026"    = "Май",
                          "Июнь 2026"   = "Июнь",
                          "Июль 2026"   = "Июль"
    ),
    month_num = recode(period,
                       "Январь 2026" = 1,
                       "Q2 2026"     = 4,
                       "Май 2026"    = 5,
                       "Июнь 2026"   = 6,
                       "Июль 2026"   = 7
    ),
    period_label = factor(period_label,
                          levels = c("Январь", "Апрель (Q2)", "Май", "Июнь", "Июль"))
  )

g3 <- ggplot(trend, aes(month_num, regular_or_sometimes_total_pct)) +
  geom_line(color = "lightblue", linewidth = 1) +
  geom_point(size = 4, color = "lightblue") +
  geom_text(aes(label = sprintf("%d%%", regular_or_sometimes_total_pct)),
            vjust = -1, size = 4) +
  scale_x_continuous(
    breaks = c(1, 4, 5, 6, 7),
    labels = c("Янв\n2026", "Апр\n(Q2)", "Май", "Июнь", "Июль")
  ) +
  scale_y_continuous(limits = c(55, 65)) +
  labs(
    title = "Доля покупателей СТМ по волнам, 2026",
    subtitle = "Описательная динамика. Регрессия не строится: 5 точек и неоднородность периодов.",
    x = NULL, y = "Доля покупателей, %"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray", size = 10)
  )

ggsave(here("visualizations", "H1_stm_trend.png"), g3,
       width = 8, height = 4.5, dpi = 150)