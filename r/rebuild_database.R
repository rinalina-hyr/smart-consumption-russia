# ============================================
# Пересборка smart_consumption.db из обновлённого
# умное_потребление_данные.xlsx
# ============================================
#
# ВАЖНО. Две таблицы в текущей базе НЕ имеют
# соответствующего листа в Excel: sberpro_trends
# и theory_formulas. Они занесены в базу вручную
# из PDF/веб-источников. Этот скрипт их НЕ трогает —
# ни не читает из Excel, ни не удаляет из базы.
# Если тебе нужно их изменить — делай это отдельным
# UPDATE-запросом, не через этот скрипт.
#
# Всё остальное — таблица-к-листу, с перезаписью.
# ============================================

pacman::p_load(readxl, DBI, RSQLite, dplyr, here)

xlsx_path <- here("data", "raw", "умное_потребление_данные.xlsx")
db_path   <- here("data", "smart_consumption.db")

# ============================================
# Карта: имя листа Excel -> имя таблицы в БД
# ============================================
# Пары "старое" сохранены как есть (совместимость с H1.R/H2.R/H3.R/
# exploration_categories.R — они обращаются к таблицам по этим именам).
# Новые листы получили новые snake_case имена.

sheet_to_table <- c(
  "Chizhik_ВШЭ_Индекс"        = "chizhik_index",
  "Chizhik_ВШЭ_Паттерны"      = "chizhik_patterns",
  "Chizhik_ВШЭ_Сегменты"      = "chizhik_segments",
  "Romir_ИПУ"                 = "consumer_confidence",
  "Флаги"                     = "data_flags",
  "Демография_Сегменты"       = "demographics",
  "Домохозяйства_Профили"     = "household_profiles",
  "HSE_Портрет_Потребителя"   = "hse_consumer_portrait",
  "HSE_Защита_Прав"           = "hse_consumer_rights",
  "HSE_Макро_Контекст"        = "hse_macro_context",
  "HSE_Сбережения_Кредиты"    = "hse_savings_credits",
  "NoBuy_Опрос"                = "nobuy_survey",
  "Rosstat_Оборот_Розницы"    = "retail_turnover",
  "Rosstat_Интернет_Торговля" = "rosstat_online",
  "Rosstat_Структура_Расходов" = "rosstat_spending",
  "СберИндекс_Категории"      = "sberindex_categories",
  "СберИндекс_Динамика"       = "sberindex_dynamics",
  "СберИндекс_Офлайн"         = "sberindex_offline",
  "STM_Топ_Категорий"         = "stm_categories",
  "STM_Опыт_Покупок"          = "stm_experience",
  "STM_Мотивация"             = "stm_motivation",
  "Romir_Кошелек"             = "wallet_metrics",
  
  # --- новые листы, которых раньше не было в базе ---
  "STM_Мотивация_2022"        = "stm_motivation_2022",
  "FMCG_Инфляция"             = "fmcg_inflation",
  "ИПУ_Ромир_Динамика"        = "romir_confidence_dynamics",
  "STM_Доля_Рынка"            = "stm_market_share",
  "NielsenIQ_Тренды_2024"     = "nielseniq_trends_2024",
  "HSE_Защита_Прав_Волны"     = "hse_consumer_rights_waves"
  
  # Лист "Источники" намеренно не импортируется как таблица —
  # это документация, а не данные для анализа. Если нужна
  # в базе — добавь вручную пару "Источники" = "sources".
)

# ============================================
# Проверка: какие листы Excel есть, но не в карте
# (чтобы не забыть новый лист в будущем)
# ============================================

actual_sheets <- excel_sheets(xlsx_path)
mapped_sheets <- names(sheet_to_table)

unmapped <- setdiff(actual_sheets, c(mapped_sheets, "Источники"))
if (length(unmapped) > 0) {
  cat("ВНИМАНИЕ: в Excel есть листы, не описанные в sheet_to_table:\n")
  cat(paste(" -", unmapped, collapse = "\n"), "\n\n")
  cat("Они НЕ будут импортированы. Добавь их в карту вручную.\n\n")
}

missing_in_xlsx <- setdiff(mapped_sheets, actual_sheets)
if (length(missing_in_xlsx) > 0) {
  cat("ВНИМАНИЕ: в карте есть листы, которых нет в Excel:\n")
  cat(paste(" -", missing_in_xlsx, collapse = "\n"), "\n\n")
  stop("Останавливаюсь — проверь названия листов перед пересборкой.")
}

# ============================================
# Пересборка: читаем каждый лист, пишем в таблицу
# ============================================

con <- dbConnect(SQLite(), db_path)

# Таблицы, которые НИКОГДА не трогаем этим скриптом,
# т.к. у них нет исходного листа в Excel
protected_tables <- c("sberpro_trends", "theory_formulas")

cat("Защищённые таблицы (не тронуты):\n")
cat(paste(" -", protected_tables, collapse = "\n"), "\n\n")

for (sheet in mapped_sheets) {
  table_name <- sheet_to_table[[sheet]]
  
  if (table_name %in% protected_tables) {
    # страховка на случай опечатки в карте выше
    warning(sprintf("Пропускаю %s — защищённая таблица.", table_name))
    next
  }
  
  df <- read_excel(xlsx_path, sheet = sheet)
  # clean_names() убран: он транслитерирует кириллические
  # заголовки в латиницу (есть_... -> est_...), а H1.R/H2.R/H3.R
  # обращаются к столбцам по точным кириллическим именам.
  
  dbWriteTable(con, table_name, df, overwrite = TRUE)
  
  cat(sprintf("  %-32s <- %-30s (%d строк, %d колонок)\n",
              table_name, sheet, nrow(df), ncol(df)))
}

# ============================================
# Итог: сверка со старым списком таблиц
# ============================================

all_tables <- dbListTables(con)
cat(sprintf("\nВсего таблиц в базе после пересборки: %d\n", length(all_tables)))
cat("Список:\n")
cat(paste(" -", sort(all_tables), collapse = "\n"), "\n")

dbDisconnect(con)
con <- dbConnect(SQLite(), here("data", "smart_consumption.db"))
names(dbGetQuery(con, "SELECT * FROM hse_savings_credits"))
dbDisconnect(con)

cat("\nГотово. Проверь вывод выше:\n")
cat("  1. Не появилось ли предупреждение про 'неописанные листы'.\n")
cat("  2. Совпадает ли число строк с ожидаемым (сверь пару таблиц вручную).\n")
cat("  3. sberpro_trends и theory_formulas должны остаться как были —\n")
cat("     запусти SELECT * FROM sberpro_trends в SQLiteBrowser и сверь.\n")