-- ============================================================
-- Аналитические SQL-запросы
-- Проект: Умное потребление в России
-- База: data/smart_consumption.db (SQLite, 24 таблицы)
-- ============================================================


-- ------------------------------------------------------------
-- 1. Динамика покупок СТМ по периодам 2026 года
-- Приём: ORDER BY через CASE, чтобы сохранить хронологию
-- ------------------------------------------------------------
SELECT
    period,
    regular_or_sometimes_total_pct AS share_pct
FROM stm_experience
ORDER BY
    CASE period
        WHEN 'Январь 2026' THEN 1
        WHEN 'Q1 2026'     THEN 2
        WHEN 'Q2 2026'     THEN 3
        WHEN 'Май 2026'    THEN 4
        WHEN 'Июнь 2026'   THEN 5
        WHEN 'Июль 2026'   THEN 6
    END;


-- ------------------------------------------------------------
-- 2. Средний рост по группам категорий (GROUP BY + CASE)
-- Сравнение продовольственных, непродовольственных и услуг
-- ------------------------------------------------------------
SELECT
    CASE
        WHEN category LIKE 'из них:%' THEN 'подкатегории'
        ELSE category
    END AS group_type,
    ROUND(AVG(growth_pct_apr_may_2026), 1) AS avg_growth,
    COUNT(*) AS num_categories
FROM sberindex_categories
GROUP BY group_type
ORDER BY avg_growth DESC;


-- ------------------------------------------------------------
-- 3. Оборот розницы: сравнение 2025 и 2026 по месяцам (JOIN)
-- ------------------------------------------------------------
SELECT
    r25.month,
    r25.value_million_rub AS y2025,
    r26.value_million_rub AS y2026,
    ROUND(
        (r26.value_million_rub - r25.value_million_rub) * 100.0
        / r25.value_million_rub, 1
    ) AS yoy_growth_pct
FROM retail_turnover r25
JOIN retail_turnover r26
    ON r25.month = r26.month
   AND r25.category = r26.category
WHERE r25.year = 2025
  AND r26.year = 2026
  AND r25.category = 'Всего'
ORDER BY r25.month;


-- ------------------------------------------------------------
-- 4. Категории, растущие быстрее среднего по рынку (подзапрос)
-- ------------------------------------------------------------
SELECT
    category,
    growth_pct_apr_may_2026
FROM sberindex_categories
WHERE growth_pct_apr_may_2026 > (
    SELECT AVG(growth_pct_apr_may_2026)
    FROM sberindex_categories
)
ORDER BY growth_pct_apr_may_2026 DESC;


-- ------------------------------------------------------------
-- 5. Динамика финансовой осторожности волна к волне
-- Приём: оконная функция LAG для прироста от волны к волне
-- ------------------------------------------------------------
SELECT
    wave,
    нет_сбережений_нет_кредитов_pct AS no_savings_pct,
    LAG(нет_сбережений_нет_кредитов_pct) OVER (ORDER BY wave)
        AS prev_wave_pct,
    ROUND(
        нет_сбережений_нет_кредитов_pct
        - LAG(нет_сбережений_нет_кредитов_pct) OVER (ORDER BY wave)
    , 1) AS change_pp
FROM hse_savings_credits
WHERE wave LIKE '%20%'
ORDER BY wave;