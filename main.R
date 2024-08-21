# Package ----
library(dplyr)
library(readxl)
library(ggplot2)

# Data ----
raw <- read_xls("data_raw/raw_data_20240816.xls") %>%
  rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%
  rename("fu" = "fu\n(functional_unit)")
names(raw)
str(raw)

# Analysis ----
# 统计数据各类的种类和分布。
get_freq_col_val <- function(col_x) {
  res_mid <- table(raw[[col_x]]) %>%
    sort(decreasing = TRUE)
  plot(res_mid, xaxt = "n", ylab = col_x)

  res_mid %>%
    data.frame() %>%
    rename_with(~ c(col_x, "freq"))
}
# 统计rice system的种类和分布。
get_freq_col_val("rice_system")

# 统计intervention的种类和分布。
get_freq_col_val("intervention")

# 统计impact_categories的种类和分布。
get_freq_col_val("impact_categories")

# 统计研究地区的种类和分布。
get_freq_col_val("country")

# 提取数据。
raw_gwp100 <- raw %>%
  filter(
    impact_categories == "GWP100",
    fu == "1 ton of rice" | fu == "1 kg of rice"
  )
# 看看不同系统下的GWP100。
raw_gwp100 %>%
  mutate(
    rice_system = paste0(substr(rice_system, 1, 15), "...")
  ) %>%
  ggplot(aes(rice_system, results)) +
  geom_boxplot() +
  geom_jitter(aes(col = intervention), alpha = 0.5) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "none"
  )
# 看看不同干预下的GWP100。
ggplot(raw_gwp100, aes(intervention, results)) +
  geom_boxplot() +
  geom_jitter(aes(col = rice_system), alpha = 0.5) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "none"
  )
