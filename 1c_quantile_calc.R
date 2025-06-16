library(tidyverse); library(readxl); library(data.table);library(openxlsx); library(stringr); library(rlang)

full_data <- as.data.table(read.csv("data/full_data.csv"), show_col_types = FALSE)

antibiotic_order <- c(
  "rifampicin", "isoniazid", "ethambutol", 
  "clofazimine", "bedaquiline", "linezolid", 
  "amikacin", "capreomycin", "kanamycin",       
  "levofloxacin", "moxifloxacin", "ofloxacin"
)

# Calculate MIC quantiles
country_quantiles <- full_data %>%
  group_by(antibiotic, country) %>%
  summarise(
    Number_of_isolates = n(),
    Q1 = quantile(mic, 0.25, na.rm = TRUE),
    Q2 = quantile(mic, 0.5, na.rm = TRUE),
    Q3 = quantile(mic, 0.75, na.rm = TRUE),
    Inter_quartile_range = IQR(mic, na.rm = TRUE),
    Min_mic = min(mic, na.rm = TRUE),
    Max_mic = max(mic, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(antibiotic = factor(antibiotic, levels = antibiotic_order)) %>%
  arrange(antibiotic, country)

subtype_quantiles <- full_data %>%
  group_by(antibiotic, new_subtype) %>%
  summarise(
    Number_of_isolates = n(),
    Q1 = quantile(mic, 0.25, na.rm = TRUE),
    Q2 = quantile(mic, 0.5, na.rm = TRUE),
    Q3 = quantile(mic, 0.75, na.rm = TRUE),
    Inter_quartile_range = IQR(mic, na.rm = TRUE),
    Min_mic = min(mic, na.rm = TRUE),
    Max_mic = max(mic, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(antibiotic = factor(antibiotic, levels = antibiotic_order)) %>%
  arrange(antibiotic, new_subtype)

year_quantiles <- full_data %>%
  group_by(antibiotic, year) %>%
  filter(year!=2011) %>%
  summarise(
    Number_of_isolates = n(),
    Q1 = quantile(mic, 0.25, na.rm = TRUE),
    Q2 = quantile(mic, 0.5, na.rm = TRUE),
    Q3 = quantile(mic, 0.75, na.rm = TRUE),
    Inter_quartile_range = IQR(mic, na.rm = TRUE),
    Min_mic = min(mic, na.rm = TRUE),
    Max_mic = max(mic, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(antibiotic = factor(antibiotic, levels = antibiotic_order)) %>%
  arrange(antibiotic, year)

write.xlsx(country_quantiles, file="data/country_quantiles.xlsx", sheetName="Countries", rowNames=FALSE)
write.xlsx(subtype_quantiles, file="data/subtype_quantiles.xlsx", sheetName="Subtypes", rowNames=FALSE)
write.xlsx(year_quantiles, file="data/year_quantiles.xlsx", sheetName="Year", append=TRUE, rowNames=FALSE)



