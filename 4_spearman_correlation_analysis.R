# Calculates Spearman rank correlation coefficients for MIC values for all antibiotics in all countries
# Libraries
library(data.table);library(ggplot2);library(cowplot); library(dplyr); 
library(patchwork); library(tidyr); library(ggthemes)
# Set theme for all plots
theme_set(theme_minimal(base_family="Helvetica", base_size=16))

# This script generates functions for the following: 
# 1) Spearman rank correlation calculation definition
# 2) Define how one country should be processed 
# 3) Main analysis function to process all countries using 1) and 2)
# 4) Categorize correlations into "Negligible", "Weak", "Moderate", "Strong"
# 5) Create a correlation heatmap for each country with category-based colors
# The analysis is then run on the full data and the results are saved to CSV files and plots

# List all antibiotics and countries
antibiotics <- c("bedaquiline","isoniazid","levofloxacin","moxifloxacin","ofloxacin","linezolid",
                 "amikacin","kanamycin","capreomycin","clofazimine","ethambutol","rifampicin")
countries <- c("South Africa", "United States", "Thailand", "Lithuania", "Pakistan","Taiwan",
               "Vietnam","Turkey","Philippines","India","Korea, South", "Global")
# Load in clean data and sort antibiotic order
full_data <- as.data.table(read.csv("data/full_data.csv")) %>%
  mutate(antibiotic = factor(antibiotic, levels = c("bedaquiline","isoniazid","levofloxacin","moxifloxacin",
                                                    "ofloxacin","linezolid","amikacin","kanamycin","capreomycin",
                                                    "clofazimine","ethambutol","rifampicin"))) %>%
  arrange(antibiotic) 

# Handle missing MIC values
full_data$mic[full_data$mic==''] <- NA  
full_data$mic[is.na(full_data$mic)] <- "NA"
full_data <- full_data[!full_data$mic == "NA", ]
   
# 1) Spearman rank correlation function
spearman_correlation <- function(x, y) {
  # Convert to ranks
  rank_x <- rank(x)
  rank_y <- rank(y)
  
  # Calculate correlation using ranks
  n <- length(x)
  mean_rx <- mean(rank_x)
  mean_ry <- mean(rank_y)
  numerator <- sum((rank_x - mean_rx) * (rank_y - mean_ry))
  denominator <- sqrt(sum((rank_x - mean_rx)^2) * sum((rank_y - mean_ry)^2))
  corr <- numerator / denominator
  
  return(corr)
}

# 2) Function to process data and calculate correlations for one country
process_country2 <- function(data, country_name) {
  # Filter data for country and pivot wider
  country_mic <- data %>% 
    mutate(antibiotic = factor(antibiotic, levels = c("bedaquiline","rifampicin","isoniazid",
                                                      "levofloxacin","moxifloxacin","ofloxacin",
                                                      "linezolid","amikacin","kanamycin","capreomycin",
                                                      "clofazimine","ethambutol"))) %>%
    arrange(antibiotic) %>%
    filter(country == country_name) %>%
    select(mic, index, antibiotic, country, who_tb_group) %>%
    pivot_wider(names_from = antibiotic, values_from = mic) %>%
    as.data.frame()
  
  # Get number of isolates and WHO TB group
  n_isolates <- nrow(country_mic)
  
  # Handle missing WHO TB group
  who_tb_group <- unique(data$who_tb_group[data$country == country_name])[1]
  who_tb_group <- if(is.na(who_tb_group)) "No WHO TB group available" else who_tb_group
  
  # Calculate correlation matrix
  n_antibiotics <- length(antibiotics)
  cor_matrix <- matrix(0, nrow = n_antibiotics, ncol = n_antibiotics,
                       dimnames = list(antibiotics, antibiotics))
  
  # Fill correlation matrix
  for (i in 1:n_antibiotics) {
    for (j in 1:n_antibiotics) {
      cor_matrix[i, j] <- spearman_correlation(
        country_mic[, antibiotics[i]],
        country_mic[, antibiotics[j]]
      )
    }
  }
  
  return(list(correlation_matrix = cor_matrix, 
              n_isolates = n_isolates, 
              who_tb_group = who_tb_group))
}

# 3) Main analysis function
analyze_correlations <- function(data_file) {
  # Read data
  full_data <- (data_file)
  
  # Process each country
  correlation_results <- list()
  for (country in countries) {
    message("Processing ", country)
    correlation_results[[country]] <- process_country2(full_data, country)
    
    # Save correlation matrix properly
    write.csv(correlation_results[[country]]$correlation_matrix, 
              file = file.path("plots", paste0(country, "_spearman_correlations.csv")))
  }
  
  return(correlation_results)
}

# 4) Categorize correlations into "Negligible", "Weak", "Moderate", "Strong"
categorize_correlations <- function(results) {
  summary_data <- data.frame()
  
  for (country in names(results)) {
    cor_matrix <- results[[country]]$correlation_matrix
    
    # Get upper triangle correlations only
    upper_tri <- which(upper.tri(cor_matrix), arr.ind = TRUE)
    correlations <- data.frame(
      country = country,
      pair = paste(rownames(cor_matrix)[upper_tri[,1]], "-", 
                   colnames(cor_matrix)[upper_tri[,2]]),
      correlation = cor_matrix[upper.tri(cor_matrix)],
      category = cut(abs(cor_matrix[upper.tri(cor_matrix)]),
                     breaks = c(-Inf, 0.1, 0.4, 0.7, Inf),
                     labels = c("Negligible", "Weak", "Moderate", "Strong"))
    )
    
     summary_data <- rbind(summary_data,
                          correlations)
  }
  
  # Write to single CSV with all results
  write.csv(summary_data, "plots/spearman_correlation_categories.csv", row.names = FALSE)
  
  return(summary_data)
}

# 5) Function to create a correlation heatmap with category-based colors
create_category_heatmap <- function(results, country) {
  cor_matrix <- results[[country]]$correlation_matrix
  n_isolates <- results[[country]]$n_isolates
  who_tb_group <- results[[country]]$who_tb_group
  
  # Create title with conditional WHO TB group display
  title_text <- if(who_tb_group == "No WHO TB group available") {
         paste0(country, " (", n_isolates, " isolates)\n", who_tb_group)
       } else {
         paste0(country, " (", n_isolates, " isolates)\nWHO TB priority group - ", who_tb_group)
       }
  
  # Convert matrix to long format
  cor_long <- as.data.frame(cor_matrix) %>%
    mutate(Var1 = factor(rownames(cor_matrix), levels = antibiotics)) %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
    mutate(Var2 = factor(Var2, levels = antibiotics)) %>%
    mutate(
      Var1_pos = as.numeric(Var1),
      Var2_pos = as.numeric(Var2)) %>%
    filter(Var1_pos > Var2_pos) %>%
    select(-Var1_pos, -Var2_pos) %>%
    # Add category column
    mutate(category = case_when(
      abs(value) >= 0.7 ~ "Strong",
      abs(value) >= 0.4 ~ "Moderate",
      abs(value) >= 0.1 ~ "Weak",
      TRUE ~ "Negligible"
    )) %>%
    # Convert category to factor with specific order
    mutate(category = factor(category, levels = c("Negligible", "Weak", "Moderate", "Strong")))
  
  # Create heatmap with category-based colors
  gg <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = category)) +
    geom_tile(color = "white", lwd = 1, linetype = 1) +
    scale_fill_manual(values = c(
      "Negligible" = "#FBFED1",
      "Weak" = "#D4F2AA",
      "Moderate" = "#9AD2AD",
      "Strong" = "#57AABA"
    )) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
          axis.text.y = element_text(size = 12),
          legend.position = "bottom",
          plot.title = element_text(size = 12)) +
    labs(title = title_text,
         x = "",
         y = "",
         fill = "Correlation\nStrength") +
    geom_text(aes(label = sprintf("%.2f", value),
                  color = category),
              size = 3.4) +
    scale_color_manual(values = c(
      "Negligible" = "azure3",
      "Weak" = "azure4",
      "Moderate" = "azure",
      "Strong" = "azure"
    ), guide = "none")
  
  ggsave(file.path("plots", paste0(country, "_correlation_category_heatmap.png")), 
         plot = gg,
         bg = "white",
         width = 10,
         height = 8)
  
  return(gg)
}

# Run the analysis
results <- analyze_correlations(full_data)
correlation_categories <- categorize_correlations(results)

# Generate plots for each country
category_plots <- list()
for (country in countries) {
  category_plots[[country]] <- create_category_heatmap(results, country)
}

sa_plot <- category_plots[["South Africa"]]
li_plot <- category_plots[["Lithuania"]]
tw_plot <- category_plots[["Taiwan"]]
th_plot <- category_plots[["Thailand"]]
us_plot <- category_plots[["United States"]]
pk_plot <- category_plots[["Pakistan"]]
vn_plot <- category_plots[["Vietnam"]]
tr_plot <- category_plots[["Turkey"]]
ph_plot <- category_plots[["Philippines"]]
in_plot <- category_plots[["India"]]
kr_plot <- category_plots[["Korea, South"]]
gl_plot <- category_plots[["Global"]]

# Combine plots and save
(combined_plot <- sa_plot + li_plot + th_plot + tw_plot + plot_layout(ncol=2, nrow=2,guides="collect", widths = c(1, -1 ,1,-1),heights =c(1, -1 ,1,-1,1,-1,1,-1)) &
    theme(legend.position='bottom') +
  plot_annotation(tag_levels = 'A'))

combined_plot2 <- us_plot + tr_plot + kr_plot + gl_plot + plot_layout(ncol=2, nrow=2,guides="collect", widths = c(1, -1 ,1,-1),heights =c(1, -1 ,1,-1,1,-1,1,-1)) &
  theme(legend.position='bottom')+
  plot_annotation(tag_levels = 'A')

combined_plot3 <- pk_plot + vn_plot + ph_plot + in_plot + plot_layout(ncol=2, nrow=2, guides="collect",widths = c(1, -1 ,1,-1),heights =c(1, -1 ,1,-1,1,-1,1,-1)) &
  theme(legend.position='bottom')+
  plot_annotation(tag_levels = 'A')

ggsave(file.path("plots", "spearman_correlation_heatmaps_combined.png"), plot=combined_plot,
       bg = "white", width = 30, height = 27, units = "cm", dpi=320)

ggsave(file.path("plots", "spearman_correlation_heatmaps_combined2.png"), plot=combined_plot2,
       bg = "white", width = 30, height = 27, units = "cm", dpi=320)

ggsave(file.path("plots", "spearman_correlation_heatmaps_combined3.png"), plot=combined_plot3,
       bg = "white", width = 30, height = 27, units = "cm", dpi=320)

# Create table S5
table_S5 <- correlation_categories %>% 
  group_by(country,pair) %>%
  mutate(index = row_number()) %>%
  select(-category) %>%
  pivot_wider(names_from = country, values_from = correlation) %>%
  na.omit() %>%
  select(-index)

# Save results to CSV
write.csv(correlation_categories, file = file.path("plots", "spearman_correlation_categories.csv"), row.names = FALSE)
write.csv(table_S5, file = file.path("plots", "table_S5.csv"), row.names = FALSE)
