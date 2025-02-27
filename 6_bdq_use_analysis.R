# Perform bedaquiline use analysis
# Libraries
library(ggplot2); library(tidyr); library(ggthemes); library(dplyr); 
library(RColorBrewer);library(openxlsx); library(tidyverse); library(patchwork)
# Set theme for all plots
theme_set(theme_minimal(base_family="Times", base_size=16))

colors <- c("South Africa" = "#000000", "Pakistan" = "#E69F00","Thailand" = "#56B4E9",        
            "Lithuania" = "#009E73","Vietnam" = "#F0E442","Turkey" = "#0072B2",         
            "Philippines" = "#D55E00","India" = "#CC79A7","United States" = "#882255")  

# Load in clean MIC data
full_data <- (read.csv("data/full_data.csv"))
# Load in WHO bedaquiline use data
who_bdq <- read.xlsx("data/who_bdq_use.xlsx")
# Prep data for use 
colnames(who_bdq) <- c("year","rr_conf","bdq_treat","rr_on_bdq","bdq_used","country")
who_bdq <- who_bdq%>% na.omit()
# select bedaquiline only
country_bdq <- full_data%>%
  filter(antibiotic=="bedaquiline") 
# Combine MIC data with WHO bedaquiline use data
plot_data <- merge(country_bdq, who_bdq, by=c("country", "year")) %>%
  group_by(country,year) %>%
  mutate(average_mic = mean(mic))
# Calculate mean, sd, median, n, se, ci
plot_data2 <- country_bdq %>%
  group_by(country,year) %>%
  summarize(
  mean_mic = mean(mic),
  sd_mic = sd(mic),
  median_mic = median(mic),
  n = n(),
  se = sd_mic / sqrt(n),
  ci = se * qt(0.95, df = n - 1)
) %>%
  merge( who_bdq, by=c("country", "year")) 

# Perform linear regression
dd <- lm(mean_mic ~ rr_on_bdq + year, data = plot_data2)
summary(dd)
summary(dd)$r.squared

# Plot bedaquiline use 
(rr <-ggplot(plot_data2, aes(x=rr_on_bdq,y=mean_mic)) +
    geom_point(aes(color=country, shape=as.factor(country)), size=3) +
    scale_color_tableau(palette = "Jewel Bright") +
    scale_shape_manual(values = c(1,16,1,16,1,16,1,16,1)) +
    labs( x = "Percentage of previous MDR/RR-TB patients treated with a regimen containing bedaquiline", 
          y = "MIC of bedaquiline isolates per year (mg/l)",
          color ="Country",
          shape="Country") +
    annotate("text", x = 1.1, y = 0.075, label =  bquote(R^2 == .(0.21)), color = "#808080", size = 3) +
    geom_errorbar(aes(ymin =mean_mic - sd_mic ,ymax = mean_mic + sd_mic, color=country, 
                  width = 0.01),linewidth  = 0.75) +
    geom_smooth(method = "lm", color="lightgrey",fill="lightgrey",alpha=0.3 ) +
    geom_hline(yintercept = 0.25, linetype="dashed", color = "#ff1493"))

# Plot bedaquiline use split by country
(rr2 <-ggplot(plot_data2, aes(x=rr_on_bdq,y=mean_mic)) +
    geom_point(aes(color=as.factor(year), shape=as.factor(year))) +
    scale_color_tableau(palette = "Nuriel Stone")  +
    facet_wrap(~country, ncol=3 ) +
    labs(x = "Percentage of previous MDR/RR-TB patients treated with a regimen containing bedaquiline", 
         y = "MIC of bedaquiline isolates per year (mg/l)",
         color ="Year",
         shape="Year") +
    geom_errorbar(aes(ymin = mean_mic - sd_mic,ymax = mean_mic + sd_mic, color=as.factor(year), 
                      width = 0.02)) +
    geom_smooth(method = "lm", color="lightgrey",fill="lightgrey",alpha=0.3 ) +
    geom_hline(yintercept = 0.25, linetype="dashed", color = "#ff1493")+
    scale_y_continuous(limits=c(-0.1,0.25)) )

# Save plots
ggsave(paste("plots/bdq_use_all.tiff"), plot = rr,  units = "cm", width = 25, height = 20, dpi = 320, bg = 'white')
ggsave(paste("plots/bdq_use_all2.tiff"), plot = rr2, width = 25, height = 20, units = "cm", dpi = 320, bg = 'white')
