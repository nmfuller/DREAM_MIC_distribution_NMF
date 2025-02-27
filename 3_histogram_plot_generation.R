#####  Plot generation for histograms of MIC distributions
# Libraries
library(ggplot2); library(tidyverse); library(patchwork); library(ggthemes); 
library(ggridges); library(scales); library(colorBlindness)

# Set theme for all figures
theme_set(theme_minimal(base_family="Times", base_size=16))
# Read in antibiotic family data
ab_fam <- read_csv("data/antibiotic_families.csv", show_col_types = FALSE)

# Load in the clean MIC data, combine with antibiotic family data and set the order for the plots
hist_mic <-  as.data.table(read.csv("data/full_data.csv"))
hist_mic <- merge(hist_mic, ab_fam, by="antibiotic") 
hist_mic <- hist_mic %>%
  arrange(family,antibiotic)
hist_mic <- hist_mic %>% mutate(country=factor(country, levels = c("Global","India","Lithuania","Pakistan",
                                                                   "Philippines","South Africa","Korea, South",
                                                                   "Taiwan","Thailand","Turkey","United States","Vietnam")))
# Create a folder to save the plots
dir.create("plots", recursive = TRUE, showWarnings = FALSE)

###### Country figure ######
yaxis <- c("South Africa" = "SA", "Pakistan" = "PA", "Thailand" = "TH",
           "Taiwan" = "TA", "Lithuania" = "LI", "Vietnam" = "VN",
           "Turkey" = "TK", "Philippines" = "PH", "India" = "ID",
           "United States" = "US","Korea, South"= "SK", "Global"="GL")

my_palette <- c("#000000", "seagreen4", "lightseagreen", "#ff6db6", "#ffb6db","dodgerblue", "#b6dbff",
                "darkorange2","orange", "darkmagenta", "orchid",  "khaki")

(country_plot <-  ggplot(hist_mic, aes(x=mic,y=fct_rev(country),group=country, fill=country)) +
    geom_density_ridges(alpha=0.6, stat = "binline", bins=13, scale = 1.5, show.legend = FALSE) +
    scale_fill_manual(values = my_palette) +
    geom_vline(data=hist_mic, aes(xintercept = breakpoint*0.65), color = "firebrick1", linetype = "twodash", linewidth = 1.5) +
    labs(x= "MIC value (mg/l)", y="")  +
    scale_y_discrete(labels=yaxis) +
    scale_x_log10(labels = label_number(accuracy=0.01)) +
    facet_wrap(.~fct_inorder(antibiotic), ncol=4))

ggsave(paste("plots/country_plot_hist.tiff"), plot = country_plot, width = 30, height = 37, units = "cm", dpi = 320, bg = 'white')

###### Year #####
(year_plot1 <-  ggplot(hist_mic, aes(x=mic,y=as.factor(year),group=as.factor(year), fill=as.factor(year))) +
   geom_density_ridges(alpha=0.75, stat = "binline", bins=13, scale =2, show.legend = FALSE) +
   scale_fill_tableau(palette="Tableau 10") +
   geom_vline(data=hist_mic, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
   labs(x= "MIC value (mg/l)",
        y='')  +
   scale_x_log10(labels = label_number(accuracy=0.01)) +
   facet_wrap(.~fct_inorder(antibiotic), ncol=4))

# Load in the year cumulative data, combine with antibiotic family data and set the order for the plots
year_mic <- read_csv(paste0("plots/" ,"yearoutput.csv"), show_col_types = FALSE)
year_mic <- merge(year_mic, ab_fam, by="antibiotic")
year_mic <- year_mic %>%
  arrange(family,antibiotic)

label7 <- c("2019" = "2019 (753)", "2018" = "2018 (1190)",
            "2017" = "2017 (1543)", "2016" = "2016 (1245)",
            "2015" = "2015 (926)", "2014" = "2014 (143)",
            "2013" = "2013 (126)", "2011" = "2011 (2)")

year_plot2 <-  ggplot(year_mic, aes(x=MIC,y=cumulative_sum,group=as.factor(year), color=as.factor(year))) +
  geom_line()+
  scale_color_tableau(palette="Tableau 10", type="regular", labels=label7) +
  geom_vline(data=year_mic, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
  labs(x= "MIC value (mg/l)",
       y='Cumulative proportion of isolates by Year',
       color="Year",
       shape="Year")  +
  scale_x_log10() +
  facet_wrap(.~fct_inorder(antibiotic), ncol=4) +
  theme(legend.position = "bottom") +
  geom_point(data = year_mic, aes(x = MIC, y = cumulative_sum, colour = as.factor(year),group=as.factor(year), shape=as.factor(year)), size = 1) +
  scale_shape_manual(values = c(1, 16, 1, 16,1,16,1,16), labels=label7)

## Combine year plots
hist_combo1 <- year_plot1 / year_plot2 + plot_layout(heights = c(5,7)) +  plot_annotation(tag_levels = 'A')

ggsave(paste("plots/hist_combo1.tiff"), plot = hist_combo1, width = 25, height = 35, units = "cm", dpi = 320, bg = 'white')
