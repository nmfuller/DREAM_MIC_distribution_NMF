#####  Plot generation for histograms of MIC distributions
# Libraries
library(ggplot2); library(tidyverse); library(patchwork); library(ggthemes); 
library(ggridges); library(scales); library(colorBlindness)

# Set theme for all figures
theme_set(theme_minimal(base_family="Helvetica", base_size=10))
# Read in antibiotic family data
ab_fam <- read_csv("data/antibiotic_families.csv", show_col_types = FALSE)

antibiotics <- c("bedaquiline","isoniazid","levofloxacin","moxifloxacin",
                 "ofloxacin","linezolid","amikacin","kanamycin","capreomycin",
                 "clofazimine","ethambutol","rifampicin")

# Load in the clean MIC data, combine with antibiotic family data and set the order for the plots
hist_mic <-  (read.csv("data/full_data.csv"))
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

country_plots <- list()

for (i in antibiotics) {
  
  # Filter data for current antibiotic
  current_data <- hist_mic %>% filter(antibiotic == i)
  
  find_bin_breaks <- hist_mic %>% filter(antibiotic == i) %>%
    select(mic) %>%
    arrange(mic) 
  
  bin_breaks <- unique(current_data$mic)
  
  country_plots[[i]] <-  ggplot(current_data, aes(x=mic,y=fct_rev(country),group=country, fill=country)) +
    geom_density_ridges(alpha=0.6, stat = "binline", breaks=bin_breaks, scale = 1.5, show.legend = FALSE) +
    scale_fill_manual(values = my_palette) +
    geom_vline(data=current_data, aes(xintercept = breakpoint), color = "firebrick1", linetype = "twodash", linewidth = 1.5) +
    labs(x= "MIC value (mg/L)", y="Number of isolates per MIC value", title=i)  +
    scale_y_discrete(labels=yaxis) +
    scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0), limits=c(0.001,100), labels = label_number(accuracy=0.01)) +
    coord_cartesian(clip = "off") +
    theme(axis.text.x= element_text(angle = 45, hjust = 0.5),
          plot.title = element_text(size = 10))
}

rif <- country_plots[["rifampicin"]]
inh <- country_plots[["isoniazid"]]
bdq <- country_plots[["bedaquiline"]]
mox <- country_plots[["moxifloxacin"]]
lev <- country_plots[["levofloxacin"]]
ofx <- country_plots[["ofloxacin"]]
ami <- country_plots[["amikacin"]]
kan <- country_plots[["kanamycin"]]
cap <- country_plots[["capreomycin"]]
clo <- country_plots[["clofazimine"]]
eth <- country_plots[["ethambutol"]]
lin <- country_plots[["linezolid"]]

(country_plot <- (rif + inh + eth + bdq + mox + lev + ofx + lin + ami + kan + cap  + clo) +
  plot_layout(ncol=4, nrow=3, guides = "collect", axes="collect"))

ggsave(paste("plots/fig1.jpeg"), plot = country_plot, width = 18, height = 24, units = "cm", dpi = 300, bg = 'white')

  ###### Year #####
year_plots <- list()

for (i in antibiotics) {
  
  # Filter data for current antibiotic
  current_data <- hist_mic %>% filter(antibiotic == i)
  
  find_bin_breaks <- hist_mic %>% filter(antibiotic == i) %>%
    select(mic) %>%
    arrange(mic) 
  
  bin_breaks <- unique(current_data$mic)
  
  year_plots[[i]] <-  ggplot(current_data, aes(x=mic,y=as.factor(year),group=as.factor(year), fill=as.factor(year))) +
    geom_density_ridges(alpha=0.6, stat = "binline", breaks=bin_breaks, scale = 1.5, show.legend = FALSE) +
    scale_fill_tableau(palette="Tableau 10") +
    geom_vline(data=current_data, aes(xintercept = breakpoint), color = "darkgrey", linetype = "twodash", linewidth = 1) +
    labs(x= "MIC value (mg/L)",
        y='Number of isolates per MIC value')  +
    scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0), limits=c(0.001,100), labels = label_number(accuracy=0.01)) +
    coord_cartesian(clip = "off") +
    theme(axis.text.x= element_text(angle = 45, hjust = 0.5))
}

rif2 <- year_plots[["rifampicin"]]
inh2 <- year_plots[["isoniazid"]]
bdq2 <- year_plots[["bedaquiline"]]
mox2 <- year_plots[["moxifloxacin"]]
lev2 <- year_plots[["levofloxacin"]]
ofx2 <- year_plots[["ofloxacin"]]
ami2 <- year_plots[["amikacin"]]
kan2 <- year_plots[["kanamycin"]]
cap2 <- year_plots[["capreomycin"]]
clo2 <- year_plots[["clofazimine"]]
eth2 <- year_plots[["ethambutol"]]
lin2 <- year_plots[["linezolid"]]

# Load in the year cumulative data, combine with antibiotic family data and set the order for the plots
year_mic <- read_csv(paste0("plots/" ,"yearoutput.csv"), show_col_types = FALSE)
year_mic <- merge(year_mic, ab_fam, by="antibiotic")
year_mic <- year_mic %>%
  arrange(family,antibiotic)

label7 <- c("2019" = "2019 (753)", "2018" = "2018 (1190)",
            "2017" = "2017 (1543)", "2016" = "2016 (1245)",
            "2015" = "2015 (926)", "2014" = "2014 (143)",
            "2013" = "2013 (126)", "2011" = "2011 (2)")

year_plots2 <- list()

for (i in antibiotics) {
  current_data <- year_mic %>% filter(antibiotic == i)
  
 year_plots2[[i]] <- ggplot(current_data, aes(x=MIC,y=cumulative_sum,group=as.factor(year), color=as.factor(year))) +
  geom_line()+
  scale_color_tableau(palette="Tableau 10", type="regular", labels=label7) +
  geom_vline(data=current_data, aes(xintercept = breakpoint), color = "darkgrey", linetype = "twodash", linewidth = 1) +
  labs(x= "MIC value (mg/L)",
       y='Cumulative proportion',
       color="Year",
       shape="Year",
       title=i)  +
   scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0), limits=c(0.001,100), labels = label_number(accuracy=0.01)) +
   scale_y_continuous(limits = c(0,1)) +
  theme(axis.text.x= element_text(angle = 45, hjust = 0.5),
        plot.title = element_text(size = 14)) +
  geom_point(data = current_data, aes(x = MIC, y = cumulative_sum, colour = as.factor(year),group=as.factor(year), shape=as.factor(year)), size = 1) +
  scale_shape_manual(values = c(1, 16, 1, 16,1,16,1,16), labels=label7)
 
}

rif3 <- year_plots2[["rifampicin"]]
inh3 <- year_plots2[["isoniazid"]]
bdq3 <- year_plots2[["bedaquiline"]]
mox3 <- year_plots2[["moxifloxacin"]]
lev3 <- year_plots2[["levofloxacin"]]
ofx3 <- year_plots2[["ofloxacin"]]
ami3 <- year_plots2[["amikacin"]]
kan3 <- year_plots2[["kanamycin"]]
cap3 <- year_plots2[["capreomycin"]]
clo3 <- year_plots2[["clofazimine"]]
eth3 <- year_plots2[["ethambutol"]]
lin3 <- year_plots2[["linezolid"]]

## Combine year plots
(year_plot <-  rif3 + inh3 +  eth3 + bdq3 +
    rif2 + inh2 + eth2 + bdq2 +
    mox3 + lev3 + ofx3 +lin3 +
    mox2 + lev2 + ofx2 + lin2 +
    ami3 + kan3 + cap3 + clo3 +
    ami2 + kan2 + cap2 + clo2 +
    plot_layout(ncol=4, nrow=6, guides = "collect", axes="collect") &
    theme(legend.position = "bottom", legend.box ="vertical" ) )

ggsave(paste("plots/year_plot.pdf"), plot = year_plot, width = 18, height = 33, units = "cm", dpi = 300, bg = 'white')
