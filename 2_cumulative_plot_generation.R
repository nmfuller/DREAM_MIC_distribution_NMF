#####  Plot generation for cumulative proportion graphs 
# Libraries
library(ggplot2); library(tidyverse); library(patchwork); library(ggthemes)

# Set theme for all figures
theme_set(theme_minimal(base_family="Helvetica", base_size=16))

# Read in antibiotic family data
ab_fam <- read_csv("data/antibiotic_families.csv", show_col_types = FALSE)

antibiotics <- c("bedaquiline","isoniazid","levofloxacin","moxifloxacin",
                 "ofloxacin","linezolid","amikacin","kanamycin","capreomycin",
                 "clofazimine","ethambutol","rifampicin")
# The code in this script follows the structure of loading in the grouping relevant data,
# Combining with the antibiotic family data, setting the order and colours for the plots,
# then generating the plots for the cumulative proportion of isolates by different characteristics.
# Plots are then save in the plots folder.

###### Country figure ######
country_mic <- read_csv(paste0("plots/" ,"countryoutput.csv"), show_col_types = FALSE)
country_mic <- merge(country_mic, ab_fam, by="antibiotic") 
country_mic <- country_mic %>%
  arrange(family,antibiotic)
country_mic <- country_mic %>% mutate(country2=factor(country, 
               levels = c("Global","India","Lithuania","Pakistan","Philippines","South Africa",
                          "Korea, South","Taiwan","Thailand","Turkey","United States","Vietnam")))

colors <- c("South Africa" = "#FF7F0F", "Pakistan" = "#BCBD21", "Thailand" = "#cc00cc",
            "Taiwan" = "#D53A3A", "Lithuania" = "#2BA030", "Vietnam" = "#666699",
            "Turkey" = "#5c00e6", "Philippines" = "#FFBF50", "India" = "#1E83B4",
            "United States" = "#660066","Korea, South"= "#0FA2A7", "Global"="black")

label <- c("South Africa" = "South Africa (978)", "Pakistan" = "Pakistan (899)", "Thailand" = "Thailand (668)",
           "Taiwan" = "Taiwan (874)", "Lithuania" = "Lithuania (616)", "Vietnam" = "Vietnam (344)",
           "Turkey" = "Turkey (180)", "Philippines" = "Philippines (366)", "India" = "India (401)",
           "United States" = "United States (583)","Korea, South"= "South Korea (19)", "Global"="Global (5928)")

country_plots <- list()

for (i in antibiotics) {
  current_data <- country_mic %>% filter(antibiotic == i)

  country_plots[[i]] <-  ggplot(current_data, aes(x=MIC,y=cumulative_sum,group=country2, color=country2)) +
                  geom_line() +
                  scale_color_manual(values=colors, labels=label) +
                  geom_vline(data=current_data, aes(xintercept = breakpoint), color = "darkgrey", linetype = "twodash", linewidth = 1) +
                  labs(x= "MIC value (mg/L)",
                       y='Cumulative proportion of isolates by country',
                       color ="Country (# of isolates)",
                       shape="Country (# of isolates)",
                       title=i)  +
                  scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0), limits=c(0.001,100), labels = label_number(accuracy=0.01)) +
                  scale_y_continuous(limits = c(0,1)) +
                  theme(axis.text.x= element_text(angle = 45, hjust = 0.5),
                        plot.title = element_text(size = 14)) +
                  geom_point(data = current_data, aes(x = MIC, y = cumulative_sum, colour = country2,group=country2, shape=country2), size = 1) +
                  scale_shape_manual(values = c(8, 1, 16, 1, 16, 1, 16, 1, 16, 1, 16,1), labels=label)
  
}

rif_country <- country_plots[["rifampicin"]]; inh_country <- country_plots[["isoniazid"]]
lev_country <- country_plots[["levofloxacin"]]; mox_country <- country_plots[["moxifloxacin"]]
ofl_country <- country_plots[["ofloxacin"]]; lin_country <- country_plots[["linezolid"]]
ami_country <- country_plots[["amikacin"]]; kan_country <- country_plots[["kanamycin"]]
cap_country <- country_plots[["capreomycin"]]; clo_country <- country_plots[["clofazimine"]]
emb_country <- country_plots[["ethambutol"]]; bdq_country <- country_plots[["bedaquiline"]]

###### WHO region figure ######
whoreg_mic <- read_csv(paste0("plots/" ,"who_regionoutput.csv"), show_col_types = FALSE)
whoreg_mic <- merge(whoreg_mic, ab_fam, by="antibiotic")
whoreg_mic <- whoreg_mic %>%
  arrange(family,antibiotic)

colors3 <- c("Africa" = "#FF7F0F", "Americas" = "#6F62BB", "Eastern Mediterranean" = "#BCBD21",
            "Europe" = "#2BA030", "South-East Asia" = "#C7519C", "Western Pacific" = "#0FA2A7",
            "Global" = "black")

label3 <- c("Africa" = "Africa (978)", "Americas" = "Americas (583)", "Eastern Mediterranean" = "Eastern Mediterranean (899)",
           "Europe" = "Europe (796)", "South-East Asia" = "South-East Asia (1069)", "Western Pacific" = "Western Pacific (1603)",
           "Global" = "Global (5928)")

who_plots <- list()

for (i in antibiotics) {
  current_data <- whoreg_mic %>% filter(antibiotic == i)

  who_plots[[i]] <-  ggplot(current_data, aes(x=MIC,y=cumulative_sum,group=who_region, color=who_region)) +
  geom_line()+
  scale_color_manual(values=colors3, labels=label3) +
  geom_vline(data=current_data, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
  labs(x= "MIC value (mg/L)",
       y='Cumulative proportion of isolates by WHO Region',
       color="WHO Region (# of isolates)",
       shape="WHO Region (# of isolates)",
       title=i)  +
    scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0), limits=c(0.001,100), labels = label_number(accuracy=0.01)) +
    theme(axis.text.x= element_text(angle = 45, hjust = 0.5),
          plot.title = element_text(size = 14)) +
    scale_y_continuous(limits = c(0,1)) +
  geom_point(data = current_data, aes(x = MIC, y = cumulative_sum, colour = who_region,group=who_region, shape=who_region), size = 1) +
  scale_shape_manual(values = c(1, 16, 1, 16,1,16,1), labels=label3) 
  
}

rif_who <- who_plots[["rifampicin"]]; inh_who <- who_plots[["isoniazid"]]
lev_who <- who_plots[["levofloxacin"]]; mox_who <- who_plots[["moxifloxacin"]]
ofl_who <- who_plots[["ofloxacin"]]; lin_who <- who_plots[["linezolid"]]
ami_who <- who_plots[["amikacin"]]; kan_who <- who_plots[["kanamycin"]]
cap_who <- who_plots[["capreomycin"]]; clo_who <- who_plots[["clofazimine"]]
emb_who <- who_plots[["ethambutol"]]; bdq_who <- who_plots[["bedaquiline"]]

###### WHO TB Priority Group figure ######
whotbgroup_mic <- read_csv(paste0("plots/" ,"who_tb_groupoutput.csv"), show_col_types = FALSE)
whotbgroup_mic <- (whotbgroup_mic) %>% drop_na(who_tb_group)
whotbgroup_mic <- merge(whotbgroup_mic, ab_fam, by="antibiotic")
whotbgroup_mic <- whotbgroup_mic %>%
  arrange(family,antibiotic)

colors5 <- c("All" = "#D53A3A", "None" = "#1E83B4", "TB+HIV" = "#C7519C",
            "TB+MDR/RR" = "#0FA2A7")
label5 <- c("All" = "All (1745)", "None" = "None (1398)", "TB+HIV" = "TB+HIV (668)",
            "TB+MDR/RR" = "TB+MDR/RR (1243)")

tbgroup_plots <- list()

for (i in antibiotics) {
  current_data <- whotbgroup_mic %>% filter(antibiotic == i)

  tbgroup_plots[[i]] <-  ggplot(current_data, aes(x=MIC,y=cumulative_sum,group=who_tb_group, color=who_tb_group)) +
  geom_line()+
  scale_color_manual(values=colors5, labels=label5) +
  geom_vline(data=current_data, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
  labs(x= "MIC value (mg/L)",
       y='Cumulative proportion of isolates by WHO TB Priority Group',
       color="WHO TB Priority Group\n(# of isolates)",
       shape="WHO TB Priority Group\n(# of isolates)",
       title=i)  +
    scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0), limits=c(0.001,100), labels = label_number(accuracy=0.01)) +
    theme(axis.text.x= element_text(angle = 45, hjust = 0.5),
          plot.title = element_text(size = 14)) +
    scale_y_continuous(limits = c(0,1)) +
  geom_point(data = current_data, aes(x = MIC, y = cumulative_sum, colour = who_tb_group,group=who_tb_group, shape=who_tb_group), size = 1) +
  scale_shape_manual(values = c(1, 16,1,16), labels=label5) 
}

rif_tbgroup <- tbgroup_plots[["rifampicin"]]; inh_tbgroup <- tbgroup_plots[["isoniazid"]]
lev_tbgroup <- tbgroup_plots[["levofloxacin"]]; mox_tbgroup <- tbgroup_plots[["moxifloxacin"]]
ofl_tbgroup <- tbgroup_plots[["ofloxacin"]]; lin_tbgroup <- tbgroup_plots[["linezolid"]]
ami_tbgroup <- tbgroup_plots[["amikacin"]]; kan_tbgroup <- tbgroup_plots[["kanamycin"]]
cap_tbgroup <- tbgroup_plots[["capreomycin"]]; clo_tbgroup <- tbgroup_plots[["clofazimine"]]
emb_tbgroup <- tbgroup_plots[["ethambutol"]]; bdq_tbgroup <- tbgroup_plots[["bedaquiline"]]

###### Income group #####
income_grp_mic <- read_csv(paste0("plots/" ,"income_grpoutput.csv"), show_col_types = FALSE)
income_grp_mic <- merge(income_grp_mic, ab_fam, by="antibiotic")
income_grp_mic <- income_grp_mic %>%
  arrange(family,antibiotic) %>%
  filter(income_grp %in% c("Lower middle income","Upper middle income","High income"))


colors6 <- c("Lower middle income" = "#FFBF50", "Upper middle income" = "#C7519C",
            "High income" = "#6F62BB")
label6 <- c("Lower middle income" = "Lower middle income (2010)", "Upper middle income" = "Upper middle income (1826)",
            "High income" = "High income (2092)")

income_grp_plots <- list()

for (i in antibiotics) {
  current_data <- income_grp_mic %>% filter(antibiotic == i)
  
  income_grp_plots[[i]] <-  ggplot(current_data, aes(x=MIC,y=cumulative_sum,group=income_grp, color=income_grp)) +
  geom_line()+
  scale_color_manual(values=colors6, labels=label6) +
  geom_vline(data=current_data, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
  labs(x= "MIC value (mg/L)",
       y='Cumulative proportion of isolates by Income Group',
       color="Income Group (# of isolates)",
       shape="Income Group (# of isolates)",
       title=i)  +
    scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0), limits=c(0.001,100), labels = label_number(accuracy=0.01)) +
    theme(axis.text.x= element_text(angle = 45, hjust = 0.5),
          plot.title = element_text(size = 14)) +
    scale_y_continuous(limits = c(0,1)) +
  geom_point(data = current_data, aes(x = MIC, y = cumulative_sum, colour = income_grp,group=income_grp, shape=income_grp), size = 1) +
  scale_shape_manual(values = c(1, 16, 1, 16,1), labels=label6) 
}

rif_income_grp <- income_grp_plots[["rifampicin"]]; inh_income_grp <- income_grp_plots[["isoniazid"]]
lev_income_grp <- income_grp_plots[["levofloxacin"]]; mox_income_grp <- income_grp_plots[["moxifloxacin"]]
ofl_income_grp <- income_grp_plots[["ofloxacin"]]; lin_income_grp <- income_grp_plots[["linezolid"]]
ami_income_grp <- income_grp_plots[["amikacin"]]; kan_income_grp <- income_grp_plots[["kanamycin"]]
cap_income_grp <- income_grp_plots[["capreomycin"]]; clo_income_grp <- income_grp_plots[["clofazimine"]]
emb_income_grp <- income_grp_plots[["ethambutol"]]; bdq_income_grp <- income_grp_plots[["bedaquiline"]]

##### Combine plots #####
cum_combo1 <- rif_country + inh_country + emb_country +  bdq_country +
              lev_country + mox_country + ofl_country + lin_country + 
              ami_country + kan_country + cap_country + clo_country + 
              rif_income_grp + inh_income_grp + emb_income_grp + bdq_income_grp +
              lev_income_grp + mox_income_grp + ofl_income_grp + lin_income_grp +
              ami_income_grp + kan_income_grp + cap_income_grp + clo_income_grp +
              plot_layout(ncol = 4, nrow = 6, guides = "collect", axes = "collect") 
  
  
cum_combo2 <- rif_who + inh_who + emb_who +  bdq_who +
              lev_who + mox_who + ofl_who + lin_who + 
              ami_who + kan_who + cap_who + clo_who + 
              rif_tbgroup + inh_tbgroup + emb_tbgroup + bdq_tbgroup +
              lev_tbgroup + mox_tbgroup + ofl_tbgroup + lin_tbgroup +
              ami_tbgroup + kan_tbgroup + cap_tbgroup + clo_tbgroup +
              plot_layout(ncol = 4, nrow = 6, guides = "collect", axes = "collect")

# Create folder to store plots
dir.create("plots", recursive = TRUE, showWarnings = FALSE)

ggsave(paste("plots/cum_combo1.pdf"), plot = cum_combo1, width = 35, height = 35, units = "cm", dpi = 320, bg = 'white')
ggsave(paste("plots/cum_combo2.pdf"), plot = cum_combo2, width = 35, height = 35, units = "cm", dpi = 320, bg = 'white')
