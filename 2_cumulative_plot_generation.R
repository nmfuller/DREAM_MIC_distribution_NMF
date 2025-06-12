#####  Plot generation for cumulative proportion graphs 
# Libraries
library(ggplot2); library(tidyverse); library(patchwork); library(ggthemes)

# Set theme for all figures
theme_set(theme_minimal(base_family="Helvetica", base_size=16))

# Read in antibiotic family data
ab_fam <- read_csv("data/antibiotic_families.csv", show_col_types = FALSE)

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

(country_plot <-  ggplot(country_mic, aes(x=MIC,y=cumulative_sum,group=country2, color=country2)) +
                  geom_line() +
                  scale_color_manual(values=colors, labels=label) +
                  geom_vline(data=country_mic, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
                  labs(x= "MIC value (mg/l)",
                       y='Cumulative proportion of isolates by country',
                       color ="Country (# of isolates)",
                       shape="Country (# of isolates)")  +
                  scale_x_log10() +
                  facet_wrap(.~fct_inorder(antibiotic), ncol=3) +
                  geom_point(data = country_mic, aes(x = MIC, y = cumulative_sum, colour = country2,group=country2, shape=country2), size = 1) +
                  scale_shape_manual(values = c(8, 1, 16, 1, 16, 1, 16, 1, 16, 1, 16,1), labels=label))

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

whoreg_plot <-  ggplot(whoreg_mic, aes(x=MIC,y=cumulative_sum,group=who_region, color=who_region)) +
  geom_line()+
  scale_color_manual(values=colors3, labels=label3) +
  geom_vline(data=whoreg_mic, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
  labs(x= "MIC value (mg/l)",
       y='Cumulative proportion of isolates by WHO Region',
       color="WHO Region (# of isolates)",
       shape="WHO Region (# of isolates)")  +
  scale_x_log10() +
  facet_wrap(.~fct_inorder(antibiotic), ncol=3) +
  geom_point(data = whoreg_mic, aes(x = MIC, y = cumulative_sum, colour = who_region,group=who_region, shape=who_region), size = 1) +
  scale_shape_manual(values = c(1, 16, 1, 16,1,16,1), labels=label3) 

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

whotbgroup_plot <-  ggplot(whotbgroup_mic, aes(x=MIC,y=cumulative_sum,group=who_tb_group, color=who_tb_group)) +
  geom_line()+
  scale_color_manual(values=colors5, labels=label5) +
  geom_vline(data=whotbgroup_mic, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
  labs(x= "MIC value (mg/l)",
       y='Cumulative proportion of isolates by WHO TB Priority Group',
       color="WHO TB Priority Group\n(# of isolates)",
       shape="WHO TB Priority Group\n(# of isolates)")  +
  scale_x_log10() +
  facet_wrap(.~fct_inorder(antibiotic), ncol=3) +
  geom_point(data = whotbgroup_mic, aes(x = MIC, y = cumulative_sum, colour = who_tb_group,group=who_tb_group, shape=who_tb_group), size = 1) +
  scale_shape_manual(values = c(1, 16,1,16), labels=label5) 

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

income_grp_plot <-  ggplot(income_grp_mic, aes(x=MIC,y=cumulative_sum,group=income_grp, color=income_grp)) +
  geom_line()+
  scale_color_manual(values=colors6, labels=label6) +
  geom_vline(data=income_grp_mic, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
  labs(x= "MIC value (mg/l)",
       y='Cumulative proportion of isolates by Income Group',
       color="Income Group (# of isolates)",
       shape="Income Group (# of isolates)")  +
  scale_x_log10() +
  facet_wrap(.~fct_inorder(antibiotic), ncol=3) +
  geom_point(data = income_grp_mic, aes(x = MIC, y = cumulative_sum, colour = income_grp,group=income_grp, shape=income_grp), size = 1) +
  scale_shape_manual(values = c(1, 16, 1, 16,1), labels=label6) 

###### Year #####
year_mic <- read_csv(paste0("plots/" ,"yearoutput.csv"), show_col_types = FALSE)
year_mic <- merge(year_mic, ab_fam, by="antibiotic")
year_mic <- year_mic %>%
  arrange(family,antibiotic)

label7 <- c("2019" = "2019 (753)", "2018" = "2018 (1190)",
            "2017" = "2017 (1543)", "2016" = "2016 (1245)",
            "2015" = "2015 (926)", "2014" = "2014 (143)",
            "2013" = "2013 (126)", "2011" = "2011 (2)")

year_plot <-  ggplot(year_mic, aes(x=MIC,y=cumulative_sum,group=as.factor(year), color=as.factor(year))) +
  geom_line()+
  scale_color_tableau(palette="Tableau 10", type="regular", labels=label7) +
  geom_vline(data=year_mic, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
  labs(x= "MIC value (mg/l)",
       y='Cumulative proportion of isolates by Year',
       color="Year",
       shape="Year")  +
  scale_x_log10() +
  facet_wrap(.~fct_inorder(antibiotic), ncol=3) +
  geom_point(data = year_mic, aes(x = MIC, y = cumulative_sum, colour = as.factor(year),group=as.factor(year), shape=as.factor(year)), size = 1) +
  scale_shape_manual(values = c(1, 16, 1, 16,1,16,1,16), labels=label7)

##### Combine plots #####
cum_combo1 <- country_plot / income_grp_plot + plot_layout(axes = "collect") +  plot_annotation(tag_levels = 'A')
cum_combo2 <- whoreg_plot / whotbgroup_plot + plot_layout(axes = "collect") +  plot_annotation(tag_levels = 'A')

# Create folder to store plots
dir.create("plots", recursive = TRUE, showWarnings = FALSE)

ggsave(paste("plots/cum_combo1.tiff"), plot = cum_combo1, width = 25, height = 35, units = "cm", dpi = 320, bg = 'white')
ggsave(paste("plots/cum_combo2.tiff"), plot = cum_combo2, width = 25, height = 35, units = "cm", dpi = 320, bg = 'white')
