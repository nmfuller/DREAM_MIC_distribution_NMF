# Create cumulative and histogram plots of MIC distributions by resistance class
# Libraries
library(ggplot2); library(tidyr); library(ggthemes); library(dplyr); library(ggridges)
library(RColorBrewer);library(openxlsx); library(tidyverse); library(patchwork)
# Set theme for all plots
theme_set(theme_minimal(base_family="Times", base_size=16))
# Load in clean MIC data
full_data <- (read.csv("data/full_data.csv"))
# Set colours and labels for resistance classes
colors <- c("Rifampicin Susceptible" = "#76B7AF", "MDR/RR" = "#4F77A8", "Pre-XDR" = "#F28E2C",
            "XDR" = "#E1565B")
labels <- c("Rifampicin Susceptible" = "Rifampicin\nSusceptible (292)", "MDR/RR" = "MDR/RR (3714)",
            "Pre-XDR" = "Pre-XDR (1836)", "XDR" = "XDR (86)")

# Create folder to store plots
dir.create("plots", recursive = TRUE, showWarnings = FALSE)

#### Cumulative Plot ####
# Read in cumulative country data stratified by resistance class
test1 <- read_csv(paste0("plots/" ,"NEW_subtype_country_output_test.csv"), show_col_types = FALSE) %>%
  filter(antibiotic %in% c("rifampicin","isoniazid","moxifloxacin","bedaquiline","amikacin","linezolid")) %>%
  mutate(antibiotic = factor(antibiotic, levels = c("rifampicin","isoniazid","moxifloxacin","bedaquiline","amikacin","linezolid"))) %>%
  arrange(antibiotic) %>%
  filter(country == "Global") %>%
  mutate(new_subtype = factor(new_subtype, levels=c("Rifampicin Susceptible","MDR/RR","Pre-XDR","XDR"))) %>%
  arrange(new_subtype) 

(test1_plot <-  ggplot(test1, aes(x=MIC,y=cumulative_sum, group=new_subtype,color=new_subtype) ) +
    geom_line() +
    scale_color_manual(values=colors, labels=labels) +
    geom_vline(data=test1, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
    labs(x= "MIC value (mg/l)",
         y='Cumulative proportion of isolates by resistance class',
         color="WHO TB\nresistance\nclassifications\n(# of isolates)",
         shape="WHO TB\nresistance\nclassifications\n(# of isolates)")  +
    scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0)) +
    guides(color = guide_legend(reverse=T), shape=guide_legend(reverse=T)) +
    facet_wrap(.~fct_inorder(antibiotic), ncol=3) +
    geom_point(data = test1, aes(x = MIC, y = cumulative_sum,group=new_subtype,color=new_subtype,shape=new_subtype), size = 1) +
    scale_shape_manual(values = c(1, 16, 1,16),labels=labels))

#### Ridge plot ####
# Select antibiotics to use
full_data_newdefs1 <- full_data %>%
  filter(antibiotic %in% c("rifampicin","isoniazid","moxifloxacin","bedaquiline","amikacin","linezolid")) %>%
  mutate(antibiotic = factor(antibiotic, levels = c("rifampicin","isoniazid","moxifloxacin","bedaquiline","amikacin","linezolid"))) %>%
  arrange(antibiotic) %>%
  mutate(new_subtype = factor(new_subtype, levels=c("Rifampicin Susceptible","MDR/RR","Pre-XDR","XDR"))) %>%
  arrange(new_subtype) %>%
  filter(country=="Global")

(density <- ggplot(full_data_newdefs1, aes(x=mic,y=new_subtype,fill= new_subtype, group=new_subtype)) +
    geom_density_ridges(alpha=0.5, stat = "binline", bins=13, scale = 1.3) +
    facet_wrap(.~fct_inorder(antibiotic), ncol=3)+
    scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0)) +
    scale_fill_manual(values=colors, labels=labels) +
    guides(fill = guide_legend(reverse=T)) +
    labs(x= "MIC value (mg/l)",
         y='',
         fill="WHO TB\nresistance\nclassifications\n(# of isolates)")  +
    scale_y_discrete(labels=c("Rifampicin Susceptible"="Rifampicin\nSusceptible")) +
    geom_vline(data=full_data_newdefs1, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1)
) 

#### Rest of the antibiotics ####
# Read in cumulative country data stratified by resistance class
# Select antibiotics to use and set order
test2 <- read_csv(paste0("plots/" ,"NEW_subtype_country_output_test.csv"), show_col_types = FALSE) %>%
  mutate(new_subtype = factor(new_subtype, levels=c("Rifampicin Susceptible","MDR/RR","Pre-XDR","XDR"))) %>%
  arrange(new_subtype) %>%
  filter(!antibiotic%in%c("rifampicin","isoniazid","moxifloxacin","bedaquiline","amikacin","linezolid")) %>%  
  filter(country == "Global") %>% 
  mutate(antibiotic = factor(antibiotic, levels = c("capreomycin","clofazimine","ethambutol","kanamycin","levofloxacin","ofloxacin"))) %>%
  arrange(antibiotic) 

(test2_plot <-  ggplot(test2, aes(x=MIC,y=cumulative_sum, group=new_subtype,color=new_subtype) ) +
    geom_line() +
    scale_color_manual(values=colors, labels=labels) +
    geom_vline(data=test2, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1) +
    labs(x= "MIC value (mg/l)",
         y='Cumulative proportion of isolates by resistance class',
         color="WHO TB\nresistance\nclassifications\n(# of isolates)",
         shape="WHO TB\nresistance\nclassifications\n(# of isolates)")  +
    scale_x_log10() +
    guides(color = guide_legend(reverse=T), shape=guide_legend(reverse=T)) +
    facet_wrap(.~fct_inorder(antibiotic), ncol=3) +
    geom_point(data = test2, aes(x = MIC, y = cumulative_sum,group=new_subtype,color=new_subtype,shape=new_subtype), size = 1) +
    scale_shape_manual(values = c(1, 16, 1,16),labels=labels))

# Select antibiotics to use
full_data_newdefs2 <- full_data_newdefs %>%
  filter(!antibiotic %in% c("rifampicin","isoniazid","moxifloxacin","bedaquiline","amikacin","linezolid")) %>%
  mutate(antibiotic = factor(antibiotic, levels = c("capreomycin","clofazimine","ethambutol","kanamycin","levofloxacin","ofloxacin"))) %>%
  arrange(antibiotic) %>%
  mutate(new_subtype = factor(new_subtype, levels=c("Rifampicin Susceptible","MDR/RR","Pre-XDR","XDR"))) %>%
  arrange(new_subtype) %>%
  filter(country=="Global")

(density2 <- ggplot(full_data_newdefs2, aes(x=mic,y=new_subtype,fill= new_subtype, group=new_subtype)) +
    geom_density_ridges(alpha=0.5, stat = "binline", bins=13, scale = 1.3) +
    facet_wrap(.~fct_inorder(antibiotic), ncol=3)+
    scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0)) +
    scale_fill_manual(values=colors, labels=labels) +
    labs(x= "MIC value (mg/l)",
         y='',
         fill="WHO TB\nresistance\nclassifications\n(# of isolates)")  +
    scale_y_discrete(labels=c("Rifampicin Susceptible"="Rifampicin\nSusceptible")) +
    guides(fill = guide_legend(reverse=T)) +
    geom_vline(data=full_data_newdefs2, aes(xintercept = breakpoint*0.65), color = "darkgrey", linetype = "twodash", linewidth = 1)
) 

### Save plots ###
(fig3 <- test1_plot / density + plot_layout(heights = c(5,7)) + plot_annotation(tag_levels = 'A'))
(fig3S <- test2_plot / density2 + plot_layout(heights = c(5,7)) + plot_annotation(tag_levels = 'A'))

ggsave(paste("plots/fig3_density_cum.tiff"), plot = fig3, width = 26, height = 30, units = "cm", dpi = 320, bg = 'white')
ggsave(paste("plots/fig3S_density_cum.tiff"), plot = fig3S, width = 25, height = 30, units = "cm", dpi = 320, bg = 'white')
