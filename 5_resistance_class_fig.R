# Create cumulative and histogram plots of MIC distributions by resistance class
# Libraries
library(ggplot2); library(tidyr); library(ggthemes); library(dplyr); library(ggridges)
library(RColorBrewer);library(openxlsx); library(tidyverse); library(patchwork);library(scales)
# Set theme for all plots
theme_set(theme_minimal(base_family="Helvetica", base_size=14))
# Load in clean MIC data
full_data <- (read.csv("data/full_data.csv"))
# Set colours and labels for resistance classes
colors <- c("Rifampicin Susceptible" = "#76B7AF", "MDR/RR" = "#4F77A8", "Pre-XDR" = "#F28E2C",
            "XDR" = "#E1565B")
labels <- c("Rifampicin Susceptible" = "Rifampicin\nSusceptible (292)", "MDR/RR" = "MDR/RR (3782)",
            "Pre-XDR" = "Pre-XDR (1792)", "XDR" = "XDR (62)")
antibiotics <- c("bedaquiline","isoniazid","levofloxacin","moxifloxacin",
                 "ofloxacin","linezolid","amikacin","kanamycin","capreomycin",
                 "clofazimine","ethambutol","rifampicin")

# Create folder to store plots
dir.create("plots", recursive = TRUE, showWarnings = FALSE)

#### Cumulative Plot ####
# Read in cumulative country data stratified by resistance class
test1 <- read_csv(paste0("plots/" ,"NEW_subtype_country_output_test.csv"), show_col_types = FALSE) %>%
  mutate(new_subtype = factor(new_subtype, levels=c("Rifampicin Susceptible","MDR/RR","Pre-XDR","XDR"))) %>%
  arrange(new_subtype) 

c_plots <- list()

for (i in antibiotics) {
  current_data <- test1 %>% filter(antibiotic == i)
  c_plots[[i]] <- ggplot(current_data, aes(x=MIC,y=cumulative_sum, group=new_subtype,color=new_subtype) ) +
    geom_line() +
    scale_color_manual(values=colors, labels=labels) +
    geom_vline(data=current_data, aes(xintercept = breakpoint), color = "darkgrey", linetype = "twodash", linewidth = 1) +
    labs(x= "MIC value (mg/L)",
         y='Cumulative proportion',
         color="WHO TB\nresistance\nclassifications\n(# of isolates)",
         shape="WHO TB\nresistance\nclassifications\n(# of isolates)",
         title=i)  +
    scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0), limits=c(0.001,100), labels = label_number(accuracy=0.01)) +
    scale_y_continuous(limits = c(0,1)) +
    guides(color = guide_legend(reverse=T), shape=guide_legend(reverse=T)) +
    geom_point(data = current_data, aes(x = MIC, y = cumulative_sum,group=new_subtype,color=new_subtype,shape=new_subtype), size = 1) +
    scale_shape_manual(values = c(1, 16, 1,16),labels=labels)+
    theme(axis.text.x= element_text(angle = 45, hjust = 0.5),
          plot.title = element_text(size = 14))
}

#### Ridge plot ####
# Select antibiotics to use
full_data_newdefs1 <- full_data %>%
  filter(country!="Global") %>%
  mutate(new_subtype = factor(new_subtype, levels=c("Rifampicin Susceptible","MDR/RR","Pre-XDR","XDR"))) %>%
  arrange(new_subtype) 

r_plots <- list()

for (i in antibiotics) {
  
  # Filter data for current antibiotic
  current_data <- full_data_newdefs1 %>% filter(antibiotic == i)
  
  find_bin_breaks <- full_data_newdefs1 %>% filter(antibiotic == i) %>%
    select(mic) %>%
    arrange(mic) 
  
  bin_breaks <- unique(current_data$mic)
  
  r_plots[[i]] <- ggplot(current_data, aes(x=mic,y=new_subtype,fill= new_subtype, group=new_subtype)) +
    geom_density_ridges(alpha=0.5, stat = "binline", breaks=bin_breaks, scale = 1.3) +
    scale_x_log10(breaks = c(0.01, 0.10,1.00,10.0), limits=c(0.001,100), labels = label_number(accuracy=0.01)) +
    scale_fill_manual(values=colors, labels=labels) +
    guides(fill = guide_legend(reverse=T)) +
    labs(x= "MIC value (mg/L)",
         y='',
         fill="WHO TB\nresistance\nclassifications\n(# of isolates)")  +
    scale_y_discrete(labels=c("Rifampicin Susceptible"="Rifampicin\nSusceptible")) +
    geom_vline(data=current_data, aes(xintercept = breakpoint), color = "darkgrey", linetype = "twodash", linewidth = 1) +
    coord_cartesian(clip = "off") +
    theme(axis.text.x= element_text(angle = 45, hjust = 0.5))
  
}

#### Construct plots ####
rif_c <- c_plots[["rifampicin"]]; rif_r <- r_plots[["rifampicin"]]
inh_c <- c_plots[["isoniazid"]]; inh_r <- r_plots[["isoniazid"]]
mox_c <- c_plots[["moxifloxacin"]]; mox_r <- r_plots[["moxifloxacin"]]
lev_c <- c_plots[["levofloxacin"]]; lev_r <- r_plots[["levofloxacin"]]
ofl_c <- c_plots[["ofloxacin"]]; ofl_r <- r_plots[["ofloxacin"]]
ami_c <- c_plots[["amikacin"]]; ami_r <- r_plots[["amikacin"]]
kan_c <- c_plots[["kanamycin"]]; kan_r <- r_plots[["kanamycin"]]
cap_c <- c_plots[["capreomycin"]]; cap_r <- r_plots[["capreomycin"]]
clo_c <- c_plots[["clofazimine"]]; clo_r <- r_plots[["clofazimine"]]
eth_c <- c_plots[["ethambutol"]]; eth_r <- r_plots[["ethambutol"]]
lin_c <- c_plots[["linezolid"]]; lin_r <- r_plots[["linezolid"]]
bdq_c <- c_plots[["bedaquiline"]]; bdq_r <- r_plots[["bedaquiline"]]

(fig3 <- rif_c + inh_c + mox_c + 
    rif_r + inh_r + mox_r + 
    bdq_c + lin_c + ami_c + 
    bdq_r + lin_r + ami_r +
  plot_layout(ncol=3, nrow=4, guides = "collect", axes="collect"))

(fig3S <- cap_c + clo_c + eth_c + 
    cap_r + clo_r + eth_r + 
    kan_c + ofl_c + lev_c + 
    kan_r + ofl_r + lev_r +
  plot_layout(ncol=3, nrow=4, guides = "collect", axes="collect"))

### Save plots ###

ggsave(paste("plots/fig3_density_cum.pdf"), plot = fig3, width = 30, height = 30, units = "cm", dpi = 320, bg = 'white')
ggsave(paste("plots/fig3S_density_cum.pdf"), plot = fig3S, width = 30, height = 30, units = "cm", dpi = 320, bg = 'white')
