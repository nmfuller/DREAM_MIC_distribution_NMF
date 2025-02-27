##### Calculating cumulative proportion of countries with resistance class stratification
# Libraries
library(data.table);library(ggplot2);library(cowplot); library(tidyverse); library(patchwork)

# read in the clean MIC data 
full_data <- as.data.table(read.csv("data/full_data.csv")) 
# read in the cumulative MIC data for countries
country_mic <- read_csv(paste0("plots/" ,"countryoutput.csv"), show_col_types = FALSE)

drugs <- unique(full_data$antibiotic)

drugs <- sort(drugs)

output_plot <- c()

for(i in drugs){
  data_sub_drug <- full_data[antibiotic == i]
  #count observations by subset
  test <- data_sub_drug[, .N, by = .(mic, new_subtype, country)]
  colnames(test) <- c("MIC", "new_subtype","country", "N")
  test2 <- data_sub_drug[, .N, by = .(new_subtype)]
  colnames(test2) <- c("new_subtype", "N")
  # note total number of MIC samples
  tot_samps <- sum(test2$N)
  # combine the two together so can work out proportion
  test[test2, on = c("new_subtype"), Total := i.N]
  #work out proportion
  test[, prop := N/Total]
  # cumulative sum of proportion (first order)
  test <- test[order(MIC, get("new_subtype"))]
  for_plot <-test[, cumulative_sum := cumsum(prop), by = c("new_subtype")]
  for_plot <- for_plot %>% 
    group_by(new_subtype)
 
  ### Output and add back the breakpoints 
  output_plot <- rbind(output_plot, for_plot %>% mutate(antibiotic = i)
                       %>% mutate(breakpoint = if_else(antibiotic=="amikacin", "2",
                                               if_else(antibiotic=="bedaquiline", "0.25",
                                               if_else(antibiotic=="isoniazid", "0.25",
                                               if_else(antibiotic=="rifampicin", "0.5",
                                               if_else(antibiotic=="levofloxacin", "1",
                                               if_else(antibiotic=="moxifloxacin", "0.5",
                                               if_else(antibiotic=="ethambutol", "NA",
                                               if_else(antibiotic=="linezolid", "2",
                                               if_else(antibiotic=="clofazimine", "0.5",
                                               if_else(antibiotic=="ofloxacin", "2",
                                               if_else(antibiotic=="capreomycin", "4",
                                               if_else(antibiotic=="kanamycin", "4","NA"
                                  )))))))))))))
                       )
  write.csv(output_plot, paste0("plots/NEW_subtype_country_output_test.csv"))
}
