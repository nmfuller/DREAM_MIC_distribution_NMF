#####  Calculating cumulative proportion per sub-group 
# Libraries 
library(data.table);library(ggplot2);library(cowplot); library(dplyr); 
library(patchwork);library(tidyverse)

# Read in the clean MIC data
full_data <- as.data.table(read.csv("data/full_data.csv"), show_col_types = FALSE)

# Select groupings to generate cumulative data (Note: Must match column name)
characteristics <- c("who_region", "country", "who_tb_group","income_grp","year") 

for(characteristic in characteristics){
    
    # make sure there's a folder to store the plots
    dir.create(file.path("plots"), showWarnings = FALSE)
    index_store <- c()
    output_plot <- c()
     
        data_sub <- full_data
        
        # vector for storing relevant drugs and plots
        drugs <- unique(data_sub$antibiotic)
        
        drugs <- sort(drugs)
    
        #for each of the relevant drugs
        for(i in drugs){
          data_sub_drug <- data_sub[antibiotic == i]
          #count observations by group
          test <- data_sub_drug[, .N, by = .(mic, get(characteristic))]
          colnames(test) <- c("MIC", characteristic, "N")
          test2 <- data_sub_drug[, .N, by = .(get(characteristic))]
          colnames(test2) <- c(characteristic, "N")
          # note total number of MIC samples
          tot_samps <- sum(test2$N)
          # combine the two together so can work out proportion
          test[test2, on = c(characteristic), Total := i.N]
          #work out proportion
          test[, prop := N/Total]
          # cumulative sum of proportion (first order)
          test <- test[order(MIC, get(characteristic))]
          for_plot <-test[, cumulative_sum := cumsum(prop), by = c(characteristic)]

          ### Output 
          # Add in breakpoints to data
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
      }
      write.csv(output_plot, paste0("plots/",characteristic, "output.csv"))
    }
