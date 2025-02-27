##### Data explore and clean 
## Libraries
library(tidyverse); library(readxl); library(data.table);library(openxlsx); library(stringr)

## Read in data 
dream1 <- readxl::read_excel("data/BEDAQUILINE DREAM DATASET FOR VIVLI - 06-06-2022.xlsx")
dream2 <- readxl::read_excel("data/BEDAQUILINE DREAM DATASET FOR VIVLI - 06-06-2022_global.xlsx")

dream <- rbind(dream1, dream2)

colnames(dream)
dim(dream) # 5928
table(dream$Specimen) 
table(dream$SubType) # Resistance classification
table(dream$Continent)
table(dream$Country)
table(dream$`BDQ Broth`)

### Explore antibiotic data
unique(dream$INH)
unique(dream$RMP)
unique(dream$LZD)
unique(dream$CFZ)
unique(dream$LVX)
unique(dream$OFX)
unique(dream$MXF)
unique(dream$CAP)
unique(dream$KAN)
unique(dream$EMB)
unique(dream$`BDQ Broth`)
unique(dream$`BDQ MGIT`) 

dream$index <- 1:nrow(dream)

# Pivot longer to explore ranges in MIC
dream_clean <- dream %>% select(-c("BDQ MGIT", "Rv0678_NT", "Rv0678_AA","atpE_NT","atpE_AA","pepQ_NT","pepQ_AA","Rv1979c_NT","Rv1979c_AA","...28","...29","...30","...31","...32")) %>%
  pivot_longer(cols = `INH`:`BDQ Broth`, values_to = "mic", names_to = "antibiotic") 

dream_clean <- rename(dream_clean, "year" = "Year Collected")
  
unique(dream_clean$mic)
colnames(dream_clean) <- tolower(colnames(dream_clean))

#### Combine data
col_use <- c("age","gender","source","year", "country","organism","antibiotic","mic","data")
 
full_data <- rbind(dream_clean) 

dim(full_data)  

# Clean mic
unique(full_data$mic)
full_data$mic <- gsub('>4', '8', full_data$mic)
full_data$mic <- gsub('>16', '32', full_data$mic)
full_data$mic <- gsub('>8', '16', full_data$mic)
full_data$mic <- gsub('≤0.06', '0.06', full_data$mic)
full_data$mic <- gsub('≤0.015', '0.0075', full_data$mic)
full_data$mic <- gsub('≤0.008', '0.004', full_data$mic)
full_data$mic <- gsub('≤0.12', '0.06', full_data$mic)
full_data$mic <- gsub('≤0.03', '0.015', full_data$mic)
full_data$mic <- gsub('≤0.25', '0.125', full_data$mic)
full_data$mic <- gsub('1.4999999999999999E-2', '0.015', full_data$mic)
unique(full_data$mic)

# suppressing warning as expect NAs!
suppressWarnings(full_data$mic <- as.numeric(full_data$mic))

### Antibiotics tidy ###
full_data$antibiotic <- tolower(full_data$antibiotic)
abx <- unique(full_data$antibiotic) 
# rename antibiotics with full name)
full_data[str_which(full_data$antibiotic, "inh"), "antibiotic"] <- "isoniazid"
full_data[str_which(full_data$antibiotic, "rmp"), "antibiotic"] <- "rifampicin"
full_data[str_which(full_data$antibiotic, "lzd"), "antibiotic"] <- "linezolid"
full_data[str_which(full_data$antibiotic, "cfz"), "antibiotic"] <- "clofazimine"
full_data[str_which(full_data$antibiotic, "lvx"), "antibiotic"] <- "levofloxacin"
full_data[str_which(full_data$antibiotic, "ofx"), "antibiotic"] <- "ofloxacin"
full_data[str_which(full_data$antibiotic, "mxf"), "antibiotic"] <- "moxifloxacin"
full_data[str_which(full_data$antibiotic, "cap"), "antibiotic"] <- "capreomycin"
full_data[str_which(full_data$antibiotic, "kan"), "antibiotic"] <- "kanamycin"
full_data[str_which(full_data$antibiotic, "ami"), "antibiotic"] <- "amikacin"
full_data[str_which(full_data$antibiotic, "emb"), "antibiotic"] <- "ethambutol"
full_data[str_which(full_data$antibiotic, "bdq broth"), "antibiotic"] <- "bedaquiline"

# check them
unique(full_data$antibiotic)

## Clean countries 
full_data$country[full_data$country== "US"] <- "United States"
full_data$country[full_data$country== "South Korea"] <- "Korea, South"
full_data$country[full_data$country== "INDIA"] <- "India"
full_data$country[full_data$country== "PHILIPPINES"] <- "Philippines"

full_data$continent[full_data$continent== "ASIA"] <- "Asia"

full_data$organism[full_data$organism== "M. Tuberculosis"] <- "M. tuberculosis"

### Focus
dim(full_data)

# add income groups (world bank), who regions, who tb groupings, antibiotic families
income_grps <- as.data.table(read_csv("data/income.csv"),show_col_types = FALSE)
who_regions <- as.data.table(read_csv("data/who-regions.csv"), show_col_types = FALSE)
who_tb_groups <- as.data.table(read_csv("data/who_tb_grouping.csv"), show_col_types = FALSE)
ab_fam <- as.data.table(read_csv("data/antibiotic_families.csv"), show_col_types = FALSE)

full_data <- as.data.table(full_data)

# match into full data
full_data[income_grps, on = "country", income_grp := income]
full_data[who_regions, on = "country", who_region := i.who_region]
full_data[who_tb_groups, on = "country", who_tb_group := WHO_grouping]
full_data[ab_fam, on = "antibiotic", ab_family := family]

# add breakpoints
full_data <- full_data %>% mutate(breakpoint = if_else(antibiotic=="amikacin", "2",
                                               if_else(antibiotic=="bedaquiline","0.25",
                                               if_else(antibiotic=="isoniazid", "0.25",
                                               if_else(antibiotic=="rifampicin", "0.5",
                                               if_else(antibiotic=="levofloxacin", "1",
                                               if_else(antibiotic=="moxifloxacin", "0.5",
                                               if_else(antibiotic=="ethambutol", NA,
                                               if_else(antibiotic=="linezolid", "2",
                                               if_else(antibiotic=="clofazimine", "0.5",
                                               if_else(antibiotic=="ofloxacin", "2",
                                               if_else(antibiotic=="capreomycin", "4",
                                               if_else(antibiotic=="kanamycin", "4",NA
                                               ))))))))))))) 
# add in new resistance classifications
full_data <- full_data %>%
  mutate(res = ifelse(mic <= breakpoint, "Susceptible", "Resistant")) %>%
  group_by(index) %>%  # Group by a unique identifier for each sample
  mutate(
    is_mdr = any(antibiotic == "rifampicin" & res == "Resistant") ,
    is_pre_xdr = is_mdr & 
      (any(antibiotic == "moxifloxacin" & res == "Resistant") | 
         any(antibiotic == "levofloxacin" & res == "Resistant")|
         any(antibiotic == "ofloxacin" & res == "Resistant")),
    is_xdr = is_pre_xdr & 
      (any(antibiotic == "bedaquiline" & res == "Resistant") | 
         any(antibiotic == "linezolid" & res == "Resistant")),
    new_subtype = case_when(
      is_xdr ~ "XDR",
      is_pre_xdr ~ "Pre-XDR",
      is_mdr ~ "MDR/RR",
      TRUE ~ "Rifampicin Susceptible"
    )
  ) %>%
  ungroup()

### Check number of isolates
gg <- full_data %>% filter(country =="Global")
length(unique(gg$index))

### Count number of isolates per grouping 
ll <- full_data %>%
  select(c("country","who_region","income_grp", 
           "who_tb_group", "year", "index","new_subtype")) %>%
  filter(country!="Global") %>%
  unique()
  
grouping <- c("country","who_region","income_grp", 
              "who_tb_group", "year","new_subtype") 

results_list <- lapply(grouping, function(variable) {
  ll[[variable]] %>%
    table() %>%
    as.data.frame() %>%
    arrange(desc(Freq))
})

combined_df <- as.data.frame(bind_rows(results_list, .id = "Variable")) 

colnames(combined_df) <- c("Grouping","Categories","Number of isolates")

# export breakpoints as table
bp <- full_data %>%
  select(c(antibiotic, breakpoint)) %>%
  unique()

# count number of MIC values with a qualifier 
quali <- dream_clean %>%
  filter(country == "Global") %>%
  #select(c("antibiotic", "mic")) %>%
  select(c( "mic")) %>%
  count( mic) %>%
  filter(str_detect(mic, "[>≤]")) %>%
  summarise(sum(n))

# Calculate resistance proportions
res_prop <- full_data %>%
  group_by(antibiotic, country, res) %>%
  summarise(n = n()) %>%
  mutate(per = round((n / sum(n))*100, digits=2)) %>%
  filter(res == "Resistant") %>%
  select(antibiotic, country, per) %>%
  pivot_wider(names_from = country, values_from = per) %>% 
  replace(is.na(.), 0) 

### Select columns of use
full_data <- full_data %>% select(-c(continent,specimen,organism,subtype,is_xdr,is_pre_xdr,is_mdr))

#### output
write.csv(full_data, "data/full_data.csv")
write.xlsx(combined_df, "data/allgroupings.xlsx", sheetName="Results")
write.csv(bp, "data/breakpoints.csv")
write.csv(res_prop,"data/resistance_proportions.csv") 
                                     