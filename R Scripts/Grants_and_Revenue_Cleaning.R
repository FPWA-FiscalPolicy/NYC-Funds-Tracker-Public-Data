# load libraries
library(tidyverse)
library(data.table)
library(RCurl)
library(xlsx)
library(naniar)
# put raw_revenue.csv, CPI_index.csv, and ACFR_Revenue_Corrected.csv
# into the same folder as this R file and set working directory to source file location
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


############################## Checkbook Grants ################################

# read revenue data
Revenue_data <- fread('raw_revenue.csv', drop=1) # # of obs= 8890828

# read CPI data for inflation adjustment
CPI <- fread('CPI_index.csv') %>% 
  filter(Year >= 2011) %>% 
  select(Year, V17) %>% 
  rename(fiscal_year=Year, FY_CPI=V17)
CPI2024 <- CPI[CPI$fiscal_year==2024]$FY_CPI

# filter grants by closing classification
revenue_grants <- filter(Revenue_data, revenue_category%in%c("Federal Grants-Categorical", 
                                                                         "Unrestricted Federal and State Aid",
                                                                         "State Grants and Contracts-Categorical",
                                                                         "State Grants-Categorical",
                                                                         "Federal Grants and Contracts-Categorical",
                                                                         "Non-Governmental Grants"))%>%
  filter(closing_classification_name != "Collected Unearned Revenue Roll" &
           closing_classification_name != "Unbilled Revenue" &
           closing_classification_name != "Accounts Left In Old Year") %>% 
  left_join(CPI) %>% 
  mutate(Adjusted.recognized=recognized*CPI2024/FY_CPI) # calculate inflation-adjusted grants

# save grants data in csv file
write.csv(revenue_grants, "revenue_grants.csv")


############################## ACFR Revenue ################################

# read ACFR revenue data

ACFR <- fread("ACFR_Revenue_Corrected.csv", skip = 2, header = T)

# classify categories using revenue_category(top layer), subcategory(middle layer), category(bottom layer)
category <- ACFR$V1
category[endsWith(category,":")|startsWith(category, "Total")]

# create a vector for revenue_category
revenue_category_vec <- c("Taxes (Net of Refunds):",
                      "Federal Grants and Contracts--Categorical:",
                      "State Grants and Contracts--Categorical:",
                      "Non-Governmental Grants:",
                      "Provision for Disallowances Federal, State and Other Aid",
                      "Unrestricted Federal and State Aid:",
                      "Charges for Services:",
                      "Investment Income",
                      "Licenses, Permits, Privileges and Franchises:",
                      "Fines and Forfeitures:",
                      "Miscellaneous",
                      "Pollution Remediation-Bond Sales",
                      "Transfer from General Debt Service Fund",
                      "Transfer from Nonmajor Debt Service Fund")

# create a vector for subcategory
subcategory_vec <- c(category[grepl(category, pattern=":") & (!category%in%revenue_category_vec)],
  revenue_category_vec[!grepl(revenue_category_vec, pattern=":")],
  "Income Taxes, Other (Net of Refunds)",
  "Personal Income Taxes (Net of Refunds)",
  "Personal Income Taxes (Net of Refunds)",
  category[(which(category=="Federal Grants and Contracts--Categorical:")+1):(which(category=="Total Federal Grants")-1)],
  category[(which(category=="State Grants and Contracts--Categorical:")+1):(which(category=="Total State Grants")-1)],
  category[(which(category=="Non-Governmental Grants:")+1):(which(category=="Total Non-Governmental Grants")-1)],
  category[(which(category=="Unrestricted Federal and State Aid:")+1):(which(category=="Total Unrestricted Federal and State Aid")-1)],
  category[(which(category=="Charges for Services:")+1):(which(category=="Total Charges for Services")-1)],
  category[(which(category=="Licenses, Permits, Privileges and Franchises:")+1):(which(category=="Total Licenses, Permits, Privileges and Franchises")-1)],
  category[(which(category=="Fines and Forfeitures:")+1):(which(category=="Total Fines and Forfeitures")-1)],
  "Miscellaneous",
  "Pollution Remediation-Bond Sales",
  "Transfer from General Debt Service Fund",
  "Transfer from Nonmajor Debt Service Fund")



# pivot ACFR data to long form and clean 

ACFR_revenue_cleaned <- ACFR %>% replace_with_na_all(condition=~.x =="-") %>% # replace - with NA
  select(V1:`2011`) %>% 
  gather("fiscal_year", "amount_in_thousands", -V1) %>% # pivot to long form
  mutate(fiscal_year=as.numeric(fiscal_year), # convert years to numeric
         negative=case_when(substring(amount_in_thousands,1,1)=="(" ~ -1,
                            T ~ 1)) %>% # covert numbers with parentheses into negative
  rename(category=V1)%>% 
  mutate(amount_in_thousands=parse_number(amount_in_thousands)) %>% # properly convert revenue into numeric
  mutate(amount_in_thousands=amount_in_thousands*negative) %>% 
  select(-negative) %>% 
  mutate(revenue_category=case_when(category%in%revenue_category_vec ~ category,
                                    T ~ NA)) %>% # create revenue_category column
  fill(revenue_category) %>% # fill missing with previous values
  mutate(subcategory=case_when(category%in%subcategory_vec ~ category,
                                    T ~ NA)) %>% # create subcategory column
  fill(subcategory) %>% # fill missing with previous values
  select(revenue_category, subcategory, category, fiscal_year, amount_in_thousands) %>% # select and reorder columns of interest
  filter(!category%in%c(category[grepl(category, pattern=":")|startsWith(category, "Total")],
                        "Income Taxes, Other (Net of Refunds)")|category=="Total Other Revenues") %>% # drop rows with category totals
  mutate(subcategory=if_else((revenue_category=="Taxes (Net of Refunds):" & category=="Real Estate Taxes"), "Real Estate Taxes", subcategory)) %>% # correct wrong filling missing
  rename(Categorical.Citywide.Rev=amount_in_thousands) %>% # rename columns according to the naming convention
  left_join(CPI) %>% 
  mutate(Adjusted.Categorical.Citywide.Rev=Categorical.Citywide.Rev*CPI2024/FY_CPI,
         Categorical.Citywide.Rev=replace_na(Categorical.Citywide.Rev, 0),
         Adjusted.Categorical.Citywide.Rev=replace_na(Adjusted.Categorical.Citywide.Rev, 0)) # calculate inflation-adjusted revenue and replace mising revenues with 0

# formatting category names
ACFR_revenue_cleaned_formatted <- ACFR_revenue_cleaned %>% # drop unwanted characters
  mutate(revenue_category=gsub(pattern=" (Net of Refunds)", 
                               replacement="", 
                               x=revenue_category, fixed = T),
         revenue_category=gsub(pattern=":", 
                               replacement="", 
                               x=revenue_category, fixed = T),
         subcategory=gsub(pattern=" (Net of Refunds)", 
                               replacement="", 
                               x=subcategory, fixed = T),
         subcategory=gsub(pattern=":", 
                          replacement="", 
                          x=subcategory, fixed = T),
         category=gsub(pattern=" (Net of Refunds)", 
                          replacement="", 
                          x=category, fixed = T),
         category=gsub(pattern=":", 
                          replacement="", 
                          x=category, fixed = T))

# save cleaned ACFR revenue data in a csv file
write.csv(ACFR_revenue_cleaned_formatted, "ACFR_revenue_long.csv")

# sum to check for errors
ACFR_sum_by_year <- ACFR_revenue_cleaned_formatted %>% 
  group_by(fiscal_year) %>% 
  summarise(sum=sum(Categorical.Citywide.Rev, na.rm = T))

ACFR_sum_by_revenue_category <- ACFR_revenue_cleaned_formatted  %>% 
  filter(fiscal_year==2011) %>% 
  group_by(revenue_category) %>% 
  summarise(sum=sum(Categorical.Citywide.Rev, na.rm = T))

ACFR_sum_by_category <- ACFR_revenue_cleaned_formatted  %>% 
  filter(fiscal_year==2011) %>% 
  group_by(revenue_category, subcategory) %>% 
  summarise(sum=sum(Categorical.Citywide.Rev, na.rm = T))

# compare revenue_grants with ACFR

# Checkbook grants by year in thousands
revenue_grants_sum_by_year <- revenue_grants %>% 
  filter(fiscal_year<2024) %>% 
  group_by(fiscal_year) %>% 
  summarise(revenue.recognized=sum(recognized, na.rm = T),
            Adjusted.revenue.recognized=sum(Adjusted.recognized, na.rm = T)) %>% 
  arrange(fiscal_year) %>% 
  mutate(revenue.recognized=round(revenue.recognized/1000),
         Adjusted.revenue.recognized=round(Adjusted.revenue.recognized/1000))

# ACFR grants
ACFR_total_grants <- ACFR %>% filter(V1%in%c("Total Federal Grants", 
                                             "Total State Grants", 
                                             "Total Non-Governmental Grants",
                                             "Total Unrestricted Federal and State Aid")) %>% 
  replace_with_na_all(condition=~.x =="-") %>% 
  gather("fiscal_year", "ACFR_total", -V1) %>% 
  mutate(fiscal_year=as.numeric(fiscal_year)) %>% 
  rename(category=V1)%>% 
  mutate(ACFR_total=parse_number(ACFR_total)) %>% 
  left_join(CPI) %>% 
  mutate(Adjusted.ACFR_total=ACFR_total*CPI2024/FY_CPI)

# ACFR grants by year
ACFR_grants_sum_by_year <- ACFR_total_grants %>% 
  select(fiscal_year, ACFR_total, Adjusted.ACFR_total) %>% 
  group_by(fiscal_year) %>%
  summarise(ACFR.total=sum(ACFR_total, na.rm = T),
         Adjusted.ACFR.total=sum(Adjusted.ACFR_total, na.rm = T))

# ACFR and Checkbook grants difference
ACFR_revenue_grant_diff <- ACFR_grants_sum_by_year %>% 
  left_join(revenue_grants_sum_by_year) %>% 
  mutate(diff=ACFR.total-revenue.recognized,
         Adjusted.diff=Adjusted.ACFR.total-Adjusted.revenue.recognized) %>% 
  select(fiscal_year, ACFR.total, revenue.recognized, diff, Adjusted.ACFR.total, Adjusted.revenue.recognized, Adjusted.diff)

# sum of difference and adjusted difference
(diff_sum <- sum(ACFR_revenue_grant_diff$diff))

(adjusted_diff_sum <- sum(ACFR_revenue_grant_diff$Adjusted.diff))

