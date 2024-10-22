################################################################################
# This script merges the raw revenue and raw budget of all agencies from different years


################################################################################
# To update 2024 data, simply download the data from CheckbookNYC and save it 
# to the same folder where all data files for other years are saved
# The newly downloaded data file should follow the naming convention "all_agency_revene_..."
# The code is automated to read all csv files in "All Agency Revenue Data" folder
# that starts with "all_agency_revenue" and bind them together. Similarly for budget.

library(tidyverse)

##### Revenue
# list all the revenue csv files in "All Agency Revenue Data" folder folder
# Note: this may need to be changed according to different path and folder names
# Note: working directory should be set to be the path before 
# "/NYC-Funds-Tracker-Expansion/All Agency Revenue Data"
# the period in "./NYC-Funds-Tracker-Expansion/All Agency Revenue Data" is a shortcut
# to represent the working directory path
revenue_files <- list.files(path="./NYC-Funds-Tracker-Expansion/All Agency Revenue Data",pattern = "all_agency_revenue", full.names = TRUE)
raw_revenue <- data.frame()
# read and bind revenue for all agencies
for (i in revenue_files){
  temp <- read.csv(i)
  raw_revenue <-rbind(raw_revenue, temp)
}

# reorganize revenue 
raw_revenue <- raw_revenue %>% 
  mutate(Adopted=as.numeric(Adopted)) %>% # convert adopted to numeric
  rename(agency=Agency, revenue_category=Revenue.Category, revenue_source=Revenue.Source., 
         fund_class=Fund.Class, funding_class=Funding.Class, revenue_class=Revenue.Class,
         budget_fiscal_year=Budget.Fiscal.Year, fiscal_year=Fiscal.Year, adopted=Adopted,
         modified=Modified, recognized=Recognized, 
         closing_classification_name=Closing.Classification.Name) %>% # rename columns for easier downstream cleaning
  select(agency, revenue_category, revenue_source, 
          fund_class, funding_class, revenue_class,
          budget_fiscal_year, fiscal_year,
          modified, recognized, closing_classification_name) # reorder columns

# save the merged data in a csv file
write.csv(raw_revenue,"raw_revenue.csv")

##### Budget
# list all the budget csv files
budget_files <- list.files(path="./NYC-Funds-Tracker-Expansion/All Agency Budget Data",pattern = "all_agency_budget", full.names = TRUE)
raw_budget <- data.frame()

# read and bind budget for all agencies
for (i in budget_files){
  temp <- read.csv(i)
  raw_budget <-rbind(raw_budget, temp)
}

# reorganize budget
raw_budget <- raw_budget %>% 
  select(Agency, Year, Department, Expense.Category, Budget.Code, Budget.Name, Adopted,
         Modified, Encumbered, Cash.Expense, Pre.Encumbered, Post.Adjustments, Accrued.Expense,
         Committed) %>% # reorder columns
  rename(agency=Agency, year=Year, department=Department,  expense_category=Expense.Category,
         budget_code=Budget.Code, budget_name=Budget.Name, adopted=Adopted,
         modified=Modified, encumbered=Encumbered, cash_expense=Cash.Expense, 
         pre_encumbered=Pre.Encumbered, post_adjustment=Post.Adjustments, 
         accrued_expense=Accrued.Expense, committed=Committed) # rename columns for easier downstream cleaning
# save merged data in a csv file
write.csv(raw_budget,"raw_budget.csv")
