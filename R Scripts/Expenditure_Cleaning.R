# load libraries
library(tidyverse)
library(data.table)
library(RCurl)
library(xlsx)
library(naniar)
# put ACFR_Expenditure_Corrected.csv, CPI_index.csv, and raw_budget.csv
# into the same folder as this R script is in and set working directory to source file location
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

################################# ACFR Expenditure ############################
# read corrected ACFR data
ACFR <- fread("ACFR_Expenditure_Corrected.csv", skip = 2, header = T)

# read CPI data for inflation adjustment
CPI <- fread('CPI_index.csv') %>% 
  filter(Year >= 2011) %>% 
  select(Year, V17) %>% 
  rename(fiscal_year=Year, FY_CPI=V17)
CPI2024 <- CPI[CPI$fiscal_year==2024]$FY_CPI

# classify categories using expenditure_category(top layer), subcategory(middle layer), agency(bottom layer)
category <- ACFR$V1
category[endsWith(category,":")|startsWith(category, "Total")]

# create a vector for expenditure_category
expenditure_category_vec <- c(category[endsWith(category,":")],
                      "Judgments and Claims",
                      "Benefit Payments",
                      "Interest on Short-term Borrowing",
                      "Lease Payments")
expenditure_category_vec <- expenditure_category_vec[!(expenditure_category_vec%in%c("General Debt Service Fund:",
                                                             "Nonmajor Debt Service Funds:"))]

# create a vector for subcategory
subcategory_vec <- c("General Debt Service Fund:",
                  "Nonmajor Debt Service Funds:",
                  category[(which(category=="General Government:")+1):(which(category=="Total General Government")-1)],
                  category[(which(category=="Public Safety and Judicial:")+1):(which(category=="Total Public Safety and Judicial")-1)],
                  category[(which(category=="Education:")+1)],
                  category[(which(category=="City University:")+1):(which(category=="Total City University")-1)],
                  category[(which(category=="Social Services:")+1):(which(category=="Total Social Services")-1)],
                  category[(which(category=="Environmental Protection:")+1):(which(category=="Total Environmental Protection")-1)],
                  category[(which(category=="Transportation Services:")+1):(which(category=="Total Transportation Services")-1)],
                  category[(which(category=="Parks, Recreation, and Cultural Activities:")+1):(which(category=="Total Parks, Recreation, and Cultural Activities")-1)],
                  category[(which(category=="Housing:")+1):(which(category=="Total Housing")-1)],
                  category[(which(category=="Health:")+1):(which(category=="Total Health" )-1)],
                  category[(which(category=="Libraries:")+1):(which(category=="Total Libraries" )-1)],
                  category[(which(category=="Pensions:")+1)],
                  category[(which(category=="Other:")+1)],
                  "Miscellaneous-Payments to New York City Capital Projects Fund",
                  "Judgments and Claims",
                  "Benefit Payments",
                  "Interest on Short-term Borrowing",
                  "Lease Payments")



# pivot ACFR data to long form and clean
ACFR_expenditure_cleaned <- ACFR %>% replace_with_na_all(condition=~.x =="-") %>% 
  select(V1:`2011`) %>% 
  gather("fiscal_year", "amount_in_thousands", -V1) %>% 
  mutate(fiscal_year=as.numeric(fiscal_year), # convert year into numeric
         negative=case_when(substring(amount_in_thousands,1,1)=="(" ~ -1,
                            T ~ 1)) %>% # make sure number in parentheses are converted to negative numbers
  rename(category=V1)%>% 
  mutate(amount_in_thousands=parse_number(amount_in_thousands)) %>% # expenditure amounts are properly converted to numeric
  mutate(amount_in_thousands=amount_in_thousands*negative) %>% 
  select(-negative) %>% 
  mutate(expenditure_category=case_when(category%in%expenditure_category_vec ~ category,
                                    T ~ NA)) %>% # add expenditure_category
  fill(expenditure_category) %>% # fill missing expenditure_category with the previous value
  mutate(subcategory=case_when(category%in%subcategory_vec ~ category,
                               T ~ NA)) %>% # add subcategory
  fill(subcategory) %>% # fill missing subcategory with the previous value
  select(expenditure_category, subcategory, category, fiscal_year, amount_in_thousands) %>% # select columns of interest
  filter(!category%in%c(category[grepl(category, pattern=":")|startsWith(category, "Total")],
                        "Income Taxes, Other (Net of Refunds)")|category=="Total Other Revenues") %>% # drop rows with category totals
  mutate(subcategory=if_else((expenditure_category=="Taxes (Net of Refunds):" & category=="Real Estate Taxes"), "Real Estate Taxes", subcategory)) %>% # correct wrong filling missing using previous values
  rename(Categorical.Citywide.Exp=amount_in_thousands) %>% # rename columns according to the naming convention
  left_join(CPI) %>%
  mutate(Categorical.Citywide.Exp=Categorical.Citywide.Exp*1000,
         Adjusted.Categorical.Citywide.Exp=Categorical.Citywide.Exp*CPI2024/FY_CPI) # calculate inflation-adjusted expenditure

# create a vector for human services agencies
human_services <- c("Commission on Human Rights",
                    "Department of Youth and Community Development",
                    "Department of Small Business Services",
                    "Administration for Children's Services",
                    "Department of Social Services",
                    "Department of Homeless Services",
                    "Human Resources Administration",
                    "Department for the Aging",
                    "Housing Preservation and Development",
                    "Miscellaneous-Payments to Housing Authority",
                    "Department of Health and Mental Hygiene")

# formatting the category names
ACFR_expenditure_cleaned_formatted <- ACFR_expenditure_cleaned %>% 
  mutate(subcategory=
           if_else(!subcategory%in%c("Miscellaneous--Technology Development Corporation",
                                     "Miscellaneous-Contributions Legal Aid",
                                     "Miscellaneous-Criminal Justice Programs",
                                     "Miscellaneous-Other", 
                                     "Hunter Campus Schools",
                                     "Educational Aid", 
                                     "Miscellaneous-Payments to Transit Authority", 
                                     "Miscellaneous-Payments to Private Bus Companies",
                                     "Miscellaneous-Payments to Housing Authority", 
                                     "Judgments and Claims",
                                     "Benefit Payments",
                                     "Lease Payments", 
                                     "General Debt Service Fund:",
                                     "Nonmajor Debt Service Funds:"),
                   substring(subcategory,5), subcategory), # drop numbers before subcategories
         category=
           if_else(!category%in%c("Miscellaneous--Technology Development Corporation",
                            "Miscellaneous-Contributions Legal Aid",
                            "Miscellaneous-Criminal Justice Programs",
                            "Miscellaneous-Other", 
                            "Hunter Campus Schools",
                            "Educational Aid", 
                            "Miscellaneous-Payments to Transit Authority", 
                            "Miscellaneous-Payments to Private Bus Companies",
                            "Miscellaneous-Payments to Housing Authority", 
                            "Judgments and Claims",
                            "Benefit Payments",
                            "Lease Payments", 
                            "Miscellaneous -Building Aid Revenue Bonds",
                            "Miscellaneous -Future Tax Secured"),
                   substring(category,5), category)) %>% # drop numbers before categories
  mutate(expenditure_category=gsub(pattern=":", replacement="", x=expenditure_category),
         subcategory=gsub(pattern=":", replacement="", x=subcategory)) %>%  # remove colons
  mutate(human_services=case_when(category%in%human_services ~ T,
                                  T~F), # add a column to indication if the expenditure belongs to human services
         category=gsub("Administrator - ", "Administrator-", category),
         category=gsub("Administrator- ", "Administrator-", category)) %>% # correct irregular use of space
  rename(agency=category) # rename category as agency to match Checkbook expenditure data


# sum in different ways to check for errors

ACFR_expenditure_sum_by_year <- ACFR_expenditure_cleaned_formatted %>% 
  group_by(fiscal_year) %>% 
  summarise(sum_by_year=sum(Categorical.Citywide.Exp, na.rm = T))

ACFR_expenditure_sum_by_expenditure_category <- ACFR_expenditure_cleaned_formatted %>% 
  group_by(expenditure_category) %>% 
  summarise(sum_by_year=sum(Categorical.Citywide.Exp, na.rm = T))


ACFR_expenditure_sum_by_subcategory <- ACFR_expenditure_cleaned_formatted %>% 
  group_by(expenditure_category, subcategory) %>% 
  summarise(sum_by_subcategory=sum(Categorical.Citywide.Exp, na.rm = T))


# save the cleaned and formatted data to a csv file
write.csv(ACFR_expenditure_cleaned_formatted, "ACFR_expenditure_long.csv")



############################ Checkbook Expenditure ############################


# classification of ACFR expenditure
ACFR_classification <- ACFR_expenditure_cleaned_formatted %>% 
  select(expenditure_category, subcategory, agency) %>% 
  distinct()

# agencies in expenditure detail but not in category of expenditure long
ACFR_categories <- unique(ACFR_expenditure_cleaned_formatted$agency)

# read checkbook expenditure data
raw_budget <- fread("raw_budget.csv")

# format checkbook expenditure agency names
expenditure_detail <- raw_budget %>% mutate(agency=str_to_title(agency),
                                            agency=gsub("Of ", "of ", agency),
                                            agency=gsub("For ", "for ", agency),
                                            agency=gsub("And ", "and ", agency),
                                            agency=gsub("The ", "the ", agency),
                                            agency=gsub("Nyc ", "NYC ", agency),
                                            agency=gsub("It ", "IT ", agency),
                                            agency=gsub("Dept ", "Department ", agency),
                                            agency=gsub("# ", "#", agency),
                                            agency=gsub(" - ", "-", agency),
                                            agency=gsub("District Attorney-", "District Attorney ", agency),
                                            agency=gsub("District Attorney -", "District Attorney ", agency),
                                            agency=gsub("Office of Prosecution-Special Narcotics", "Office of Prosecutor -- Special Narcotics", agency),
                                            agency=gsub("Office of Administrative Trials & Hearings", "Office of Administrative Trials and Hearings", agency),
                                            agency=gsub("Commission On Human Rights", "Commission on Human Rights", agency),
                                            agency=gsub("City University of New York", "City University of New York-Community Colleges", agency),
                                            agency=gsub("Health and Hospitals Corporation", "New York City Health and Hospitals Corporation", agency),
                                            agency=gsub("Mac", "MAC", agency),
                                            agency=gsub("Department of Veterans' Services", "Department of Veterans Service", agency),
                                            agency=gsub("  ", " ", agency),
                                            agency=gsub("MAC Debt Service Funding", "Debt Service", agency),
                                            agency=gsub("Department of Consumer Affairs", "Department of Consumer & Worker Protection", agency),
                                            agency=gsub("Department of Mental Health", "Department of Health and Mental Hygiene", agency),
                                            agency=gsub("General Reserve", "Debt Service", agency)) %>% 
  filter(!agency%in%c("Prior Year Payables",
                      "Energy Adjustment",
                      "Procurement Peg",
                      "Federal Actions",
                      "IT Efficiency Savings",
                      "Federal / State Actions",
                      "Fringe Benefits Cost Containment",
                      "Department of Employment",
                      "Lease Adjustment")) %>% # drop agencies that don't match ACFR
  left_join(ACFR_classification) %>% # align with ACFR classification
  select(V1, expenditure_category, subcategory, agency:committed) # select and reorder columns of interest


# make expense_category, department, budget name properly capitalized with special words left all cap
# Keep - PS and - OTPS all cap
expenditure_detail_proper <- expenditure_detail %>%
  mutate(department=str_to_title(department),
         department=gsub("- Ps", "- PS", department),
         department=gsub("-Ps", "-PS", department),
         department=gsub("- Otps", "- OTPS", department),
         department=gsub(" For ", " for ", department),
         department=gsub(" And ", " and ", department),
         department=gsub("The ", "the ", department),
         department=gsub("Nyc", "NYC", department),
         department=gsub("On ", "on ", department),
         department=gsub("Of ", "of ", department),
         department=gsub("To ", "to ", department),
         department=gsub("O.t.p.s.", "O.T.P.S.", department),
         department=gsub("-Otps", "-OTPS", department),
         department=gsub("Hra", "HRA", department),
         expense_category=str_to_title(expense_category),
         expense_category=gsub(" For ", " for ", expense_category),
         expense_category=gsub(" And ", " and ", expense_category),
         expense_category=gsub("The ", "the ", expense_category),
         expense_category=gsub("Nyc", "NYC", expense_category),
         expense_category=gsub("On ", "on ", expense_category),
         expense_category=gsub("Of ", "of ", expense_category),
         expense_category=gsub("To ", "to ", expense_category),
         expense_category=gsub("Pmts ", "PMTS ", expense_category),
         expense_category=gsub("Hhc", "HHC ", expense_category),
         expense_category=gsub("Ta ", "TA ", expense_category),
         expense_category=gsub("Jtpa-", "JTPA-", expense_category),
         expense_category=gsub("Pmnts ", "PMNTS ", expense_category),
         expense_category=gsub("Be ", "be", expense_category),
         expense_category=gsub("Fcb", "FCB", expense_category),
         expense_category=gsub("-Boe", "-BOE", expense_category),
         expense_category=gsub("Otps", "OTPS", expense_category),
         expense_category=gsub("Cuny", "CUNY", expense_category),
         expense_category=gsub("Mta", "MTA", expense_category),
         expense_category=gsub("H.h.c.", "H.H.C.", expense_category),
         expense_category=gsub("Sbita", "SBITA", expense_category),
         expense_category=gsub("Hra", "HRA", expense_category),
         budget_name=str_to_title(budget_name),
         budget_name=gsub("- Ifa", "- IFA", budget_name),
         budget_name=gsub("- Bbm", "- BBM", budget_name),
         budget_name=gsub("-Tl", "-TL", budget_name),
         budget_name=gsub("- Tfa", "- TFA", budget_name),
         budget_name=gsub("- Fphny", "- FPHNY", budget_name),
         budget_name=gsub("- Cdbg", "- CDBG", budget_name),
         budget_name=gsub("Mmis", "MMIS", budget_name),
         budget_name=gsub("Hhc", "HHC", budget_name),
         budget_name=gsub("- Otps", "- OTPS", budget_name),
         budget_name=gsub(" Ii", " II", budget_name),
         budget_name=gsub(" It", " IT", budget_name),
         budget_name=gsub("Xxxi", "XXXI", budget_name),
         budget_name=gsub("Hra", "HRA", budget_name),
         budget_name=gsub("Asap", "ASAP", budget_name),
         budget_name=gsub(" For ", " for ", budget_name),
         budget_name=gsub(" And ", " and ", budget_name),
         budget_name=gsub("The ", "the ", budget_name),
         budget_name=gsub("Nyc", "NYC", budget_name),
         budget_name=gsub("On ", "on ", budget_name),
         budget_name=gsub("Of ", "of ", budget_name),
         budget_name=gsub("To ", "to ", budget_name))

# save cleaned Checkbook expenditure data into csv file
write.csv(expenditure_detail_proper, "expenditure_detail.csv")

# check discrepancy between ACFR agency and checkbook agency
#discrepancy <- unique(expenditure_detail$agency[!expenditure_detail$agency%in%ACFR_classification$agency])

# compare expenditure details with ACFR
expenditure_by_ACFR_category <- ACFR_expenditure_cleaned_formatted %>% 
  group_by(expenditure_category) %>% 
  summarise(sum_by_category=sum(Categorical.Citywide.Exp, na.rm = T)) 

expenditure_detail_by_expenditure_category <- expenditure_detail %>% 
  group_by(expenditure_category) %>% 
  summarise(total=round(sum(committed, na.rm = T)/1000))

diff <- expenditure_detail_by_expenditure_category %>% 
  right_join(expenditure_by_ACFR_category) %>% 
  rename(checkbook=total, ACFR=sum_by_category) %>% 
  mutate(diff= checkbook-ACFR,
         percent_diff=round(diff/ACFR,2)*100)

# sum of difference
sum(diff$diff, na.rm = T)

# Checkbook expenditure sum by year 
expenditure_detail_by_year <- expenditure_detail %>% 
  group_by(year) %>% 
  summarise(total=sum(committed, na.rm = T))


