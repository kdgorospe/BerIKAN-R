# Analyses for contextualization of MEL baseline report

rm(list=ls())
library(tidyverse)
library(reshape2)
library(corrplot)
library(car)

dat <- read.csv(file = "C:\\Users\\Kevin Gorospe\\OneDrive - Resonance\\Ber-IKAN\\BerIKAN-R\\Data\\01 Fishers Main Dataset 18052024.csv")
# Retrieved from: https://ssgadvisors.sharepoint.com/gs/pdu/BER-IKAN/SitePages/Home.aspx 

################################################################################################
# Break up dat into smaller components - e.g., location, social, environmental, etc
# Removing qualitative data, can ask MEL team to translate this as supporting material 

# Location data columns: 3, 4, 8, 9, 
dat_location <- dat[,c(8, 9)] %>% 
  mutate(across(everything(), as.factor))# province, district city; N = 100 to 202, depending on province

# Household head demographics: 13, 14, 16
dat_head <- dat[,c(13, 14, 16, 18:48)] %>% # gender, age, education, religion of HH head
  rename_with(~ ifelse(grepl("Age.of.the.Household.Head", .x), "age_hh_head", .x)) %>%
  mutate(max_age = pmax(age_hh_head, Age.of.Household.Member.1, Age.of.Household.Member.2, Age.of.Household.Member.3, Age.of.Household.Member.4, Age.of.Household.Member.5, Age.of.Household.Member.6, Age.of.Household.Member.7, na.rm = TRUE))
# Also additional info on gender, age, education of individual HH members, but ignoring for now
# NOTE: correlation matrix shows max age highly correlated with age of hh head, don't include

# Relationship with household head
dat_hh_head <- dat[,15] # 1 = interviewee is the household head; may want to use this for filtering 
# Note: only three individuals identified as someone other than HH head

# Household size and assets: 
# total number (male v female)
dat_hh <- dat[,c(45:47, 200:211)] %>%
  rename_with(~ ifelse(grepl("Total.Number.of.Household.Members", .x), "hh_size", .x))

################################################################################################
# Livelihoods data:
dat_livelihood <- dat[,49:68] 

# Clean data
dat_livelihood <- dat_livelihood %>%
  #mutate_at(vars(!(starts_with("If")|starts_with("Total"))), as.factor) %>%
  mutate_at(vars(contains("months")), ~ifelse(. > 12, 12, .)) %>%
  rename_with(~ ifelse(grepl("Artisanal.Fishing", .x), "Fishing", .x)) %>%
  rename_with(~ ifelse(grepl("Fish.buying", .x), "Fish trading", .x)) %>%
  rename_with(~ ifelse(grepl("Fish.Processing", .x), "Fish processing", .x)) %>%
  rename_with(~ ifelse(grepl("Aquaculture", .x), "Aquaculture", .x)) %>%
  rename_with(~ ifelse(grepl("Farming", .x), "Farming", .x)) %>%
  rename_with(~ ifelse(grepl("Other", .x), "Other", .x)) %>%
  rename_with(~ gsub("Livelihood.A.", "fishing", .x), contains("Livelihood.A.")) %>%
  rename_with(~ gsub("Livelihood.B.", "fish trading", .x), contains("Livelihood.B.")) %>%
  rename_with(~ gsub("Livelihood.C.", "fish processing", .x), contains("Livelihood.C.")) %>%
  rename_with(~ gsub("Livelihood.D.", "aquaculture", .x), contains("Livelihood.D.")) %>%
  rename_with(~ gsub("Livelihood.E.", "farming", .x), contains("Livelihood.E.")) %>%
  rename_with(~ gsub("Livelihood.F.", "other", .x), contains("Livelihood.F.")) %>%
  rename_with(~ gsub("If.yes..how.many.months.of.the.year.do.you.engage.in.this.activity..", "no. of months ", .x), 
              contains("If.yes..how.many.months.of.the.year.do.you.engage.in.this.activity..")) %>%
  rename_with(~ gsub("If.yes..what.proportion.of.your.income.is.supported.by.this.activity..in.percent....", "pct income from ", .x), 
              contains("If.yes..what.proportion.of.your.income.is.supported.by.this.activity..in.percent....")) %>%
  rename_with(~ ifelse(grepl("Total.Number", .x), "Total no. of livelihoods", .x)) %>%
  # remove trailing periods "."
  rename_with(~ gsub("[.]$", "", .x)) %>% 
  # remove total percentage of livelihood
  select(-Total.Percentage.of.Livelihood)

################################################################################################
# Number of fishers
dat_fishers <- dat[,c(69:72)] # number of fishers, men, women, children

# Clean data
dat_fishers <- dat_fishers %>%
  rename_with(~ ifelse(grepl("Male", .x), "No. of male fishers", .x)) %>%
  rename_with(~ ifelse(grepl("Female", .x), "No. of female fishers", .x)) %>%
  rename_with(~ ifelse(grepl("Children", .x), "No. of child fishers", .x)) %>%
  rename_with(~ ifelse(grepl("Total", .x), "Total no. of fishers", .x)) 

################################################################################################
# Number of processors
dat_processors <- dat[,c(73:76)] %>%
  rename_with(~ ifelse(grepl("Total.number", .x), "processors", .x))

################################################################################################
# Number of vessels
dat_vessels <- dat[,c(77:78, 82)]

################################################################################################
# Household fisher formalization (vessel registration, proficiency certificate, kusuka card - fisher ID card for gov benefits)
dat_formal <- dat[,c(78:84, 117:118)]

# Fishing gears and equipment
dat_gears <- dat[,c(117:173)]

# Fishing effort - time and distance
dat_effort <- dat[,c(174:176, 179:181)]

# Perception of fish catch trends
dat_trend <- dat[,c(182:197)] %>%
  # Clean column name
  rename_with(~ ifelse(grepl("Compared.to", .x), "catch_trend", .x)) %>%
  # Convert catch trend to ordered categorical variables
  mutate(catch_trend_cat = factor(catch_trend,
                                  levels = c("Declined a lot", "Declined slightly", "Stayed the same", "Improved slightly", "Improved heavily"),
                                  ordered = TRUE)) %>%
  # Create option for treating catch trend as a continuous variable: assumption that categories are equidistant
  mutate(catch_trend_cont = case_when(
    catch_trend == "Declined a lot" ~ 1,
    catch_trend == "Declined slightly" ~ 2,
    catch_trend == "Stayed the same" ~ 3,
    catch_trend == "Improved slightly" ~ 4,
    catch_trend == "Improved heavily" ~ 5
  ))

# Access to business and financial resources - loans, insurance, savings, public assistance
dat_fin_access <- dat[,c(212:313)] %>%
  # Yes/No member of KUB, fishers group, or cooperative
  rename_with(~ ifelse(grepl("member.of.KUB", .x), "any_biz_group_membership", .x)) %>%
  rename_with(~ ifelse(grepl("Total.HH.s.accesses.to.financial.institution", .x), "num_hh_members_fin_accounts", .x)) %>%
  rename_with(~ ifelse(grepl("Total.household.those.have.access.to.financial.institution", .x), "access_to_fin_accounts_binary", .x)) %>%
  rename_with(~ ifelse(grepl("Female..who.has.bank.account", .x), "bank_female", .x)) %>% # May want to turn this into binary instead of "total number of females"
  rename_with(~ ifelse(grepl("Number.of.HH.those.have.saving.to.any.financial.institution", .x), "savings_binary", .x)) %>% 
  #rename_with(~ ifelse(grepl("Does.your.household.have.insurance.", .x), "insurance_binary", .x)) %>% 
  rename_with(~ ifelse(grepl("Anount.of.loan.from.Bank", .x), "loan_amount", .x)) %>% 
  # Clean loan amount column
  mutate(loan_amount = gsub("Rp|,", "", loan_amount)) %>%
  mutate(loan_amount = as.numeric(loan_amount)) %>%
  # Create column for whether anyone in the household has an account in Bank, microfinance, coop, or other financial institution (0: No / 1: Yes)
  mutate(num_fin_accounts = 
           Total.household.members.who.have.bank.account +
           Total.household.members.who.have.account.in.microfinance.institution +
           Total.household.members.who.have.account.in.cooperative +
           Total.household.members.who.have.account.in.financial.program.of.NGO.or.government +
           Total.household.members.who.have.account.in.other.financial.institution
  ) %>%
  # Clean and include as predictor, access to insurance column: currently split across multiple columns
  mutate(insurance_binary = case_when(
    Yes..BPJS.Kesehatan..Kartu.Indonesia.Sehat..JKN. == 1 | Yes..Private.health.insurance == 1 | Yes..Life.insurance == 1 | Workers.insurance.or.BPJS.Ketenagakerjaan ==1 | Other.insurance == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(health_insurance_binary = case_when(
    Yes..BPJS.Kesehatan..Kartu.Indonesia.Sehat..JKN. == 1 | Yes..Private.health.insurance == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(other_insurance_binary = case_when(
    Yes..Life.insurance == 1 | Other.insurance == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  rename_with(~ ifelse(grepl("Workers.insurance.or.BPJS.Ketenagakerjaan", .x), "workers_insurance_binary", .x)) %>%
  rename_with(~ ifelse(grepl("Yes..BPJS.Kesehatan..Kartu.Indonesia.Sehat..JKN.", .x), "gov_health_insurance_binary", .x)) %>%
  rename_with(~ ifelse(grepl("Yes..Private.health.insurance", .x), "priv_health_insurance_binary", .x)) %>%
  # Include government assistance variable
  rename_with(~ ifelse(grepl("Vessel.Machine", .x), "gov_vessel_engine", .x)) %>%
  # Not enough 1's in the other government fishing subsidies, so combine across all of them
  mutate(gov_other_fishing_subsidy = case_when(
    Vessel.Body == 1 | Fishing.Gear.Support == 1 | Supporting.fishing.equipment == 1 ~ 1,
    TRUE ~ 0
  ))


################################################################################################
# HH finances - fish consumption, gross income from fish, operational expense, total expenses, net_income, decision maker, include gender of head of household (column 13)
dat_income <- dat[,c(13, 314, 333:339)] %>%
  # Clean column names
  rename_with(~ ifelse(grepl("Average.amount.of.fish.consumed", .x), "fish_consumed", .x)) %>% # in kilograms
  rename_with(~ ifelse(grepl("Estimated.gross.income.per.month.for.all.fish.types", .x), "gross_income", .x)) %>%
  rename_with(~ ifelse(grepl("Estimated.fishing.operational.cost.per.", .x), "fish_expenses", .x)) %>%
  rename_with(~ ifelse(grepl("Estimasi.Value.Addedd", .x), "net_income", .x)) %>%
  rename_with(~ ifelse(grepl("Estimated.monthly.household.expenditure", .x), "hh_expenses", .x)) %>%
  rename_with(~ ifelse(grepl("Estimated.monthly.income.balance", .x), "income_minus_hh_expenses", .x)) %>%
  rename_with(~ ifelse(grepl("Ability.to.cover.household.needs", .x), "fin_ability", .x)) %>%
  rename_with(~ ifelse(grepl("Gender.of.the.Household.Head", .x), "head_gender", .x)) %>%
  rename_with(~ ifelse(grepl("Who.makes.the.financial.decisions", .x), "fin_decider", .x)) %>%
  # TRIM EXTRA SPACES (found one in financial decision column)
  mutate(across(everything(), str_trim)) %>%
  # Convert financial security to ordered categorical variable
  mutate(fin_ability_cat = factor(fin_ability,
                                  levels = c("Very difficult", "Difficult", "Easy", "Quite easy", "Very easy"), # Confirmed order with Dodi
                                  ordered = TRUE)) %>%
  # Create option for treating fin_security as a continuous variable: assumption that categories are equidistant
  mutate(fin_ability_cont = case_when(
    fin_ability == "Very difficult" ~ 1,
    fin_ability == "Difficult" ~ 2,
    fin_ability == "Easy" ~ 3,
    fin_ability == "Quite easy" ~ 4,
    fin_ability == "Very easy" ~ 5
  )) %>%
  # Clean income and expenses columns
  mutate(net_income = gsub("Rp|,", "", net_income)) %>%
  mutate(net_income = as.numeric(net_income)) %>%
  mutate(gross_income = gsub("Rp|,", "", gross_income)) %>%
  mutate(gross_income = as.numeric(gross_income)) %>%
  mutate(hh_expenses = gsub("Rp|,", "", hh_expenses)) %>%
  mutate(hh_expenses = as.numeric(hh_expenses)) %>%
  mutate(income_minus_hh_expenses = gsub("Rp|,", "", income_minus_hh_expenses)) %>%
  mutate(income_minus_hh_expenses = as.numeric(income_minus_hh_expenses)) %>%
  # Identify gender of financial decider
  # NOTE: Only one case where household head is not male
  mutate(fin_decider_gender = case_when(
    fin_decider == "Household head" & head_gender == 1 ~ "Male",
    fin_decider == "Spouse of household head" & head_gender == 2 ~ "Male",
    fin_decider == "Household head" & head_gender == 2 ~ "Female",
    fin_decider == "Spouse of household head" & head_gender == 1 ~"Female",
    fin_decider == "Household head with spouse" ~ "Both",
    TRUE ~ NA)) %>%
  # Create numeric option for plotting gender of financial decider
  mutate(fin_decider_gender_num = case_when(
    fin_decider_gender == "Male" ~ 1,
    fin_decider_gender == "Female" ~ 2,
    fin_decider_gender == "Both" ~ 3, 
    TRUE ~ NA)) %>%
  # Create dummy variables
  mutate(male_decider = case_when(
    fin_decider_gender == "Male" ~ 1,
    TRUE ~ 0)) %>%
  mutate(female_decider = case_when(
    fin_decider_gender == "Female" ~ 1,
    TRUE ~ 0)) %>%
  mutate(both_decider = case_when(
    fin_decider_gender == "Both" ~ 1,
    TRUE ~ 0)) %>%
  # Fish consumption column to numeric
  mutate(fish_consumed = as.numeric(fish_consumed))

################################################################################################
# Training history and needs; include gender of head of household (column 13)
dat_training <- dat[,c(13, 340:378)] %>%
  # Create column for whether anyone in the household (0:No / 1:Yes) received fisher skills training (== 1)
  mutate(fisher_skills_training = if_any(contains("Training.Type"), ~ . == 1) * 1) %>% 
  # Create column for whether anyone in the household received fish processing skills training (== 2)
  mutate(processing_training = if_any(contains("Training.Type"), ~ . == 2) * 1) %>%
  # Create column for whether anyone in the household received financial training (== 3)
  mutate(fin_training = if_any(contains("Training.Type"), ~ . == 3) * 1) %>%
  # Create column for whether anyone in the household received business training (== 4)
  mutate(business_training = if_any(contains("Training.Type"), ~ . == 4) * 1) %>%
  # Create column for whether anyone in the household received gender training (== 5)
  mutate(gender_training = if_any(contains("Training.Type"), ~ . == 5) * 1) %>%
  # Create column for whether anyone in the household received environmental management training (== 6)
  mutate(em_training = if_any(contains("Training.Type"), ~ . == 6) * 1) %>%
  # Create column for whether anyone in the household received ETP training (== 7)
  mutate(etp_training = if_any(contains("Training.Type"), ~ . == 7) * 1) %>%
  select(contains("_training")) 

# mutate into factors and summarizes
summary(dat_training %>% mutate(across(contains("_training"), as.factor)))

# General rule of thumb is there should be at least 10-20% observations in the smaller category for binary variable to be useful
# i.e., of the training variables, should probably only include fisher skills training as a predictor variable
# fisher_skills_training processing_training fin_training business_training
# 0:981                  0:1270              0:1303       0:1307           
# 1:330                  1:  41              1:   8       1:   4           
# gender_training em_training etp_training
# 0:1306          0:1308      0:1244      
# 1:   5          1:   3      1:  67    
################################################################################################
# ETP
dat_etp <- dat[,c(379:395)]

################################################################################################
################################################################################################
# CREATE AND CLEAN DATAFRAME FOR ANALYSIS
# Cleaning is done up top; below is just binding the columns of interest
# Include all POTENTIAL response and predictors here, will filter later once regression analysis is decided
# But also, only include variables that have sufficient variation (e.g., other_insurance_binary only includes 35 "Yes" so too skewed)
dat_analysis <- cbind(dat_head %>% select(age_hh_head),
                      dat_income %>% select(c(fish_consumed, fin_ability_cat, fin_ability_cont, gross_income, net_income, income_minus_hh_expenses, male_decider, female_decider, both_decider, fin_decider_gender)),
                      dat_fishers %>% select(`Total no. of fishers`),
                      dat_livelihood %>% select(contains("pct") | contains("no.")),
                      dat_trend %>% select(catch_trend_cont),
                      dat_fin_access %>% select(any_biz_group_membership, num_fin_accounts, access_to_fin_accounts_binary, savings_binary, loan_amount, workers_insurance_binary, health_insurance_binary, gov_health_insurance_binary, priv_health_insurance_binary, gov_vessel_engine, gov_other_fishing_subsidy), 
                      dat_hh %>% select(hh_size),
                      dat_training %>% select(fisher_skills_training)) %>%
  ## Consider removing one crazy outlier with net income minus hh expenses of negative Rp 21,275,000 %>%
  ## Need to do this here rather than during cleaning, otherwise datasets won't c-bind
  filter(income_minus_hh_expenses > -20000000) %>%
  # Remove cases where gender of financial decider unknown (e.g., sibling/parent/grandparent)
  filter(!is.na(fin_decider_gender)) %>%
  mutate(fish_consumed_pp = fish_consumed / hh_size)


# FIRST, TEST FOR CORRELATIONS AMONG PREDICTOR VARIABLES
#############################
# For now, calculate correlation coefficient using full data frame (all numeric vars)
# Note: Removing other livelihoods because significant number of NAs (only fishing has vast majority participating)   
# Removing highly zero-inflated vars (e.g., workers insurance binary, )
# Remvoving redundant vars (e.g., removing female decider, both decider [already have male decider]; removing gov and private health insurance [already have health insurance binary])

predictor_vars <- dat_analysis %>%
  select(where(is.numeric)) %>%
  select(age_hh_head, hh_size,  
  fish_consumed, 
  fin_ability_cont, gross_income, net_income, income_minus_hh_expenses, male_decider,
  `Total no. of fishers`, `pct income from fishing`, `no. of months fishing`, `Total no. of livelihoods`,
  catch_trend_cont,               
  any_biz_group_membership, num_fin_accounts, access_to_fin_accounts_binary, savings_binary, loan_amount,                           
  health_insurance_binary, gov_vessel_engine, gov_other_fishing_subsidy,                       
  fisher_skills_training)

# Later, can select the predictor variables by setting response variable (income_pp or fin_ability_cont, etc)
# predictor_vars <- dat_analysis %>%
#   select(where(is.numeric)) %>%
#   select(-fin_ability_cont)

# Calculate Pearson's correlation for testing continuous vars
cor_coeffs <- cor(predictor_vars, use = "pairwise.complete.obs")
#write.csv(cor_coeffs, file="Figure_Cor_coeffs_0.5.csv")
cor_coeffs_test <- abs(cor_coeffs) > 0.5
#write.csv(cor_coeffs_test, file="Figure_Cor_coeffs_0.5.test.csv")

# Spearman's for testing ranked, ordered vars
cor_spear <- cor(predictor_vars, method="spearman", use = "pairwise.complete.obs")
#write.csv(cor_spear, file="Figure_spearCor_coeffs_0.5.csv")
cor_spear_test <- abs(cor_spear) > 0.5
#write.csv(cor_spear_test, file="Figure_spearCor_coeffs_0.5.csv")

# Save correlation plots
pdf(file = "Figure_correlationVisual.pdf")
corrplot(cor_coeffs, insig="p-value")
dev.off()

pdf(file = "Figure_correlationSpearVisual.pdf")
corrplot(cor_spear, insig="p-value")
dev.off()
