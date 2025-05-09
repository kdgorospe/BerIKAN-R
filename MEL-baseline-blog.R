# MEL baseline blog

rm(list=ls())
library(tidyverse)
library(reshape2)
library(corrplot)
library(car)

#dat <- read.csv(file = "~C:\\Users\\Kevin Gorospe\\OneDrive - Resonance\\Ber-IKAN\\BerIKAN-R\\Data\\01 Fishers Main Dataset 18052024.csv")
dat <- read.csv(file = '~/R/BerIKAN-R/Data/01 Fishers Main Dataset 18052024.csv')
# Retrieved from: https://ssgadvisors.sharepoint.com/gs/pdu/BER-IKAN/SitePages/Home.aspx 
# File path: Documents / Technical / MEL / Data Collection / Baseline Data / Data Analysis / Quantitative Data Analysis

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
  rename_with(~ ifelse(grepl("member.of.KUB", .x), "any_group_membership", .x)) %>%
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
# Create some data visualizations - BOXPLOTS SECTION
################################################################################################
# Plot livelihoods data
# Select columns - Remove City, Total Percentage of Livelihoods, and Total Livelihoods
dat_temp <- cbind(dat_location %>% select(-District.City), 
                  dat_livelihood %>% select(-`Total no. of livelihoods`))

# Melt the dataframe to make it long format
df_melted <- dat_temp  %>%
  pivot_longer(cols = -Province, names_to = "variable", values_to = "value") %>%
  mutate_at(vars(matches("variable")), as.factor)

# PLOT LIVELIHOODS
df_melted_livelihood <- df_melted %>% 
  filter(!str_detect(variable, "no\\. of months")) %>%
  filter((!str_detect(variable, "pct income")))

# Set factor levels to order plots
df_melted_livelihood$variable <- factor(df_melted_livelihood$variable, 
                                        levels = c("Fishing", 
                                                   "Fish trading", 
                                                   "Fish processing", 
                                                   "Aquaculture",   
                                                   "Farming",   
                                                   "Other"))

# Set factor level of Province to order x axis: WPP 711 (Kalimantan and Riau) and then WPP 715 (x6)
levels(df_melted_livelihood$Province) <- c("Kalimantan Barat", "Kepulauan Riau",
                                "Gorontalo", "Maluku", "Maluku Utara", "Papua Barat/Barat Daya", "Sulawesi Tengah", "Sulawesi Utara")

# Plot box plots for each column
ggplot(df_melted_livelihood, aes(x = Province, y = value)) +
  #geom_violin() +
  geom_jitter(height = 0.01) +
  labs(x = "Province", y = "Value") +
  facet_wrap(~ variable, scales = "fixed", ncol = 3, nrow = 7) +  # Create separate plots for each column; free_y allows y variable to vary; use ncol to set number of columns
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

plotfile = paste(getwd(), "/plot_livelihoods.pdf", sep = "")
ggsave(filename = plotfile, device = "pdf", width = 11, height = 8.5)

################################################################################################
# PLOT NO. OF MONTHS PER LIVELIHOOD
df_melted_livelihood_months <- df_melted %>% 
  filter(str_detect(variable, "no\\. of months")) %>%
  mutate(value = replace_na(value, 0))

# Set factor levels to order plots
df_melted_livelihood_months$variable <- factor(df_melted_livelihood_months$variable, 
                                               levels = c("no. of months fishing", 
                                                          "no. of months fish trading", 
                                                          "no. of months fish processing", 
                                                          "no. of months aquaculture",  
                                                          "no. of months farming",      
                                                          "no. of months other"))


# Set factor level of Province to order x axis: WPP 711 (Kalimantan and Riau) and then WPP 715 (x6)
levels(df_melted_livelihood_months$Province) <- c("Kalimantan Barat", "Kepulauan Riau",
                                                  "Gorontalo", "Maluku", "Maluku Utara", "Papua Barat/Barat Daya", "Sulawesi Tengah", "Sulawesi Utara")

# Plot box plots for each column
ggplot(df_melted_livelihood_months, aes(x = Province, y = value)) +
  #geom_violin() +
  geom_jitter() +
  labs(x = "Province", y = "No. of Months") +
  facet_wrap(~ variable, scales = "fixed", ncol = 3) +  # Create separate plots for each column; free_y allows y variable to vary; use ncol to set number of columns
  scale_y_continuous(limits = c(0, 12.0), breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  #theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1, size = 13),
        axis.text.x = element_text(angle = 50, hjust = 1, size = 13),
        axis.title = element_text(size = 13),
        strip.text.x = element_text(size = 13),
        plot.margin = margin(l = 15))


plotfile = paste(getwd(), "/plot_livelihoods_months.png", sep = "")
ggsave(filename = plotfile, width = 11, height = 8.5, units = "in", dpi = 300, bg = "white")

#ggsave("plot_coeffs_net_income.png", width = 8, height = 6, units = "in", dpi = 300, bg = "white")

################################################################################################
# PLOT PERCENT INCOME PER LIVELIHOOD
df_melted_livelihood_income <- df_melted %>% 
  filter((str_detect(variable, "pct income"))) %>%
  mutate(variable = str_replace_all(variable, "pct", "%")) %>%
  mutate(value = replace_na(value, 0))

# Set factor levels to order plots
df_melted_livelihood_income$variable <- factor(df_melted_livelihood_income$variable, 
                             levels = c("% income from fishing",
                                        "% income from fish trading",   
                                        "% income from fish processing",
                                        "% income from aquaculture",    
                                        "% income from farming",        
                                        "% income from other"))


# Set factor level of Province to order x axis: WPP 711 (Kalimantan and Riau) and then WPP 715 (x6)
levels(df_melted_livelihood_income$Province) <- c("Kalimantan Barat", "Kepulauan Riau",
                                                  "Gorontalo", "Maluku", "Maluku Utara", "Papua Barat/Barat Daya", "Sulawesi Tengah", "Sulawesi Utara")

# Plot box plots for each column
ggplot(df_melted_livelihood_income, aes(x = Province, y = value)) +
  #geom_violin() +
  geom_jitter(height = 5) +
  labs(x = "Province", y = "Percent of total income") +
  facet_wrap(~ variable, scales = "fixed", ncol = 3) +  # Create separate plots for each column; free_y allows y variable to vary; use ncol to set number of columns
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
  #theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1, size = 12),
        axis.text.x = element_text(angle = 50, hjust = 1, size = 12),
        axis.title = element_text(size = 12),
        strip.text.x = element_text(size = 12))


plotfile = paste(getwd(), "/plot_livelihoods_income.png", sep = "")
ggsave(filename = plotfile, width = 11, height = 8.5, units = "in", dpi = 300, bg = "white")
################################################################################################
# Plot number of fishers data
# Select columns - Remove City
dat_temp <- cbind(dat_location %>% select(-District.City), 
                  dat_fishers)

# Melt the dataframe to make it long format
df_melted <- dat_temp  %>%
  pivot_longer(cols = -Province, names_to = "variable", values_to = "value") %>%
  mutate_at(vars(matches("variable")), as.factor)

# Set factor levels to order plots
# df_melted$variable <- factor(df_melted$variable, 
#                              levels = c(""))

# Set factor level of Province to order x axis: WPP 711 (Kalimantan and Riau) and then WPP 715 (x6)
levels(df_melted$Province) <- c("Kalimantan Barat", "Kepulauan Riau",
                                "Gorontalo", "Maluku", "Maluku Utara", "Papua Barat/Barat Daya", "Sulawesi Tengah", "Sulawesi Utara")

# Plot box plots for each column
ggplot(df_melted, aes(x = Province, y = value)) +
  geom_violin() +
  labs(x = "Province", y = "Value") +
  facet_wrap(~ variable, scales = "free_y", ncol = 3, nrow = 7) +  # Create separate plots for each column; free_y allows y variable to vary; use ncol to set number of columns
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

plotfile = paste(getwd(), "/plot_no-of-fishers.pdf", sep = "")
#ggsave(filename = plotfile, p, device = "pdf", width = 11, height = 8.5)
ggsave(filename = plotfile, device = "pdf", width = 11, height = 8.5)

################################################################################################
# Plot income data
# REMINDER for fin_decider_gender_num: Male = 1; Female = 2; Both = 3
dat_temp <- cbind(dat_location %>% 
                    select(-District.City), 
                  dat_income %>% 
                    select(c(net_income, fin_ability_cont, fin_decider_gender_num))) %>%
  filter(!is.na(fin_decider_gender_num)) %>%
  ## Consider removing one crazy outlier with NET income of negative Rp 21,275,000 %>%
  filter(net_income > -20000000)

# Melt the dataframe to make it long format
df_melted <- dat_temp  %>%
  pivot_longer(cols = -Province, names_to = "variable", values_to = "value") %>%
  mutate_at(vars(matches("variable")), as.factor)


# Set factor level of Province to order x axis: WPP 711 (Kalimantan and Riau) and then WPP 715 (x6)
levels(df_melted$Province) <- c("Kalimantan Barat", "Kepulauan Riau",
                                "Gorontalo", "Maluku", "Maluku Utara", "Papua Barat/Barat Daya", "Sulawesi Tengah", "Sulawesi Utara")

# Plot box plots for each column
ggplot(df_melted, aes(x = Province, y = value)) +
  geom_violin() +
  labs(x = "Province", y = "Value") +
  facet_wrap(~ variable, scales = "free_y", ncol = 3, nrow = 7) +  # Create separate plots for each column; free_y allows y variable to vary; use ncol to set number of columns
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

plotfile = paste(getwd(), "/plot_income_vars.pdf", sep = "")
ggsave(filename = plotfile, device = "pdf", width = 11, height = 8.5)

png("plot_net_income_jitter.png")
# Plot just income
ggplot(df_melted %>% filter(variable == "net_income"), aes(x = Province, y = value)) +
  #geom_violin() +
  geom_jitter() +
  labs(x = "Province", y = "Value") +
  facet_wrap(~ variable, scales = "free_y", ncol = 3, nrow = 7) +  # Create separate plots for each column; free_y allows y variable to vary; use ncol to set number of columns
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))
dev.off()

png("plot_meet_fin_needs_jitter.png")
# Plot just financial ability to meet needs
ggplot(df_melted %>% filter(variable == "fin_ability_cont"), aes(x = Province, y = value)) +
  #geom_violin() +
  geom_jitter(height = 0.2) +
  labs(x = "Province", y = "Value") +
  facet_wrap(~ variable, scales = "free_y", ncol = 3, nrow = 7) +  # Create separate plots for each column; free_y allows y variable to vary; use ncol to set number of columns
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))
dev.off()

################################################################################################
################################################################################################
# RUN REGRESSION: 

# POTENTIAL RESPONSE VARIABLES: (1) ABILITY TO COVER HOUSEHOLD NEEDS; (2) VARIOUS INCOME MEASUREMENTS (GROSS, NET AFTER FISHING EXPENSES, AFTER HH EXPENSES)

# POTENTIAL PREDICTORS: 
# "Who makes financial decisions in your household" column #339
# KUB/coop membership (binary) column #212
# Trend in catch (integer) column #182
# For catch trend, 1: Declined a lot / 2: Declined slightly / 3: Stayed the same / 4: Improved slightly / 5: Improved heavily
# Total number (genderless) of fishers column # 72
# Total number (genderless) of processors column #76
# Total number of livelihoods column #67
# Income balance after expenditures (continuous) column #337
# DUMMY VARIABLES FOR MALE, FEMALE, VS BOTH AS FINANCIAL DECIDERS
#### NOTE: Only 8 households have a HH member who previously received financial trainings

# CREATE AND CLEAN DATAFRAME FOR ANALYSIS
# Cleaning is done up top; below is just binding the columns of interest
# Include all POTENTIAL response and predictors here, will filter later in dat_regress
# But also, only include variables that have sufficient variation (e.g., other_insurance_binary only includes 35 "Yes" so too skewed)
dat_analysis <- cbind(dat_head %>% select(age_hh_head),
                      dat_income %>% select(c(fish_consumed, fin_ability_cat, fin_ability_cont, gross_income, net_income, income_minus_hh_expenses, male_decider, female_decider, both_decider, fin_decider_gender)),
                      dat_fishers %>% select(`Total no. of fishers`),
                      dat_livelihood %>% select(contains("pct") | contains("no.")),
                      dat_trend %>% select(catch_trend_cont),
                      dat_fin_access %>% select(any_group_membership, num_fin_accounts, access_to_fin_accounts_binary, savings_binary, loan_amount, workers_insurance_binary, health_insurance_binary, gov_health_insurance_binary, priv_health_insurance_binary, gov_vessel_engine, gov_other_fishing_subsidy), 
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
# Calculate correlation coefficient using full data frame
# Select the predictor variables by setting response variable (income_pp or fin_ability_cont, etc)
predictor_vars <- dat_analysis %>%
  select(where(is.numeric)) %>%
  select(-fin_ability_cont)

# Calculate Pearson's correlation for testing continuous vars
cor_coeffs <- cor(predictor_vars, use = "pairwise.complete.obs")
write.csv(cor_coeffs, file="Figure_Cor_coeffs_0.5.csv")
cor_coeffs_test <- abs(cor_coeffs) > 0.5
write.csv(cor_coeffs_test, file="Figure_Cor_coeffs_0.5.test.csv")

# Spearman's for testing ranked, ordered vars
cor_spear <- cor(predictor_vars, method="spearman", use = "pairwise.complete.obs")
write.csv(cor_spear, file="Figure_spearCor_coeffs_0.5.csv")
cor_spear_test <- abs(cor_spear) > 0.5
write.csv(cor_spear_test, file="Figure_spearCor_coeffs_0.5.csv")

# Save correlation plots
pdf(file = "Figure_correlationVisual.pdf")
corrplot(cor_coeffs, insig="p-value")
dev.off()

pdf(file = "Figure_correlationSpearVisual.pdf")
corrplot(cor_spear, insig="p-value")
dev.off()

# Correlated vars - i.e., only include one of each set
# age_hh_head highly correlated with max_age
# total no. of livelihoods correlates with all pct income from X and no. of months doing X EXCEPT no. of months doing fishing  
# i.e., - can include total no. of livelihoods AND no. of months doing fishing, but everything else is correlated
# All financial access info is somewhat correlated; loan amount and savings_binary are least correlated so try including both of these 
# All versions of the health insurance variable is correlated - only include one

# TEST FOR MULTICOLINEARITY
# Fit the linear model
model <- lm(fin_ability_cont ~ 
              age_hh_head +
              net_income + 
              `Total no. of livelihoods` + 
              `no. of months fishing` +
              catch_trend_cont + 
              fish_consumed_pp +
              any_group_membership +
              savings_binary + 
              loan_amount +
              health_insurance_binary +
              workers_insurance_binary +
              hh_size +
              fisher_skills_training +
              female_decider +
              gov_vessel_engine +
              gov_other_fishing_subsidy,
            data = dat_analysis)

# Calculate VIF for each predictor
vif_values <- vif(model)

# Print VIF values
print(vif_values)

# Create a bar plot of VIF values
pdf(file = "Figure_VIF_plot.pdf")
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2) # Add a vertical line at VIF = 5
dev.off()

# All VIF < 5 (pass multi-colinearity test)

##########################################################################
# CODE FOR net_income as response variable - i.e., income from fishing minus operational expenses
# Include all POTENTIAL response and predictors here, will filter later in dat_regress
# CREATE predictors data frame and center / standardize variabless
# Question: what explains higher vs lower income?
dat_predictors <- dat_analysis %>%
  select(c(age_hh_head,
           income_minus_hh_expenses, 
           `Total no. of livelihoods`,
           `no. of months fishing`,
           catch_trend_cont,
           fish_consumed_pp,
           any_group_membership,
           fisher_skills_training,
           num_fin_accounts,
           savings_binary,
           health_insurance_binary,
           gov_health_insurance_binary,
           priv_health_insurance_binary,
           workers_insurance_binary,
           access_to_fin_accounts_binary,
           loan_amount,
           savings_binary,
           gov_vessel_engine,
           gov_other_fishing_subsidy,
           hh_size,
           male_decider,
           both_decider, 
           female_decider))

# Scale variables by two standard deviations (the default in R is to scale by one SD; see Gelman and Hill pg 56)
covars_sd <- apply(dat_predictors, MARGIN=2, FUN=sd)
dat_scale <- scale(dat_predictors, center=TRUE, scale=2*covars_sd) %>%
  as.data.frame(I(.)) # I() prevents R from interpreting it as a vector

# RUN REGRESSION
dat_regress <- bind_cols(dat_analysis %>% select(net_income), dat_scale)

model <- lm(net_income ~ 
              `no. of months fishing` +
              age_hh_head +
              gov_vessel_engine +
              fisher_skills_training +
              health_insurance_binary +
              workers_insurance_binary,
            data = dat_regress)

# Print model summary
summary(model)


# Extract coefficient estimates and confidence intervals
# Remove "Intercept"
coef <- coef(model)[-1]
conf_int <- confint(model)[-1,]

# Create a data frame for plotting
plot_data <- data.frame(
  term = names(coef),
  estimate = coef,
  lower = conf_int[, 1],
  upper = conf_int[, 2]) %>%
  # Clean up coefficient names
  mutate(term = if_else(term == "`no. of months fishing`", "Fishing effort (# of months per year)", term)) %>%
  mutate(term = if_else(term == "age_hh_head", "Age of household head", term)) %>%
  mutate(term = if_else(term == "gov_vessel_engine", "Boat engine government assistance program", term)) %>%
  mutate(term = if_else(term == "fisher_skills_training", "Participation in fisher skills training", term)) %>%
  mutate(term = if_else(term == "health_insurance_binary", "Having access to health insurance", term)) %>%
  mutate(term = if_else(term == "workers_insurance_binary", "Having access to work insurance", term)) 

# Rescale coefficient estimates
# Reorder axis by increasing coefficient estimate
scaling_factor <- 1e5
plot_data <- plot_data %>%
  mutate(term = factor(term)) %>%
  mutate(estimate = estimate / scaling_factor,
         lower = lower / scaling_factor,
         upper = upper / scaling_factor, 
         term = fct_reorder(term, estimate))

#pdf(file = "plot_coeffs_net_income.pdf")
#png(file = "plot_coeffs_net_income.png")
# Plot coefficient estimates with error bars
ggplot(plot_data, aes(y = term, x = estimate)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, color = "black", size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(y = "", x = "Effect on Monthly Net Income") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1, size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12, margin = margin(t=20)))
ggsave("plot_coeffs_net_income.png", width = 8, height = 6, units = "in", dpi = 300, bg = "white")
#dev.off()

# With color scale:
ggplot(plot_data, aes(y = term, x = estimate, color = estimate)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, size = 1) +
  geom_point(size = 3) +
  scale_color_gradient2(
    low = "darkred", 
    mid = "gray",
    high = "darkgreen", 
    midpoint = 0
  ) +
  labs(y = "", x = "Effect on Monthly Net Income (in 100,000 IDR)", color = "Effect") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1, size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12, margin = margin(t=20)))
ggsave("plot_coeffs_net_income-COLOR-SCALE.png", width = 8, height = 6, units = "in", dpi = 300, bg = "white")



##########################################################################
# CODE FOR fin_ability_cont as response variable - i.e., ability to meet household financial needs
# Include all POTENTIAL response and predictors here, will filter later in dat_regress
# CREATE predictors data frame and center / standardize variabless
# Use either gross_income and/or income_minus_hh_expenses as a predictor of ability to meet financial needs
# Story: income and ability to meet financial needs not a 1:1 // in other words, besides income, what else affects financial ablity?
dat_predictors <- dat_analysis %>%
  select(c(age_hh_head,
           gross_income,
           net_income,
           income_minus_hh_expenses, 
           `Total no. of livelihoods`,
           `no. of months fishing`,
           catch_trend_cont,
           fish_consumed,
           fish_consumed_pp,
           any_group_membership,
           fisher_skills_training,
           num_fin_accounts,
           savings_binary,
           health_insurance_binary,
           gov_health_insurance_binary,
           priv_health_insurance_binary,
           workers_insurance_binary,
           access_to_fin_accounts_binary,
           loan_amount,
           savings_binary,
           gov_vessel_engine,
           gov_other_fishing_subsidy,
           hh_size,
           male_decider,
           both_decider, 
           female_decider))

# Scale variables by two standard deviations (the default in R is to scale by one SD; see Gelman and Hill pg 56)
covars_sd <- apply(dat_predictors, MARGIN=2, FUN=sd)
dat_scale <- scale(dat_predictors, center=TRUE, scale=2*covars_sd) %>%
  as.data.frame(I(.)) # I() prevents R from interpreting it as a vector

# RUN REGRESSION
dat_regress <- bind_cols(dat_analysis %>% select(fin_ability_cont), dat_scale)

model <- lm(fin_ability_cont ~ 
              net_income + 
              `Total no. of livelihoods` + 
              catch_trend_cont + 
              #fish_consumed_pp +
              fish_consumed +
              savings_binary +
              loan_amount +
              hh_size,
            data = dat_regress)
# NOTES:
## Removing the following because of near-zero effect and less interesting: any_group_membership and fisher_skills_training (rather, test this pre vs post)
## No effect of access to financial accounts (loans or savings accounts) underscores need for financial training
## Keep story simple for now - i.e., Wait until we can include financial training attendance as a variable (possibly midpoint of program), before including gender of household financial decision maker

# Print model summary
summary(model)

# Extract coefficient estimates and confidence intervals
# Remove "Intercept"
coef <- coef(model)[-1]
conf_int <- confint(model)[-1,]

# Create a data frame for plotting
plot_data <- data.frame(
  term = names(coef),
  estimate = coef,
  lower = conf_int[, 1],
  upper = conf_int[, 2]) %>%
  # Clean coefficient names
  mutate(term = if_else(term == "net_income", "Net income", term)) %>%
  mutate(term = if_else(term == "`Total no. of livelihoods`", "# of livelihoods", term)) %>%
  mutate(term = if_else(term == "catch_trend_cont", "Increasing fish catch", term)) %>%
  mutate(term = if_else(term == "fish_consumed", "Household fish consumption", term)) %>%
  mutate(term = if_else(term == "hh_size", "# of household members", term)) %>%
  mutate(term = if_else(term == "loan_amount", "Total household loans", term)) %>%
  mutate(term = if_else(term == "savings_binary", "Having access to a savings account", term)) %>%
  mutate(term = as.factor(term)) %>%
  mutate(term = fct_reorder(term, estimate))

# Save to pdf/png or just plot to RStudio
#pdf(file = "plot_coeffs_financial_needs_met.pdf")
#png(file = "plot_coeffs_financial_needs_met.png")
# Plot coefficient estimates with error bars
ggplot(plot_data, aes(y = term, x = estimate)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, color = "black", size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(y = "", x = "Effect on Household Financial Security") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1, size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12, margin = margin(t=20)))
ggsave("plot_coeffs_financial_needs_met.png", width = 8, height = 6, units = "in", dpi = 300, bg = "white")
#dev.off()

# With color scale:
ggplot(plot_data, aes(y = term, x = estimate, color = estimate)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, size = 1) +
  geom_point(size = 3) +
  scale_color_gradient2(
    low = "darkred", 
    mid = "gray",
    high = "darkgreen", 
    midpoint = 0
  ) +
  labs(y = "", x = "Effect on Household Financial Security", color = "Effect") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1, size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12, margin = margin(t=20)))
ggsave("plot_coeffs_financial_needs_met-COLOR-SCALE.png", width = 8, height = 6, units = "in", dpi = 300, bg = "white")
