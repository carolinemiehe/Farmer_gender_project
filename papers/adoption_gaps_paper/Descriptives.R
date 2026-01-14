rm(list=ls())
path <- getwd()
path
library(readr)
baseline_farmers <- read.csv(paste(path,"/../../data/baseline/farmer/baseline_farmers.csv",sep="/"), stringsAsFactors=TRUE)

library(dplyr)
library(knitr)

trim <- function(var,dataset,trim_perc=.025){
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  return(dataset)}

#Asset ownership - Q10, Q11, Q

#Q10 and Q11
summary(baseline_farmers$Check2.check.maize.q10)
baseline_farmers$Check2.check.maize.q10[baseline_farmers$Check2.check.maize.q10 == 999] <- NA
baseline_farmers$Check2.check.maize.q10[baseline_farmers$Check2.check.maize.q10 == "n/a"] <- NA
baseline_farmers$Check2.check.maize.q10 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q10))
baseline_farmers$Check2.check.maize.q10[baseline_farmers$Check2.check.maize.q10 < 0] <- NA


baseline_farmers$Check2.check.maize.q11[baseline_farmers$Check2.check.maize.q11 == 999] <- NA
baseline_farmers$Check2.check.maize.q11[baseline_farmers$Check2.check.maize.q11 == "n/a"] <- NA
baseline_farmers$Check2.check.maize.q11 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q11))
table(baseline_farmers$Check2.check.maize.q11)

baseline_farmers$num_shops <- baseline_farmers$Check2.check.maize.q11
baseline_farmers$distance_agroshops <- baseline_farmers$Check2.check.maize.q10
baseline_farmers$distance_agroshops <- as.numeric(as.character(baseline_farmers$distance_agroshops))
baseline_farmers$num_shops <- as.numeric(as.character(baseline_farmers$num_shops))

baseline_farmers$distance_agroshops[baseline_farmers$distance_agroshops == 0] <- NA
baseline_farmers$num_shops[baseline_farmers$num_shops == 0] <- NA


baseline_farmers$num_shops         <- baseline_farmers$Check2.check.maize.q11
baseline_farmers$distance_agroshops <- baseline_farmers$Check2.check.maize.q10

baseline_farmers$num_shops[baseline_farmers$num_shops %in% c(999,"999","n/a","NA","")] <- NA
baseline_farmers$distance_agroshops[baseline_farmers$distance_agroshops %in% c(999,"999","n/a","NA","")] <- NA

baseline_farmers$num_shops <- as.numeric(as.character(baseline_farmers$num_shops))
baseline_farmers$distance_agroshops <- as.numeric(as.character(baseline_farmers$distance_agroshops))

#Q22
table(baseline_farmers$Check2.check.maize.q22)
baseline_farmers$Check2.check.maize.q22[baseline_farmers$Check2.check.maize.q22 == 999] <- NA
baseline_farmers$Check2.check.maize.q22[baseline_farmers$Check2.check.maize.q22 == "n/a"] <- NA
table(baseline_farmers$Check2.check.maize.q22)
baseline_farmers$Check2.check.maize.q22 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q22))
summary(baseline_farmers$Check2.check.maize.q22)

# values that could be <0.1 o >200 diventano NA
baseline_farmers$Check2.check.maize.q22[baseline_farmers$Check2.check.maize.q22 < 0.1 |baseline_farmers$Check2.check.maize.q22 > 200] <- NA
summary(baseline_farmers$Check2.check.maize.q22)
mean(baseline_farmers$Check2.check.maize.q22, na.rm = TRUE)
median(baseline_farmers$Check2.check.maize.q22, na.rm = TRUE)
sd(baseline_farmers$Check2.check.maize.q22, na.rm = TRUE)
summary(baseline_farmers$Check2.check.maize.q22)
# hist(baseline_farmers$Check2.check.maize.q22,
#      main = "Household Land Available for Crop Production",
#      xlab = "Acres",
#      col = "skyblue",
#      breaks = 20)


baseline_farmers$maize_plot_area <- baseline_farmers$Check2.check.maize.q22
summary(baseline_farmers$maize_plot_area )

#Q27 integer "On how many fields (plots) did you grow maize in the second season (entoigo) of 2020 (either stand alone or mixed)?"	 >=1 and .<=5

baseline_farmers$Check2.check.maize.q27 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q27))
baseline_farmers$Check2.check.maize.q27[baseline_farmers$Check2.check.maize.q27 < 1 |
                                          baseline_farmers$Check2.check.maize.q27 > 5] <- NA
summary(baseline_farmers$Check2.check.maize.q27)
mean(baseline_farmers$Check2.check.maize.q27, na.rm = TRUE)
median(baseline_farmers$Check2.check.maize.q27, na.rm = TRUE)
sd(baseline_farmers$Check2.check.maize.q27, na.rm = TRUE)
# hist(baseline_farmers$Check2.check.maize.q27,
#      main = "Number of Fields Used for Maize (Second Season 2020)",
#      xlab = "Number of Fields",
#      col = "skyblue",
#      breaks = 5)  # da 1 a 5

baseline_farmers$plots_maize <- baseline_farmers$Check2.check.maize.q27


#Q29 decimal "What is the area of this **${plot_select_name}** maize field during the second season of **2020 (entoigo 2020)**?" **ACRES** 	.<=${q22}	This land can not be greater total land used for crops					
baseline_farmers$Check2.check.maize.q29[baseline_farmers$Check2.check.maize.q29 == 999] <- NA
baseline_farmers$Check2.check.maize.q29[baseline_farmers$Check2.check.maize.q29 == "n/a"] <- NA
baseline_farmers$Check2.check.maize.q29[baseline_farmers$Check2.check.maize.q29 < 0] <- NA
table(baseline_farmers$Check2.check.maize.q29)
baseline_farmers$Check2.check.maize.q29 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q29))
summary(baseline_farmers$Check2.check.maize.q29)

baseline_farmers$r_maize_plot_area <- baseline_farmers$Check2.check.maize.q29

summary(baseline_farmers$r_maize_plot_area)

#Q39 How much was the cost of 1 kg of this seed? (in UGX)
summary(baseline_farmers$Check2.check.maize.q39)
baseline_farmers$seed_price_kg_ugx <- baseline_farmers$Check2.check.maize.q39
baseline_farmers$seed_price_kg_ugx[baseline_farmers$seed_price_kg_ugx %in% c("999","n/a","NA","")] <- NA
baseline_farmers$seed_price_kg_ugx <- as.numeric(as.character(baseline_farmers$seed_price_kg_ugx))
baseline_farmers$seed_price_kg_ugx[baseline_farmers$seed_price_kg_ugx < 0 | baseline_farmers$seed_price_kg_ugx > 15000] <- NA
summary(baseline_farmers$seed_price_kg_ugx)


#Q38 How much seed did you use  on **${plot_select_name}** in the second season (entoigo) of 2020? **(in kg)**?
baseline_farmers$seed_qty_kg <- baseline_farmers$Check2.check.maize.q38
baseline_farmers$seed_qty_kg[baseline_farmers$seed_qty_kg %in% c("999","n/a","NA","")] <- NA
baseline_farmers$seed_qty_kg <- as.numeric(as.character(baseline_farmers$seed_qty_kg))
summary(baseline_farmers$seed_qty_kg)

#Q38xQ39 seed_total_cost_ugx
baseline_farmers$seed_total_cost_ugx <- baseline_farmers$seed_qty_kg * baseline_farmers$seed_price_kg_ugx

summary(baseline_farmers$seed_total_cost_ugx)

sum(!is.na(baseline_farmers$seed_qty_kg))
sum(!is.na(baseline_farmers$seed_price_kg_ugx))
sum(!is.na(baseline_farmers$seed_total_cost_ugx))


#Q43 -fertilizers
baseline_farmers$dap_npk_applied <- tolower(as.character(baseline_farmers$Check2.check.maize.q43))
baseline_farmers$dap_npk_applied[baseline_farmers$dap_npk_applied %in% c("999","n/a","na","")] <- NA
baseline_farmers$dap_npk_applied <- ifelse(baseline_farmers$dap_npk_applied %in% c("yes","y","1"), 1,
                                           ifelse(baseline_farmers$dap_npk_applied %in% c("no","n","0"), 0, NA))
table(baseline_farmers$dap_npk_applied)

#Q44 -fertilizers
baseline_farmers$urea_applied <- tolower(as.character(baseline_farmers$Check2.check.maize.q44))
baseline_farmers$urea_applied[baseline_farmers$urea_applied %in% c("999","n/a","na","")] <- NA
baseline_farmers$urea_applied <- ifelse(baseline_farmers$urea_applied %in% c("yes","y","1"), 1,
                                        ifelse(baseline_farmers$urea_applied %in% c("no","n","0"), 0, NA))
table(baseline_farmers$urea_applied)

#Q47 used any pesticides/herbicides/fungicides on the plot? (yes/no)
baseline_farmers$chemicals_applied <- tolower(as.character(baseline_farmers$Check2.check.maize.q47))
baseline_farmers$chemicals_applied[baseline_farmers$chemicals_applied %in% c("999","n/a","na","")] <- NA
baseline_farmers$chemicals_applied <- ifelse(baseline_farmers$chemicals_applied %in% c("yes","y","1"), 1,
                                             ifelse(baseline_farmers$chemicals_applied %in% c("no","n","0"), 0, NA))
table(baseline_farmers$chemicals_applied)


#INFORMATION AND TECHNOLOGY

# Q17 Highest level of education of household head (numeric version)
baseline_farmers$education_head <- tolower(as.character(baseline_farmers$Check2.check.maize.q17))
baseline_farmers$education_head[baseline_farmers$education_head %in% c("999","n/a","na","")] <- NA


library(dplyr)

baseline_farmers$education_head_num <- as.numeric(dplyr::recode(
  baseline_farmers$education_head,
  "a" = 0,  # No formal education
  "b" = 0,  # Some primary
  "c" = 1,  # Finished primary
  "d" = 1,  # Some secondary
  "e" = 1,  # Finished secondary
  "f" = 1,  # Higher than secondary
  "g" = NA_real_  # Other → NA
))


summary(baseline_farmers$education_head_num)


#LABOUR MARKET IMPERFECTIONS - TIME POVERTY AND CARE RESPONSABILITY
#Q18 -household size (number of members)
baseline_farmers$household_size <- baseline_farmers$Check2.check.maize.q18
baseline_farmers$household_size[baseline_farmers$household_size %in% c("999","98","n/a","NA","")] <- NA
baseline_farmers$household_size <- as.numeric(as.character(baseline_farmers$household_size))
baseline_farmers$household_size[baseline_farmers$household_size < 1 | baseline_farmers$household_size > 25] <- NA
summary(baseline_farmers$household_size)
table(baseline_farmers$household_size, useNA = "ifany")

# Q25a
baseline_farmers$quality_seed_used <- baseline_farmers$Check2.check.maize.q25a
baseline_farmers$quality_seed_used <- tolower(as.character(baseline_farmers$quality_seed_used))
baseline_farmers$quality_seed_used[baseline_farmers$quality_seed_used %in% c("n/a","na","", "99","999")] <- NA
baseline_farmers$quality_seed_used <- ifelse(
  baseline_farmers$quality_seed_used %in% c("yes","y","true","1"), 1,
  ifelse(baseline_farmers$quality_seed_used %in% c("no","n","false","0"), 0, NA)
)
baseline_farmers$quality_seed_used <- as.numeric(baseline_farmers$quality_seed_used)
table(baseline_farmers$quality_seed_used, useNA = "ifany")

#Laws and Social Tradition - Q13 , Q14, Q15, Q16 (Marital status of household head)
#Q13(relationship with the household-head)
library(dplyr)
table(baseline_farmers$Check2.check.maize.q13)
baseline_farmers$relationship <- tolower(as.character(baseline_farmers$Check2.check.maize.q13))

baseline_farmers$relationship[baseline_farmers$relationship %in% c("n/a","na")] <- NA

baseline_farmers$relationship_hh_num <- ifelse(baseline_farmers$relationship == "a", 1, 0)

table(baseline_farmers$relationship_hh_num, useNA="ifany")

#Q13 the respondent is the household head
baseline_farmers$respondent_is_hh <- ifelse(
  is.na(baseline_farmers$relationship) |
    grepl("^(self|head|household head)$", baseline_farmers$relationship),
  1L, 0L
)
table(baseline_farmers$respondent_is_hh)




#Q14(Age of household head)
baseline_farmers$Check2.check.maize.q14 <- as.character(baseline_farmers$Check2.check.maize.q14)
baseline_farmers$Check2.check.maize.q14[
  baseline_farmers$Check2.check.maize.q14 %in% c("999", "9999", "n/a", "NA", "", " ")] <- NA
baseline_farmers$Check2.check.maize.q14 <- as.numeric(baseline_farmers$Check2.check.maize.q14)
baseline_farmers$Check2.check.maize.q14[
  baseline_farmers$Check2.check.maize.q14 < 15 | baseline_farmers$Check2.check.maize.q14 > 99] <- NA
baseline_farmers$hh_age <- baseline_farmers$Check2.check.maize.q14
summary(baseline_farmers$hh_age)


#Q15(Gender of household head)
baseline_farmers$Check2.check.maize.q15 <- as.character(baseline_farmers$Check2.check.maize.q15)
baseline_farmers$Check2.check.maize.q15[baseline_farmers$Check2.check.maize.q15 %in% c("999", "n/a", "NA", "", " ")
] <- NA

baseline_farmers$hh_gender_num <- ifelse(
  baseline_farmers$Check2.check.maize.q15 == "Male", 1,
  ifelse(baseline_farmers$Check2.check.maize.q15 == "Female", 0, NA)
)

table(baseline_farmers$hh_gender_num, useNA = "ifany")


#Q16 (status)
table(baseline_farmers$Check2.check.maize.q16, useNA = "ifany")
baseline_farmers$Check2.check.maize.q16[baseline_farmers$Check2.check.maize.q16 %in% c("n/a","N/A","NA"," ")] <- NA
table(baseline_farmers$Check2.check.maize.q16, useNA = "ifany")
baseline_farmers$hh_marital_status <- tolower(as.character(baseline_farmers$Check2.check.maize.q16))

baseline_farmers$hh_marital_status[baseline_farmers$hh_marital_status %in% c("n/a","na","")] <- NA

baseline_farmers$hh_marital_status_num <- ifelse(baseline_farmers$hh_marital_status == "a", 1,
                                                 ifelse(baseline_farmers$hh_marital_status %in% c("b","c","d","e"), 0, NA))
table(baseline_farmers$hh_marital_status_num, useNA="ifany")



#MARKET ORIENTATION AND RISK PREFERENCES  Q53, Q54, Q55

#Q53 (Did you sell any maize that you harvested on this plot during the second season of 2020?)
baseline_farmers$maize_sold <- baseline_farmers$Check2.check.maize.q53
baseline_farmers$maize_sold[baseline_farmers$maize_sold %in% c("999","98","n/a","NA","")] <- NA
baseline_farmers$maize_sold <- ifelse(baseline_farmers$maize_sold == "Yes", 1,
                                      ifelse(baseline_farmers$maize_sold == "No", 0, NA))
summary(baseline_farmers$maize_sold)
table(baseline_farmers$maize_sold, useNA = "ifany")
mean(baseline_farmers$maize_sold, na.rm = TRUE) * 100  # % sold

#Q54 (How many bags of maize did you sell from this *plot* in the second season (entoigo) of 2020?)
baseline_farmers$bags_sold <- baseline_farmers$Check2.check.maize.q54
baseline_farmers$bags_sold[baseline_farmers$bags_sold %in% c("999","n/a","NA")] <- NA
baseline_farmers$bags_sold <- as.numeric(as.character(baseline_farmers$bags_sold))

summary(baseline_farmers$bags_sold)
table(baseline_farmers$bags_sold)

#Q55 - bags price
baseline_farmers$bag_price <- baseline_farmers$Check2.check.maize.q55
baseline_farmers$bag_price[baseline_farmers$bag_price %in% c("999","n/a","NA","")] <- NA
baseline_farmers$bag_price <- as.numeric(as.character(baseline_farmers$bag_price))
summary(baseline_farmers$bag_price)
baseline_farmers$bag_price <- baseline_farmers$bag_price / 1000
table(baseline_farmers$bag_price, useNA = "ifany")
#put 0 for the ones that did not sell anything
baseline_farmers <- baseline_farmers %>%
  mutate(
    bags_sold = ifelse(maize_sold == 0 & is.na(bags_sold), 0, bags_sold),
    bag_price = ifelse(maize_sold == 0 & is.na(bag_price), 0, bag_price),
    maize_income = ifelse(is.na(bags_sold) | is.na(bag_price),
                          0, bags_sold * bag_price)
  )



#OUTPUT VARIABLES 
baseline_farmers$Check2.check.maize.q50[baseline_farmers$Check2.check.maize.q50 %in% c(999, "999", "n/a", "NA", "", " ")] <- NA
baseline_farmers$Check2.check.maize.q51[baseline_farmers$Check2.check.maize.q51 %in% c(999, "999", "n/a", "NA", "", " ")] <- NA

baseline_farmers$Check2.check.maize.q50 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q50))
baseline_farmers$Check2.check.maize.q51 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q51))
baseline_farmers$yield_inkg <- baseline_farmers$Check2.check.maize.q50 * baseline_farmers$Check2.check.maize.q51

aggregate(yield_inkg ~ hh_gender_num, data = baseline_farmers, mean, na.rm = TRUE)
        

table(is.na(baseline_farmers$yield_inkg), baseline_farmers$hh_gender_num)


#yield per acre (PRODUCTIVITY)
baseline_farmers$yield_per_acre <- baseline_farmers$yield_inkg / baseline_farmers$r_maize_plot_area
summary(baseline_farmers$yield_per_acre)
baseline_farmers$yield_per_acre[baseline_farmers$yield_per_acre %in% c("999","n/a","NA","", 99900)] <- NA
aggregate(yield_per_acre ~ hh_gender_num, data = baseline_farmers, mean, na.rm = TRUE)
#analysis of productivity
#hist(baseline_farmers$yield_per_acre,
     #main = "Distribution of yield_per_acre",
     #xlab = "Yield per acre",
     #col = "lightblue",
     #breaks = 30)



library(moments)
skewness(baseline_farmers$yield_per_acre, na.rm = TRUE)
skewness(baseline_farmers$maize_income, na.rm = TRUE)
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}

baseline_farmers$yield_per_acre_ihs <- ihs(baseline_farmers$yield_per_acre)
summary(baseline_farmers$yield_per_acre_ihs)
#hist(baseline_farmers$yield_per_acre_ihs,
     #main = "Distribution di IHS(yield_per_acre)",
     #col = "lightgreen",
     #breaks = 30)


#MAIZE INCOME (bags sold × bag price)
baseline_farmers$maize_income <- with(baseline_farmers,
                                      ifelse(is.na(bags_sold) | is.na(bag_price), 0, bags_sold * bag_price)
)
table(is.na(baseline_farmers$maize_income), baseline_farmers$hh_gender_num)
summary(baseline_farmers$maize_income)

#with this definition of income also people that did not sell anything are included
table(baseline_farmers$maize_income)
summary(baseline_farmers$maize_income)
baseline_farmers$maize_income[baseline_farmers$maize_income %in% c("999","n/a","NA","", 99900)] <- NA
aggregate(maize_income ~ hh_gender_num, data = baseline_farmers, mean, na.rm = TRUE)

baseline_farmers$maize_income_ihs <- ihs(baseline_farmers$maize_income)
summary(baseline_farmers$maize_income_ihs)
table(baseline_farmers$maize_income_ihs)
#DIVIDED per gender Q15
# hh_gender_num <- trimws(tolower(as.character(baseline_farmers$Check2.check.maize.q15)))  # rimuove spazi e mette in minuscolo
# 
# baseline_farmers$gender <- ifelse(
#   hh_gender_num %in% c("1", "male", "m"), "male",
#   ifelse(hh_gender_num %in% c("0", "female", "f"), "female", NA)
# )
# 
# table(baseline_farmers$gender, useNA = "ifany")

#some variables to be added
#Q42Did you apply organic manure to the soil on this ${plot_select_name}  before planting second season of 2020? 

baseline_farmers$organic_manure_applied <- baseline_farmers$Check2.check.maize.q42
baseline_farmers$organic_manure_applied[baseline_farmers$organic_manure_applied %in% c("999","98","n/a","NA","")] <- NA
baseline_farmers$organic_manure_applied <- ifelse(baseline_farmers$organic_manure_applied == "Yes", 1,
                                                  ifelse(baseline_farmers$organic_manure_applied == "No", 0, NA))
baseline_farmers$organic_manure_applied <- as.numeric(as.character(baseline_farmers$organic_manure_applied))
summary(baseline_farmers$organic_manure_applied)

# Q45. How many times did  you weed this maize garden during the second season on 2020?
baseline_farmers$weed_times <- baseline_farmers$Check2.check.maize.q45
baseline_farmers$weed_times[baseline_farmers$weed_times %in% c("999", "n/a", "NA", "")] <- NA
baseline_farmers$weed_times <- as.numeric(as.character(baseline_farmers$weed_times))
baseline_farmers$weed_times[baseline_farmers$weed_times < 0 | baseline_farmers$weed_times > 5] <- NA

summary(baseline_farmers$weed_times)
table(baseline_farmers$weed_times, useNA = "ifany")

#Q49. Did you re-sow where seeds did not germinate on **${plot_select_name}** in the second season (entoigo) of 2020?

baseline_farmers$resow <- baseline_farmers$Check2.check.maize.q49
baseline_farmers$resow <- tolower(as.character(baseline_farmers$resow))
baseline_farmers$resow[baseline_farmers$resow %in% c("n/a","na","","99","999")] <- NA
baseline_farmers$resow <- ifelse(
  baseline_farmers$resow %in% c("yes","y","true","1"), 1,
  ifelse(baseline_farmers$resow %in% c("no","n","false","0"), 0, NA)
)
baseline_farmers$resow <- as.numeric(baseline_farmers$resow)
table(baseline_farmers$resow, useNA = "ifany")


## Q24: Member of farmer group / cooperative
baseline_farmers$farmer_group_member <- baseline_farmers$Check2.check.maize.q24
baseline_farmers$farmer_group_member[baseline_farmers$farmer_group_member %in% c("98","999","n/a","NA","")] <- NA

baseline_farmers$farmer_group_member <- ifelse(baseline_farmers$farmer_group_member == "Yes", 1,
                                               ifelse(baseline_farmers$farmer_group_member == "No", 0, NA))
table(baseline_farmers$farmer_group_member)


# DESCRIPTIVE TABLE + T-TEST (male vs female) + STARS (BASE)

library(dplyr)  

options(scipen = 999)

variables <- c(
  "distance_agroshops",
  "num_shops",
  "maize_plot_area",
  "plots_maize",
  "household_size",
  "hh_age",
  "education_head_num",
  "relationship_hh_num",
  "respondent_is_hh",
  "hh_marital_status_num",
  "quality_seed_used",
  "farmer_group_member",
  "r_maize_plot_area",
  "seed_total_cost_ugx",
  "dap_npk_applied",
  "urea_applied",
  "chemicals_applied",
  "organic_manure_applied",
  "resow",
  "weed_times",
  "maize_sold",
  "bags_sold",
  "bag_price",
  "yield_inkg",
  "yield_per_acre",
  "yield_per_acre_ihs",
  "maize_income",
  "maize_income_ihs"
)

male_idx   <- baseline_farmers$hh_gender_num == 1
female_idx <- baseline_farmers$hh_gender_num == 0
diff_means <- rep(NA_real_, length(variables))

df_descriptives_male   <- array(NA, dim = c(length(variables), 5))
df_descriptives_female <- array(NA, dim = c(length(variables), 5))
ttest_pvalues          <- rep(NA_real_, length(variables))

for (i in seq_along(variables)) {
  
  v_m <- baseline_farmers[[variables[i]]][male_idx]
  v_f <- baseline_farmers[[variables[i]]][female_idx]
  diff_means[i] <- mean(v_m, na.rm = TRUE) - mean(v_f, na.rm = TRUE)
  
  # MALE descriptives
  df_descriptives_male[i,1] <- mean(v_m, na.rm = TRUE)
  df_descriptives_male[i,2] <- min(v_m,  na.rm = TRUE)
  df_descriptives_male[i,3] <- max(v_m,  na.rm = TRUE)
  df_descriptives_male[i,4] <- sd(v_m,   na.rm = TRUE)
  df_descriptives_male[i,5] <- sum(!is.na(v_m))
  
  # FEMALE descriptives
  df_descriptives_female[i,1] <- mean(v_f, na.rm = TRUE)
  df_descriptives_female[i,2] <- min(v_f,  na.rm = TRUE)
  df_descriptives_female[i,3] <- max(v_f,  na.rm = TRUE)
  df_descriptives_female[i,4] <- sd(v_f,   na.rm = TRUE)
  df_descriptives_female[i,5] <- sum(!is.na(v_f))
  
  # T-TEST 
  if (sum(!is.na(v_m)) >= 2 && sum(!is.na(v_f)) >= 2) {
    tt <- try(t.test(v_m, v_f), silent = TRUE)
    if (!inherits(tt, "try-error")) ttest_pvalues[i] <- tt$p.value
  }
}

# Round (keep as data.frame)
df_descriptives_male   <- as.data.frame(lapply(as.data.frame(df_descriptives_male),   function(x) round(x, 3)))
df_descriptives_female <- as.data.frame(lapply(as.data.frame(df_descriptives_female), function(x) round(x, 3)))

# Robustness to read (same as your approach)
df_descriptives_male   <- as.data.frame(matrix(unlist(df_descriptives_male),   ncol = 5, byrow = FALSE))
df_descriptives_female <- as.data.frame(matrix(unlist(df_descriptives_female), ncol = 5, byrow = FALSE))


# Create stars and formatted string for LyX (e.g., "0.023**")
ttest_stars <- ifelse(is.na(ttest_pvalues), "",
                      ifelse(ttest_pvalues < 0.01, "***",
                             ifelse(ttest_pvalues < 0.05, "**",
                                    ifelse(ttest_pvalues < 0.10, "*", ""))))

diff_fmt <- ifelse(
  is.na(diff_means) | is.na(ttest_pvalues),
  "",
  paste0(
    format(round(diff_means, 2), nsmall = 2),
    ttest_stars
  )
)

df_descriptives_male   <<- df_descriptives_male
df_descriptives_female <<- df_descriptives_female
ttest_pvalues          <<- ttest_pvalues
diff_fmt <<- diff_fmt


# Checks
sapply(baseline_farmers[variables], class)
colSums(is.na(baseline_farmers[, variables]))



#SEED QUALITY INDEX

#TIMING PRODUCTIVITY







#Analysis OLS REGRESSIONS
library (car)
library(lmtest)

#(respondent_is_hh excluded for collinearity)
#yield per acre (no ihs) gender is never significant 
lm1 <- lm(yield_per_acre ~ hh_gender_num, data = baseline_farmers)
summary(lm1)

lm1_semifull <- lm(yield_per_acre ~ hh_gender_num + education_head_num + household_size +
                     hh_age + quality_seed_used + dap_npk_applied + urea_applied +
                     chemicals_applied + r_maize_plot_area, data = baseline_farmers)
summary(lm1_semifull)

lm1_full <- lm(yield_per_acre ~ hh_gender_num + education_head_num + household_size +
                 hh_age + hh_marital_status_num + relationship_hh_num +
                 quality_seed_used + dap_npk_applied + urea_applied + chemicals_applied + distance_agroshops + num_shops + bags_sold + bag_price +
                 maize_income, data = baseline_farmers)
summary(lm1_full)



#lm2 IHS TRANSFORMATION OF YIELD PER ACRE - gender is significant until semifull
lm2 <- lm(yield_per_acre_ihs ~ hh_gender_num, data = baseline_farmers)
summary(lm2)
#Semifull: control for agricultural inputs (it includes the indirect effect of discrimination that it is reflecting in input)
lm2_semifull <- lm(yield_per_acre_ihs ~ hh_gender_num + education_head_num + household_size +
                     hh_age + quality_seed_used + dap_npk_applied + urea_applied +
                     chemicals_applied + r_maize_plot_area, data = baseline_farmers)
summary(lm2_semifull)
resettest(lm2_semifull)
baseline_farmers$age_sq <- baseline_farmers$hh_age^2
#crPlots(lm2_semifull)
lm2_semifull_fix <- lm(yield_per_acre_ihs ~ 
    hh_gender_num + education_head_num + household_size +
    hh_age +
    distance_agroshops + num_shops +
    quality_seed_used + dap_npk_applied + 
    urea_applied +
    organic_manure_applied  + maize_plot_area +
    chemicals_applied + weed_times + resow + farmer_group_member,
  data = baseline_farmers)


summary(lm2_semifull_fix)
resettest(lm2_semifull_fix)
#crPlots(lm2_semifull_fix)

#lm3 Maize-income (maybe esclude seed_total_cost_ugx, maize sold and bags sold(plots_maize)) - gender is significant just when there are no control variables

lm3 <- lm(maize_income ~ hh_gender_num, data = baseline_farmers)
summary(lm3)

lm3_semifull <- lm(maize_income ~ hh_gender_num + r_maize_plot_area +
                     education_head_num + household_size + hh_age + 
                     quality_seed_used + dap_npk_applied + urea_applied +
                     chemicals_applied, data = baseline_farmers)
summary(lm3_semifull)
lm3_full <- lm(maize_income ~ hh_gender_num + r_maize_plot_area +
                 education_head_num + household_size + hh_age +
                 hh_marital_status_num + relationship_hh_num +
                 quality_seed_used + dap_npk_applied + urea_applied + chemicals_applied + distance_agroshops + num_shops,
               data = baseline_farmers)
summary(lm3_full)


#lm4

lm4 <- lm(maize_income_ihs ~ hh_gender_num, data = baseline_farmers)
summary(lm4)


lm4_semifull_fix <- lm(maize_income_ihs ~ 
                         hh_gender_num +
                         log1p(r_maize_plot_area) +
                         education_head_num  + household_size + hh_age + quality_seed_used + 
                         dap_npk_applied + urea_applied + chemicals_applied + 
                         organic_manure_applied + num_shops + distance_agroshops+ maize_plot_area+
                         weed_times + resow + farmer_group_member,
                       data = baseline_farmers)
summary(lm4_semifull_fix)
resettest(lm4_semifull_fix) 



#MULTICOLLINEARITY
vif(lm2_semifull_fix)

vif(lm4_semifull_fix)




#Etheroskedasticity
library(sandwich)
library(lmtest)
bptest(lm2_semifull_fix) #there is (moderate)etheroskedasticity
bptest(lm4_semifull_fix) # there is (strong) etheroskedasticity


coeftest(lm2_semifull_fix, vcov = vcovHC(lm2_semifull_fix, type = "HC3")) #white robust SE robust variance estimator (eteroschedasticity-consistent)
coeftest(lm4_semifull_fix, vcov = vcovHC(lm4_semifull_fix, type = "HC3"))


#Time invariant fixed effect (village or district level)
names(baseline_farmers)
summary(baseline_farmers$catchID)

#table OLS 

#PRODUCTIVITY TABLE with robust SE

library(lmtest)
library(sandwich)

options(scipen = 999)

add_stars <- function(p){
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

cell_coef_se <- function(model, term){
  ct <- tryCatch(
    coeftest(model, vcov = vcovHC(model, type = "HC3")),
    error = function(e) NULL
  )
  if (is.null(ct) || !term %in% rownames(ct)) return(c("—",""))
  
  est <- ct[term,"Estimate"]
  se  <- ct[term,"Std. Error"]
  p   <- ct[term,"Pr(>|t|)"]
  
  c(
    paste0(format(round(est,3), nsmall=3), add_stars(p)),
    paste0("(", format(round(se,3), nsmall=3), ")")
  )
}

vars_prod <- c(
  "hh_gender_num",
  "education_head_num",
  "household_size",
  "hh_age",
  "distance_agroshops",
  "num_shops",
  "quality_seed_used",
  "dap_npk_applied",
  "urea_applied",
  "organic_manure_applied",
  "maize_plot_area",
  "chemicals_applied",
  "weed_times",
  "resow",
  "farmer_group_member"
)


    labels_prod <- c(
      "hh_gender_num"          = "Household head is male (1 = yes)",
      "education_head_num"     = "Household-head finished primary education  (1 = Yes)",
      "household_size"         = "Number of household members",
      "hh_age"                 = "Age of household head in years",
      "distance_agroshops"     = "Distance of homestead to nearest agro-input shop selling maize seed in km",
      "num_shops"              = "Number of agro-input shops in the village or neighborhood",
      "quality_seed_used"      = "The respondent used quality seeds (1 = Yes)",
      "dap_npk_applied"        = "DAP/NPK applied in the randomly selected plot (1 = Yes)",
      "urea_applied"           = "Urea applied in the randomly selected plot (1 = Yes)",
      "organic_manure_applied" = "Organic manure applied in the randomly selected plot (1 = Yes)",
      "maize_plot_area"        = "Available land for crop production in acres",
      "chemicals_applied"      = "Pesticides, herbicides or fungicides applied in the randomly selected plot (1 = Yes)",
      "weed_times"             = "Number of weeding times in the randomly selected plot",
      "resow"                  = "Resowing in the randomly selected plot (1 = Yes)",
      "farmer_group_member"    = "The respondent is part of a farmer group or cooperative (1 = Yes)"
    )
    

form_prod <- as.formula(
  paste("yield_per_acre_ihs ~", paste(vars_prod, collapse = " + "))
)

m_raw    <- lm(yield_per_acre_ihs ~ hh_gender_num, data = baseline_farmers)
m_full   <- lm(form_prod, data = baseline_farmers)
m_male   <- lm(form_prod, data = baseline_farmers, subset = (hh_gender_num==1))
m_female <- lm(form_prod, data = baseline_farmers, subset = (hh_gender_num==0))


# N (Observations) coerenti col sample effettivo dei modelli
N_prod_raw_fmt    <- format(stats::nobs(m_raw),    big.mark = ",")
N_prod_full_fmt   <- format(stats::nobs(m_full),   big.mark = ",")
N_prod_male_fmt   <- format(stats::nobs(m_male),   big.mark = ",")
N_prod_female_fmt <- format(stats::nobs(m_female), big.mark = ",")

N_prod_raw_fmt    <<- N_prod_raw_fmt
N_prod_full_fmt   <<- N_prod_full_fmt
N_prod_male_fmt   <<- N_prod_male_fmt
N_prod_female_fmt <<- N_prod_female_fmt


tab_prod <- data.frame(Label=character(), Raw=character(),
                       Full=character(), Male=character(),
                       Female=character(), stringsAsFactors=FALSE)

for (v in vars_prod) {
  
  raw <- c("—","")
  if (v=="hh_gender_num") raw <- cell_coef_se(m_raw, "hh_gender_num")
  
  full   <- cell_coef_se(m_full, v)
  male   <- cell_coef_se(m_male, v)
  female <- cell_coef_se(m_female, v)
  
  tab_prod <- rbind(
    tab_prod,
    data.frame(Label=labels_prod[v], Raw=raw[1],
               Full=full[1], Male=male[1], Female=female[1]),
    data.frame(Label="", Raw=raw[2],
               Full=full[2], Male=male[2], Female=female[2])
  )
}

# INCOME OLS — Robust SE (HC3)
library(lmtest)
library(sandwich)

options(scipen = 999)

add_stars <- function(p){
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

cell_coef_se <- function(model, term){
  ct <- tryCatch(
    coeftest(model, vcov = vcovHC(model, type = "HC3")),
    error = function(e) NULL
  )
  if (is.null(ct) || !term %in% rownames(ct)) return(c("—",""))
  
  est <- ct[term,"Estimate"]
  se  <- ct[term,"Std. Error"]
  p   <- ct[term,"Pr(>|t|)"]
  
  c(
    paste0(format(round(est,3), nsmall=3), add_stars(p)),
    paste0("(", format(round(se,3), nsmall=3), ")")
  )
}

# regressors 
vars_inc <- c(
  "hh_gender_num","log1p(r_maize_plot_area)","education_head_num","household_size","hh_age",
  "quality_seed_used","dap_npk_applied","urea_applied","chemicals_applied",
  "organic_manure_applied","num_shops","distance_agroshops","weed_times",
  "resow","farmer_group_member"
)

# labels 
lab_inc <- c(
  "hh_gender_num"            = "Household head is male (1 = yes)",
  "log1p(r_maize_plot_area)" = "Area of the randomly selected plot in acres (log)",
  "education_head_num"       = "Household-head finished primary education  (1 = Yes)",
  "household_size"           = "Number of household members",
  "hh_age"                   = "Age of household head in years",
  "quality_seed_used"        = "The respondent used quality seeds (1 = Yes)",
  "dap_npk_applied"          = "DAP/NPK applied in the randomly selected plot (1 = Yes)",
  "urea_applied"             = "Urea applied in the randomly selected plot (1 = Yes)",
  "chemicals_applied"        = "Pesticides, herbicides or fungicides applied in the randomly selected plot (1 = Yes)",
  "organic_manure_applied"   = "Organic manure applied in the randomly selected plot (1 = Yes)",
  "num_shops"                = "Number of agro-input shops in the village or neighborhood",
  "distance_agroshops"       = "Distance of homestead to nearest agro-input shop selling maize seed in km",
  "weed_times"               = "Number of weeding times in the randomly selected plot",
  "resow"                    = "Resowing in the randomly selected plot (1 = Yes)",
  "farmer_group_member"      = "The respondent is part of a farmer group or cooperative (1 = Yes)"
)


# models
# assumes lm4_semifull_fix already exists and is your full model
m_inc_full   <- lm4_semifull_fix
m_inc_male   <- lm(formula(m_inc_full), data = baseline_farmers, subset = (hh_gender_num==1))
m_inc_female <- lm(formula(m_inc_full), data = baseline_farmers, subset = (hh_gender_num==0))
m_inc_raw    <- lm(maize_income_ihs ~ hh_gender_num, data = baseline_farmers)
# --- N (Observations) 
N_inc_raw_fmt    <- format(stats::nobs(m_inc_raw),    big.mark = ",")
N_inc_full_fmt   <- format(stats::nobs(m_inc_full),   big.mark = ",")
N_inc_male_fmt   <- format(stats::nobs(m_inc_male),   big.mark = ",")
N_inc_female_fmt <- format(stats::nobs(m_inc_female), big.mark = ",")

N_inc_raw_fmt    <<- N_inc_raw_fmt
N_inc_full_fmt   <<- N_inc_full_fmt
N_inc_male_fmt   <<- N_inc_male_fmt
N_inc_female_fmt <<- N_inc_female_fmt



# build table object: 2 rows per regressor (coef + se)
tab_inc <- data.frame(
  Label=character(), Raw=character(), Full=character(),
  Male=character(), Female=character(), stringsAsFactors=FALSE
)

for (v in vars_inc) {
  
  raw <- c("—","")
  if (v == "hh_gender_num") raw <- cell_coef_se(m_inc_raw, "hh_gender_num")
  
  full   <- cell_coef_se(m_inc_full, v)
  male   <- cell_coef_se(m_inc_male, v)
  female <- cell_coef_se(m_inc_female, v)
  
  tab_inc <- rbind(
    tab_inc,
    data.frame(Label=lab_inc[v], Raw=raw[1], Full=full[1], Male=male[1], Female=female[1], stringsAsFactors=FALSE),
    data.frame(Label="",         Raw=raw[2], Full=full[2], Male=male[2], Female=female[2], stringsAsFactors=FALSE)
  )
}

#OAXACA BLINDER #refErence==1(different from Jann 2008)

library(oaxaca)


group_var <- "hh_gender_num"

# ---- Productivity ----
y_prod <- "yield_per_acre_ihs"

vars_OB_prod <- c(
  "education_head_num"     = "Household-head finished primary education  (1 = Yes)",
  "household_size"         = "Number of household members",
  "hh_age"                 = "Age of household head in years",
  "distance_agroshops"     = "Distance of homestead to nearest agro-input shop selling maize seed in km",
  "num_shops"              = "Number of agro-input shops in the village or neighborhood",
  "quality_seed_used"      = "The respondent used quality seeds (1 = Yes)",
  "dap_npk_applied"        = "DAP/NPK applied in the randomly selected plot (1 = Yes)",
  "urea_applied"           = "Urea applied in the randomly selected plot (1 = Yes)",
  "organic_manure_applied" = "Organic manure applied in the randomly selected plot (1 = Yes)",
  "maize_plot_area"        = "Available land for crop production in acres",
  "chemicals_applied"      = "Pesticides, herbicides or fungicides applied in the randomly selected plot (1 = Yes)",
  "weed_times"             = "Number of weeding times in the randomly selected plot",
  "resow"                  = "Resowing in the randomly selected plot (1 = Yes)",
  "farmer_group_member"    = "The respondent is part of a farmer group or cooperative (1 = Yes)"
)

x_prod <- setdiff(names(vars_OB_prod), group_var)

df_prod <- baseline_farmers[complete.cases(baseline_farmers[, c(y_prod, group_var, x_prod)]),]

oax_prod <- oaxaca(
  formula = as.formula(
    paste(y_prod, "~", paste(x_prod, collapse = " + "), "|", group_var)
  ),
  data = df_prod,
  R = 500
)

extract_oaxaca_aggregate <- function(oax){
  ov <- oax$twofold$overall
  explained   <- ov[1, "coef(explained)"]
  unexplained <- ov[1, "coef(unexplained)"]
  total_gap   <- explained + unexplained
  
  data.frame(
    N = oax$n$n.pooled,
    Total_gap = total_gap,
    Explained = explained,
    Unexplained = unexplained,
    Explained_share = explained / total_gap,
    Unexplained_share = unexplained / total_gap
  )
}

prod_sum <- extract_oaxaca_aggregate(oax_prod)


baseline_farmers$log_maize_plot_area <- log1p(baseline_farmers$maize_plot_area)

vars_OB_inc <- c(
  "log_maize_plot_area"      = "Area of the randomly selected plot in acres (log)",
  "education_head_num"       = "Household-head finished primary education  (1 = Yes)",
  "household_size"           = "Number of household members",
  "hh_age"                   = "Age of household head in years",
  "quality_seed_used"        = "The respondent used quality seeds (1 = Yes)",
  "dap_npk_applied"          = "DAP/NPK applied in the randomly selected plot (1 = Yes)",
  "urea_applied"             = "Urea applied in the randomly selected plot (1 = Yes)",
  "chemicals_applied"        = "Pesticides, herbicides or fungicides applied in the randomly selected plot (1 = Yes)",
  "organic_manure_applied"   = "Organic manure applied in the randomly selected plot (1 = Yes)",
  "num_shops"                = "Number of agro-input shops in the village or neighborhood",
  "distance_agroshops"       = "Distance of homestead to nearest agro-input shop selling maize seed in km",
  "weed_times"               = "Number of weeding times in the randomly selected plot",
  "resow"                    = "Resowing in the randomly selected plot (1 = Yes)",
  "farmer_group_member"      = "The respondent is part of a farmer group or cooperative (1 = Yes)"
)

y_inc <- "maize_income_ihs"
x_inc <- setdiff(names(vars_OB_inc), group_var)

df_inc <- baseline_farmers[
  complete.cases(baseline_farmers[, c(y_inc, group_var, x_inc)]),
]

oax_inc <- oaxaca(
  formula = as.formula(paste(y_inc, "~", paste(x_inc, collapse = " + "), "|", group_var)), data = df_inc, R = 500)
oax_inc

inc_sum <- extract_oaxaca_aggregate(oax_inc)

summary_table_clean <- rbind(
  cbind(Outcome = "Maize Productivity in kg/acres (IHS)", prod_sum),
  cbind(Outcome = "Maize Income  in thousand UGX (IHS)", inc_sum)
)

fmt3 <- function(x) format(round(x, 3), nsmall = 3, trim = TRUE)
tab_oax <- summary_table_clean
tab_oax$N <- as.integer(tab_oax$N)
tab_oax$Total_gap <- fmt3(tab_oax$Total_gap)
tab_oax$Explained <- fmt3(tab_oax$Explained)
tab_oax$Unexplained <- fmt3(tab_oax$Unexplained)
tab_oax$Explained_share <- fmt3(tab_oax$Explained_share)
tab_oax$Unexplained_share <- fmt3(tab_oax$Unexplained_share)


with(df_prod, tapply(yield_per_acre_ihs, hh_gender_num, mean))
