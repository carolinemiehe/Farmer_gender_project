rm(list=ls())
path <- getwd()
path
getwd()
list.files()

getwd()
library(knitr)

library(readr)
baseline_farmers <- read.csv(
  "../../data/baseline/farmer/baseline_farmers.csv",
  stringsAsFactors = FALSE
)

midline_farmers <- read.csv(
  "../../data/midline/farmer/midline.csv",
  stringsAsFactors = FALSE
)

endline_farmers <- read.csv(
  "../../data/endline/farmer/endline.csv",
  stringsAsFactors = FALSE
)


library(dplyr)
library(knitr)

trim <- function(var,dataset,trim_perc=.025){
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  return(dataset)}
#check endline gender respondent and hh
table(endline_farmers$check.maize.q5a, useNA = "ifany")  # respondent
table(endline_farmers$check.maize.q5b, useNA = "ifany")  # household head

#STUCK DATASET
baseline_farmers$round <- "baseline"
midline_farmers$round  <- "midline"
endline_farmers$round  <- "endline"

farmers_long <- bind_rows(baseline_farmers, midline_farmers, endline_farmers)
table(farmers_long$round)


"farmer_ID" %in% names(farmers_long)
#Unic ID
tapply(farmers_long$farmer_ID, farmers_long$round, function(x) length(unique(x)))

# ID in more rounds
id_counts <- table(farmers_long$farmer_ID)
table(id_counts)

#GENDER
baseline_farmers$farmer_ID <- as.character(baseline_farmers$farmer_ID)
endline_farmers$farmer_ID  <- as.character(endline_farmers$farmer_ID)


baseline_farmers$hh_gender_num <- baseline_farmers[["Check2.check.maize.q15"]]
table(baseline_farmers$hh_gender_num)

baseline_farmers$hh_gender_num <- as.character(baseline_farmers$hh_gender_num)
baseline_farmers$hh_gender_num[baseline_farmers$hh_gender_num %in% c("999", "n/a", "NA", "", " ")
] <- NA
baseline_farmers$hh_gender_num <- ifelse(
  baseline_farmers$hh_gender_num == "Male", 1,
  ifelse(baseline_farmers$hh_gender_num == "Female", 0, NA)
)
table(baseline_farmers$hh_gender_num )


x <- endline_farmers[["check.maize.q5b"]]
x <- trimws(tolower(as.character(x)))
endline_farmers$hh_gender_num <- ifelse(x == "male", 1,ifelse(x == "female", 0, NA))

#checks
cat("\nBaseline hh_gender_num:\n")
print(table(baseline_farmers$hh_gender_num, useNA = "ifany"))

cat("\nEndline hh_gender_num:\n")
print(table(endline_farmers$hh_gender_num, useNA = "ifany"))



#confront bl vs el
gender_bl <- baseline_farmers[, c("farmer_ID", "hh_gender_num")]
gender_el <- endline_farmers[,  c("farmer_ID", "hh_gender_num")]

gender_check <- merge(gender_bl, gender_el,
                      by = "farmer_ID",
                      suffixes = c("_bl", "_el"))

table(gender_check$hh_gender_num_bl == gender_check$hh_gender_num_el,
      useNA = "ifany")

#creation of gender_master
gender_master <- gender_check
gender_master$hh_gender_num <- gender_master$hh_gender_num_bl

gender_master$gender_inconsistent <- ifelse(
  !is.na(gender_master$hh_gender_num_bl) &
    !is.na(gender_master$hh_gender_num_el) &
    gender_master$hh_gender_num_bl != gender_master$hh_gender_num_el,
  1, 0
)

gender_master <- gender_master[, c("farmer_ID", "hh_gender_num", "gender_inconsistent")]

table(gender_master$gender_inconsistent, useNA = "ifany")
#0=2786  1=655 this means that for 655 farmers gender BL different from gender EL, for 2786 the gender is the same

#stuck
farmers_long$farmer_ID <- as.character(farmers_long$farmer_ID)
gender_master$farmer_ID <- as.character(gender_master$farmer_ID)

farmers_long <- merge(farmers_long, gender_master,
                      by = "farmer_ID",
                      all.x = TRUE)
table(farmers_long$round, is.na(farmers_long$hh_gender_num))

# I stacked (appended) the farmer-level baseline, midline, and endline datasets into a long/panel dataset called `farmers_long`,
# where each row corresponds to a farmer_ID observed in a given survey round (baseline, midline, or endline).

# Household-head gender is treated as a time-invariant characteristic measured at baseline (`hh_gender_num`);
# a flag `gender_inconsistent` identifies farmers whose reported gender differs between baseline and endline, for robustness checks.



#hybrid SEED - baseline
baseline_farmers$base_hybrid <- (
  baseline_farmers$Check2.check.maize.q31 == "Longe_10H" |
    baseline_farmers$Check2.check.maize.q31 == "Longe_7H" |
    baseline_farmers$Check2.check.maize.q31 == "Longe_7R_Kayongo-go" |
    baseline_farmers$Check2.check.maize.q31 == "Bazooka" |
    baseline_farmers$Check2.check.maize.q31 == "Longe_6H" |
    baseline_farmers$Check2.check.maize.q31 == "Panner" |
    baseline_farmers$Check2.check.maize.q31 == "Wema" |
    baseline_farmers$Check2.check.maize.q31 == "KH_series")
baseline_farmers$base_hybrid<-ifelse(baseline_farmers$base_hybrid=="TRUE",1,0)
baseline_farmers$base_hybrid[baseline_farmers$Check2.check.maize.q31=="Other_hybrid"] <- NA #because =Other hybrid or OPV
#hybrid SEED midline
midline_farmers$mid_hybrid <- (
  midline_farmers$check.maize.q31 == "Longe_10H" |
    midline_farmers$check.maize.q31 == "Longe_7H" |
    midline_farmers$check.maize.q31 == "Longe_7R_K" |
    midline_farmers$check.maize.q31 == "Bazooka" |
    midline_farmers$check.maize.q31 == "Longe_6H" |
    midline_farmers$check.maize.q31 == "Panner" |
    midline_farmers$check.maize.q31 == "Wema" |
    midline_farmers$check.maize.q31 == "KH_series"
)

midline_farmers$mid_hybrid <- ifelse(midline_farmers$mid_hybrid, 1, 0)

midline_farmers$mid_hybrid[midline_farmers$check.maize.q31 == "Other_hybrid"] <- NA

#hybrid SEED -endline
endline_farmers$end_hybrid <- (endline_farmers$check.maize.q31 == "Longe_10H" |
                                 endline_farmers$check.maize.q31 == "Longe_7H" |
                                 endline_farmers$check.maize.q31 == "Longe_7R_K" |
                                 endline_farmers$check.maize.q31 == "Bazooka" |
                                 endline_farmers$check.maize.q31 == "Longe_6H" |
                                 endline_farmers$check.maize.q31 == "Panner" |
                                 endline_farmers$check.maize.q31 == "Wema" |
                                 endline_farmers$check.maize.q31 == "KH_series")
endline_farmers$end_hybrid<-ifelse(endline_farmers$end_hybrid=="TRUE",1,0)
endline_farmers$end_hybrid[endline_farmers$check.maize.q31=="Other_hybrid"] <- NA #because =Other hybrid or OPV




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
#  main = "Household Land Available for Crop Production",
#  xlab = "Acres",
# col = "skyblue",
#    breaks = 20)


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



#OUTPUT VARIABLES 
baseline_farmers$Check2.check.maize.q50[baseline_farmers$Check2.check.maize.q50 %in% c(999, "999", "n/a", "NA", "", " ")] <- NA
baseline_farmers$Check2.check.maize.q51[baseline_farmers$Check2.check.maize.q51 %in% c(999, "999", "n/a", "NA", "", " ")] <- NA

baseline_farmers$Check2.check.maize.q50 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q50))
baseline_farmers$Check2.check.maize.q51 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q51))
baseline_farmers$yield_inkg <- baseline_farmers$Check2.check.maize.q50 * baseline_farmers$Check2.check.maize.q51

aggregate(yield_inkg ~ hh_gender_num, data = baseline_farmers, mean, na.rm = TRUE)
        

table(is.na(baseline_farmers$yield_inkg), baseline_farmers$hh_gender_num)

#TRIMMING (TRIAL)
# Create numeric copies of the three productivity components
baseline_farmers$q50_bags_untrimmed <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q50))
baseline_farmers$q51_kg_bag_untrimmed <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q51))
baseline_farmers$q29_area_untrimmed <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q29))

# Replace special missing codes
baseline_farmers$q50_bags_untrimmed[baseline_farmers$q50_bags_untrimmed == 999] <- NA
baseline_farmers$q51_kg_bag_untrimmed[baseline_farmers$q51_kg_bag_untrimmed == 999] <- NA
baseline_farmers$q29_area_untrimmed[baseline_farmers$q29_area_untrimmed == 999] <- NA

# Define upper cutoffs at the 99th percentile
q50_upper_cutoff <- quantile(baseline_farmers$q50_bags_untrimmed, 0.99, na.rm = TRUE)
q29_upper_cutoff <- quantile(baseline_farmers$q29_area_untrimmed, 0.99, na.rm = TRUE)
q50_upper_cutoff
q29_upper_cutoff

# Create trimmed copies
baseline_farmers$q50_bags_trimmed <- baseline_farmers$q50_bags_untrimmed
baseline_farmers$q51_kg_bag_trimmed <- baseline_farmers$q51_kg_bag_untrimmed
baseline_farmers$q29_area_trimmed <- baseline_farmers$q29_area_untrimmed

# Trim bags above the 99th percentile
baseline_farmers$q50_bags_trimmed[baseline_farmers$q50_bags_trimmed > q50_upper_cutoff] <- NA

# Keep all kilograms-per-bag values

# Trim plot areas below 0.25 acres and above the 99th percentile
baseline_farmers$q29_area_trimmed[baseline_farmers$q29_area_trimmed < 0.25 | baseline_farmers$q29_area_trimmed > q29_upper_cutoff] <- NA



# Count removed observations
sum(baseline_farmers$q50_bags_untrimmed > q50_upper_cutoff, na.rm = TRUE)
sum(baseline_farmers$q29_area_untrimmed < 0.25, na.rm = TRUE)
sum(baseline_farmers$q29_area_untrimmed > q29_upper_cutoff, na.rm = TRUE)



#yield per acre (PRODUCTIVITY)
baseline_farmers$yield_per_acre <- baseline_farmers$yield_inkg / baseline_farmers$r_maize_plot_area
summary(baseline_farmers$yield_per_acre)
baseline_farmers$yield_per_acre[baseline_farmers$yield_per_acre %in% c("999","n/a","NA","", 99900)] <- NA
aggregate(yield_per_acre ~ hh_gender_num, data = baseline_farmers, mean, na.rm = TRUE)
#analysis of productivity
# hist(baseline_farmers$yield_per_acre,
# main = "Distribution of productivity",
# xlab = "Yield per acre",
# col = "lightblue",
#breaks = 30)
# Construct output, productivity and IHS productivity
baseline_farmers$yield_inkg_trimmed <- baseline_farmers$q50_bags_trimmed * baseline_farmers$q51_kg_bag_trimmed
baseline_farmers$yield_per_acre_trimmed <- baseline_farmers$yield_inkg_trimmed / baseline_farmers$q29_area_trimmed
baseline_farmers$yield_per_acre_trimmed_ihs <- asinh(baseline_farmers$yield_per_acre_trimmed)

# Compare original and trimmed productivity
summary(baseline_farmers$yield_per_acre)
summary(baseline_farmers$yield_per_acre_trimmed)
summary(baseline_farmers$yield_per_acre_ihs)
summary(baseline_farmers$yield_per_acre_trimmed_ihs)


# SAFE skewness check it in the future

baseline_farmers$yield_per_acre_ihs <- asinh(baseline_farmers$yield_per_acre)
summary(baseline_farmers$yield_per_acre_ihs)
# hist(baseline_farmers$yield_per_acre_ihs,
# main = "Distribution of IHS Productivity",
# col = "lightgreen",
# breaks = 30)


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




# maize income: set to 0 only for confirmed non-sellers; keep NA if sold==1 but missing bags/price
baseline_farmers$maize_income <- NA_real_
baseline_farmers$maize_income[baseline_farmers$maize_sold == 0] <- 0
baseline_farmers$maize_income[baseline_farmers$maize_sold == 1] <-
  baseline_farmers$bags_sold[baseline_farmers$maize_sold == 1] *
  baseline_farmers$bag_price[baseline_farmers$maize_sold == 1]

baseline_farmers$maize_income_ihs <- asinh(baseline_farmers$maize_income)
summary(baseline_farmers$maize_income_ihs)
table(baseline_farmers$maize_income_ihs)


# 
# hist(baseline_farmers$maize_income,
#      main = "Distribution of Maize Income",
#      col = "lightgreen",
#      breaks = 30)
# 
# hist(baseline_farmers$maize_income,
#      main = "Distribution of IHS Maize Income",
#      col = "lightgreen",
#      breaks = 30)


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




# =========================================================
# PREPARING MIDLINE & ENDLINE (COMMON VARIABLES ONLY)
# =========================================================
clean_yesno01 <- function(x){
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("999","99","98","n/a","na","", " ")] <- NA
  ifelse(x %in% c("yes","y","1","true"), 1,
         ifelse(x %in% c("no","n","0","false"), 0, NA))
}

clean_num <- function(x){
  x <- trimws(as.character(x))
  x[x %in% c("999","99","98","n/a","na","", " ")] <- NA
  as.numeric(x)
}

ihs <- function(x) log(x + sqrt(x^2 + 1))

clean_wave_common <- function(df){
  
  stopifnot(is.data.frame(df))  # safety check
  
  # --- practices / plot controls (common)
  df$r_maize_plot_area      <- clean_num(df[["check.maize.q29"]])
  df$quality_seed_used      <- clean_yesno01(df[["check.maize.q25a"]])
  df$organic_manure_applied <- clean_yesno01(df[["check.maize.q42"]])
  df$dap_npk_applied        <- clean_yesno01(df[["check.maize.q43"]])
  df$urea_applied           <- clean_yesno01(df[["check.maize.q44"]])
  df$chemicals_applied      <- clean_yesno01(df[["check.maize.q47"]])
  df$weed_times             <- clean_num(df[["check.maize.q45"]])
  df$resow                  <- clean_yesno01(df[["check.maize.q49"]])
  
  # --- outcomes (common)
  # Yield: q50 (bags harvested) * q51 (kg per bag)
  df$q50_bags_harvested <- clean_num(df[["check.maize.q50"]])
  df$q51_kg_per_bag     <- clean_num(df[["check.maize.q51"]])
  df$yield_inkg         <- df$q50_bags_harvested * df$q51_kg_per_bag
  
  # Productivity
  df$yield_per_acre <- df$yield_inkg / df$r_maize_plot_area
  df$yield_per_acre[df$yield_per_acre < 0 | is.infinite(df$yield_per_acre)] <- NA
  df$yield_per_acre_ihs <- ihs(df$yield_per_acre)
  
  # Sales participation (q53), quantity (q54), and price (q55)
  df$maize_sold <- clean_yesno01(df[["check.maize.q53"]])
  df$bags_sold  <- clean_num(df[["check.maize.q54"]])
  df$bag_price  <- clean_num(df[["check.maize.q55"]]) / 1000  # keep consistent with baseline if you used /1000
  
  # Maize income: 0 only for confirmed non-sellers; NA stays NA if sold==1 but missing bags/price
  df$maize_income <- NA_real_
  idx0 <- which(df$maize_sold == 0)
  idx1 <- which(df$maize_sold == 1)
  df$maize_income[idx0] <- 0
  df$maize_income[idx1] <- df$bags_sold[idx1] * df$bag_price[idx1]
  
  df$maize_income_ihs <- ihs(df$maize_income)
  
  return(df)
}

# Apply cleaning to each wave
midline_farmers <- clean_wave_common(midline_farmers)
endline_farmers <- clean_wave_common(endline_farmers)




# DESCRIPTIVE TABLE + T-TEST (male vs female) + STARS (BASE)

library(dplyr)  

options(scipen = 999)

variables <- c(
  "distance_agroshops",
  "num_shops",
  
  "household_size",
  "hh_age",
  "education_head_num",
  "relationship_hh_num",
  "respondent_is_hh",
  "hh_marital_status_num",
  
  "maize_plot_area",
  "plots_maize",
  "quality_seed_used",
  "farmer_group_member",
  
  "r_maize_plot_area",
  "base_hybrid",
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

#added fror lyx problems
# --- SAFE helpers (incolla prima del for) ---
safe_num <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    x <- gsub(",", ".", x)
    x <- suppressWarnings(as.numeric(x))
  }
  x
}

safe_mean <- function(x) {
  x <- safe_num(x)
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_min <- function(x) {
  x <- safe_num(x)
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  x <- safe_num(x)
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  x <- safe_num(x)
  if (length(x) == 0 || sum(!is.na(x)) < 2) return(NA_real_)
  sd(x, na.rm = TRUE)
}

safe_n <- function(x) sum(!is.na(x))

fmt2 <- function(x) {
  if (is.na(x)) return("NA")
  format(round(x, 2), nsmall = 2)
}
 #until here

for (i in seq_along(variables)) {
  v_m <- baseline_farmers[[variables[i]]][male_idx]
  v_f <- baseline_farmers[[variables[i]]][female_idx]
  
  diff_means[i] <- safe_mean(v_m) - safe_mean(v_f)
  
  df_descriptives_male[i,1] <- safe_mean(v_m)
  df_descriptives_male[i,2] <- safe_min(v_m)
  df_descriptives_male[i,3] <- safe_max(v_m)
  df_descriptives_male[i,4] <- safe_sd(v_m)
  df_descriptives_male[i,5] <- safe_n(v_m)
  
  df_descriptives_female[i,1] <- safe_mean(v_f)
  df_descriptives_female[i,2] <- safe_min(v_f)
  df_descriptives_female[i,3] <- safe_max(v_f)
  df_descriptives_female[i,4] <- safe_sd(v_f)
  df_descriptives_female[i,5] <- safe_n(v_f)
  
  # T-TEST (usa versioni numeriche, vedi punto 2)
  if (sum(!is.na(safe_num(v_m))) >= 2 && sum(!is.na(safe_num(v_f))) >= 2) {
    tt <- try(t.test(safe_num(v_m), safe_num(v_f)), silent = TRUE)
    if (!inherits(tt, "try-error")) ttest_pvalues[i] <- tt$p.value
  }
}

# Round (keep as data.frame)
df_descriptives_male   <- as.data.frame(lapply(as.data.frame(df_descriptives_male),   function(x) round(x, 3)))
df_descriptives_female <- as.data.frame(lapply(as.data.frame(df_descriptives_female), function(x) round(x, 3)))

# Robustness to read (same as your approach)
df_descriptives_male   <- as.data.frame(df_descriptives_male)
df_descriptives_female <- as.data.frame(df_descriptives_female)

df_descriptives_male[]   <- lapply(df_descriptives_male,   function(x) round(as.numeric(x), 3))
df_descriptives_female[] <- lapply(df_descriptives_female, function(x) round(as.numeric(x), 3))


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

#t-test for dummy variables could be diffuicult whn coming to interpretation
df_descriptives_male   <<- df_descriptives_male
df_descriptives_female <<- df_descriptives_female
ttest_pvalues          <<- ttest_pvalues
diff_fmt <<- diff_fmt


# Checks
sapply(baseline_farmers[variables], class)
colSums(is.na(baseline_farmers[, variables]))



# =========================================================
# Mini descriptives for panel sample (round x gender) for LyX
# Outputs:
#   - farmers_long
#   - df_pooled_desc_g
#   - fmt_cell_g() to be called from \Sexpr in LyX
# =========================================================

library(dplyr)

# ---- 0) Build pooled long dataset from cleaned waves
baseline_farmers$round <- "baseline"
midline_farmers$round  <- "midline"
endline_farmers$round  <- "endline"

farmers_long <- bind_rows(baseline_farmers, midline_farmers, endline_farmers)

# ---- 1) Merge baseline-based gender to all rounds (idempotent + fixes .x/.y)
farmers_long$farmer_ID   <- as.character(farmers_long$farmer_ID)
gender_master$farmer_ID  <- as.character(gender_master$farmer_ID)

farmers_long <- merge(
  farmers_long,
  gender_master[, c("farmer_ID", "hh_gender_num")],
  by = "farmer_ID",
  all.x = TRUE
)

# Consolidate if merge created hh_gender_num.x / hh_gender_num.y
if ("hh_gender_num.x" %in% names(farmers_long) && "hh_gender_num.y" %in% names(farmers_long)) {
  farmers_long$hh_gender_num <- ifelse(
    is.na(farmers_long$hh_gender_num.x),
    farmers_long$hh_gender_num.y,
    farmers_long$hh_gender_num.x
  )
  farmers_long$hh_gender_num.x <- NULL
  farmers_long$hh_gender_num.y <- NULL
}

# Gender label (safe)
farmers_long$hh_gender <- NA_character_
farmers_long$hh_gender[farmers_long$hh_gender_num == 1] <- "Male"
farmers_long$hh_gender[farmers_long$hh_gender_num == 0] <- "Female"

# ---- 2) Variables to report (panel/common + outcomes)
vars_pool <- c(
  "yield_per_acre_ihs",
  "maize_income_ihs",
  "r_maize_plot_area",
  "quality_seed_used",
  "dap_npk_applied",
  "urea_applied",
  "organic_manure_applied",
  "chemicals_applied",
  "weed_times",
  "resow",
  "maize_sold"
)
vars_pool <- intersect(vars_pool, names(farmers_long))

# ---- 3) Long summary table: (variable, round, gender) -> mean, sd, N
df_pooled_desc_g <- data.frame()

for (v in vars_pool) {
  tmp <- farmers_long %>%
    filter(!is.na(hh_gender)) %>%
    group_by(round, hh_gender) %>%
    summarise(
      mean = mean(.data[[v]], na.rm = TRUE),
      sd   = sd(.data[[v]],   na.rm = TRUE),
      N    = sum(!is.na(.data[[v]])),
      .groups = "drop"
    ) %>%
    mutate(variable = v) %>%
    select(variable, round, hh_gender, mean, sd, N)
  
  df_pooled_desc_g <- bind_rows(df_pooled_desc_g, tmp)
}

# Format for LyX
df_pooled_desc_g$mean <- round(df_pooled_desc_g$mean, 3)
df_pooled_desc_g$sd   <- round(df_pooled_desc_g$sd,   3)

# ---- 4) Helper for LyX \Sexpr
fmt_cell_g <- function(var, rnd, g){
  x <- subset(df_pooled_desc_g, variable == var & round == rnd & hh_gender == g)
  if (nrow(x) == 0) return("--")
  paste0(format(x$mean, nsmall = 3),
         " (", format(x$sd, nsmall = 3), ") [", x$N, "]")
}

# Export for LyX
farmers_long       <<- farmers_long
df_pooled_desc_g   <<- df_pooled_desc_g
fmt_cell_g         <<- fmt_cell_g



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
    quality_seed_used +  dap_npk_applied + 
    urea_applied +
    organic_manure_applied  + maize_plot_area +
    chemicals_applied + weed_times + resow + farmer_group_member,
  data = baseline_farmers)


summary(lm2_semifull_fix)
resettest(lm2_semifull_fix)
#crPlots(lm2_semifull_fix)

#lm3 Maize-income (maybe esclude maize sold and bags sold(plots_maize)) - gender is significant just when there are no control variables

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
                         weed_times + resow + farmer_group_member +yield_per_acre,
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


#intersection between the 3 dataset to see the common variables
common_vars <- Reduce(intersect, list(
  names(baseline_farmers),
  names(midline_farmers),
  names(endline_farmers)
))

length(common_vars)
head(sort(common_vars), 30)


#TABLES OLS 


#PRODUCTIVITY TABLE with robust SE
library(dplyr)
library(lmtest)
library(sandwich)

options(scipen = 999)

# =========================
# SETTINGS
# =========================
CLUSTER_ID <- "farmer_ID"   # change to household_ID if you have it
#check if it's a good cluster
length(unique(farmers_long$farmer_ID))
table(table(farmers_long$farmer_ID))

add_stars <- function(p){
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

# Clustered SE aligned to model rows (works even if farmer_ID not in formula)
cell_coef_se <- function(model, term, data, cluster_id = CLUSTER_ID){
  mf  <- model.frame(model)
  idx <- as.integer(rownames(mf))       # rows used by lm()
  cl  <- data[[cluster_id]][idx]        # aligned cluster vector
  
  ct <- tryCatch(
    coeftest(model, vcov = vcovCL(model, cluster = cl)),
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

# =========================
# 1) PREP pooled covariates: carry forward baseline X where missing
# =========================

vars_prod <- c(
  "hh_gender_num",
  "distance_agroshops",
  "num_shops",
  "household_size",
  "hh_age",
  "education_head_num",
  "maize_plot_area",
  "quality_seed_used",
  "farmer_group_member",
  "dap_npk_applied",
  "urea_applied",
  "chemicals_applied",
  "organic_manure_applied",
  "resow",
  "weed_times"
)

baseline_farmers$r_log_maize_plot_area <-
  log1p(baseline_farmers$r_maize_plot_area)

farmers_long$r_log_maize_plot_area <-
  log1p(farmers_long$r_maize_plot_area)

vars_inc <- c(
  "hh_gender_num",
  "distance_agroshops",
  "num_shops",
  "household_size",
  "hh_age",
  "education_head_num",
  "quality_seed_used",
  "farmer_group_member",
  "r_log_maize_plot_area",
  "dap_npk_applied",
  "urea_applied",
  "chemicals_applied",
  "organic_manure_applied",
  "resow",
  "weed_times"
)

vars_hybrid <- c(
  "hh_gender_num",
  "education_head_num",
  "household_size",
  "hh_age",
  "distance_agroshops",
  "num_shops",
  "dap_npk_applied",
  "urea_applied",
  "organic_manure_applied",
  "chemicals_applied",
  "maize_plot_area",
  "weed_times",
  "resow",
  "farmer_group_member"
)

# Baseline covariates to carry forward (raw vars, not transformations)
baseline_covs <- baseline_farmers %>%
  mutate(farmer_ID = as.character(farmer_ID)) %>%
  select(
    farmer_ID,
    hh_gender_num, education_head_num, household_size, hh_age,
    distance_agroshops, num_shops,
    quality_seed_used, dap_npk_applied, urea_applied, organic_manure_applied,
    maize_plot_area, chemicals_applied, weed_times, resow, farmer_group_member,
    r_maize_plot_area, yield_per_acre
  )

farmers_long <- farmers_long %>%
  mutate(farmer_ID = as.character(farmer_ID)) %>%
  left_join(baseline_covs, by = "farmer_ID", suffix = c("", "_bl"))

fill_vars <- c(
  "education_head_num","household_size","hh_age","distance_agroshops","num_shops",
  "quality_seed_used","dap_npk_applied","urea_applied","organic_manure_applied",
  "maize_plot_area","chemicals_applied","weed_times","resow","farmer_group_member",
  "r_maize_plot_area","yield_per_acre"
)

for(v in fill_vars){
  blv <- paste0(v, "_bl")
  if(blv %in% names(farmers_long) && v %in% names(farmers_long)){
    farmers_long[[v]] <- ifelse(is.na(farmers_long[[v]]), farmers_long[[blv]], farmers_long[[v]])
  }
}

# =========================
# 2) PRODUCTIVITY: models + table (Baseline + Pooled)
# =========================

labels_prod <- c(
  "hh_gender_num"          = "Household head is male (1=yes)",
  "distance_agroshops"     = "Homestead's distance to nearest agro-input shop in km",
  "num_shops"              = "Number of agro-input shops in village or neighborhood",
  "household_size"         = "Number of household members",
  "hh_age"                 = "Household head's age in years",
  "education_head_num"     = "Household head finished primary education (1=yes)",
  "maize_plot_area"        = "Land for crop production in acres",
  "quality_seed_used"      = "Farmer used quality seed (1=yes)",
  "farmer_group_member"    = "Farmer is member of maize farmer group/cooperative (1=yes)",
  "dap_npk_applied"        = "Farmer applied DAP/NPK on the randomly selected plot (1=yes)",
  "urea_applied"           = "Farmer applied Urea on this plot (1=yes)",
  "chemicals_applied"      = "Farmer applied agro-chemicals on this plot (1=yes)",
  "organic_manure_applied" = "Farmer applied organic manure on this plot (1=yes)",
  "resow"                  = "Farmer re-sowed seeds on this plot (1=yes)",
  "weed_times"             = "Number of times the farmer weeded this plot"
)

vars_prod <- names(labels_prod)
vars_prod_group <- setdiff(vars_prod, "hh_gender_num")

form_prod_bl <- as.formula(
  paste("yield_per_acre_ihs ~", paste(vars_prod, collapse = " + "))
)

form_prod_bl_group <- as.formula(
  paste("yield_per_acre_ihs ~", paste(vars_prod_group, collapse = " + "))
)

m_prod_raw_bl    <- lm(yield_per_acre_ihs ~ hh_gender_num, data = baseline_farmers)
m_prod_full_bl   <- lm(form_prod_bl, data = baseline_farmers)
m_prod_male_bl   <- lm(form_prod_bl_group, data = baseline_farmers,
                       subset = hh_gender_num == 1)
m_prod_female_bl <- lm(form_prod_bl_group, data = baseline_farmers,
                       subset = hh_gender_num == 0)

# Add survey-round fixed effects
form_prod_pool <- as.formula(
  paste("yield_per_acre_ihs ~",
        paste(vars_prod, collapse = " + "),
        "+ factor(round)")
)

form_prod_pool_group <- as.formula(
  paste("yield_per_acre_ihs ~",
        paste(vars_prod_group, collapse = " + "),
        "+ factor(round)")
)

m_prod_pool_total  <- lm(form_prod_pool, data = farmers_long)
m_prod_pool_male   <- lm(form_prod_pool_group, data = farmers_long,
                         subset = hh_gender_num == 1)
m_prod_pool_female <- lm(form_prod_pool_group, data = farmers_long,
                         subset = hh_gender_num == 0)

N_prod_raw_fmt         <- format(nobs(m_prod_raw_bl), big.mark = ",")
N_prod_full_fmt        <- format(nobs(m_prod_full_bl), big.mark = ",")
N_prod_male_fmt        <- format(nobs(m_prod_male_bl), big.mark = ",")
N_prod_female_fmt      <- format(nobs(m_prod_female_bl), big.mark = ",")
N_prod_pool_total_fmt  <- format(nobs(m_prod_pool_total), big.mark = ",")
N_prod_pool_male_fmt   <- format(nobs(m_prod_pool_male), big.mark = ",")
N_prod_pool_female_fmt <- format(nobs(m_prod_pool_female), big.mark = ",")

tab_prod <- data.frame(
  Label = character(), Raw = character(),
  Full = character(), Male = character(), Female = character(),
  Pooled_Total = character(), Pooled_Male = character(),
  Pooled_Female = character(),
  stringsAsFactors = FALSE
)

for (v in vars_prod) {
  
  raw <- if (v == "hh_gender_num") {
    cell_coef_se(m_prod_raw_bl, v, data = baseline_farmers,
                 cluster_id = CLUSTER_ID)
  } else c("—", "")
  
  full  <- cell_coef_se(m_prod_full_bl, v, data = baseline_farmers,
                        cluster_id = CLUSTER_ID)
  poolT <- cell_coef_se(m_prod_pool_total, v, data = farmers_long,
                        cluster_id = CLUSTER_ID)
  
  if (v == "hh_gender_num") {
    male <- female <- poolM <- poolF <- c("—", "")
  } else {
    male   <- cell_coef_se(m_prod_male_bl, v, data = baseline_farmers,
                           cluster_id = CLUSTER_ID)
    female <- cell_coef_se(m_prod_female_bl, v, data = baseline_farmers,
                           cluster_id = CLUSTER_ID)
    poolM  <- cell_coef_se(m_prod_pool_male, v, data = farmers_long,
                           cluster_id = CLUSTER_ID)
    poolF  <- cell_coef_se(m_prod_pool_female, v, data = farmers_long,
                           cluster_id = CLUSTER_ID)
  }
  
  tab_prod <- rbind(
    tab_prod,
    data.frame(
      Label = labels_prod[v], Raw = raw[1],
      Full = full[1], Male = male[1], Female = female[1],
      Pooled_Total = poolT[1],
      Pooled_Male = poolM[1],
      Pooled_Female = poolF[1]
    ),
    data.frame(
      Label = "", Raw = raw[2],
      Full = full[2], Male = male[2], Female = female[2],
      Pooled_Total = poolT[2],
      Pooled_Male = poolM[2],
      Pooled_Female = poolF[2]
    )
  )
}
# =========================
# 3) INCOME: models + table (Baseline + Pooled)
# =========================

lab_inc <- c(
  "hh_gender_num"          = "Household head is male (1=yes)",
  "distance_agroshops"     = "Homestead's distance to nearest agro-input shop in km",
  "num_shops"              = "Number of agro-input shops in village or neighborhood",
  "household_size"         = "Number of household members",
  "hh_age"                 = "Household head's age in years",
  "education_head_num"     = "Household head finished primary education (1=yes)",
  "quality_seed_used"      = "Farmer used quality seed (1=yes)",
  "farmer_group_member"    = "Farmer is member of maize farmer group/cooperative (1=yes)",
  "r_log_maize_plot_area"  = "Log of area of randomly selected plot in acres",
  "dap_npk_applied"        = "Farmer applied DAP/NPK on this plot (1=yes)",
  "urea_applied"           = "Farmer applied Urea on this plot (1=yes)",
  "chemicals_applied"      = "Farmer applied agro-chemicals on this plot (1=yes)",
  "organic_manure_applied" = "Farmer applied organic manure on this plot (1=yes)",
  "resow"                  = "Farmer re-sowed seeds on this plot (1=yes)",
  "weed_times"             = "Number of times the farmer weeded this plot"
)

form_inc_bl <- as.formula(paste("maize_income_ihs ~", paste(vars_inc, collapse = " + ")))

# pooled formula with round fixed effects
form_inc_pool <- as.formula(
  paste("maize_income_ihs ~", paste(vars_inc, collapse = " + "), "+ factor(round)")
)

m_inc_raw_bl    <- lm(maize_income_ihs ~ hh_gender_num, data = baseline_farmers)
m_inc_full_bl   <- lm(form_inc_bl, data = baseline_farmers)
m_inc_male_bl   <- lm(form_inc_bl, data = baseline_farmers, subset = (hh_gender_num == 1))
m_inc_female_bl <- lm(form_inc_bl, data = baseline_farmers, subset = (hh_gender_num == 0))

# pooled models (use form_inc_pool here)
m_inc_pool_total  <- lm(form_inc_pool, data = farmers_long)
m_inc_pool_male   <- lm(form_inc_pool, data = farmers_long, subset = (hh_gender_num == 1))
m_inc_pool_female <- lm(form_inc_pool, data = farmers_long, subset = (hh_gender_num == 0))

N_inc_raw_fmt         <- format(nobs(m_inc_raw_bl), big.mark=",")
N_inc_full_fmt        <- format(nobs(m_inc_full_bl), big.mark=",")
N_inc_male_fmt        <- format(nobs(m_inc_male_bl), big.mark=",")
N_inc_female_fmt      <- format(nobs(m_inc_female_bl), big.mark=",")
N_inc_pool_total_fmt  <- format(nobs(m_inc_pool_total), big.mark=",")
N_inc_pool_male_fmt   <- format(nobs(m_inc_pool_male), big.mark=",")
N_inc_pool_female_fmt <- format(nobs(m_inc_pool_female), big.mark=",")

setdiff(vars_inc, names(lab_inc))
tab_inc <- data.frame(
  Label=character(), Raw=character(),
  Full=character(), Male=character(), Female=character(),
  Pooled_Total=character(), Pooled_Male=character(), Pooled_Female=character(),
  stringsAsFactors=FALSE
)

resettest(m_inc_full_bl)

# For display: use the var names from vars_inc, but note that the model term is log1p(r_maize_plot_area)
for(v in vars_inc){
  
  raw <- c("—","")
  if(v == "hh_gender_num"){
    raw <- cell_coef_se(m_inc_raw_bl, "hh_gender_num", data = baseline_farmers, cluster_id = CLUSTER_ID)
  }
  
  # term names in lm are identical to v, except log1p(...) which is already correct in vars_inc
  full   <- cell_coef_se(m_inc_full_bl,   v, data = baseline_farmers, cluster_id = CLUSTER_ID)
  male   <- cell_coef_se(m_inc_male_bl,   v, data = baseline_farmers, cluster_id = CLUSTER_ID)
  female <- cell_coef_se(m_inc_female_bl, v, data = baseline_farmers, cluster_id = CLUSTER_ID)
  
  poolT  <- cell_coef_se(m_inc_pool_total,  v, data = farmers_long, cluster_id = CLUSTER_ID)
  poolM  <- cell_coef_se(m_inc_pool_male,   v, data = farmers_long, cluster_id = CLUSTER_ID)
  poolF  <- cell_coef_se(m_inc_pool_female, v, data = farmers_long, cluster_id = CLUSTER_ID)
  
  tab_inc <- rbind(
    tab_inc,
    data.frame(Label=lab_inc[v], Raw=raw[1],
               Full=full[1], Male=male[1], Female=female[1],
               Pooled_Total=poolT[1], Pooled_Male=poolM[1], Pooled_Female=poolF[1]),
    data.frame(Label="", Raw=raw[2],
               Full=full[2], Male=male[2], Female=female[2],
               Pooled_Total=poolT[2], Pooled_Male=poolM[2], Pooled_Female=poolF[2])
  )
}


#correcting the misspecification found with the reset test
#  INCOME – Flexible Specification (Spec 2)


form_inc_bl_flex <- as.formula(
  paste(
    "maize_income_ihs ~",
    paste(vars_inc, collapse = " + "),
    "+ I(log1p(r_maize_plot_area)^2)",
    "+ I(distance_agroshops^2)",
    "+ I(hh_age^2)",
    "+ log1p(r_maize_plot_area):quality_seed_used",
    "+ log1p(r_maize_plot_area):dap_npk_applied"
  )
)

m_inc_full_bl_flex <- lm(form_inc_bl_flex, data = baseline_farmers)

# RESET test on flexible spec
resettest(m_inc_full_bl_flex)

#check again income because problematic
mean(baseline_farmers$maize_income == 0, na.rm = TRUE) * 100
mean(baseline_farmers$maize_income_ihs == 0, na.rm = TRUE) * 100
mean(baseline_farmers$bags_sold == 0, na.rm = TRUE) * 100

tapply(baseline_farmers$maize_income_ihs == 0,
       baseline_farmers$hh_gender_num,
       mean, na.rm = TRUE)

#quick check: compare with non-sellers model
# Subset sellers
m_inc_full_sellers <- lm(
  form_inc_bl,
  data = baseline_farmers,
  subset = (maize_sold == 1)
)
sum(baseline_farmers$maize_sold == 1, na.rm = TRUE)
# RESET test
resettest(m_inc_full_sellers)



# =========================
# HYBRID SEED — POOLED VARIABLE + OLS TABLE
# =========================

# ---------- CREATE POOLED HYBRID VARIABLE ----------
farmers_long$hybrid_seed <- NA

farmers_long$hybrid_seed[farmers_long$round == "baseline"] <-
  farmers_long$base_hybrid[farmers_long$round == "baseline"]

farmers_long$hybrid_seed[farmers_long$round == "midline"] <-
  farmers_long$mid_hybrid[farmers_long$round == "midline"]

farmers_long$hybrid_seed[farmers_long$round == "endline"] <-
  farmers_long$end_hybrid[farmers_long$round == "endline"]

table(farmers_long$hybrid_seed,
      farmers_long$round,
      useNA = "ifany")


# ---------- FORMULAS ----------
form_hybrid_bl <- as.formula(
  paste("base_hybrid ~", paste(vars_hybrid, collapse = " + "))
)

form_hybrid_pool <- as.formula(
  paste("hybrid_seed ~", paste(vars_hybrid, collapse = " + "), "+ factor(round)")
)


# ---------- LABELS ----------
labels_hybrid <- c(
  "hh_gender_num"          = "Household head is male (1=yes)",
  "education_head_num"     = "Household head finished primary education (1=yes)",
  "household_size"         = "Number of household members",
  "hh_age"                 = "Household head’s age in years",
  "distance_agroshops"     = "Homestead’s distance to nearest agro-input shop in km",
  "num_shops"              = "Number of agro-input shops in village or neighborhood",
  "dap_npk_applied"        = "Farmer applied DAP/NPK on randomly selected plot (1=yes)",
  "urea_applied"           = "Farmer applied Urea on this plot (1=yes)",
  "organic_manure_applied" = "Farmer applied organic manure on this plot (1=yes)",
  "maize_plot_area"        = "Land for crop production in acres",
  "chemicals_applied"      = "Farmer applied agro-chemicals on this plot (1=yes)",
  "weed_times"             = "Number of times the farmer weeded this plot",
  "resow"                  = "Farmer re-sowed seeds on this plot (1=yes)",
  "farmer_group_member"    = "Farmer is member of maize farmer group/cooperative (1=yes)"
)


# ---------- MODELS ----------
m_hybrid_raw_bl    <- lm(base_hybrid ~ hh_gender_num, data = baseline_farmers)
m_hybrid_full_bl   <- lm(form_hybrid_bl, data = baseline_farmers)
m_hybrid_male_bl   <- lm(form_hybrid_bl, data = baseline_farmers,
                         subset = (hh_gender_num == 1))
m_hybrid_female_bl <- lm(form_hybrid_bl, data = baseline_farmers,
                         subset = (hh_gender_num == 0))

m_hybrid_pool_total  <- lm(form_hybrid_pool, data = farmers_long)
m_hybrid_pool_male   <- lm(form_hybrid_pool, data = farmers_long,
                           subset = (hh_gender_num == 1))
m_hybrid_pool_female <- lm(form_hybrid_pool, data = farmers_long,
                           subset = (hh_gender_num == 0))


# ---------- N OBS ----------
N_hybrid_raw_fmt         <- format(nobs(m_hybrid_raw_bl), big.mark=",")
N_hybrid_full_fmt        <- format(nobs(m_hybrid_full_bl), big.mark=",")
N_hybrid_male_fmt        <- format(nobs(m_hybrid_male_bl), big.mark=",")
N_hybrid_female_fmt      <- format(nobs(m_hybrid_female_bl), big.mark=",")
N_hybrid_pool_total_fmt  <- format(nobs(m_hybrid_pool_total), big.mark=",")
N_hybrid_pool_male_fmt   <- format(nobs(m_hybrid_pool_male), big.mark=",")
N_hybrid_pool_female_fmt <- format(nobs(m_hybrid_pool_female), big.mark=",")


# ---------- TABLE ----------
tab_hybrid <- data.frame(
  Label=character(), Raw=character(),
  Full=character(), Male=character(), Female=character(),
  Pooled_Total=character(), Pooled_Male=character(), Pooled_Female=character(),
  stringsAsFactors=FALSE
)

for(v in vars_hybrid){
  
  raw <- c("—","")
  if(v == "hh_gender_num"){
    raw <- cell_coef_se(m_hybrid_raw_bl, "hh_gender_num",
                        data = baseline_farmers,
                        cluster_id = CLUSTER_ID)
  }
  
  full   <- cell_coef_se(m_hybrid_full_bl,   v,
                         data = baseline_farmers,
                         cluster_id = CLUSTER_ID)
  
  male   <- cell_coef_se(m_hybrid_male_bl,   v,
                         data = baseline_farmers,
                         cluster_id = CLUSTER_ID)
  
  female <- cell_coef_se(m_hybrid_female_bl, v,
                         data = baseline_farmers,
                         cluster_id = CLUSTER_ID)
  
  poolT  <- cell_coef_se(m_hybrid_pool_total,  v,
                         data = farmers_long,
                         cluster_id = CLUSTER_ID)
  
  poolM  <- cell_coef_se(m_hybrid_pool_male,   v,
                         data = farmers_long,
                         cluster_id = CLUSTER_ID)
  
  poolF  <- cell_coef_se(m_hybrid_pool_female, v,
                         data = farmers_long,
                         cluster_id = CLUSTER_ID)
  
  tab_hybrid <- rbind(
    tab_hybrid,
    data.frame(Label=labels_hybrid[v], Raw=raw[1],
               Full=full[1], Male=male[1], Female=female[1],
               Pooled_Total=poolT[1],
               Pooled_Male=poolM[1],
               Pooled_Female=poolF[1]),
    data.frame(Label="", Raw=raw[2],
               Full=full[2], Male=male[2], Female=female[2],
               Pooled_Total=poolT[2],
               Pooled_Male=poolM[2],
               Pooled_Female=poolF[2])
  )
}

# * TWO-PART ANALYSIS OF GROSS SALES REVENUE

library(lmtest)
library(sandwich)


vars_inc_group <- setdiff(vars_inc, "hh_gender_num")

# * Intensive-margin outcome: log revenue among sellers only
baseline_farmers$log_maize_income_pos <- ifelse(
  baseline_farmers$maize_sold == 1 & !is.na(baseline_farmers$maize_income) & baseline_farmers$maize_income > 0,
  log(baseline_farmers$maize_income), NA_real_)

farmers_long$log_maize_income_pos <- ifelse(
  farmers_long$maize_sold == 1 & !is.na(farmers_long$maize_income) & farmers_long$maize_income > 0,
  log(farmers_long$maize_income), NA_real_)


# * Helper: clustered variance-covariance matrix
vcov_cluster_model <- function(model, data, cluster_id = CLUSTER_ID){
  idx <- as.integer(rownames(model.frame(model)))
  vcovCL(model, cluster = data[[cluster_id]][idx], type = "HC1")
}

# * Helper: format estimates
format_margin <- function(est, se, p){
  st <- ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", "")))
  c(paste0(format(round(est, 3), nsmall = 3), st),
    paste0("(", format(round(se, 3), nsmall = 3), ")"))
}

# * Average marginal effects for logit without marginaleffects package
ame_logit <- function(model, term, data, cluster_id = CLUSTER_ID){
  mf <- model.frame(model)
  idx <- as.integer(rownames(mf))
  d <- data[idx, , drop = FALSE]
  beta <- coef(model)
  V <- vcovCL(model, cluster = data[[cluster_id]][idx], type = "HC1")
  trm <- delete.response(terms(model))
  
  vals <- unique(d[[term]][!is.na(d[[term]])])
  binary <- length(vals) <= 2 && all(vals %in% c(0,1))
  
  ame_fun <- function(b){
    if(binary){
      d0 <- d; d1 <- d
      d0[[term]] <- 0
      d1[[term]] <- 1
      X0 <- model.matrix(trm, d0)
      X1 <- model.matrix(trm, d1)
      mean(plogis(X1 %*% b) - plogis(X0 %*% b))
    } else {
      X <- model.matrix(trm, d)
      p <- plogis(X %*% b)
      j <- which(colnames(X) == term)
      mean(p * (1 - p) * b[j])
    }
  }
  
  est <- ame_fun(beta)
  
  grad <- sapply(seq_along(beta), function(j){
    h <- 1e-6 * max(1, abs(beta[j]))
    b1 <- b0 <- beta
    b1[j] <- b1[j] + h
    b0[j] <- b0[j] - h
    (ame_fun(b1) - ame_fun(b0)) / (2 * h)
  })
  
  se <- sqrt(as.numeric(t(grad) %*% V %*% grad))
  pval <- 2 * pnorm(-abs(est / se))
  
  c(est = est, se = se, p = pval)
}

cell_logit_ame <- function(model, term, data, cluster_id = CLUSTER_ID){
  z <- tryCatch(
    ame_logit(model, term, data, cluster_id),
    error = function(e) NULL
  )
  
  if(is.null(z)) return(c("—", ""))
  
  stars <- ifelse(z["p"] < .01, "***",
                  ifelse(z["p"] < .05, "**",
                         ifelse(z["p"] < .10, "*", "")))
  
  c(
    paste0(format(round(z["est"], 3), nsmall = 3), stars),
    paste0("(", format(round(z["se"], 3), nsmall = 3), ")")
  )
}

# * EXTENSIVE MARGIN: logit of maize market participation
form_ext_bl <- as.formula(paste("maize_sold ~", paste(vars_inc, collapse = " + ")))
form_ext_bl_group <- as.formula(paste("maize_sold ~", paste(vars_inc_group, collapse = " + ")))
form_ext_pool <- as.formula(paste("maize_sold ~", paste(vars_inc, collapse = " + "), "+ factor(round)"))
form_ext_pool_group <- as.formula(paste("maize_sold ~", paste(vars_inc_group, collapse = " + "), "+ factor(round)"))

m_ext_raw_bl <- glm(maize_sold ~ hh_gender_num, data = baseline_farmers, family = binomial("logit"))
m_ext_full_bl <- glm(form_ext_bl, data = baseline_farmers, family = binomial("logit"))
m_ext_male_bl <- glm(form_ext_bl_group, data = baseline_farmers, subset = hh_gender_num == 1, family = binomial("logit"))
m_ext_female_bl <- glm(form_ext_bl_group, data = baseline_farmers, subset = hh_gender_num == 0, family = binomial("logit"))

m_ext_pool_total <- glm(form_ext_pool, data = farmers_long, family = binomial("logit"))
m_ext_pool_male <- glm(form_ext_pool_group, data = farmers_long, subset = hh_gender_num == 1, family = binomial("logit"))
m_ext_pool_female <- glm(form_ext_pool_group, data = farmers_long, subset = hh_gender_num == 0, family = binomial("logit"))

tab_ext <- data.frame(Label=character(), Raw=character(), Full=character(), Male=character(), Female=character(),
                      Pooled_Total=character(), Pooled_Male=character(), Pooled_Female=character(),
                      stringsAsFactors=FALSE)

for(v in vars_inc){
  raw <- if(v == "hh_gender_num") cell_logit_ame(m_ext_raw_bl, v, baseline_farmers) else c("—", "")
  full <- cell_logit_ame(m_ext_full_bl, v, baseline_farmers)
  poolT <- cell_logit_ame(m_ext_pool_total, v, farmers_long)
  
  if(v == "hh_gender_num"){
    male <- female <- poolM <- poolF <- c("—", "")
  } else {
    male <- cell_logit_ame(m_ext_male_bl, v, baseline_farmers)
    female <- cell_logit_ame(m_ext_female_bl, v, baseline_farmers)
    poolM <- cell_logit_ame(m_ext_pool_male, v, farmers_long)
    poolF <- cell_logit_ame(m_ext_pool_female, v, farmers_long)
  }
  
  tab_ext <- rbind(tab_ext,
                   data.frame(Label=lab_inc[v], Raw=raw[1], Full=full[1], Male=male[1], Female=female[1],
                              Pooled_Total=poolT[1], Pooled_Male=poolM[1], Pooled_Female=poolF[1]),
                   data.frame(Label="", Raw=raw[2], Full=full[2], Male=male[2], Female=female[2],
                              Pooled_Total=poolT[2], Pooled_Male=poolM[2], Pooled_Female=poolF[2]))
}

N_ext_raw_fmt <- format(nobs(m_ext_raw_bl), big.mark=",")
N_ext_full_fmt <- format(nobs(m_ext_full_bl), big.mark=",")
N_ext_male_fmt <- format(nobs(m_ext_male_bl), big.mark=",")
N_ext_female_fmt <- format(nobs(m_ext_female_bl), big.mark=",")
N_ext_pool_total_fmt <- format(nobs(m_ext_pool_total), big.mark=",")
N_ext_pool_male_fmt <- format(nobs(m_ext_pool_male), big.mark=",")
N_ext_pool_female_fmt <- format(nobs(m_ext_pool_female), big.mark=",")

print(tab_ext)
tapply(baseline_farmers$maize_sold, baseline_farmers$hh_gender_num, mean, na.rm=TRUE)


# * INTENSIVE MARGIN: OLS of IHS gross revenue among sellers

form_int_bl <- as.formula(paste("maize_income_ihs ~", paste(vars_inc, collapse = " + ")))
form_int_bl_group <- as.formula(paste("maize_income_ihs ~", paste(vars_inc_group, collapse = " + ")))
form_int_pool <- as.formula(paste("maize_income_ihs ~", paste(vars_inc, collapse = " + "), "+ factor(round)"))
form_int_pool_group <- as.formula(paste("maize_income_ihs ~", paste(vars_inc_group, collapse = " + "), "+ factor(round)"))

seller_bl <- baseline_farmers$maize_sold == 1 & baseline_farmers$maize_income_ihs > 0
seller_pool <- farmers_long$maize_sold == 1 & farmers_long$maize_income_ihs > 0

m_int_raw_bl <- lm(maize_income_ihs ~ hh_gender_num, data = baseline_farmers, subset = seller_bl)
m_int_full_bl <- lm(form_int_bl, data = baseline_farmers, subset = seller_bl)
m_int_male_bl <- lm(form_int_bl_group, data = baseline_farmers, subset = seller_bl & hh_gender_num == 1)
m_int_female_bl <- lm(form_int_bl_group, data = baseline_farmers, subset = seller_bl & hh_gender_num == 0)

m_int_pool_total <- lm(form_int_pool, data = farmers_long, subset = seller_pool)
m_int_pool_male <- lm(form_int_pool_group, data = farmers_long, subset = seller_pool & hh_gender_num == 1)
m_int_pool_female <- lm(form_int_pool_group, data = farmers_long, subset = seller_pool & hh_gender_num == 0)

tab_int <- data.frame(Label=character(), Raw=character(), Full=character(), Male=character(), Female=character(),
                      Pooled_Total=character(), Pooled_Male=character(), Pooled_Female=character(),
                      stringsAsFactors=FALSE)

for(v in vars_inc){
  raw <- if(v == "hh_gender_num") cell_coef_se(m_int_raw_bl, v, baseline_farmers, CLUSTER_ID) else c("—", "")
  full <- cell_coef_se(m_int_full_bl, v, baseline_farmers, CLUSTER_ID)
  poolT <- cell_coef_se(m_int_pool_total, v, farmers_long, CLUSTER_ID)
  
  if(v == "hh_gender_num"){
    male <- female <- poolM <- poolF <- c("—", "")
  } else {
    male <- cell_coef_se(m_int_male_bl, v, baseline_farmers, CLUSTER_ID)
    female <- cell_coef_se(m_int_female_bl, v, baseline_farmers, CLUSTER_ID)
    poolM <- cell_coef_se(m_int_pool_male, v, farmers_long, CLUSTER_ID)
    poolF <- cell_coef_se(m_int_pool_female, v, farmers_long, CLUSTER_ID)
  }
  
  tab_int <- rbind(tab_int,
                   data.frame(Label=lab_inc[v], Raw=raw[1], Full=full[1], Male=male[1], Female=female[1],
                              Pooled_Total=poolT[1], Pooled_Male=poolM[1], Pooled_Female=poolF[1]),
                   data.frame(Label="", Raw=raw[2], Full=full[2], Male=male[2], Female=female[2],
                              Pooled_Total=poolT[2], Pooled_Male=poolM[2], Pooled_Female=poolF[2]))
}

N_int_raw_fmt <- format(nobs(m_int_raw_bl), big.mark=",")
N_int_full_fmt <- format(nobs(m_int_full_bl), big.mark=",")
N_int_male_fmt <- format(nobs(m_int_male_bl), big.mark=",")
N_int_female_fmt <- format(nobs(m_int_female_bl), big.mark=",")
N_int_pool_total_fmt <- format(nobs(m_int_pool_total), big.mark=",")
N_int_pool_male_fmt <- format(nobs(m_int_pool_male), big.mark=",")
N_int_pool_female_fmt <- format(nobs(m_int_pool_female), big.mark=",")

print(tab_int)



#various tests - multicollinearity not present
library(car)
vif(m_prod_full_bl)
vif(m_inc_full_bl)
vif(m_hybrid_full_bl)


#etheroskedasticity is trong but it it corrected with clustered st errors
library(lmtest)
bptest(m_prod_full_bl)
bptest(m_inc_full_bl)
bptest(m_hybrid_full_bl)

#reset tests: the model is not correctly specified for income shall I do smt?
library(lmtest)
resettest(m_prod_full_bl)
resettest(m_inc_full_bl)
resettest(m_hybrid_full_bl)
resettest(m_inc_full_bl)



# ============================================================
# OAXACA-BLINDER DECOMPOSITION WITH catchID FIXED EFFECTS
# Productivity + Gross Revenue | Aggregate (nested) + Detailed
# Pooled benchmark WITHOUT gender dummy
# Bootstrap SE clustered at catchID level
# ============================================================

library(dplyr)

# ---- 0. SETTINGS ----

y_prod       <- "yield_per_acre_ihs"
y_inc        <- "maize_income_ihs"
group_var    <- "hh_gender_num"
male_value   <- 1
female_value <- 0
R_boot       <- 300
seed_boot    <- 123

baseline_farmers$maize_income_ihs     <- asinh(baseline_farmers$maize_income)
baseline_farmers$log_maize_plot_area  <- log1p(baseline_farmers$maize_plot_area)

# Covariates for productivity
x_prod <- c(
  "distance_agroshops", "num_shops", "household_size", "hh_age",
  "education_head_num", "maize_plot_area", "farmer_group_member","dap_npk_applied", "urea_applied", "chemicals_applied",
  "organic_manure_applied", "resow", "weed_times"
)

# Covariates for gross revenue (fixed: "log_maize_plot_area", matches labels_inc)
x_inc <- c(
  "distance_agroshops", "num_shops", "household_size", "hh_age",
  "education_head_num", "quality_seed_used", "farmer_group_member",
  "log_maize_plot_area", "dap_npk_applied", "urea_applied",
  "chemicals_applied", "organic_manure_applied", "resow", "weed_times"
)

# Labels for tables
labels_prod <- c(
  "distance_agroshops"     = "Distance to nearest agro-input shop (km)",
  "num_shops"              = "Number of agro-input shops in village",
  "household_size"         = "Number of household members",
  "hh_age"                 = "Household head's age (years)",
  "education_head_num"     = "Household head finished primary education (1=yes)",
  "maize_plot_area"        = "Land for crop production (acres)",
  "quality_seed_used"      = "Used quality seed (1=yes)",
  "farmer_group_member"    = "Member of farmer group/cooperative (1=yes)",
  "dap_npk_applied"        = "Applied DAP/NPK (1=yes)",
  "urea_applied"           = "Applied Urea (1=yes)",
  "chemicals_applied"      = "Applied pesticides/herbicides/fungicides (1=yes)",
  "organic_manure_applied" = "Applied organic manure (1=yes)",
  "resow"                  = "Re-sowed seeds (1=yes)",
  "weed_times"             = "Number of times weeded"
)

labels_inc <- c(
  "distance_agroshops"     = "Distance to nearest agro-input shop (km)",
  "num_shops"              = "Number of agro-input shops in village",
  "household_size"         = "Number of household members",
  "hh_age"                 = "Household head's age (years)",
  "education_head_num"     = "Household head finished primary education (1=yes)",
  "quality_seed_used"      = "Used quality seed (1=yes)",
  "farmer_group_member"    = "Member of farmer group/cooperative (1=yes)",
  "log_maize_plot_area"    = "Log of plot area (acres)",
  "dap_npk_applied"        = "Applied DAP/NPK (1=yes)",
  "urea_applied"           = "Applied Urea (1=yes)",
  "chemicals_applied"      = "Applied pesticides/herbicides/fungicides (1=yes)",
  "organic_manure_applied" = "Applied organic manure (1=yes)",
  "resow"                  = "Re-sowed seeds (1=yes)",
  "weed_times"             = "Number of times weeded"
)


# ---- 1. KEEP ONLY catchID WITH BOTH MALE AND FEMALE HHs ----

valid_catch <- baseline_farmers %>%
  group_by(catchID) %>%
  summarise(
    n_male   = sum(hh_gender_num == male_value, na.rm = TRUE),
    n_female = sum(hh_gender_num == female_value, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  filter(n_male > 0 & n_female > 0) %>%
  pull(catchID)

baseline_fe <- baseline_farmers %>% filter(catchID %in% valid_catch)


# ---- 2. WITHIN TRANSFORMATION (remove catchID average, add overall average) ----

within_transform <- function(df, vars, group_var = "catchID") {
  for (v in vars) {
    if (!v %in% names(df) || !is.numeric(df[[v]])) next
    group_mean   <- ave(df[[v]], df[[group_var]], FUN = function(z) mean(z, na.rm = TRUE))
    overall_mean <- mean(df[[v]], na.rm = TRUE)
    df[[v]] <- df[[v]] - group_mean + overall_mean
  }
  df
}

vars_to_demean <- unique(c(y_prod, y_inc, x_prod, x_inc))
vars_to_demean <- vars_to_demean[vars_to_demean %in% names(baseline_fe)]

baseline_fe_within <- within_transform(baseline_fe, vars_to_demean, "catchID")
baseline_fe_within$catchID <- baseline_fe$catchID  # keep original catchID for clustering


# ---- 3. HELPER FUNCTIONS FOR FORMATTING ----

fmt3 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  format(round(x, 3), nsmall = 3, trim = TRUE)
}

pstars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

# Coefficient with significance stars, based on bootstrap SE
star_cell <- function(est, se) {
  est <- suppressWarnings(as.numeric(est))
  se  <- suppressWarnings(as.numeric(se))
  if (is.na(est)) return("")
  if (is.na(se) || se == 0) return(fmt3(est))
  p <- 2 * pnorm(-abs(est / se))
  paste0(fmt3(est), pstars(p))
}

latex_esc <- function(x) gsub("([%#$&_{}])", "\\\\\\1", x)

safe_share <- function(a, total) {
  if (is.na(total) || abs(total) < 1e-12) return(NA_real_)
  a / total
}


# ---- 4. CORE OAXACA-BLINDER FUNCTION (pooled benchmark, no gender dummy) ----

oaxaca_decomp_nogender <- function(df, y, x_vars, group_var = "hh_gender_num",
                                   male_val = 1, female_val = 0) {
  
  vars_need <- c(y, group_var, x_vars)
  d <- df[complete.cases(df[, vars_need]), , drop = FALSE]
  dm   <- d[d[[group_var]] == male_val, , drop = FALSE]
  dfem <- d[d[[group_var]] == female_val, , drop = FALSE]
  rhs  <- paste(x_vars, collapse = " + ")
  
  beta_m <- coef(lm(as.formula(paste(y, "~", rhs)), data = dm))
  beta_f <- coef(lm(as.formula(paste(y, "~", rhs)), data = dfem))
  beta_p <- coef(lm(as.formula(paste(y, "~", rhs)), data = d))  # pooled benchmark
  
  X_m <- colMeans(model.matrix(as.formula(paste("~", rhs)), data = dm))
  X_f <- colMeans(model.matrix(as.formula(paste("~", rhs)), data = dfem))
  
  # Align names across all vectors (in case some covariate is missing in a subgroup)
  all_names <- Reduce(union, list(names(beta_m), names(beta_f), names(beta_p), names(X_m), names(X_f)))
  align <- function(x) {
    out <- setNames(rep(0, length(all_names)), all_names)
    out[intersect(names(x), all_names)] <- x[intersect(names(x), all_names)]
    out
  }
  beta_m <- align(beta_m); beta_f <- align(beta_f); beta_p <- align(beta_p)
  X_m <- align(X_m); X_f <- align(X_f)
  
  # Threefold decomposition, term by term
  Endow_k       <- (X_m - X_f) * beta_p
  Male_coef_k   <- X_m * (beta_m - beta_p)
  Female_coef_k <- X_f * (beta_p - beta_f)
  
  Explained_k    <- Endow_k
  Coefficients_k <- Male_coef_k + Female_coef_k
  
  Endow <- sum(Endow_k); Male_coef <- sum(Male_coef_k); Female_coef <- sum(Female_coef_k)
  Explained <- sum(Explained_k); Coefficients <- sum(Coefficients_k)
  Total_gap <- Endow + Male_coef + Female_coef
  
  detailed <- data.frame(
    term = all_names, Endow_k, Male_coef_k, Female_coef_k, Explained_k, Coefficients_k,
    stringsAsFactors = FALSE
  )
  
  list(
    N = nrow(d), N_male = nrow(dm), N_female = nrow(dfem), Total_gap = Total_gap,
    Explained = Explained, Coefficients = Coefficients,
    Share_Explained = safe_share(Explained, Total_gap),
    Share_Coefficients = safe_share(Coefficients, Total_gap),
    Endowment = Endow, Male_coef = Male_coef, Female_coef = Female_coef,
    Share_Endowment = safe_share(Endow, Total_gap),
    Share_Male_coef = safe_share(Male_coef, Total_gap),
    Share_Female_coef = safe_share(Female_coef, Total_gap),
    detailed = detailed
  )
}


# ---- 5. BOOTSTRAP, CLUSTERED AT catchID LEVEL ----

bootstrap_oaxaca_fe <- function(df, y, x_vars, group_var = "hh_gender_num",
                                male_val = 1, female_val = 0, cluster = "catchID",
                                R = 300, seed = 123) {
  
  set.seed(seed)
  d <- df[complete.cases(df[, c(y, group_var, x_vars, cluster)]), , drop = FALSE]
  base <- oaxaca_decomp_nogender(d, y, x_vars, group_var, male_val, female_val)
  terms <- base$detailed$term
  clusters <- unique(d[[cluster]])
  n_cl <- length(clusters)
  
  agg_draws <- matrix(NA_real_, R, 5,
                      dimnames = list(NULL, c("Explained", "Coefficients", "Endowment", "Male_coef", "Female_coef")))
  det_names <- c("Explained", "Coefficients", "Endowment", "Male_coef", "Female_coef")
  det_draws <- setNames(
    lapply(det_names, function(x) matrix(NA_real_, R, length(terms), dimnames = list(NULL, terms))),
    det_names
  )
  
  for (r in seq_len(R)) {
    sampled <- sample(clusters, n_cl, replace = TRUE)
    dd <- do.call(rbind, lapply(seq_along(sampled), function(i) d[d[[cluster]] == sampled[i], , drop = FALSE]))
    
    rr <- tryCatch(oaxaca_decomp_nogender(dd, y, x_vars, group_var, male_val, female_val), error = function(e) NULL)
    if (is.null(rr)) next
    
    agg_draws[r, ] <- c(rr$Explained, rr$Coefficients, rr$Endowment, rr$Male_coef, rr$Female_coef)
    det <- rr$detailed[match(terms, rr$detailed$term), ]
    det_draws$Explained[r, ]    <- det$Explained_k
    det_draws$Coefficients[r, ] <- det$Coefficients_k
    det_draws$Endowment[r, ]    <- det$Endow_k
    det_draws$Male_coef[r, ]    <- det$Male_coef_k
    det_draws$Female_coef[r, ]  <- det$Female_coef_k
  }
  
  list(
    base = base,
    se_agg = apply(agg_draws, 2, sd, na.rm = TRUE),
    se_det = lapply(det_draws, function(m) apply(m, 2, sd, na.rm = TRUE))
  )
}


# ---- 6. RUN DECOMPOSITIONS (with catchID fixed effects) ----

boot_prod_fe <- bootstrap_oaxaca_fe(baseline_fe_within, y_prod, x_prod, group_var,
                                    male_value, female_value, "catchID", R_boot, seed_boot)

boot_inc_fe <- bootstrap_oaxaca_fe(baseline_fe_within, y_inc, x_inc, group_var,
                                   male_value, female_value, "catchID", R_boot, seed_boot)


# ---- 7. NESTED AGGREGATE TABLE (twofold + threefold in one table) ----

make_nested_oaxaca_table <- function(boot_obj) {
  b  <- boot_obj$base
  se <- boot_obj$se_agg
  
  data.frame(
    Component = c(
      "Total gap",
      "Endowment effect",
      "Coefficient effect (unexplained)",
      "Male coefficient effect",
      "Female coefficient effect"
    ),
    
    Coefficient = c(
      fmt3(b$Total_gap),
      star_cell(b$Explained, se["Explained"]),
      star_cell(b$Coefficients, se["Coefficients"]),
      star_cell(b$Male_coef, se["Male_coef"]),
      star_cell(b$Female_coef, se["Female_coef"])
    ),
    
    Share_pct = c(
      "100.0",
      sprintf("%.1f", b$Share_Explained * 100),
      sprintf("%.1f", b$Share_Coefficients * 100),
      sprintf("%.1f", b$Share_Male_coef * 100),
      sprintf("%.1f", b$Share_Female_coef * 100)
    ),
    
    stringsAsFactors = FALSE
  )
}
tab_nested_prod <- make_nested_oaxaca_table(boot_prod_fe)
tab_nested_inc  <- make_nested_oaxaca_table(boot_inc_fe)

N_prod_male   <- boot_prod_fe$base$N_male
N_prod_female <- boot_prod_fe$base$N_female
N_inc_male    <- boot_inc_fe$base$N_male
N_inc_female  <- boot_inc_fe$base$N_female


# ---- 8. DETAILED DECOMPOSITION TABLES (twofold + threefold, per outcome) ----

make_twofold_det_fe <- function(boot_obj, labels_map, outcome_label) {
  det <- boot_obj$base$detailed
  det <- det[det$term != "(Intercept)", , drop = FALSE]
  se  <- boot_obj$se_det
  det$Variable <- ifelse(det$term %in% names(labels_map), labels_map[det$term], det$term)
  data.frame(
    Outcome = outcome_label,
    Variable = latex_esc(det$Variable),
    Explained = mapply(star_cell, det$Explained_k, se$Explained[det$term]),
    Coefficients = mapply(star_cell, det$Coefficients_k, se$Coefficients[det$term]),
    stringsAsFactors = FALSE
  )
}

make_threefold_det_fe <- function(boot_obj, labels_map, outcome_label) {
  det <- boot_obj$base$detailed
  det <- det[det$term != "(Intercept)", , drop = FALSE]
  se  <- boot_obj$se_det
  det$Variable <- ifelse(det$term %in% names(labels_map), labels_map[det$term], det$term)
  data.frame(
    Outcome = outcome_label,
    Variable = latex_esc(det$Variable),
    Endowment = mapply(star_cell, det$Endow_k, se$Endowment[det$term]),
    Male_coefficients = mapply(star_cell, det$Male_coef_k, se$Male_coef[det$term]),
    Female_coefficients = mapply(star_cell, det$Female_coef_k, se$Female_coef[det$term]),
    stringsAsFactors = FALSE
  )
}

# Build detailed tables, split directly by outcome (no need for subset() afterwards)
tab_det_twofold_prod <- make_twofold_det_fe(boot_prod_fe, labels_prod, "Maize productivity (IHS)")
tab_det_twofold_inc  <- make_twofold_det_fe(boot_inc_fe,  labels_inc,  "Maize gross revenue (IHS)")

tab_det_threefold_prod <- make_threefold_det_fe(boot_prod_fe, labels_prod, "Maize productivity (IHS)")
tab_det_threefold_inc  <- make_threefold_det_fe(boot_inc_fe,  labels_inc,  "Maize gross revenue (IHS)")


# ---- 9. EXPORT EVERYTHING FOR LYX ----

tab_nested_prod         <<- tab_nested_prod
tab_nested_inc          <<- tab_nested_inc
tab_det_twofold_prod    <<- tab_det_twofold_prod
tab_det_twofold_inc     <<- tab_det_twofold_inc
tab_det_threefold_prod  <<- tab_det_threefold_prod
tab_det_threefold_inc   <<- tab_det_threefold_inc
N_prod_male   <<- N_prod_male
N_prod_female <<- N_prod_female
N_inc_male    <<- N_inc_male
N_inc_female  <<- N_inc_female

cat("Ready for LyX:\n")
cat("- tab_nested_prod, tab_nested_inc (aggregate, twofold+threefold combined)\n")
cat("- tab_det_twofold_prod, tab_det_twofold_inc\n")
cat("- tab_det_threefold_prod, tab_det_threefold_inc\n")


#quick fix of names
# ---- CREATE LATEX ROWS FOR THE EXISTING LYX TABLES ----

make_latex_rows <- function(tab, columns) {
  rows <- apply(
    tab[, columns, drop = FALSE],
    1,
    function(x) paste(x, collapse = " & ")
  )
  
  paste0(rows, " \\\\", collapse = "\n")
}

# Twofold detailed decomposition
latex_rows_twofold_prod <- make_latex_rows(
  tab_det_twofold_prod,
  c("Variable", "Explained", "Coefficients")
)

latex_rows_twofold_inc <- make_latex_rows(
  tab_det_twofold_inc,
  c("Variable", "Explained", "Coefficients")
)

# Threefold detailed decomposition
latex_rows_threefold_prod <- make_latex_rows(
  tab_det_threefold_prod,
  c("Variable", "Endowment",
    "Male_coefficients", "Female_coefficients")
)

latex_rows_threefold_inc <- make_latex_rows(
  tab_det_threefold_inc,
  c("Variable", "Endowment",
    "Male_coefficients", "Female_coefficients")
)

latex_rows_twofold_prod   <<- latex_rows_twofold_prod
latex_rows_twofold_inc    <<- latex_rows_twofold_inc
latex_rows_threefold_prod <<- latex_rows_threefold_prod
latex_rows_threefold_inc  <<- latex_rows_threefold_inc


#I try to devide income in intensive and extensive margin

# ============================================================
# OAXACA CLASSICO - INTENSIVE MARGIN (solo chi vende)
# ============================================================

sellers_only <- baseline_farmers[baseline_farmers$maize_sold == 1, ]

result_intensive <- oaxaca_decomp_nogender(
  sellers_only,
  y = "maize_income_ihs",      # oppure log(bags_sold) se vuoi guardare solo la quantità, non il valore
  x_vars = x_inc,
  group_var = "hh_gender_num",
  male_val = 1,
  female_val = 0
)

print(result_intensive)



#SOME GRAPHS
library(dplyr)
library(ggplot2)
library(patchwork)

# =========================
# 1. PREPARE DATA
# =========================

plot_data <- baseline_farmers %>%
  filter(!is.na(hh_gender_num)) %>%
  mutate(
    hh_gender = case_when(
      hh_gender_num == 1 ~ "Male-headed households",
      hh_gender_num == 0 ~ "Female-headed households",
      TRUE ~ NA_character_
    ),
    hh_gender = factor(
      hh_gender,
      levels = c("Male-headed households", "Female-headed households")
    )
  )

# =========================
# 2. PANEL A: PRODUCTIVITY
# =========================

p_prod <- ggplot(
  plot_data %>% filter(!is.na(yield_per_acre_ihs)),
  aes(x = yield_per_acre_ihs, colour = hh_gender, linetype = hh_gender)
) +
  geom_density(linewidth = 1.1, adjust = 1) +
  scale_colour_manual(values = c(
    "Male-headed households" = "#F8766D",
    "Female-headed households" = "#00BFC4"
  )) +
  scale_linetype_manual(values = c(
    "Male-headed households" = "solid",
    "Female-headed households" = "dashed"
  )) +
  labs(
    x = "IHS-transformed outcome",
    y = "Density",
    colour = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# =========================
# 3. PANEL B: GROSS REVENUE
# =========================

p_inc <- ggplot(
  plot_data %>% filter(!is.na(maize_income_ihs)),
  aes(x = maize_income_ihs, colour = hh_gender, linetype = hh_gender)
) +
  geom_density(linewidth = 1.1, adjust = 1) +
  scale_colour_manual(values = c(
    "Male-headed households" = "#F8766D",
    "Female-headed households" = "#00BFC4"
  )) +
  scale_linetype_manual(values = c(
    "Male-headed households" = "solid",
    "Female-headed households" = "dashed"
  )) +
  labs(
    x = "IHS-transformed outcome",
    y = "Density",
    colour = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# =========================
# 4. COMBINE VERTICALLY
# =========================

fig_density_vertical <- p_prod / p_inc

# Show in R
fig_density_vertical

# =========================
# 5. SAVE NEW IMAGE
# =========================

ggsave(
  filename = "density_productivity_revenue_vertical.png",
  plot = fig_density_vertical,
  width = 8,
  height = 10,
  dpi = 300
)


#ROBUSTNESS CHECKS
#1. OAXACA BLINDER DECOMPOSITION IN LEVELS WITH TRIMMING
# ============================================================
# LEVEL (NON-IHS) OUTCOMES, TRIMMED AT EXTREMES
# ============================================================

# --- Trim components of productivity (se non già create prima nello script) ---
q50_upper_cutoff <- quantile(baseline_farmers$Check2.check.maize.q50, 0.99, na.rm = TRUE)
q29_upper_cutoff <- quantile(baseline_farmers$Check2.check.maize.q29, 0.99, na.rm = TRUE)

baseline_farmers$q50_bags_trimmed <- baseline_farmers$Check2.check.maize.q50
baseline_farmers$q50_bags_trimmed[baseline_farmers$q50_bags_trimmed > q50_upper_cutoff] <- NA

baseline_farmers$q29_area_trimmed <- baseline_farmers$Check2.check.maize.q29
baseline_farmers$q29_area_trimmed[baseline_farmers$q29_area_trimmed < 0.25 |
                                    baseline_farmers$q29_area_trimmed > q29_upper_cutoff] <- NA

baseline_farmers$yield_inkg_trimmed     <- baseline_farmers$q50_bags_trimmed *
  baseline_farmers$Check2.check.maize.q51
baseline_farmers$yield_per_acre_trimmed <- baseline_farmers$yield_inkg_trimmed /
  baseline_farmers$q29_area_trimmed

summary(baseline_farmers$yield_per_acre_trimmed)

# --- Trim maize income (level, not IHS) ---
# --- Trim maize income DIRETTAMENTE sul livello finale, solo tra i venditori ---
sellers_income <- baseline_farmers$maize_income[baseline_farmers$maize_sold == 1]

inc_upper_cutoff <- quantile(sellers_income, 0.99, na.rm = TRUE)
inc_lower_cutoff <- quantile(sellers_income, 0.01, na.rm = TRUE)

baseline_farmers$maize_income_trimmed <- NA_real_
baseline_farmers$maize_income_trimmed[baseline_farmers$maize_sold == 0] <- 0
baseline_farmers$maize_income_trimmed[baseline_farmers$maize_sold == 1] <- 
  baseline_farmers$maize_income[baseline_farmers$maize_sold == 1]

# ora trimma SOLO il livello finale, non i due input separatamente
baseline_farmers$maize_income_trimmed[
  baseline_farmers$maize_sold == 1 &
    (baseline_farmers$maize_income_trimmed > inc_upper_cutoff |
       baseline_farmers$maize_income_trimmed < inc_lower_cutoff)
] <- NA

summary(baseline_farmers$maize_income_trimmed)


# quante osservazioni perdi rispetto al criterio precedente (componenti separate)?
sum(is.na(baseline_farmers$maize_income_trimmed))

# quante osservazioni perdi col trimming rispetto alla versione IHS non trimmata
sum(is.na(baseline_farmers$yield_per_acre_trimmed)) - sum(is.na(baseline_farmers$yield_per_acre))
sum(is.na(baseline_farmers$maize_income_trimmed)) - sum(is.na(baseline_farmers$maize_income))
# ---- Nuovi outcome in livelli ----
y_prod_lvl <- "yield_per_acre_trimmed"
y_inc_lvl  <- "maize_income_trimmed"

# ---- Ricostruisci il sotto-campione con catchID che hanno sia MHH sia FHH ----
# (valid_catch è lo stesso di prima, basato su hh_gender_num e catchID)
baseline_fe_lvl <- baseline_farmers %>% filter(catchID %in% valid_catch)

# ---- Within transformation (demeaning per catchID) sui nuovi outcome + stessi covariati ----
vars_to_demean_lvl <- unique(c(y_prod_lvl, y_inc_lvl, x_prod, x_inc))
vars_to_demean_lvl <- vars_to_demean_lvl[vars_to_demean_lvl %in% names(baseline_fe_lvl)]

baseline_fe_within_lvl <- within_transform(baseline_fe_lvl, vars_to_demean_lvl, "catchID")
baseline_fe_within_lvl$catchID <- baseline_fe_lvl$catchID

# ---- Bootstrap Oaxaca-Blinder, livelli trimmati ----
boot_prod_fe_lvl <- bootstrap_oaxaca_fe(baseline_fe_within_lvl, y_prod_lvl, x_prod, group_var,
                                        male_value, female_value, "catchID", R_boot, seed_boot)

boot_inc_fe_lvl <- bootstrap_oaxaca_fe(baseline_fe_within_lvl, y_inc_lvl, x_inc, group_var,
                                       male_value, female_value, "catchID", R_boot, seed_boot)

# ---- Tabelle aggregate (nested: twofold + threefold) ----
tab_nested_prod_lvl <- make_nested_oaxaca_table(boot_prod_fe_lvl)
tab_nested_inc_lvl  <- make_nested_oaxaca_table(boot_inc_fe_lvl)

N_prod_male_lvl   <- boot_prod_fe_lvl$base$N_male
N_prod_female_lvl <- boot_prod_fe_lvl$base$N_female
N_inc_male_lvl    <- boot_inc_fe_lvl$base$N_male
N_inc_female_lvl  <- boot_inc_fe_lvl$base$N_female

# ---- Tabelle dettagliate (per covariata) ----
tab_det_twofold_prod_lvl <- make_twofold_det_fe(boot_prod_fe_lvl, labels_prod,
                                                "Maize productivity (levels, trimmed)")
tab_det_twofold_inc_lvl  <- make_twofold_det_fe(boot_inc_fe_lvl,  labels_inc,
                                                "Maize gross revenue (levels, trimmed)")

tab_det_threefold_prod_lvl <- make_threefold_det_fe(boot_prod_fe_lvl, labels_prod,
                                                    "Maize productivity (levels, trimmed)")
tab_det_threefold_inc_lvl  <- make_threefold_det_fe(boot_inc_fe_lvl,  labels_inc,
                                                    "Maize gross revenue (levels, trimmed)")

# ---- Export per LyX ----
tab_nested_prod_lvl        <<- tab_nested_prod_lvl
tab_nested_inc_lvl         <<- tab_nested_inc_lvl
tab_det_twofold_prod_lvl   <<- tab_det_twofold_prod_lvl
tab_det_twofold_inc_lvl    <<- tab_det_twofold_inc_lvl
tab_det_threefold_prod_lvl <<- tab_det_threefold_prod_lvl
tab_det_threefold_inc_lvl  <<- tab_det_threefold_inc_lvl
N_prod_male_lvl   <<- N_prod_male_lvl
N_prod_female_lvl <<- N_prod_female_lvl
N_inc_male_lvl    <<- N_inc_male_lvl
N_inc_female_lvl  <<- N_inc_female_lvl

cat("Ready for LyX (levels, trimmed):\n")
cat("- tab_nested_prod_lvl, tab_nested_inc_lvl\n")
cat("- tab_det_twofold_prod_lvl, tab_det_twofold_inc_lvl\n")
cat("- tab_det_threefold_prod_lvl, tab_det_threefold_inc_lvl\n")


# ============================================================
# ROBUSTNESS CHECK 2:
# OAXACA OF GROSS SALES REVENUE AMONG SELLERS ONLY
# LEVELS, TRIMMED
# ============================================================

# Outcome already created above:
# y_inc_lvl <- "maize_income_trimmed"

# ---- 1. Keep only sellers with positive, non-missing revenue ----

vars_inc_sellers <- unique(c(
  y_inc_lvl,
  group_var,
  x_inc,
  "catchID"
))

sellers_lvl <- baseline_farmers[
  baseline_farmers$maize_sold == 1 &
    !is.na(baseline_farmers[[y_inc_lvl]]) &
    baseline_farmers[[y_inc_lvl]] > 0,
  ,
  drop = FALSE
]

# Keep complete observations for all variables used in the decomposition
sellers_lvl <- sellers_lvl[
  complete.cases(sellers_lvl[, vars_inc_sellers, drop = FALSE]),
  ,
  drop = FALSE
]


# ---- 2. Keep catchments with both male and female sellers ----

valid_catch_sellers <- sellers_lvl %>%
  group_by(catchID) %>%
  summarise(
    n_male = sum(hh_gender_num == male_value, na.rm = TRUE),
    n_female = sum(hh_gender_num == female_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_male > 0 & n_female > 0) %>%
  pull(catchID)

sellers_fe_lvl <- sellers_lvl %>%
  filter(catchID %in% valid_catch_sellers)


# ---- 3. Within transformation among sellers only ----

vars_to_demean_sellers <- unique(c(
  y_inc_lvl,
  x_inc
))

vars_to_demean_sellers <- vars_to_demean_sellers[
  vars_to_demean_sellers %in% names(sellers_fe_lvl)
]

sellers_fe_within_lvl <- within_transform(
  sellers_fe_lvl,
  vars_to_demean_sellers,
  "catchID"
)

# Restore original catchment identifier for clustered bootstrap
sellers_fe_within_lvl$catchID <- sellers_fe_lvl$catchID


# ---- 4. Bootstrap Oaxaca among sellers only ----

boot_inc_sellers_lvl <- bootstrap_oaxaca_fe(
  df = sellers_fe_within_lvl,
  y = y_inc_lvl,
  x_vars = x_inc,
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_boot,
  seed = seed_boot
)


# ---- 5. Aggregate table ----

tab_nested_inc_sellers_lvl <- make_nested_oaxaca_table(
  boot_inc_sellers_lvl
)

N_inc_sellers_male <- boot_inc_sellers_lvl$base$N_male
N_inc_sellers_female <- boot_inc_sellers_lvl$base$N_female


# ---- 6. Detailed twofold and threefold tables ----

tab_det_twofold_inc_sellers_lvl <- make_twofold_det_fe(
  boot_inc_sellers_lvl,
  labels_inc,
  "Maize gross sales revenue among sellers (levels, trimmed)"
)

tab_det_threefold_inc_sellers_lvl <- make_threefold_det_fe(
  boot_inc_sellers_lvl,
  labels_inc,
  "Maize gross sales revenue among sellers (levels, trimmed)"
)


# ---- 7. Export for LyX ----

tab_nested_inc_sellers_lvl <<- tab_nested_inc_sellers_lvl

tab_det_twofold_inc_sellers_lvl <<-
  tab_det_twofold_inc_sellers_lvl

tab_det_threefold_inc_sellers_lvl <<-
  tab_det_threefold_inc_sellers_lvl

N_inc_sellers_male <<- N_inc_sellers_male
N_inc_sellers_female <<- N_inc_sellers_female


# ---- 8. Check the estimation sample ----

cat("\nOaxaca among sellers only:\n")
cat("Male sellers:", N_inc_sellers_male, "\n")
cat("Female sellers:", N_inc_sellers_female, "\n")
cat(
  "Total sellers:",
  N_inc_sellers_male + N_inc_sellers_female,
  "\n"
)

print(tab_nested_inc_sellers_lvl)





#ROBUSTNESS CHECK 3
# ============================================================
# EXTENSIVE MARGIN:
# FAIRLIE-TYPE DECOMPOSITION OF MAIZE MARKET PARTICIPATION
# Outcome: maize_sold = 0/1
# ============================================================

fairlie_twofold_core <- function(
    df,
    y,
    x_vars,
    group_var = "hh_gender_num",
    male_val = 1,
    female_val = 0,
    cluster = "catchID",
    link = "logit",
    catch_fe = TRUE
) {
  
  vars_need <- unique(c(y, group_var, x_vars, cluster))
  
  d <- df[
    complete.cases(df[, vars_need, drop = FALSE]),
    ,
    drop = FALSE
  ]
  
  # Outcome must be binary
  d <- d[d[[y]] %in% c(0, 1), , drop = FALSE]
  
  # Keep catchments with both MHH and FHH
  male_counts <- tapply(
    d[[group_var]] == male_val,
    d[[cluster]],
    sum
  )
  
  female_counts <- tapply(
    d[[group_var]] == female_val,
    d[[cluster]],
    sum
  )
  
  valid_catch <- intersect(
    names(male_counts)[male_counts > 0],
    names(female_counts)[female_counts > 0]
  )
  
  d <- d[
    as.character(d[[cluster]]) %in% valid_catch,
    ,
    drop = FALSE
  ]
  
  d[[cluster]] <- factor(d[[cluster]])
  
  dm <- d[d[[group_var]] == male_val, , drop = FALSE]
  dfem <- d[d[[group_var]] == female_val, , drop = FALSE]
  
  rhs_terms <- x_vars
  
  # For binary outcomes, do not use the within transformation.
  # Catchment fixed effects enter directly as dummies.
  if (catch_fe) {
    rhs_terms <- c(
      rhs_terms,
      paste0("factor(", cluster, ")")
    )
  }
  
  form <- as.formula(
    paste(y, "~", paste(rhs_terms, collapse = " + "))
  )
  
  pooled_model <- suppressWarnings(
    glm(
      form,
      data = d,
      family = binomial(link = link)
    )
  )
  
  pred_male <- predict(
    pooled_model,
    newdata = dm,
    type = "response"
  )
  
  pred_female <- predict(
    pooled_model,
    newdata = dfem,
    type = "response"
  )
  
  # Observed market-participation gap
  total_gap <- mean(dm[[y]]) - mean(dfem[[y]])
  
  # Difference due to observable characteristics
  endowment <- mean(pred_male) - mean(pred_female)
  
  # Remaining coefficient/unexplained component
  coefficients <- total_gap - endowment
  
  list(
    data = d,
    N_male = nrow(dm),
    N_female = nrow(dfem),
    
    Mean_male = mean(dm[[y]]),
    Mean_female = mean(dfem[[y]]),
    
    Total_gap = total_gap,
    Endowment = endowment,
    Coefficients = coefficients,
    
    Share_Endowment = safe_share(endowment, total_gap),
    Share_Coefficients = safe_share(coefficients, total_gap),
    
    link = link
  )
}


# ============================================================
# CLUSTER BOOTSTRAP AT catchID LEVEL
# ============================================================

bootstrap_fairlie_twofold <- function(
    df,
    y,
    x_vars,
    group_var = "hh_gender_num",
    male_val = 1,
    female_val = 0,
    cluster = "catchID",
    link = "logit",
    catch_fe = TRUE,
    R = 300,
    seed = 123
) {
  
  set.seed(seed)
  
  base <- fairlie_twofold_core(
    df = df,
    y = y,
    x_vars = x_vars,
    group_var = group_var,
    male_val = male_val,
    female_val = female_val,
    cluster = cluster,
    link = link,
    catch_fe = catch_fe
  )
  
  d <- base$data
  clusters <- unique(as.character(d[[cluster]]))
  n_clusters <- length(clusters)
  
  draws <- matrix(
    NA_real_,
    nrow = R,
    ncol = 3,
    dimnames = list(
      NULL,
      c("Total_gap", "Endowment", "Coefficients")
    )
  )
  
  for (r in seq_len(R)) {
    
    sampled_clusters <- sample(
      clusters,
      n_clusters,
      replace = TRUE
    )
    
    dd <- do.call(
      rbind,
      lapply(seq_along(sampled_clusters), function(i) {
        
        z <- d[
          as.character(d[[cluster]]) == sampled_clusters[i],
          ,
          drop = FALSE
        ]
        
        # Give each bootstrap copy a separate catchment ID
        z[[cluster]] <- paste0("bootstrap_", i)
        
        z
      })
    )
    
    result_r <- tryCatch(
      fairlie_twofold_core(
        df = dd,
        y = y,
        x_vars = x_vars,
        group_var = group_var,
        male_val = male_val,
        female_val = female_val,
        cluster = cluster,
        link = link,
        catch_fe = catch_fe
      ),
      error = function(e) NULL
    )
    
    if (is.null(result_r)) next
    
    draws[r, ] <- c(
      result_r$Total_gap,
      result_r$Endowment,
      result_r$Coefficients
    )
  }
  
  list(
    base = base,
    se_agg = apply(draws, 2, sd, na.rm = TRUE),
    successful_replications = sum(complete.cases(draws))
  )
}


# ============================================================
# FORMAT FAIRLIE TABLE
# ============================================================

make_fairlie_table <- function(boot_obj) {
  
  b <- boot_obj$base
  se <- boot_obj$se_agg
  
  data.frame(
    Component = c(
      "Total gap",
      "Endowment effect",
      "Coefficient effect (unexplained)"
    ),
    
    Coefficient = c(
      fmt3(b$Total_gap),
      star_cell(b$Endowment, se["Endowment"]),
      star_cell(b$Coefficients, se["Coefficients"])
    ),
    
    Share_pct = c(
      "100.0",
      sprintf("%.1f", b$Share_Endowment * 100),
      sprintf("%.1f", b$Share_Coefficients * 100)
    ),
    
    stringsAsFactors = FALSE
  )
}

boot_ext_logit <- bootstrap_fairlie_twofold(
  df = baseline_farmers,
  y = "maize_sold",
  x_vars = x_inc,
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  link = "logit",
  catch_fe = TRUE,
  R = R_boot,
  seed = seed_boot
)

tab_ext_logit <- make_fairlie_table(boot_ext_logit)

N_ext_logit_male <- boot_ext_logit$base$N_male
N_ext_logit_female <- boot_ext_logit$base$N_female

rate_ext_logit_male <- boot_ext_logit$base$Mean_male
rate_ext_logit_female <- boot_ext_logit$base$Mean_female

print(tab_ext_logit)

boot_ext_probit <- bootstrap_fairlie_twofold(
  df = baseline_farmers,
  y = "maize_sold",
  x_vars = x_inc,
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  link = "probit",
  catch_fe = TRUE,
  R = R_boot,
  seed = seed_boot
)

tab_ext_probit <- make_fairlie_table(boot_ext_probit)

N_ext_probit_male <- boot_ext_probit$base$N_male
N_ext_probit_female <- boot_ext_probit$base$N_female

rate_ext_probit_male <- boot_ext_probit$base$Mean_male
rate_ext_probit_female <- boot_ext_probit$base$Mean_female

print(tab_ext_probit)


tab_ext_logit <<- tab_ext_logit
tab_ext_probit <<- tab_ext_probit

N_ext_logit_male <<- N_ext_logit_male
N_ext_logit_female <<- N_ext_logit_female

N_ext_probit_male <<- N_ext_probit_male
N_ext_probit_female <<- N_ext_probit_female

rate_ext_logit_male <<- rate_ext_logit_male
rate_ext_logit_female <<- rate_ext_logit_female

rate_ext_probit_male <<- rate_ext_probit_male
rate_ext_probit_female <<- rate_ext_probit_female



# ============================================================
# INTENSIVE MARGIN:
# OAXACA OF IHS GROSS SALES REVENUE AMONG SELLERS ONLY
# ============================================================

y_inc_sellers_ihs <- "maize_income_ihs"

vars_inc_sellers_ihs <- unique(c(
  y_inc_sellers_ihs,
  group_var,
  x_inc,
  "catchID"
))

# 1. Keep sellers only
sellers_ihs <- baseline_farmers[
  baseline_farmers$maize_sold == 1 &
    !is.na(baseline_farmers[[y_inc_sellers_ihs]]) &
    baseline_farmers[[y_inc_sellers_ihs]] > 0,
  ,
  drop = FALSE
]

# 2. Keep complete observations
sellers_ihs <- sellers_ihs[
  complete.cases(
    sellers_ihs[, vars_inc_sellers_ihs, drop = FALSE]
  ),
  ,
  drop = FALSE
]

# 3. Keep catchments with both male and female sellers
male_counts_sellers <- tapply(
  sellers_ihs[[group_var]] == male_value,
  sellers_ihs$catchID,
  sum
)

female_counts_sellers <- tapply(
  sellers_ihs[[group_var]] == female_value,
  sellers_ihs$catchID,
  sum
)

valid_catch_sellers_ihs <- intersect(
  names(male_counts_sellers)[male_counts_sellers > 0],
  names(female_counts_sellers)[female_counts_sellers > 0]
)

sellers_fe_ihs <- sellers_ihs[
  as.character(sellers_ihs$catchID) %in%
    valid_catch_sellers_ihs,
  ,
  drop = FALSE
]

# 4. Within transformation among sellers only
vars_to_demean_sellers_ihs <- unique(c(
  y_inc_sellers_ihs,
  x_inc
))

vars_to_demean_sellers_ihs <-
  vars_to_demean_sellers_ihs[
    vars_to_demean_sellers_ihs %in%
      names(sellers_fe_ihs)
  ]

sellers_fe_within_ihs <- within_transform(
  sellers_fe_ihs,
  vars_to_demean_sellers_ihs,
  "catchID"
)

sellers_fe_within_ihs$catchID <-
  sellers_fe_ihs$catchID

# 5. Oaxaca with clustered bootstrap
boot_inc_sellers_ihs <- bootstrap_oaxaca_fe(
  df = sellers_fe_within_ihs,
  y = y_inc_sellers_ihs,
  x_vars = x_inc,
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_boot,
  seed = seed_boot
)

# 6. Aggregate table
tab_nested_inc_sellers_ihs <-
  make_nested_oaxaca_table(
    boot_inc_sellers_ihs
  )

N_inc_sellers_ihs_male <-
  boot_inc_sellers_ihs$base$N_male

N_inc_sellers_ihs_female <-
  boot_inc_sellers_ihs$base$N_female

print(tab_nested_inc_sellers_ihs)


tab_nested_inc_sellers_ihs <<-
  tab_nested_inc_sellers_ihs


N_inc_sellers_ihs_male <<-
  N_inc_sellers_ihs_male

N_inc_sellers_ihs_female <<-
  N_inc_sellers_ihs_female

#DETAILED SEPRATED
# * DETAILED EXTENSIVE MARGIN: FAIRLIE LOGIT

fairlie_detailed_core <- function(df, y, x_vars, group_var="hh_gender_num",
                                  male_val=1, female_val=0, cluster="catchID",
                                  link="logit", catch_fe=TRUE,
                                  reps_order=1000, seed=123){
  
  set.seed(seed)
  vars_need <- unique(c(y, group_var, x_vars, cluster))
  d <- df[complete.cases(df[, vars_need, drop=FALSE]), , drop=FALSE]
  d <- d[d[[y]] %in% c(0,1), , drop=FALSE]
  
  male_counts <- tapply(d[[group_var]] == male_val, d[[cluster]], sum)
  female_counts <- tapply(d[[group_var]] == female_val, d[[cluster]], sum)
  valid_catch <- intersect(names(male_counts)[male_counts > 0],
                           names(female_counts)[female_counts > 0])
  d <- d[as.character(d[[cluster]]) %in% valid_catch, , drop=FALSE]
  d[[cluster]] <- factor(d[[cluster]])
  
  rhs <- x_vars
  if(catch_fe) rhs <- c(rhs, paste0("factor(", cluster, ")"))
  form <- as.formula(paste(y, "~", paste(rhs, collapse=" + ")))
  
  mod <- suppressWarnings(glm(form, data=d, family=binomial(link=link)))
  X <- model.matrix(mod)
  b <- coef(mod)
  b[is.na(b)] <- 0
  
  linkinv <- family(mod)$linkinv
  pred <- linkinv(drop(X %*% b))
  
  idx_m <- which(d[[group_var]] == male_val)
  idx_f <- which(d[[group_var]] == female_val)
  n_match <- min(length(idx_m), length(idx_f))
  
  term_labels <- attr(terms(mod), "term.labels")
  assign_vec <- attr(X, "assign")
  
  block_names <- x_vars
  block_terms <- x_vars
  
  if(catch_fe){
    block_names <- c(block_names, "Catchment-area fixed effects")
    block_terms <- c(block_terms, paste0("factor(", cluster, ")"))
  }
  
  block_cols <- lapply(block_terms, function(z){
    j <- match(z, term_labels)
    which(assign_vec == j)
  })
  
  contrib_draws <- matrix(NA_real_, reps_order, length(block_names),
                          dimnames=list(NULL, block_names))
  
  for(r in seq_len(reps_order)){
    im <- if(length(idx_m) > n_match) sample(idx_m, n_match) else idx_m
    jf <- if(length(idx_f) > n_match) sample(idx_f, n_match) else idx_f
    
    im <- im[order(pred[im])]
    jf <- jf[order(pred[jf])]
    
    Xm <- X[im, , drop=FALSE]
    Xf <- X[jf, , drop=FALSE]
    eta <- drop(Xf %*% b)
    
    ord <- sample(seq_along(block_names))
    cc <- setNames(numeric(length(block_names)), block_names)
    
    for(k in ord){
      cols <- block_cols[[k]]
      delta <- drop((Xm[, cols, drop=FALSE] - Xf[, cols, drop=FALSE]) %*% b[cols])
      eta_new <- eta + delta
      cc[k] <- mean(linkinv(eta_new)) - mean(linkinv(eta))
      eta <- eta_new
    }
    
    contrib_draws[r, ] <- cc
  }
  
  contributions <- colMeans(contrib_draws, na.rm=TRUE)
  endowment <- mean(pred[idx_m]) - mean(pred[idx_f])
  
  list(data=d,
       N_male=length(idx_m), N_female=length(idx_f),
       Endowment=endowment,
       Contributions=contributions,
       Detailed_sum=sum(contributions),
       model=mod)
}


# * Catchment-cluster bootstrap for detailed Fairlie

bootstrap_fairlie_detailed <- function(df, y, x_vars, group_var="hh_gender_num",
                                       male_val=1, female_val=0, cluster="catchID",
                                       link="logit", catch_fe=TRUE,
                                       R=300, reps_order_base=1000,
                                       reps_order_boot=100, seed=123){
  
  base <- fairlie_detailed_core(
    df, y, x_vars, group_var, male_val, female_val, cluster,
    link, catch_fe, reps_order_base, seed
  )
  
  d <- base$data
  clusters <- unique(as.character(d[[cluster]]))
  terms_det <- names(base$Contributions)
  
  draws <- matrix(NA_real_, R, length(terms_det),
                  dimnames=list(NULL, terms_det))
  
  for(r in seq_len(R)){
    set.seed(seed + r)
    sampled <- sample(clusters, length(clusters), replace=TRUE)
    
    dd <- do.call(rbind, lapply(seq_along(sampled), function(i){
      z <- d[as.character(d[[cluster]]) == sampled[i], , drop=FALSE]
      z[[cluster]] <- paste0("bootstrap_", i)
      z
    }))
    
    rr <- tryCatch(
      fairlie_detailed_core(
        dd, y, x_vars, group_var, male_val, female_val, cluster,
        link, catch_fe, reps_order_boot, seed + 10000 + r
      ),
      error=function(e) NULL
    )
    
    if(!is.null(rr))
      draws[r, ] <- rr$Contributions[terms_det]
  }
  
  list(base=base,
       se_det=apply(draws, 2, sd, na.rm=TRUE),
       successful_replications=sum(complete.cases(draws)))
}


# * Run detailed Fairlie

boot_det_ext_logit <- bootstrap_fairlie_detailed(
  df=baseline_farmers,
  y="maize_sold",
  x_vars=x_inc,
  group_var=group_var,
  male_val=male_value,
  female_val=female_value,
  cluster="catchID",
  link="logit",
  catch_fe=TRUE,
  R=R_boot,
  reps_order_base=1000,
  reps_order_boot=100,
  seed=seed_boot
)


# * Format detailed Fairlie table

make_fairlie_detailed_table <- function(boot_obj, labels_map){
  est <- boot_obj$base$Contributions
  se <- boot_obj$se_det[names(est)]
  nm <- names(est)
  pretty <- ifelse(nm %in% names(labels_map), labels_map[nm], nm)
  
  data.frame(
    Variable=latex_esc(pretty),
    Contribution=mapply(star_cell, est, se),
    stringsAsFactors=FALSE
  )
}

tab_det_ext_logit <- make_fairlie_detailed_table(
  boot_det_ext_logit, labels_inc
)

latex_rows_det_ext_logit <- make_latex_rows(
  tab_det_ext_logit,
  c("Variable", "Contribution")
)

print(tab_det_ext_logit)

cat("Detailed Fairlie sum: ",
    round(boot_det_ext_logit$base$Detailed_sum, 4), "\n")
cat("Aggregate explained component: ",
    round(boot_det_ext_logit$base$Endowment, 4), "\n")

# * Remove catchment fixed effects from displayed detailed table
tab_det_ext_logit_show <- tab_det_ext_logit[
  tab_det_ext_logit$Variable != "Catchment-area fixed effects",
  , drop = FALSE
]

latex_rows_det_ext_logit <- make_latex_rows(
  tab_det_ext_logit_show,
  c("Variable", "Contribution")
)

tab_det_ext_logit_show <<- tab_det_ext_logit_show
latex_rows_det_ext_logit <<- latex_rows_det_ext_logit
# * DETAILED INTENSIVE MARGIN: OAXACA AMONG SELLERS

tab_det_twofold_inc_sellers_ihs <- make_twofold_det_fe(
  boot_inc_sellers_ihs,
  labels_inc,
  "Gross revenue among sellers (IHS)"
)

latex_rows_twofold_inc_sellers_ihs <- make_latex_rows(
  tab_det_twofold_inc_sellers_ihs,
  c("Variable", "Explained", "Coefficients")
)

print(tab_det_twofold_inc_sellers_ihs)


# * Optional threefold intensive table

tab_det_threefold_inc_sellers_ihs <- make_threefold_det_fe(
  boot_inc_sellers_ihs,
  labels_inc,
  "Gross revenue among sellers (IHS)"
)

latex_rows_threefold_inc_sellers_ihs <- make_latex_rows(
  tab_det_threefold_inc_sellers_ihs,
  c("Variable", "Endowment", "Male_coefficients", "Female_coefficients")
)


# * Export for LyX

tab_det_ext_logit <<- tab_det_ext_logit
latex_rows_det_ext_logit <<- latex_rows_det_ext_logit

tab_det_twofold_inc_sellers_ihs <<- tab_det_twofold_inc_sellers_ihs
latex_rows_twofold_inc_sellers_ihs <<- latex_rows_twofold_inc_sellers_ihs

tab_det_threefold_inc_sellers_ihs <<- tab_det_threefold_inc_sellers_ihs
latex_rows_threefold_inc_sellers_ihs <<- latex_rows_threefold_inc_sellers_ihs
#check fast
boot_det_ext_logit$successful_replications
boot_det_ext_logit$base$Detailed_sum
boot_det_ext_logit$base$Endowment



# ============================================================
# COMMON SUPPORT CHECK (extensive margin, logit)
# ============================================================

check_common_support <- function(df, y, x_vars, group_var = "hh_gender_num",
                                 male_val = 1, female_val = 0,
                                 cluster = "catchID", link = "logit",
                                 catch_fe = TRUE) {
  
  vars_need <- unique(c(y, group_var, x_vars, cluster))
  d <- df[complete.cases(df[, vars_need, drop = FALSE]), , drop = FALSE]
  d <- d[d[[y]] %in% c(0, 1), , drop = FALSE]
  
  male_counts <- tapply(d[[group_var]] == male_val, d[[cluster]], sum)
  female_counts <- tapply(d[[group_var]] == female_val, d[[cluster]], sum)
  valid_catch <- intersect(names(male_counts)[male_counts > 0],
                           names(female_counts)[female_counts > 0])
  d <- d[as.character(d[[cluster]]) %in% valid_catch, , drop = FALSE]
  d[[cluster]] <- factor(d[[cluster]])
  
  rhs <- x_vars
  if (catch_fe) rhs <- c(rhs, paste0("factor(", cluster, ")"))
  form <- as.formula(paste(y, "~", paste(rhs, collapse = " + ")))
  
  mod <- suppressWarnings(glm(form, data = d, family = binomial(link = link)))
  
  d$pred <- predict(mod, type = "response")
  
  list(data = d, model = mod)
}

cs <- check_common_support(
  df = baseline_farmers,
  y = "maize_sold",
  x_vars = x_inc,
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  link = "logit",
  catch_fe = TRUE
)

# Grafico di densità sovrapposta
library(ggplot2)

ggplot(cs$data, aes(x = pred, fill = factor(get(group_var)))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(
    values = c("#E69F00", "#56B4E9"),
    labels = c("FHH", "MHH"),
    name = "Household type"
  ) +
  labs(
    x = "Predicted probability of selling maize",
    y = "Density"
  ) +
  theme_minimal()


ggsave("common_support_ext_margin.png", width = 7, height = 5)





#APPENDIX
# ============================================================
# RIF-OAXACA DECOMPOSITION
# Productivity + overall gross sales revenue


library(dplyr)

# -------------------- SETTINGS --------------------

y_prod       <- "yield_per_acre_ihs"
y_inc        <- "maize_income_ihs"
group_var    <- "hh_gender_num"
male_value   <- 1
female_value <- 0
R_rif        <- 300
seed_rif     <- 123

# Exact productivity specification used in the main Oaxaca
x_prod_rif <- c(
  "distance_agroshops", "num_shops", "household_size", "hh_age",
  "education_head_num", "maize_plot_area", "farmer_group_member",
  "dap_npk_applied", "urea_applied", "chemicals_applied",
  "organic_manure_applied", "resow", "weed_times", "base_hybrid"
)

# Exact gross-revenue specification used in the main Oaxaca
x_inc_rif <- c(
  "distance_agroshops", "num_shops", "household_size", "hh_age",
  "education_head_num", "quality_seed_used", "farmer_group_member",
  "log_maize_plot_area", "dap_npk_applied", "urea_applied",
  "chemicals_applied", "organic_manure_applied", "resow", "weed_times"
)

# -------------------- FORMATTERS --------------------

fmt3 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  format(round(x, 3), nsmall = 3, trim = TRUE)
}

pstars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

star_cell <- function(est, se) {
  est <- suppressWarnings(as.numeric(est))
  se  <- suppressWarnings(as.numeric(se))
  if (is.na(est)) return("")
  if (is.na(se) || se == 0) return(fmt3(est))
  p <- 2 * pnorm(-abs(est / se))
  paste0(fmt3(est), pstars(p))
}

fmt_share_rif <- function(x) {
  if (is.na(x) || !is.finite(x)) return("--")
  sprintf("%.1f", 100 * x)
}

# -------------------- CHECK SAMPLES --------------------

check_prod <- baseline_fe[
  complete.cases(baseline_fe[, c(y_prod, group_var, x_prod_rif, "catchID")]),
  , drop = FALSE
]

check_inc <- baseline_fe[
  complete.cases(baseline_fe[, c(y_inc, group_var, x_inc_rif, "catchID")]),
  , drop = FALSE
]

cat("\nPRODUCTIVITY SAMPLE\n")
print(table(check_prod[[group_var]]))

cat("\nREVENUE SAMPLE\n")
print(table(check_inc[[group_var]]))

# Productivity should be:
# 0 = 594
# 1 = 2132

# -------------------- COMPUTE RIF --------------------

compute_rif <- function(y, tau) {
  y <- as.numeric(y)
  q_tau <- as.numeric(quantile(y, probs = tau, type = 7, na.rm = TRUE))
  dens <- density(y, na.rm = TRUE, bw = "nrd0")
  f_q <- approx(dens$x, dens$y, xout = q_tau, rule = 2)$y
  
  if (!is.finite(f_q) || f_q <= 1e-8) {
    stop("Density at the selected quantile is too small.")
  }
  
  rif_raw <- q_tau + (tau - as.numeric(y <= q_tau)) / f_q
  
  # finite-sample recentering
  rif <- rif_raw - mean(rif_raw, na.rm = TRUE) + q_tau
  
  list(
    rif = rif,
    q = q_tau,
    density = f_q,
    mass = mean(y == q_tau, na.rm = TRUE)
  )
}

# -------------------- ONE RIF-OAXACA --------------------

rif_oaxaca_single <- function(df, y, x_vars, tau,
                              group_var = "hh_gender_num",
                              male_val = 1, female_val = 0,
                              cluster = "catchID") {
  
  vars_need <- c(y, group_var, x_vars, cluster)
  d <- df[complete.cases(df[, vars_need]), , drop = FALSE]
  
  # baseline_fe is already restricted to catchments containing MHH and FHH
  d[[cluster]] <- factor(d[[cluster]])
  
  dm   <- d[d[[group_var]] == male_val, , drop = FALSE]
  dfem <- d[d[[group_var]] == female_val, , drop = FALSE]
  
  # Group-specific RIFs
  r_m <- compute_rif(dm[[y]], tau)
  r_f <- compute_rif(dfem[[y]], tau)
  
  # Pooled RIF used only to obtain pooled benchmark coefficients
  r_p <- compute_rif(d[[y]], tau)
  
  dm$rif_y   <- r_m$rif
  dfem$rif_y <- r_f$rif
  d$rif_y    <- r_p$rif
  
  # Catchment-area fixed effects
  rhs <- paste(c(x_vars, paste0("factor(", cluster, ")")), collapse = " + ")
  
  form_group <- as.formula(paste("rif_y ~", rhs))
  
  # IMPORTANT: pooled benchmark WITHOUT gender dummy
  form_pool <- as.formula(paste("rif_y ~", rhs))
  
  form_x <- as.formula(paste("~", rhs))
  
  fit_m <- lm(form_group, data = dm)
  fit_f <- lm(form_group, data = dfem)
  fit_p <- lm(form_pool, data = d)
  
  beta_m0 <- coef(fit_m)
  beta_f0 <- coef(fit_f)
  beta_p0 <- coef(fit_p)
  
  X_m0 <- colMeans(model.matrix(form_x, data = dm))
  X_f0 <- colMeans(model.matrix(form_x, data = dfem))
  
  all_names <- Reduce(
    union,
    list(names(beta_m0), names(beta_f0), names(beta_p0), names(X_m0), names(X_f0))
  )
  
  align <- function(x) {
    out <- setNames(rep(0, length(all_names)), all_names)
    good <- intersect(names(x), all_names)
    out[good] <- x[good]
    out
  }
  
  beta_m <- align(beta_m0)
  beta_f <- align(beta_f0)
  beta_p <- align(beta_p0)
  X_m <- align(X_m0)
  X_f <- align(X_f0)
  
  # Oaxaca components
  endow_k <- (X_m - X_f) * beta_p
  male_coef_k <- X_m * (beta_m - beta_p)
  female_coef_k <- X_f * (beta_p - beta_f)
  coefficient_k <- male_coef_k + female_coef_k
  
  Endowment <- sum(endow_k)
  Male_coef <- sum(male_coef_k)
  Female_coef <- sum(female_coef_k)
  Coefficient <- sum(coefficient_k)
  
  # Actual unconditional quantile gap
  Total_gap <- r_m$q - r_f$q
  
  Reconstruction_error <- Endowment + Coefficient - Total_gap
  
  detailed <- data.frame(
    term = all_names,
    Endowment = endow_k,
    Male_coef = male_coef_k,
    Female_coef = female_coef_k,
    Coefficient = coefficient_k,
    stringsAsFactors = FALSE
  )
  
  list(
    tau = tau,
    N_male = nrow(dm),
    N_female = nrow(dfem),
    Q_male = r_m$q,
    Q_female = r_f$q,
    Total_gap = Total_gap,
    Endowment = Endowment,
    Coefficient = Coefficient,
    Male_coef = Male_coef,
    Female_coef = Female_coef,
    Share_Endowment = ifelse(abs(Total_gap) < 1e-10, NA, Endowment / Total_gap),
    Share_Coefficient = ifelse(abs(Total_gap) < 1e-10, NA, Coefficient / Total_gap),
    Reconstruction_error = Reconstruction_error,
    Mass_male = r_m$mass,
    Mass_female = r_f$mass,
    detailed = detailed
  )
}

# -------------------- CATCHMENT-CLUSTER BOOTSTRAP --------------------

bootstrap_rif_oaxaca <- function(df, y, x_vars, tau,
                                 group_var = "hh_gender_num",
                                 male_val = 1, female_val = 0,
                                 cluster = "catchID",
                                 R = 300, seed = 123) {
  
  set.seed(seed)
  
  vars_need <- c(y, group_var, x_vars, cluster)
  d <- df[complete.cases(df[, vars_need]), , drop = FALSE]
  
  base <- rif_oaxaca_single(
    d, y, x_vars, tau,
    group_var, male_val, female_val, cluster
  )
  
  terms <- base$detailed$term
  clusters <- unique(d[[cluster]])
  n_cl <- length(clusters)
  
  agg_names <- c(
    "Total_gap", "Endowment", "Coefficient",
    "Male_coef", "Female_coef"
  )
  
  agg_draws <- matrix(
    NA_real_, nrow = R, ncol = length(agg_names),
    dimnames = list(NULL, agg_names)
  )
  
  det_endow <- matrix(
    NA_real_, nrow = R, ncol = length(terms),
    dimnames = list(NULL, terms)
  )
  
  det_coef <- matrix(
    NA_real_, nrow = R, ncol = length(terms),
    dimnames = list(NULL, terms)
  )
  
  for (r in seq_len(R)) {
    
    sampled_clusters <- sample(clusters, n_cl, replace = TRUE)
    
    dd <- do.call(
      rbind,
      lapply(seq_along(sampled_clusters), function(i) {
        tmp <- d[d[[cluster]] == sampled_clusters[i], , drop = FALSE]
        tmp[[cluster]] <- paste0("boot_", i)
        tmp
      })
    )
    
    rr <- tryCatch(
      rif_oaxaca_single(
        dd, y, x_vars, tau,
        group_var, male_val, female_val, cluster
      ),
      error = function(e) NULL
    )
    
    if (is.null(rr)) next
    
    agg_draws[r, ] <- c(
      rr$Total_gap,
      rr$Endowment,
      rr$Coefficient,
      rr$Male_coef,
      rr$Female_coef
    )
    
    det <- rr$detailed[
      match(terms, rr$detailed$term),
      , drop = FALSE
    ]
    
    det_endow[r, ] <- det$Endowment
    det_coef[r, ] <- det$Coefficient
  }
  
  list(
    base = base,
    se_agg = apply(agg_draws, 2, sd, na.rm = TRUE),
    se_endow = apply(det_endow, 2, sd, na.rm = TRUE),
    se_coef = apply(det_coef, 2, sd, na.rm = TRUE),
    successful_boot = sum(complete.cases(agg_draws))
  )
}

# -------------------- RUN GRID --------------------

run_rif_grid <- function(df, y, x_vars, quantiles,
                         group_var = "hh_gender_num",
                         male_val = 1, female_val = 0,
                         cluster = "catchID",
                         R = 300, seed = 123) {
  
  out <- lapply(
    quantiles,
    function(tau) {
      bootstrap_rif_oaxaca(
        df = df,
        y = y,
        x_vars = x_vars,
        tau = tau,
        group_var = group_var,
        male_val = male_val,
        female_val = female_val,
        cluster = cluster,
        R = R,
        seed = seed
      )
    }
  )
  
  names(out) <- paste0(
    "q",
    as.integer(round(100 * quantiles))
  )
  
  out
}

# -------------------- QUANTILES --------------------

q_prod <- seq(0.10, 0.90, by = 0.10)
q_rev  <- c(0.70, 0.80, 0.90)

# -------------------- RUN PRODUCTIVITY --------------------

rif_prod_fe <- run_rif_grid(
  df = baseline_fe,
  y = y_prod,
  x_vars = x_prod_rif,
  quantiles = q_prod,
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_rif,
  seed = seed_rif
)

# -------------------- RUN REVENUE --------------------

rif_rev_fe <- run_rif_grid(
  df = baseline_fe,
  y = y_inc,
  x_vars = x_inc_rif,
  quantiles = q_rev,
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_rif,
  seed = seed_rif
)

# -------------------- AGGREGATE TABLES --------------------

make_rif_table <- function(obj) {
  
  rows <- lapply(obj, function(r) {
    
    b <- r$base
    se <- r$se_agg
    
    data.frame(
      Quantile = paste0(as.integer(round(100 * b$tau)), "th"),
      MHH = fmt3(b$Q_male),
      FHH = fmt3(b$Q_female),
      Total_gap = star_cell(b$Total_gap, se["Total_gap"]),
      Endowment = star_cell(b$Endowment, se["Endowment"]),
      Share_Endowment = fmt_share_rif(b$Share_Endowment),
      Coefficient = star_cell(b$Coefficient, se["Coefficient"]),
      Share_Coefficient = fmt_share_rif(b$Share_Coefficient),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, rows)
}

tab_rif_prod <- make_rif_table(rif_prod_fe)
tab_rif_rev  <- make_rif_table(rif_rev_fe)

N_rif_prod_male   <- rif_prod_fe[[1]]$base$N_male
N_rif_prod_female <- rif_prod_fe[[1]]$base$N_female
N_rif_rev_male    <- rif_rev_fe[[1]]$base$N_male
N_rif_rev_female  <- rif_rev_fe[[1]]$base$N_female

print(tab_rif_prod)
print(tab_rif_rev)

cat("\nProductivity N male/female:", N_rif_prod_male, "/", N_rif_prod_female, "\n")
cat("Revenue N male/female:", N_rif_rev_male, "/", N_rif_rev_female, "\n")

# -------------------- DIAGNOSTICS --------------------

reconstruction_prod <- sapply(
  rif_prod_fe,
  function(x) x$base$Reconstruction_error
)

reconstruction_rev <- sapply(
  rif_rev_fe,
  function(x) x$base$Reconstruction_error
)

successful_prod <- sapply(
  rif_prod_fe,
  function(x) x$successful_boot
)

successful_rev <- sapply(
  rif_rev_fe,
  function(x) x$successful_boot
)

mass_prod <- data.frame(
  Quantile = names(rif_prod_fe),
  MHH = sapply(rif_prod_fe, function(x) x$base$Mass_male),
  FHH = sapply(rif_prod_fe, function(x) x$base$Mass_female)
)

cat("\nRECONSTRUCTION PRODUCTIVITY\n")
print(reconstruction_prod)

cat("\nRECONSTRUCTION REVENUE\n")
print(reconstruction_rev)

cat("\nSUCCESSFUL BOOTSTRAPS PRODUCTIVITY\n")
print(successful_prod)

cat("\nSUCCESSFUL BOOTSTRAPS REVENUE\n")
print(successful_rev)

cat("\nMASS POINTS PRODUCTIVITY\n")
print(mass_prod)

# -------------------- CHECK AGAINST MAIN MEAN OAXACA --------------------
# This uses your original main-Oaxaca function and baseline_fe_within.
# It should reproduce the main productivity Oaxaca, not the q50 RIF.

mean_prod_check <- oaxaca_decomp_nogender(
  baseline_fe_within,
  y = y_prod,
  x_vars = x_prod_rif,
  group_var = group_var,
  male_val = male_value,
  female_val = female_value
)

cat("\nMAIN MEAN OAXACA CHECK\n")

print(c(
  N_male = mean_prod_check$N_male,
  N_female = mean_prod_check$N_female,
  Total_gap = mean_prod_check$Total_gap,
  Endowment = mean_prod_check$Explained,
  Share_Endowment = 100 * mean_prod_check$Share_Explained,
  Coefficient = mean_prod_check$Coefficients,
  Share_Coefficient = 100 * mean_prod_check$Share_Coefficients
))

# RESET TESTS
library(lmtest)

reset_prod_bl <- resettest(m_prod_full_bl, power = 2:3, type = "fitted")
reset_inc_bl <- resettest(m_inc_full_bl, power = 2:3, type = "fitted")
reset_inc_sellers <- resettest(m_inc_full_sellers, power = 2:3, type = "fitted")

tab_reset <- data.frame(
  Model = c(
    "Maize productivity, baseline",
    "Gross sales revenue, baseline",
    "Gross sales revenue, sellers only"
  ),
  Fstat = c(
    unname(reset_prod_bl$statistic),
    unname(reset_inc_bl$statistic),
    unname(reset_inc_sellers$statistic)
  ),
  df1 = c(
    unname(reset_prod_bl$parameter[1]),
    unname(reset_inc_bl$parameter[1]),
    unname(reset_inc_sellers$parameter[1])
  ),
  df2 = c(
    unname(reset_prod_bl$parameter[2]),
    unname(reset_inc_bl$parameter[2]),
    unname(reset_inc_sellers$parameter[2])
  ),
  pvalue = c(
    reset_prod_bl$p.value,
    reset_inc_bl$p.value,
    reset_inc_sellers$p.value
  )
)
# ROBUSTNESS: PROBIT FOR MARKET PARTICIPATION

m_ext_full_bl_probit <- glm(
  formula(m_ext_full_bl),
  data = model.frame(m_ext_full_bl),
  family = binomial(link = "probit")
)

summary(m_ext_full_bl_probit)

# ============================================================
# PROBIT AVERAGE MARGINAL EFFECTS - MARKET PARTICIPATION
# ============================================================
library(sandwich)
ame_probit <- function(model, term, data, cluster_id = CLUSTER_ID){
  mf <- model.frame(model)
  idx <- as.integer(rownames(mf))
  d <- data[idx, , drop = FALSE]
  
  beta <- coef(model)
  V <- vcovCL(model, cluster = data[[cluster_id]][idx], type = "HC1")
  trm <- delete.response(terms(model))
  
  vals <- unique(d[[term]][!is.na(d[[term]])])
  binary <- length(vals) <= 2 && all(vals %in% c(0,1))
  
  ame_fun <- function(b){
    if(binary){
      d0 <- d
      d1 <- d
      d0[[term]] <- 0
      d1[[term]] <- 1
      
      X0 <- model.matrix(trm, d0)
      X1 <- model.matrix(trm, d1)
      
      mean(pnorm(X1 %*% b) - pnorm(X0 %*% b))
      
    } else {
      X <- model.matrix(trm, d)
      xb <- drop(X %*% b)
      j <- which(colnames(X) == term)
      
      mean(dnorm(xb) * b[j])
    }
  }
  
  est <- ame_fun(beta)
  
  grad <- sapply(seq_along(beta), function(j){
    h <- 1e-6 * max(1, abs(beta[j]))
    b1 <- b0 <- beta
    b1[j] <- b1[j] + h
    b0[j] <- b0[j] - h
    (ame_fun(b1) - ame_fun(b0)) / (2 * h)
  })
  
  se <- sqrt(as.numeric(t(grad) %*% V %*% grad))
  pval <- 2 * pnorm(-abs(est / se))
  
  c(est = est, se = se, p = pval)
}

# Compute Probit AMEs for all variables
probit_ame_results <- do.call(
  rbind,
  lapply(vars_inc, function(v){
    z <- ame_probit(
      m_ext_full_bl_probit,
      v,
      baseline_farmers,
      CLUSTER_ID
    )
    
    data.frame(
      Variable = v,
      AME = z["est"],
      SE = z["se"],
      pvalue = z["p"]
    )
  })
)

rownames(probit_ame_results) <- NULL

probit_ame_results$AME <- round(probit_ame_results$AME, 3)
probit_ame_results$SE <- round(probit_ame_results$SE, 3)
probit_ame_results$pvalue <- round(probit_ame_results$pvalue, 4)

print(probit_ame_results)
# ============================================================
# ROBUSTNESS TABLE: LOGIT VS PROBIT - MARKET PARTICIPATION
# ============================================================

cell_probit_ame <- function(model, term, data, cluster_id = CLUSTER_ID){
  z <- tryCatch(
    ame_probit(model, term, data, cluster_id),
    error = function(e) NULL
  )
  
  if(is.null(z)) return(c("—", ""))
  
  stars <- ifelse(z["p"] < .01, "***",
                  ifelse(z["p"] < .05, "**",
                         ifelse(z["p"] < .10, "*", "")))
  
  c(
    paste0(format(round(z["est"], 3), nsmall = 3), stars),
    paste0("(", format(round(z["se"], 3), nsmall = 3), ")")
  )
}

tab_ext_robust <- data.frame(
  Label = character(),
  Logit = character(),
  Probit = character(),
  stringsAsFactors = FALSE
)

for(v in vars_inc){
  
  logit <- cell_logit_ame(
    m_ext_full_bl,
    v,
    baseline_farmers
  )
  
  probit <- cell_probit_ame(
    m_ext_full_bl_probit,
    v,
    baseline_farmers
  )
  
  tab_ext_robust <- rbind(
    tab_ext_robust,
    data.frame(
      Label = lab_inc[v],
      Logit = logit[1],
      Probit = probit[1]
    ),
    data.frame(
      Label = "",
      Logit = logit[2],
      Probit = probit[2]
    )
  )
}

N_ext_robust_logit <- format(nobs(m_ext_full_bl), big.mark = ",")
N_ext_robust_probit <- format(nobs(m_ext_full_bl_probit), big.mark = ",")

print(tab_ext_robust)

tab_ext_robust <<- tab_ext_robust
N_ext_robust_logit <<- N_ext_robust_logit
N_ext_robust_probit <<- N_ext_robust_probit



# ============================================================
# ROBUSTNESS: ALTERNATIVE OAXACA-BLINDER BENCHMARKS
# MHH benchmark + FHH benchmark + equal-group-weight pooled
# Twofold decomposition only
# ============================================================

oaxaca_decomp_benchmark <- function(
    df, y, x_vars,
    benchmark = c("male", "female", "equal_weight"),
    group_var = "hh_gender_num",
    male_val = 1,
    female_val = 0) {
  
  benchmark <- match.arg(benchmark)
  
  vars_need <- c(y, group_var, x_vars)
  d <- df[complete.cases(df[, vars_need]), , drop = FALSE]
  dm <- d[d[[group_var]] == male_val, , drop = FALSE]
  dfem <- d[d[[group_var]] == female_val, , drop = FALSE]
  
  rhs <- paste(x_vars, collapse = " + ")
  form_y <- as.formula(paste(y, "~", rhs))
  form_x <- as.formula(paste("~", rhs))
  
  beta_m0 <- coef(lm(form_y, data = dm))
  beta_f0 <- coef(lm(form_y, data = dfem))
  
  # Alternative benchmark beta*
  if(benchmark == "male"){
    beta_star0 <- beta_m0
  }
  
  if(benchmark == "female"){
    beta_star0 <- beta_f0
  }
  
  if(benchmark == "equal_weight"){
    nm <- nrow(dm)
    nf <- nrow(dfem)
    n <- nrow(d)
    
    # Each group receives the same total weight
    w <- ifelse(
      d[[group_var]] == male_val,
      n / (2 * nm),
      n / (2 * nf)
    )
    
    beta_star0 <- coef(
      lm(form_y, data = d, weights = w)
    )
  }
  
  X_m0 <- colMeans(model.matrix(form_x, data = dm))
  X_f0 <- colMeans(model.matrix(form_x, data = dfem))
  
  all_names <- Reduce(
    union,
    list(
      names(beta_m0), names(beta_f0),
      names(beta_star0),
      names(X_m0), names(X_f0)
    )
  )
  
  align <- function(x){
    out <- setNames(rep(0, length(all_names)), all_names)
    good <- intersect(names(x), all_names)
    out[good] <- x[good]
    out
  }
  
  beta_m <- align(beta_m0)
  beta_f <- align(beta_f0)
  beta_star <- align(beta_star0)
  X_m <- align(X_m0)
  X_f <- align(X_f0)
  
  # Twofold Oaxaca-Blinder
  Endow_k <- (X_m - X_f) * beta_star
  
  Coefficients_k <-
    X_m * (beta_m - beta_star) +
    X_f * (beta_star - beta_f)
  
  Endow <- sum(Endow_k)
  Coefficients <- sum(Coefficients_k)
  Total_gap <- Endow + Coefficients
  
  detailed <- data.frame(
    term = all_names,
    Explained_k = Endow_k,
    Coefficients_k = Coefficients_k,
    stringsAsFactors = FALSE
  )
  
  list(
    N = nrow(d),
    N_male = nrow(dm),
    N_female = nrow(dfem),
    Total_gap = Total_gap,
    Explained = Endow,
    Coefficients = Coefficients,
    Share_Explained = safe_share(Endow, Total_gap),
    Share_Coefficients = safe_share(Coefficients, Total_gap),
    detailed = detailed,
    benchmark = benchmark
  )
}


# ============================================================
# CLUSTER BOOTSTRAP AT catchID LEVEL
# ============================================================

bootstrap_oaxaca_benchmark <- function(
    df, y, x_vars, benchmark,
    group_var = "hh_gender_num",
    male_val = 1,
    female_val = 0,
    cluster = "catchID",
    R = 300,
    seed = 123) {
  
  set.seed(seed)
  
  d <- df[
    complete.cases(df[, c(y, group_var, x_vars, cluster)]),
    ,
    drop = FALSE
  ]
  
  base <- oaxaca_decomp_benchmark(
    d, y, x_vars, benchmark,
    group_var, male_val, female_val
  )
  
  clusters <- unique(d[[cluster]])
  n_cl <- length(clusters)
  
  draws <- matrix(
    NA_real_, R, 2,
    dimnames = list(NULL, c("Explained", "Coefficients"))
  )
  
  for(r in seq_len(R)){
    
    sampled <- sample(clusters, n_cl, replace = TRUE)
    
    dd <- do.call(
      rbind,
      lapply(
        sampled,
        function(cl)
          d[d[[cluster]] == cl, , drop = FALSE]
      )
    )
    
    rr <- tryCatch(
      oaxaca_decomp_benchmark(
        dd, y, x_vars, benchmark,
        group_var, male_val, female_val
      ),
      error = function(e) NULL
    )
    
    if(is.null(rr)) next
    
    draws[r, ] <- c(
      rr$Explained,
      rr$Coefficients
    )
  }
  
  list(
    base = base,
    se_agg = apply(draws, 2, sd, na.rm = TRUE)
  )
}


# ============================================================
# RUN: PRODUCTIVITY
# ============================================================

boot_prod_bm_male <- bootstrap_oaxaca_benchmark(
  baseline_fe_within,
  y_prod, x_prod,
  benchmark = "male",
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_boot,
  seed = seed_boot
)

boot_prod_bm_female <- bootstrap_oaxaca_benchmark(
  baseline_fe_within,
  y_prod, x_prod,
  benchmark = "female",
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_boot,
  seed = seed_boot
)

boot_prod_bm_equal <- bootstrap_oaxaca_benchmark(
  baseline_fe_within,
  y_prod, x_prod,
  benchmark = "equal_weight",
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_boot,
  seed = seed_boot
)


# ============================================================
# RUN: GROSS SALES REVENUE
# ============================================================

boot_inc_bm_male <- bootstrap_oaxaca_benchmark(
  baseline_fe_within,
  y_inc, x_inc,
  benchmark = "male",
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_boot,
  seed = seed_boot
)

boot_inc_bm_female <- bootstrap_oaxaca_benchmark(
  baseline_fe_within,
  y_inc, x_inc,
  benchmark = "female",
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_boot,
  seed = seed_boot
)

boot_inc_bm_equal <- bootstrap_oaxaca_benchmark(
  baseline_fe_within,
  y_inc, x_inc,
  benchmark = "equal_weight",
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_boot,
  seed = seed_boot
)

# ============================================================
# TABLES FOR LYX
# ============================================================

benchmark_effect_cell <- function(boot, component){
  
  if(component == "Explained"){
    est <- boot$base$Explained
    se <- boot$se_agg["Explained"]
    share <- boot$base$Share_Explained
  } else {
    est <- boot$base$Coefficients
    se <- boot$se_agg["Coefficients"]
    share <- boot$base$Share_Coefficients
  }
  
  paste0(
    star_cell(est, se),
    " (",
    sprintf("%.1f", 100 * share),
    "\\%)"
  )
}


make_benchmark_comparison <- function(
    pooled_boot,
    male_boot,
    female_boot,
    equal_boot) {
  
  data.frame(
    Component = c(
      "Total gap",
      "Endowment effect",
      "Coefficient effect"
    ),
    
    Pooled = c(
      fmt3(pooled_boot$base$Total_gap),
      benchmark_effect_cell(pooled_boot, "Explained"),
      benchmark_effect_cell(pooled_boot, "Coefficients")
    ),
    
    MHH = c(
      fmt3(male_boot$base$Total_gap),
      benchmark_effect_cell(male_boot, "Explained"),
      benchmark_effect_cell(male_boot, "Coefficients")
    ),
    
    FHH = c(
      fmt3(female_boot$base$Total_gap),
      benchmark_effect_cell(female_boot, "Explained"),
      benchmark_effect_cell(female_boot, "Coefficients")
    ),
    
    Equal_weight = c(
      fmt3(equal_boot$base$Total_gap),
      benchmark_effect_cell(equal_boot, "Explained"),
      benchmark_effect_cell(equal_boot, "Coefficients")
    ),
    
    stringsAsFactors = FALSE
  )
}


tab_benchmark_prod <- make_benchmark_comparison(
  boot_prod_fe,
  boot_prod_bm_male,
  boot_prod_bm_female,
  boot_prod_bm_equal
)

tab_benchmark_inc <- make_benchmark_comparison(
  boot_inc_fe,
  boot_inc_bm_male,
  boot_inc_bm_female,
  boot_inc_bm_equal
)


latex_rows_benchmark_prod <- make_latex_rows(
  tab_benchmark_prod,
  c("Component", "Pooled", "MHH", "FHH", "Equal_weight")
)

latex_rows_benchmark_inc <- make_latex_rows(
  tab_benchmark_inc,
  c("Component", "Pooled", "MHH", "FHH", "Equal_weight")
)


tab_benchmark_prod <<- tab_benchmark_prod
tab_benchmark_inc <<- tab_benchmark_inc

latex_rows_benchmark_prod <<- latex_rows_benchmark_prod
latex_rows_benchmark_inc <<- latex_rows_benchmark_inc


print(tab_benchmark_prod)
print(tab_benchmark_inc)
