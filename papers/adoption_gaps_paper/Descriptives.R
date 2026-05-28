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



# SAFE skewness check it in the future

baseline_farmers$yield_per_acre_ihs <- asinh(baseline_farmers$yield_per_acre)
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

baseline_farmers$maize_income_ihs <- asinh(baseline_farmers$maize_income)
summary(baseline_farmers$maize_income_ihs)
table(baseline_farmers$maize_income_ihs)



# maize income: set to 0 only for confirmed non-sellers; keep NA if sold==1 but missing bags/price
baseline_farmers$maize_income <- NA_real_
baseline_farmers$maize_income[baseline_farmers$maize_sold == 0] <- 0
baseline_farmers$maize_income[baseline_farmers$maize_sold == 1] <-
  baseline_farmers$bags_sold[baseline_farmers$maize_sold == 1] *
  baseline_farmers$bag_price[baseline_farmers$maize_sold == 1]




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
  "maize_income_ihs",
  "base_hybrid"
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

# Vars used in baseline productivity table/model
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
  "farmer_group_member")

# Vars used in baseline income table/model (note: log1p(r_maize_plot_area) uses r_maize_plot_area)
vars_inc <- c(
  "hh_gender_num",
  "log1p(r_maize_plot_area)",
  "education_head_num",
  "household_size",
  "hh_age",
  "quality_seed_used",
  "dap_npk_applied",
  "urea_applied",
  "chemicals_applied",
  "organic_manure_applied",
  "num_shops",
  "distance_agroshops",
  "weed_times",
  "resow",
  "farmer_group_member",
  "yield_per_acre"
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
  "education_head_num"     = "Household head finished primary education (1=yes)",
  "household_size"         = "Number of household members",
  "hh_age"                 = "Household head’s age in years",
  "distance_agroshops"     = "Homestead’s distance to nearest agro-input shop in km",
  "num_shops"              = "Number of agro-input shops in village or neighborhood",
  "quality_seed_used"      = "Farmer used quality seed (1=yes)",
  "dap_npk_applied"        = "Farmer applied DAP/NPK on the randomly selected plot (1=yes)",
  "urea_applied"           = "Farmer applied Urea on this plot (1=yes)",
  "organic_manure_applied" = "Farmer applied organic manure on this plot (1=yes)",
  "maize_plot_area"        = "Land for crop production in acres",
  "chemicals_applied"      = "Farmer applied agro-chemicals on this plot (1=yes)",
  "weed_times"             = "Number of times the farmer weeded this plot",
  "resow"                  = "Farmer re-sowed seeds on this plot (1=yes)",
  "farmer_group_member"    = "Farmer is member of maize farmer group/cooperative (1=yes)"
)

form_prod_bl <- as.formula(paste("yield_per_acre_ihs ~", paste(vars_prod, collapse = " + ")))

m_prod_raw_bl    <- lm(yield_per_acre_ihs ~ hh_gender_num, data = baseline_farmers)
m_prod_full_bl   <- lm(form_prod_bl, data = baseline_farmers)
m_prod_male_bl   <- lm(form_prod_bl, data = baseline_farmers, subset = (hh_gender_num == 1))
m_prod_female_bl <- lm(form_prod_bl, data = baseline_farmers, subset = (hh_gender_num == 0))

#add + factor(round) to have FE
form_prod_pool <- as.formula(paste("yield_per_acre_ihs ~", paste(vars_prod, collapse = " + "), "+ factor(round)"))


m_prod_pool_total  <- lm(form_prod_pool, data = farmers_long)
m_prod_pool_male   <- lm(form_prod_pool, data = farmers_long, subset = (hh_gender_num == 1))
m_prod_pool_female <- lm(form_prod_pool, data = farmers_long, subset = (hh_gender_num == 0))

N_prod_raw_fmt         <- format(nobs(m_prod_raw_bl), big.mark=",")
N_prod_full_fmt        <- format(nobs(m_prod_full_bl), big.mark=",")
N_prod_male_fmt        <- format(nobs(m_prod_male_bl), big.mark=",")
N_prod_female_fmt      <- format(nobs(m_prod_female_bl), big.mark=",")
N_prod_pool_total_fmt  <- format(nobs(m_prod_pool_total), big.mark=",")
N_prod_pool_male_fmt   <- format(nobs(m_prod_pool_male), big.mark=",")
N_prod_pool_female_fmt <- format(nobs(m_prod_pool_female), big.mark=",")

tab_prod <- data.frame(
  Label=character(), Raw=character(),
  Full=character(), Male=character(), Female=character(),
  Pooled_Total=character(), Pooled_Male=character(), Pooled_Female=character(),
  stringsAsFactors=FALSE
)

for(v in vars_prod){
  
  raw <- c("—","")
  if(v == "hh_gender_num"){
    raw <- cell_coef_se(m_prod_raw_bl, "hh_gender_num", data = baseline_farmers, cluster_id = CLUSTER_ID)
  }
  
  full   <- cell_coef_se(m_prod_full_bl,   v, data = baseline_farmers, cluster_id = CLUSTER_ID)
  male   <- cell_coef_se(m_prod_male_bl,   v, data = baseline_farmers, cluster_id = CLUSTER_ID)
  female <- cell_coef_se(m_prod_female_bl, v, data = baseline_farmers, cluster_id = CLUSTER_ID)
  
  poolT  <- cell_coef_se(m_prod_pool_total,  v, data = farmers_long, cluster_id = CLUSTER_ID)
  poolM  <- cell_coef_se(m_prod_pool_male,   v, data = farmers_long, cluster_id = CLUSTER_ID)
  poolF  <- cell_coef_se(m_prod_pool_female, v, data = farmers_long, cluster_id = CLUSTER_ID)
  
  tab_prod <- rbind(
    tab_prod,
    data.frame(Label=labels_prod[v], Raw=raw[1],
               Full=full[1], Male=male[1], Female=female[1],
               Pooled_Total=poolT[1], Pooled_Male=poolM[1], Pooled_Female=poolF[1]),
    data.frame(Label="", Raw=raw[2],
               Full=full[2], Male=male[2], Female=female[2],
               Pooled_Total=poolT[2], Pooled_Male=poolM[2], Pooled_Female=poolF[2])
  )
}

# =========================
# 3) INCOME: models + table (Baseline + Pooled)
# =========================

lab_inc <- c(
  "hh_gender_num"            = "Household head is male (1=yes)",
  "log1p(r_maize_plot_area)" = "Area of randomly selected plot in acres (log)",
  "education_head_num"       = "Household head finished primary education (1=yes)",
  "household_size"           = "Number of household members",
  "hh_age"                   = "Household head’s age in years",
  "quality_seed_used"        = "Farmer used quality seed (1=yes)",
  "dap_npk_applied"          = "Farmer applied DAP/NPK on this plot (1=yes)",
  "urea_applied"             = "Farmer applied Urea on this plot (1=yes)",
  "chemicals_applied"        = "Farmer applied agro-chemicals on this plot (1=yes)",
  "organic_manure_applied"   = "Farmer applied organic manure on this plot (1=yes)",
  "num_shops"                = "Number of agro-input shops in village or neighborhood",
  "distance_agroshops"       = "Homestead’s distance to nearest agro-input shop in km",
  "weed_times"               = "Number of times the farmer weeded this plot",
  "resow"                    = "Farmer re-sowed seeds on this plot (1=yes)",
  "farmer_group_member"      = "Farmer is member of maize farmer group/cooperative (1=yes)",
  "yield_per_acre"           = "Maize productivity on this plot in kg/acre"
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

#quick checcke: compare with non-sellers model
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
# Twofold + Threefold | Aggregate + Detailed
# Pooled benchmark WITHOUT gender dummy
# Bootstrap SE clustered at catchID level
# LyX-ready tables
# ============================================================

library(dplyr)

# ============================================================
# 0. SETTINGS
# ============================================================

y_prod       <- "yield_per_acre_ihs"
y_inc        <- "maize_income_ihs"
group_var    <- "hh_gender_num"
male_value   <- 1
female_value <- 0

R_boot    <- 300
seed_boot <- 123

# Important: make sure income transformation is updated
baseline_farmers$maize_income_ihs <- asinh(baseline_farmers$maize_income)

# Log plot area for income decomposition
baseline_farmers$log_maize_plot_area <- log1p(baseline_farmers$maize_plot_area)

# Productivity variables
x_prod <- c(
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
  "farmer_group_member",
  "base_hybrid"
)

# Income variables
# Main version: without yield_per_acre, because productivity is an intermediate mechanism
x_inc <- c(
  "log_maize_plot_area",
  "education_head_num",
  "household_size",
  "hh_age",
  "quality_seed_used",
  "dap_npk_applied",
  "urea_applied",
  "chemicals_applied",
  "organic_manure_applied",
  "num_shops",
  "distance_agroshops",
  "weed_times",
  "resow",
  "farmer_group_member",
  "base_hybrid"
)

# Variable labels
labels_prod <- c(
  "education_head_num"     = "HH head finished primary education (1=yes)",
  "household_size"         = "Number of HH members",
  "hh_age"                 = "HH head age in years",
  "distance_agroshops"     = "Distance to nearest agro-input shop (km)",
  "num_shops"              = "Number of agro-input shops in village",
  "quality_seed_used"      = "Used quality seed (1=yes)",
  "dap_npk_applied"        = "Applied DAP/NPK (1=yes)",
  "urea_applied"           = "Applied Urea (1=yes)",
  "organic_manure_applied" = "Applied organic manure (1=yes)",
  "maize_plot_area"        = "Land for crop production (acres)",
  "chemicals_applied"      = "Applied agro-chemicals (1=yes)",
  "weed_times"             = "Number of times weeded",
  "resow"                  = "Re-sowed seeds (1=yes)",
  "farmer_group_member"    = "Member of farmer group/cooperative (1=yes)",
  "base_hybrid"            = "Used hybrid seed (1=yes)"
)

labels_inc <- c(
  "log_maize_plot_area"    = "Plot area (log acres)",
  "education_head_num"     = "HH head finished primary education (1=yes)",
  "household_size"         = "Number of HH members",
  "hh_age"                 = "HH head age in years",
  "quality_seed_used"      = "Used quality seed (1=yes)",
  "dap_npk_applied"        = "Applied DAP/NPK (1=yes)",
  "urea_applied"           = "Applied Urea (1=yes)",
  "chemicals_applied"      = "Applied agro-chemicals (1=yes)",
  "organic_manure_applied" = "Applied organic manure (1=yes)",
  "num_shops"              = "Number of agro-input shops in village",
  "distance_agroshops"     = "Distance to nearest agro-input shop (km)",
  "weed_times"             = "Number of times weeded",
  "resow"                  = "Re-sowed seeds (1=yes)",
  "farmer_group_member"    = "Member of farmer group/cooperative (1=yes)",
  "base_hybrid"            = "Used hybrid seed (1=yes)"
)

# ============================================================
# 1. KEEP ONLY catchID WITH BOTH MALE AND FEMALE HHs
# ============================================================

valid_catch <- baseline_farmers %>%
  group_by(catchID) %>%
  summarise(
    n_male   = sum(hh_gender_num == male_value, na.rm = TRUE),
    n_female = sum(hh_gender_num == female_value, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  filter(n_male > 0 & n_female > 0) %>%
  pull(catchID)

baseline_fe <- baseline_farmers %>%
  filter(catchID %in% valid_catch)

cat("FE sample N:", nrow(baseline_fe), "\n")
cat("Number of catchIDs:", length(valid_catch), "\n")

# ============================================================
# 2. WITHIN TRANSFORMATION
# ============================================================

within_transform <- function(df, vars, group_var = "catchID") {
  df_w <- df
  
  for (v in vars) {
    if (!v %in% names(df_w)) next
    if (!is.numeric(df_w[[v]])) next
    
    group_mean <- ave(
      df_w[[v]],
      df_w[[group_var]],
      FUN = function(z) mean(z, na.rm = TRUE)
    )
    
    overall_mean <- mean(df_w[[v]], na.rm = TRUE)
    
    df_w[[v]] <- df_w[[v]] - group_mean + overall_mean
  }
  
  df_w
}

vars_to_demean <- unique(c(y_prod, y_inc, x_prod, x_inc))
vars_to_demean <- vars_to_demean[vars_to_demean %in% names(baseline_fe)]

baseline_fe_within <- within_transform(
  baseline_fe,
  vars = vars_to_demean,
  group_var = "catchID"
)

# Keep catchID unchanged for bootstrap clustering
baseline_fe_within$catchID <- baseline_fe$catchID

# ============================================================
# 3. FORMATTERS
# ============================================================

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
  
  z <- est / se
  p <- 2 * pnorm(-abs(z))
  
  paste0(fmt3(est), pstars(p))
}

latex_esc <- function(x) {
  gsub("([%#$&_{}])", "\\\\\\1", x)
}

safe_share <- function(a, total) {
  if (is.na(total) || abs(total) < 1e-12) return(NA_real_)
  a / total
}

# ============================================================
# 4. CORE OAXACA FUNCTION
# Pooled benchmark WITHOUT gender dummy
# ============================================================

oaxaca_decomp_nogender <- function(df, y, x_vars,
                                   group_var = "hh_gender_num",
                                   male_val = 1,
                                   female_val = 0) {
  
  vars_need <- c(y, group_var, x_vars)
  miss <- setdiff(vars_need, names(df))
  if (length(miss) > 0) {
    stop("Missing variables: ", paste(miss, collapse = ", "))
  }
  
  keep <- complete.cases(df[, vars_need])
  d <- df[keep, , drop = FALSE]
  
  dm <- d[d[[group_var]] == male_val, , drop = FALSE]
  dfem <- d[d[[group_var]] == female_val, , drop = FALSE]
  
  rhs <- paste(x_vars, collapse = " + ")
  
  # Group-specific coefficients
  beta_m <- coef(lm(as.formula(paste(y, "~", rhs)), data = dm))
  beta_f <- coef(lm(as.formula(paste(y, "~", rhs)), data = dfem))
  
  # Pooled coefficients WITHOUT gender dummy
  beta_p <- coef(lm(as.formula(paste(y, "~", rhs)), data = d))
  
  # Means of covariates
  X_m <- colMeans(model.matrix(as.formula(paste("~", rhs)), data = dm))
  X_f <- colMeans(model.matrix(as.formula(paste("~", rhs)), data = dfem))
  
  # Align all vectors
  all_names <- Reduce(union, list(
    names(beta_m),
    names(beta_f),
    names(beta_p),
    names(X_m),
    names(X_f)
  ))
  
  align <- function(x) {
    out <- setNames(rep(0, length(all_names)), all_names)
    common <- intersect(names(x), all_names)
    out[common] <- x[common]
    out
  }
  
  beta_m <- align(beta_m)
  beta_f <- align(beta_f)
  beta_p <- align(beta_p)
  X_m    <- align(X_m)
  X_f    <- align(X_f)
  
  # Threefold decomposition
  Endow_k        <- (X_m - X_f) * beta_p
  Male_coef_k    <- X_m * (beta_m - beta_p)
  Female_coef_k  <- X_f * (beta_p - beta_f)
  
  # Twofold decomposition
  Explained_k   <- Endow_k
  Coefficients_k <- Male_coef_k + Female_coef_k
  
  Endow       <- sum(Endow_k)
  Male_coef   <- sum(Male_coef_k)
  Female_coef <- sum(Female_coef_k)
  
  Explained    <- sum(Explained_k)
  Coefficients <- sum(Coefficients_k)
  
  Total_gap <- Endow + Male_coef + Female_coef
  
  detailed <- data.frame(
    term            = all_names,
    Endow_k         = Endow_k,
    Male_coef_k     = Male_coef_k,
    Female_coef_k   = Female_coef_k,
    Explained_k     = Explained_k,
    Coefficients_k  = Coefficients_k,
    stringsAsFactors = FALSE
  )
  
  list(
    N          = nrow(d),
    N_male     = nrow(dm),
    N_female   = nrow(dfem),
    Total_gap  = Total_gap,
    
    # Twofold aggregate
    Explained    = Explained,
    Coefficients = Coefficients,
    Share_Explained    = safe_share(Explained, Total_gap),
    Share_Coefficients = safe_share(Coefficients, Total_gap),
    
    # Threefold aggregate
    Endowment     = Endow,
    Male_coef     = Male_coef,
    Female_coef   = Female_coef,
    Share_Endowment   = safe_share(Endow, Total_gap),
    Share_Male_coef   = safe_share(Male_coef, Total_gap),
    Share_Female_coef = safe_share(Female_coef, Total_gap),
    
    detailed = detailed
  )
}

# ============================================================
# 5. BOOTSTRAP CLUSTERED AT catchID LEVEL
# ============================================================

bootstrap_oaxaca_fe <- function(df, y, x_vars,
                                group_var = "hh_gender_num",
                                male_val = 1,
                                female_val = 0,
                                cluster = "catchID",
                                R = 300,
                                seed = 123) {
  
  set.seed(seed)
  
  vars_need <- c(y, group_var, x_vars, cluster)
  keep <- complete.cases(df[, vars_need])
  d <- df[keep, , drop = FALSE]
  
  base <- oaxaca_decomp_nogender(
    d, y, x_vars,
    group_var = group_var,
    male_val = male_val,
    female_val = female_val
  )
  
  terms <- base$detailed$term
  
  clusters <- unique(d[[cluster]])
  n_cl <- length(clusters)
  
  agg_draws <- matrix(
    NA_real_,
    nrow = R,
    ncol = 5,
    dimnames = list(
      NULL,
      c("Explained", "Coefficients", "Endowment", "Male_coef", "Female_coef")
    )
  )
  
  det_Explained   <- matrix(NA_real_, R, length(terms), dimnames = list(NULL, terms))
  det_Coefficients <- det_Explained
  det_Endowment   <- det_Explained
  det_Male_coef   <- det_Explained
  det_Female_coef <- det_Explained
  
  for (r in seq_len(R)) {
    
    sampled_clusters <- sample(clusters, n_cl, replace = TRUE)
    
    dd <- do.call(rbind, lapply(seq_along(sampled_clusters), function(i) {
      tmp <- d[d[[cluster]] == sampled_clusters[i], , drop = FALSE]
      tmp$boot_cluster_id <- paste0(sampled_clusters[i], "_", i)
      tmp
    }))
    
    rr <- tryCatch(
      oaxaca_decomp_nogender(
        dd, y, x_vars,
        group_var = group_var,
        male_val = male_val,
        female_val = female_val
      ),
      error = function(e) NULL
    )
    
    if (is.null(rr)) next
    
    agg_draws[r, ] <- c(
      rr$Explained,
      rr$Coefficients,
      rr$Endowment,
      rr$Male_coef,
      rr$Female_coef
    )
    
    det <- rr$detailed
    det <- det[match(terms, det$term), ]
    
    det_Explained[r, ]    <- det$Explained_k
    det_Coefficients[r, ] <- det$Coefficients_k
    det_Endowment[r, ]    <- det$Endow_k
    det_Male_coef[r, ]    <- det$Male_coef_k
    det_Female_coef[r, ]  <- det$Female_coef_k
  }
  
  list(
    base = base,
    se_agg = apply(agg_draws, 2, sd, na.rm = TRUE),
    se_det = list(
      Explained    = apply(det_Explained, 2, sd, na.rm = TRUE),
      Coefficients = apply(det_Coefficients, 2, sd, na.rm = TRUE),
      Endowment    = apply(det_Endowment, 2, sd, na.rm = TRUE),
      Male_coef    = apply(det_Male_coef, 2, sd, na.rm = TRUE),
      Female_coef  = apply(det_Female_coef, 2, sd, na.rm = TRUE)
    )
  )
}

# ============================================================
# 6. RUN DECOMPOSITIONS WITH FIXED EFFECTS
# ============================================================

boot_prod_fe <- bootstrap_oaxaca_fe(
  baseline_fe_within,
  y = y_prod,
  x_vars = x_prod,
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_boot,
  seed = seed_boot
)

boot_inc_fe <- bootstrap_oaxaca_fe(
  baseline_fe_within,
  y = y_inc,
  x_vars = x_inc,
  group_var = group_var,
  male_val = male_value,
  female_val = female_value,
  cluster = "catchID",
  R = R_boot,
  seed = seed_boot
)

# ============================================================
# 7. TABLE 1: TWOFOLD AGGREGATE WITH FIXED EFFECTS
# ============================================================

make_twofold_agg_fe <- function(boot_prod, boot_inc) {
  
  bp <- boot_prod$base
  bi <- boot_inc$base
  sp <- boot_prod$se_agg
  si <- boot_inc$se_agg
  
  data.frame(
    Outcome = c(
      "Maize productivity (IHS)",
      "Share of total gap (%)",
      "Maize income (IHS)",
      "Share of total gap (%)"
    ),
    N = c(
      as.character(bp$N),
      "",
      as.character(bi$N),
      ""
    ),
    Total_gap = c(
      fmt3(bp$Total_gap),
      "",
      fmt3(bi$Total_gap),
      ""
    ),
    Explained = c(
      star_cell(bp$Explained, sp["Explained"]),
      fmt3(bp$Share_Explained * 100),
      star_cell(bi$Explained, si["Explained"]),
      fmt3(bi$Share_Explained * 100)
    ),
    Coefficients = c(
      star_cell(bp$Coefficients, sp["Coefficients"]),
      fmt3(bp$Share_Coefficients * 100),
      star_cell(bi$Coefficients, si["Coefficients"]),
      fmt3(bi$Share_Coefficients * 100)
    ),
    stringsAsFactors = FALSE
  )
}

tab_fe_twofold_agg <- make_twofold_agg_fe(
  boot_prod_fe,
  boot_inc_fe
)

# ============================================================
# 8. TABLE 2: THREEFOLD AGGREGATE WITH FIXED EFFECTS
# ============================================================

make_threefold_agg_fe <- function(boot_prod, boot_inc) {
  
  bp <- boot_prod$base
  bi <- boot_inc$base
  sp <- boot_prod$se_agg
  si <- boot_inc$se_agg
  
  data.frame(
    Outcome = c(
      "Maize productivity (IHS)",
      "Share of total gap (%)",
      "Maize income (IHS)",
      "Share of total gap (%)"
    ),
    N = c(
      as.character(bp$N),
      "",
      as.character(bi$N),
      ""
    ),
    Total_gap = c(
      fmt3(bp$Total_gap),
      "",
      fmt3(bi$Total_gap),
      ""
    ),
    Endowment = c(
      star_cell(bp$Endowment, sp["Endowment"]),
      fmt3(bp$Share_Endowment * 100),
      star_cell(bi$Endowment, si["Endowment"]),
      fmt3(bi$Share_Endowment * 100)
    ),
    Male_coefficients = c(
      star_cell(bp$Male_coef, sp["Male_coef"]),
      fmt3(bp$Share_Male_coef * 100),
      star_cell(bi$Male_coef, si["Male_coef"]),
      fmt3(bi$Share_Male_coef * 100)
    ),
    Female_coefficients = c(
      star_cell(bp$Female_coef, sp["Female_coef"]),
      fmt3(bp$Share_Female_coef * 100),
      star_cell(bi$Female_coef, si["Female_coef"]),
      fmt3(bi$Share_Female_coef * 100)
    ),
    stringsAsFactors = FALSE
  )
}

tab_fe_threefold_agg <- make_threefold_agg_fe(
  boot_prod_fe,
  boot_inc_fe
)

# ============================================================
# 9. TABLE 3: TWOFOLD DETAILED WITH FIXED EFFECTS
# ============================================================

make_twofold_det_fe <- function(boot_obj, labels_map, outcome_label) {
  
  det <- boot_obj$base$detailed
  det <- det[det$term != "(Intercept)", , drop = FALSE]
  
  se <- boot_obj$se_det
  
  det$Variable <- ifelse(
    det$term %in% names(labels_map),
    labels_map[det$term],
    det$term
  )
  
  data.frame(
    Outcome = outcome_label,
    Variable = latex_esc(det$Variable),
    Explained = mapply(
      star_cell,
      det$Explained_k,
      se$Explained[det$term]
    ),
    Coefficients = mapply(
      star_cell,
      det$Coefficients_k,
      se$Coefficients[det$term]
    ),
    stringsAsFactors = FALSE
  )
}

tab_fe_twofold_det <- bind_rows(
  make_twofold_det_fe(
    boot_prod_fe,
    labels_prod,
    "Maize productivity (IHS)"
  ),
  make_twofold_det_fe(
    boot_inc_fe,
    labels_inc,
    "Maize income (IHS)"
  )
)

# ============================================================
# 10. TABLE 4: THREEFOLD DETAILED WITH FIXED EFFECTS
# ============================================================

make_threefold_det_fe <- function(boot_obj, labels_map, outcome_label) {
  
  det <- boot_obj$base$detailed
  det <- det[det$term != "(Intercept)", , drop = FALSE]
  
  se <- boot_obj$se_det
  
  det$Variable <- ifelse(
    det$term %in% names(labels_map),
    labels_map[det$term],
    det$term
  )
  
  data.frame(
    Outcome = outcome_label,
    Variable = latex_esc(det$Variable),
    Endowment = mapply(
      star_cell,
      det$Endow_k,
      se$Endowment[det$term]
    ),
    Male_coefficients = mapply(
      star_cell,
      det$Male_coef_k,
      se$Male_coef[det$term]
    ),
    Female_coefficients = mapply(
      star_cell,
      det$Female_coef_k,
      se$Female_coef[det$term]
    ),
    stringsAsFactors = FALSE
  )
}

tab_fe_threefold_det <- bind_rows(
  make_threefold_det_fe(
    boot_prod_fe,
    labels_prod,
    "Maize productivity (IHS)"
  ),
  make_threefold_det_fe(
    boot_inc_fe,
    labels_inc,
    "Maize income (IHS)"
  )
)

# ============================================================
# 11. EXPORT OBJECTS FOR LYX
# ============================================================

tab_fe_twofold_agg   <<- tab_fe_twofold_agg
tab_fe_threefold_agg <<- tab_fe_threefold_agg
tab_fe_twofold_det   <<- tab_fe_twofold_det
tab_fe_threefold_det <<- tab_fe_threefold_det

boot_prod_fe <<- boot_prod_fe
boot_inc_fe  <<- boot_inc_fe

cat("\nObjects exported for LyX:\n")
cat("1. tab_fe_twofold_agg\n")
cat("2. tab_fe_threefold_agg\n")
cat("3. tab_fe_twofold_det\n")
cat("4. tab_fe_threefold_det\n")

# Optional checks
print(tab_fe_twofold_agg)
print(tab_fe_threefold_agg)
head(tab_fe_twofold_det)
head(tab_fe_threefold_det)


tab_fe_twofold_det <- bind_rows(
  make_twofold_det_fe(
    boot_prod_fe,
    labels_prod,
    "Maize productivity (IHS)"
  ),
  make_twofold_det_fe(
    boot_inc_fe,
    labels_inc,
    "Maize income (IHS)"
  )
)


tab_fe_threefold_det <- bind_rows(
  make_threefold_det_fe(
    boot_prod_fe,
    labels_prod,
    "Maize productivity (IHS)"
  ),
  make_threefold_det_fe(
    boot_inc_fe,
    labels_inc,
    "Maize income (IHS)"
  )
)

#twofold and threefold DETAILED prod and income

tab_fe_twofold_det_prod <- subset(
  tab_fe_twofold_det,
  Outcome == "Maize productivity (IHS)"
)

tab_fe_twofold_det_inc <- subset(
  tab_fe_twofold_det,
  Outcome == "Maize income (IHS)"
)

tab_fe_threefold_det_prod <- subset(
  tab_fe_threefold_det,
  Outcome == "Maize productivity (IHS)"
)

tab_fe_threefold_det_inc <- subset(
  tab_fe_threefold_det,
  Outcome == "Maize income (IHS)"
)

make_latex_rows_twofold_simple <- function(tab) {
  rows <- apply(tab, 1, function(x) {
    paste0(
      x["Variable"], " & ",
      x["Explained"], " & ",
      x["Coefficients"], " \\\\"
    )
  })
  paste(rows, collapse = "\n")
}

latex_rows_twofold_prod <- make_latex_rows_twofold_simple(tab_fe_twofold_det_prod)
latex_rows_twofold_inc  <- make_latex_rows_twofold_simple(tab_fe_twofold_det_inc)

make_latex_rows_threefold_simple <- function(tab) {
  rows <- apply(tab, 1, function(x) {
    paste0(
      x["Variable"], " & ",
      x["Endowment"], " & ",
      x["Male_coefficients"], " & ",
      x["Female_coefficients"], " \\\\"
    )
  })
  paste(rows, collapse = "\n")
}

latex_rows_threefold_prod <- make_latex_rows_threefold_simple(tab_fe_threefold_det_prod)
latex_rows_threefold_inc  <- make_latex_rows_threefold_simple(tab_fe_threefold_det_inc)






