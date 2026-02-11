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
#DIVIDED per gender Q15
# hh_gender_num <- trimws(tolower(as.character(baseline_farmers$Check2.check.maize.q15)))  # rimuove spazi e mette in minuscolo
# 
# baseline_farmers$gender <- ifelse(
#   hh_gender_num %in% c("1", "male", "m"), "male",
#   ifelse(hh_gender_num %in% c("0", "female", "f"), "female", NA)
# )
# 
# table(baseline_farmers$gender, useNA = "ifany")



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


#table OLS 
#check for adoption
#hybrid
baseline_farmers$end_Check2.check.maize.q31 <- baseline_farmers$Check2.check.maize.q31
baseline_farmers$end_hybrid<-((baseline_farmers$end_Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$end_Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$end_Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$end_Check2.check.maize.q31=="Bazooka")|(baseline_farmers$end_Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$end_Check2.check.maize.q31=="Panner")|(baseline_farmers$end_Check2.check.maize.q31=="Wema")|(baseline_farmers$end_Check2.check.maize.q31=="KH_series"))
baseline_farmers$end_hybrid<-ifelse(baseline_farmers$end_hybrid=="TRUE",1,0)
baseline_farmers$end_hybrid[baseline_farmers$end_Check2.check.maize.q31=="Other_hybrid"] <- NA #because =Other hybrid or OPV


#PRODUCTIVITY TABLE with robust SE
library(dplyr)
library(lmtest)
library(sandwich)

options(scipen = 999)

# =========================
# SETTINGS
# =========================
CLUSTER_ID <- "farmer_ID"   # change to household_ID if you have it

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
  "farmer_group_member"      = "The respondent is part of a farmer group or cooperative (1 = Yes)",
  "yield_per_acre"           = "Yield per acre"
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

tab_inc <- data.frame(
  Label=character(), Raw=character(),
  Full=character(), Male=character(), Female=character(),
  Pooled_Total=character(), Pooled_Male=character(), Pooled_Female=character(),
  stringsAsFactors=FALSE
)

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


#OAXACA BLINDER #no refErence( Jann 2008)

# --- Packages ---
# (oaxaca non è necessario per questa versione simmetrica;
#  usiamo lm + bootstrap per essere 100% in controllo)
# =========================

# ============================================
# THREEFOLD OAXACA (POOLED / NO REFERENCE) + DETAILED + STARS (AGG + DET)
# OUTPUT objects for LyX:
#   - tab_agg_kilic_star      (4 rows, aggregate Kilic style, stars on coef rows)
#   - tab_det_prod_star       (all vars, stars)
#   - tab_det_inc_star        (all vars, stars)
# ============================================

# ---------- SETTINGS ----------
group_var   <- "hh_gender_num"
male_value  <- 1
female_value <- 0

y_prod <- "yield_per_acre_ihs"
y_inc  <- "maize_income_ihs"

# income: log plot area
baseline_farmers$log_maize_plot_area <- log1p(baseline_farmers$maize_plot_area)

# ---------- FORMATTERS ----------
fmt3_safe <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", format(round(x, 3), nsmall = 3, trim = TRUE))
}

latex_escape <- function(x) {
  gsub("([%#$&_{}])", "\\\\\\1", x)
}

pstars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", ""))))
}

stars_from_est_se <- function(est, se) {
  est <- suppressWarnings(as.numeric(est))
  se  <- suppressWarnings(as.numeric(se))
  z <- est / se
  p <- 2 * pnorm(-abs(z))
  paste0(fmt3_safe(est), pstars(p))
}

# ---------- HELPERS ----------
.get_rhs <- function(x_vars) paste(x_vars, collapse = " + ")

.get_Xbar <- function(df, rhs) {
  X <- model.matrix(as.formula(paste("~", rhs)), data = df)
  colMeans(X)
}

.fit_beta <- function(df, y, rhs) {
  coef(lm(as.formula(paste(y, "~", rhs)), data = df))
}

.align_named <- function(x, all_names) {
  out <- setNames(rep(0, length(all_names)), all_names)
  out[names(x)] <- x
  out
}

# ---------- CORE: threefold pooled returning detailed ----------
threefold_pooled_detailed <- function(df, y, group_var, x_vars, male_value, female_value) {
  
  keep <- complete.cases(df[, c(y, group_var, x_vars)])
  d <- df[keep, , drop = FALSE]
  
  dm   <- d[d[[group_var]] == male_value, , drop = FALSE]
  dfem <- d[d[[group_var]] == female_value, , drop = FALSE]
  
  rhs <- .get_rhs(x_vars)
  
  Xbar_m <- .get_Xbar(dm, rhs)
  Xbar_f <- .get_Xbar(dfem, rhs)
  
  beta_m <- .fit_beta(dm, y, rhs)
  beta_f <- .fit_beta(dfem, y, rhs)
  beta_p <- .fit_beta(d,  y, rhs)   # pooled
  
  all_names <- union(names(beta_p), union(names(beta_m), names(beta_f)))
  beta_m <- .align_named(beta_m, all_names)
  beta_f <- .align_named(beta_f, all_names)
  beta_p <- .align_named(beta_p, all_names)
  Xbar_m <- Xbar_m[all_names]
  Xbar_f <- Xbar_f[all_names]
  
  Endow_k <- (Xbar_m - Xbar_f) * beta_p
  Male_k  <- Xbar_m * (beta_m - beta_p)
  Fem_k   <- Xbar_f * (beta_p - beta_f)
  
  Endow <- sum(Endow_k)
  Male  <- sum(Male_k)
  Fem   <- sum(Fem_k)
  Total <- Endow + Male + Fem
  
  share <- function(a, tot) if (abs(tot) < 1e-12) NA_real_ else a / tot
  
  list(
    N = nrow(d),
    Total_gap = Total,
    Endowment = Endow,
    Male_adv  = Male,
    Female_dis = Fem,
    Share_Endow = share(Endow, Total),
    Share_Male  = share(Male, Total),
    Share_Fem   = share(Fem, Total),
    detailed = data.frame(
      term = all_names,
      Endowment  = Endow_k,
      Male_adv   = Male_k,
      Female_dis = Fem_k,
      stringsAsFactors = FALSE
    ))}


# VARIABLES (define once)

vars_OB_prod <- c(
  "education_head_num"     = "Household-head finished primary education (1 = Yes)",
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
  "farmer_group_member"    = "The respondent is part of a farmer group or cooperative (1 = Yes)",
  "end_hybrid"             = "The respondent adopted hybrid seed in the randomly selected plot (1=yes)"
)

vars_OB_inc <- c(
  "log_maize_plot_area"      = "Area of the randomly selected plot in acres (log)",
  "education_head_num"       = "Household-head finished primary education (1 = Yes)",
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
  "farmer_group_member"      = "The respondent is part of a farmer group or cooperative (1 = Yes)",
  "yield_per_acre"           = "Yield per acre",
  "end_hybrid"               = "The respondent adopted hybrid seed in the randomly selected plot (1=yes)"
)

# ============================================
# RUN point estimates
# ============================================
x_prod <- names(vars_OB_prod)
x_inc  <- names(vars_OB_inc)

res_prod <- threefold_pooled_detailed(baseline_farmers, y_prod, group_var, x_prod, male_value, female_value)
res_inc  <- threefold_pooled_detailed(baseline_farmers, y_inc,  group_var, x_inc,  male_value, female_value)

# ============================================
# BOOTSTRAP SE: aggregate + detailed
# ============================================
bootstrap_se_all <- function(df, y, group_var, x_vars, male_value, female_value, R = 300, seed = 123) {
  set.seed(seed)
  
  keep <- complete.cases(df[, c(y, group_var, x_vars)])
  d <- df[keep, , drop = FALSE]
  
  dm   <- d[d[[group_var]] == male_value, , drop = FALSE]
  dfem <- d[d[[group_var]] == female_value, , drop = FALSE]
  
  nm <- nrow(dm); nf <- nrow(dfem)
  
  base <- threefold_pooled_detailed(d, y, group_var, x_vars, male_value, female_value)
  terms <- base$detailed$term
  
  agg_draws <- matrix(NA_real_, nrow = R, ncol = 3)
  colnames(agg_draws) <- c("Endowment","Male_adv","Female_dis")
  
  detE <- matrix(NA_real_, nrow = R, ncol = length(terms), dimnames = list(NULL, terms))
  detM <- detE
  detF <- detE
  
  for (r in seq_len(R)) {
    sm <- dm[sample.int(nm, nm, replace = TRUE), , drop = FALSE]
    sf <- dfem[sample.int(nf, nf, replace = TRUE), , drop = FALSE]
    dd <- rbind(sm, sf)
    
    rr <- threefold_pooled_detailed(dd, y, group_var, x_vars, male_value, female_value)
    
    agg_draws[r, ] <- c(rr$Endowment, rr$Male_adv, rr$Female_dis)
    
    det <- rr$detailed
    det <- det[match(terms, det$term), ]
    detE[r, ] <- det$Endowment
    detM[r, ] <- det$Male_adv
    detF[r, ] <- det$Female_dis
  }
  
  list(
    base = base,
    se_agg = apply(agg_draws, 2, sd, na.rm = TRUE),
    se_det = list(
      Endowment  = apply(detE, 2, sd, na.rm = TRUE),
      Male_adv   = apply(detM, 2, sd, na.rm = TRUE),
      Female_dis = apply(detF, 2, sd, na.rm = TRUE)
    )
  )
}

boot_prod <- bootstrap_se_all(baseline_farmers, y_prod, group_var, x_prod, male_value, female_value, R = 300, seed = 123)
boot_inc  <- bootstrap_se_all(baseline_farmers, y_inc,  group_var, x_inc,  male_value, female_value, R = 300, seed = 123)

# ============================================
# AGGREGATE TABLE (Kilic style) WITH STARS
# ============================================
tab_agg_kilic_star <- data.frame(
  Outcome = c("Maize productivity (IHS)", "Share of gender differential",
              "Maize income (IHS)",       "Share of gender differential"),
  N = c(as.character(boot_prod$base$N), "", as.character(boot_inc$base$N), ""),
  Total_gap = c(fmt3_safe(boot_prod$base$Total_gap), "", fmt3_safe(boot_inc$base$Total_gap), ""),
  
  Endowment = c(
    stars_from_est_se(boot_prod$base$Endowment, boot_prod$se_agg["Endowment"]),
    fmt3_safe(boot_prod$base$Share_Endow * 100),
    stars_from_est_se(boot_inc$base$Endowment,  boot_inc$se_agg["Endowment"]),
    fmt3_safe(boot_inc$base$Share_Endow  * 100)
  ),
  Male_adv = c(
    stars_from_est_se(boot_prod$base$Male_adv, boot_prod$se_agg["Male_adv"]),
    fmt3_safe(boot_prod$base$Share_Male * 100),
    stars_from_est_se(boot_inc$base$Male_adv,  boot_inc$se_agg["Male_adv"]),
    fmt3_safe(boot_inc$base$Share_Male  * 100)
  ),
  Female_dis = c(
    stars_from_est_se(boot_prod$base$Female_dis, boot_prod$se_agg["Female_dis"]),
    fmt3_safe(boot_prod$base$Share_Fem * 100),
    stars_from_est_se(boot_inc$base$Female_dis,  boot_inc$se_agg["Female_dis"]),
    fmt3_safe(boot_inc$base$Share_Fem  * 100)
  ),
  stringsAsFactors = FALSE
)

# LyX safety
for (cc in names(tab_agg_kilic_star)) tab_agg_kilic_star[[cc]] <- as.character(tab_agg_kilic_star[[cc]])
tab_agg_kilic_star[is.na(tab_agg_kilic_star)] <- ""

# ============================================
# DETAILED TABLES WITH STARS
# ============================================
make_det_star <- function(boot_obj, vars_map) {
  det <- boot_obj$base$detailed
  det <- det[det$term != "(Intercept)", , drop = FALSE]
  
  det$Variable <- ifelse(det$term %in% names(vars_map), vars_map[det$term], det$term)
  det$Variable <- latex_escape(det$Variable)
  
  seE <- boot_obj$se_det$Endowment[det$term]
  seM <- boot_obj$se_det$Male_adv[det$term]
  seF <- boot_obj$se_det$Female_dis[det$term]
  
  data.frame(
    Variable = det$Variable,
    Endowment  = mapply(stars_from_est_se, det$Endowment,  seE),
    Male_adv   = mapply(stars_from_est_se, det$Male_adv,   seM),
    Female_dis = mapply(stars_from_est_se, det$Female_dis, seF),
    stringsAsFactors = FALSE
  )
}

tab_det_prod_star <- make_det_star(boot_prod, vars_OB_prod)
tab_det_inc_star  <- make_det_star(boot_inc,  vars_OB_inc)

# quick check
print(tab_agg_kilic_star)
head(tab_det_prod_star)
head(tab_det_inc_star)



#check for adoption
#hybrid
baseline_farmers$end_Check2.check.maize.q31 <- baseline_farmers$Check2.check.maize.q31
baseline_farmers$end_hybrid<-((baseline_farmers$end_Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$end_Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$end_Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$end_Check2.check.maize.q31=="Bazooka")|(baseline_farmers$end_Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$end_Check2.check.maize.q31=="Panner")|(baseline_farmers$end_Check2.check.maize.q31=="Wema")|(baseline_farmers$end_Check2.check.maize.q31=="KH_series"))
baseline_farmers$end_hybrid<-ifelse(baseline_farmers$end_hybrid=="TRUE",1,0)
baseline_farmers$end_hybrid[baseline_farmers$end_Check2.check.maize.q31=="Other_hybrid"] <- NA #because =Other hybrid or OPV
#opv
baseline_farmers$end_OPV<-(baseline_farmers$end_Check2.check.maize.q31=="Longe_5")|(baseline_farmers$end_Check2.check.maize.q31=="Longe_4")
baseline_farmers$end_OPV<-ifelse(baseline_farmers$end_OPV=="TRUE",1,0)
baseline_farmers$end_OPV[baseline_farmers$end_Check2.check.maize.q31=="Other_hybrid"] <- NA

#farmer saved seed
baseline_farmers$end_Check2.check.maize.q32 <- baseline_farmers$Check2.check.maize.q32
baseline_farmers$end_farmer_saved_seed<-((baseline_farmers$end_Check2.check.maize.q32=="a")|(baseline_farmers$end_Check2.check.maize.q32=="b"))
baseline_farmers$end_farmer_saved_seed<-ifelse(baseline_farmers$end_farmer_saved_seed=="TRUE",1,0)

#6. farmer bought from agro input shop (didn't save)
baseline_farmers$end_Bought_from_agro_input_shop<-ifelse(baseline_farmers$end_Check2.check.maize.q32=="d",1,0)

#new
#7. adoption
baseline_farmers$end_hybridbutsaved <- NA
baseline_farmers$end_hybridbutsaved[baseline_farmers$end_hybrid == 1 & baseline_farmers$end_farmer_saved_seed == 1] <- 1
baseline_farmers$end_hybridbutsaved[baseline_farmers$end_hybrid == 1 & baseline_farmers$end_farmer_saved_seed == 0] <- 0
baseline_farmers$end_hybridbutsaved[baseline_farmers$end_hybrid == 0] <- 0

baseline_farmers$end_OPVbutsaved <- NA
baseline_farmers$end_OPVbutsaved[baseline_farmers$end_OPV == 1 & baseline_farmers$end_farmer_saved_seed == 1] <- 1
baseline_farmers$end_OPVbutsaved[baseline_farmers$end_OPV == 1 & baseline_farmers$end_farmer_saved_seed == 0] <- 0
baseline_farmers$end_OPVbutsaved[baseline_farmers$end_OPV == 0] <- 0

#Check2.check.maize.q34 - How often 
baseline_farmers$end_Check2.check.maize.q34 <- baseline_farmers$Check2.check.maize.q34
baseline_farmers$end_fourthormore_timeused<-((baseline_farmers$end_Check2.check.maize.q34=="d")|(baseline_farmers$end_Check2.check.maize.q34=="e")|(baseline_farmers$end_Check2.check.maize.q34=="f"))
baseline_farmers$end_fourthormore_timeused<-ifelse(baseline_farmers$end_fourthormore_timeused=="TRUE",1,0)
#end_improved=1 non-improved=0
baseline_farmers$end_OPVbutfourthormore_timeused <- NA
baseline_farmers$end_OPVbutfourthormore_timeused[baseline_farmers$end_OPV==1 & baseline_farmers$end_farmer_saved_seed==1 & baseline_farmers$end_fourthormore_timeused==1] <- 1
baseline_farmers$end_OPVbutfourthormore_timeused[baseline_farmers$end_OPV==1 & baseline_farmers$end_farmer_saved_seed==1 & baseline_farmers$end_fourthormore_timeused==0] <- 0
baseline_farmers$end_OPVbutfourthormore_timeused[baseline_farmers$end_OPV==1 & baseline_farmers$end_farmer_saved_seed==0] <- 0
baseline_farmers$end_OPVbutfourthormore_timeused[baseline_farmers$end_OPV == 0] <- 0



## Define end_adoption_onfield based on end_improved,
# but set adoption to 0 when end_hybridbutsaved == 1
# or when end_OPVbutfourthormore_timeused == 1
baseline_farmers$end_improved<-((baseline_farmers$end_Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$end_Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$end_Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$end_Check2.check.maize.q31=="Bazooka")|(baseline_farmers$end_Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$end_Check2.check.maize.q31=="Panner")|(baseline_farmers$end_Check2.check.maize.q31=="Wema")|(baseline_farmers$end_Check2.check.maize.q31=="KH_series"|baseline_farmers$end_Check2.check.maize.q31=="Longe_5")|(baseline_farmers$end_Check2.check.maize.q31=="Longe_4")|(baseline_farmers$end_Check2.check.maize.q31=="Other_hybrid"))
baseline_farmers$end_improved<-ifelse(baseline_farmers$end_improved=="TRUE",1,0)
baseline_farmers$end_adoption_onfield <- baseline_farmers$end_improved
baseline_farmers$end_adoption_onfield[baseline_farmers$end_hybridbutsaved==1] <- 0
baseline_farmers$end_adoption_onfield[baseline_farmers$end_OPVbutfourthormore_timeused==1] <- 0
#baseline_farmers$end_adoption_onfield[baseline_farmers$end_OPVbutsaved==1] <- 0