rm(list=ls())
path <- getwd()

#install.packages("dplyr")
library(dplyr)

#Farmer data from all rounds 
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

#Dealer data from all rounds 
baseline_dealer <- read.csv(
  "../../data/baseline/dealer/baseline_dealer.csv",
  stringsAsFactors = FALSE
)

midline_dealer <- read.csv(
  "../../data/midline/dealer/midline_dealer.csv",
  stringsAsFactors = FALSE
)

endline_dealer <- read.csv(
  "../../data/endline/dealer/dealer_endline.csv",
  stringsAsFactors = FALSE
)

#Descriptives of services given by agro-input dealers at baseline 
##################################################################

#Q85. When farmers buy seed, do you explain how the seed should be used (seed spacing, seed rate, complementary inputs)
baseline_dealer$seed_adv <-as.numeric(dplyr::recode(
  baseline_dealer$maize.owner.agree.q85,
  "a" = 0,  # Never
  "b" = 0,  # Only if farmers ask
  "c" = 1,  # Yes, always
))
table(baseline_dealer$seed_adv)      #189 never/only if farmers ask; 159 yes always
mean(baseline_dealer$seed_adv, na.rm=T)   #0.4568966

#Q86. When farmers buy seed, do you usually recommend complementary inputs (fertilizer, chemical,…)
baseline_dealer$input_recommend <-as.numeric(dplyr::recode(
  baseline_dealer$maize.owner.agree.q86,
  "a" = 0,  # Never
  "b" = 0,  # Only if farmers ask
  "c" = 1,  # Yes, always
))
table(baseline_dealer$input_recommend)     #164 never/only if farmers ask; 184 yes always
mean(baseline_dealer$input_recommend, na.rm=T)   # 0.5287356

#Q87. Do you offer extension/training to your clients on how to use improved seed varieties?
table(baseline_dealer$maize.owner.agree.q87)     #180 say no, 77 say yes but only to some, 91 say yes, to anyone who wants 
baseline_dealer$training <-as.numeric(dplyr::recode(
  baseline_dealer$maize.owner.agree.q87,
  "1" = 0, 
  "2" = 1, 
  "3" = 1,  
))
table(baseline_dealer$training) 
mean(baseline_dealer$training, na.rm=T)      #0.4827586

#Q88. Did you offer discounts to clients that buy large quantities of maize seed during the second season of 2020?
baseline_dealer$quan_discount <-as.numeric(dplyr::recode(
  baseline_dealer$maize.owner.agree.q88,
  "No" = 0,  
  "Yes" = 1, 
))
table(baseline_dealer$quan_discount)   #87 say no and 261 say yes 
mean(baseline_dealer$ quan_discount, na.rm=T)   0.75

#Q90. Do you repackage seed yourself when clients wanted smaller packages than what the seed comes in?
baseline_dealer$repackage <-as.numeric(dplyr::recode(
  baseline_dealer$maize.owner.agree.q90,
  "a" = 1,  # yes for any seed
  "b" = 1,  # yes but only for selected seed
  "c" = 0,  # no
))
table(baseline_dealer$repackage)   #166 say 0 and 182 say yes 
mean(baseline_dealer$repackage, na.rm=T)   #0.5229885

#Q93. Do you provide seed on credit (pay after harvest)? 
table(baseline_dealer$maize.owner.agree.q93)    #141 say no; 198 say yes but only to some; 9 say yes, to anyone who wants 
baseline_dealer$seed_credit <-as.numeric(dplyr::recode(
  baseline_dealer$maize.owner.agree.q93,
  "1" = 0, 
  "2" = 1, 
  "3" = 1,  
))
table(baseline_dealer$seed_credit)   #141 say 0 and 207 say yes 
mean(baseline_dealer$seed_credit, na.rm=T)   #0.5948276

#Q94. Give an estimate of how many people you gave credit since last season?
table(baseline_dealer$maize.owner.agree.q94)

baseline_dealer$maize.owner.agree.q94[baseline_dealer$maize.owner.agree.q94=="n/a"] <- NA
baseline_dealer$maize.owner.agree.q94[baseline_dealer$maize.owner.agree.q94==999] <- NA
baseline_dealer$credit_nr <-as.numeric(baseline_dealer$maize.owner.agree.q94)

mean(baseline_dealer$credit_nr, na.rm = T)   #10.68342

#Q95. How many of these were women?
table(baseline_dealer$maize.owner.agree.q95)

baseline_dealer$maize.owner.agree.q95[baseline_dealer$maize.owner.agree.q95=="n/a"] <- NA
baseline_dealer$maize.owner.agree.q95[baseline_dealer$maize.owner.agree.q95==999] <- NA
baseline_dealer$credit_nr_fem <-as.numeric(baseline_dealer$maize.owner.agree.q95)

mean(baseline_dealer$credit_nr_fem, na.rm = T)   #3.43