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

#Rating dyads data
#baseline 
baseline_rating <- read.csv(
  "../../data/baseline/farmer/rating_dyads.csv",
  stringsAsFactors = FALSE
)
#midline 
midline_rating <- read.csv(
  "../../data/midline/farmer/midline_dealer_services_dyads.csv",
  stringsAsFactors = FALSE
)
#the variable knows_SA_rating is in a different dataset, loading that in
midline_rating2 <- read.csv(
  "../../data/midline/farmer/midline_rating_dyads.csv",
  stringsAsFactors = FALSE
)
#endline 
endline_rating <- read.csv(
  "../../data/endline/farmer/endline_rating_dyads.csv",
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
mean(baseline_dealer$ quan_discount, na.rm=T)  # 0.75

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


#Descriptives of services farmers are getting at baseline
##################################################################

#Q70. If there is a problem with the seed of  **${calc_biz}**, can you carry the seed back to **${calc_biz}** and get a refund (insurance)?
table(baseline_rating$refunds)    #283 say 98 which means 'dont know'
baseline_rating$refunds[baseline_rating$refunds=="n/a"] <- NA
baseline_rating$refunds[baseline_rating$refunds==98] <- NA
table(baseline_rating$refunds)      #522 say no and 217 say yes 
baseline_rating$refunds_base <-as.numeric(dplyr::recode(    #recoding to get numerical values
  baseline_rating$refunds,
  "No" = 0,  
  "Yes" = 1, 
))
table(baseline_rating$refunds_base)
mean(baseline_rating$refunds_base, na.rm = T)   #0.2936401

#Q71. Does  **${calc_biz}** give credit, i.e. gives you seed (or inputs more in general) that you can pay for later (after harvest)
table(baseline_rating$gives_credit)    #198 say 98 which means 'dont know'
baseline_rating$gives_credit[baseline_rating$gives_credit=="n/a"] <- NA
baseline_rating$gives_credit[baseline_rating$gives_credit==98] <- NA
table(baseline_rating$gives_credit)      #460 say no and 364 say yes 
baseline_rating$gives_credit_base <-as.numeric(dplyr::recode(    #recoding to get numerical values
  baseline_rating$gives_credit,
  "No" = 0,  
  "Yes" = 1, 
))
table(baseline_rating$gives_credit_base)
mean(baseline_rating$gives_credit_base, na.rm = T)   #0.4417476

#Q72. Does  **${calc_biz}** train/advise you on how to use improved seed varieties while buying seed?
table(baseline_rating$gives_advice)    #49 say 98 which means 'dont know'
baseline_rating$gives_advice[baseline_rating$gives_advice=="n/a"] <- NA
baseline_rating$gives_advice[baseline_rating$gives_advice==98] <- NA
table(baseline_rating$gives_advice)      #233 say no and 740 say yes 
baseline_rating$gives_advice_base <-as.numeric(dplyr::recode(    #recoding to get numerical values
  baseline_rating$gives_advice,
  "No" = 0,  
  "Yes" = 1, 
))
table(baseline_rating$gives_advice_base)
mean(baseline_rating$gives_advice_base, na.rm = T)   #0.7605344

#Q73. Does **${calc_biz}** deliver seeds to clients at home? 
table(baseline_rating$delivers)    #178 say 98 which means 'dont know'
baseline_rating$delivers[baseline_rating$delivers=="n/a"] <- NA
baseline_rating$delivers[baseline_rating$delivers==98] <- NA
table(baseline_rating$delivers)      #643 say no and 201 say yes 
baseline_rating$delivers_base <-as.numeric(dplyr::recode(    #recoding to get numerical values
  baseline_rating$delivers,
  "No" = 0,  
  "Yes" = 1, 
))
table(baseline_rating$delivers_base)
mean(baseline_rating$delivers_base, na.rm = T)   #0.2381517

#Q74. Does  **${calc_biz}** offer after-sales service?
table(baseline_rating$after_sales_service)    #128 say 98 which means 'dont know'
baseline_rating$after_sales_service[baseline_rating$after_sales_service=="n/a"] <- NA
baseline_rating$after_sales_service[baseline_rating$after_sales_service==98] <- NA
table(baseline_rating$after_sales_service)      #659 say no and 235 say yes 
baseline_rating$after_sales_service_base <-as.numeric(dplyr::recode(    #recoding to get numerical values
  baseline_rating$after_sales_service,
  "No" = 0,  
  "Yes" = 1, 
))
table(baseline_rating$after_sales_service_base)
mean(baseline_rating$after_sales_service_base, na.rm = T)   #0.2628635

#Q75. Does **${calc_biz}** accept different payment methods?
table(baseline_rating$payment_mehtods)    #290 say 98 which means 'dont know'
baseline_rating$payment_mehtods[baseline_rating$payment_mehtods=="n/a"] <- NA
baseline_rating$payment_mehtods[baseline_rating$payment_mehtods==98] <- NA
table(baseline_rating$payment_mehtods)      #439 say no and 293 say yes 
baseline_rating$payment_methods_base <-as.numeric(dplyr::recode(    #recoding to get numerical values
  baseline_rating$payment_mehtods,
  "No" = 0,  
  "Yes" = 1, 
))
table(baseline_rating$payment_methods_base)
mean(baseline_rating$payment_methods_base, na.rm = T)   #0.4002732

#Q76. Does  **${calc_biz}** sell small quantities if necessary (1kg)?
table(baseline_rating$small_quant)    #44 say 98 which means 'dont know'
baseline_rating$small_quant[baseline_rating$small_quant=="n/a"] <- NA
baseline_rating$small_quant[baseline_rating$small_quant==98] <- NA
table(baseline_rating$small_quant)      #92 say no and 886 say yes 
baseline_rating$small_quant_base <-as.numeric(dplyr::recode(    #recoding to get numerical values
  baseline_rating$small_quant,
  "No" = 0,  
  "Yes" = 1, 
))
table(baseline_rating$small_quant_base)
mean(baseline_rating$small_quant_base, na.rm = T)   #0.9059305

#we need to now get gender of the farming hh head in the same dataset as the services 
table(baseline_farmers$Check2.check.maize.q15)   #gender of farming household head
merged_basefarmer_dyad <- merge(baseline_farmers, baseline_rating, by = "farmer_ID")   #merging the datasets by farmer ID
merged_basefarmer_dyad$farmer_gender <-as.numeric(dplyr::recode(    #recoding to get numerical values for gender of hh head
  merged_basefarmer_dyad$Check2.check.maize.q15,
  "Female" = 0,  
  "Male" = 1, 
))   

desc_base_serv<- rbind(c(mean(baseline_rating$refunds_base, na.rm=T),
                        sd(baseline_rating$refunds_base, na.rm=T),
                        sum(!is.na (baseline_rating$refunds_base))),
            
                      c(mean(baseline_rating$gives_credit_base, na.rm=T),
                        sd(baseline_rating$gives_credit_base, na.rm=T),
                        sum(!is.na (baseline_rating$gives_credit_base))),
                      
                      c(mean(baseline_rating$gives_advice_base, na.rm=T),
                        sd(baseline_rating$gives_advice_base, na.rm=T),
                        sum(!is.na (baseline_rating$gives_advice_base))),
                      
                      c(mean(baseline_rating$delivers_base, na.rm=T),
                        sd(baseline_rating$delivers_base, na.rm=T),
                        sum(!is.na (baseline_rating$delivers_base))),
                      
                      c(mean(baseline_rating$after_sales_service_base, na.rm=T),
                        sd(baseline_rating$after_sales_service_base, na.rm=T),
                        sum(!is.na (baseline_rating$after_sales_service_base))),
                      
                      c(mean(baseline_rating$payment_methods_base, na.rm=T),
                        sd(baseline_rating$payment_methods_base, na.rm=T),
                        sum(!is.na (baseline_rating$payment_methods_base))),
                      
                      c(mean(baseline_rating$small_quant_base, na.rm=T),
                        sd(baseline_rating$small_quant_base, na.rm=T),
                        sum(!is.na (baseline_rating$small_quant_base))))
                      
#number of males vs. females having the services (baseline)

tab_refunds <- table( merged_basefarmer_dyad$farmer_gender, merged_basefarmer_dyad$refunds_base)
addmargins(tab_refunds)   #out of 99 F, 28 say yes; out of 640 M, 189 say yes 
percent_tab_refunds <- round(prop.table(tab_refunds, margin = 1) * 100, 1 ) 
#print (percent_tab_refunds)   #28.3% F vs. 29.5% M say yes 

tab_credit <- table(merged_basefarmer_dyad$farmer_gender, merged_basefarmer_dyad$gives_credit_base)
addmargins(tab_credit)    #out of 110 F, 50 say yes; out of 714 male, 314 say yes 
percent_tab_credit <- round(prop.table(tab_credit, margin = 1) * 100, 1 ) 
#print (percent_tab_credit)   #45.5% F vs. 44% M say yes 

tab_advice <- table(merged_basefarmer_dyad$farmer_gender, merged_basefarmer_dyad$gives_advice_base)
addmargins(tab_advice)    #out of 126 F, 93 say yes; out of 847 male, 647 say yes 
percent_tab_advice <- round(prop.table(tab_advice, margin = 1) * 100, 1 ) 
#print (percent_tab_advice)   #73.8% F vs. 76.4% M say yes 

tab_delivers <- table(merged_basefarmer_dyad$farmer_gender, merged_basefarmer_dyad$delivers_base)
addmargins(tab_delivers)    #out of 113 F, 26 say yes; out of 731 male, 175 say yes 
percent_tab_delivers <- round(prop.table(tab_delivers, margin = 1) * 100, 1 ) 
#print (percent_tab_delivers)   #23% F vs. 23.9% M say yes 

tab_aftersales <- table(merged_basefarmer_dyad$farmer_gender, merged_basefarmer_dyad$after_sales_service_base)
addmargins(tab_aftersales)    #out of 119 F, 30 say yes; out of 775 male, 205 say yes 
percent_tab_aftersales <- round(prop.table(tab_aftersales, margin = 1) * 100, 1 ) 
#print (percent_tab_aftersales)   #25.2% F vs. 26.5% M say yes 

tab_payment_method <- table(merged_basefarmer_dyad$farmer_gender, merged_basefarmer_dyad$payment_methods_base)
addmargins(tab_payment_method)    #out of 86 F, 33 say yes; out of 646 male, 260 say yes 
percent_tab_payment_method <- round(prop.table(tab_payment_method, margin = 1) * 100, 1 ) 
#print (percent_tab_payment_method)   #38.4% F vs. 40.2% M say yes 

tab_smallquant <- table(merged_basefarmer_dyad$farmer_gender, merged_basefarmer_dyad$small_quant_base)
addmargins(tab_smallquant)    #out of 127 F, 113 say yes; out of 851 male, 773 say yes 
percent_tab_smallquant <- round(prop.table(tab_smallquant, margin = 1) * 100, 1 ) 
#print (percent_tab_smallquant)   #89% F vs. 90.8% M say yes 

desc_base_serv_gender<- rbind(c(mean(merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==1])),
                                mean(merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==1])),
                                mean(merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==1])),
                                mean(merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==1])),
                                mean(merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==1])),
                                mean(merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==1])),
                                mean(merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1])),
                                mean(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==0]))))


#now we will only focus on the sample who bought at dealer -- number of males vs. females having the services 
table(merged_basefarmer_dyad$bought_at_dealer)
merged_basefarmer_dyad$bought_at_dealer[merged_basefarmer_dyad$bought_at_dealer=="n/a"] <- NA
merged_basefarmer_dyad$bought_dealer <-as.numeric(dplyr::recode(    #recoding to get numerical values 
  merged_basefarmer_dyad$bought_at_dealer,
  "No" = 0,  
  "Yes" = 1, 
))  

tab_refunds_bought <- table( merged_basefarmer_dyad$farmer_gender[merged_basefarmer_dyad$bought_dealer==1], merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$bought_dealer==1])
addmargins(tab_refunds_bought)   #out of 83 F, 25 say yes; out of 515 M, 154 say yes 
percent_tab_refunds_bought <- round(prop.table(tab_refunds_bought, margin = 1) * 100, 1 ) 
#print (percent_tab_refunds_bought)   #30.1% F vs. 29.9% M say yes 

tab_credit_bought <- table( merged_basefarmer_dyad$farmer_gender[merged_basefarmer_dyad$bought_dealer==1], merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$bought_dealer==1])
addmargins(tab_credit_bought)   #out of 88 F, 43 say yes; out of 569 M, 251 say yes 
percent_tab_credit_bought <- round(prop.table(tab_credit_bought, margin = 1) * 100, 1 ) 
#print (percent_tab_credit_bought)   #48.9% F vs. 44.1% M say yes 

tab_advice_bought <- table(merged_basefarmer_dyad$farmer_gender[merged_basefarmer_dyad$bought_dealer==1], merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$bought_dealer==1])
addmargins(tab_advice_bought)    #out of 105 F, 77 say yes; out of 684 male, 517 say yes 
percent_tab_advice_bought <- round(prop.table(tab_advice_bought, margin = 1) * 100, 1 ) 
#print (percent_tab_advice_bought)   #73.3% F vs. 75.6% M say yes 

tab_delivers_bought <- table(merged_basefarmer_dyad$farmer_gender[merged_basefarmer_dyad$bought_dealer==1], merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$bought_dealer==1])
addmargins(tab_delivers_bought)    #out of 91 F, 22 say yes; out of 581 male, 150 say yes 
percent_tab_delivers_bought <- round(prop.table(tab_delivers_bought, margin = 1) * 100, 1 ) 
#print (percent_tab_delivers_bought)   #24.2% F vs. 25.8% M say yes 

tab_aftersales_bought <- table(merged_basefarmer_dyad$farmer_gender[merged_basefarmer_dyad$bought_dealer==1], merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$bought_dealer==1])
addmargins(tab_aftersales_bought)    #out of 99 F, 27 say yes; out of 624 male, 171 say yes 
percent_tab_aftersales_bought <- round(prop.table(tab_aftersales_bought, margin = 1) * 100, 1 ) 
#print (percent_tab_aftersales_bought)   #27.3% F vs. 27.4% M say yes 

tab_payment_method_bought <- table(merged_basefarmer_dyad$farmer_gender[merged_basefarmer_dyad$bought_dealer==1], merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$bought_dealer==1])
addmargins(tab_payment_method_bought)    #out of 71 F, 28 say yes; out of 503 male, 215 say yes 
percent_tab_payment_method_bought <- round(prop.table(tab_payment_method_bought, margin = 1) * 100, 1 ) 
#print (percent_tab_payment_method_bought)   #39.4% F vs. 42.7% M say yes 

tab_smallquant_bought <- table(merged_basefarmer_dyad$farmer_gender[merged_basefarmer_dyad$bought_dealer==1], merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$bought_dealer==1])
addmargins(tab_smallquant_bought)    #out of 104 F, 93 say yes; out of 679 male, 624 say yes 
percent_tab_smallquant_bought <- round(prop.table(tab_smallquant_bought, margin = 1) * 100, 1 ) 
#print (percent_tab_smallquant_bought)   #89.4% F vs. 91.9% M say yes 

#mean(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T)   #0.9083431
#min(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1] , na.rm=T)   #0
#max(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1] , na.rm=T)    #1
#sd(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1] , na.rm=T)    #0.2887107
#quantile(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T, 0.25)    #1
#quantile(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1], na.rm=T, 0.75)    #1

desc_base_serv_genderbought<- rbind(c(mean(merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1])),
                                mean(merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$refunds_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1]))),
                              
                              c(mean(merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1])),
                                mean(merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$gives_credit_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1]))),
                              
                              c(mean(merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1])),
                                mean(merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$gives_advice_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1]))),
                              
                              c(mean(merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1])),
                                mean(merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$delivers_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1]))),
                              
                              c(mean(merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1])),
                                mean(merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$after_sales_service_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1]))),
                              
                              c(mean(merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1])),
                                mean(merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$payment_methods_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1]))),
                              
                              c(mean(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==1 & merged_basefarmer_dyad$bought_dealer==1])),
                                mean(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sd(merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1], na.rm=T),
                                sum(!is.na (merged_basefarmer_dyad$small_quant_base[merged_basefarmer_dyad$farmer_gender==0 & merged_basefarmer_dyad$bought_dealer==1]))))


#Descriptives of services farmers are getting at midline 
##################################################################

#Q70. If there is a problem with the seed of  **${calc_biz}**, can you carry the seed back to **${calc_biz}** and get a refund (insurance)?
table(midline_rating$refunds)    #826 say 98 which means 'dont know'
midline_rating$refunds[midline_rating$refunds=="n/a"] <- NA
midline_rating$refunds[midline_rating$refunds==98] <- NA
table(midline_rating$refunds)      #771 say no and 719 say yes 
midline_rating$refunds_mid <-as.numeric(dplyr::recode(    #recoding to get numerical values
  midline_rating$refunds,
  "No" = 0,  
  "Yes" = 1, 
))
table(midline_rating$refunds_mid)
mean(midline_rating$refunds_mid, na.rm = T)   # 0.4825503

#Q71. Does  **${calc_biz}** give credit, i.e. gives you seed (or inputs more in general) that you can pay for later (after harvest)
table(midline_rating$gives_credit)    #549 say 98 which means 'dont know'
midline_rating$gives_credit[midline_rating$gives_credit=="n/a"] <- NA
midline_rating$gives_credit[midline_rating$gives_credit==98] <- NA
table(midline_rating$gives_credit)      #796 say no and 971 say yes 
midline_rating$gives_credit_mid <-as.numeric(dplyr::recode(    #recoding to get numerical values
  midline_rating$gives_credit,
  "No" = 0,  
  "Yes" = 1, 
))
table(midline_rating$gives_credit_mid)
mean(midline_rating$gives_credit_mid, na.rm = T)   #0.549519

#Q72. Does  **${calc_biz}** train/advise you on how to use improved seed varieties while buying seed?
table(midline_rating$gives_advice)    #132 say 98 which means 'dont know'
midline_rating$gives_advice[midline_rating$gives_advice=="n/a"] <- NA
midline_rating$gives_advice[midline_rating$gives_advice==98] <- NA
table(midline_rating$gives_advice)      #370 say no and 1814 say yes 
midline_rating$gives_advice_mid <-as.numeric(dplyr::recode(    #recoding to get numerical values
  midline_rating$gives_advice,
  "No" = 0,  
  "Yes" = 1, 
))
table(midline_rating$gives_advice_mid)
mean(midline_rating$gives_advice_mid, na.rm = T)   #0.8305861

#Q73. Does **${calc_biz}** deliver seeds to clients at home? 
table(midline_rating$delivers)    #457 say 98 which means 'dont know'
midline_rating$delivers[midline_rating$delivers=="n/a"] <- NA
midline_rating$delivers[midline_rating$delivers==98] <- NA
table(midline_rating$delivers)      #1095 say no and 764 say yes 
midline_rating$delivers_mid <-as.numeric(dplyr::recode(    #recoding to get numerical values
  midline_rating$delivers,
  "No" = 0,  
  "Yes" = 1, 
))
table(midline_rating$delivers_mid)
mean(midline_rating$delivers_mid, na.rm = T)   #0.4109736

#Q74. Does  **${calc_biz}** offer after-sales service?
table(midline_rating$after_sales_service)    #386 say 98 which means 'dont know'
midline_rating$after_sales_service[midline_rating$after_sales_service=="n/a"] <- NA
midline_rating$after_sales_service[midline_rating$after_sales_service==98] <- NA
table(midline_rating$after_sales_service)      #1168 say no and 762 say yes 
midline_rating$after_sales_service_mid <-as.numeric(dplyr::recode(    #recoding to get numerical values
  midline_rating$after_sales_service,
  "No" = 0,  
  "Yes" = 1, 
))
table(midline_rating$after_sales_service_mid)
mean(midline_rating$after_sales_service_mid, na.rm = T)   #0.3948187

#Q75. Does **${calc_biz}** accept different payment methods?
table(midline_rating$payment_mehtods)    #620 say 98 which means 'dont know'
midline_rating$payment_mehtods[midline_rating$payment_mehtods=="n/a"] <- NA
midline_rating$payment_mehtods[midline_rating$payment_mehtods==98] <- NA
table(midline_rating$payment_mehtods)      #675 say no and 1021 say yes 
midline_rating$payment_methods_mid <-as.numeric(dplyr::recode(    #recoding to get numerical values
  midline_rating$payment_mehtods,
  "No" = 0,  
  "Yes" = 1, 
))
table(midline_rating$payment_methods_mid)
mean(midline_rating$payment_methods_mid, na.rm = T)   #0.6020047

#Q76. Does  **${calc_biz}** sell small quantities if necessary (1kg)?
table(midline_rating$small_quant)    #112 say 98 which means 'dont know'
midline_rating$small_quant[midline_rating$small_quant=="n/a"] <- NA
midline_rating$small_quant[midline_rating$small_quant==98] <- NA
table(midline_rating$small_quant)      #179 say no and 2025 say yes 
midline_rating$small_quant_mid <-as.numeric(dplyr::recode(    #recoding to get numerical values
  midline_rating$small_quant,
  "No" = 0,  
  "Yes" = 1, 
))
table(midline_rating$small_quant_mid)
mean(midline_rating$small_quant_mid, na.rm = T)  #0.918784

#we need to now get gender of the farming hh head in the same dataset as the services
baseline_farmer_gender <- baseline_farmers[, c("Check2.check.maize.q15", "farmer_ID")]
merged_midfarmer_dyad <- merge(baseline_farmer_gender, midline_rating, by = "farmer_ID")   #merging the datasets by farmer ID to get the gender of the hh head
merged_midfarmer_dyad$farmer_gender <-as.numeric(dplyr::recode(    #recoding to get numerical values for gender of hh head
  merged_midfarmer_dyad$Check2.check.maize.q15,
  "Female" = 0,  
  "Male" = 1, 
))   

desc_mid_serv<- rbind(c(mean(midline_rating$refunds_mid, na.rm=T),
                         sd(midline_rating$refunds_mid, na.rm=T),
                         sum(!is.na (midline_rating$refunds_mid))),
                       
                       c(mean(midline_rating$gives_credit_mid, na.rm=T),
                         sd(midline_rating$gives_credit_mid, na.rm=T),
                         sum(!is.na (midline_rating$gives_credit_mid))),
                       
                       c(mean(midline_rating$gives_advice_mid, na.rm=T),
                         sd(midline_rating$gives_advice_mid, na.rm=T),
                         sum(!is.na (midline_rating$gives_advice_mid))),
                       
                       c(mean(midline_rating$delivers_mid, na.rm=T),
                         sd(midline_rating$delivers_mid, na.rm=T),
                         sum(!is.na (midline_rating$delivers_mid))),
                       
                       c(mean(midline_rating$after_sales_service_mid, na.rm=T),
                         sd(midline_rating$after_sales_service_mid, na.rm=T),
                         sum(!is.na (midline_rating$after_sales_service_mid))),
                       
                       c(mean(midline_rating$payment_methods_mid, na.rm=T),
                         sd(midline_rating$payment_methods_mid, na.rm=T),
                         sum(!is.na (midline_rating$payment_methods_mid))),
                       
                       c(mean(midline_rating$small_quant_mid, na.rm=T),
                         sd(midline_rating$small_quant_mid, na.rm=T),
                         sum(!is.na (midline_rating$small_quant_mid))))

#number of males vs. females having the services (midline)

tab_refunds_mid <- table( merged_midfarmer_dyad$farmer_gender, merged_midfarmer_dyad$refunds_mid)
addmargins(tab_refunds_mid)   #out of 224 F, 102 say yes; out of 1266 M, 617 say yes 
percent_tab_refunds_mid <- round(prop.table(tab_refunds_mid, margin = 1) * 100, 1 ) 
#print (percent_tab_refunds_mid)   #45.5% F vs. 48.7% M say yes 

tab_credit_mid <- table(merged_midfarmer_dyad$farmer_gender, merged_midfarmer_dyad$gives_credit_mid)
addmargins(tab_credit_mid)    #out of 266 F, 150 say yes; out of 1501 male, 821 say yes 
percent_tab_credit_mid <- round(prop.table(tab_credit_mid, margin = 1) * 100, 1 ) 
#print (percent_tab_credit_mid)   #56.4% F vs. 54.7% M say yes 

tab_advice_mid <- table(merged_midfarmer_dyad$farmer_gender, merged_midfarmer_dyad$gives_advice_mid)
addmargins(tab_advice_mid)    #out of 358 F, 302 say yes; out of 1826 male, 1512 say yes 
percent_tab_advice_mid <- round(prop.table(tab_advice_mid, margin = 1) * 100, 1 ) 
#print (percent_tab_advice_mid)   #84.4% F vs. 82.8% M say yes 

tab_delivers_mid <- table(merged_midfarmer_dyad$farmer_gender, merged_midfarmer_dyad$delivers_mid)
addmargins(tab_delivers_mid)    #out of 294 F, 121 say yes; out of 1565 male, 643 say yes 
percent_tab_delivers_mid <- round(prop.table(tab_delivers_mid, margin = 1) * 100, 1 ) 
#print (percent_tab_delivers_mid)   #41.2% F vs. 41.1% M say yes 

tab_aftersales_mid <- table(merged_midfarmer_dyad$farmer_gender, merged_midfarmer_dyad$after_sales_service_mid)
addmargins(tab_aftersales_mid)    #out of 312 F, 131 say yes; out of 1618 male, 631 say yes 
percent_tab_aftersales_mid <- round(prop.table(tab_aftersales_mid, margin = 1) * 100, 1 ) 
#print (percent_tab_aftersales_mid)   #42% F vs. 39% M say yes 

tab_payment_method_mid <- table(merged_midfarmer_dyad$farmer_gender, merged_midfarmer_dyad$payment_methods_mid)
addmargins(tab_payment_method_mid)    #out of 265 F, 158 say yes; out of 1431 male, 863 say yes 
percent_tab_payment_method_mid <- round(prop.table(tab_payment_method_mid, margin = 1) * 100, 1 ) 
#print (percent_tab_payment_method_mid)   #59.6% F vs. 60.3% M say yes 

tab_smallquant_mid <- table(merged_midfarmer_dyad$farmer_gender, merged_midfarmer_dyad$small_quant_mid)
addmargins(tab_smallquant_mid)    #out of 364 F, 343 say yes; out of 1840 male, 1682 say yes 
percent_tab_smallquant_mid <- round(prop.table(tab_smallquant_mid, margin = 1) * 100, 1 ) 
#print (percent_tab_smallquant_mid)   #94.2% F vs. 91.4% M say yes

desc_mid_serv_gender<- rbind(c(mean(merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==1])),
                                mean(merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==1])),
                                mean(merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==1])),
                                mean(merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==1])),
                                mean(merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==1])),
                                mean(merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==1])),
                                mean(merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==0]))),
                              
                              c(mean(merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sd(merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==1], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==1])),
                                mean(merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sd(merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==0], na.rm=T),
                                sum(!is.na (merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==0]))))

#now we will only focus on the sample who bought at dealer -- number of males vs. females having the services 
table(merged_midfarmer_dyad$bought_at_dealer)
merged_midfarmer_dyad$bought_at_dealer[merged_midfarmer_dyad$bought_at_dealer=="n/a"] <- NA
merged_midfarmer_dyad$bought_dealer <-as.numeric(dplyr::recode(    #recoding to get numerical values 
  merged_midfarmer_dyad$bought_at_dealer,
  "No" = 0,  
  "Yes" = 1, 
))  

tab_refunds_boughtmid <- table( merged_midfarmer_dyad$farmer_gender[merged_midfarmer_dyad$bought_dealer==1], merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$bought_dealer==1])
addmargins(tab_refunds_boughtmid)   #out of 200 F, 93 say yes; out of 1174 M, 572 say yes 
percent_tab_refunds_boughtmid <- round(prop.table(tab_refunds_boughtmid, margin = 1) * 100, 1 ) 
#print (percent_tab_refunds_boughtmid)   #46.5% F vs. 48.7% M say yes 

tab_credit_boughtmid <- table( merged_midfarmer_dyad$farmer_gender[merged_midfarmer_dyad$bought_dealer==1], merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$bought_dealer==1])
addmargins(tab_credit_boughtmid)   #out of 237 F, 136 say yes; out of 1384 M, 758 say yes 
percent_tab_credit_boughtmid <- round(prop.table(tab_credit_boughtmid, margin = 1) * 100, 1 ) 
#print (percent_tab_credit_boughtmid)   #57.4% F vs. 54.8% M say yes 

tab_advice_boughtmid <- table(merged_midfarmer_dyad$farmer_gender[merged_midfarmer_dyad$bought_dealer==1], merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$bought_dealer==1])
addmargins(tab_advice_boughtmid)    #out of 317 F, 263 say yes; out of 1678 male, 1385 say yes 
percent_tab_advice_boughtmid <- round(prop.table(tab_advice_boughtmid, margin = 1) * 100, 1 ) 
#print (percent_tab_advice_boughtmid)   #83% F vs. 82.5% M say yes 

tab_delivers_boughtmid <- table(merged_midfarmer_dyad$farmer_gender[merged_midfarmer_dyad$bought_dealer==1], merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$bought_dealer==1])
addmargins(tab_delivers_boughtmid)    #out of 264 F, 112 say yes; out of 1447 male, 593 say yes 
percent_tab_delivers_boughtmid <- round(prop.table(tab_delivers_boughtmid, margin = 1) * 100, 1 ) 
#print (percent_tab_delivers_boughtmid)   #42.4% F vs. 41% M say yes 

tab_aftersales_boughtmid <- table(merged_midfarmer_dyad$farmer_gender[merged_midfarmer_dyad$bought_dealer==1], merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$bought_dealer==1])
addmargins(tab_aftersales_boughtmid)    #out of 282 F, 120 say yes; out of 1514 male, 591 say yes 
percent_tab_aftersales_boughtmid <- round(prop.table(tab_aftersales_boughtmid, margin = 1) * 100, 1 ) 
#print (percent_tab_aftersales_boughtmid)   #42.6% F vs. 39% M say yes 

tab_payment_method_boughtmid <- table(merged_midfarmer_dyad$farmer_gender[merged_midfarmer_dyad$bought_dealer==1], merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$bought_dealer==1])
addmargins(tab_payment_method_boughtmid)    #out of 243 F, 148 say yes; out of 1320 male, 795 say yes 
percent_tab_payment_method_boughtmid <- round(prop.table(tab_payment_method_boughtmid, margin = 1) * 100, 1 ) 
#print (percent_tab_payment_method_boughtmid)   #60.9% F vs. 60.2% M say yes 

tab_smallquant_boughtmid <- table(merged_midfarmer_dyad$farmer_gender[merged_midfarmer_dyad$bought_dealer==1], merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$bought_dealer==1])
addmargins(tab_smallquant_boughtmid)    #out of 323 F, 303 say yes; out of 1684 male, 1536 say yes 
percent_tab_smallquant_boughtmid <- round(prop.table(tab_smallquant_boughtmid, margin = 1) * 100, 1 ) 
#print (percent_tab_smallquant_boughtmid)   #93.8% F vs. 91.2% M say yes 

desc_mid_serv_genderbought<- rbind(c(mean(merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1])),
                                      mean(merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$refunds_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1]))),
                                    
                                    c(mean(merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1])),
                                      mean(merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$gives_credit_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1]))),
                                    
                                    c(mean(merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1])),
                                      mean(merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$gives_advice_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1]))),
                                    
                                    c(mean(merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1])),
                                      mean(merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$delivers_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1]))),
                                    
                                    c(mean(merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1])),
                                      mean(merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$after_sales_service_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1]))),
                                    
                                    c(mean(merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1])),
                                      mean(merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$payment_methods_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1]))),
                                    
                                    c(mean(merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==1 & merged_midfarmer_dyad$bought_dealer==1])),
                                      mean(merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sd(merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1], na.rm=T),
                                      sum(!is.na (merged_midfarmer_dyad$small_quant_mid[merged_midfarmer_dyad$farmer_gender==0 & merged_midfarmer_dyad$bought_dealer==1]))))

merged_midfarmer_dyadSA <- merge(merged_midfarmer_dyad , midline_rating2,
                               by = c('farmer_ID', 'shop_ID'))   #10872 obs 

desc_mid_serv_genderSA<- rbind(c(mean(merged_midfarmer_dyadSA$refunds_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$refunds_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$refunds_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1])),
                                     mean(merged_midfarmer_dyadSA$refunds_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$refunds_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$refunds_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1]))),
                                   
                                   c(mean(merged_midfarmer_dyadSA$gives_credit_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$gives_credit_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$gives_credit_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1])),
                                     mean(merged_midfarmer_dyadSA$gives_credit_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$gives_credit_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$gives_credit_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1]))),
                                   
                                   c(mean(merged_midfarmer_dyadSA$gives_advice_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$gives_advice_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$gives_advice_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1])),
                                     mean(merged_midfarmer_dyadSA$gives_advice_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$gives_advice_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$gives_advice_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1]))),
                                   
                                   c(mean(merged_midfarmer_dyadSA$delivers_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$delivers_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$delivers_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1])),
                                     mean(merged_midfarmer_dyadSA$delivers_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$delivers_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$delivers_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1]))),
                                   
                                   c(mean(merged_midfarmer_dyadSA$after_sales_service_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$after_sales_service_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$after_sales_service_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1])),
                                     mean(merged_midfarmer_dyadSA$after_sales_service_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$after_sales_service_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$after_sales_service_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1]))),
                                   
                                   c(mean(merged_midfarmer_dyadSA$payment_methods_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$payment_methods_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$payment_methods_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1])),
                                     mean(merged_midfarmer_dyadSA$payment_methods_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$payment_methods_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$payment_methods_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1]))),
                                   
                                   c(mean(merged_midfarmer_dyadSA$small_quant_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$small_quant_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$small_quant_mid[merged_midfarmer_dyadSA$farmer_gender==1 & merged_midfarmer_dyadSA$knows_SA_rating==1])),
                                     mean(merged_midfarmer_dyadSA$small_quant_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sd(merged_midfarmer_dyadSA$small_quant_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1], na.rm=T),
                                     sum(!is.na (merged_midfarmer_dyadSA$small_quant_mid[merged_midfarmer_dyadSA$farmer_gender==0 & merged_midfarmer_dyadSA$knows_SA_rating==1]))))

#Descriptives of services farmers are getting at endline 
##################################################################

#Q70. If there is a problem with the seed of  **${calc_biz}**, can you carry the seed back to **${calc_biz}** and get a refund (insurance)?
table(endline_rating$refunds)    #848 say 98 which means 'dont know'
endline_rating$refunds[endline_rating$refunds=="n/a"] <- NA
endline_rating$refunds[endline_rating$refunds==98] <- NA
table(endline_rating$refunds)      #735 say no and 748 say yes 
endline_rating$refunds_end <-as.numeric(dplyr::recode(    #recoding to get numerical values
  endline_rating$refunds,
  "No" = 0,  
  "Yes" = 1, 
))
table(endline_rating$refunds_end)
mean(endline_rating$refunds_end, na.rm = T)   # 0.504383

#Q71. Does  **${calc_biz}** give credit, i.e. gives you seed (or inputs more in general) that you can pay for later (after harvest)
table(endline_rating$gives_credit)    #549 say 98 which means 'dont know'
endline_rating$gives_credit[endline_rating$gives_credit=="n/a"] <- NA
endline_rating$gives_credit[endline_rating$gives_credit==98] <- NA
table(endline_rating$gives_credit)      #756 say no and 1026 say yes 
endline_rating$gives_credit_end <-as.numeric(dplyr::recode(    #recoding to get numerical values
  endline_rating$gives_credit,
  "No" = 0,  
  "Yes" = 1, 
))
table(endline_rating$gives_credit_end)
mean(endline_rating$gives_credit_end, na.rm = T)   #0.5757576

#Q72. Does  **${calc_biz}** train/advise you on how to use improved seed varieties while buying seed?
table(endline_rating$gives_advice)    #117 say 98 which means 'dont know'
endline_rating$gives_advice[endline_rating$gives_advice=="n/a"] <- NA
endline_rating$gives_advice[endline_rating$gives_advice==98] <- NA
table(endline_rating$gives_advice)      #309 say no and 1905 say yes 
endline_rating$gives_advice_end <-as.numeric(dplyr::recode(    #recoding to get numerical values
  endline_rating$gives_advice,
  "No" = 0,  
  "Yes" = 1, 
))
table(endline_rating$gives_advice_end)
mean(endline_rating$gives_advice_end, na.rm = T)   #0.8604336

#Q73. Does **${calc_biz}** deliver seeds to clients at home? 
table(endline_rating$delivers)    #446 say 98 which means 'dont know'
endline_rating$delivers[endline_rating$delivers=="n/a"] <- NA
endline_rating$delivers[endline_rating$delivers==98] <- NA
table(endline_rating$delivers)      #1288 say no and 597 say yes 
endline_rating$delivers_end <-as.numeric(dplyr::recode(    #recoding to get numerical values
  endline_rating$delivers,
  "No" = 0,  
  "Yes" = 1, 
))
table(endline_rating$delivers_end)
mean(endline_rating$delivers_end, na.rm = T)   # 0.3167109

#Q74. Does  **${calc_biz}** offer after-sales service?
table(endline_rating$after_sales_service)    #371 say 98 which means 'dont know'
endline_rating$after_sales_service[endline_rating$after_sales_service=="n/a"] <- NA
endline_rating$after_sales_service[endline_rating$after_sales_service==98] <- NA
table(endline_rating$after_sales_service)      #1260 say no and 700 say yes 
endline_rating$after_sales_service_end <-as.numeric(dplyr::recode(    #recoding to get numerical values
  endline_rating$after_sales_service,
  "No" = 0,  
  "Yes" = 1, 
))
table(endline_rating$after_sales_service_end)
mean(endline_rating$after_sales_service_end, na.rm = T)   #0.3571429

#Q75. Does **${calc_biz}** accept different payment methods?
table(endline_rating$payment_mehtods)    #520 say 98 which means 'dont know'
endline_rating$payment_mehtods[endline_rating$payment_mehtods=="n/a"] <- NA
endline_rating$payment_mehtods[endline_rating$payment_mehtods==98] <- NA
table(endline_rating$payment_mehtods)      #801 say no and 1010 say yes 
endline_rating$payment_methods_end <-as.numeric(dplyr::recode(    #recoding to get numerical values
  endline_rating$payment_mehtods,
  "No" = 0,  
  "Yes" = 1, 
))
table(endline_rating$payment_methods_end)
mean(endline_rating$payment_methods_end, na.rm = T)   #0.5577029

#Q76. Does  **${calc_biz}** sell small quantities if necessary (1kg)?
table(endline_rating$small_quant)    #117 say 98 which means 'dont know'
endline_rating$small_quant[endline_rating$small_quant=="n/a"] <- NA
endline_rating$small_quant[endline_rating$small_quant==98] <- NA
table(endline_rating$small_quant)      #172 say no and 2042 say yes 
endline_rating$small_quant_end <-as.numeric(dplyr::recode(    #recoding to get numerical values
  endline_rating$small_quant,
  "No" = 0,  
  "Yes" = 1, 
))
table(endline_rating$small_quant_end)
mean(endline_rating$small_quant_end, na.rm = T)    #0.9223126

desc_end_serv<- rbind(c(mean(endline_rating$refunds_end, na.rm=T),
                        sd(endline_rating$refunds_end, na.rm=T),
                        sum(!is.na (endline_rating$refunds_end))),
                      
                      c(mean(endline_rating$gives_credit_end, na.rm=T),
                        sd(endline_rating$gives_credit_end, na.rm=T),
                        sum(!is.na (endline_rating$gives_credit_end))),
                      
                      c(mean(endline_rating$gives_advice_end, na.rm=T),
                        sd(endline_rating$gives_advice_end, na.rm=T),
                        sum(!is.na (endline_rating$gives_advice_end))),
                      
                      c(mean(endline_rating$delivers_end, na.rm=T),
                        sd(endline_rating$delivers_end, na.rm=T),
                        sum(!is.na (endline_rating$delivers_end))),
                      
                      c(mean(endline_rating$after_sales_service_end, na.rm=T),
                        sd(endline_rating$after_sales_service_end, na.rm=T),
                        sum(!is.na (endline_rating$after_sales_service_end))),
                      
                      c(mean(endline_rating$payment_methods_end, na.rm=T),
                        sd(endline_rating$payment_methods_end, na.rm=T),
                        sum(!is.na (endline_rating$payment_methods_end))),
                      
                      c(mean(endline_rating$small_quant_end, na.rm=T),
                        sd(endline_rating$small_quant_end, na.rm=T),
                        sum(!is.na (endline_rating$small_quant_end))))

#we need to now get gender of the farming hh head in the same dataset as the services
merged_endfarmer_dyad <- merge(endline_farmers, endline_rating, by = "farmer_ID")   #merging the datasets by farmer ID 
merged_endfarmer_dyad$farmer_gender <-as.numeric(dplyr::recode(    #recoding to get numerical values for gender of hh head
  merged_endfarmer_dyad$check.maize.q5b,
  "Female" = 0,  
  "Male" = 1, 
))   
table(merged_endfarmer_dyad$farmer_gender)

#number of males vs. females having the services (endline)

tab_refunds_end <- table( merged_endfarmer_dyad$farmer_gender, merged_endfarmer_dyad$refunds_end)
addmargins(tab_refunds_end)   #out of 275 F, 157 say yes; out of 1208 M, 591 say yes 
percent_tab_refunds_end <- round(prop.table(tab_refunds_end, margin = 1) * 100, 1 ) 
#print (percent_tab_refunds_end)   #57.1% F vs. 48.9% M say yes 

tab_credit_end <- table(merged_endfarmer_dyad$farmer_gender, merged_endfarmer_dyad$gives_credit_end)
addmargins(tab_credit_end)    #out of 327 F, 169 say yes; out of 1455 male, 857 say yes 
percent_tab_credit_end <- round(prop.table(tab_credit_end, margin = 1) * 100, 1 ) 
#print (percent_tab_credit_end)   #51.7% F vs. 58.9% M say yes 

tab_advice_end <- table(merged_endfarmer_dyad$farmer_gender, merged_endfarmer_dyad$gives_advice_end)
addmargins(tab_advice_end)    #out of 400 F, 335 say yes; out of 1814 male, 1570 say yes 
percent_tab_advice_end <- round(prop.table(tab_advice_end, margin = 1) * 100, 1 ) 
#print (percent_tab_advice_end)   #83.8% F vs. 86.5% M say yes 

tab_delivers_end <- table(merged_endfarmer_dyad$farmer_gender, merged_endfarmer_dyad$delivers_end)
addmargins(tab_delivers_end)    #out of 344 F, 96 say yes; out of 1541 male, 501 say yes 
percent_tab_delivers_end <- round(prop.table(tab_delivers_end, margin = 1) * 100, 1 ) 
#print (percent_tab_delivers_end)   #27.9% F vs. 32.5% M say yes 

tab_aftersales_end <- table(merged_endfarmer_dyad$farmer_gender, merged_endfarmer_dyad$after_sales_service_end)
addmargins(tab_aftersales_end)    #out of 347 F, 129 say yes; out of 1618 male, 571 say yes 
percent_tab_aftersales_end <- round(prop.table(tab_aftersales_end, margin = 1) * 100, 1 ) 
#print (percent_tab_aftersales_end)   #37.2% F vs. 35.4% M say yes 

tab_payment_method_end <- table(merged_endfarmer_dyad$farmer_gender, merged_endfarmer_dyad$payment_methods_end)
addmargins(tab_payment_method_end)    #out of 332 F, 180 say yes; out of 1479 male, 830 say yes 
percent_tab_payment_method_end <- round(prop.table(tab_payment_method_end, margin = 1) * 100, 1 ) 
#print (percent_tab_payment_method_end)   #54.2% F vs. 56.1% M say yes 

tab_smallquant_end <- table(merged_endfarmer_dyad$farmer_gender, merged_endfarmer_dyad$small_quant_end)
addmargins(tab_smallquant_end)    #out of 404 F, 371 say yes; out of 1810 male, 1671 say yes 
percent_tab_smallquant_end <- round(prop.table(tab_smallquant_end, margin = 1) * 100, 1 ) 
#print (percent_tab_smallquant_end)   #91.8% F vs. 92.3% M say yes

desc_end_serv_gender<- rbind(c(mean(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sd(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==1])),
                               mean(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sd(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==0]))),
                             
                             c(mean(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sd(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==1])),
                               mean(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sd(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==0]))),
                             
                             c(mean(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sd(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==1])),
                               mean(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sd(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==0]))),
                             
                             c(mean(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sd(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==1])),
                               mean(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sd(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==0]))),
                             
                             c(mean(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sd(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==1])),
                               mean(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sd(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==0]))),
                             
                             c(mean(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sd(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==1])),
                               mean(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sd(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==0]))),
                             
                             c(mean(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sd(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==1], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==1])),
                               mean(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sd(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==0], na.rm=T),
                               sum(!is.na (merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==0]))))

#now we will only focus on the sample who bought at dealer -- number of males vs. females having the services 
table(merged_endfarmer_dyad$bought_at_dealer)
merged_endfarmer_dyad$bought_at_dealer[merged_endfarmer_dyad$bought_at_dealer=="n/a"] <- NA
merged_endfarmer_dyad$bought_dealer <-as.numeric(dplyr::recode(    #recoding to get numerical values 
  merged_endfarmer_dyad$bought_at_dealer,
  "No" = 0,  
  "Yes" = 1, 
))  

tab_refunds_boughtend <- table( merged_endfarmer_dyad$farmer_gender[merged_endfarmer_dyad$bought_dealer==1], merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$bought_dealer==1])
addmargins(tab_refunds_boughtend)   #out of 252 F, 145 say yes; out of 1145 M, 555 say yes 
percent_tab_refunds_boughtend <- round(prop.table(tab_refunds_boughtend, margin = 1) * 100, 1 ) 
#print (percent_tab_refunds_boughtend)   #57.5% F vs. 48.5% M say yes 

tab_credit_boughtend <- table( merged_endfarmer_dyad$farmer_gender[merged_endfarmer_dyad$bought_dealer==1], merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$bought_dealer==1])
addmargins(tab_credit_boughtend)   #out of 301 F, 152 say yes; out of 1373 M, 816 say yes 
percent_tab_credit_boughtend <- round(prop.table(tab_credit_boughtend, margin = 1) * 100, 1 ) 
#print (percent_tab_credit_boughtend)   #50.5% F vs. 59.4% M say yes 

tab_advice_boughtend <- table(merged_endfarmer_dyad$farmer_gender[merged_endfarmer_dyad$bought_dealer==1], merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$bought_dealer==1])
addmargins(tab_advice_boughtend)    #out of 366 F, 305 say yes; out of 1710 male, 1471 say yes 
percent_tab_advice_boughtend <- round(prop.table(tab_advice_boughtend, margin = 1) * 100, 1 ) 
#print (percent_tab_advice_boughtend)   #83.3% F vs. 86.0% M say yes 

tab_delivers_boughtend <- table(merged_endfarmer_dyad$farmer_gender[merged_endfarmer_dyad$bought_dealer==1], merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$bought_dealer==1])
addmargins(tab_delivers_boughtend)    #out of 317 F, 87 say yes; out of 1457 male, 476 say yes 
percent_tab_delivers_boughtend <- round(prop.table(tab_delivers_boughtend, margin = 1) * 100, 1 ) 
#print (percent_tab_delivers_boughtend)   #27.4% F vs. 32.7% M say yes 

tab_aftersales_boughtend <- table(merged_endfarmer_dyad$farmer_gender[merged_endfarmer_dyad$bought_dealer==1], merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$bought_dealer==1])
addmargins(tab_aftersales_boughtend)    #out of 317 F, 111 say yes; out of 1523 male, 530 say yes 
percent_tab_aftersales_boughtend <- round(prop.table(tab_aftersales_boughtend, margin = 1) * 100, 1 ) 
#print (percent_tab_aftersales_boughtend)   #35% F vs. 34.8% M say yes 

tab_payment_method_boughtend <- table(merged_endfarmer_dyad$farmer_gender[merged_endfarmer_dyad$bought_dealer==1], merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$bought_dealer==1])
addmargins(tab_payment_method_boughtend)    #out of 303 F, 167 say yes; out of 1404 male, 793 say yes 
percent_tab_payment_method_boughtend <- round(prop.table(tab_payment_method_boughtend, margin = 1) * 100, 1 ) 
#print (percent_tab_payment_method_boughtend)   #55.1% F vs. 56.5% M say yes 

tab_smallquant_boughtend <- table(merged_endfarmer_dyad$farmer_gender[merged_endfarmer_dyad$bought_dealer==1], merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$bought_dealer==1])
addmargins(tab_smallquant_boughtend)    #out of 367 F, 336 say yes; out of 1699 male, 1567 say yes 
percent_tab_smallquant_boughtend <- round(prop.table(tab_smallquant_boughtend, margin = 1) * 100, 1 ) 
#print (percent_tab_smallquant_boughtend)   #91.6% F vs. 92.2% M say yes 

desc_end_serv_genderbought<- rbind(c(mean(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1])),
                                     mean(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1]))),
                                   
                                   c(mean(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1])),
                                     mean(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1]))),
                                   
                                   c(mean(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1])),
                                     mean(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1]))),
                                   
                                   c(mean(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1])),
                                     mean(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1]))),
                                   
                                   c(mean(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1])),
                                     mean(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1]))),
                                   
                                   c(mean(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1])),
                                     mean(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1]))),
                                   
                                   c(mean(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$bought_dealer==1])),
                                     mean(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sd(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$bought_dealer==1]))))

desc_end_serv_genderSA<- rbind(c(mean(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"])),
                                     mean(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$refunds_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"]))),
                                   
                                   c(mean(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"])),
                                     mean(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$gives_credit_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"]))),
                                   
                                   c(mean(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"])),
                                     mean(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$gives_advice_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"]))),
                                   
                                   c(mean(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"])),
                                     mean(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$delivers_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"]))),
                                   
                                   c(mean(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"])),
                                     mean(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$after_sales_service_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"]))),
                                   
                                   c(mean(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"])),
                                     mean(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$payment_methods_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"]))),
                                   
                                   c(mean(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==1 & merged_endfarmer_dyad$knows_SA_rating=="Yes"])),
                                     mean(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sd(merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"], na.rm=T),
                                     sum(!is.na (merged_endfarmer_dyad$small_quant_end[merged_endfarmer_dyad$farmer_gender==0 & merged_endfarmer_dyad$knows_SA_rating=="Yes"]))))
