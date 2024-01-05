######ROX <=4.88
#########starting to create the population
###########Let's keep patients from the medical/surgical ICU
target.trial.backup<-target.trial.proof ###safe object
target.trial.proof<-target.trial.backup
target.trial.proof<-target.trial.proof%>%filter(first_careunit %in%c("Medical/Surgical Intensive Care Unit (MICU/SICU)","Medical Intensive Care Unit (MICU)","Coronary Care Unit (CCU)" ))
length(unique(target.trial.proof$id))
quantile(target.trial.proof$SaFi,na.rm=T)
target.trial.intubated<-target.trial.proof%>%filter(ever.intubated==1)
length(unique(target.trial.intubated$id))


##########1st STEP
intermediate.proof<-target.trial.proof%>%group_by(id)%>% #####we can use the general dataframe since we will then filter for condition and all observations at time 0 will be removed (lagged so NA)
  mutate(invasive_cumsum = cumsum(invasive)) %>% 
  filter(!(invasive_cumsum > 1)) %>%
  select(-invasive_cumsum)

dim(target.trial.proof)
dim(intermediate.proof)
length(unique(intermediate.proof$id))
length(unique(target.trial.proof$id))
intubated.only.proof<-intermediate.proof %>%
  filter(invasive == 1) 
length(unique(intubated.only.proof$id))
######14000 patients
quantile(intubated.only.proof$hour) #####most in the first few hours
quantile(intubated.only.proof$SaFi,na.rm=T)
quantile(intubated.only.proof$RR,na.rm=T)
sum(is.na(intubated.only.proof$SaFi))
colSums(is.na(intubated.only.proof))

#########################################################
#####2nd STEP: Define the target trial
#####ROX<=4.88, RR <=40, GCS>=12

#####Here, we keep only the observations that meet the criteria for the target trial in the first 24 hours after admission
#####However, these patients must not fulfill an exclusion criteria before
# create a new variable that accumulates the number of times the exclusion criteria has been met by subject and hour
intermediate.proof <- intermediate.proof %>%
  group_by(id,hour) %>%
  mutate(exclusion_met = cumsum((SaFi<88|RR>39|gcs<12)))%>%ungroup()

intermediate.final.proof <- intermediate.proof %>%
  filter(exclusion_met == 0)
length(unique(intermediate.final.proof$id))
#####how many intubated without exclusion criteria
intubated.only.no.exclusion.proof<-intermediate.final.proof %>%
  filter(ever.intubated == 1)
length(unique(intubated.only.no.exclusion.proof$id))
dim(intubated.only.no.exclusion.proof)
#####2000 patients
quantile(intubated.only.no.exclusion.proof$SaFi,na.rm=T)
sum(is.na(intubated.only.no.exclusion.proof$SaFi))

length(unique(intermediate.final.proof$id)) 
####12000 patients without exclusion criteria.

#####Identify those with inclusion criteria
quantile(intermediate.final.proof$SaFi,na.rm=T)
intermediate.final.proof$code_status[is.na(intermediate.final.proof$code_status)]<-"Full code"
inclusion.proof<-intermediate.final.proof %>% 
  filter(
    hour <= 48,
    ROX <=4.88 & spO2<=97 & code_status=="Full code"
  )

length(unique(inclusion.proof$id))

####These are the patients who fulfill the criteria between admission and T24
####2200 patients
dim(inclusion.proof) ####roughly 19000 observations
intubated.with.inclusion.proof<-inclusion.proof%>%filter(invasive==1)
dim(intubated.with.inclusion.proof)


#####I will now proceed to identify observations from these patients after the 24th hour
#####We will here get the observations that fulfill the criteria after the 24th hour and do not fulfil exclusion criteria
ids.24h <- intermediate.final.proof %>%
  filter(
    hour >48,
    ROX<=4.88 & spO2<=97 & code_status=="Full code"
  )

####with an semi join i will retain only those present in the dataframe called intermediate_filtered
ids.24h <- ids.24h %>%
  semi_join(inclusion.proof, by = "id")

dim(ids.24h) ####30000 observations

####We here merge them
target.unimputed.proof<-rbind(inclusion.proof,ids.24h)
length(unique(target.unimputed.proof$id))
dim(target.unimputed.proof)
intubated.proof<-target.unimputed.proof%>%filter(invasive==1)
dim(intubated.proof)####348 patients intubated out of 1293 patients
min(target.unimputed.proof$hour) 
quantile(target.unimputed.proof$hour)

####finally, we need to remove observations due to patients not being in the icu (dead/discharged)
target.unimputed.proof <- target.unimputed.proof %>%
  mutate(time_diff = ICU_los*24)
####remove lines that have time_diff < hr
target.unimputed.proof<-target.unimputed.proof%>%filter(time_diff>=hour)
dim(target.unimputed.proof) ####we retain roughly 7600 observations from 1200 patients 
length(unique(target.unimputed.proof$id))

#######We'll change the flag to reset it to hour=1 when the conditions are met
target.unimputed.proof <- target.unimputed.proof %>%
  group_by(id) %>% 
  mutate(reset.hour = row_number())

########we'll filter out observations beyond the 48 repetitions
target.unimputed.proof<-target.unimputed.proof%>%filter(reset.hour<49)
dim(target.unimputed.proof)

####Since we have removed rows we don't know the mortality at 30 days (death_outcome is useless)
##### we have dod as date
##### create 1yr mortality
target.unimputed.proof$dod<-as.Date(target.unimputed.proof$dod)
target.unimputed.proof$mortality.1y<-ifelse(is.na(target.unimputed.proof$dod),0,1)
target.unimputed.proof$mortality.1y<-as.factor(target.unimputed.proof$mortality.1y)
# Filter for the condition
deaths.proof <- target.unimputed.proof %>%
  filter(mortality.1y == 1) 

# Identify unique ids
unique_ids.proof <- deaths.proof %>%
  pull(id) %>%
  unique()
length(unique_ids.proof)
length(unique(target.unimputed.proof$id))
####roughly 1500 deaths out of 2900 patients
target.unimputed.proof <- target.unimputed.proof %>%
  group_by(id) %>%
  mutate(first_date = min(starttime, na.rm = TRUE))
target.unimputed.proof$first_date<-as.Date(target.unimputed.proof$first_date)
target.unimputed.proof$time.to.death<-difftime(target.unimputed.proof$dod,target.unimputed.proof$first_date,units="days")
target.unimputed.proof$time.to.death[target.unimputed.proof$time.to.death<0]<-NA ####0 that are negative
target.unimputed.proof$time.to.death[is.na(target.unimputed.proof$time.to.death)]<-365
target.unimputed.proof$mortality.1y.censored<-ifelse(target.unimputed.proof$time.to.death>365,NA,as.factor(target.unimputed.proof$mortality.1y))
target.unimputed.proof$mortality.1y.censored<-as.factor(target.unimputed.proof$mortality.1y.censored)
levels(target.unimputed.proof$mortality.1y.censored)<-c(0,1)
target.unimputed.proof$mortality.1y.censored[is.na(target.unimputed.proof$mortality.1y.censored)]<-0
target.unimputed.proof$time.to.death[target.unimputed.proof$time.to.death>365]<-365

#################3st STEP: Impute the data
#####Let's explore some data before that
colSums(is.na(target.unimputed.proof))
######Few missings
target.to.impute.proof<-target.unimputed.proof
######Saving the object

#####Let's summarise by IMV group
target.to.impute.proof$invasive<-as.factor(target.to.impute.proof$invasive)
dependent <- 'invasive'
target.to.impute.proof$SBP<-as.integer(target.to.impute.proof$SBP)
target.to.impute.proof$DBP<-as.integer(target.to.impute.proof$DBP)
target.to.impute.proof$MBP<-as.integer(target.to.impute.proof$MBP)

####Table at eligibility
target.to.impute.proof<-target.to.impute.proof%>%group_by(id)%>%mutate(ever.intubated= ifelse(any(invasive == 1), 1, 0))%>%ungroup()
eligibility<-target.to.impute.proof%>%filter(reset.hour==1)
dependent1<-eligibility$ever.intubated
eligibility$ever.intubated<-as.factor(eligibility$ever.intubated)
explanatory1<-names(eligibility[,c("age","elixhauser","SaFi","RR","ROX","fio2.final","gcs","vasopressor","SBP","DBP","MBP","HR","temp","bilirubin_max","creatinine_max","platelet_min")])
eligibility%>% 
  summary_factorlist("ever.intubated",explanatory1,cont_nonpara=8,cont_range=T,p=TRUE)
quantile(target.to.impute.proof$vasopressor,na.rm=T)

############MICE
set.seed(123)
target.to.impute.proof<-target.to.impute.proof%>%fill(bilirubin_max,creatinine_max,platelet_min,HR,temp,glucose)
colSums(is.na(target.to.impute.proof))
missing_percentage <- sapply(target.to.impute.proof, function(x) {
  tapply(x, rep(1, length(x)), function(y) {
    mean(is.na(y)) * 100
  })
})

print(missing_percentage)

target_imp<-subset(target.to.impute.proof[,c("age","elixhauser","SaFi","RR","ROX","fio2.final","gcs","vasopressor","SBP","DBP","MBP","HR","temp","bilirubin_max","creatinine_max","platelet_min")])
imp_mat.mimic<-mice(target_imp,m=5)
Ind.Na11.imput.mimic = apply(is.na(target_imp),2,which)
median.imputed.data.mimic= target_imp
for(i in 1:length(median.imputed.data.mimic)){
  aa2.mimic = matrix(unlist(imp_mat.mimic$imp[i]),ncol=5,byrow = F)
  median.imputed.data.mimic[unlist(Ind.Na11.imput.mimic[i]),i] = apply(aa2.mimic,1,median)
}
target.unimputed.to.merge<-target.to.impute.proof%>%select(-c("age","elixhauser","SaFi","RR","ROX","fio2.final","gcs","vasopressor","SBP","DBP","MBP","HR","temp","bilirubin_max","creatinine_max","platelet_min"))
mimic.imputed.final<-cbind(target.unimputed.to.merge,median.imputed.data.mimic)
mimic.imputed.final$SaFi<-mimic.imputed.final$spO2/(mimic.imputed.final$fio2.final/100)
mimic.imputed.final$ROX<-mimic.imputed.final$SaFi/mimic.imputed.final$RR


#######let's study data before and after imputation
apply(target.to.impute.proof[,c("bilirubin_max","creatinine_max","platelet_min")],2,mean,na.rm=T)
apply(target.to.impute.proof[,c("bilirubin_max","creatinine_max","platelet_min")],2,sd,na.rm=T)
apply(target.to.impute.proof[,c("bilirubin_max","creatinine_max","platelet_min")],2,quantile,na.rm=T)

apply(mimic.imputed.final[,c("bilirubin_max","creatinine_max","platelet_min")],2,mean,na.rm=T)
apply(mimic.imputed.final[,c("bilirubin_max","creatinine_max","platelet_min")],2,sd,na.rm=T)
apply(mimic.imputed.final[,c("bilirubin_max","creatinine_max","platelet_min")],2,quantile,na.rm=T)

####No changes from before to after imputation

eligibility.after<-mimic.imputed.final%>%filter(reset.hour==1)
dependent1<-eligibility.after$ever.intubated
eligibility.after$ever.intubated<-as.factor(eligibility.after$ever.intubated)
explanatory.final<-names(mimic.imputed.final[,c("age","elixhauser","SaFi","RR","ROX","noninvasive","highflow","fio2.final","gcs","vasopressor","SBP","DBP","MBP","HR","temp","bilirubin_max","creatinine_max","platelet_min","first_careunit","admission_type","admission_location")])
eligibility.after%>% 
  summary_factorlist("ever.intubated",explanatory.final, cont_nonpara=8,cont_range=T,p=TRUE)
write_xlsx(eligibility.after%>% 
             summary_factorlist("ever.intubated", explanatory.final, p=TRUE),path ="baseline.table.mimic.xlsx")
length(unique(mimic.imputed.final$id))
sum(eligibility.after$ever.intubated==1)
intubated<-mimic.imputed.final%>%filter(invasive==1)
quantile(intubated$hour)

################## 5TH STEP:IPTW########
mimic.imputed.final.target<-mimic.imputed.final
colSums(is.na(mimic.imputed.final.target))
mimic.imputed.final.target$invasive<-as.numeric(mimic.imputed.final.target$invasive)
mimic.imputed.final.target$subject_id<-as.numeric(mimic.imputed.final.target$subject_id)
mimic.imputed.final.target$reset.hour<-as.numeric(mimic.imputed.final.target$reset.hour)
mimic.imputed.final.target<-as.data.frame(mimic.imputed.final.target)
mimic.imputed.final.target$invasive<-as.factor(mimic.imputed.final.target$invasive)
levels(mimic.imputed.final.target$invasive)<-c(0,1)
table(mimic.imputed.final.target$reset.hour,mimic.imputed.final.target$invasive)

#####lagging to allow modelling the deltas
mimic.imputed.final.target <- mimic.imputed.final.target %>%
  arrange(id, reset.hour) %>%  # Make sure the data is ordered by id and then by time
  group_by(id) %>%  # Group the data by the 'id' variable
  mutate(
    prev_SaFi = lag(SaFi, 1, order_by = reset.hour), # Create a lagged variable for 'measurement'
    # You can add as many lagged variables as you need:
    prev_RR = lag(RR, 1, order_by = reset.hour),
    prev_fio2=lag(fio2.final,1,order_by=reset.hour)
    # '1' is the lag interval; you can change it to create lagged variables over different intervals
  ) %>%
  ungroup()

set.seed(123)
mimic.imputed.final.target<-as.data.frame(mimic.imputed.final.target)

#####calculate weights
####for hour 1
mimic.imputed.final.target.hour1<-mimic.imputed.final.target%>%filter(reset.hour==1)
ps_model1<-glm(invasive~gcs+age+elixhauser+RR+fio2.final+SaFi+vasopressor+first_careunit,family="binomial",data=mimic.imputed.final.target.hour1)
probability.of.intubation<-sum(mimic.imputed.final.target.hour1$invasive==1)/dim(mimic.imputed.final.target.hour1)[1]
mimic.imputed.final.target.hour1$ps<-ps_model1$fitted.values
mimic.imputed.final.target.hour1$weights<-ifelse(mimic.imputed.final.target.hour1$invasive==1,probability.of.intubation/mimic.imputed.final.target.hour1$ps,(1-probability.of.intubation)/(1-mimic.imputed.final.target.hour1$ps))

#####for the remaining hours
mimic.imputed.final.target.hours<-mimic.imputed.final.target%>%filter(reset.hour>1)
ps_model2<-glm(invasive~reset.hour+gcs+age+elixhauser+SaFi+RR+fio2.final+vasopressor+prev_SaFi+prev_RR+prev_fio2+first_careunit,family="binomial",data=mimic.imputed.final.target.hours)
mimic.imputed.final.target.hours$ps<-ps_model2$fitted.values
marginal_probs <- mimic.imputed.final.target.hours %>%
  group_by(reset.hour) %>%
  summarize(marginal_treatment_prob = mean(as.numeric(invasive)-1),
            marginal_no_treatment_prob = 1 - marginal_treatment_prob) %>%
  ungroup()

# Join the marginal probabilities back to the original dataframe
mimic.imputed.final.target.hours <- mimic.imputed.final.target.hours %>%
  left_join(marginal_probs%>% select(marginal_treatment_prob,marginal_no_treatment_prob,reset.hour), by = "reset.hour")

# Then calculate the stabilized weights
mimic.imputed.final.target.hours <- mimic.imputed.final.target.hours %>%
  mutate(
    weights = ifelse(invasive == 1, 
                     marginal_treatment_prob / ps,
                     marginal_no_treatment_prob / (1 - ps))
  )

####merge the cohorts
mimic.imputed.final.target.hours<-mimic.imputed.final.target.hours%>%select(-c("marginal_treatment_prob","marginal_no_treatment_prob"))
mimic.imputed.final.target<-rbind(mimic.imputed.final.target.hour1,mimic.imputed.final.target.hours)

#######weights accross time
weight.plot<-ggplot(mimic.imputed.final.target,aes(x=as.factor(reset.hour),y=weights))+geom_boxplot()
weight.plot

#####################TRUNCATION
########Perform some weight truncation at 99%
mimic.to.truncate<-mimic.imputed.final.target
quantile(mimic.imputed.final.target$weights)
mimic.to.truncate$weights[mimic.to.truncate$weights>quantile(mimic.to.truncate$weights,0.99)]<-quantile(mimic.to.truncate$weights,0.99)
quantile(mimic.to.truncate$weights)

#######weights accross time
weight.plot.truncated<-ggplot(mimic.to.truncate,aes(x=as.factor(reset.hour),y=weights))+geom_boxplot()
weight.plot.truncated

#####MAIN ANALYSIS
#####SURVIVAL OBJECT
set.seed(123)
surv_object.truncated <- with(mimic.to.truncate, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated <- coxph(surv_object.truncated ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = mimic.to.truncate,robust=T)
summary(cox_model.truncated)

###we fit a stratified object to use it for the KM
cox_model.truncated.strata <- coxph(surv_object.truncated ~ strata(invasive) +SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,cluster=id,data = mimic.to.truncate, weights = weights,robust=T)
summary(cox_model.truncated.strata)

km_model.truncated <- survfit(cox_model.truncated.strata, data = mimic.to.truncate)
end_time <- 365
summary(km_model.truncated,times=end_time)

####KM
# Plot Kaplan-Meier curves
KM.plot.truncated<-ggsurvplot(km_model.truncated,data=mimic.to.truncate,pval="Hazard ratio: 0.79, 95% CI 0.62-1.008, p= 0.06",censor=F,risk.table = T,legend.labs=c("Intubation = No", "Intubation = Yes"),title = "Kaplan-Meier Curves by Intubation Group",xlab = "Time",
                              ylab = "Survival Probability",pval.coord = c(100,0.3))
KM.plot.truncated$plot+geom_vline(aes(xintercept = 28), linetype = "dashed", color = "blue") +
  geom_vline(aes(xintercept = 60), linetype = "dashed", color = "red")+scale_x_continuous(breaks = c(0,90,365),expand = c(0,0))

############

######30-day mortality######
mimic.to.truncate<-mimic.to.truncate%>%mutate(mortality.30d.censored=ifelse(time.to.death<=30,1,0),time.fu=ifelse(time.to.death<=30,time.to.death,30))
surv_object.truncated.30d <- with(mimic.to.truncate, Surv(time.fu, mortality.30d.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.30d <- coxph(surv_object.truncated.30d ~ invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,cluster=id,weights=weights,data = mimic.to.truncate,robust=T)
summary(cox_model.truncated.30d)

cox_model.truncated.strata.30 <- coxph(surv_object.truncated.30d ~ strata(invasive) + age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,cluster=id,data = mimic.to.truncate, weights = weights,robust=T)
summary(cox_model.truncated.strata.30)

km_model.truncated.30d <- survfit(cox_model.truncated.strata.30, data = mimic.to.truncate)

####KM
# Plot Kaplan-Meier curves
KM.plot.truncated.30d<-ggsurvplot(km_model.truncated.30d,data=mimic.to.truncate,pval="Hazard ratio: 0.87, 95% CI 0.65-1.16, p= 0.35",censor=F,risk.table = T,legend.labs=c("Intubation = No", "Intubation = Yes"),title = "Kaplan-Meier Curves by Intubation Group",xlab = "Time",
                                  ylab = "Survival Probability")
KM.plot.truncated.30d

######LR#######
mortality.lr<-geeglm(as.numeric(mortality.1y.censored)-1 ~ invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,id=id,data = mimic.to.truncate,family = binomial,corstr = "exchangeable")
summary(mortality.lr)
exp(mortality.lr$coefficients)
mimic.to.truncate$predicted_outcome <- predict(mortality.lr, type = "response")
cov<-c("age","elixhauser","RR","SaFi","fio2.final","gcs","vasopressor","SBP","DBP","MBP","HR","temp","bilirubin_max","creatinine_max","platelet_min","first_careunit")
aipw.mortality.lr<-AIPW$new(Y=as.numeric(mimic.to.truncate$mortality.1y.censored)-1,A=as.numeric(mimic.to.truncate$invasive)-1,W=subset(mimic.to.truncate,select=cov),Q.SL.library="SL.glm",g.SL.library="SL.glm",verbose=T,k_split=10)
aipw.mortality.lr$fit()
aipw.mortality.lr$plot.p_score()
suppressWarnings({
  aipw.mortality.lr$stratified_fit()$summary()
})

#####ICU LOS
set.seed(123)
bootstrap_stats <- function(data, n_bootstrap=1000) {
  replicate(n_bootstrap, {
    # Sample IDs with replacement
    sampled_ids <- sample(unique(data$id), size = length(unique(data$id)), replace = TRUE)
    
    # Get corresponding rows for sampled IDs
    sampled_data <- data %>% filter(id %in% sampled_ids)
    
    # Calculate medians and quantiles for each group
    stats_group1 <- sampled_data %>% filter(ever.intubated == 0) %>% summarize(median = weightedMedian(los,weights=weights,na.rm=T), q25 = weightedQuantile(los, weights=weights,0.25,na.rm=T), q75 = weightedQuantile(los, weights=weights,0.75,na.rm=T))
    stats_group2 <- sampled_data %>% filter(ever.intubated == 1) %>% summarize(median = weightedMedian(los,weights=weights,na.rm=T), q25 = weightedQuantile(los, weights=weights,0.25,na.rm=T), q75 = weightedQuantile(los, weights=weights,0.75,na.rm=T))
    
    # Return results
    list(
      median_1 = stats_group1$median,
      q25_1 = stats_group1$q25,
      q75_1 = stats_group1$q75,
      median_2 = stats_group2$median,
      q25_2 = stats_group2$q25,
      q75_2 = stats_group2$q75,
      diff_median = stats_group1$median - stats_group2$median,
      diff_q25 = stats_group1$q25 - stats_group2$q25,
      diff_q75 = stats_group1$q75 - stats_group2$q75
    )
  }, simplify = "data.frame")
}

# Get bootstrapped statistics
set.seed(123)
boot_results <- bootstrap_stats(mimic.imputed.final.target)
median_1<-unlist(boot_results[1,])
q25_1<-unlist(boot_results[2,])
q75_1<-unlist(boot_results[3,])
median_2<-unlist(boot_results[4,])
q25_2<-unlist(boot_results[5,])
q75_2<-unlist(boot_results[6,])
diff_median<-unlist(boot_results[7,])
diff_q25<-unlist(boot_results[8,])
diff_q75<-unlist(boot_results[9,])
results<-cbind(median_1,q25_1,q75_1,median_2,q25_2,q75_2,diff_median,diff_q25,diff_q75)
summarized<-apply(results,2,mean)
summarized
-mean(diff_median)-1.96*sd(diff_median)
-mean(diff_median)+1.96*sd(diff_median)

####Hosp_LOS
####modification by sicheng. data from hr minus 1
mimic.target.trial.dod.minus1 <- read.csv("~/Downloads/mimic.target.trial.dod.minus1.csv")
mimic.target.trial.4.hour0<-mimic.target.trial.dod.minus1%>%filter(hr==0)
mimic.to.truncate.hosp.los <- mimic.to.truncate %>%left_join(select(mimic.target.trial.4.hour0, subject_id, HOSP_los), by = "subject_id")
mimic.to.truncate.hosp.los$HOSP_los[mimic.to.truncate.hosp.los$HOSP_los<0]<-NA
####convert to days
mimic.to.truncate.hosp.los$HOSP_los<-mimic.to.truncate.hosp.los$HOSP_los/24

bootstrap_stats <- function(data, n_bootstrap=1000) {
  replicate(n_bootstrap, {
    # Sample IDs with replacement
    sampled_ids <- sample(unique(data$id), size = length(unique(data$id)), replace = TRUE)
    
    # Get corresponding rows for sampled IDs
    sampled_data <- data %>% filter(id %in% sampled_ids)
    
    # Calculate medians and quantiles for each group
    stats_group1 <- sampled_data %>% filter(ever.intubated == 0) %>% summarize(median = weightedMedian(HOSP_los,weights=weights,na.rm=T), q25 = weightedQuantile(HOSP_los, weights=weights,0.25,na.rm=T), q75 = weightedQuantile(HOSP_los, weights=weights,0.75,na.rm=T))
    stats_group2 <- sampled_data %>% filter(ever.intubated == 1) %>% summarize(median = weightedMedian(HOSP_los,weights=weights,na.rm=T), q25 = weightedQuantile(HOSP_los, weights=weights,0.25,na.rm=T), q75 = weightedQuantile(HOSP_los, weights=weights,0.75,na.rm=T))
    
    # Return results
    list(
      median_1 = stats_group1$median,
      q25_1 = stats_group1$q25,
      q75_1 = stats_group1$q75,
      median_2 = stats_group2$median,
      q25_2 = stats_group2$q25,
      q75_2 = stats_group2$q75,
      diff_median = stats_group1$median - stats_group2$median,
      diff_q25 = stats_group1$q25 - stats_group2$q25,
      diff_q75 = stats_group1$q75 - stats_group2$q75
    )
  }, simplify = "data.frame")
}

# Get bootstrapped statistics
boot_results <- bootstrap_stats(mimic.to.truncate.hosp.los)
median_1<-unlist(boot_results[1,])
q25_1<-unlist(boot_results[2,])
q75_1<-unlist(boot_results[3,])
median_2<-unlist(boot_results[4,])
q25_2<-unlist(boot_results[5,])
q75_2<-unlist(boot_results[6,])
diff_median<-unlist(boot_results[7,])
diff_q25<-unlist(boot_results[8,])
diff_q75<-unlist(boot_results[9,])
results<-cbind(median_1,q25_1,q75_1,median_2,q25_2,q75_2,diff_median,diff_q25,diff_q75)
summarized<-apply(results,2,mean)
summarized
-mean(diff_median)-1.96*sd(diff_median)
-mean(diff_median)+1.96*sd(diff_median)
