
##################################################
#########starting to create the population
###########Let's keep patients from the medical/surgical ICU
target.trial.proof<-target.trial.proof%>%filter(first_careunit %in%c("Medical/Surgical Intensive Care Unit (MICU/SICU)","Medical Intensive Care Unit (MICU)" ))
length(unique(target.trial.proof$id))
quantile(target.trial.proof$SaFi,na.rm=T)
target.trial.intubated<-target.trial.proof%>%filter(ever.intubated==1)
length(unique(target.trial.intubated$id))


##########1st STEP
intermediate.proof<-target.trial.proof%>%group_by(id)%>% #####we can use the general dataframe since we will then filter for condition and all observations at time 0 will be removed (lagged so NA)
  mutate(invasive_cumsum = cumsum(invasive)) %>% 
  filter(!(invasive == 1 & invasive_cumsum > 1)) %>%
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
#####SaFi 88-200, RR <=40, GCS>=12

#####Here, we keep only the observations that meet the criteria for the target trial in the first 24 hours after admission
#####However, these patients must not fulfill an exclusion criteria before
# create a new variable that accumulates the number of times the exclusion criteria has been met by subject and hour
intermediate.proof <- intermediate.proof %>%
  group_by(id,hour) %>%
  mutate(exclusion_met = cumsum((SaFi<88|SaFi>200|RR>39|gcs<12)))%>%ungroup()

intermediate.final.proof <- intermediate.proof %>%
  filter(exclusion_met == 0)
length(unique(intermediate.final.proof$id))
#####how many intubated without exclusion criteria
intubated.only.no.exclusion.proof<-intermediate.final.proof %>%
  filter(ever.intubated == 1)
length(unique(intubated.only.no.exclusion$id))
dim(intubated.only.no.exclusion)
#####1000 patients
quantile(intubated.only.no.exclusion.proof$SaFi,na.rm=T)
sum(is.na(intubated.only.no.exclusion$SaFi))

length(unique(intermediate.final.proof$id)) 
####12000 patients without exclusion criteria.

#####Identify those with inclusion criteria
quantile(intermediate.final.proof$SaFi,na.rm=T)
inclusion.proof<-intermediate.final.proof %>% 
  filter(
    hour <= 24,
    SaFi <=200 & spO2<=97
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
    hour >24,
    SaFi<=200 & spO2<=97
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
dim(intubated.proof)####731 patients intubated out of 2100 patients
min(target.unimputed.proof$hour) ####No observations from hour 0.

####finally, we need to remove observations due to patients not being in the icu (dead/discharged)
target.unimputed.proof <- target.unimputed.proof %>%
  mutate(time_diff = los*24)
####remove lines that have time_diff < hr
target.unimputed.proof<-target.unimputed.proof%>%filter(time_diff>=hour)
dim(target.unimputed.proof) ####we retain roughly 28000 observations from 2100 patients 
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
length(unique_ids)
length(unique(target.unimputed.proof$id))
####roughly 1047 deaths out of 2200 patients
target.unimputed.proof <- target.unimputed.proof %>%
  group_by(id) %>%
  mutate(first_date = min(starttime, na.rm = TRUE))
target.unimputed.proof$first_date<-as.Date(target.unimputed.proof$first_date)
target.unimputed.proof$time.to.death<-difftime(target.unimputed.proof$dod,target.unimputed.proof$first_date,units="days")
target.unimputed.proof$time.to.death[target.unimputed.proof$time.to.death<0]<-NA

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
target.to.impute.proof$vasopressor<-as.factor(target.to.impute.proof$vasopressor)

####Table at eligibility
target.to.impute.proof<-target.to.impute.proof%>%group_by(id)%>%mutate(ever.intubated= ifelse(any(invasive == 1), 1, 0))%>%ungroup()
eligibility<-target.to.impute.proof%>%filter(reset.hour==1)
dependent1<-eligibility$ever.intubated
eligibility$ever.intubated<-as.factor(eligibility$ever.intubated)
explanatory1<-names(eligibility[,c(9:11,13,20,21,22,24,26:30,37,47:52)])
eligibility%>% 
  summary_factorlist("ever.intubated",explanatory1, p=TRUE)


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

target.to.impute.proof$vasopressor<-as.integer(target.to.impute.proof$vasopressor)
target_imp<-subset(target.to.impute.proof[,c(13,20,21,22:24,26:29,30,31,48,49,50)])
imp_mat.mimic<-mice(target_imp,m=5)
Ind.Na11.imput.mimic = apply(is.na(target_imp),2,which)
median.imputed.data.mimic= target_imp
for(i in 1:length(median.imputed.data.mimic)){
  aa2.mimic = matrix(unlist(imp_mat.mimic$imp[i]),ncol=5,byrow = F)
  median.imputed.data.mimic[unlist(Ind.Na11.imput.mimic[i]),i] = apply(aa2.mimic,1,median)
}
mimic.imputed.final<-cbind(target.to.impute.proof[,-c(13,20,20,21,22:24,26:29,30,31,48,49,50)],median.imputed.data.mimic)
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
explanatory.final<-names(mimic.imputed.final[,c(25,57,56,9:11,49,35,36,47,37,46,58:60,51:55)])
eligibility.after%>% 
  summary_factorlist("ever.intubated",explanatory.final, p=TRUE)
write_xlsx(eligibility.after%>% 
             summary_factorlist("ever.intubated", explanatory.final, p=TRUE),path ="baseline.table.mimic.xlsx")
length(unique(mimic.imputed.final$id))
sum(eligibility.after$ever.intubated==1)
intubated<-mimic.imputed.final%>%filter(invasive==1)
quantile(intubated$hour)
##################IPTW
mimic.imputed.final.target<-mimic.imputed.final
colSums(is.na(mimic.imputed.final.target))
mimic.imputed.final.target$invasive<-as.numeric(mimic.imputed.final.target$invasive)
mimic.imputed.final.target$subject_id<-as.numeric(mimic.imputed.final.target$subject_id)
mimic.imputed.final.target$reset.hour<-as.numeric(mimic.imputed.final.target$reset.hour)
mimic.imputed.final.target<-as.data.frame(mimic.imputed.final.target)
mimic.imputed.final.target$invasive<-as.factor(mimic.imputed.final.target$invasive)
levels(mimic.imputed.final.target$invasive)<-c(0,1)
mimic.imputed.final.target$time.to.death[is.na(mimic.imputed.final.target$time.to.death)]<-365
mimic.imputed.final.target$mortality.1y.censored<-ifelse(mimic.imputed.final.target$time.to.death>365,NA,as.factor(mimic.imputed.final.target$mortality.1y))
mimic.imputed.final.target$mortality.1y.censored<-as.factor(mimic.imputed.final.target$mortality.1y.censored)
levels(mimic.imputed.final.target$mortality.1y.censored)<-c(0,1)
mimic.imputed.final.target$mortality.1y.censored[is.na(mimic.imputed.final.target$mortality.1y.censored)]<-0
mimic.imputed.final.target$time.to.death[mimic.imputed.final.target$time.to.death>365]<-365


set.seed(123)
mimic.imputed.final.target<-as.data.frame(mimic.imputed.final.target)
ps_model<-ipwtm(exposure=invasive,family="binomial",link="logit",numerator = ~1+age+elixhauser,denominator=~reset.hour+gcs+age+elixhauser+ROX+RR+fio2.final+spO2+vasopressor,id=id,type="first",timevar = reset.hour,data=mimic.imputed.final.target)
mimic.imputed.final.target$weights<-ps_model$ipw.weights
quantile(mimic.imputed.final.target$weights)

#######weights accross time
weight.plot<-ggplot(mimic.imputed.final.target,aes(x=as.factor(reset.hour),y=weights))+geom_boxplot()
weight.plot

#####################TRUNCATION
########Perform some weight truncation
mimic.to.truncate<-mimic.imputed.final.target
mimic.to.truncate$weights[mimic.to.truncate$weights>quantile(mimic.to.truncate$weights,0.95)]<-quantile(mimic.to.truncate$weights,0.95)
quantile(mimic.to.truncate$weights)

#######weights accross time
weight.plot.truncated<-ggplot(mimic.to.truncate,aes(x=as.factor(reset.hour),y=weights))+geom_boxplot()
weight.plot.truncated

#####MAIN ANALYSIS
#####SURVIVAL OBJECT
set.seed(123)
surv_object.truncated <- with(mimic.to.truncate, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated <- coxph(surv_object.truncated ~ invasive + age+elixhauser+RR+ROX+SaFi+vasopressor+fio2.final+temp+SBP+gcs+DBP+MBP+HR+bilirubin_max+creatinine_max+platelet_min+spO2,data = mimic.to.truncate, weights = weights,robust=T)
summary(cox_model.truncated)

###we fit a stratified object to use it for the KM
cox_model.truncated.strata <- coxph(surv_object.truncated ~ strata(invasive) + age+elixhauser+RR+ROX+SaFi+vasopressor+fio2.final+temp+SBP+gcs+DBP+MBP+HR+bilirubin_max+creatinine_max+platelet_min+spO2, data = mimic.to.truncate, weights = weights,robust=T)
summary(cox_model.truncated.strata)

km_model.truncated <- survfit(cox_model.truncated.strata, data = mimic.to.truncate)

####KM
# Plot Kaplan-Meier curves
KM.plot.truncated<-ggsurvplot(km_model.truncated,data=mimic.to.truncate,pval="Hazard ratio: 0.81, 95% CI 0.65-1.01, p= 0.06",censor=F,risk.table = T,legend.labs=c("Intubation = No", "Intubation = Yes"),title = "Kaplan-Meier Curves by Intubation Group",xlab = "Time",
                              ylab = "Survival Probability",pval.coord = c(100,0.3))
KM.plot.truncated$plot+geom_vline(aes(xintercept = 28), linetype = "dashed", color = "blue") +
  geom_vline(aes(xintercept = 60), linetype = "dashed", color = "red")+scale_x_continuous(breaks = c(0,90,365),expand = c(0,0))


###crude mortality
ever.intubated<-mimic.to.truncate%>%filter(invasive==1)
sum(ever.intubated$mortality.1y.censored==1) 
sum(ever.intubated$mortality.1y.censored==1) /dim(ever.intubated)[1]
never.intubated<-mimic.to.truncate%>%filter(ever.intubated==0)
sum(never.intubated$mortality.1y.censored==1) 
sum(never.intubated$mortality.1y.censored==1) /dim(never.intubated)[1]####crude mortality


#####ICU LOS
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
mimic.target.trial.dod.minus1<-mimic.target.trial.dod.minus1%>% arrange(desc(subject_id), hr)
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