#####hour 1#####
mimic.prova<-mimic.to.truncate%>%filter(reset.hour==1)
table(mimic.prova$invasive)
mimic.prova<-mimic.prova%>%mutate(mortality.30d.censored=ifelse(time.to.death<=30,1,0),time.fu=ifelse(time.to.death<=30,time.to.death,30))


###one-year mortality
surv_object.truncated.prova <- with(mimic.prova, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
set.seed(123)
cox_model.truncated.prova <- coxph(surv_object.truncated.prova ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova)

####30-day mortality
surv_object.truncated.prova.30 <- with(mimic.prova, Surv(time.fu, mortality.30d.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova.30 <- coxph(surv_object.truncated.prova.30 ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova.30)
#######

#####hour 2 to 6#######
mimic.prova<-mimic.to.truncate%>%filter(reset.hour>1&reset.hour<7)
table(mimic.prova$invasive)
mimic.prova<-mimic.prova%>%mutate(mortality.30d.censored=ifelse(time.to.death<=30,1,0),time.fu=ifelse(time.to.death<=30,time.to.death,30))
###one-year mortality
set.seed(123)
surv_object.truncated.prova <- with(mimic.prova, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova <- coxph(surv_object.truncated.prova ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova)

####30-day mortality
surv_object.truncated.prova.30 <- with(mimic.prova, Surv(time.fu, mortality.30d.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova.30 <- coxph(surv_object.truncated.prova.30 ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova.30)
###########


#####hour 7 to 12#######
mimic.prova<-mimic.to.truncate%>%filter(reset.hour>6&reset.hour<13)
table(mimic.prova$invasive)
mimic.prova<-mimic.prova%>%mutate(mortality.30d.censored=ifelse(time.to.death<=30,1,0),time.fu=ifelse(time.to.death<=30,time.to.death,30))
set.seed(123)

####one-year mortality
surv_object.truncated.prova <- with(mimic.prova, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova <- coxph(surv_object.truncated.prova ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova)

####30-day mortality
surv_object.truncated.prova.30 <- with(mimic.prova, Surv(time.fu, mortality.30d.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova.30 <- coxph(surv_object.truncated.prova.30 ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova.30)
#########

#####hour 13 to 24########
mimic.prova<-mimic.to.truncate%>%filter(reset.hour>12&reset.hour<25)
table(mimic.prova$invasive)
mimic.to.truncate<-mimic.to.truncate%>%mutate(mortality.30d.censored=ifelse(time.to.death<=30,1,0),time.fu=ifelse(time.to.death<=30,time.to.death,30))


###one-year mortality
set.seed(123)
surv_object.truncated.prova <- with(mimic.prova, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova <- coxph(surv_object.truncated.prova ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova)

####30-day mortality
surv_object.truncated.prova.30 <- with(mimic.prova, Surv(time.fu, mortality.30d.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova.30 <- coxph(surv_object.truncated.prova.30 ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova.30)
################

#####hour 25 to 48#######
mimic.prova<-mimic.to.truncate%>%filter(reset.hour>24&reset.hour<48)
table(mimic.prova$invasive)
mimic.prova<-mimic.prova%>%mutate(mortality.30d.censored=ifelse(time.to.death<=30,1,0),time.fu=ifelse(time.to.death<=30,time.to.death,30))

####one.year mortality
surv_object.truncated.prova <- with(mimic.prova, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova <- coxph(surv_object.truncated.prova ~ invasive+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova)

####30-day mortality
surv_object.truncated.prova.30 <- with(mimic.prova, Surv(time.fu, mortality.30d.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova.30 <- coxph(surv_object.truncated.prova.30 ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova.30)
############


mimic.prova<-mimic.to.truncate%>%filter(reset.hour==2)
table(mimic.prova$invasive)
mimic.prova$mortality.30d.censored<-ifelse(mimic.prova$time.to.death>30,0,1)
mimic.prova$mortality.30d.censored<-as.factor(mimic.prova$mortality.30d.censored)
levels(mimic.prova$mortality.30d.censored)<-c(0,1)
set.seed(123)
glm(mortality.30d.censored~invasive,data=mimic.prova,family="binomial")
glm(mortality.30d.censored~invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,data=mimic.prova,family="binomial")

surv_object.truncated.prova <- with(mimic.prova, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova <- coxph(surv_object.truncated.prova ~ invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova)


mimic.prova<-mimic.to.truncate%>%filter(reset.hour==3)
table(mimic.prova$invasive)
mimic.prova$mortality.30d.censored<-ifelse(mimic.prova$time.to.death>30,0,1)
mimic.prova$mortality.30d.censored<-as.factor(mimic.prova$mortality.30d.censored)
levels(mimic.prova$mortality.30d.censored)<-c(0,1)
set.seed(123)
glm(mortality.30d.censored~invasive,data=mimic.prova,family="binomial")
glm(mortality.30d.censored~invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,data=mimic.prova,family="binomial")

surv_object.truncated.prova <- with(mimic.prova, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova <- coxph(surv_object.truncated.prova ~ invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova)


mimic.prova<-mimic.to.truncate%>%filter(reset.hour==4)
table(mimic.prova$invasive)
mimic.prova$mortality.30d.censored<-ifelse(mimic.prova$time.to.death>30,0,1)
mimic.prova$mortality.30d.censored<-as.factor(mimic.prova$mortality.30d.censored)
levels(mimic.prova$mortality.30d.censored)<-c(0,1)
set.seed(123)
glm(mortality.30d.censored~invasive,data=mimic.prova,family="binomial")
glm(mortality.30d.censored~invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,data=mimic.prova,family="binomial")

surv_object.truncated.prova <- with(mimic.prova, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova <- coxph(surv_object.truncated.prova ~ invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova)

mimic.prova<-mimic.to.truncate%>%filter(reset.hour==5)
table(mimic.prova$invasive)
mimic.prova$mortality.30d.censored<-ifelse(mimic.prova$time.to.death>30,0,1)
mimic.prova$mortality.30d.censored<-as.factor(mimic.prova$mortality.30d.censored)
levels(mimic.prova$mortality.30d.censored)<-c(0,1)
set.seed(123)
glm(mortality.30d.censored~invasive,data=mimic.prova,family="binomial")
glm(mortality.30d.censored~invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,data=mimic.prova,family="binomial")

surv_object.truncated.prova <- with(mimic.prova, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova <- coxph(surv_object.truncated.prova ~ invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova)

mimic.prova<-mimic.to.truncate%>%filter(reset.hour==6)
table(mimic.prova$invasive)
mimic.prova$mortality.30d.censored<-ifelse(mimic.prova$time.to.death>30,0,1)
mimic.prova$mortality.30d.censored<-as.factor(mimic.prova$mortality.30d.censored)
levels(mimic.prova$mortality.30d.censored)<-c(0,1)
set.seed(123)
glm(mortality.30d.censored~invasive,data=mimic.prova,family="binomial")
glm(mortality.30d.censored~invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,data=mimic.prova,family="binomial")

surv_object.truncated.prova <- with(mimic.prova, Surv(time.to.death, mortality.1y.censored == 1))

# Fit a weighted Cox proportional hazards model
cox_model.truncated.prova <- coxph(surv_object.truncated.prova ~ invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,cluster=id,data = mimic.prova,robust=T)
summary(cox_model.truncated.prova)


