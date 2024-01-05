####low ROX and low SF
low.rox<-mimic.to.truncate%>%filter(ROX<=4.88)
surv_object.truncated <- with(low.rox, Surv(time.to.death, mortality.1y.censored == 1))
cox_model.truncated <- coxph(surv_object.truncated ~ invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,cluster=id,data = low.rox,robust=T)
summary(cox_model.truncated)
low.sf<-mimic.to.truncate%>%filter(SaFi<=100)
surv_object.truncated <- with(low.sf, Surv(time.to.death, mortality.1y.censored == 1))
cox_model.truncated <- coxph(surv_object.truncated ~ invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,cluster=id,data = low.sf,robust=T)
summary(cox_model.truncated)
high.sf<-mimic.to.truncate%>%filter(SaFi>100)
surv_object.truncated <- with(high.sf, Surv(time.to.death, mortality.1y.censored == 1))
cox_model.truncated <- coxph(surv_object.truncated ~ invasive+ age+elixhauser+RR+SaFi+fio2.final+gcs+vasopressor+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min+first_careunit,weights=weights,cluster=id,data = high.sf,robust=T)
summary(cox_model.truncated)

####by units####
###micu
micu<-mimic.to.truncate%>%filter(first_careunit=="Medical Intensive Care Unit (MICU)")
surv_object.truncated <- with(micu, Surv(time.to.death, mortality.1y.censored == 1))
cox_model.truncated <- coxph(surv_object.truncated ~ invasive+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = micu,robust=T)
summary(cox_model.truncated)

cov<-c("age","elixhauser","RR","SaFi","fio2.final","gcs","vasopressor","SBP","DBP","MBP","HR","temp","bilirubin_max","creatinine_max","platelet_min")
cov2<-c("age","elixhauser","RR","SaFi","fio2.final","gcs","vasopressor")

aipw.mortality.lr<-AIPW$new(Y=as.numeric(micu$mortality.1y.censored)-1,A=as.numeric(micu$invasive)-1,W.Q=subset(micu,select=cov),W.g=subset(micu,select=cov2),Q.SL.library="SL.glm",g.SL.library="SL.glm",verbose=T,k_split=10)
aipw.mortality.lr$fit()
aipw.mortality.lr$plot.p_score()
suppressWarnings({
  aipw.mortality.lr$stratified_fit()$summary()
})

surv_object.truncated.30d <- with(micu, Surv(time.fu, mortality.30d.censored == 1))
# Fit a weighted Cox proportional hazards model
cox_model.truncated.30d <- coxph(surv_object.truncated.30d ~ invasive+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,cluster=id,weights=weights,data = micu,robust=T)
summary(cox_model.truncated.30d)

####msicu
msicu<-mimic.to.truncate%>%filter(first_careunit=="Medical/Surgical Intensive Care Unit (MICU/SICU)")
surv_object.truncated <- with(msicu, Surv(time.to.death, mortality.1y.censored == 1))
cox_model.truncated <- coxph(surv_object.truncated ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = msicu,robust=T)
summary(cox_model.truncated)

surv_object.truncated.30d <- with(msicu, Surv(time.fu, mortality.30d.censored == 1))
# Fit a weighted Cox proportional hazards model
cox_model.truncated.30d <- coxph(surv_object.truncated.30d ~ invasive+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,cluster=id,weights=weights,data = msicu,robust=T)
summary(cox_model.truncated.30d)

aipw.mortality.lr<-AIPW$new(Y=as.numeric(msicu$mortality.1y.censored)-1,A=as.numeric(msicu$invasive)-1,W.Q=subset(msicu,select=cov),W.g=subset(msicu,select=cov2),Q.SL.library="SL.glm",g.SL.library="SL.glm",verbose=T,k_split=10)
aipw.mortality.lr$fit()
aipw.mortality.lr$plot.p_score()
suppressWarnings({
  aipw.mortality.lr$stratified_fit()$summary()
})

####cicu
cicu<-mimic.to.truncate%>%filter(first_careunit=="Coronary Care Unit (CCU)")
surv_object.truncated <- with(cicu, Surv(time.to.death, mortality.1y.censored == 1))
cox_model.truncated <- coxph(surv_object.truncated ~ invasive+ SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,weights=weights,cluster=id,data = cicu,robust=T)
summary(cox_model.truncated)

surv_object.truncated.30d <- with(cicu, Surv(time.fu, mortality.30d.censored == 1))
# Fit a weighted Cox proportional hazards model
cox_model.truncated.30d <- coxph(surv_object.truncated.30d ~ invasive+SBP+DBP+MBP+HR+temp+bilirubin_max+creatinine_max+platelet_min,cluster=id,weights=weights,data = cicu,robust=T)
summary(cox_model.truncated.30d)

aipw.mortality.lr<-AIPW$new(Y=as.numeric(cicu$mortality.1y.censored)-1,A=as.numeric(cicu$invasive)-1,W.Q=subset(cicu,select=cov),W.g=subset(cicu,select=cov2),Q.SL.library="SL.glm",g.SL.library="SL.glm",verbose=T,k_split=10)
aipw.mortality.lr$fit()
aipw.mortality.lr$plot.p_score()
suppressWarnings({
  aipw.mortality.lr$stratified_fit()$summary()
})