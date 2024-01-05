#####Rename the dataset to make it user-friendly

'%!in%' <- function(x,y)!('%in%'(x,y))

#### Connect to BigQuery#####
bq_auth(use_oob = TRUE)

projectid = "physionet-387311"  
bigrquery::bq_auth() #login with google account associated with physionet account

sql <- "SELECT *
FROM `physionet-387311.mv_icu_720h.final_dec23_short`"

bq_data <- bq_project_query(x=projectid,query = sql)

df.mimic.dec23 = bq_table_download(bq_data)

target.trial<-df.mimic.dec23 ####df.mimic comes from bigquery
dim(target.trial)
length(unique(target.trial$subject_id)) ####47000 unique patients
target.trial<-target.trial%>%select(-c("stay_id_1","stay_id_2","subject_id_1","hadm_id_1","hr_1"))

length(unique(target.trial$subject_id)) ####47714 unique patients

#############################################
######Organize the dataframe in descending order
target.trial<-target.trial%>% arrange(desc(subject_id), hr)
target.trial <- target.trial %>%mutate(female = case_when(gender == 'M' ~ 0,
                                                          gender == 'F' ~ 1),
                                       medicare_medicaid = case_when(insurance == 'Medicaid' | insurance == 'Medicare' ~ 1,
                                                                     insurance == 'Other' ~ 0)) %>% 
  rename(id = stay_id,
         hour = hr,
         age = anchor_age,
         HR = heart_rate,
         RR = resp_rate,
         temp = temperature,
         spO2 = spo2,
         fiO2 = fio2,
         glucose = glucose,
         gcs = gcs_min,
         elixhauser = elixhauser_vanwalraven)

#####FiO2
target.trial$fio2.flow<-21+(target.trial$o2_flow*3)
target.trial$fio2.flow[target.trial$fio2.flow>100]<-100
target.trial$fio2.final<-ifelse(target.trial$invasive==1|target.trial$noninvasive==1|target.trial$highflow==1,target.trial$fiO2,target.trial$fio2.flow)

####Aggregate non-invasive and invasive values
target.trial$sbp.final<-apply(target.trial[,c("sbp","sbp_ni")],1,min,na.rm=T)
target.trial$dbp.final<-apply(target.trial[,c("dbp","dbp_ni")],1,min,na.rm=T)
target.trial$mbp.final<-apply(target.trial[,c("mbp","mbp_ni")],1,min,na.rm=T)

target.trial<-target.trial%>%rename(SBP=sbp.final,DBP=dbp.final,MBP=mbp.final)

###SaFi
target.trial$SaFi<-target.trial$spO2/(target.trial$fio2.final/100)
###ROX
target.trial$ROX<-target.trial$SaFi/target.trial$RR

target.trial$SBP[is.infinite(target.trial$SBP)] <- NA
target.trial$DBP[is.infinite(target.trial$DBP)] <- NA
target.trial$MBP[is.infinite(target.trial$MBP)] <- NA

target.trial.proof<-target.trial%>%group_by(id) %>%
  mutate(HR = lag(HR),
         SBP = lag(SBP),
         DBP = lag(DBP),
         MBP = lag(MBP),
         RR = lag(RR),
         temp = lag(temp),
         spO2 = lag(spO2),
         fiO2 = lag(fiO2),
         SaFi= lag(SaFi),
         ROX= lag (ROX),
         glucose = lag(glucose),
         gcs = lag(gcs),
         vasopressor = lag(vasopressor))

target.trial.proof<-target.trial.proof%>%group_by(id)%>%mutate(ever.intubated= ifelse(any(invasive == 1), 1, 0))%>%ungroup()
target.trial.proof<-target.trial.proof%>%group_by(id)%>%mutate(cumulative.invasive=cumsum(invasive))%>%ungroup()
target.trial.proof<-target.trial.proof%>%fill(SaFi,RR,gcs)
target.trial.proof$RR[target.trial.proof$RR<10]<-NA
quantile(target.trial.proof$SaFi,na.rm=T)
target.trial.proof$SaFi<-target.trial.proof$spO2/(target.trial.proof$fio2.final/100)
quantile(target.trial.proof$SaFi,na.rm=T)
target.trial.proof$ROX<-target.trial.proof$SaFi/target.trial.proof$RR
target.trial.proof<-target.trial.proof%>%fill(ROX)
target.trial.proof<-target.trial.proof%>%filter(hour>-1)
length(unique(target.trial.proof$id))