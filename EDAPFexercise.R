setwd("C:/Users/roseg/practicefusion")



library(tidyverse)

set.seed(33) 

Patients <-read.csv(".\\data\\training_SyncPatient.csv")
Medications <- read.csv(".\\data\\training_SyncMedication.csv")
Diagnosis  <- read.csv(".\\data\\training_SyncDiagnosis.csv")
LabPanel  <- read.csv(".\\data\\training_SyncLabPanel.csv")
LabResult  <- read.csv(".\\data\\training_SyncLabResult.csv")
Labs  <- read.csv(".\\data\\training_SyncLabObservation.csv")
Prescriptions <- read.csv(".\\data\\training_SyncPrescription.csv")
Transcriptions <- read.csv(".\\data\\training_SyncTranscript.csv")


uniquemedications <- Medications %>% 
  count(NdcCode,MedicationName) %>%
  arrange(desc(n))

### This is an usually high number of antibiotics. 
### This does not match NHANES or more other databases.


uniquediagnosis <- Diagnosis %>% 
  count(ICD9Code,DiagnosisDescription) %>%
  arrange(desc(n))


## Hypothyroidism is a frequent disease 
## while the usual suspects of high BP, high cholesterol etc 
## are the frequent uses of medications , along with 
## bacterial infections which are broadly applicable 



## Let's build a list of  hypothyroid patients 

hypothyroidpropatients <- Diagnosis  %>%
  filter(str_detect(DiagnosisDescription,fixed('hypothyroid', ignore_case=FALSE)))  %>%
  distinct(PatientGuid)

nrow(hypothyroidpropatients)

nrow(Patients)
percenthypothyroid <- nrow(hypothyroidpropatients)/nrow(Patients)*100
percenthypothyroid

#11.08% hyperthyroid patients. This is higher than national average
#of approx 5% (Source: NHANES, Research literature)

mean(Patients$YearOfBirth)

## mean YoB is 1957 which means this overall population is older
## than national average 

Transcriptions %>% 
  count(PhysicianSpecialty)%>%
  arrange(desc(n))

##High percentage of geriatric/cardiology , is that why population 
## is older? 


#############################################################
## Let's look at what goes on with hypothyroid patients ,
## labs, medications, comorbid conditions 
#############################################################

#labs
hypothyroidlabs <- hypothyroidpropatients %>% 
                   inner_join(LabResult,by='PatientGuid')  %>%
                   select(PatientGuid,LabResultGuid) %>%
                   inner_join(LabPanel,by='LabResultGuid') %>%
                   select(PatientGuid,LabResultGuid,LabPanelGuid) %>%
                   inner_join(Labs,by='LabPanelGuid')

## We actually have few labs per patient
## We don't even have TSH, T4, T3 for most patients. Why ? 
## Where are these labs? 

## What do we examine most often for hypothyroid patients? 
uniquelabshypo <- hypothyroidlabs %>% 
  count(HL7Text) %>%
  arrange(desc(n))

uniquelabshypo

## This is really interesting. Protein is the most requested 
## lab for hypothyroid patients. We have over 1000 hypothyroid patients
## Yet only 18 TSH tests - the Standard of Care test
## We have 62 Thyroxine tests. This ratio seems off.
## Perhaps doctors are testing T4 instead of TSH in most cases?
## This seems wrong. 
## Also low number of chol tests (of any type) and where is A1C
## Not sure this can be right? 

#Meds 
hypothyroidmedications <- hypothyroidpropatients %>%
                          inner_join(Medications,by='PatientGuid')%>%
                          count(MedicationName)%>%
                          arrange(desc(n))


##Comorbid conditions 
hypothyroidcountdiagnosis <- hypothyroidpropatients %>%
  inner_join(Diagnosis,by='PatientGuid')%>%
  count(DiagnosisDescription)%>%
  arrange(desc(n))


## Let's try to sequence the diagnoses for hypothyroid patients
## so we have some idea about cause and effect
hypothyroiddiagnosis <- hypothyroidpropatients %>%
  inner_join(Diagnosis,by='PatientGuid')%>%
  arrange(PatientGuid,StartYear)

## We don't really have longitundinal data
## 2009 to 2012 doesn't give us much to work with

##Demographics 
hypothyroidemo <- hypothyroidpropatients %>%
  inner_join(Patients,by='PatientGuid')

summary(hypothyroidemo$Gender)

#mainly women. Matches national data  

summary(hypothyroidemo$YearOfBirth)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1922    1939    1951    1951    1963    1993 

# mainly an older person's disease. Matches national data

# Heights in different units , leading to invalid BMI 
# Blood Pressure has weird values like 1 
# Temps - lots of low grade fevers, Some really weird entries. 
# These data do not look very reliable . Not sure  of provenace.
# I wonder if based on the .csv file name if these were generated 
# from NLP model. Low confidence for building a model using this data

# The relationships between diagnosis, patient, labs, medication
# look the most promising



###################################################
#Let's build a record for hypothyroid patients that 
#includes their relevent history events as words 
#then we can try to build a topic modelor look 
# for disease constellations. 
# Doing this as NLP to demonstrate NLP skills 
##################################################


#First lets pull out all the diagnosis text and examine
#Note: We could do this using ICD codes BUT 
#using the description will allow us to understand what is 
#happening. It also allows us to understand different names for 
#similiar diseases, or where there are multiple possible codes


HTDText <- hypothyroiddiagnosis %>%
          group_by(PatientGuid) %>% 
          filter(!str_detect(DiagnosisDescription,fixed('hypothyroid', ignore_case=FALSE)))  %>%
          mutate(MedicalText = paste0(DiagnosisDescription, collapse = "."))%>%
          distinct(PatientGuid,MedicalText)


## I want to create 2 datasets so we can understand how useful 
## this classifier might be
## We will remove any mention of "hypothyroidism" from 
## 1 dataset, so we can see if disease contellations 
## are useful
## We will create a second dataset that includes 
## "hypothyroidism". If a language model like this 
## has any value it should be able to see the "hypothyroidism"
## in the text and classify as a positive result. We should get 
## a good result for a classifier if language model is 
## a decent approach. 

ThyDiagOmitHypo <- hypothyroiddiagnosis %>%
                   group_by(PatientGuid) %>% 
                   filter(!str_detect(DiagnosisDescription,fixed('hypothyroid', ignore_case=FALSE)))  %>%
                   mutate(MedicalText = paste0(DiagnosisDescription, collapse = "."))%>%
                   distinct(PatientGuid,MedicalText)%>%
                   ungroup()

ThyDiagHypo <- hypothyroiddiagnosis %>%
  group_by(PatientGuid) %>% 
  mutate(MedicalText = paste0(DiagnosisDescription, collapse = "."))%>%
  distinct(PatientGuid,MedicalText)%>%
  ungroup()

Thylabs <- hypothyroidpropatients %>% 
  select(PatientGuid)%>%
  inner_join(LabResult,by='PatientGuid')  %>%
  select(PatientGuid,LabResultGuid) %>%
  inner_join(LabPanel,by='LabResultGuid') %>%
  select(PatientGuid,LabResultGuid,LabPanelGuid) %>%
  inner_join(Labs,by='LabPanelGuid')%>%
  group_by(PatientGuid) %>% 
  mutate(MedicalTextL = paste0(HL7Text, collapse = ".")) %>%
  distinct(PatientGuid,MedicalTextL)%>%
  select(PatientGuid,MedicalTextL)




## Now lets create a comparison dataset of other patients
## that are neg for hypothyroidism
## Let's try a balanced dataset - oversample patients
## knowing that I will need to drop some that by chance are 
## hypothyroid
negativepatients <- slice_sample(Patients,n=1200) %>% 
                    select('PatientGuid')%>%
                    inner_join(Diagnosis,by='PatientGuid')%>%
                    group_by(PatientGuid) %>% 
                    mutate(MedicalText = paste0(DiagnosisDescription, collapse = "."))%>%
                    distinct(PatientGuid,MedicalText)%>%
                    filter(!str_detect(MedicalText,fixed('hypothyroid', ignore_case=FALSE))) 
  
                    
negativelabs <- negativepatients %>% 
                select(PatientGuid)%>%
                inner_join(LabResult,by='PatientGuid')  %>%
                select(PatientGuid,LabResultGuid) %>%
                inner_join(LabPanel,by='LabResultGuid') %>%
                select(PatientGuid,LabResultGuid,LabPanelGuid) %>%
                inner_join(Labs,by='LabPanelGuid')%>%
                group_by(PatientGuid) %>% 
                mutate(MedicalTextL = paste0(HL7Text, collapse = ".")) %>%
                distinct(PatientGuid,MedicalTextL)%>%
                select(PatientGuid,MedicalTextL)


### Ok , join labs and diagnosis text
### for negative patients and set the target label to 0
MEDNEGTXT <- negativepatients %>%
  left_join(negativelabs)%>%
  group_by(PatientGuid) %>% 
  mutate(MedicalText = paste0(MedicalText,(ifelse(is.na(MedicalTextL),"",MedicalTextL)), collapse = ".")) %>%
  distinct(PatientGuid,MedicalText)%>%
  mutate(Y_Label = 0)%>%
  select(Y_Label,MedicalText)

 # 
 # THLText <- negativepatients  %>%
 #            select(PatientGuid)%>%
 #            inner_join(Labs)%>%
 #            group_by(PatientGuid) %>% 
 #            mutate(MedicalTextL = paste0(HL7Text, collapse = ".")) %>%
 #            distinct(PatientGuid,MedicalTextL)

### Now create my positive dataset
           
           
THYTXTnohypo <- ThyDiagOmitHypo %>%
                left_join(Thylabs )%>%
                group_by(PatientGuid) %>% 
                mutate(MedicalText = paste0(MedicalText,(ifelse(is.na(MedicalTextL),"",MedicalTextL)), collapse = ".")) %>%
                distinct(PatientGuid,MedicalText)%>%
                mutate(Y_Label = 1)%>%
                select(Y_Label,MedicalText)


THYTXThypo <- ThyDiagHypo %>%
              left_join(Thylabs)%>%
              group_by(PatientGuid) %>%
              mutate(MedicalText = paste0(MedicalText,(ifelse(is.na(MedicalTextL),"",MedicalTextL)), collapse = ".")) %>%
              distinct(PatientGuid,MedicalText)%>%
              mutate(Y_Label = 1)%>%
              select(Y_Label,MedicalText)

InHypo <-rbind(THYTXThypo,MEDNEGTXT)%>%
                select(Y_Label,MedicalText)
#randomize before we test/train split 
InHypo <- InHypo[sample(nrow(InHypo)),]

# Hold back 100 patients that for test above validation
split_dummy <- sample(c(rep(0, (nrow(InHypo)-100)),  # Create dummy for splitting
                        rep(1, 100)))

PFModelInHypo <- InHypo[split_dummy == 0, ]
reservedTestHypo <- InHypo[split_dummy == 1, ] 

  
InOmitHypo<-rbind(THYTXTnohypo,MEDNEGTXT)%>%
                  select(Y_Label,MedicalText)
#randomize before we test/train split 
InOmitHypo <- InOmitHypo[sample(nrow(InOmitHypo)),]

# Hold back 100 patients that for test above validation
split_dummy <- sample(c(rep(0, (nrow(InOmitHypo)-100)),  # Create dummy for splitting
                        rep(1, 100)))

PFModelInOmitHypo <- InOmitHypo[split_dummy == 0, ]
reservedTestOmitHypo <- InOmitHypo[split_dummy == 1, ] 


#PFModelInOmitHypo<-rbind(THYTXTnohypo,MEDNEGTXT)%>%
#                   select(Y_Label,MedicalText)


write.csv(PFModelInHypo,'PFModelInHypo.csv')
write.csv(reservedTestHypo,'TestInHypo.csv')

write.csv(PFModelInOmitHypo,'PFModelInOmitHypo.csv')
write.csv(reservedTestOmitHypo,'TestInOmitHypo.csv')



##############################################
# Language Model input 
# 
## now create bigger dataset containing all of the 
## diagnostic text, Labs text 
## 
##############################################

Dtext <- Diagnosis %>%
  mutate(MedicalText = DiagnosisDescription)%>%
  select(MedicalText)

Dtext <- Dtext[sample(nrow(Dtext)),] # shuffle for the LM 


write.csv(Dtext,'Dtext.csv')

Mtext <- Medications %>%
  mutate(MedicalText = MedicationName)%>%
  select(MedicalText) %>%
  slice_sample(n=10000)

Mtext <- Mtext[sample(nrow(Mtext)),] # shuffle for the LM 

write.csv(Mtext,'Mtext.csv')

Ltext <- Labs %>%
  mutate(MedicalText = HL7Text)%>%
  select(MedicalText) 
Ltext <- Ltext[sample(nrow(Ltext)),] # shuffle for the LM 


write.csv(Ltext,'Ltext.csv')


##################################################
## Medications for hypothyroidism                #
## High use of lexapro in hypothyroid patients   #
## If we had labs we could establish cause/effect#
##################################################
  

uniquemedicationsLevo <- Medications %>% 
                         filter(str_detect(MedicationName,fixed('Levothyroxine', ignore_case=FALSE))) %>% 
                         count(NdcCode,MedicationName) %>%
                         arrange(desc(n))

uniquemedicationsLexa <- Medications %>% 
  filter(str_detect(MedicationName,fixed('escitalopram', ignore_case=FALSE))) %>% 
  count(NdcCode,MedicationName) %>%
  arrange(desc(n))

levopropatients <- Medications  %>%
                   filter(str_detect(MedicationName,fixed('levothyroxine', ignore_case=FALSE)))  


lexapropatients <- Medications  %>%
  filter(str_detect(MedicationName,fixed('escitalopram', ignore_case=FALSE)))  

diagnosisforLexaproPatients <- lexapropatients %>%
                               left_join(Diagnosis, by = 'PatientGuid')
uniquelexadiagnoses <- diagnosisforLexaproPatients %>% 
                   #distinct(ICD9Code)%>% 
                   count(ICD9Code,DiagnosisDescription) %>%
                   arrange(desc(n))

diagnosisforLevoPatients <- levopropatients %>%
  left_join(Diagnosis, by = 'PatientGuid')

uniquelevodiagnoses <- diagnosisforLevoPatients %>% 
  count(ICD9Code,DiagnosisDescription) %>%
  arrange(desc(n))


