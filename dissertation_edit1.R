rm(list = ls()) 

#Library ####
pacman::p_load(haven, dplyr, ggplot2, GGally, lavaan,
               psych, car, semPlot, semTable, table1, 
               effects, visdat, sjPlot, pROC, lmtest, 
               lme4, srvyr, survey, gtsummary, fmsb)
#####

#Non-Victim Form data ####
csew_nvf_20 <- read_sav("Data/csew_apr19mar20_nvf.sav")
csew_nvf_19 <- read_sav("Data/csew_apr18mar19_nvf.sav")
csew_nvf_18 <- read_sav("Data/csew_apr17mar18_nvf.sav")

#Add rounds 
csew_nvf_20$round <- rep(c("2019/2020"))
head(csew_nvf_20$round)

csew_nvf_19$round <- rep(c("2018/2019"))
tail(csew_nvf_19$round)

csew_nvf_18$round <- rep(c("2017/2018")) 
head(csew_nvf_18$round)  


#Merge datasets from 3 years 
csew_nvf <- bind_rows(csew_nvf_18, csew_nvf_19, csew_nvf_20)

rm("csew_nvf_18", "csew_nvf_19", "csew_nvf_20")
#####

#CFA ####
#Reverse code police attitudes variables 
reverse_cols = c("polatt1", "polatt2", "polatt3", "polatt5", "polatt6",  
                 "polatt6b", "polatt7") 
csew_nvf[ , reverse_cols] = 6 - csew_nvf[ , reverse_cols]

#Numeric 
csew_nvf$polatt1 <- as.numeric(csew_nvf$polatt1)
csew_nvf$polatt2 <- as.numeric(csew_nvf$polatt2)
csew_nvf$polatt3 <- as.numeric(csew_nvf$polatt3)
csew_nvf$polatt5 <- as.numeric(csew_nvf$polatt5)
csew_nvf$polatt6 <- as.numeric(csew_nvf$polatt6)
csew_nvf$polatt6b <- as.numeric(csew_nvf$polatt6b)
csew_nvf$polatt7 <- as.numeric(csew_nvf$polatt7)

#One factor model
trust.scale1 <-  'trust =~  polatt1 + polatt2 + polatt3 + polatt6 + polatt7'

trust.fit1 <- cfa(trust.scale1, data = csew_nvf, std.lv = TRUE,
                  missing = "fiml", mimic = "Mplus") 

summary(trust.fit1, fit.measures = TRUE)

#Two factor model
trust.scale2 <-  'pj =~  polatt2 + polatt3 
                 eff =~ polatt1 + polatt6 + polatt7'

trust.fit2 <- cfa(trust.scale2, data = csew_nvf, std.lv = TRUE,
                 missing = "fiml", mimic = "Mplus") 

summary(trust.fit2, fit.measures = TRUE) 

#Cronbach's alpha
alpha(csew_nvf[,c("polatt2","polatt3")])
alpha(csew_nvf[,c("polatt1","polatt6", "polatt7")])

#Path diagrams
#one factor 
semPaths(trust.fit1, what = "paths", whatLabels = "est", intercepts = FALSE,residuals = FALSE,
         sizeMan = 12, sizeMan2 = 8,layout = "tree2", sizeLat = 20, sizeLat2 = 10,
         width = 5, height = 3, label.cex = 1, nCharNodes = 0, curve = 2.5,
         label.scale = FALSE, edge.label.cex = 1.2)

#two factors 
semPaths(trust.fit2, what = "paths", whatLabels = "est", intercepts = FALSE, residuals = FALSE, #removes curved arrows 
         sizeMan = 12, sizeMan2 = 8,layout = "tree2", rotation = 1, sizeLat = 20, sizeLat2 = 10,
         width = 5, height = 3, label.cex = 1, nCharNodes = 0, curve = 2.5,
         label.scale = FALSE, edge.label.cex = 1.2)

#Create a latent variable in NVF dataset for one factor model 
csew_nvf$trust <- as.data.frame(lavPredict(trust.fit1))$trust

#Create two latent variables in NVF dataset for two factors model 
csew_nvf$procedural_justice <- as.data.frame(lavPredict(trust.fit2))$pj 
csew_nvf$effectiveness <- as.data.frame(lavPredict(trust.fit2))$eff

#recode CFA scores to 0-1 values
summary(csew_nvf$trust)
summary(csew_nvf$procedural_justice)
summary(csew_nvf$effectiveness)

csew_nvf <- mutate(csew_nvf, trust_recoded = 
                     ((trust - min(csew_nvf$trust, na.rm = TRUE))/
                        (max(csew_nvf$trust, na.rm = TRUE)-min(csew_nvf$trust, na.rm = TRUE))))

summary(csew_nvf$trust_recoded)

csew_nvf <- mutate(csew_nvf, procedural_justice_recoded = 
                                         ((procedural_justice- min(csew_nvf$procedural_justice, na.rm = TRUE))/
                                            (max(csew_nvf$procedural_justice, na.rm = TRUE)-min(csew_nvf$procedural_justice, na.rm = TRUE))))

summary(csew_nvf$procedural_justice_recoded)

csew_nvf <- mutate(csew_nvf, effectiveness_recoded = 
                     ((effectiveness - min(csew_nvf$effectiveness, na.rm = TRUE))/
                        (max(csew_nvf$effectiveness, na.rm = TRUE)-min(csew_nvf$effectiveness, na.rm = TRUE))))

summary(csew_nvf$effectiveness_recoded)
#####
                   
#Victim Form data ####
csew_vf_20 <- read_sav("Data/csew_apr19mar20_vf.sav")
csew_vf_19 <- read_sav("Data/csew_apr18mar19_vf.sav")
csew_vf_18 <- read_sav("Data/csew_apr17mar18_vf.sav")

#Add rounds 
csew_vf_20$round <- rep(c("2019/2020"))
head(csew_vf_20$round)

csew_vf_19$round <- rep(c("2018/2019"))
tail(csew_vf_19$round)

csew_vf_18$round <- rep(c("2017/2018")) 
head(csew_vf_18$round) 

#Merge datasets from 3 years 
csew_vf <- bind_rows(csew_vf_18, csew_vf_19, csew_vf_20)

rm("csew_vf_18", "csew_vf_19", "csew_vf_20")

#Join NVF (with latent variables) to VF by common variable 'rowlabel'
csew <- csew_vf %>% left_join(csew_nvf, by = 'rowlabel')

rm("csew_vf", "csew_nvf")
#####

#Clean ####
#Filter by violent offences
#All BCS violence labels - 11,12,13,21,32,33,41,42 
csew <- filter(csew, offence == 11 | offence == 12 | offence == 13
               | offence == 21 | offence == 32 | offence == 33 | offence == 41
               | offence == 42)


#Make new variables 
#Variables were coded as either 0 (no) or 1 (yes).

#Dependent variable - reported
csew <- csew %>% mutate(reported = ifelse(copsknow == 1 & howcopk == 1, 1, NA),
                        reported = ifelse(copsknow == 2 | howcopk == 2 | howcopk == 3 | 
                                            howcopk == 4 | howcopk == 5 | howcopk == 9, 0, 
                                          reported)) 

sum(is.na(csew$reported))
class(csew$reported)
#Factorise and assign labels 
csew$reported <- 
  factor(csew$reported, levels=c(0,1),
         labels=c("No", 
                  "Yes"))

table(csew$reported) 

#Situational characteristics
#Attempted
csew <- csew %>% mutate(attempted = ifelse(offence == 21 | offence == 42, 1, 0))

sum(is.na(csew$attempted)) 
class(csew$attempted)
#Factorise and assign labels
csew$attempted <- 
  factor(csew$attempted, levels=c(0,1),
         labels=c("No", 
                  "Yes"))

table(csew$attempted) 

#Stranger
csew <- csew %>% mutate(stranger = case_when(knewoff1 == 2 | knewoff == 3 ~ 1, 
                                             knewoff1 == 1 | knewoff == 1 ~ 0,
                                             TRUE ~ as.numeric(NA)))

sum(is.na(csew$stranger))
class(csew$stranger)
# Factorise and assign labels
csew$stranger <- 
  factor(csew$stranger, levels=c(0,1),
         labels=c("No", 
                  "Yes"))

table(csew$stranger)

#Injury 
csew <- csew %>% mutate(injury = case_when(injury1 == 1 | respinj == 1 ~ 1, 
                                           injury1 == 2 | respinj == 0 ~ 0,
                                           TRUE ~ as.numeric(NA)))

sum(is.na(csew$injury))
class(csew$injury)

# Factorise and assign labels 
csew$injury <- 
  factor(csew$injury, levels=c(0,1),
         labels=c("No", 
                  "Yes"))

table(csew$injury) 

#Weapon 
table(csew$weapon) #yes=1 / no=2
#Recode 9 to NA
csew$weapon[csew$weapon == 9] <- NA
sum(is.na(csew$weapon)) 
class(csew$weapon)
#factorise
csew$weapon <- as.factor(csew$weapon)

#Recode so that label 2 = 0(no) and 1 = 1(yes)
#Check levels 
levels(csew$weapon) #1; 2
#Reassign levels 
levels(csew$weapon) <- c(1, 0)

#Check reference category 
contrasts(csew$weapon)
#Reverse the order so that no=0 is reference category and yes=1 is dummy 
csew$weapon <- relevel(csew$weapon, "0")
#Check
contrasts(csew$weapon)

#Assign labels 
csew$weapon <- factor(csew$weapon, levels=c(0,1),
                      labels=c("No", "Yes"))

table(csew$weapon)

#Multiple offenders 
csew <- csew %>% mutate(multiple_offenders = case_when(Numoff2 == 2 | 
                                                         Numoff2 == 3 |
                                                         Numoff2 == 4	~ 1, 
                                                       Numoff2 == 1 ~ 0,
                                                       TRUE ~ as.numeric(NA)))

sum(is.na(csew$multiple_offenders))
class(csew$multiple_offenders)
#Factorise and assign labels 
csew$multiple_offenders <- 
  factor(csew$multiple_offenders, levels=c(0,1),
         labels=c("No", 
                  "Yes"))

table(csew$multiple_offenders) 

#Victim characteristics 
#Ethnicity 
#White=1
#Non-White=0
attributes(csew$nsethgrp) #1 = White and [2;5] = Non-White

csew <- csew %>%
  mutate(ethnicity = ifelse(nsethgrp == 1 , 1, NA),
         ethnicity = ifelse(nsethgrp == 2 | nsethgrp == 3 | nsethgrp == 4 |
                              nsethgrp == 5, 0, ethnicity)) 

sum(is.na(csew$ethnicity))
class(csew$ethnicity)
#Factorise and assign labels
csew$ethnicity <- 
  factor(csew$ethnicity, levels=c(0,1),
         labels=c("Non-White", 
                  "White"))

table(csew$ethnicity)

#Sex 
table(csew$sex) #male=1 and female=2
sum(is.na(csew$sex)) 

#Recode so that female=0; male=1
#Factorise
csew$sex <- as.factor(csew$sex)
#Check levels
levels(csew$sex) #1; 2
#Reassign levels 
levels(csew$sex) <- c(0, 1)

#Assign labels  
csew$sex <- 
  factor(csew$sex, levels=c(0,1),
         labels=c("Male", 
                  "Female"))

table(csew$sex)

#Age 
table(csew$ageshort) 
attributes(csew$ageshort) #1=16-24 and [2;5]=25+

#16-24 = 0
#25+ = 1
csew <- csew %>%
  mutate(age = ifelse(ageshort == 1 , 0, NA),
         age = ifelse(ageshort == 2 |ageshort == 3 | ageshort == 4 |
                        ageshort == 5, 1, age)) 

sum(is.na(csew$age))
class(csew$age) 
#Factorise and assign labels  
csew$age <- 
  factor(csew$age, levels=c(0,1),
         labels=c("16-24", 
                  "25+"))

table(csew$age)

#Education
table(csew$educat4)
attributes(csew$educat4) # 1 = A levels or above and [2; 3] = below A-level

#Below A-level = 0
#A-level or above = 1
csew <- csew %>% mutate(education = case_when(educat4 == 2 | educat4 == 3 ~ 0, 
                                              educat4 == 1 ~ 1,
                                              TRUE ~ as.numeric(NA)))

sum(is.na(csew$education)) 
class(csew$education)
#Factorise and assign labels  
csew$education <- 
  factor(csew$education, levels=c(0,1),
         labels=c("Below A-level", 
                  "A-level or above"))

table(csew$education) 

#Series
table(csew$pincid) #1 = Series and 2 = Single
#Recode so that 1 = yes (series) and 0 = no (single)
#Factorise 
csew$series <- as.factor(csew$pincid)
#Check levels 
levels(csew$series) #1; 2 
#Reasssign levels 
levels(csew$series) <- c(1, 0)

#Check reference category 
contrasts(csew$series)
#Reverse so that reference category is no instead of yes
csew$series <- relevel(csew$series, "0")
#Check
contrasts(csew$series)

#Assign lables 
csew$series <- 
  factor(csew$series, levels=c(0,1),
         labels=c("No", "Yes"))

table(csew$series)

#Survey rounds 
table(csew$round.x)
sum(is.na(csew$round.x)) #0 
class(csew$round.x) #character 
#Factorise and rename
csew$survey_round <- as.factor(csew$round.x)

table(csew$survey_round) 

#Subset data
csew_viol <- select(csew, rowlabel, 
                    reported, attempted, trust_recoded, effectiveness_recoded, procedural_justice_recoded,
                    injury, weapon, stranger, multiple_offenders,
                    ethnicity, sex, age, education, series, survey_round, 
                    C11weighti)


#Repeat violent victimisation 
#Victim form count per respondent
vf_per_respondent <- csew %>%
  group_by(rowlabel) %>% 
  summarise(vf_count_per_respondent=n()) 

class(vf_per_respondent$vf_count_per_respondent)
table(vf_per_respondent$vf_count_per_respondent)

#Binary 
#Repeat_victim
#if vf_per_reponded >1, then yes=1; 
#ow no=0 - not a repeat violence victim in last 12 months
vf_per_respondent$repeat_victim <- ifelse(vf_per_respondent$vf_count_per_respondent > 1, "Yes", "No")

table(vf_per_respondent$repeat_victim)
sum(is.na(vf_per_respondent$repeat_victim))#0

#Join vf_per_respondent to csew_viol
csew_viol <- csew_viol %>% left_join(vf_per_respondent, by='rowlabel')

class(csew_viol$repeat_victim) #character 
#Factorise
csew_viol$repeat_victim <- as.factor(csew_viol$repeat_victim)

#Missing data 
vis_miss(csew_viol)
#Complete cases 
csew_viol$complete <- complete.cases((csew_viol))
table(csew_viol$complete)
#Check what proportion are complete cases 
mean(csew_viol$complete) #full data in 89% of cases 
#####

#Descriptive statistics ####
label(csew_viol$trust_recoded) <- "Trust in Police"
label(csew_viol$effectiveness_recoded) <- "Trust in Police Effectiveness"
label(csew_viol$procedural_justice_recoded) <- "Trust in Police Procedural Justice"
label(csew_viol$reported) <- "Reported"
label(csew_viol$ethnicity) <- "Ethnicity"
label(csew_viol$sex) <- "Sex"
label(csew_viol$age) <- "Age"
label(csew_viol$education) <- "Education"
label(csew_viol$attempted) <- "Attempted"
label(csew_viol$injury) <- "Injury"
label(csew_viol$weapon) <- "Weapon"
label(csew_viol$stranger) <- "Stranger"
label(csew_viol$multiple_offenders) <- "Multiple Offenders"
label(csew_viol$series) <- "Series"
label(csew_viol$survey_round) <- "Survey Round"
label(csew_viol$repeat_victim) <- "Repeat Violent Victimisation"

#Table outputs for descriptive statistics 
#Customise using renders 
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f%%)", FREQ, PCT))))
}

caption1  <- "Descriptive Statistics by Dependent Variable Outcome"

#Grouped by reported 
table1(~ effectiveness_recoded + procedural_justice_recoded + 
         ethnicity + sex + age + education + 
         attempted + injury + weapon + stranger + multiple_offenders + series +
         repeat_victim + survey_round | reported,  
       data = na.omit(csew_viol), caption = caption1,
       render.continuous = c(.="Mean (SD)", .="[Min, Max]"), render.categorical = my.render.cat)

#Descriptive statistics table, with missing values 
table1(~ reported + effectiveness_recoded + procedural_justice_recoded + 
         ethnicity + sex + age + education + 
         attempted + injury + weapon + stranger + multiple_offenders + series +
         repeat_victim + survey_round,  
       data = csew_viol,
       render.continuous = c(.="Mean (SD)", .="[Min, Max]"), render.categorical = my.render.cat)


#####

#Logistic regression - trust in police effectiveness (from two factor model) ####

#Full cases only
csew_full <- na.omit(csew_viol)

#Victim form count per respondent from full cases dataset
vf_per_respondent_full <- csew_full %>%
  group_by(rowlabel) %>% 
  summarise(vf_count_per_respondent_full=n()) 

table(vf_per_respondent_full$vf_count_per_respondent_full)
#1757 victims 
#1896 victim forms 
#120 victims experienced completed more than one victim from 
#6.8% of victims experienced repeat victimisation 

#Null model
m_null<- glm(reported ~ 1, 
             data = csew_full, family = binomial(logit))

summary(m_null) 

m1 <- glm(reported ~ effectiveness_recoded + 
                 repeat_victim + series + survey_round, 
               data = csew_full, family = binomial(link="logit"))

summary(m1)
vif(m1)


m2 <- glm(reported ~ effectiveness_recoded + 
                         ethnicity + sex + age + education +
                         repeat_victim + series + survey_round, 
                       data = csew_full, family = binomial(link="logit"))

summary(m2) 
vif(m2)


m3 <- glm(reported ~ effectiveness_recoded +
                            attempted + injury + weapon + multiple_offenders +
                            stranger +
                            repeat_victim  + series + survey_round,
                          data = csew_full, family = binomial(link="logit"))

summary(m3) 
vif(m3)


m4 <- glm(reported ~ effectiveness_recoded +
                       ethnicity + sex + age + education +
                       attempted + injury + weapon + multiple_offenders + 
                       stranger + 
                       repeat_victim  + series + survey_round,  
                     data = csew_full, family = binomial(link="logit")) 

summary(m4)
vif(m4)


#Regression table output
tab_model(m1, m2, m3, m4,
          show.intercept = TRUE,
          show.est = TRUE,
          show.ci = 0.95,
          show.p = TRUE,
          show.r2 = TRUE,
          show.obs = FALSE,
          title = "Table 1. Logistic regression predicting violent crime reporting",
          dv.labels = c("Model 1" ,"Model 2", "Model 3", "Model 4"), 
          string.pred = "Predictors",
          string.est = "OR",
          string.ci = "CI",
          string.resp = "Response",
          string.intercept = "Intercept",
          collapse.ci = FALSE,
          col.order = c("est", "ci"),
          digits = 2,
          p.style = c("stars"),
          CSS = css_theme("regression") 
          #file = "regression_table.doc"
)




#Model fit statistics 
#Model 1
#Model chi squared
with(m1, null.deviance - deviance) #6.33
#df
with(m1, df.null - df.residual)
#p value
with(m1, pchisq(null.deviance - deviance, 
                      df.null - df.residual, lower.tail = FALSE)) #not signif

#Model 2
#Model chi squared 
with(m2, null.deviance - deviance) #30.05
#df
with(m2, df.null - df.residual) 
#p value
with(m2, pchisq(null.deviance - deviance, 
                             df.null - df.residual, lower.tail = FALSE))

#Model 3
#model chi squared
with(m3, null.deviance - deviance) #54.09
#df
with(m3, df.null - df.residual)
#p value 
with(m3, pchisq(null.deviance - deviance, 
                                df.null - df.residual, lower.tail = FALSE))

#Model 4
#Model chi squared 
with(m4, null.deviance - deviance) #73.45
#df
with(m4, df.null - df.residual)
#p value 
with(m4, pchisq(null.deviance - deviance, 
                           df.null - df.residual, lower.tail = FALSE))


#McFadden R2 
1-logLik(m1)/logLik(m_null) #0.29%
1-logLik(m2)/logLik(m_null) #1.38%
1-logLik(m3)/logLik(m_null) #2.49%
1-logLik(m4)/logLik(m_null) #3.38%

#Nagelkerke's R2
#adjusted version of the Cox & Snell R2, values 0-1
#Nagelkerke will typically be higher than McFadden
NagelkerkeR2(m1) #0.49%
NagelkerkeR2(m2) #2.31%
NagelkerkeR2(m3) #4.12%
NagelkerkeR2(m4) #5.57%

#Tjur's R2 calculated in table output 

#Likelihood ratio test for differences in models
lrtest(m2, m4) 
#Reject H0 - full model predictors offer a 
#significant improvement in fit over the model with just victim characteristics.

lrtest(m3, m4) 
#Reject H0 - full model predictors offer a 
#significant improvement in fit over the model with just situational characteristics.

#Full model (Model 4) accuracy
predicted_1 <- round(predict(m4, csew_full, type = "response"), 3)
#Confusion matrix
confusion_matrix_1 <- table(csew_full$reported, round(predicted_1))
confusion_matrix_1

#Accuracy rate
accuracy <- function(x){
  sum(diag(x) / (sum(rowSums(x)))) * 100
}
accuracy(confusion_matrix_1) #74%

#Full model sensitivity analysis
#Conventional cut-off = 0.5
precision<-function(c) {
  tab1 <- table(predicted_1>c, csew_full$reported)
  out <- diag(tab1)/apply(tab1, 2, sum)
  names(out) <- c('specificity', 'sensitivity')
  list(tab1, out)
}
precision(.5) 
#sensitivity 2%

#ROC curve
roc1 <- pROC::roc(reported ~ predicted_1, data = csew_full, plot=T)

auc(roc1) #0.6244 - poor
#The closer the AUC value is to the 1, the better the given model fits the data.
ci.auc(roc1) #95% CI: 0.596-0.6527


#Select optimal cut off point
#Maximise sensitivity
cutoff_point <- coords(roc1, x = "best", best.method = "closest.topleft")
cutoff_point
#sensitivity 60%
#threshold 25%

#Based on Youden index
#Maximise both sensitivity and specificity
cutoff_point1 <- coords(roc1, x ="best", best.method = "youden")
cutoff_point1
#sensitivity 45%
#threshold 29%
#####

#Logistic regression - one factor trust ####

m1_t <- glm(reported ~ trust_recoded + 
            repeat_victim + series + survey_round, 
          data = csew_full, family = binomial(link="logit"))

summary(m1_t)
vif(m1_t)


m2_t <- glm(reported ~ trust_recoded + 
            ethnicity + sex + age + education +
            repeat_victim + series + survey_round, 
          data = csew_full, family = binomial(link="logit"))

summary(m2_t) 
vif(m2_t)

m3_t <- glm(reported ~ trust_recoded +
            attempted + injury + weapon + multiple_offenders +
            stranger +
            repeat_victim  + series + survey_round,
          data = csew_full, family = binomial(link="logit"))

summary(m3_t) 
vif(m3_t)

#Full model
m4_t <- glm(reported ~ trust_recoded +
            ethnicity + sex + age + education +
            attempted + injury + weapon + multiple_offenders + 
            stranger + 
            repeat_victim  + series + survey_round,  
          data = csew_full, family = binomial(link="logit")) 

summary(m4_t)
vif(m4_t)

#Regression table output
tab_model(m1_t, m2_t, m3_t, m4_t,
          show.intercept = TRUE,
          show.est = TRUE,
          show.ci = 0.95,
          show.p = TRUE,
          show.r2 = TRUE,
          show.obs = FALSE,
          title = "Table 1. Logistic regression predicting violent crime reporting",
          dv.labels = c("Model 1" ,"Model 2", "Model 3", "Model 4"), 
          string.pred = "Predictors",
          string.est = "OR",
          string.ci = "CI",
          string.resp = "Response",
          string.intercept = "Intercept",
          collapse.ci = FALSE,
          col.order = c("est", "ci"),
          digits = 2,
          p.style = c("stars"),
          CSS = css_theme("regression") 
         # file = "regression_table_one_factor.doc"
)

#Model fit statistics 
#Model 1
#Model chi squared
with(m1_t, null.deviance - deviance) #6.48
#df
with(m1_t, df.null - df.residual)
#p value
with(m1_t, pchisq(null.deviance - deviance, 
                  df.null - df.residual, lower.tail = FALSE)) #not signif

#Model 2
#Model chi squared 
with(m2_t, null.deviance - deviance) #30.20
#df
with(m2_t, df.null - df.residual) 
#p value
with(m2_t, pchisq(null.deviance - deviance, 
                  df.null - df.residual, lower.tail = FALSE))

#Model 3
#model chi squared
with(m3_t, null.deviance - deviance) #54.30
#df
with(m3_t, df.null - df.residual)
#p value 
with(m3_t, pchisq(null.deviance - deviance, 
                  df.null - df.residual, lower.tail = FALSE))

#Model 4
#Model chi squared 
with(m4_t, null.deviance - deviance) #73.66
#df
with(m4_t, df.null - df.residual)
#p value 
with(m4_t, pchisq(null.deviance - deviance, 
                  df.null - df.residual, lower.tail = FALSE))

#McFadden R2 (Likelihood ratio R2)
1-logLik(m1_t)/logLik(m_null) #0.3%
1-logLik(m2_t)/logLik(m_null) #1.39%
1-logLik(m3_t)/logLik(m_null) #2.50%
1-logLik(m4_t)/logLik(m_null) #3.39%

#Nagelkerke's R2
NagelkerkeR2(m1_t) #0.5%
NagelkerkeR2(m2_t) #2.32%
NagelkerkeR2(m3_t) #4.14%
NagelkerkeR2(m4_t) #5.59%

#Tjur's R2 calculated in table output 

#Full model (Model 4) accuracy
predicted_2 <- round(predict(m4_t, csew_full, type = "response"), 3)
#Confusion matrix
confusion_matrix_2 <- table(csew_full$reported, round(predicted_2))
confusion_matrix_2

#Accuracy rate
accuracy <- function(x){
  sum(diag(x) / (sum(rowSums(x)))) * 100
}
accuracy(confusion_matrix_2) #74%

#Full model sensitivity analysis
#Conventional cut-off = 0.5
precision<-function(c) {
  tab1 <- table(predicted_2>c, csew_full$reported)
  out <- diag(tab1)/apply(tab1, 2, sum)
  names(out) <- c('specificity', 'sensitivity')
  list(tab1, out)
}
precision(.5) 
#sensitivity 2%

#ROC curve
roc2 <- pROC::roc(reported ~ predicted_2, data = csew_full, plot=T)

auc(roc2) #0.6243 #poor
#The closer the AUC value is to the 1, the better the given model fits the data.
ci.auc(roc2) #95% CI: 0.596-0.6526

#Select optimal cut off point
#Maximise sensitivity
cutoff_point_trust1 <- coords(roc2, x = "best", best.method = "closest.topleft")
cutoff_point_trust1
#sensitivity 62%
#threshold 25%

#Based on Youden index 
#Maximise both sensitivity and specificity 
cutoff_point_trust2 <- coords(roc2,  x ="best", best.method = "youden")
cutoff_point_trust2
#sensitivity 46%
#threshold 29%
#####





