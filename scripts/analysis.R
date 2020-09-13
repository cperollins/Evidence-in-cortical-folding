## Analysis code for manuscript "Evidence in cortical folding patterns 
## for prenatal predispositions to hallucinations in schizophrenia"
## Colleen PE Rollins et al. (2020)
## cper2@cam.ac.uk

## September 2020

# load dependencies
library(tidyverse)
library(ggplot2)
library(lmerTest)
library(ggpubr)
library(tableone)

# load data. Requestions should be made to the author
data <- read.csv('data/data_uk-shanghai.csv',na.strings=c("","NA")) %>% 
  mutate(pcs_length_AI = 2*(pcs_length_rh - pcs_length_lh) / (pcs_length_rh + pcs_length_lh)) %>% 
  mutate(pcs_depth_AI = 2*(pcs_depth_rh - pcs_depth_lh) / (pcs_depth_rh + pcs_depth_lh)) %>%   
  mutate(sts_length_AI = 2*(sts_length_rh - sts_length_lh) / (sts_length_rh + sts_length_lh)) %>% 
  mutate(sts_depth_AI = 2*(sts_depth_rh - sts_depth_lh) / (sts_depth_rh + sts_depth_lh)) %>%   
  mutate(panss_ptotal_minusp3 = panss_ptotal - panss_p3) %>% 
  filter(quality_control == '1')
# mutate creates asymmetry indices

# linear models for paracingulate and superior temporal sulci (PCS, STS)
# length and depth analyses by hemisphere
# plot function to assess assumptions of linear regression

# pcs length
model1 <- lm(pcs_length_lh ~ group + centre + age + sex + etiv, data=data)
summary(model1)
plot(model1)
# additional covariates tested but not included in final model:
# education_yrs, iq, lgi_total, csa_total, olanzapine_equivalent, panss_p1, 
# panss_ptotal_minusp3
# model1_ext <- lm(pcs_length_lh ~ group + centre + age + sex + etiv + 
#                  education_yrs + iq + lgi_total + csa_total + 
#                  olanzapine_equivalent + panss_p1 + panss_ptotal_minusp3, 
#                data=data)
# summary(model1_ext)
model2 <- lm(pcs_length_rh ~ group + centre + age + sex + etiv, data=data)
summary(model2)
plot(model2)

# pcs depth
model3 <- lm(pcs_depth_lh ~ group + centre + age + sex + etiv, data=data)
summary(model3)
plot(model3)
model4 <- lm(pcs_depth_rh ~ group + centre + age + sex + etiv, data=data)
summary(model4)
plot(model4)

# sts length
model5 <- lm(sts_length_lh ~ group + centre + age + sex + etiv, data=data)
summary(model5)
plot(model5)
model6 <- lm(sts_length_rh ~ group + centre + age + sex + etiv, data=data)
summary(model6)
plot(model6)

# sts depth
model7 <- lm(sts_depth_lh ~ group + centre + age + sex + etiv, data=data)
summary(model7)
plot(model7)
model8 <- lm(sts_depth_rh ~ group + centre + age + sex + etiv, data=data)
summary(model8)
plot(model8)

# subgroup analyses
# separated by dataset
model1_uk <- lm(pcs_length_lh ~ group + centre + age + sex + etiv, 
                data=(data %>% filter(sample == "uk")))
summary(model1_uk)
plot(model1_uk)
model1_shanghai <- lm(pcs_length_lh ~ group + age + sex + etiv, 
                data=(data %>% filter(sample == "shanghai")))
summary(model1_shanghai)
plot(model1_shanghai)

model7_uk <- lm(sts_depth_rh ~ group + centre + age + sex + etiv, 
               data=(data %>% filter(sample == "uk")))
summary(model7_uk)
plot(model7_uk)
model7_shanghai<- lm(sts_depth_rh ~ group + age + sex + etiv, 
                     data=(data %>% filter(sample == "shanghai")))
summary(model7_shanghai)
plot(model7_shanghai)

# asymmetry index analyses
anova1 <- aov(pcs_length_AI~group,data=data)
summary(anova1)
TukeyHSD(anova1)

anova2 <- aov(pcs_depth_AI~group,data=data)
summary(anova2)

anova3 <- aov(sts_length_AI~group,data=data)
summary(anova3)

anova4 <- aov(sts_depth_AI~group,data=data)
summary(anova4)

t.test((data %>% filter(group == "H"))$pcs_length_AI, mu = 0, alternative = "two.sided")
t.test((data %>% filter(group == "NH"))$pcs_length_AI, mu = 0, alternative = "two.sided")
t.test((data %>% filter(group == "HC"))$pcs_length_AI, mu = 0, alternative = "two.sided")

t.test((data %>% filter(group == "H"))$pcs_depth_AI, mu = 0, alternative = "two.sided")
t.test((data %>% filter(group == "NH"))$pcs_depth_AI, mu = 0, alternative = "two.sided")
t.test((data %>% filter(group == "HC"))$pcs_depth_AI, mu = 0, alternative = "two.sided")

t.test((data %>% filter(group == "H"))$sts_length_AI, mu = 0, alternative = "two.sided")
t.test((data %>% filter(group == "NH"))$sts_length_AI, mu = 0, alternative = "two.sided")
t.test((data %>% filter(group == "HC"))$sts_length_AI, mu = 0, alternative = "two.sided")

t.test((data %>% filter(group == "H"))$sts_depth_AI, mu = 0, alternative = "two.sided")
t.test((data %>% filter(group == "NH"))$sts_depth_AI, mu = 0, alternative = "two.sided")
t.test((data %>% filter(group == "HC"))$sts_depth_AI, mu = 0, alternative = "two.sided")

# table 1
# demographic and clinical characteristics 
data$group <- as.factor(data$group)
data$group <- relevel(data$group, ref="NH")
data$group <- relevel(data$group, ref="H")
data$sample <- relevel(data$sample, ref="uk")
listVarsM <- c("sample", "sex", "age", "centre", "education_yrs", 
               "iq", "panss_ptotal", "panss_ptotal_minusp3", "panss_ntotal", 
               "panss_p3", "panss_p3_avg","panss_p1", "etiv", "olanzapine_equivalent")
catVarsM <- c("sample", "sex", "group", "centre")
tbl <- CreateTableOne(vars = listVarsM, data = data, factorVars = catVarsM, strata = c("group", "sample"),includeNA=FALSE)
tbl
tbl <- print(tbl)
