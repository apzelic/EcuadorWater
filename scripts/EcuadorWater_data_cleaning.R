## Install packages  ####
library(kable)
library(ggplot2)
library(gmodels)
library(pscl)
library(xlsx)
library(lme4)

## Read in data ####
EcData <-read.xlsx(file = "data/EcuadorDataWithSES.xlsx", sheetIndex = "Ecuador data final3", header = T)
EcData$DiarOrPain <- ifelse((EcData$diarrhea == 1 | EcData$sto_pain ==1), 1,0)
EcData$Gender <-ifelse((EcData$sex==1), "Male", "Female")
EcData$Gender <- as.factor(EcData$Gender)
EcData$ses_2 <-ifelse((EcData$ses<5), 4, 5)

# subset data by site
EcDataST <- EcData[EcData$site ==1,]
EcDataCol <-EcData[EcData$site ==2,]

#t-test for age
ttestAge <-t.test(EcDataCol$age, EcDataST$age)

# boxplots of raw data
bp1 <- ggplot(EcData, aes(y=age, x=factor(site))) + geom_boxplot() + 
    labs(title = "Age Distribution (in years)", x="Sites (1 = Santa Theresa, 2 = Colaguila)", y="Age") +
    geom_jitter(color = 'red', size=1, width = c(0.2), height = 0)

pdf("figures/boxplot.pdf")
bp1
dev.off()


multilevel_model1 = glmer(DiarOrPain ~ as.factor(site) 
                          + cat_age_2 + (1|code), 
                          data=EcData, family = 'binomial')

multilevel_model2 = glmer(DiarOrPain ~ as.factor(site) + as.factor(ses_2)
                          + cat_age_2 + (1|code), 
                          data=EcData, family = 'binomial')


interactionModel1<-glmer(DiarOrPain ~ (as.factor(site) * as.factor(ses_2))
                         + cat_age_2 + (1|code), 
                         data=EcData, family = 'binomial')
                         
interactionModel2<-glmer(DiarOrPain ~ (as.factor(site) * as.factor(ses_2) *cat_age_2) + 
                             (1|code), 
                         data=EcData, family = 'binomial')  

interactionModel3<-glmer(DiarOrPain ~ (as.factor(site) * cat_age_2) + 
                                           as.factor(ses_2)+ (1|code), 
                         data=EcData, family = 'binomial')

interactionModel4<-glmer(DiarOrPain ~ as.factor(site) + (cat_age_2*as.factor(ses_2)) + 
                             (1|code), 
                         data=EcData, family = 'binomial')
