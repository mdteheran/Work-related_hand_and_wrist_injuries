###Database curation##
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("actuar","AICcmodavg","agricolae","apaTables","aod","arm","astsa",
              "boot","broom","car","carData","caret","cmprsk","corrr","corrplot",
              "cowplot","correlationfunnel","explore","explor","DescTools","DALEX",
              "DataExplorer","datasets","DataEditR","data.table","dplyr","dynlm",
              "dlookr","editData","ellipse","easystats","esquisse","effects","effectsize",
              "faraway","fdth","fable","flexmix","flexsurv","forcats","forecast","grafify",
              "foreign","gapminder","GGally","ggcorrplot","ggalt","ggpubr","ggstatsplot","ggrepel",
              "gmodels","gtsummary","ggridges","ggfortify","ggplot2","ggview","gplots","gridExtra",
              "ggthemes","ggrain","ggdist","ggVennDiagram","haven","HH","Hmisc","hrbrthemes","ISLR","ISLR2","interactions",
              "janitor","jtools","kableExtra","KMsurv","lubridate","lsr","lmtest","lmSupport",
              "MASS","meta","missForest","modelr","modelsummary","modelStudio","pls","mFilter",
              "MLmetrics","mstate","multcompView","multcomp","nlme","oddsratio","parameters",
              "PairedData","palmerpenguins","pwr","PerformanceAnalytics","PASWR","picante",
              "psych","plotly","psychometric","QuantPsyc","rlang","reshape2","rattle","rgl",
              "ranger","RColorBrewer","readr","rmarkdown","rcompanion","riskRegression",
              "readxl","rstatix","randomForest","rpivotTable","RVAideMemoire","splines",
              "skimr","SMPracticals","stats","sjstats","ssym","summarytools","scatterD3",
              "survMisc","survminer","survival","texreg","tidyverse","timsac","tidyverse",
              "timetk","tsdl","tsbox","tsibble","tseries","TSstudio","vars","viridis",
              "vioplot","wesanderson","xts","xtable","yarrr")
ipak(packages)

library(remotes)
remotes::install_github("arcruz0/paqueteadp")
library(paqueteadp)
library(devtools)
devtools::install_github("FinYang/tsdl")

dim(HWinjur)
names(HWinjur)
str(HWinjur)
attach(HWinjur)

##Descriptive analysis##
shapiro.test(Age)
describe(Age)
summary(Age)

Freq(Sex)
Freq(Econ_activity)
Freq(ARL)
Freq(day_moment)
Freq(Day_week)

shapiro.test(Disability_days)
describe(Disability_days)
summary(Disability_days)
Freq(hand_injury_hx)
Freq(Limb_laterality)
Freq(Injury_type)
Freq(fx)
Freq(Nerve)
Freq(Tendon)
Freq(Vascular)
Freq(Bone)
Freq(Finger)
Freq(Bone_scan)
Freq(Suture)
Freq(Immobilization)
Freq(Orthopedic_assess)
Freq(surgery.)
Freq(Plastic_surg_assess)
Freq(Referral)

boxLabels = c("1-Monday (13.9%)","2-Tuesday (17.3%)","3-Wednesday (21.3%)","4-Thursday (18.3%)",
              "5-Friday (16.3%)","6-Saturday (10.4%)","7-Sunday (1.89%)","0-Overall (20.1%)")
df <- data.frame(yAxis = length(boxLabels):8, 
                 boxProportions = c(13.9, 17.3, 21.3, 18.3, 16.3, 10.4, 1.89, 20.1), 
                 boxCILow = c(10.8, 13.8, 17.5, 15.9, 12.9, 7.66, 0.62, 18.4), 
                 boxCIHigh = c(17.6, 21.2, 25.4, 22.9, 20.2, 13.7, 3.69, 21.9))

(p <- ggplot(df, aes(x = boxProportions, y = boxLabels)) + 
    geom_vline(aes(xintercept = 20.1), size = .8, linetype = "dashed", col= "black") + 
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = 1, height = .2, color = "black") +
    geom_point(size = 3.5, color = "darkblue") +
    scale_x_continuous(breaks = seq(0,27,3), labels = seq(0,27,3), limits = c(0,27)) +
    theme_economist () +
    theme(axis.title = element_text(size = 20)) +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(axis.text = element_text(face="bold")) +
    theme(plot.title = element_text(size = 20)) +
    theme(legend.text = element_text(size = 18)) +
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Proportion, 95%IC") +
    annotate(geom = "text", y =8.2, x = 20.2,
             label = "X-squared= 93.652, df= 6, p-value <0.001", size = 5, hjust = 0, color = "darkblue") + 
    ggtitle("Frequency of hands or wrist injuries")
)
