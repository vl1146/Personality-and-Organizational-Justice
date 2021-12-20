#load libraries
library(psych)
library(tidyverse)
library(ggcorrplot)
library(pequod)
library(extrafont)

extrafont::font_import()
extrafont::loadfonts(device = "win")

#load data

data1 <- read.csv("D:/desktop/Programming/R/ARM/data.csv")
str(data1)
summary(data1)

#tag the item columns that need median impute

need.md <- c("q17","q18","q19","q20","q21","q22","q23","q24","q25","q26","q29",
             "q30","q31")

#impute missing values in selected columns with median

data2 <- data1

for(i in need.md){
  blank <- is.na(data2[,i])
  md <- median(data2[,i], na.rm = TRUE)
  data2[blank,i] <-  md
}

summary(data2)

theme_set(theme_classic())


#Create Mean Columns to Represent Domains

data2 <- data2 %>% 
  mutate(N=(q1+q2+q3)/3, E=(q4+q5+q6)/3, O=(q7+q8+q9)/3, 
                          A=(q10+q11+q12)/3, C=(q13+q14+q15)/3,
                          Affective=(q17+q18+q19)/3, Continuance=(q20+q21+q22)/3,
                          Normative=(q23+q24+q25)/3, PJustice=(q26+q27+q28)/3,
                          GJustice=(q29+q30+q31)/3, Justice=(PJustice+GJustice)/2)

hist(data2$Justice)
hist(data2$Affective)
hist(data2$Normative)


data2 %>% 
  select(Age, Gender, O,C,E,A,N, Justice, Affective, Continuance, Normative) %>%
  ggplot(aes(Gender,Normative)) +
  geom_boxplot() +
  expand_limits(y = 0, x=0)

data2 %>%
  filter(!is.na(Total))



#bar charts for demographics

ggplot(data2, aes(Age)) + 
  geom_bar(fill='#57068c', color="#000000") +
  labs(y = "Frequency", title = "Age") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.25),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, family = "Verdana", hjust = .5, face ="bold"))


#order the levels for the Education variable so it displays in order in the next graph

data2$Education <- factor(data2$Education, levels=c("High School", "Bachelors", 
                                                    "Masters", "Doctorate"))

ggplot(data2, aes(Education)) + 
  geom_bar(fill='#57068c', color="#000000") +
  labs(y = "Frequency", title = "Highest Education Level") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, family = "Verdana", hjust = .5, face ="bold"))

ggplot(data2, aes(Gender)) + 
  geom_bar(fill='#57068c', color="#000000") +
  labs(y = "Frequency", title = "Gender") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, family = "Verdana", hjust = .5, face ="bold"))


data2$Ethnicity <- factor(data2$Ethnicity, levels=c("White", "Hispanic", 
                                                    "Asian", "Black"))
ggplot(data2, aes(Ethnicity)) + 
  geom_bar(fill='#57068c', color="#000000") +
  labs(y = "Frequency", title = "Ethnicity") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, family = "Verdana", hjust = .5, face ="bold"))

ggplot(data2, aes(Justice)) + 
  geom_histogram( binwidth = .35, fill='#57068c', color="#000000") +
  labs(y = "Frequency", x = "Justice Score",
       title = "Histogram of Justice") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_text(family = "Verdana",
                                    size = 20, vjust = -.5),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        plot.title = element_text(size = 20, family = "Verdana", hjust = .5, face ="bold"))

ggplot(data2, aes(Affective)) + 
  geom_histogram( binwidth = .35, fill='#57068c', color="#000000") +
  labs(y = "Frequency", x = "Affective Commitment",
       title = "Histogram of Affective Commitment") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_text(family = "Verdana",
                                    size = 20, vjust = -.5),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        plot.title = element_text(size = 20, family = "Verdana", hjust = .5, face ="bold"))

ggplot(data2, aes(Continuance)) + 
  geom_histogram( binwidth = .35, fill='#57068c', color="#000000") +
  labs(y = "Frequency", x = "Continuance Commitment",
       title = "Histogram of Continuance Commitment") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_text(family = "Verdana",
                                    size = 20, vjust = -.5),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        plot.title = element_text(size = 20, family = "Verdana", hjust = .5, face ="bold"))

ggplot(data2, aes(Normative)) + 
  geom_histogram( binwidth = .35, fill='#57068c', color="#000000") +
  labs(y = "Frequency", x = "Normative Commitment",
       title = "Histogram of Normative Commitment") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_text(family = "Verdana",
                                    size = 20, vjust = -.5),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        plot.title = element_text(size = 20, family = "Verdana", hjust = .5, face ="bold"))

#test to see if the three DVS are correlated

corr_AN <- cor.test(data2$Affective, data2$Normative, 
         method = "pearson") # p < .001

corr_AC <- cor.test(data2$Affective, data2$Continuance, 
                    method = "pearson")

corr_NC <- cor.test(data2$Continuance, data2$Normative, 
                    method = "pearson") # p < .001


#test for main effect of justice on Affective commitment

lma1 <- lm(Affective~Justice, data = data2)
summary(lma1) #p <.001

ggplot(data2, aes(Justice, Affective)) + 
  geom_point() +
  labs(y = "Affective Commitment", x = "Overall Justice",
       title = "Affective Commitment vs. Justice") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_text(family = "Verdana",
                                    size = 20, vjust = -.5),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        plot.title = element_text(size = 20, family = "Verdana", 
                                  hjust = 10, face ="bold"))+
  geom_smooth(method='lm', formula= y~x, color ="purple")

ggplot(data2, aes(Justice, Normative)) + 
  geom_point() +
  labs(y = "Normative Commitment", x = "Overall Justice",
       title = "Normative Commitment vs. Justice") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_text(family = "Verdana",
                                    size = 20, vjust = -.5),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        plot.title = element_text(size = 20, family = "Verdana", 
                                  hjust = 10, face ="bold"))+
  geom_smooth(method='lm', formula= y~x, color ="purple")


#Openness moderates Justice on Continuance Commitment!

mod1ca <- lm(Continuance~Justice+O+Justice*O, data=data2)
summary(mod1ca)

mod1c <- lmres(Continuance~Justice+O+Justice*O,
               centered=c("O", "Justice"), data=data2)
summary(mod1c)

mod1cSS <- simpleSlope(mod1c,pred="Justice",mod1="O")
summary.simpleSlope(mod1cSS) #sig effect for high O but not low O

PlotSlope(mod1cSS, namey = "Commitment",
          namex = "Overall Justice") +
  labs(title = "Continuance Commitment vs. Justice",
       subtitle = "(For Low and High Levels of Openness)") +
  coord_cartesian(xlim = c(2, 4.5), ylim = c(2.5, 3.5)) +
  theme_classic() + 
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_text(family = "Verdana",
                                    size = 20, vjust = -.5),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        plot.title = element_text(size = 20, family = "Verdana", 
                                  hjust = .2, face ="bold"),
        plot.subtitle = element_text(size = 18, family = "Verdana", 
                                     hjust = .35, face ="bold"),
        legend.position = "right",
        legend.title = element_blank())


#Neuroticism moderates Justice on Normative Commitment !

mod5na <- lm(Normative~Justice+N+Justice*N, data=data2)
summary(mod5na) # p < .001

mod5n <- lmres(Normative~Justice+N+Justice*N, 
               centered=c("N", "Justice"), data=data2)
summary(mod5n) 

mod1nSS <- simpleSlope(mod5n,pred="Justice",mod1="N")
summary.simpleSlope(mod1nSS) #sig effect for high and low N

PlotSlope(mod1nSS, namey = "Commitment",
          namex = "Justice Score") +
  labs(title = "Normative Commitment vs. Justice",
       subtitle = "(For High and Low Levels of Neuroticism)") +
  coord_cartesian(xlim = c(2, 4.5), ylim = c(2, 3.5)) +
  theme_classic() + 
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_text(family = "Verdana",
                                    size = 20, vjust = -.5),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        plot.title = element_text(size = 20, family = "Verdana", 
                                  hjust = .2, face ="bold"),
        plot.subtitle = element_text(size = 18, family = "Verdana", 
                                     hjust = .15, face ="bold"),
        legend.position = "right",
        legend.title = element_blank())

#Test for reliability of scales used using Cronbach's Alpha // cross check with sources

psych::alpha(data2[,1:3]) #N .78
psych::alpha(data2[,4:6]) #E .77
psych::alpha(data2[,7:9]) #O.7
psych::alpha(data2[,10:12]) #A .51
psych::alpha(data2[,13:15]) #C .48

psych::alpha(data2[,17:19]) #Affective .88
psych::alpha(data2[,20:22]) #Continuance .67
psych::alpha(data2[,23:25]) #Normative .68

psych::alpha(data2[,26:31]) #Justice .96



ggplot(data2) + 
  geom_point(mapping = aes(x = Justice, y = Affective)) + 
  facet_wrap(~ O, nrow = 2) +
  theme_bw()

ggplot(data2) + 
  geom_point(mapping = aes(x = Justice, y = Continuance)) + 
  facet_wrap(~ Age, nrow = 2) +
  theme_bw()

ggplot(data2) + 
  geom_point(mapping = aes(x = Justice, y = Normative)) + 
  facet_wrap(~ Age, nrow = 2) + 
  them_bw()

###full analyses below###

#overall corr

corr1 <- cor(apply(data2[,c(1:15,17:32)],2,as.numeric))

ggcorrplot(corr1, type ="lower")

#corr N

corrN <- cor(apply(data2[,c(1:3)],2,as.numeric))

ggcorrplot(corrN, type ="lower", lab = TRUE)

#corr E

corrE <- cor(apply(data2[,c(4:6)],2,as.numeric))

ggcorrplot(corrE, type ="lower", lab = TRUE)

#corr O

corrO <- cor(apply(data2[,c(7:9)],2,as.numeric))

ggcorrplot(corrO, type ="lower", lab = TRUE)

#corr A

corrA <- cor(apply(data2[,c(10:12)],2,as.numeric))

ggcorrplot(corrA, type ="lower", lab = TRUE)

#corr C

corrC <- cor(apply(data2[,c(13:15)],2,as.numeric))

ggcorrplot(corrC, type ="lower", lab = TRUE)

#corr Affective

corrAff <- cor(apply(data2[,c(17:19)],2,as.numeric))

ggcorrplot(corrAff, type ="lower", lab = TRUE)

#corr Continuance

corrCon <- cor(apply(data2[,c(20:22)],2,as.numeric))

ggcorrplot(corrCon, type ="lower", lab = TRUE)

#corr Normative

corrNorm <- cor(apply(data2[,c(23:25)],2,as.numeric))

ggcorrplot(corrNorm, type ="lower", lab = TRUE)

#corr Personal Justice

corrPJ <- cor(apply(data2[,c(26:28)],2,as.numeric))

ggcorrplot(corrPJ, type ="lower", lab = TRUE)

#corr General Justice

corrGJ <- cor(apply(data2[,c(29:31)],2,as.numeric))

ggcorrplot(corrGJ, type ="lower", lab = TRUE)

#corr IVs and DVs

corrVars <- cor(apply(data2[,c(38:45,48)],2,as.numeric))

ggcorrplot(corrVars, type ="lower", lab = TRUE)

#corr everything

corrALL <- cor(apply(data2,2,as.numeric))

ggcorrplot(corrALL, type ="lower")

#Regression of Affective Commitment on Overall Justice and OCEAN

lma1 <- lm(Affective~Justice, data = data2)
summary(lma1) #p <.001

lma2 <- lm(Affective~O, data = data2)
summary(lma2)

lma3 <- lm(Affective~C, data = data2)
summary(lma3)

lma4 <- lm(Affective~E, data = data2)
summary(lma4) #p <.01

lma5 <- lm(Affective~A, data = data2)
summary(lma5)

lma6 <- lm(Affective~N, data = data2)
summary(lma6)

#testing moderation of personality on justice on affective

mod1a <- lm(Affective~Justice+O+Justice*O, data=data2)
summary(mod1a)

mod2a <- lm(Affective~Justice+C+Justice*C, data=data2)
summary(mod2a)

mod3a <- lm(Affective~Justice+E+Justice*E, data=data2)
summary(mod3a)

mod4a <- lm(Affective~Justice+A+Justice*A, data=data2)
summary(mod4a)

mod5a <- lm(Affective~Justice+N+Justice*N, data=data2)
summary(mod5a)

#Regression of Continuance Commitment on Overall Justice and OCEAN

lmc1 <- lm(Continuance~Justice, data = data2)
summary(lmc1) 

lmc2 <- lm(Continuance~O, data = data2)
summary(lmc2)

lmc3 <- lm(Continuance~C, data = data2)
summary(lmc3)

lmc4 <- lm(Continuance~E, data = data2)
summary(lmc4)

lmc5 <- lm(Continuance~A, data = data2)
summary(lmc5)

lmc6 <- lm(Continuance~N, data = data2)
summary(lmc6) #p = .055

#testing moderation of personality on justice on continuance

mod1ca <- lm(Continuance~Justice+O+Justice*O, data=data2)
summary(mod1ca)

mod1c <- lmres(Continuance~Justice+O+Justice*O,
               centered=c("O", "Justice"), data=data2)
summary(mod1c)

mod1cSS <- simpleSlope(mod1c,pred="Justice",mod1="O")
summary.simpleSlope(mod1cSS)

PlotSlope(mod1cSS, namey = "Continuance Commitment",
          namex = "Perceptions of Justice")+
  coord_cartesian(xlim = c(2, 4.5), ylim = c(2.5, 3.5))+
  theme_classic()

mod2c <- lm(Continuance~Justice+C+Justice*C, data=data2)
summary(mod2c)

mod3c <- lm(Continuance~Justice+E+Justice*E, data=data2)
summary(mod3c)

mod4c <- lm(Continuance~Justice+A+Justice*A, data=data2)
summary(mod4c)

mod5c <- lm(Continuance~Justice+N+Justice*N, data=data2)
summary(mod5c)

#Regression of Normative Commitment on Overall Justice and OCEAN

lmn1 <- lm(Normative~Justice, data = data2)
summary(lmn1) #p <.001

lmn2 <- lm(Normative~O, data = data2)
summary(lmn2)

lmn3 <- lm(Normative~C, data = data2)
summary(lmn3)

lmn4 <- lm(Normative~E, data = data2)
summary(lmn4)

lmn5 <- lm(Normative~A, data = data2)
summary(lmn5)

lmn6 <- lm(Normative~N, data = data2)
summary(lmn6)

#testing moderation of personality on justice on normative commitment

mod1n <- lmres(Normative~Justice+N+Justice*O, data=data2)
summary(mod1n)

mod2n <- lm(Normative~Justice+C+Justice*C, data=data2)
summary(mod2n)

mod3n <- lm(Normative~Justice+E+Justice*E, data=data2)
summary(mod3n)

mod4n <- lm(Normative~Justice+A+Justice*A, data=data2)
summary(mod4n)

mod5n <- lmres(Normative~Justice+N+Justice*N, 
               centered=c("N", "Justice"), data=data2)
summary(mod5n) # p < .05

mod1nSS <- simpleSlope(mod5n,pred="Justice",mod1="N")
summary.simpleSlope(mod1nSS)

PlotSlope(mod1nSS, namey = "Normative Commitment", 
          namex = "Perceptions of Justice")+
  coord_cartesian(xlim = c(2, 4.5), ylim = c(2, 3.5))+
  theme_classic()

#Regress Org Commitment on Age

lmAGE1 <- lm(Affective~Age, data = data2)
summary(lmAGE1)

lmAGE2 <- lm(Continuance~Age, data = data2)
summary(lmAGE2)

lmAGE3 <- lm(Normative~Age, data = data2)
summary(lmAGE3)

#Regress Org Commitment on Age

lmEth1 <- lm(Affective~Ethnicity, data = data2)
summary(lmEth1)

lmEth2 <- lm(Continuance~Ethnicity, data = data2)
summary(lmEth2)

lmEth3 <- lm(Normative~Ethnicity, data = data2)
summary(lmEth3)

ggplot(data2) + 
  geom_point(mapping = aes(x = Justice, y = Normative)) + 
  facet_wrap(~ Ethnicity, nrow = 2) + 
  theme_bw()

#Group data by race and education

Asian <- filter(data2, Ethnicity == "Asian")
White <- filter(data2, Ethnicity == "White")
Black <- filter(data2, Ethnicity == "Black")

HS <- data2 %>%
  filter(Education == "High School")

Bachelors <- data2 %>%
  filter(Education == "Bachelors")

Graduate <- data2 %>%
  filter(Education == "Masters" | Education == "Doctorate")

Male <- data2 %>%
  filter(Gender == "Male")

Female <- data2 %>%
  filter(Gender == "Female")

#Test for Group Differences

t.test(Black$Normative, White$Normative, alternative='g', paired = FALSE)
t.test(Asian$Affective, White$Affective, alternative='g', paired = FALSE)
t.test(Asian$Continuance, White$Continuance, alternative='g', paired = FALSE)
t.test(Asian$Normative, White$Normative, alternative='g', paired = FALSE)
t.test(Asian$O, White$O, alternative='g', paired = FALSE)
t.test(Asian$C, White$C, alternative='g', paired = FALSE)
t.test(Asian$E, White$E, alternative='g', paired = FALSE)
t.test(Asian$A, White$A, alternative='g', paired = FALSE)
t.test(Asian$N, White$N, alternative='g', paired = FALSE)
t.test(HS$Justice, Bachelors$Justice, alternative='g', paired = FALSE)
t.test(Male$Justice, Female$Justice, alternative='g', paired = FALSE)

#no significant differences found based on race, gender, or education
