library(car)
library(tidyverse)
library(ggplot2)
library(emmeans)

headset.df <- read.csv('HW4_Data.csv')
headset.df <- headset.df %>% 
  mutate(Headset=factor(Headset),
         Age_Group=factor(Age_Group))

head(headset.df)



# PART 1:

grand_mean <- mean(headset.df$Immersion)

SS_A <- headset.df %>% 
  group_by(Headset) %>% 
  mutate(diff = (mean(Immersion)-grand_mean)^2) %>% 
  ungroup() %>% 
  select(diff) %>% 
  sum()

SS_B <- headset.df %>% 
  group_by(Age_Group) %>% 
  mutate(diff = (mean(Immersion)-grand_mean)^2) %>% 
  ungroup() %>% 
  select(diff) %>% 
  sum()

SS_E <- headset.df %>% 
  group_by(Headset,Age_Group) %>% 
  mutate(diff = (Immersion - mean(Immersion))^2) %>% 
  ungroup() %>% 
  select(diff) %>% 
  sum()

SS_AB <- headset.df %>% 
  group_by(Headset) %>% 
  mutate(Headset_mu = mean(Immersion)) %>% 
  ungroup() %>% 
  group_by(Age_Group) %>% 
  mutate(Age_Group_mu = mean(Immersion)) %>% 
  ungroup() %>% 
  group_by(Headset,Age_Group) %>% 
  mutate(diff = (mean(Immersion) - Headset_mu - Age_Group_mu + grand_mean)^2) %>% 
  ungroup() %>% 
  select(diff) %>% 
  sum()

p <- nlevels(headset.df$Headset)
q <- nlevels(headset.df$Age_Group)
n <- headset.df %>% 
  group_by(Age_Group,Headset) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  unique()

n <- as.numeric(n)

MS_A <- SS_A/(p - 1)
MS_B <- SS_B/(q - 1)
MS_AB <- SS_AB/((p - 1)*(q - 1))
MS_E <- SS_E/(p*q*(n-1))

# P_Value calculation
pf_headset <- pf(MS_A/MS_AB, p-1, p*q*(n-1), lower.tail = FALSE)
pf_Age_Group <- pf(MS_B/MS_AB, q-1, p*q*(n-1), lower.tail = FALSE)
pf_interaction <- pf(MS_AB/MS_E, (p-1)*(q-1), p*q*(n-1), lower.tail = FALSE)

custom.anovaTable <- data.frame(source=c('Headset','Age_Group','Headset:Age_Group','Residual'),
                                SS=round(c(SS_A,SS_B,SS_AB,SS_E),2),
                                df=c(p-1,q-1,(p-1)*(q-1),p*q*(n-1)),
                                MS=round(c(MS_A,MS_B,MS_AB,MS_E),2),
                                F.val=round(c(MS_A/MS_AB,MS_B/MS_AB,MS_AB/MS_E,NA),2),
                                P.val=round(c(pf_headset,pf_Age_Group,pf_interaction,NA),4))

custom.anovaTable

# JUST FOR CHECKING THE ANSWERS:
random.model <- aov(Immersion ~ Error(Headset*Age_Group), data=headset.df)
summary(random.model)



# PART 2:

headset_age_means <- headset.df %>% 
  group_by(Headset,Age_Group) %>% 
  summarise(mu = mean(Immersion),
            mu_se = sd(Immersion)/sqrt(n()))

ggplot(headset_age_means, aes(x=Headset, y=mu, color=Age_Group)) + 
  geom_errorbar(aes(ymin=mu-mu_se,ymax=mu+mu_se), 
                show.legend=F, width=.1, color='black') + 
  geom_point(size=2) + 
  geom_line(aes(group=Age_Group), show.legend = F, size=1.2) + 
  labs(y='Immersion', x='Headset', color='Age Group') + 
  theme_minimal()

# ALTERNATIVELY, we can see the interaction in another way below:

#ggplot(headset_age_means, aes(x=Age_Group, y=mu, color=Headset)) + 
#  geom_errorbar(aes(ymin=mu-mu_se,ymax=mu+mu_se), 
#                show.legend=F, width=.1, color='black') + 
#  geom_point(size=2) + 
#  geom_line(aes(group=Headset), show.legend = F, size=1.2) + 
#  labs(y='Immersion', x='Age Group', color='Headset') + 
#  theme_minimal()




# PART 3:

Headset.A <- subset(headset.df, Headset == 'A')
Headset.B <- subset(headset.df, Headset == 'B')
Headset.C <- subset(headset.df, Headset == 'C')

Age_group.ch <- subset(headset.df, Age_Group == 'Children')
Age_group.mi <- subset(headset.df, Age_Group == 'Middle')
Age_group.ol <- subset(headset.df, Age_Group == 'Older')
Age_group.yo <- subset(headset.df, Age_Group == 'Young')

summary(aov(Immersion ~ Age_Group, data=Headset.A))
summary(aov(Immersion ~ Age_Group, data=Headset.B))
summary(aov(Immersion ~ Age_Group, data=Headset.C))

summary(aov(Immersion ~ Headset, data=Age_group.ch))
summary(aov(Immersion ~ Headset, data=Age_group.mi))
summary(aov(Immersion ~ Headset, data=Age_group.ol))
summary(aov(Immersion ~ Headset, data=Age_group.yo))
