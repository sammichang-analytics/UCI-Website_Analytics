library(dplyr)
library(ggplot2)
library(igraph)
library(psych)
library(corrplot)

#Import csv
setwd("/Users/isha_/Desktop")

#read in smp campaign data
smp_cd <- read.csv('Campaign Data.csv', header=T, stringsAsFactors=T)


lm_FB_UC <- lm(UniqueClicks ~ isFacebook, data = smp_cd)
summary(lm_FB_UC)

#linear regression on CTR as dependent and other variables as independent

linFB2<- lm( UniqueClicks ~ isFacebook + Reach + Impressions 
               + AmountSpent + PageEngagement + PageLikes + PostComments
                + PostReactions + PostShares
               + CampaignDuration , data = smp_cd)
summary(linFB2)

