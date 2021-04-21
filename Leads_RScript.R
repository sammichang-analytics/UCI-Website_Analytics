 library(mltools)
  library(data.table)
library(dplyr)
  library(plyr)
 #install.packages("ROSE")
 library(ROSE)
 library(stringr)
 library(hrbrthemes)
 library(ggplot2)
 
setwd("~/Desktop/Winter 2021/Bana 277/project/final submit")
dat = read.csv("Leads Data.csv")

#dummy var for gender
#dat$Salutation = ifelse(dat$Salutation== "Mrs.",0, 1)

colnames(dat) = c("Salutation", "date_first_visited", "Lead", "Stage", 
                  "pardot_created_date", "Conversion_object", "pardot_referrer_type",
                  "First_referrer", "Programs", "sheet", "X")
#dont need this cols
dat = subset(dat, select = -c(date_first_visited, pardot_referrer_type, X, sheet,First_referrer ))

#only want smp
smp_programs = c("Master of Finance", "Master of Innovation and Entrepreneurship",
                 "Master of Science in Business Analytics", "Master of Professional Accountancy")
dat<- dat %>% filter( Programs %in% smp_programs)

# 
# ##subset out empty rows 
# dat = subset(dat, first_referrer != "")

#deal with date
#dat$date_first_visited = as.POSIXct(dat$date_first_visited, format='%m/%d/%y')

dat$Lead = as.factor(dat$Lead)
dat$Stage = as.factor(dat$Stage)
dat$Conversion_object = as.factor(dat$Conversion_object)

dat$Program = as.factor(dat$Program)

dat$Salutation = as.factor(dat$Salutation)

dat<- one_hot(as.data.table(dat))


dat$Male = dat$Salutation_Mr. - dat$Salutation_Dr. - dat$Salutation_M.
dat$Male = replace( dat$Male, dat$Male < 0 , 0)


dat1 = dat %>% select("pardot_created_date", "Lead_Online Application", "Lead_Web Inquiry Form", "Male",
                      "Stage_Archive", "Stage_Inquiry", "Stage_Applicant", "Conversion_object_SMP Major Event Registration", 
                     "Conversion_object_Web Inquiry Form CTA","Conversion_object_Web Inquiry Form MFin",
                     "Conversion_object_Web Inquiry Form MIE","Conversion_object_Web Inquiry Form MPAC",
                     "Conversion_object_Web Inquiry Form MSBA", "Program_Master of Finance",
                     "Program_Master of Innovation and Entrepreneurship", 
                     "Program_Master of Professional Accountancy", "Program_Master of Science in Business Analytics")
colnames(dat1) = c("pardot_created_date", "lead_onlineapp", "lead_webinq", "male", "stage_archive", "stage_inq","stage_app",
                   "event_reg","CTA_form","MFin_form", "MIE_form",
                   "MPAC_form","MSBA_form", "program_fin", "program_mie", "program_mpac", "program_msba")

dat1$match1 = ifelse( (dat1$MFin_form + dat1$program_fin ==2),1,0)
dat1$match2 = ifelse(  (dat1$MIE_form +dat1$program_mie==2), 1,0)
dat1$match3 = ifelse( (dat1$MPAC_form + dat1$program_mpac==2), 1,0)
dat1$match4 = ifelse( (dat1$MSBA_form +dat1$program_msba==2), 1,0)

##this variable was insignificant. Matches is 1 if the program that the applicant 
#applied for was the same that they submitted the inquiry for.
dat1$matches = dat1$match1 + dat1$match2 +dat1$match3 + dat1$match4


##preprocess the dates
dat1$pardot_created_date = substr(dat1$pardot_created_date, 1, 8)
dat1$pardot_created_date = sub(" .", " ", dat1$pardot_created_date)
#lol
#dat1$pardot_created_date = sub("/$", "", dat1$pardot_created_date)
#dat1$pardot_created_date = sub("/1$", "", dat1$pardot_created_date)
#dat1$pardot_created_date = sub("/2$", "", dat1$pardot_created_date)

dat1$pardot_created_date = str_trim(dat1$pardot_created_date)

dat1$pardot_created_date = as.POSIXct(dat1$pardot_created_date, format = '%m/%d/%y')
##filter, we dont want 2020 data
dat1 = dat1 %>% filter(pardot_created_date <= as.POSIXct("2020-01-01"))


hist1 = dat1 %>% ggplot(aes(x = pardot_created_date)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) +xlab("Date") + ggtitle("Distribution of First Visits") +theme_ipsum() + 
  theme(
    plot.title = element_text(size=15)
  )


dat1$pardot_created_date = format( dat1$pardot_created_date, format="%m/%d")
dat1$pardot_created_date = as.Date(dat1$pardot_created_date, format = '%m/%d')
#abs value difference til october
dat1$time_til_oct =  floor(abs(difftime(dat1$pardot_created_date, c("2021-10-01"), units = "days")))
dat1$time_til_oct = as.numeric(dat1$time_til_oct)
dat1$time_til_oct = replace(dat1$time_til_oct, dat1$time_til_oct ==0, 1)

sum(dat1$event_reg)
##combine archive and applicant together 
dat1$arch_or_appl = dat1$stage_archive + dat1$stage_app

##oversample the data to balance classes
oversampling_result <- ovun.sample(stage_archive ~ ., data = dat1, method = "over", p = .5)
new_data = oversampling_result$data

oversampling_result <- ovun.sample(arch_or_appl ~ ., data = dat1, method = "over", p = .5)
new_data1 = oversampling_result$data

sum(new_data$matches)
sum(new_data$stage_archive)

library(viridis)

##plot density plots to see diff in dist between groups
dat2 = as.data.frame(dat1)
dat2$stage_archive = as.factor(dat1$stage_archive)
dat2 %>%
  ggplot( aes(x=lead_webinq, color=stage_archive, fill= stage_archive))+
  ggtitle("Density of Web Inquiry between Archives and Non-archives" ) +
  geom_bar(alpha=0.6) +
  xlab("Lead: Web Inquiry") +
  ylab("Density")

dat2$start_application = as.factor(dat1$stage_archive + dat1$stage_app)

dat2 %>%
  ggplot( aes(x=event_reg, color=start_application, fill= start_application))+
  ggtitle("Starting an Application vs. Registering for an Event" ) +
  geom_bar(alpha=0.6) +
  xlab("Event Registration") +
  ylab("Density")

dat2$stage_archive = as.factor(dat1$stage_archive)
dat2 %>%
  ggplot( aes(x=event_reg, color=stage_archive, fill= stage_archive))+
  ggtitle("Submitting an Application vs. Registering for an Event" ) +
  geom_bar(alpha=0.6) +
  xlab("Event Registration") +
  ylab("Density")

#prelim
regress_archive = glm(stage_archive  ~ male + lead_webinq + event_reg + CTA_form + MFin_form + MIE_form +
              MPAC_form + MSBA_form + log(time_til_oct), family = binomial(), data = new_data)
summary(regress_archive)



##regression to predict if the student submitted an application 
#regression results are different on every run because I randomly oversampled the data
regress_archive_reduced = glm(stage_archive  ~ male + lead_webinq +
                                log(time_til_oct), family = binomial(), data = new_data)
summary(regress_archive_reduced)

exp(coef(regress_archive_reduced))

##regression to predict if the student started an app
regress_archive_reduced1 = glm(arch_or_appl  ~ male + lead_webinq + event_reg+
                                log(time_til_oct), family = binomial(), data = new_data1)
summary(regress_archive_reduced1)

exp(coef(regress_archive_reduced1))





write.csv(dat1, file = "stage_archive.csv")

####START MODELING BELOW
