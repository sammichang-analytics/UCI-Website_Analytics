install.packages(c("survival", "survminer"))
library(survival)
library(survminer)
library(readxl)
library(readr)

# Import Data
setwd("~/Desktop/Winter 2021/Bana 277/project/final submit ")
data <- read_csv("Applicants Data.csv")

# Survival Analysis on Residency_intl
data$status <- rep(1,1239)
fit <- survfit(Surv(time_elapsed, status) ~ Residency_intl, data = data)
print(fit)

summary(fit)
summary(fit)$table

d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower)

ggsurvplot(fit, data = data,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))


# Survival Analysis on Lead_onlineapp
fit2 <- survfit(Surv(time_elapsed_since_app_window_opens, status) ~ Lead_onlineapp, data = data)
print(fit2)

summary(fit2)
summary(fit2)$table

d2 <- data.frame(time = fit2$time,
                n.risk = fit2$n.risk,
                n.event = fit2$n.event,
                n.censor = fit2$n.censor,
                surv = fit2$surv,
                upper = fit2$upper,
                lower = fit2$lower
)
head(d2)

ggsurvplot(fit2, data = data,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

# Survival Analysis on Programs_msba
fit3 <- survfit(Surv(time_elapsed_since_app_window_opens, status) ~ Programs_msba, data = data)
print(fit2)

summary(fit3)
summary(fit3)$table

d3 <- data.frame(time = fit3$time,
                 n.risk = fit3$n.risk,
                 n.event = fit3$n.event,
                 n.censor = fit3$n.censor,
                 surv = fit3$surv,
                 upper = fit3$upper,
                 lower = fit3$lower
)
head(d3)

ggsurvplot(fit3, data = data,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))


# Poisson Regression
data$app_submit_date

data1 <- subset(data, data$time_elapsed >= 0)


glm1 <- glm(time_elapsed ~ Programs_fin + Programs_mpa + Programs_mie + Residency_citizen +
               Lead_onlineapp + Lead_webinquiryform, family = poisson(), data = data1)
summary(glm1)
