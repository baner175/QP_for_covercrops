rm(list = ls())
library(lme4)
library(MASS)
df <- read.csv('../QP_fit_data.csv', header = TRUE)
dim(df)
head(df)
df$YEAR <- as.factor(df$YEAR)
df$SITE <- as.factor(df$SITE)
df$COVERCROP <- as.factor(df$COVERCROP)
str(df)


# fixed: cover crop; random: year:site
m0 <- lmer(plateau_val ~ COVERCROP + (1|YEAR:SITE),
           data = df)
summary(m0)
boxcox(plateau_val ~ COVERCROP + SITE*YEAR,
       data = df, lambda = seq(-3,3,0.01)) # Needs no transformation
car::Anova(m0, test = 'F')
fixef(m0)

plot(plateau_val~COVERCROP, data = df,
     main = 'Distribution of Optimal Yield across different covercrops',
     xlab = 'Covercrop', ylab = 'Optimal Yield',
     col = c('darkgreen', 'blue', 'orange'))


# fixed: cover crop + site; random: year
m1 <- lmer(plateau_val ~ COVERCROP + SITE + (1|YEAR),
           data = df)
summary(m1)
car::Anova(m1, test = 'F')
boxcox(plateau_val ~ COVERCROP + SITE + YEAR,
       data = df, lambda = seq(0,5,0.01)) # Needs cube transformation
m1_trans <-  lmer(plateau_val^3 ~ COVERCROP + SITE + (1|YEAR),
                  data = df)
summary(m1_trans)
car::Anova(m1_trans, test = 'F')
fixef(m1_trans)
