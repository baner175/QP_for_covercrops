rm(list = ls())
library(lme4)
library(MASS)
df <- read.csv('../QP_fit_data.csv', header = TRUE)
df <- df[!(df$SITE == 'Brookings' & df$YEAR == 2020), ]
dim(df)
head(df)
df$YEAR <- as.factor(df$YEAR)
df$SITE <- as.factor(df$SITE)
df$COVERCROP <- as.factor(df$COVERCROP)
str(df)


# fixed: cover crop; random: year:site
m0 <- lmer(a ~ COVERCROP + (1|YEAR:SITE),
           data = df)
summary(m0)
boxcox(a ~ COVERCROP + SITE*YEAR,
       data = df, lambda = seq(-3,3,0.01)) # Needs no transformation
car::Anova(m0, test = 'F')
fixef(m0)

plot(a~COVERCROP, data = df,
     main = 'Distribution of Intercept term across different covercrops',
     xlab = 'Covercrop', ylab = 'Intercept',
     col = c('darkgreen', 'blue', 'orange'))


# fixed: cover crop + site; random: year
m1 <- lmer(a ~ COVERCROP + SITE + (1|YEAR),
           data = df)
summary(m1)
car::Anova(m1, test = 'F')
boxcox(a ~ COVERCROP + SITE + YEAR,
       data = df, lambda = seq(-5,3,0.01)) # Needs inverse transformation
m1_trans <-  lmer(1/a ~ COVERCROP + SITE + (1|YEAR),
                  data = df)
summary(m1_trans)
car::Anova(m1_trans, test = 'F')
fixef(m1_trans)
