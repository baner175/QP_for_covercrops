rm(list = ls())
library(lme4)
library(MASS)
library(multcomp)
df <- read.csv('../QP_fit_data.csv', header = TRUE)
dim(df)
head(df)
df$YEAR <- as.factor(df$YEAR)
df$SITE <- as.factor(df$SITE)
df$COVERCROP <- as.factor(df$COVERCROP)
str(df)


# fixed: cover crop; random: year:site
m0 <- lmer(c ~ COVERCROP + (1|YEAR:SITE),
           data = df)
summary(m0)
car::Anova(m0, test = 'F')
fixef(m0)

pair_wise <- matrix(c(0,1,0,
                      0,0,1,
                      0,1,-1), nrow = 3, byrow = TRUE)
rownames(pair_wise)<- c('NC - MC', 'SC - MC', 'NC - SC')

pair_wise_glht <- glht(m0, linfct = pair_wise,
                       adjust = 'tukey')
summary(pair_wise_glht)

diff_effect <- matrix(c(1,0,0,
                        1,1,0,
                        1,0,1), nrow = 3, byrow = TRUE)
rownames(diff_effect)<- c('MC', 'NC', 'SC')
diff_effect_glht <- glht(m0, linfct = diff_effect,
                         adjust = 'tukey')
summary(diff_effect_glht)

confint(diff_effect_glht, level = 0.67)
confint(diff_effect_glht, level = 0.95)

plot(c~COVERCROP, data = df,
     main = 'Distribution of Quadratic term across different covercrops',
     xlab = 'Covercrop', ylab = 'Quadratic term',
     col = c('darkgreen', 'blue', 'orange'))


# fixed: cover crop + site; random: year
m1 <- lmer(c ~ COVERCROP + SITE + (1|YEAR),
           data = df)
summary(m1)
car::Anova(m1, test = 'F')
fixef(m1)
