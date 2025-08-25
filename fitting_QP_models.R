library(tidyr)

source('necessary_functions.R')

df <- read_excel('CoverCrop_Nrates_.xlsx')

df$SITE <- as.factor(df$SITE)
df$YEAR <- as.factor(df$YEAR)
df$COVERCROP <- as.factor(df$COVERCROP)
str(df)

# 6 levels of Nitorgen rates used in Baresford but 4 levels used in Brookings
# Each Site-year combination has #_replicates(4) X #_Covercrops (3) X #_Nrate (6 or 4) many observatiosn
df %>% group_by(SITE_YEAR) %>% summarise(n_obs = n())

sites <- unique(df$SITE)
years <- unique(df$YEAR)

# df_sub <- df %>% filter(YEAR %in% 2023, SITE == 'Brookings')


res <- df %>% 
  distinct(SITE, YEAR) %>%
  rowwise() %>% 
  mutate(coefs = list(extract_qp_coef(site = SITE, year = YEAR, df = df)))

res_long <- res %>%
  ungroup() %>%
  mutate(COVERCROP = list(c("MC", "NC", "SC"))) %>%
  unnest(c(coefs, COVERCROP)) %>%
  relocate(COVERCROP, .after = YEAR)

res_long <- res_long %>% 
  mutate('c' = -0.5*b/jp,
         'plateau_val' = a + b*jp + c*jp*jp) %>% 
  relocate(c, .after = b) %>% 
  relocate(plateau_val, .after = jp)

res_long

write.csv(res_long, 'QP_fit_data.csv',
          row.names = FALSE)
