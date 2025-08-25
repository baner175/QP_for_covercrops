rm(list = ls())
source('necessay_functions.R')
qp_dat <- read.csv('QP_fit_data.csv', header = TRUE)
df <- read_excel('CoverCrop_Nrates_.xlsx')
site <- 'Beresford' # Beresford or Brookings
year <- 2019 # Anything between 2019 - 2024
qp_dat_sub <- qp_dat %>% filter(SITE == site, YEAR == year)
df1 <- df %>% filter(SITE == site, YEAR == year)
#-------------------------------------------------------------------------------

plot(y = df1$YIELD, x = df1$NITROGENRATE, type = 'n',
     xlab = 'Nitrogen rate', ylab = 'Corrn yield',
     ylim = c(min(df1$YIELD)-5000, max(df1$YIELD) + 2000),
     xlim = c(-1, max(df1$NITROGENRATE)),
     main = paste0('Qudratic Plateau plots for ', site, '-', year))

cols <- c('darkgreen', 'blue', 'orange')
legends <- sapply(unique(df$COVERCROP),
                  function(trt){
                    paste0(trt, ' - ', qp_dat_sub$message[qp_dat_sub$COVERCROP == trt])
                  })
pty <- c(18, 16, 17)

for(i in 1:3){
  trt <- unique(df$COVERCROP)[i]
  temp_data <- df1 %>% filter(COVERCROP == trt) %>% select(y = YIELD, x = NITROGENRATE)
  points(y = temp_data$y, x = temp_data$x,
         col = cols[i], pch = pty[i])
  coefs <- qp_dat_sub %>% filter(COVERCROP == trt) %>% select(a, b, c, jp, 'plateau_val')
  a <- coefs[1]; b <- coefs[2]; c <- coefs[3]; jp <- coefs[4]; h_val <- coefs[5]
  curve(qp(x, a, b, jp), col = cols[i],
        add = TRUE, lwd = 2)
  abline(v = jp, h = qp(jp, a, b, jp),
         col = cols[i],
         lty = 2, lwd = 2)
}

legend('bottomright',
       legend = legends,
       col = cols, pch = pty, cex = 1)
