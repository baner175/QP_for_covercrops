rm(list = ls())

library(dplyr)
library(readxl)

qp <- function(x, a, b, jp) {
  c <- -0.5 * b / jp
  if_else(condition = x < jp,
          true  = a + (b * x) + (c * x * x),
          false = a + (b * jp) + (c * jp * jp))
}
qp <- Vectorize(qp)

fit_qp <- function(site, year, df){
  site_year <- paste0(site,'-',year)
  df1 <- df %>% filter(SITE_YEAR == site_year)
  mod_list <- list(NA, NA, NA)
  names(mod_list) <- unique(df$COVERCROP)
  
  print('-----------------------------------------------------------------------')
  print(paste0('Evaluating QP at: ', site_year))
  print('-----------------------------------------------------------------------')
  
  for(i in 1:3){
    trt <- unique(df$COVERCROP)[i]
    print(paste0('Working on Covercrop: ', trt))
    temp_data <- df %>% filter(SITE_YEAR == site_year,
                               COVERCROP == trt) %>% select(x = NITROGENRATE, y = YIELD)
    quad_mod <- lm(y~x + I(x^2), data = temp_data)
    a_start <- coef(quad_mod)[1]; b_start <- coef(quad_mod)[2]
    jp_start <- mean(temp_data$x)
    mod_res <- tryCatch({
      mod <- nls(y ~ qp(x, a, b, jp),
                 data = temp_data,
                 start = list(a = a_start, 
                              b = b_start, 
                              jp = jp_start),
                 upper = c(Inf, Inf, max(temp_data$x)),
                 lower = c(-Inf, -Inf, min(temp_data$x)),
                 algorithm = 'port',
                 control = nls.control(
                   maxiter = 200
                 ))
    }, error = function(e) NULL
    )
    mod_list[trt] <- list(list(
      model = mod_res,
      status = if (is.null(mod_res)) NULL else "Model converged with nls"
    ))
  }
  
  initial_null_count <- sapply(mod_list, function(t) is.null(t$model)) %>% sum()
  
  if(initial_null_count == 3){
    message('No models converged using nls; applying Brute Force')
    for(i in 1:3){
      trt <- unique(df$COVERCROP)[i]
      print(paste0('Working on Covercrop: ', trt))
      temp_data <- df %>% filter(SITE_YEAR == site_year,
                                 COVERCROP == trt) %>% select(x = NITROGENRATE, y = YIELD)
      quad_mod <- lm(y~x + I(x^2), data = temp_data)
      out <- summary(quad_mod)
      coef_df <- as.data.frame(out$coefficients[,1:2]) %>% mutate(upper = Estimate + 3*`Std. Error`,
                                                                  lower = Estimate - 3*`Std. Error`)
      
      starts <- expand.grid(
        a = seq(coef_df[1, 'upper'], coef_df[1, 'lower'], length = 10),
        b = seq(coef_df[2, 'upper'], coef_df[2, 'lower'], length = 10),
        jp = seq(quantile(temp_data$x, 0.3), 
                 quantile(temp_data$x, 0.9), length = 5)
      )
      mod_res <- nls2::nls2(y~qp(x, a, b, jp),
                            data = temp_data,
                            start = starts,
                            algorithm = "brute-force")
      
      mod_list[trt] <- list(list(model = mod_res,
                                 status = 'Used brute-force'))
    }
  } else if(initial_null_count == 0)
  {
    message('All models converged using nls :)')
  } else{
    working_model_index <- min(which(sapply(mod_list, function(t) !is.null(t$model))))
    working_model <- mod_list[[working_model_index]]$model
    working_coefs <- coef(working_model)
    non_working_model_index <- which(sapply(mod_list, function(t) is.null(t$model)))
    message(paste0(length(non_working_model_index),
                   ' model(s) did not converge with nls. Trying to fit with new start values'))
    
    for(i in non_working_model_index)
    {
      trt <- unique(df$COVERCROP)[i]
      print(paste0('Working on Covercrop: ', trt))
      temp_data <- df %>% filter(SITE_YEAR == site_year,
                                 COVERCROP == trt) %>% select(x = NITROGENRATE, y = YIELD)
      a_start <- working_coefs['a']; b_start <- working_coefs['b']
      jp_start <- working_coefs['jp']
      mod_res <- tryCatch({
        mod <- nls(y ~ qp(x, a, b, jp),
                   data = temp_data,
                   start = list(a = a_start,
                                b = b_start,
                                jp = jp_start),
                   upper = c(Inf, Inf, max(temp_data$x)),
                   lower = c(-Inf, -Inf, min(temp_data$x)),
                   algorithm = 'port',
                   control = nls.control(
                     maxiter = 200
                   ))
      }, error = function(e) NULL
      )
      status <- 'Model converged with nls'
      if(is.null(mod_res)){
        out <- summary(working_model)$parameters
        coef_df <- as.data.frame(out[,1:2]) %>% mutate(upper = Estimate + 2*`Std. Error`,
                                                       lower = Estimate - 3*`Std. Error`)
        
        starts <- expand.grid(
          a = seq(coef_df[1, 'upper'], coef_df[1, 'lower'], length = 10),
          b = seq(coef_df[2, 'upper'], coef_df[2, 'lower'], length = 10),
          jp = seq(quantile(temp_data$x, 0.3), 
                   quantile(temp_data$x, 0.9), length = 10)
        )
        mod_res <- nls2::nls2(y~qp(x, a, b, jp),
                              data = temp_data,
                              start = starts,
                              algorithm = "brute-force",
                              trace = FALSE)
        status <- 'Used brute-force'
      }
      mod_list[trt] <- list(list(model = mod_res,
                                 status = status))
    } 
  }
  print('-----------------------------------------------------------------------')
  return(mod_list)
}

extract_qp_coef <- function(site, year, df){
  mod_list <- fit_qp(site = site, year = year, df = df)
  mod_MC <- mod_list[['MC']]$model %>% coef()
  mod_NC <- mod_list[['NC']]$model %>% coef()
  mod_SC <- mod_list[['SC']]$model %>% coef()
  
  status_MC <- mod_list[['MC']]$status
  status_NC <- mod_list[['NC']]$status
  status_SC <- mod_list[['SC']]$status 
  
  res_mat <- rbind(mod_MC, mod_NC, mod_SC)
  res_mat <- as.data.frame(res_mat)
  colnames(res_mat) <- c("a", "b", "jp")
  res_mat["message"] <- c(status_MC, status_NC, status_SC)
  
  return(res_mat)
}