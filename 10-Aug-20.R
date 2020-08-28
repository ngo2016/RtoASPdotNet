library(RODBC)
cn <- odbcConnect("CNC", "truong", "z")
viewRD <- sqlFetch(cn, "V_RD")
odbcClose(cn)

library(tidyverse)
library(prophet)

library(geofacet)
library(ggplot2)

RD_data <- viewRD %>%
  select(TENDN_VT, ds = NAM, y = TY_LE_CHI_PHI_RD, KINH_PHI) %>%
  mutate(ds = paste(ds, "01", "01", sep = "-") %>% as.Date())

model_func <- function(RD_name){
  data <- RD_data %>% 
    filter(TENDN_VT == RD_name) %>% 
    select(-TENDN_VT)
  
  model <- prophet()
  model <- add_regressor(model, 'KINH_PHI')
  model <- fit.prophet(model, data)
  
  future <- make_future_dataframe(model, 1, freq = "year") %>%
    left_join((model$history %>%  select(ds, KINH_PHI)), by=c('ds'='ds'))
  
  future <- replace_na(future, list(KINH_PHI = mean(RD_data$KINH_PHI[RD_data$TENDN_VT == RD_name])))
  
  results <- predict(model, future) %>% 
    select(ds, yhat, yhat_upper, yhat_lower)
  
  return(results)
}

RD_list <- RD_data %>% select(TENDN_VT) %>% distinct()

results <- RD_list %>%
  mutate(results = map(TENDN_VT, model_func))

results %>%
  unnest() %>%
  select(TENDN_VT, ds, yhat, yhat_upper, yhat_lower) %>%
  mutate(ds = ds -2000) %>%
  ggplot(aes(x = ds, y = yhat, color = TENDN_VT)) +
  geom_ribbon(aes(ymax = yhat_upper, ymin = yhat_lower, fill = TENDN_VT), alpha = .1) +
  geom_line(color = "black") +
  facet_wrap(~TENDN_VT, scales = "free") +
  theme(legend.position = "none") +
  labs(title = "Du bao ty le chi RD", x = "Nam", y = "%")

####################################################
RD_DH_data <- viewRD %>%
  select(TENDN_VT, ds = NAM, y = TY_LE_DH_TREN_DH_THAM_GIA_RD, KINH_PHI) %>%
  mutate(ds = paste(ds, "01", "01", sep = "-") %>% as.Date())

model_DH_func <- function(RD_name){
  data <- RD_data %>% 
    filter(TENDN_VT == RD_name) %>% 
    select(-TENDN_VT)
  
  model <- prophet()
  model <- add_regressor(model, 'KINH_PHI')
  model <- fit.prophet(model, data)
  
  future <- make_future_dataframe(model, 1, freq = "year") %>%
    left_join((model$history %>% select(ds, KINH_PHI)), by=c('ds'='ds'))

  future <- replace_na(future, list(KINH_PHI = mean(RD_data$KINH_PHI[RD_data$TENDN_VT == RD_name])))
 
  results <- predict(model, future) %>% select(ds, yhat, yhat_upper, yhat_lower)
  
  return(results)
}

RD_DH_list <- RD_DH_data %>% select(TENDN_VT) %>% distinct()

DH_results <- RD_DH_list %>%
  mutate(DH_results = map(TENDN_VT, model_DH_func))

DH_results %>%
  unnest() %>%
  select(TENDN_VT, ds, yhat, yhat_upper, yhat_lower) %>%
  mutate(ds = ds -2000) %>%
  ggplot(aes(x = ds, y = yhat, color = TENDN_VT)) +
  geom_ribbon(aes(ymax = yhat_upper, ymin = yhat_lower, fill = TENDN_VT), alpha = .1) +
  geom_line(color = "black") +
  facet_wrap(~TENDN_VT, scales = "free") +
  theme(legend.position = "none") +
  labs(title = "Du bao ty le dh - tren dh tham gia RD", x = "Nam", y = "%")

DH_results[[2]][[15]] %>%
  ggplot(aes(x = ds, y = yhat)) +
  geom_ribbon(aes(ymax = yhat_upper, ymin = yhat_lower), alpha = .1) +
  geom_line(color = "black") +
  theme(legend.position = "none") +
  labs(title = "Du bao ty le dh - tren dh tham gia RD", x = "Nam", y = "%")

####################################################
library(RODBC)
cn <- odbcConnect("CNC", "truong", "z")
xuat_khau <- sqlFetch(cn, "XUAT_KHAU_CNC")
odbcClose(cn)

library(tidyverse)
library(prophet)
library(geofacet)
library(ggplot2)

data_xuat_khau <- xuat_khau %>%
  select(y = XUAT_KHAU, ds = NAM) %>%
  mutate(ds = paste(ds, "01", "01", sep = "-") %>% as.Date())

model_xuat_khau <- prophet(data_xuat_khau)

future_xuat_khau <- make_future_dataframe(model_xuat_khau, 1, freq = "year")

forecast_xuat_khau <- predict(model_xuat_khau, future_xuat_khau)

forecast_xuat_khau %>%
  select(ds, yhat, yhat_upper, yhat_lower) %>%
  ggplot(aes(x = ds, y = yhat)) +
  geom_ribbon(aes(ymax = yhat_upper, ymin = yhat_lower), alpha = .1) +
  geom_line(color = "red") +
  labs(title = "Du bao tinh hinh xuat khau khu cnc", x = "Nam", y = "Ngan ty USD")
