library(RODBC)
cn <- odbcConnect("CNC", "truong", "z")
viewRD <- sqlFetch(cn, "V_RD")
odbcClose(cn)

library(tidyverse)
library(prophet)
library(geofacet)
library(ggplot2)

viewRD %>% 
  ggplot(aes(x = NAM, y = TY_LE_CHI_PHI_RD, color = "red"))+
  geom_line()+
  facet_wrap(~TEN_DU_AN_TIENG_VIET, scales = "free", ncol = 4)+
  theme(legend.position = "none")+
  ggthemes::theme_economist()

viewRD %>% class() 

Encoding(viewRD$TEN_DU_AN_TIENG_VIET) <- "unicode"

modelData <- viewRD %>%
  select(TEN_DU_AN_TIENG_VIET, NAM, TY_LE_CHI_PHI_RD) %>%
  mutate(month = "01", 
         year = as.character(NAM), 
         ds = paste(NAM, "01", "01", sep = "-") %>% as.Date()) %>%
  select(TEN_DU_AN_TIENG_VIET, ds, TY_LE_CHI_PHI_RD)

key <- modelData %>%
  select(TEN_DU_AN_TIENG_VIET)%>%
  distinct()%>%
  mutate(name = c("Nidec Servo", "Nidec Sankyo", "Gen", "Datalogic", 
                  "T.C", "MedicRD", "ElectricStuff", "IntellProduct", 
                  "Sonion", "LED", "Samsung", "Healthcare", "Automatic", 
                  "Platel", "Jabil", "ElectricProduct"))

modelData %>%
  ggplot(aes(x = ds, y = TY_LE_CHI_PHI_RD, color = TEN_DU_AN_TIENG_VIET))+
  geom_smooth()+
  geom_point()+
  facet_wrap(~TEN_DU_AN_TIENG_VIET, scales = "free")+
  theme(legend.position = "none")

modelResult <- data.frame()

for (i in 1:16) {
  model <- modelData %>%
    filter(TEN_DU_AN_TIENG_VIET == key$TEN_DU_AN_TIENG_VIET[i]) %>%
    select(ds, y = TY_LE_CHI_PHI_RD) %>%
    prophet()
  
  assign(paste(key$name[i], "model", sep = "_"), model)
  
  future <- make_future_dataframe(model, periods = 365)
  
  result <- predict(model, future) %>%
    select(ds, yhat_lower, yhat_upper, yhat) %>%
    mutate(TEN_DU_AN_TIENG_VIET = key$TEN_DU_AN_TIENG_VIET[i],
           ds = as.Date(ds)) %>%
    inner_join(modelData, by = c("TEN_DU_AN_TIENG_VIET" = "TEN_DU_AN_TIENG_VIET", "ds" = "ds"))
  
  modelResult <- rbind(modelResult, result)
}

modelResult %>%
  ggplot(aes(x = ds, y = TY_LE_CHI_PHI_RD, color = TEN_DU_AN_TIENG_VIET))+
  geom_line(color = "black")+
  geom_line(aes(x = ds, y = yhat, color = TEN_DU_AN_TIENG_VIET))+
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper, fill = TEN_DU_AN_TIENG_VIET), alpha = .1)+
  facet_wrap(~TEN_DU_AN_TIENG_VIET, scales = "free")+
  theme(legend.position = "none")+
  labs(title = "Du bao Ty le chi RD cua du an trong khu cnc")

forecast <- viewRD %>%
  select(TEN_DU_AN_TIENG_VIET, ds = NAM, y = TY_LE_CHI_PHI_RD)%>%
  mutate(ds = paste(ds, "01", "01",sep = "-") %>% as.Date())%>%
  group_by(TEN_DU_AN_TIENG_VIET)%>%
  do(model = prophet(.),
     future = make_future_dataframe(model, 1, freq = "year"),
     result = predict(model, future))

forecast %>%
  unnest(result)%>%
  select(TEN_DU_AN_TIENG_VIET, ds, yhat, yhat_upper, yhat_lower)%>%
  mutate(ds = ds -2000)%>%
  ggplot(aes(x = ds, y = yhat, color = TEN_DU_AN_TIENG_VIET))+
  geom_ribbon(aes(ymax = yhat_upper, ymin = yhat_lower, fill = TEN_DU_AN_TIENG_VIET), alpha = .1)+
  geom_line(color = "black")+
  facet_wrap(~TEN_DU_AN_TIENG_VIET, scales = "free")+
  theme(legend.position = "none")+
  labs(title = "Du bao ty le chi RD")

####################################################
RD_data <- viewRD %>%
  select(TEN_DU_AN_TIENG_VIET, ds = NAM, y = TY_LE_CHI_PHI_RD) %>%
  mutate(ds = paste(ds, "01", "01", sep = "-") %>% as.Date())

model_func <- function(RD_name){
  data <- RD_data %>% filter(TEN_DU_AN_TIENG_VIET == RD_name) %>% select(-TEN_DU_AN_TIENG_VIET)
  
  model <- prophet(data)
  
  future <- make_future_dataframe(model, 1, freq = "year")
  
  results <- predict(model, future) %>% select(ds, yhat, yhat_upper, yhat_lower)
  
  return(results)
}

RD_list <- RD_data %>% select(TEN_DU_AN_TIENG_VIET) %>% distinct()
results <- RD_list %>%
  mutate(results = map(TEN_DU_AN_TIENG_VIET, model_func))

results %>%
  unnest() %>%
  select(TEN_DU_AN_TIENG_VIET, ds, yhat, yhat_upper, yhat_lower) %>%
  mutate(ds = ds -2000) %>%
  ggplot(aes(x = ds, y = yhat, color = TEN_DU_AN_TIENG_VIET)) +
  geom_ribbon(aes(ymax = yhat_upper, ymin = yhat_lower, fill = TEN_DU_AN_TIENG_VIET), alpha = .1) +
  geom_line(color = "black") +
  facet_wrap(~TEN_DU_AN_TIENG_VIET, scales = "free") +
  theme(legend.position = "none") +
  labs(title = "Du bao ty le chi RD", x = "Nam", y = "%")

####################################################
RD_DH_data <- viewRD %>%
  select(TEN_DU_AN_TIENG_VIET, ds = NAM, y = TY_LE_DH_TREN_DH_THAM_GIA_RD) %>%
  mutate(ds = paste(ds, "01", "01", sep = "-") %>% as.Date())

model_DH_func <- function(RD_name){
  data <- RD_DH_data %>% filter(TEN_DU_AN_TIENG_VIET == RD_name) %>% select(-TEN_DU_AN_TIENG_VIET)
  
  model <- prophet(data)
  
  future <- make_future_dataframe(model, 1, freq = "year")
  
  results <- predict(model, future) %>% select(ds, yhat, yhat_upper, yhat_lower)
  
  return(results)
}

RD_DH_list <- RD_DH_data %>% select(TEN_DU_AN_TIENG_VIET) %>% distinct()
DH_results <- RD_DH_list %>%
  mutate(DH_results = map(TEN_DU_AN_TIENG_VIET, model_DH_func))

DH_results %>%
  unnest() %>%
  select(TEN_DU_AN_TIENG_VIET, ds, yhat, yhat_upper, yhat_lower) %>%
  mutate(ds = ds -2000) %>%
  ggplot(aes(x = ds, y = yhat, color = TEN_DU_AN_TIENG_VIET)) +
  geom_ribbon(aes(ymax = yhat_upper, ymin = yhat_lower, fill = TEN_DU_AN_TIENG_VIET), alpha = .1) +
  geom_line(color = "black") +
  facet_wrap(~TEN_DU_AN_TIENG_VIET, scales = "free") +
  theme(legend.position = "none") +
  labs(title = "Du bao ty le dh - tren dh tham gia RD", x = "Nam", y = "%")
