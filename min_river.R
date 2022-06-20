setwd("C:/Users/wangjinz/Downloads/update")
list.mgt = list.files(pattern = "\\.mgt$")
file.copy(list.mgt,"F:/wangjinz/lexi/TxtInOut",overwrite = TRUE)

# for denethor
setwd("F:/wangjinz/lexi/TxtInOut")
path <- "F:/wangjinz/lexi/TxtInOut"
# for analytix 1
setwd("H:/TxtInOut")
path <- "H:/TxtInOut"

Packages <- c("SWATplusR","purrr","sf","tidyr","fast","dplyr","ggplot2", "sensitivity",
              "lhs","hydromad","lubridate","hydroGOF","forcats","EcoHydRology","magrittr")
lapply(Packages, library, character.only = TRUE)



# Path to the subbasin shape file
sub_path <- paste(path,"/Shapes/subs1.shp",sep = "")
riv_path <- paste(path,"/Shapes/riv1.shp",sep = "")
# Path to run model 
 # for denethor
run_path_model = "F:/wangjinz/lexi/run_model"
 # for analytix 1
run_path_model = "E:/Jinzhu_WANG/lexi/run_model"

# Read shapefile from the path
sub <- read_sf(sub_path)
riv <- read_sf(riv_path)
ggplot() +
  geom_sf(data = sub) +
  geom_sf(data = riv, col = "royalblue", lwd = 0.75) +
  geom_sf_label(data = riv, aes(label = Subbasin)) +
  theme_bw()


# Read observed data and change format
q_obs_min_o <- read.csv("F:/wangjinz/lexi/stfM/minQ29.csv")
minQ65 <- read.csv("F:/wangjinz/lexi/stfM/minQ65.csv")
q_obs_min_o$date <- as.Date(as.character(q_obs_min_o$date),format ="%Y-%m-%d")
min_river_sc1 = read.csv("F:/wangjinz/lexi/stfM/minQa65.csv",col.names = c("date","nh4"))
min_river_sc1$date <- as.Date(min_river_sc1$date)
obs_min_week = min_river_sc1 %>% 
  mutate(week = as.integer(strftime(date,format = "%Y%V")))
obs_min_week_s <- obs_min_week %>% 
  filter(date <= "2016-12-31")
  


#### Step 1 model simulation ####
sim_min_1960 <- run_swat2012(project_path = path,
                             start_date = "1962-01-01",
                             end_date = "2018-12-31",
                             output = list(nh4_out = define_output(file = "rch",
                                                                   variable = "NH4_OUT",
                                                                   unit = 1:91),
                                           q_out = define_output(file = "rch",
                                                                 variable = "FLOW_OUT",
                                                                 unit = 1:91)),
                             run_path = run_path_model)


###===
df = sim_min_1960 %>% 
  select(date,q_out_72,nh4_out_72) %>% 
  mutate(q = (nh4_out_72/q_out_72)/86.4) %>%
  filter(date >= "2005-01-01")

df = df %>%
  mutate(week = as.integer(strftime(date, format = "%Y%V")))
df_week = aggregate(q~week, data = df, mean)

nh_plot <- df_week %>%
  select(week, q) %>%
  dplyr::rename(nh_sim = q) %>%
  right_join(., obs_min_week_s, by = "week") %>%
  dplyr::rename(nh_obs = nh4) %>%
  gather(., key = "variable", value = "nh4",-week,-date)

ggplot(data = nh_plot) +
  geom_line(aes(x = date, y = nh4, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("concentration ("~mg/l~")"))+
  ylim(0,5)+
  theme_bw()

######
qa <- sim_min_1960[,73]
qu <- sim_min_1960[,164]
date <- sim_min_1960[,1]
qa_sim <- (qa/qu)/86.4
qa_sim <- data.frame(date,qa_sim)
qa_sim = qa_sim %>% 
  filter(date >= "2008-01-01")

qa_sim = qa_sim %>%
  mutate(week = as.integer(strftime(date, format = "%Y%V")))
qa_sim = data.frame(qa_sim)
qa_sim_week = aggregate(qa_sim[,2],list(qa_sim$week), mean)
colnames(qa_sim_week)[1:2] = c("week","q")

nh_plot <- qa_sim_week %>%
  select(week, q) %>%
  dplyr::rename(nh_sim = q) %>%
  right_join(., obs_min_week_s, by = "week") %>%
  dplyr::rename(nh_obs = nh4) %>%
  gather(., key = "variable", value = "nh4",-week,-date)

ggplot(data = nh_plot) +
  geom_line(aes(x = date, y = nh4, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("concentration ("~mg/l~")"))+
  ylim(0,5)+
  theme_bw()


##

plot(sim_min_1960$q_out_30,ylim = c(0,1000))
ggplot() +
  geom_sf(data = sub) +
  geom_sf(data = riv, col = "royalblue", lwd = 0.75) +
  geom_sf_label(data = riv, aes(label = Subbasin)) +
  theme_bw()

###water quantity performance first running
q_obs_min <- filter(q_obs_min_o, date >= ymd("1985-01-01"),
                    date <= "2008-12-31")
q_sim_min <- filter(sim_min_1960, date >= ymd("1985-01-01"),
                         date <= "2008-12-31")

q_plot <- q_sim_min %>%
  select(date, q_out_27) %>%
  dplyr::rename(q_sim = q_out_27) %>%
  left_join(., q_obs_min, by = "date") %>%
  dplyr::rename(q_obs = discharge) %>%
  gather(., key = "variable", value = "discharge",-date)

ggplot(data = q_plot) +
  geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("streamflow ("~m^3/s~")"))+
  theme_bw()

nse_test <- q_sim_min%>%
  select(-date) %>%
  map_dbl(., ~NSE(.x, q_obs_min$discharge))
head(sort(nse_test, decreasing = TRUE))

plot(q_obs_maidilong$discharge,q_sim_2012$FLOW_OUT_10)
r_sim <- lm(q_obs_maidilong$discharge ~ q_sim_2012$FLOW_OUT_10)
summary(r_sim)

#### Step 2 Sensitivity analysis ####
par_names_all <- c("ESCO.hru| change = absval",
                   "ALPHA_BNK.rte | change = absval",
                   "CH_N2.rte | change = absval",
                   "CH_K2.rte | change = absval",
                   "SOL_ORGN.chm | change = absval",
                   "SOL_NO3.chm | change = absval",
                   "BC1.swq | change = absval",
                   "BC3.swq | change = absval",
                   "RS3.swq | change = absval",
                   "SOL_BD.sol | change = absval",
                   "SOL_AWC.sol | change = absval",
                   "SOL_K.sol | change = absval",
                   "GWQMN.gw | change = absval",
                   "ALPHA_BF.gw | change = absval",
                   "GW_REVAP.gw | change = absval",
                   "GW_DELAY.gw | change = absval",
                   "REVAPMN.gw | change = absval",
                   "CN2.mgt | change = absval")

par_fast_all <- fast_parameters(
  minimum = c(0, 0, 0, 0.01, 0,    0, 0, 0.2,
              0, 1.1, 0, 0,    0, 0, 0.02, 0,  0, 0),
  maximum = c(1, 0.5,  0.4,  200, 100, 300, 0.1, 0.4,
              1, 1.9,  1, 2000, 5000,   1, 0.2,  200, 1000, 100),
  names = par_names_all)

q_fast_all_t <- run_swat2012(project_path = path,
                             output = list(
                               q_out = define_output(file = "rch",
                                                     variable = "FLOW_OUT",
                                                     unit = c(21,22,24,27,29,38,
                                                              41,46,62,77,82)),
                               nh_out = define_output(file = "rch",
                                                      variable = "NH4_OUT",
                                                      unit = c(21,22,24,27,29,38,
                                                               41,46,62,77,82))),
                             parameter = par_fast_all,
                             start_date = "1962-01-01",
                             end_date = "2018-12-31",
                             run_path = run_path_model,
                             years_skip = 2,
                             n_thread = 6)


# * 2.1 water quantity ------------------------------------------------------

saveRDS(q_fast_all_t,"sensitivities.rds")
q_obs <- filter(q_obs_min, date >= ymd("1985-01-01"), date <= "2008-12-31")
q_sim <- filter(q_fast_all_t$simulation$q_out_29, date >= ymd("1987-01-01"),
                date <= "2008-12-31")
nse_fast <- q_sim %>%
  select(-date) %>%
  map_dbl(., ~NSE(.x, q_obs$discharge))
head(sort(nse_fast,decreasing = TRUE),n=20)
q_plot <- q_sim %>%
  select(date, run_1458) %>%
  dplyr::rename(q_sim = run_1458) %>%
  left_join(., q_obs, by = "date") %>%
  dplyr::rename(q_obs = discharge) %>%
  gather(., key = "variable", value = "discharge",-date)

ggplot(data = q_plot) +
  geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("streamflow ("~m^3/s~")"))+
  scale_y_continuous()+
  theme_bw()
sens_fast <- sensitivity(nse_fast, 18)
result_fast <- tibble(parameter = q_fast_all_t$parameter$definition$par_name,
                      fast = sens_fast) %>%
  mutate(parameter = factor(parameter) %>% 
  fct_reorder(., fast))

ggplot(data = result_fast) +
  geom_col(aes(x = parameter, y = fast)) +
  xlab("Parameter for water yield") +
  ylab("Sensitivity") +
  coord_flip() +
  theme_bw()

dotty <- q_fast_all_t$parameter$values %>%
  mutate(nse = nse_fast) %>%
  filter(nse > 0) %>%
  gather(key = "par", value = "parameter_range", -nse)

ggplot(data = dotty) +
  geom_point(aes(x = parameter_range, y = nse)) +
  facet_wrap(.~par, ncol = 4, scales = "free_x") +
  theme_bw()

# * 2.2 water quality -------------------------------------------------------
min_river_sc1 = read.csv("min_river_sc.1.csv",col.names = c("date","nh4"))
min_river_sc1$date <- as.Date(min_river_sc1$date)
obs_min_d <- min_river_sc1 %>% 
  filter(date <= "2017-01-01")
sim_min = q_fast_all_t$simulation$nh_out_65
sim_qu = q_fast_all_t$simulation$q_out_65
sim_nh = (sim_min[,-1]/sim_qu[,-1])/86.4
sim_nh$date = sim_qu$date
sim_nh = sim_nh %>% 
  select(date,everything())
sim_min_week = sim_nh %>%
  mutate(week = as.integer(strftime(date, format = "%Y%V")))
sim_min_week = round(aggregate(sim_min_week[,2:ncol(sim_min_week)],list(sim_min_week$week),mean),2)
obs_min_week = min_river_sc1 %>% 
  mutate(week = as.integer(strftime(date,format = "%Y%V"))) %>% 
  filter(date < '2016-12-31')
all_min_week = left_join(obs_min_week,sim_min_week,by = "week")
sim_min_list = list(date = all_min_week[,3],q_nh4 = all_min_week[,4:(ncol(sim_min_week)+1)])

nse_fast_nh4 <- sim_min_list$q_nh4 %>%
  map_dbl(., ~NSE(.x, obs_min_week$nh4))
head(sort(nse_fast_nh4,decreasing=TRUE))
sens_fast_nh4 <- sensitivity(nse_fast_nh4, 18)
result_fast_nh4 <- tibble(parameter = q_fast_all_t$parameter$definition$par_name,
                          fast = sens_fast_nh4) %>%
  mutate(parameter = factor(parameter) %>% fct_reorder(., fast))


qa_plot <- sim_min_week %>%
  select(week, run_0413) %>%
  dplyr::rename(qa_sim = run_0413) %>%
  right_join(., obs_min_week, by = "week") %>%
  dplyr::rename(qa_obs = nh4) %>% 
  select(-week) %>% 
  select(date,qa_sim,qa_obs) %>% 
  gather(key = "variable", value = "nh",2:3)

ggplot(data = qa_plot) +
  geom_line(aes(x = date, y = nh, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("nh4 ("~mg/l~")"))+
  ylim(0,10)+
  ggtitle("0063")+
  theme_bw()

ggplot(data = result_fast_nh4) +
  geom_col(aes(x = parameter, y = fast)) +
  xlab("Parameter for NH4") +
  ylab("Sensitivity") +
  coord_flip() +
  theme_bw()

dotty <- q_fast_all_t$parameter$values %>%
  mutate(nse = nse_fast_nh4) %>%
  filter(nse > -2) %>% 
  gather(key = "par", value = "parameter_range", -nse)

ggplot(data = dotty) +
  geom_point(aes(x = parameter_range, y = nse)) +
  facet_wrap(.~par, ncol = 4, scales = "free_x") +
  theme_bw()


#### Step 3 Calibrate ####======================================================

# * 3.1 water quantity ----------------------------------------------------------

par_test_yie <- tibble("ESCO.hru| change = absval" = c(0,1),
                       "ALPHA_BNK.rte | change = absval"= c(0,0.3),
                       "CH_N2.rte | change = absval" = c(0,0.4),
                       "CH_K2.rte | change = absval" = c(50,200),
                       "SOL_NO3.chm | change = absval" = c(0,300),
                       "SOL_BD.sol | change = absval" = c(1.1,1.9),
                       "SOL_AWC.sol | change = absval" = c(0,0.5),
                       "SOL_K.sol | change = absval" = c(0,2000),
                       "GWQMN.gw | change = absval" = c(0,5000),
                       "ALPHA_BF.gw | change = absval" = c(0.25,1),
                       "GW_DELAY.gw | change = absval" = c(0,200),
                       "CN2.mgt | change = absval" = c(0,100))
                       

n_sample = 10
n_par = ncol(par_test_yie)
par_yie <- randomLHS(n = n_sample, k = n_par) %>%
  as_tibble(., .name_repair = "minimal") %>%
  set_names(names(par_test_yie))%>%
  map2_df(., par_test_yie, ~ (.x * (.y[2] - .y[1]) + .y[1])) 
test_d <- run_swat2012(project_path = path,
                           output = list(
                             q_out= define_output(file = "rch",
                                                  variable = "FLOW_OUT",
                                                  unit = 1:91)),
                           output_interval = "d",
                           parameter = par_yie,
                           start_date = "1962-01-01",
                           end_date = "2008-12-31",
                           years_skip = 2,
                           n_thread = 2,
                           run_path = run_path_model)

q_obs_min_test <- filter(q_obs_min, date >= "1985-01-01",date <= "2008-12-31")
q_obs_min_test$date = as.Date(q_obs_min_test$date)
q_sim_min <- filter(test_d$simulation$q_out_33, date >= "1985-01-01",date <= "2008-12-31")

nse_yie_test <- q_sim_min %>%
  select(-date) %>%
  map_dbl(., ~NSE(.x, q_obs_min_test$discharge))
head(sort(nse_yie_test, decreasing = TRUE),n=20)


yie_plot <- q_sim_min %>%
  select(date,run_04) %>%
  dplyr::rename(q_sim = run_04) %>%
  left_join(., q_obs_min_test, by = "date") %>%
  dplyr::rename(q_obs = discharge) %>%
  gather(., key = "variable", value = "discharge",-date)

ggplot(data = yie_plot) +
  geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  ylim(0,2000)+
  theme_bw()


# q_obs_bf <- BaseflowSeparation(q_obs_min_test$discharge, filter_parameter = 0.925, passes = 3)
# q_sim_min_bf <- q_sim_min[,-1]
# df = apply(q_sim_min_bf,2,function(x) {
#   BaseflowSeparation(x,filter_parameter = 0.925, passes = 3)
# })
# q_sim_bf <- as.data.frame(sapply(df,"[[",1))
# nse_bf <-  map_dbl(q_sim_bf, ~NSE(.x, q_obs_bf$bt))
# head(sort(nse_bf, decreasing = TRUE),n=10)

q = test_d$simulation$q_out_65
qa = test_d$simulation$nh_out_65
df_plot = q %>% 
  left_join(.,qa,by="date") %>% 
  filter(date >= "2007-01-01") %>%   
  filter(date <= "2016-12-31") %>% 
  mutate(qa_sim = (run_2.y/run_2.x)/86.4) %>% 
  select(date,qa_sim) %>% 
  right_join(.,obs_min_week_s,by="date") %>% 
  dplyr::rename(qa_obs = nh4) %>%
  gather(., key = "variable", value = "nh4",-date)
ggplot(data = df_plot) +
  geom_line(aes(x = date, y = nh4, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  ylim(0,5)+
  theme_bw()

# * 3.2 water quality -----------------------------------------------------------

par_min_nh4 <- tibble("ESCO.hru| change = absval" = 0.146,
                      "ALPHA_BNK.rte | change = absval"= 0.202,
                      "CH_N2.rte | change = absval" = 0.101,
                      "CH_K2.rte | change = absval" = 123,
                      "SOL_NO3.chm | change = absval" = 93.5,
                      "SOL_BD.sol | change = absval" = 1.61,
                      "SOL_AWC.sol | change = absval" = 0.03,
                      "SOL_K.sol | change = absval" = 740,
                      "GWQMN.gw | change = absval" = 1436,
                      "ALPHA_BF.gw | change = absval" = 0.701,
                      "GW_DELAY.gw | change = absval" = 183.58,
                      "CN2.mgt | change = absval" = 22.98,
                      "BC1.swq | change = absval" = c(0.1,1),
                      "BC3.swq | change = absval" = c(0.2,0.4),
                      "RS3.swq | change = absval" = c(0,1),
                      "SOL_ORGN.chm | change = absval" = c(0,100)
                      )

n_sample = 10
n_par_nh4 = ncol(par_min_nh4)
par_nh4 <- randomLHS(n = n_sample, k = n_par_nh4) %>% 
           as_tibble(par_nh4,.name_repair = "minimal") %>%  
           set_names(names(par_min_nh4)) %>% 
           map2_df(., par_min_nh4, ~ (.x * (.y[2] - .y[1]) + .y[1])) 
nh4_min_d <- run_swat2012(project_path = path,
                           output = list(nh_out = define_output(file = "rch",
                                                               variable = "NH4_OUT",
                                                               unit = 1:91),
                                         q_out = define_output(file = "rch",
                                                               variable = "FLOW_OUT",
                                                               unit = 1:91)),
                           output_interval = "d",
                           parameter = par_nh4,
                           start_date = "2005-01-01",
                           end_date = "2016-12-31",
                           years_skip = 2,
                           n_thread = 1)
#** 3.2.1 nse single -------------------------------------------------------------
date <- nh4_min_d[["simulation"]][["q_out_72"]][["date"]]
q_out <- nh4_min_d[["simulation"]][["q_out_72"]]["run_0235"]
quan_72 <- data_frame(date,q_out)
quan_72$date <- as.Date(quan_74$date)
all_74 <- left_join(min_river_sc1,quan_74,by="date")
qua_74 <- all_74 %>% 
  mutate_at(vars(matches("run_")),funs("load" = nh4*.*36*24/10)) %>% 
  select(date,ends_with("load"))
nse_nh4 <- nh4_min_d %>%
  select(-date) %>%
  map_dbl(., ~NSE(.x, obs_min_week$nh4))
head(sort(nse_yie_test, decreasing = TRUE),n=20)

nh4_sim_w <- left_join(timeseries,nh4_min_d$simulation$q_out_65...1,by="date")
nse_nh4 <- nh4_sim_w %>%
  select(-date) %>%
  map_dbl(., ~NSE(.x, qua_74$load))
head(sort(nse_nh4, decreasing = TRUE),n=20)

df_nse_nh4 <- bind_rows(nse_nh4)
which(df_nse_nh4 == max(df_nse_nh4), arr.ind = TRUE)
nh4_plot <- nh4_sim_w %>%
  select(date, run_051) %>%
  dplyr::rename(nh4_sim = run_051) %>%
  left_join(., qua_74, by = "date") %>%
  dplyr::rename(nh4_obs = load) %>%
  gather(., key = "variable", value = "nh4",-date)
ggplot(data = nh4_plot) +
  geom_line(aes(x = date, y = nh4, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("nh4 (kg)"))+
  ylim(0,500000)+
  theme_bw()


###
yie_test_d_s<- filter(yie_test_d$simulation$q_out_25, date >= "2007-01-01",
                      date <= "2016-12-31")
quan_25 <- yie_test_d_s$run_1
qua_25 <- nh4_min_d[["simulation"]][["nh_out_65"]][["run_1"]]
nh4_25 <- (qua_25/quan_25)/86.4
nh4_25 <- round(nh4_25,digits = 3)
nh4_25 <- data.frame(nh4_25,date2)
nh4_25_week = nh4_25 %>%
  mutate(week = as.integer(strftime(date, format = "%Y%V"))) %>% 
  group_by(week) %>% 
  summarise_at(vars(nh4_25),list(nh4_sim=mean))
nh4_25_week$nh4_sim = round(nh4_25_week$nh4_sim,digits = 3)
nh4_plot_25 <- obs_min_week %>%
  left_join(., nh4_25_week, by = "week") %>% 
  dplyr::rename(nh4_obs = nh4) %>% 
  select(date,nh4_obs,nh4_sim) %>% 
  gather(., key = "variable", value = "nh4",-date)%>%
  filter(date < "2017-01-01")
ggplot(data = nh4_plot_25) +
  geom_line(aes(x = date, y = nh4, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("tn"))+
  ylim(0,5)+
  theme_bw()


# ** 3.2.2 brett's suggestion --------------------------------------------
#### --quanlity ####
q_sim_min <- filter(nh4_min_d$simulation$q_out_25, date >= "1985-01-01",
                    date <= "2008-12-31")
q_obs_min_test <- filter(q_obs_min, date >= "1985-01-01",date <= "2008-12-31")
q_obs_min_test$date = as.Date(q_obs_min_test$date)

nse_yie_test <- q_sim_min %>%
  select(-date) %>% 
  map_dbl(., ~NSE(.x, q_obs_min_test$discharge))
head(sort(nse_yie_test, decreasing = TRUE),n=20)

q_obs_bf <- BaseflowSeparation(q_obs_min_test$discharge, filter_parameter = 0.925, passes = 3)

df = apply(q_sim_min_bf,2,function(x) {
  BaseflowSeparation(x,filter_parameter = 0.925, passes = 3)
})
q_sim_bf <- as.data.frame(sapply(df,"[[",1))
nse_bf <-  map_dbl(q_sim_bf, ~NSE(.x, q_obs_bf$bt))
head(sort(nse_bf, decreasing = TRUE),n=10)

yie_plot = q_sim_min %>% 
  select(date,run_4) %>%  
  dplyr::rename(q_sim = run_4) %>% 
  left_join(., q_obs_min_test, by = "date") %>% 
  dplyr::rename(q_obs = discharge) %>% 
  gather(.,key = "variable",value = "discharge",-date)

ggplot(data = yie_plot) +
  geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  ylim(0,1000)+
  theme_bw()

dotty_cal1 <- yie_test_d$parameter$values %>%
  mutate(nse = nse_yie_test) %>%
  filter(nse > 0) %>%
  gather(key = "par", value = "parameter_range", -nse)

ggplot(data = dotty_cal1) +
  geom_point(aes(x = parameter_range, y = nse)) +
  facet_wrap(.~par, ncol = 2, scales = "free_x") +
  theme_bw()


#### --quality ####
nh4_min_d= qa_cal500
quan <- nh4_min_d$simulation$q_out_72
qua <- nh4_min_d$simulation$nh_out_72
date = nh4_min_d$simulation$nh_out_72$date
nh4 <- (qua[,-1]/quan[,-1])/86.4
nh4 <- round(nh4,digits = 3)
nh4_sim_w = nh4 %>%
  mutate(week = as.integer(strftime(date, format = "%Y%V")))
nh4_sim_w = round(aggregate(nh4_sim_w[,1:ncol(nh4_sim_w)],list(nh4_sim_w$week),mean),2)
# colnames(nh4_sim_w)[1]='week'
all_nh4_week = left_join(obs_min_week,nh4_sim_w,by = "week")
sim_nh4_list = list(date = all_nh4_week[,3],q_nh4 = all_nh4_week[,4:ncol(all_nh4_week)])
obs_min_week = min_river_sc1 %>% 
  mutate(week = as.integer(strftime(date,format = "%Y%V"))) %>% 
  filter(date < '2016-12-31')

nse_test_nh4 <- sim_nh4_list$q_nh4 %>%
  map_dbl(., ~NSE(.x, obs_min_week$nh4))
head(sort(nse_test_nh4,decreasing=TRUE))

nh4_plot <- nh4_sim_w %>%
  select(week, run_08) %>%
  dplyr::rename(nh4_sim = run_08) %>%
  right_join(., obs_min_week_s, by = "week") %>%  
  dplyr::rename(nh4_obs = nh4) %>%
  select(-week) %>% 
  select(date,nh4_sim,nh4_obs) %>% 
  gather(key = "variable", value = "nh4",2:3) %>%
  filter(date < "2017-01-01")
ggplot(data = nh4_plot) +
  geom_line(aes(x = date, y = nh4, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("nh4"))+
  ylim(0,5)+
  theme_bw()

##save and load
run_swat2012(project_path = path_2012,
             output  = list(nh4_out = define_output(file = "rch",
                                                  variable = "NH4_OUT",
                                                  unit = c(74,92))),
             output_interval = "d",
             parameter = par_iter1,
             start_date = "2007-01-01",
             end_date = "2012-12-31",
             save_file = "rf_yalong_0412",
             return_output = FALSE,
             years_skip = 2,
             n_thread = 8)
path_saved_q <- "E:/Lexi/data_paper3/yalong_TxtInOut/rf_yalong_0412"

q <- scan_swat_run(save_dir = path_saved_q)
q_subset <- load_swat_run(save_dir = path_saved_q,
                          variable = "q_out",
                          run = 1:400)

write.csv(q_quality,"min_river_sc1.csv")


######4. new method doesn't make different####
par_yie = readRDS("calibrate.rds")
para = par_yie$parameter$values
para = para[4,]
name = par_yie$parameter$definition$full_name
names(para)[1:12] = name
q72 <- run_swat2012(project_path = path,
                    output = list(
                         q_out= define_output(file = "rch",
                                              variable = "FLOW_OUT",
                                              unit = c(65,72)),
                         nh_out= define_output(file = "rch",
                                               variable = "NH4_OUT",
                                               unit = c(65,72))),
                       output_interval = "d",
                       parameter = par_nh4,
                       start_date = "1985-01-01",
                       end_date = "2016-12-31",
                       years_skip = 2,
                       n_thread = 1,
                       run_path = run_path_model)

df <-  as.data.frame(q72$simulation)
df_q = df %>% 
  select(q_out_72.date,q_out_72.run_01) %>% 
  mutate(FLOW = q_out_72.run_01*35.3) %>% 
  dplyr::rename(DATE = q_out_72.date) %>% 
  select(-"q_out_72.run_01") 
obs_q_week = df_q %>%
  mutate(week = as.integer(strftime(DATE, format = "%Y%V")))
obs_q_week = round(aggregate(obs_q_week[,2],list(obs_q_week$week),mean),2) %>% 
  dplyr::rename(week = Group.1)
obs_qa_week = min_river_sc1 %>% 
  mutate(week = as.integer(strftime(date,format = "%Y%V"))) %>% 
  filter(date < '2016-12-31')
obs_con <- obs_qa_week %>% 
  left_join(.,obs_q_week,by = "week") %>% 
  dplyr::rename(DATES = date,FLOW = x,NH4 = nh4) %>% 
  select(-"week")

library(rloadest)
obs_con$DATES <- as.Date(obs_con$DATES)
load.lr <- selBestModel("NH4", data  = obs_con,flow = "FLOW",flow.units="cfs",
                        dates = "DATES",conc.units = "mg/L",station = "min")# select the best model
load.lr
load.lr <- loadReg(NH4 ~ model(9), data = obs_con, flow = "FLOW", date = "DATES", conc.units="mg/L", station="min")

print(load.lr, brief=FALSE, load.only=FALSE)
k = predLoad(load.lr, newdata = obs_con, load.units="kg", by="day",print=TRUE)

obs_load_m = k %>% 
  select (Date,Flux) %>% 
  mutate(week = as.integer(strftime(Date,format = "%Y%V"))) 
sim_load = df %>% 
  select(nh_out_72.date,nh_out_72.run_08) %>% 
  filter(nh_out_72.date >= "2008-01-01")
sim_load = sim_load %>% 
  mutate(week = as.integer(strftime(nh_out_72.date,format = "%Y%V"))) 
sim_load = round(aggregate(sim_load[,2],
                           list(sim_load$week),mean),2)
names(sim_load)[1:2] = c("week","q")
nh_plot <- sim_load %>%
  select(week, q) %>%
  dplyr::rename(nh_sim = q) %>%
  right_join(., obs_load_m, by = "week") %>%
  dplyr::rename(nh_obs = Flux) %>%
  gather(., key = "variable", value = "nh4",-week,-Date)

ggplot(data = nh_plot) +
  geom_line(aes(x = Date, y = nh4, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("concentration ("~mg/l~")"))+
  ylim(0,400000)+
  theme_bw()



## original method
quan <- q72$simulation$q_out_72$run_01
qua <- q72$simulation$nh_out_72$run_08
date <- q72$simulation$q_out_72$date
nh4 <- (qua/quan)/86.4
nh4 <- round(nh4,digits = 3)
nh4 <- data.frame(date,nh4)

nh4_sim_w = nh4 %>%
  mutate(week = as.integer(strftime(date, format = "%Y%V")))
nh4_sim_w = round(aggregate(nh4_sim_w[,2:ncol(nh4_sim_w)],list(nh4_sim_w$week),mean),2)
# colnames(nh4_sim_w)[1]='week'
all_nh4_week = left_join(obs_min_week,nh4_sim_w,by = "week")
sim_nh4_list = list(date = all_nh4_week[,3],q_nh4 = all_nh4_week[,4:ncol(all_nh4_week)])
obs_min_week = min_river_sc1 %>% 
  mutate(week = as.integer(strftime(date,format = "%Y%V"))) %>% 
  filter(date < '2016-12-31')

nse_test_nh4 <- sim_nh4_list$q_nh4 %>%
  map_dbl(., ~NSE(.x, obs_min_week$nh4))
head(sort(nse_test_nh4,decreasing=TRUE))

nh4_plot <- nh4_sim_w %>%
  select(week, nh4) %>%
  dplyr::rename(nh4_sim = nh4) %>%
  right_join(., obs_min_week, by = "week") %>%  
  dplyr::rename(nh4_obs = nh4) %>%
  select(-week) %>% 
  select(date,nh4_sim,nh4_obs) %>% 
  gather(key = "variable", value = "nh4",2:3) %>%
  filter(date < "2017-01-01")
ggplot(data = nh4_plot) +
  geom_line(aes(x = date, y = nh4, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("concentration"))+
  ylim(0,5)+
  theme_bw()



