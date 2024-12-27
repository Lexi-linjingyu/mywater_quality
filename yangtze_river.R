Packages <- c("SWATplusR","purrr","sf","tidyr","fast","dplyr","ggplot2", "sensitivity",
              "lhs","hydromad","lubridate","hydroGOF","forcats")
lapply(Packages, library, character.only = TRUE)

path <- "E:/Jinzhu_WANG/lexi/TxtInOut"

# Path to the subbasin shape file
sub_path <- paste(path,"/Shapes/subs1.shp",sep = "")
riv_path <- paste(path,"/Shapes/riv1.shp",sep = "")
# Path to run model 
run_path_model = "E:/Jinzhu_WANG/lexi/run_model"

# Read shapefile from the path
sub <- read_sf(sub_path)
riv <- read_sf(riv_path)

# Read observed data and change format
q_obs_ <- read.csv("F:/lexi/min_sc_TxtInOut/min_river.csv")
nh4_obs_min <- read.csv()
q_obs_min$date <- as.Date(as.character(q_obs_min$date),format ="%Y-%m-%d")

#### Step 1 model simulation ####
sim_yz_1960 <- run_swat2012(project_path = path,
                            start_date = "1970-01-01",
                            end_date = "2019-12-31",
                            output = list(nh = define_output(file = "rch",
                                                    variable = "NH4_OUT",
                                                    unit = 1:418),
                                          q_out = define_output(file = "rch",
                                                                 variable = "FLOW_OUT",
                                                                 unit = 1:418)),
                            run_path = run_path_model)


plot(q_obs_min,ylim = c(0,2000))
plot(sim_min_1960$q_out_29,ylim = c(0,1000))
ggplot() +
  geom_sf(data = sub) +
  geom_sf(data = riv, col = "royalblue", lwd = 0.75) +
  geom_sf_label(data = riv, aes(label = Subbasin)) +
  theme_bw()

q_obs_min <- filter(q_obs_min, date >= ymd("1983-01-01"),
                    date <= "2008-12-31")
q_sim_min_1981 <- filter(q_sim_min_1960, date >= ymd("1983-01-01"),
                         date <= "2008-12-31")

q_plot <- sim_min_1960 %>%
  select(date, FLOW_OUT_29) %>%
  dplyr::rename(q_sim = FLOW_OUT_29) %>%
  left_join(., q_obs_min, by = "date") %>%
  dplyr::rename(q_obs = discharge) %>%
  gather(., key = "variable", value = "discharge",-date)

ggplot(data = q_plot) +
  geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("streamflow ("~m^3/s~")"))+
  ylim(0,1500)+
  theme_bw()

nse_test <- q_sim_min_1981%>%
  select(-date) %>%
  map_dbl(., ~NSE(.x, q_obs_min$discharge))
head(sort(nse_test, decreasing = TRUE))

plot(q_obs_maidilong$discharge,q_sim_2012$FLOW_OUT_10)
r_sim <- lm(q_obs_maidilong$discharge ~ q_sim_2012$FLOW_OUT_10)
summary(r_sim)

#### Step 2 Sensitivity analysis ####
par_names_all <- c("BC1.chm | change = absval",
                   "BC3.chm | change = absval",
                   "RSDCO.bsn | change = absval",
                   "CN2.mgt | change = relchg",
                   "SOL_AWC.sol | change = relchg",
                   "SOL_K.sol | change = relchg",
                   "ALPHA_BF.gw | change = absval",
                   "ALPHA_BNK.rte | change = absval",
                   "GWQMN.gw | change = absval",
                   "CH_N2.rte | change = absval",
                   "CH_K2.rte | change = absval",
                   "SOL_ORGN.chm | change = absval",
                   "SOL_NO3.chm | change = absval",
                   "CDN.bsn | change = absval",
                   "RS3.chm | change = absval",
                   "RCN.bsn | change = absval",
                   "NPERCO.bsn| change = absval",
                   "ESCO.bsn| change = relchg")

par_fast_all <- fast_parameters(
  minimum = c(0.1, 0.2, 0, 0, 0,-0.5, 0, 0, 0, 0, 100, 0.1, 0, 0, 0.001, 0, 0, 0),#0,0, 0, 0, 0, 0.001, 0, 0.1),#, ,
  maximum = c(1, 0.4, 1, 100, 1, 0.5, 1, 0.5, 10, 0.4, 200, 1,300, 3, 0.1, 10, 1, 1),#3, 10, 300, 1, 5, 0.1, 100, 1),#, ),
  names = par_names_all)

q_fast_all_t <- run_swat2012(project_path = path,
                             output = list(q_out = define_output(file = "rch",
                                                                 variable = "FLOW_OUT",
                                                                 unit = c(29,74,92)),
                                           nh4_out = define_output(file = "rch",
                                                                   variable = "NH4_OUT",
                                                                   unit = c(29,74,92))),
                             
                             parameter = par_fast_all,
                             start_date = "1981-01-01",
                             end_date = "2019-12-31",
                             run_path = run_path_model,
                             years_skip = 2,
                             n_thread = 8)

# water quantity
q_obs <- filter(q_obs_min, date >= ymd("1983-01-01"), date <= "2008-12-31")
q_sim <- filter(q_fast_all_t$simulation$q_out_29, date >= ymd("1983-01-01"),date <= "2008-12-31")
nse_fast <- q_sim %>%
  select(-date) %>%
  map_dbl(., ~NSE(.x, q_obs$discharge))
head(sort(nse_fast,decreasing = TRUE))
q_plot <- q_sim %>%
  select(date, run_0310) %>%
  dplyr::rename(q_sim = run_0310) %>%
  left_join(., q_obs, by = "date") %>%
  dplyr::rename(q_obs = discharge) %>%
  gather(., key = "variable", value = "discharge",-date)

ggplot(data = q_plot) +
  geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(y = expression("streamflow ("~m^3/s~")"))+
  ylim(0,1500)+
  theme_bw()
sens_fast <- sensitivity(nse_fast, 18)
result_fast <- tibble(parameter = q_fast_all_t$parameter$definition$par_name,
                      fast = sens_fast) %>%
  mutate(parameter = factor(parameter) %>% fct_reorder(., fast))

ggplot(data = result_fast) +
  geom_col(aes(x = parameter, y = fast)) +
  xlab("Parameter for water yield") +
  ylab("Sensitivity") +
  coord_flip() +
  theme_bw()

dotty <- q_fast$parameter$values %>%
  mutate(nse = nse_test) %>%
  filter(nse > 0) %>%
  gather(key = "par", value = "parameter_range", -nse)

ggplot(data = dotty) +
  geom_point(aes(x = parameter_range, y = nse)) +
  facet_wrap(.~par, ncol = 2, scales = "free_x") +
  theme_bw()

# water quality
sim_min = q_fast_all_t$simulation$nh4_out_74
min_river_sc1 = read.csv("min_river_sc.1.csv")
sim_min_week = sim_min %>%
  mutate(week = as.integer(strftime(date, format = "%Y%V")))
sim_min_week = round(aggregate(sim_min_week[,2:1656],list(sim_min_week$week),mean),2)
names(sim_min_week)[1] = 'week'
obs_min_week = min_river_sc1 %>% 
  mutate(week = as.integer(strftime(date,format = "%Y%V")))
all_min_week = left_join(obs_min_week,sim_min_week,by = "week")
sim_min_list = list(date = all_min_week[,3],q_nh4 = all_min_week[,4:1658])

nse_fast_nh4 <- sim_min_list$q_nh4 %>%
  map_dbl(., ~NSE(.x, obs_min_week$nh4))
head(sort(nse_fast_nh4,decreasing=TRUE))
sens_fast_nh4 <- sensitivity(nse_fast_nh4, 18)
result_fast_nh4 <- tibble(parameter = q_fast_all_t$parameter$definition$par_name,
                          fast = sens_fast_nh4) %>%
  mutate(parameter = factor(parameter) %>% fct_reorder(., fast))
ggplot(data = result_fast_nh4) +
  geom_col(aes(x = parameter, y = fast)) +
  xlab("Parameter for NH4") +
  ylab("Sensitivity") +
  coord_flip() +
  theme_bw()

dotty <- q_fast_all_t$parameter$values %>%
  mutate(nse = nse_fast) %>%
  filter(nse > -10) %>%
  gather(key = "par", value = "parameter_range", -nse)

ggplot(data = dotty) +
  geom_point(aes(x = parameter_range, y = nse)) +
  facet_wrap(.~par, ncol = 2, scales = "free_x") +
  theme_bw()


#### Step 3 calibrate ####======================================================
par_test_yie <- tibble("ALPHA_BNK.rte | change = absval | sub = 1:94" = c(0,1),
                       "CH_N2.rte | change = absval | sub = 1:94" = c(0,0.4),
                       "CH_K2.rte | change = absval | sub = 1:94" = c(100,200))
par_test_nh4 <- tibble("BC3.chm | change = absval | sub = 1:94" = c(0,10),
                       "CH_N2.rte | change = absval | sub = 1:94" = 0.21,
                       "NPERCO.bsn| change = absval | sub = 1:94" = 0.21,
                       "SOL_NO3.chm | change = relchg | sub = 1:94" = c(0,300))
n_sample = 400
n_par = ncol(par_test_yie)
par_yie <- randomLHS(n = n_sample, k = n_par) %>%
  as_tibble(., .name_repair = "minimal") %>%
  set_names(names(par_test_yie))%>%
  map2_df(., par_test_yie, ~ (.x * (.y[2] - .y[1]) + .y[1])) 
yie_test_d <- run_swat2012(project_path = path,
                           output = list(q_out = define_output(file = "rch",
                                                               variable = "FLOW_OUT",
                                                               unit = c(29,74,92))),
                           output_interval = "d",
                           run_path = run_path_model,
                           parameter = par_yie,
                           start_date = "1981-01-01",
                           end_date = "2019-12-31",
                           years_skip = 2,
                           n_thread = 4)

q_obs_min_test <- filter(q_obs_min, date >= "1985-01-01",date <= "2008-12-31")


nse_test <- q_test_d$simulation$q_out %>%
  select(-date) %>%
  map_dbl(., ~NSE(.x, q_obs_min_test$discharge))
head(sort(nse_test, decreasing = TRUE))

q_plot_test <- q_test_d$simulation$q_out %>%
  select(date,run_138) %>% 
  dplyr::rename(q_sim = run_138) %>%
  left_join(., q_obs_min, by = "date") %>%
  dplyr::rename(q_obs = discharge) %>%
  gather(., key = "variable", value = "discharge",-date)

ggplot(data = q_plot_test) +
  geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  ylim(0,1000)+
  theme_bw()

dotty_cal1 <- q_test_d$parameter$values %>%
  mutate(nse = nse_test) %>%
  filter(nse > -10) %>%
  gather(key = "par", value = "parameter_range", -nse)

ggplot(data = dotty_cal1) +
  geom_point(aes(x = parameter_range, y = nse)) +
  facet_wrap(.~par, ncol = 2, scales = "free_x") +
  theme_bw()

##save and load
run_swat2012(project_path = path_2012,
             output  = list(q_out = define_output(file = "rch",
                                                  variable = "FLOW_OUT",
                                                  unit = 10)),
             output_interval = "d",
             parameter = par_iter1,
             start_date = "2004-01-01",
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