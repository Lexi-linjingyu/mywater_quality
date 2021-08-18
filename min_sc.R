Packages <- c("SWATplusR","purrr","sf","tidyr","fast","dplyr","ggplot2", "sensitivity",
              "lhs","hydromad","lubridate","hydroGOF","forcats")
lapply(Packages, library, character.only = TRUE)

path <- "D:/lexi/min_sc_TxtInOut"

# Path to the subbasin shape file
sub_path <- "D:/lexi/min_sc_TxtInOut/Shapes/subs1.shp"
# Path to the subbasin shape file
riv_path <- "D:/lexi/min_sc_TxtInOut/Shapes/riv1.shp"
#> [1] "/home/christoph/R/x86_64-pc-linux-gnu-library/3.6/SWATdata/extdata/plus_shapes/sub.shp"
sub <- read_sf(sub_path)
riv <- read_sf(riv_path)

## step 1 model simulation##
q_sim_min_1960 <- run_swat2012(project_path = path,
                               start_date = "1981-01-01",
                               end_date = "2019-12-31",
                               output = list(q_out = define_output(file = "rch",
                                                                   variable = "FLOW_OUT",
                                                                   unit = 1:94),
                                             q_tn = define_output(file = "rch",
                                                                  variable = "TOT_N",
                                                                  unit = 1:94),
                                             q_nh4 =define_output(file = "rch",
                                                                  variable = "NH4_OUT",
                                                                  unit = 1:94)))
plot(q_sim_min_1960$date,q_sim_min_1960$q_tn_29,type="l",lty=1,ylim=c(0,10000000))
q_obs_min <- read.csv("E:/Lexi/data_paper3/min_sc_TxtInOut/min_river.csv")
q_obs_min$date <- as.Date(as.character(q_obs_min$date),format ="%Y-%m-%d")
plot(q_obs_min,ylim = c(0,2000))
plot(q_sim_min_1960$FLOW_OUT_29,ylim = c(0,1000))
ggplot() +
  geom_sf(data = sub) +
  geom_sf(data = riv, col = "royalblue", lwd = 0.75) +
  geom_sf_label(data = riv, aes(label = Subbasin)) +
  theme_bw()

q_obs_min <- filter(q_obs_min, date >= ymd("1983-01-01"),
                    date <= "2008-12-31")
q_sim_min_1981 <- filter(q_sim_min_1960, date >= ymd("1983-01-01"),
                         date <= "2008-12-31")

q_plot <- q_sim_min_1981 %>%
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

## STEP 2 Sensitivity analysis##
par_names <- c("CN2.mgt | change = relchg",
               "GWQMN.gw | change = absval",
               "ALPHA_BF.gw | change = absval",
               "GW_DELAY.gw | change = absval",
               "GW_REVAP.gw | change = relchg",
               "ESCO.hru | change = absval",
               "ALPHA_BNK.rte | change = absval",
               "CH_N2.rte | change = absval",
               "CH_K2.rte | change = absval",
               "SOL_AWC.sol | change = relchg",
               "SOL_K.sol | change = relchg",
               "SOL_BD.sol | change = relchg",
               "SMTMP.bsn | change = absval",
               "SMFMX.bsn | change = absval")
## "CDN.bsn | change = absval",(0:3)
##"RCN.bsn | change = absval",(0,10)
##"SOL_NO3.chm | change = relchg")  (0,300)          
##  "FER_SURFACE.mgt | change = relchg"(0:1),
## 
par_fast <- fast_parameters(
  minimum = c(0, 0, 0, 0, 0.02,0.01, 0, 0, 0, -0.5,-0.5, 1.1, -5, 1),
  maximum = c(100, 5000, 1, 1, 0.2, 1, 1, 0.4, 200, 0.5, 0.5, 1.9, 5, 8),
  names = par_names)

q_fast <- run_swat2012(project_path = path,
                       output  = list(q_out = define_output(file = "rch",
                                                            variable = "FLOW_OUT",
                                                            unit = 29)),
                       
                       parameter = par_fast,
                       start_date = "1983-01-01",
                       end_date = "2008-12-31",
                       years_skip = 2,
                       n_thread = 32)

q_obs <- filter(q_obs_min, date >= ymd("2002-01-01"),
                date <= "2008-12-31")
nse_fast <- q_fast$simulation$q_out %>%
  select(-date) %>%
  map_dbl(., ~NSE(.x, q_obs$discharge))
sens_fast <- sensitivity(nse_fast, 12)
result_fast <- tibble(parameter = q_fast$parameter$definition$par_name,
                      fast = sens_fast) %>%
  mutate(parameter = factor(parameter) %>% fct_reorder(., fast))
ggplot(data = result_fast) +
  geom_col(aes(x = parameter, y = fast)) +
  xlab("Parameter") +
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


## Step 3 calibrate ##======================================================
par_test_d <- tibble("CN2.mgt | change = relchg | sub = 1:94" = c(0,100),
                     "SOL_AWC.sol | change = relchg | sub = 1:94" = c(0,1),
                     "SOL_K.sol | change = relchg | sub = 1:94" = c(-0.5,0.5),
                     "ALPHA_BF.gw | change = absval | sub = 1:94" = c(0,1),
                     "ALPHA_BNK.rte | change = absval | sub = 1:94" = c(0,1),
                     "GWQMN.gw | change = absval | sub = 1:10" = c(0,3000),
                     "CH_N2.rte | change = absval | sub = 1:94" = c(0,0.4),
                     "CH_K2.rte | change = absval | sub = 1:94" = c(0,200)
)



#"GWQMN.gw | change = absval | sub = 1:10" = runif(4,100,300)
#"GW_DELAY.gw | change = absval | sub = 1:10" = runif(4,0,1000),
# "GW_REVAP.gw | change = absval | sub = 1:10" = runif(4,1,2)
# SMTMP.bsn | change = absval | sub = 1:10" = c(-20,-15),
# SMFMX.bsn | change = absval | sub = 1:10" = c(0,0.1))
n_sample = 400
n_par = ncol(par_test_d)
par_iter1 <- randomLHS(n = n_sample, k = n_par) %>%
  as_tibble(., .name_repair = "minimal") %>%
  set_names(names(par_test_d))%>%
  map2_df(., par_test_d, ~ (.x * (.y[2] - .y[1]) + .y[1])) 
q_test_d <- run_swat2012(project_path = path,
                         output  = list(q_out = define_output(file = "rch",
                                                              variable = "FLOW_OUT",
                                                              unit = 29)),
                         output_interval = "d",
                         parameter = par_iter1,
                         start_date = "1983-01-01",
                         end_date = "2008-12-31",
                         years_skip = 2,
                         n_thread = 64)

q_obs_min_test <- filter(q_obs_min, date >= "1985-01-01",date <= "2008-12-31")
##q_test <- filter(q_test$simulation$q_out, date >= ymd("2004-03-20"),date <= "2012-12-31")

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