
###1.Install and library packages ####
# first download R-SWAT via
# https://github.com/tamnva/R-SWAT/archive/refs/heads/master.zip

Filesource = "H:/yangtze_river/R-SWAT-master/R" 
setwd(Filesource)
Functionfiles = c("glue.R", "multiRegression.R","objFunction.R", 
                  "readSaveOutput.R", "updateTxtInOut.R",
                  "userObjFunction.R", "displayOutput.R")
lapply(Functionfiles, source) #load R-SWAT functions

setwd("H:/yangtze_river") # set workplace
path <-  getwd()
Packages <- c("SWATplusR","purrr","sf","tidyr","fast","dplyr",
              "ggplot2", "sensitivity", "lhs","hydromad","lubridate",
              "hydroGOF","forcats","EcoHydRology","magrittr")

##lapply(Packages,install.packages,character.only = TRUE) # Only for the first running
lapply(Packages, library, character.only = TRUE)



###2.Sensitivity analysis ####
#####2.1 load data ####
# sensitivities <- readRDS("sen_yangtze.rds")
# 
# # load all observed streamflow data
# 
# pathTOobs = sort(list.files("F:/wangjinz/lexi/code/CN_swat/senSTM",
#                       pattern="\\.csv$",full.names = TRUE)) # list all .csv file
# No_obs = c()
# for (i in 1:length(pathTOobs)) {
#   No_obs[i] = setdiff(strsplit(pathTOobs,"\\D+")[[i]],"")[2]
#   No_obs[i] = as.numeric(No_obs[i])
# }# extract id's number
# name_q <- paste0("q_out_",No_obs,sep="")
# 
# list_q_obsAll <- lapply(pathTOobs,read.csv)# read all .csv file
# list_q_obs <- list()
# for (i in 1:length(No_obs)){
#   list_q_obsAll[[i]][["date"]] <- as.Date(as.character(list_q_obsAll[[i]][["date"]]),
#                                      format ="%Y-%m-%d")#reformat the date
#   list_q_obs[[i]] <- filter(list_q_obsAll[[i]],
#                             date >= ymd("1982-01-01"), date <= "2018-12-31")
#   names(list_q_obs)[[i]] <- name_q[i]# name elements
# } 
# 
# ## extract simulated streamflow data 
# sens_q_sim <- sensitivities[["simulation"]][c(name_q)]
# date = list_q_obs[[c(1:15)]][["date"]]
# list_q_sim = list()
# for (i in 1:length(list_q_obs)){
#   list_q_sim[i] =lapply(sens_q_sim, 
#                         function(x){subset(Data, Column_X == x)})
# }
# 
# # list_q_sim <- lapply(sens_q_sim,
# #                      function(x) dplyr::filter(x,date >= ymd("1982-01-01"),
# #                                              date <= "2018-12-31"))
# 
# 
#####2.2 select the best performance ####
# nse_fast <- list()
# q_plot <- list()
# k <- c()
# k_name <- c()
# 
# for (i in 1:length(list_q_sim)){
#     nse_fast[[i]] <- list_q_sim[[i]] %>%
#     select(-date) %>%
#     map_dfc(., ~NSE(.x, list_q_obs[[i]][["discharge"]]))
#     k[i] = match(head(sort(nse_fast[[i]],decreasing = TRUE),n=10)[1],
#                  nse_fast[[i]])
#     if (k[i] < 10) {
#       k_name[i] = paste("run_000",k[i],sep="")
#     } else if (k[i]< 100){
#       k_name[i] = paste("run_00",k[i],sep="")    
#       } else if (k[i] < 1000) {
#         k_name[i] = paste("run_0",k[i],sep="")
#       } else  {
#         k_name[i] = paste("run_",k[i],sep = "")
#       } 
#     q_plot[[i]] <- list_q_sim[[i]] %>%
#       select(date, k_name[i]) %>%
#       dplyr::rename(q_sim = k_name[i]) %>%
#       left_join(., list_q_obs[[i]], by = "date") %>%
#       dplyr::rename(q_obs = discharge) %>%
#       gather(., key = "variable", value = "discharge",-date)
#     ggplot(data = q_plot[[i]]) +
#       geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
#       scale_color_manual(values = c("black", "tomato3")) +
#       labs(y = expression("streamflow ("~m^3/s~")"))+
#       scale_y_continuous()+
#       theme_bw()
#     ggsave((paste("best_",No_obs[i],".jpg",sep = "")),width = 9,height = 4)
# }   #just ignore the warning 
# 
#####2.3 select sensitive parameters####
# nse_fast_min <- list()
# sens_fast <- list()
# result_fast <- list()
# for (i in 1:length(list_q_sim)){
#   nse_fast_min[[i]] <- list_q_sim[[i]] %>%
#                       select(-date) %>%
#                       map_dbl(., ~NSE(.x, list_q_obs[[i]][["discharge"]]))
#   sens_fast[[i]]<- sensitivity(nse_fast_min[[i]], 
#                                nrow(sensitivities$parameter$definition))
#   result_fast[[i]] <- tibble(parameter = sensitivities$parameter$definition$par_name,
#                              fast = sens_fast[[i]]) %>%
#                              mutate(parameter = factor(parameter) %>% 
#                              fct_reorder(., fast))
#   ggplot(data = result_fast[[i]]) +
#     geom_col(aes(x = parameter, y = fast)) +
#     xlab("Parameter for water yield") +
#     ylab("Sensitivity") +
#     coord_flip() +
#     theme_bw()
#   ggsave((paste("senPA",No_obs[i],".jpg",sep = "")),width = 4,height = 4)
# }
# 
# 
#####2.4 select parameters ranges####
# dotty <- list()
# for (i in 1:length(nse_fast_min)){
#   dotty[[i]] <- sensitivities$parameter$values %>%
#     mutate(nse = nse_fast_min[[i]]) %>%
#     filter(nse > 0) %>%
#     gather(key = "par", value = "parameter_range", -nse)
#   ggplot(data = dotty[[i]]) +
#     geom_point(aes(x = parameter_range, y = nse)) +
#     facet_wrap(.~par, ncol = 4, scales = "free_x") +
#     theme_bw()
#   ggsave((paste("nsePA_",No_obs[i],".jpg")),width = 12,height = 10)
# }
# 
# 
#####2.5 package all .jpg file ####
# pic <-  list.files(path,"\\.jpg$")
# dir.create("sen")
# path2 <- paste0(path,"/","sen",sep = "")
# file.copy(paste0(path,"/",pic,sep = ""),
#           paste0(path2,"/",pic,sep = ""),
#           overwrite = TRUE)



###3.Stream flow calibration and validation ####
#####3.1 load all data ####

## Please note: before running the calibration,
## find the number of the basins via their hydroID at
## F:\wangjinz\lexi\code\yangtze_river\Others\selected_sta,
## named each observed data with basin number, add 0 to the front
## of basin number if there is only two digits, i.e. basin 56 should be named 056,
## and save those files with .csv extension at F:/wangjinz/lexi/code/yangtze_river/.

# hydroID = sta_bsn %>% 
#   select(HydroID,Subbasin) %>% 
#   filter(HydroID != 0) %>% 
#   filter(!HydroID %in% c(129199,129423,129295,129155,129381,129261)) 
# writexl::write_xlsx(hydroID,"selected_sta.xlsx")

######3.1.1 observed streamflow data####
pathTOobs = sort(list.files("H:/yangtze_river",
                            pattern=".*?W1.*?\\.csv$",full.names = TRUE)) # list all .csv file
No_obs = c()
for (i in 1:length(pathTOobs)) {
  No_obs[i] = setdiff(strsplit(pathTOobs,"\\D+")[[i]],"")[2]
  No_obs[i] = as.numeric(No_obs[i])
}# extract id's number
name_q <- paste0("q_obs_",No_obs,sep="")

All_list_q_obs <- lapply(pathTOobs,read.csv)# read all .csv file

list_q_obs <- list()
for (i in 1:length(No_obs)){
  All_list_q_obs[[i]][["date"]] <- as.Date(as.character(All_list_q_obs[[i]][["date"]]),
                                          format ="%Y-%m-%d")#reformat the date
  list_q_obs[[i]] <- All_list_q_obs[[i]]
  names(list_q_obs)[[i]] <- name_q[i]# name elements
} 

# fit the time series
# fill na data with mean daymonth for observed data 
full_q_obs<- lapply(list_q_obs, transform, 
                    daymonth = strftime(date,format = "%m-%d"))#add new column to each df in a list
full_q_obs <- lapply(full_q_obs, 
                     function(x) transform(x,
                                           mp =ave(discharge,daymonth,
                                                   FUN=function(x) mean(x,na.rm=TRUE))))
list_q_obs <- lapply(full_q_obs, 
                     function(x) transform(x, 
                                           discharge =ifelse(is.na(discharge),
                                                             round(mp,digits = 1),
                                                             round(discharge,digits = 1))))
list_q_obs <- lapply(list_q_obs,"[",1:2)
# list_q_obs <- lapply(list_q_obs,
#                      function(x) dplyr::filter(x,date >= ymd("1982-01-01"),
#                                                date <= "2016-12-31"))
Date = lapply(list_q_obs,"[",-2)

#####Addition:observed data organized for R-SWAT
list_q_obs <- lapply(list_q_obs,
                     function(x) transform(x,time=paste("00:00")))
new_colnames = c("Date","Oobs","Time")
list_q_obs <- lapply(list_q_obs,setNames,new_colnames)

smp_size = list()
list_q_obs_c = list()
loca_date = list()
for (i in 1:length(list_q_obs)){
  smp_size[i] <- floor(0.7*nrow(list_q_obs[[i]]))
  loca_date[i] <- as.character(list_q_obs[[i]][["Date"]][smp_size[[i]]])
  list_q_obs_c[i]<- lapply(list_q_obs[i],
                            function(x)transform(x,
                                                 Oobs = list_q_obs[[i]][["Oobs"]],
                                                 Time = list_q_obs[[i]][["Time"]],
                                                 Cali_Valid_Flag = 
                                                 ifelse(list_q_obs[[i]][["Date"]] <= loca_date[[i]],"C","V")
                                                 ))
  names(list_q_obs_c)[[i]] = name_q[i]
}

colnames = c("Date","Oobs","Time","Cali_Valid_Flag")
list_q_obs_c <- lapply(list_q_obs_c,setNames,colnames)
re_list_q_obs <- lapply(list_q_obs_c,
                        function(x){x[c("Date","Time","Oobs","Cali_Valid_Flag")]
                        }) # reorder columns
# re_list_q_obs = re_list_q_obs[-1]
# list_q_obs = list_q_obs[-1]

unlink("F:/wangjinz/lexi/code/yangtze_river/w1_obs/*")# remove all existing files
nam = list()
for (i in 1:length(re_list_q_obs)){
 nam[[i]] <- paste("H:/yangtze_river/w1_obs/",
                    "obs_var_",i,".txt",sep = "")
 write.table(re_list_q_obs[[i]],
             nam[[i]],
             row.names = FALSE,quote =FALSE)
}

# deal with simulated data
######3.1.2 simulated streamflow data ####
q_cali <- readRDS("F:/wangjinz/lexi/code/yangtze_river/q_cali.rds")
## or try this
q_fast_all = readRDS("H:/yangtze_river/sen_yangtze.rds")

name_q_out <- paste0("q_out_",No_obs,sep="")
cali_q_sim <- q_fast_all[["simulation"]][c(name_q_out)]


list_q_sim = list()
for (i in 1:length(cali_q_sim)){
  list_q_sim[i]=lapply(cali_q_sim[i], 
                        function(x) dplyr::filter(x,date >= min(Date[[i]][["date"]]),
                                                  date <= max(Date[[i]][["date"]])))
  names(list_q_sim)[[i]] <- name_q_out[i] # name elements
  
}

list_q_sim_nodate = lapply(list_q_sim,"[",-1)
list_q_sim_nodate[] = lapply(list_q_sim_nodate,round,2)

unlink("F:/wangjinz/lexi/code/yangtze_river/Output/Core_1/*")
nam = list()
for (i in 1:length(list_q_sim_nodate)){
  for (j in 1:ncol(list_q_sim_nodate[[i]])){
    nam[[i]] <- paste("F:/wangjinz/lexi/code/yangtze_river/Output/Core_1/",
                      "out_var_",i,".txt",sep = "")
    write.table(list_q_sim_nodate[[i]][,j],
                nam[[i]],
                row.names = FALSE,
                col.names = j,
                append = TRUE)
  }
}# delete if changing timeseries

####3.2 calibration using SUFI2####
dir = "F:/wangjinz/lexi/code/yangtze_river"
setwd(dir)
dir2 = "H:/yangtze_river/w1_obs"

df = data.frame(seq.int(nrow(q_fast_all[["parameter"]][["values"]])))
names(df) = "V1"
parameterValue = cbind(df,q_fast_all[["parameter"]][["values"]])

reach = paste(noquote(No_obs),collapse = ",")
outputExtraction <- data.frame(
  FileType = c("output.rch"),    # if for two files: = c("watout.dat", "output.rch"),
  FileName = c("output.rch"),    #                   = c("watout.dat", "output.rch"),
  Column = c("7"),               #                   = c("4"         , "6"),
  Reach = reach             #                   = c(" "         , "2")
)
OutputVar <- getNumberOutputVar(outputExtraction)
nOutputVar <- OutputVar$nOutputVar
ncores = 1
userReadSwatOutput <- OutputVar$userReadSwatOutput

observedDataFile <- list.files(dir2,pattern="obs_var",all.files = TRUE,
                               full.names = TRUE)
observedDataFile <- sortObservedDataFile(observedDataFile)
observedData <- list()
for (i in 1:length(observedDataFile)){
  # Read observed files and save to a dummy variable
  temp <- read.table(observedDataFile[i], skip = 1, sep = "")
  # Get bbserved data from dummy variable
  observedData [[i]] <- data.frame(Date = as.POSIXct(paste(temp[,1], temp[,2], 
                                                           sep = " "), 
                                                     format = "%Y-%m-%d %H:%M", 
                                                     tz = ""),
                                   Value = temp[,3],
                                   Flag = temp[,4])
}

workingFolder <- dir
objFunction <- "NSE"

obj <- calObjFunction(parameterValue,
                      ncores,
                      nOutputVar,
                      userReadSwatOutput,
                      observedData,
                      workingFolder,
                      objFunction)

behThreshold <- -2
minOrmax <- "Maximize"
samplingApproach <- "Sensi_Cali_(uniform_Latin_Hypercube_Sampling)"
# varNumber = 1

for (i in seq(1:nOutputVar)){
  name = paste0('behavioral',i)
  print(name)
  data <- behaSimulation(obj$objValueCali,
                         obj$simData,
                         parameterValue,
                         behThreshold,
                         i,
                         objFunction,
                         observedData,
                         minOrmax,
                         samplingApproach)
  assign(name,data)
}
name = c()
for (i in 1:nOutputVar){
  name[i] = paste0('behavioral',i)
}  
all_behavioral = do.call("list",mget(name))

  


####3.3 save files ####
plot(obj$objValueCali)
save(obj,all_behavioral,name_q_out,
     file = "all_yangtze.Rdata")
# load("all_yangtze.Rdata")

##don't be worried, the observed data are exclusive.

####3.4 plot figure ####
# obj_cri_df = data.frame(matrix(unlist(obj[["perCriteriaCali"]]),ncol =5, 
#                                    byrow= TRUE))
# obj_nse_df = data.frame(obj_cri_df[,1])
# obj_nse_para = obj_nse_df %>% 
#   mutate(V1= rep(1:100,nrow(obj_nse_df)/100)) %>% 
#   left_join(parameterValue,by = "V1") %>% 
#   rename_with(.col = 1,~"NSE") %>% 
#   select(3:7,1) %>% 
#   gather(.,type,value,1:5,factor_key = TRUE)
# ggplot(obj_nse_para,aes(value,NSE)) +
#   geom_point()+
#   facet_wrap(~type,scales = "free")+
#   scale_y_continuous(limits = c(0,1))

#plot best simulation verse observation
plot_list_1 = list()
behasim = list()
obs_sim = list()
for (i in 1:length(all_behavioral)){
  behasim[[i]] = all_behavioral[[i]][["ppuSimData"]]
  obs_sim[[i]] = behasim[[i]] %>% 
    left_join(list_q_obs[[i]],by = "Date") %>%   
    select(1,2,4:6) %>%  
    gather(.,type,value,c(4,5),factor_key = TRUE)
  p1 = ggplot(obs_sim[[i]])+
    geom_line(aes(Date,value,col = type))+
    geom_ribbon(aes(ymin = lower_95PPU,
                    ymax = upper_95PPU,
                    x= Date),
                fill = "grey50",alpha = 0.3)+
    scale_color_manual(values=c("red","black"))+
    theme(text = element_text(size=20))
  plot_list_1[[i]] = p1

}

for (i in 1:length(plot_list_1)){
  file_name = paste("obs_sim_",i,".tiff",sep = "")
  tiff (file_name,width = 800,height = 450)
  print(plot_list_1[[i]])
  dev.off()
}

#plot distribution of selected observed station 
path <- "F:/wangjinz/lexi/yangtze_river/TxtInOut"

# Path to the subbasin shape file
sub_path <- paste(path,"/Shapes/subs1.shp",sep = "")
riv_path <- paste(path,"/Shapes/riv1.shp",sep = "")

# Read shapefile from the path
sub <- read_sf(sub_path) %>% 
  filter(Subbasin %in% No_obs_qa)
riv <- read_sf(riv_path) 
ggplot() +
  geom_sf(data = sub) +
  geom_sf(data = riv, col = "royalblue", lwd = 0.75) +
  geom_sf_label(data = sub, aes(label = c(1:length(No_obs_qa)))) +
  theme_bw()

###4.Water quality calibration and validation ####
#####4.1 load all data ####

######4.1.1 observed water quality data####

pathTOobs_qa = sort(list.files("H:/yangtze_river",
                            pattern=".*?W2.*?\\.csv$",full.names = TRUE)) # list all .csv file
No_obs_qa = c()
for (i in 1:length(pathTOobs_qa)) {
  No_obs_qa[i] = setdiff(strsplit(pathTOobs_qa,"\\D+")[[i]],"")[2]
  No_obs_qa[i] = as.numeric(No_obs_qa[i])
}# extract id's number
name_qa <- paste0("qa_obs_",No_obs_qa,sep="")
name_q_qa <- paste0("q_out_",No_obs_qa,sep="")

All_list_qa_obs <- lapply(pathTOobs_qa,read.csv)# read all .csv file

list_qa_obs <- list()
for (i in 1:length(No_obs_qa)){
  All_list_qa_obs[[i]][["date"]] <- as.Date(as.character(All_list_qa_obs[[i]][["date"]]),
                                           format ="%Y-%m-%d")#reformat the date
  list_qa_obs[[i]] <- All_list_qa_obs[[i]]
  names(list_qa_obs)[[i]] <- name_qa[i]# name elements
} 

# fit the time series
# fill na data with mean daymonth for observed data 
full_qa_obs<- lapply(list_qa_obs, transform, 
                    daymonth = strftime(date,format = "%m-%d"))#add new column to each df in a list
full_qa_obs <- lapply(full_qa_obs, 
                     function(x) transform(x,
                                           mp =ave(nh4_con,daymonth,
                                                   FUN=function(x) mean(x,na.rm=TRUE))))
list_qa_obs <- lapply(full_qa_obs, 
                     function(x) transform(x, 
                                           nh4_con =ifelse(is.na(nh4_con),
                                                             round(mp,digits = 2),
                                                             round(nh4_con,digits = 2))))
list_qa_obs <- lapply(list_qa_obs,"[",1:2)
list_qa_obs <- lapply(list_qa_obs,
                     function(x) dplyr::filter(x,date >= ymd("2000-01-01"),
                                               date <= ymd("2018-12-31")))

Date_qa = lapply(list_qa_obs,"[",-2)

####change concentration to load ####
# riv <- read_sf("F:/wangjinz/lexi/yangtze_river/TxtInOut//Shapes/riv1.shp")
# find_riv = as.data.frame(riv) %>% 
#   select(6:7) 
q_qa_sim <- q_fast_all[["simulation"]][c(name_q_qa)]

best_4 <- unlist(all_behavioral[["behavioral4"]][["ppuParaRange"]])
k = length(best_4)/4
lower = best_4[1:k]
median = best_4[(k+1):(2*k)]
upper = best_4[(2*k+1):(3*k)]
best = best_4[(3*k+1):(4*k)]
best_4 = data.frame(lower,median,upper,best) %>% 
  mutate(id = paste0(colnames(parameterValue[-1]))) %>% 
  select(4,5)

best_8 <- unlist(all_behavioral[["behavioral8"]][["ppuParaRange"]])
k = length(best_8)/4
lower = best_8[1:k]
median = best_8[(k+1):(2*k)]
upper = best_8[(2*k+1):(3*k)]
best = best_8[(3*k+1):(4*k)]
best_all = data.frame(lower,median,upper,best) %>% 
  mutate(id = paste0(colnames(parameterValue[-1]))) %>% 
  select(4,5) %>% 
  rbind(best_4) %>% 
  group_by(id) %>% 
  summarise(mean_best =mean(best),.groups = "drop") 
best_fina = as.tibble(t(best_4[-2]))
colnames(best_fina) = colnames(par_yie)
par_yie = par_yie[-c(19,20)]

best_final = par_yie %>% 
  bind_rows(best_fina)
best_final = best_final[-1,]


sim_yz_qa <- run_swat2012(project_path = path,
                            start_date = "1990-01-01",
                            end_date = "2019-12-31",
                            output = list(nh_out = define_output(file = "rch",
                                                                 variable = "NH4_OUT",
                                                                 unit = qa),
                                          q_out = define_output(file = "rch",
                                                                variable = "FLOW_OUT",
                                                                unit = qa),
                                          tp_out = define_output(file = "rch",
                                                                 variable = "TOT_P",
                                                                 unit = qa),
                                          do_out = define_output(file = "rch",
                                                                 variable = "DISOX_OUT",
                                                                 unit = qa)),
                            parameter = best_fina,
                            run_path = run_path_model) 
con212 = sim_yz_qa[["simulation"]][["nh_out_212"]]/sim_yz_qa[["simulation"]][["q_out_212"]]/86.4
date212 = sim_yz_qa[["simulation"]][["date"]]
df212 = data.frame(date212,con212)
df212$weekday = as.numeric(format(df212$date212,format="%w"))
df212$start_date = df212$date212 + (1-df212$weekday)
df212_w = aggregate(con212~start_date,FUN = mean,data = df212,na.rm = TRUE)

flow = sim_yz_qa$simulation %>% 
  select(1,7:11) %>% 
  as.list()
  



#####Addition:observed data organized for R-SWAT
list_qa_obs <- lapply(list_qa_obs,
                     function(x) transform(x,time=paste("00:00")))
new_colnames = c("Date","Oobs","Time")
list_qa_obs <- lapply(list_qa_obs,setNames,new_colnames)

smp_size_qa = list()
list_qa_obs_c = list()
loca_date_qa = list()
for (i in 1:length(list_qa_obs)){
  smp_size_qa[i] <- floor(0.7*nrow(list_qa_obs[[i]]))
  loca_date_qa[i] <- as.character(list_qa_obs[[i]][["Date"]][smp_size_qa[[i]]])
  list_qa_obs_c[i]<- lapply(list_qa_obs[i],
                           function(x)transform(x,
                                                Oobs = list_qa_obs[[i]][["Oobs"]],
                                                Time = list_qa_obs[[i]][["Time"]],
                                                Cali_Valid_Flag = 
                                                  ifelse(list_qa_obs[[i]][["Date"]] <= loca_date_qa[[i]],"C","V")
                           ))
  names(list_qa_obs_c)[[i]] = name_qa[i]
}

colnames = c("Date","Oobs","Time","Cali_Valid_Flag")
list_qa_obs_c <- lapply(list_qa_obs_c,setNames,colnames)
re_list_qa_obs <- lapply(list_qa_obs_c,
                        function(x){x[c("Date","Time","Oobs","Cali_Valid_Flag")]
                        }) # reorder columns

unlink("H:/yangtze_river/w2_obs/*")
nam = list()
for (i in 1:length(re_list_qa_obs)){
  nam[[i]] <- paste("H:/yangtze_river/w2_obs/",
                    "obs_var_",i,".txt",sep = "")
  write.table(re_list_qa_obs[[i]],
              nam[[i]],
              row.names = FALSE,quote =FALSE)
}

# deal with simulated data


######4.1.2 simulated water quality data ####
q_fast_all = readRDS("H:/yangtze_river/sen_yangtze.rds")
name_qa_out <- paste0("nh4_out_",No_obs_qa,sep="")
name_qa_q_out <- paste0("q_out_",No_obs_qa,sep="")
date = as.list(q_fast_all[["simulation"]][[1]]["date"])


cali_qa_sim <- q_fast_all[["simulation"]][c(name_qa_out)]
cali_qa_sim <- lapply(cali_qa_sim,"[",-1)
  
cali_qa_q_sim <- q_fast_all[["simulation"]][c(name_qa_q_out)]
cali_qa_q_sim <- lapply(cali_qa_q_sim,"[",-1)

cali_qa_sim_c <- mapply(function(x1,y1) x1/y1/86.4,cali_qa_sim,
                        cali_qa_q_sim,SIMPLIFY = FALSE)
sim_qa = Map(cbind,cali_qa_sim_c,date = date)

sim_qa_id = list()
sim_qa_w = list()
simobs_qa_w = list()
qa_obs_w = list()
for (i in 1:length(sim_qa)){
  sim_qa_id[[i]] = sim_qa[[i]] %>% 
    mutate(weekday = as.numeric(format(date,format="%w"))) %>% 
    mutate(start_date = date + (1-weekday)) %>% 
    select(1656:1658,1:1655)
  sim_qa_w[[i]] = aggregate(sim_qa_id[[1]][4:1658],list(sim_qa_id[[i]][["start_date"]]),mean)
  colnames(sim_qa_w[[i]])[1] = "date"
  simobs_qa_w[[i]] = Date_qa[[i]] %>% 
    left_join(sim_qa_w[[i]],by = "date")
}

names(simobs_qa_w) = name_qa_out
Date_qa = lapply(list_qa_obs,"[",-2)
list_qa_sim = simobs_qa_w

  

# list_qa_sim = list()
# for (i in 1:length(cali_qa_sim)){
#   list_qa_sim[i]=lapply(cali_qa_sim[[i]], 
#                        function(x) dplyr::filter(x,date >= min(Date_qa[[i]][["date"]]),
#                                                  date <= max(Date_qa[[i]][["date"]])))
#   names(list_qa_sim)[[i]] <- name_qa_out[i] # name elements
#   
# }

list_qa_sim_nodate = lapply(list_qa_sim,"[",-1)
list_qa_sim_nodate[] = lapply(list_qa_sim_nodate,round,2)

unlink("H:/yangtze_river/Output/Core_1/*")
nam = list()
for (i in 1:length(list_qa_sim_nodate)){
  for (j in 1:ncol(list_qa_sim_nodate[[i]])){
    nam[[i]] <- paste("H:/yangtze_river/Output/Core_1/",
                      "out_var_",i,".txt",sep = "")
    write.table(list_qa_sim_nodate[[i]][,j],
                nam[[i]],
                row.names = FALSE,
                col.names = j,
                append = TRUE)
  }
}# delete if changing timeseries

####4.2 calibration using SUFI2####
dir = "H:/yangtze_river"
setwd(dir)
dir2 = "H:/yangtze_river/w2_obs"

df = data.frame(seq.int(nrow(q_fast_all[["parameter"]][["values"]])))
names(df) = "V1"
parameterValue = cbind(df,q_fast_all[["parameter"]][["values"]])

reach = paste(noquote(No_obs_qa),collapse = ",")
outputExtraction <- data.frame(
  FileType = c("output.rch"),    # if for two files: = c("watout.dat", "output.rch"),
  FileName = c("output.rch"),    #                   = c("watout.dat", "output.rch"),
  Column = c("20"),               #                   = c("4"         , "6"),
  Reach = reach             #                   = c(" "         , "2")
)
OutputVar <- getNumberOutputVar(outputExtraction)
nOutputVar <- OutputVar$nOutputVar
ncores = 1
userReadSwatOutput <- OutputVar$userReadSwatOutput

observedDataFile <- list.files(dir2,pattern="obs_var",all.files = TRUE,
                               full.names = TRUE)
observedDataFile <- sortObservedDataFile(observedDataFile)
observedData <- list()
for (i in 1:length(observedDataFile)){
  # Read observed files and save to a dummy variable
  temp <- read.table(observedDataFile[i], skip = 1, sep = "")
  # Get bbserved data from dummy variable
  observedData [[i]] <- data.frame(Date = as.POSIXct(paste(temp[,1], temp[,2], 
                                                           sep = " "), 
                                                     format = "%Y-%m-%d %H:%M", 
                                                     tz = ""),
                                   Value = temp[,3],
                                   Flag = temp[,4])
}

workingFolder <- dir
objFunction <- "NSE"

obj <- calObjFunction(parameterValue,
                      ncores,
                      nOutputVar,
                      userReadSwatOutput,
                      observedData,
                      workingFolder,
                      objFunction)


behThreshold <- -2
minOrmax <- "Maximize"
samplingApproach <- "Sensi_Cali_(uniform_Latin_Hypercube_Sampling)"
# varNumber = 1

for (i in seq(1:nOutputVar)){
  name = paste0('behavioral',i)
  print(name)
  data <- behaSimulation(obj$objValueCali,
                         obj$simData,
                         parameterValue,
                         behThreshold,
                         i,
                         objFunction,
                         observedData,
                         minOrmax,
                         samplingApproach)
  assign(name,data)
}
name = c()
for (i in 1:nOutputVar){
  name[i] = paste0('behavioral',i)
}  
all_behavioral = do.call("list",mget(name))




####4.3 save files ####
plot(obj$objValueCali)
save(obj,all_behavioral,name_q_out,
     file = "all_yangtze.Rdata")

####4.4 plot figure ####
#plot best simulation verse observation
plot_list_qa = list()
behasim = list()
obs_sim = list()
for (i in 1:length(all_behavioral)){
  behasim[[i]] = all_behavioral[[i]][["ppuSimData"]]
  obs_sim[[i]] = behasim[[i]] %>% 
    left_join(list_qa_obs[[i]],by = "Date") %>%   
    select(1,2,4:6) %>%  
    gather(.,type,value,c(4,5),factor_key = TRUE)
  p2 = ggplot(obs_sim[[i]])+
    geom_line(aes(Date,value,col = type))+
    geom_ribbon(aes(ymin = lower_95PPU,
                    ymax = upper_95PPU,
                    x= Date),
                fill = "grey50",alpha = 0.3)+
    scale_color_manual(values=c("red","black"))+
    theme(text = element_text(size=20))
  plot_list_qa[[i]] = p2
  
}

for (i in 1:length(plot_list_qa)){
  file_name = paste("obs_sim_qa_",i,".tiff",sep = "")
  tiff (file_name,width = 800,height = 450)
  print(plot_list_qa[[i]])
  dev.off()
}

#plot distribution of selected observed station 
path <- "F:/wangjinz/lexi/yangtze_river/TxtInOut"

# Path to the subbasin shape file
sub_path <- paste(path,"/Shapes/subs1.shp",sep = "")
riv_path <- paste(path,"/Shapes/riv1.shp",sep = "")

# Read shapefile from the path
sub <- read_sf(sub_path) %>% 
  filter(Subbasin %in% No_obs_qa)
riv <- read_sf(riv_path) 
ggplot() +
  geom_sf(data = sub) +
  geom_sf(data = riv, col = "royalblue", lwd = 0.75) +
  geom_sf_label(data = sub, aes(label = c(1:length(No_obs_qa)))) +
  theme_bw()
