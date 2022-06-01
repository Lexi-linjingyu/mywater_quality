
### 1.Install and library packages ####

setwd("F:/wangjinz/lexi/code/senSTM") # set workplace
path <-  getwd()
Packages <- c("SWATplusR","purrr","sf","tidyr","fast","dplyr",
              "ggplot2", "sensitivity", "lhs","hydromad","lubridate",
              "hydroGOF","forcats","EcoHydRology","magrittr")

##lapply(Packages,install.packages,character.only = TRUE) # Only for the first running
lapply(Packages, library, character.only = TRUE)

### 2.Sensitivity analysis ####
####2.1 load data ####
sensitivities <- readRDS("sensitivities.rds")

# load all observed streamflow data

pathTOobs = sort(list.files("F:/wangjinz/lexi/code/senSTM",
                      pattern="\\.csv$",full.names = TRUE)) # list all .csv file
No_obs = regmatches(pathTOobs, regexpr("\\d+",pathTOobs))# extract id's number
name_q <- paste0("q_out_",No_obs,sep="")

list_q_obsAll <- lapply(pathTOobs,read.csv)# read all .csv file
list_q_obs <- list()
for (i in 1:length(No_obs)){
  list_q_obsAll[[i]][["date"]] <- as.Date(as.character(list_q_obsAll[[i]][["date"]]),
                                     format ="%Y-%m-%d")#reformat the date
  list_q_obs[[i]] <- filter(list_q_obsAll[[i]],
                            date >= ymd("1963-01-01"), date <= "2008-12-31")
  names(list_q_obs)[[i]] <- name_q[i]# name elements
} 

## extract simulated streamflow data 
sens_q_sim <- sensitivities[["simulation"]][c(name_q)]
list_q_sim <- lapply(sens_q_sim,
                     function(x) dplyr::filter(x,date >= ymd("1963-01-01"), 
                                             date <= "2008-12-31"))


####2.2 select the best performance ####
nse_fast <- list()
q_plot <- list()
k <- c()
k_name <- c()

for (i in 1:length(list_q_sim)){
    nse_fast[[i]] <- list_q_sim[[i]] %>%
    select(-date) %>%
    map_dfc(., ~NSE(.x, list_q_obs[[i]][["discharge"]]))
    k[i] = match(head(sort(nse_fast[[i]],decreasing = TRUE),n=10)[1],
                 nse_fast[[i]])
    if (k[i] < 10) {
      k_name[i] = paste("run_000",k[i],sep="")
    } else if (k[i]< 100){
      k_name[i] = paste("run_00",k[i],sep="")    
      } else if (k[i] < 1000) {
        k_name[i] = paste("run_0",k[i],sep="")
      } else  {
        k_name[i] = paste("run_",k[i],sep = "")
      } 
    q_plot[[i]] <- list_q_sim[[i]] %>%
      select(date, k_name[i]) %>%
      dplyr::rename(q_sim = k_name[i]) %>%
      left_join(., list_q_obs[[i]], by = "date") %>%
      dplyr::rename(q_obs = discharge) %>%
      gather(., key = "variable", value = "discharge",-date)
    ggplot(data = q_plot[[i]]) +
      geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
      scale_color_manual(values = c("black", "tomato3")) +
      labs(y = expression("streamflow ("~m^3/s~")"))+
      scale_y_continuous()+
      theme_bw()
    ggsave((paste("best_",No_obs[i],".jpg",sep = "")),width = 9,height = 4)
}   #just ignore the warning 

####2.3 select sensitive parameters####
nse_fast_min <- list()
sens_fast <- list()
result_fast <- list()
for (i in 1:length(list_q_sim)){
  nse_fast_min[[i]] <- list_q_sim[[i]] %>%
                       select(-date) %>%
                       map_dbl(., ~NSE(.x, list_q_obs[[i]][["discharge"]]))
  sens_fast[[i]]<- sensitivity(nse_fast_min[[i]], 
                               nrow(sensitivities$parameter$definition))
  result_fast[[i]] <- tibble(parameter = sensitivities$parameter$definition$par_name,
                             fast = sens_fast[[i]]) %>%
                             mutate(parameter = factor(parameter) %>% 
                             fct_reorder(., fast))
  ggplot(data = result_fast[[i]]) +
    geom_col(aes(x = parameter, y = fast)) +
    xlab("Parameter for water yield") +
    ylab("Sensitivity") +
    coord_flip() +
    theme_bw()
  ggsave((paste("senPA",No_obs[i],".jpg",sep = "")),width = 4,height = 4)
}


####2.4 select parameters ranges####
dotty <- list()
for (i in 1:length(nse_fast_min)){
  dotty[[i]] <- sensitivities$parameter$values %>%
    mutate(nse = nse_fast_min[[i]]) %>%
    filter(nse > 0) %>%
    gather(key = "par", value = "parameter_range", -nse)
  ggplot(data = dotty[[i]]) +
    geom_point(aes(x = parameter_range, y = nse)) +
    facet_wrap(.~par, ncol = 4, scales = "free_x") +
    theme_bw()
  ggsave((paste("nsePA_",No_obs[i],".jpg")),width = 12,height = 10)
}


####2.5 package all .jpg file ####
pic <-  list.files(path,"\\.jpg$")
dir.create("sen")
path2 <- paste0(path,"/","sen",sep = "")
file.copy(paste0(path,"/",pic,sep = ""),
          paste0(path2,"/",pic,sep = ""),
          overwrite = TRUE)
