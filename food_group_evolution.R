rm(list=ls())

library(tidyr)
library(ggplot2)
library(dplyr)
library(patchwork)

guapa<-function(x)
{
  redondeo<-ifelse(abs(x)<0.00001,signif(x,1),
                   ifelse(abs(x)<0.0001,signif(x,1),
                          ifelse(abs(x)<0.001,signif(x,1),
                                 ifelse(abs(x)<0.1,sprintf("%.3f",round(x,3)),
                                        ifelse(abs(x)<1,sprintf("%.2f",round(x,2)),
                                               ifelse(abs(x)<10,sprintf("%.2f",round(x,2)),
                                                      ifelse(abs(x)<100,sprintf("%.1f",round(x,1)),
                                                             ifelse(abs(x)>=100,round(x,0),round(x,0)))))))))
  return(redondeo)
}

ic_guapa2<-function(x,y,z)
{
  ic<-paste(x," (",y," to ",z,")",sep="")
  return(ic)
}

z<-qnorm(1-0.05/2)


##############################################################
### TRAJECTORIES OF FOOD GROUPS - WOMEN/MEN AND AGE GROUPS ###
##############################################################

### WEIGHTED SUMS OF FOOD GROUP INTAKE IN ENALIA/2 ###
######################################################

# Sum of the means, squared root of the ^2 of standard errors

setwd("C:/Users/Alvaro/Documents/Documentos/Artículos/Gender_Age/Data")

dat<-read.csv2("./datos_enalia.csv",header=TRUE,sep=";",dec=".")
names(dat)<-c("nombre","categoria",
              "mean_01_03","se_01_03",
              "mean_04_09","se_04_09",
              "mean_10_17","se_10_17",
              "mean_18_39","se_18_39", 
              "mean_40_64","se_40_64",
              "mean_65_74","se_65_74")

enalia <- function(means, ses) {
  weighted_mean <- sum(means)
  weighted_se <- sqrt(sum(ses^2))
  return(c(mean = weighted_mean, se = weighted_se))
}

vars01<-c("verdura","fruta","legumbre","pan_cer","pap","pescado","carne",
          "embutido","lacteos","dulces","refrescos","snacks","zumos","huevos")
tab<-NULL

for(i in 1:length(vars01))
  
{
  datos<-subset(dat,categoria==vars01[i])
  calc01<-enalia(datos$mean_01_03, datos$se_01_03)
  calc02<-enalia(datos$mean_04_09, datos$se_04_09)
  calc03<-enalia(datos$mean_10_17, datos$se_10_17)
  calc04<-enalia(datos$mean_18_39, datos$se_18_39)
  calc05<-enalia(datos$mean_40_64, datos$se_40_64)
  calc06<-enalia(datos$mean_65_74, datos$se_65_74)
  tab<-rbind(tab,cbind(calc01[1],calc01[2],calc02[1],calc02[2],calc03[1],calc03[2],
                       calc04[1],calc04[2],calc05[1],calc05[2],calc06[1],calc06[2]))
}

colnames(tab)<-c("mean_01_03","se_01_03","mean_04_09","se_04_09","mean_10_17","se_10_17",
                 "mean_18_39","se_18_39","mean_40_64","se_40_64","mean_65_74","se_65_74")
rownames(tab)<-NULL
tab<-as.data.frame(tab)
tab$categoria<-vars01

tab$w1_01_04<-0.75
tab$w2_01_04<-0.25
tab$w1_05_14<-0.5
tab$w2_05_14<-0.5
tab$w1_15_24<-0.3
tab$w2_15_24<-0.7
tab$w1_35_44<-0.5
tab$w2_35_44<-0.5

tab$mean_0104<-(tab$mean_01_03*tab$w1_01_04)+(tab$mean_04_09*tab$w2_01_04)
tab$se_0104<-sqrt((((tab$se_01_03^2)*tab$w1_01_04)+((tab$se_04_09^2)*tab$w2_01_04))/(tab$w1_01_04+tab$w2_01_04))
tab$mean_0514<-(tab$mean_04_09*tab$w1_05_14)+(tab$mean_10_17*tab$w2_05_14)
tab$se_0514<-sqrt((((tab$se_04_09^2)*tab$w1_05_14)+((tab$se_10_17^2)*tab$w2_05_14))/(tab$w1_05_14+tab$w2_05_14))
tab$mean_1524<-(tab$mean_10_17*tab$w1_15_24)+(tab$mean_18_39*tab$w2_15_24)
tab$se_1524<-sqrt((((tab$se_10_17^2)*tab$w1_15_24)+((tab$se_18_39^2)*tab$w2_15_24))/(tab$w1_15_24+tab$w2_15_24))
tab$mean_2534<-tab$mean_18_39
tab$se_2534<-tab$se_18_39
tab$mean_3544<-(tab$mean_18_39*tab$w1_35_44)+(tab$mean_40_64*tab$w2_35_44)
tab$se_3544<-sqrt((((tab$se_18_39^2)*tab$w1_35_44)+((tab$se_40_64^2)*tab$w2_35_44))/(tab$w1_35_44+tab$w2_35_44))
tab$mean_4554<-tab$mean_40_64
tab$se_4554<-tab$se_40_64
tab$mean_5564<-tab$mean_40_64
tab$se_5564<-tab$se_40_64
tab$mean_6574<-tab$mean_65_74
tab$se_6574<-tab$se_65_74
tab$mean_7599<-tab$mean_65_74
tab$se_7599<-tab$se_65_74
tab$categoria<-c("verdura","fruta","legumbres","pan_cereales","pasta_arroz_patata","pescado","carne",
                 "embutidos","lacteos","dulces","refrescos","aperitivos","zumos","huevos")

intake<-tab[,c("categoria","mean_0104","se_0104","mean_0514","se_0514",
               "mean_1524","se_1524","mean_2534","se_2534","mean_3544","se_3544",
               "mean_4554","se_4554","mean_5564","se_5564","mean_6574","se_6574",  
               "mean_7599","se_7599")]

write.table(intake,file="./consumo_grupos_alimentos.csv",sep=";",col.names=TRUE,row.names=FALSE)

intake[,2:19]<-apply(intake[, 2:19], 2, guapa)

write.table(intake,file="./consumo_grupos_alimentos_guapa.csv",sep=";",col.names=TRUE,row.names=FALSE)


### CREATION OF CLEAN DATABASES ###
###################################

setwd("C:/Users/Alvaro/Documents/Documentos/Artículos/Gender_Age/Data")
intake<-read.csv2("./consumo_grupos_alimentos.csv",header=TRUE,sep=";",dec=".")

weighted <- function(means, ses, ns) {
  means <- as.numeric(means)
  ses <- as.numeric(ses)
  ns <- as.numeric(ns)
  weighted_mean <- sum(means * ns, na.rm = TRUE) / sum(ns, na.rm = TRUE)
  weighted_var_sum <- sum((ses^2) * ns, na.rm = TRUE)
  total_sample_size <- sum(ns, na.rm = TRUE)
  se_corrected <- sqrt(weighted_var_sum / total_sample_size)
  return(c(mean = weighted_mean, se = se_corrected))
}

vars01<-c("verdura","fruta","legumbres","pan_cereales","pasta_arroz_patata","pescado","carne",
          "embutidos","lacteos","dulces","refrescos","aperitivos","zumos","huevos")

tab<-NULL

for(i in 1:length(vars01))
  
{
  tab<-NULL
  namefile2017<-paste("./csv/2017/",vars01[i],"2017.csv",sep="")
  dat<-read.csv2(namefile2017,header=TRUE,sep=";",dec=".")
  dat[4:12,3:4]<-as.data.frame(pivot_longer(intake[intake$categoria==vars01[i],], 
                                            cols = -categoria, 
                                            names_to = c(".value", "age_range"),
                                            names_pattern = "(mean|se)_(\\d+)"))[,3:4]
  datx<-dat[4:12,3:5]
  dat$serv_mean[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[1]
  dat$serv_se[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[2]
  dat$mean2017<-with(dat,serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  factor<-with(dat,serv_mean/mean2017)
  
  # Factor para hacer que las curvas describan un valor promedio de consumo en 2017
  # igual al consumo estimado para cada franja de edad en ENALIA (estudio de 2016)
  
  namefile2003<-paste("./csv/2003/",vars01[i],"2003.csv",sep="")
  dat<-read.csv2(namefile2003,header=TRUE,sep=";",dec=".")
  dat[4:12,3:4]<-as.data.frame(pivot_longer(intake[intake$categoria==vars01[i],], 
                                            cols = -categoria, 
                                            names_to = c(".value", "age_range"),
                                            names_pattern = "(mean|se)_(\\d+)"))[,3:4]
  datx<-dat[4:12,3:5]
  dat$serv_mean[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[1]
  dat$serv_se[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[2]
  dat$factor<-factor
  dat$mean2003<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2003<-with(dat,mean2003-(z*serv_se_adj))
  dat$hi2003<-with(dat,mean2003+(z*serv_se_adj))
  tab<-rbind(tab,cbind(dat$X,dat$mean2003,dat$lo2003,dat$hi2003))
  
  namefile2006<-paste("./csv/2006/",vars01[i],"2006.csv",sep="")
  dat<-read.csv2(namefile2006,header=TRUE,sep=";",dec=".")
  dat[4:12,3:4]<-as.data.frame(pivot_longer(intake[intake$categoria==vars01[i],], 
                                            cols = -categoria, 
                                            names_to = c(".value", "age_range"),
                                            names_pattern = "(mean|se)_(\\d+)"))[,3:4]
  datx<-dat[4:12,3:5]
  dat$serv_mean[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[1]
  dat$serv_se[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[2]
  dat$factor<-factor
  dat$mean2006<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2006<-with(dat,mean2006-(z*serv_se_adj))
  dat$hi2006<-with(dat,mean2006+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2006,dat$lo2006,dat$hi2006))
  
  namefile2009<-paste("./csv/2009/",vars01[i],"2009.csv",sep="")
  dat<-read.csv2(namefile2009,header=TRUE,sep=";",dec=".")
  dat[4:12,3:4]<-as.data.frame(pivot_longer(intake[intake$categoria==vars01[i],], 
                                            cols = -categoria, 
                                            names_to = c(".value", "age_range"),
                                            names_pattern = "(mean|se)_(\\d+)"))[,3:4]
  datx<-dat[4:12,3:5]
  dat$serv_mean[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[1]
  dat$serv_se[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[2]
  dat$factor<-factor
  dat$mean2009<-with(dat,factor*serv_mean*(freq1+freq2+(freq3*5/7)+(freq4*2/7)+(freq5*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4+p_freq5)/p_total))
  dat$lo2009<-with(dat,mean2009-(z*serv_se_adj))
  dat$hi2009<-with(dat,mean2009+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2009,dat$lo2009,dat$hi2009))
  
  namefile2011<-paste("./csv/2011/",vars01[i],"2011.csv",sep="")
  dat<-read.csv2(namefile2011,header=TRUE,sep=";",dec=".")
  dat[4:12,3:4]<-as.data.frame(pivot_longer(intake[intake$categoria==vars01[i],], 
                                            cols = -categoria, 
                                            names_to = c(".value", "age_range"),
                                            names_pattern = "(mean|se)_(\\d+)"))[,3:4]
  datx<-dat[4:12,3:5]
  dat$serv_mean[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[1]
  dat$serv_se[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[2]
  dat$factor<-factor
  dat$mean2011<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2011<-with(dat,mean2011-(z*serv_se_adj))
  dat$hi2011<-with(dat,mean2011+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2011,dat$lo2011,dat$hi2011))
  
  namefile2014<-paste("./csv/2014/",vars01[i],"2014.csv",sep="")
  dat<-read.csv2(namefile2014,header=TRUE,sep=";",dec=".")
  dat[4:12,3:4]<-as.data.frame(pivot_longer(intake[intake$categoria==vars01[i],], 
                                            cols = -categoria, 
                                            names_to = c(".value", "age_range"),
                                            names_pattern = "(mean|se)_(\\d+)"))[,3:4]
  datx<-dat[4:12,3:5]
  dat$serv_mean[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[1]
  dat$serv_se[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[2]
  dat$factor<-factor
  dat$mean2014<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2014<-with(dat,mean2014-(z*serv_se_adj))
  dat$hi2014<-with(dat,mean2014+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2014,dat$lo2014,dat$hi2014))
  
  namefile2017<-paste("./csv/2017/",vars01[i],"2017.csv",sep="")
  dat<-read.csv2(namefile2017,header=TRUE,sep=";",dec=".")
  dat[4:12,3:4]<-as.data.frame(pivot_longer(intake[intake$categoria==vars01[i],], 
                                            cols = -categoria, 
                                            names_to = c(".value", "age_range"),
                                            names_pattern = "(mean|se)_(\\d+)"))[,3:4]
  datx<-dat[4:12,3:5]
  dat$serv_mean[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[1]
  dat$serv_se[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[2]
  dat$factor<-factor
  dat$mean2017<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2017<-with(dat,mean2017-(z*serv_se_adj))
  dat$hi2017<-with(dat,mean2017+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2017,dat$lo2017,dat$hi2017))
  
  namefile2020<-paste("./csv/2020/",vars01[i],"2020.csv",sep="")
  dat<-read.csv2(namefile2020,header=TRUE,sep=";",dec=".")
  dat[4:12,3:4]<-as.data.frame(pivot_longer(intake[intake$categoria==vars01[i],], 
                                            cols = -categoria, 
                                            names_to = c(".value", "age_range"),
                                            names_pattern = "(mean|se)_(\\d+)"))[,3:4]
  datx<-dat[4:12,3:5]
  dat$serv_mean[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[1]
  dat$serv_se[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[2]
  dat$factor<-factor
  dat$mean2020<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2020<-with(dat,mean2020-(z*serv_se_adj))
  dat$hi2020<-with(dat,mean2020+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2020,dat$lo2020,dat$hi2020))
  
  namefile2023<-paste("./csv/2023/",vars01[i],"2023.csv",sep="")
  dat<-read.csv2(namefile2023,header=TRUE,sep=";",dec=".")
  dat[4:12,3:4]<-as.data.frame(pivot_longer(intake[intake$categoria==vars01[i],], 
                                            cols = -categoria, 
                                            names_to = c(".value", "age_range"),
                                            names_pattern = "(mean|se)_(\\d+)"))[,3:4]
  datx<-dat[4:12,3:5]
  dat$serv_mean[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[1]
  dat$serv_se[1:3]<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)[2]
  dat$factor<-factor
  dat$mean2023<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2023<-with(dat,mean2023-(z*serv_se_adj))
  dat$hi2023<-with(dat,mean2023+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2023,dat$lo2023,dat$hi2023))
  
  namefile<-paste("./clean/",vars01[i],".csv",sep="")
  colnames(tab)<-c("category","mean2003","lo2003","hi2003","mean2006","lo2006","hi2006",
                   "mean2009","lo2009","hi2009","mean2011","lo2011","hi2011",
                   "mean2014","lo2014","hi2014","mean2017","lo2017","hi2017",
                   "mean2020","lo2020","hi2020","mean2023","lo2023","hi2023")
  write.table(tab,file=namefile,sep=";",col.names=TRUE,row.names=FALSE)
  
  namefile<-paste("./clean/export/",vars01[i],".csv",sep="")
  tab<-as.data.frame(tab)
  tab$coef2003<-ic_guapa2(guapa(as.numeric(tab$mean2003)),guapa(as.numeric(tab$lo2003)),
                          guapa(as.numeric(tab$hi2003)))
  tab$coef2003<-with(tab,ifelse(coef2003=="NA (NA to NA)",NA,coef2003))
  tab$coef2006<-ic_guapa2(guapa(as.numeric(tab$mean2006)),guapa(as.numeric(tab$lo2006)),
                          guapa(as.numeric(tab$hi2006)))
  tab$coef2006<-with(tab,ifelse(coef2006=="NA (NA to NA)",NA,coef2006))
  tab$coef2009<-ic_guapa2(guapa(as.numeric(tab$mean2009)),guapa(as.numeric(tab$lo2009)),
                          guapa(as.numeric(tab$hi2009)))
  tab$coef2009<-with(tab,ifelse(coef2009=="NA (NA to NA)",NA,coef2009))
  tab$coef2011<-ic_guapa2(guapa(as.numeric(tab$mean2011)),guapa(as.numeric(tab$lo2011)),
                          guapa(as.numeric(tab$hi2011)))
  tab$coef2011<-with(tab,ifelse(coef2011=="NA (NA to NA)",NA,coef2011))
  tab$coef2014<-ic_guapa2(guapa(as.numeric(tab$mean2014)),guapa(as.numeric(tab$lo2014)),
                          guapa(as.numeric(tab$hi2014)))
  tab$coef2014<-with(tab,ifelse(coef2014=="NA (NA to NA)",NA,coef2014))
  tab$coef2017<-ic_guapa2(guapa(as.numeric(tab$mean2017)),guapa(as.numeric(tab$lo2017)),
                          guapa(as.numeric(tab$hi2017)))
  tab$coef2017<-with(tab,ifelse(coef2017=="NA (NA to NA)",NA,coef2017))
  tab$coef2020<-ic_guapa2(guapa(as.numeric(tab$mean2020)),guapa(as.numeric(tab$lo2020)),
                          guapa(as.numeric(tab$hi2020)))
  tab$coef2020<-with(tab,ifelse(coef2020=="NA (NA to NA)",NA,coef2020))
  tab$coef2023<-ic_guapa2(guapa(as.numeric(tab$mean2023)),guapa(as.numeric(tab$lo2023)),
                          guapa(as.numeric(tab$hi2023)))
  tab$coef2023<-with(tab,ifelse(coef2023=="NA (NA to NA)",NA,coef2023))
  tab<-tab[,c("category","coef2003","coef2006","coef2009","coef2011",
              "coef2014","coef2017","coef2020","coef2023")]
  write.table(tab,file=namefile,sep=";",col.names=TRUE,row.names=FALSE)
}


### CREATION OF TREND FIGURES ###
#################################

setwd("C:/Users/Alvaro/Documents/Documentos/Artículos/Gender_Age/Data")

vars01<-c("verdura","fruta","legumbres","pan_cereales","pasta_arroz_patata","pescado","carne",
          "embutidos","lacteos","huevos","dulces","aperitivos","refrescos","zumos")
vars02<-c("Intake of vegetables (g/day)","Intake of fruits (g/day)",
          "Intake of legumes (g/day)","Intake of bread and cereals (g/day)",
          "Intake of pasta, rice and potatoes (g/day)","Intake of fish (g/day)",
          "Intake of unprocessed meat (g/day)","Intake of processed meat (g/day)",
          "Intake of dairy products (g/day)","Intake of eggs (g/day)",
          "Intake of sweets (g/day)","Intake of salty snacks (g/day)",
          "Intake of soft drinks (g/day)","Intake of fresh juices (g/day)")

tab<-NULL

for(i in 1:length(vars01))
  
{
  namefile<-paste("./clean/",vars01[i],".csv",sep="")
  dat<-read.csv2(namefile, header = TRUE, sep = ";", dec = ".")[, c(2:7, 11:25)]  
  df_all<-pivot_longer(dat[1,], 
                       cols = everything(), 
                       names_to = c(".value", "year"), 
                       names_pattern = "(mean|lo|hi)(\\d+)")
  df_women<-pivot_longer(dat[2,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_men<-pivot_longer(dat[3,], 
                       cols = everything(), 
                       names_to = c(".value", "year"), 
                       names_pattern = "(mean|lo|hi)(\\d+)")
  df_01_04<-pivot_longer(dat[4,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_05_14<-pivot_longer(dat[5,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_15_24<-pivot_longer(dat[6,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_25_34<-pivot_longer(dat[7,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_35_44<-pivot_longer(dat[8,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_45_54<-pivot_longer(dat[9,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_55_64<-pivot_longer(dat[10,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_65_74<-pivot_longer(dat[11,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_75plus<-pivot_longer(dat[12,], 
                          cols = everything(), 
                          names_to = c(".value", "year"), 
                          names_pattern = "(mean|lo|hi)(\\d+)")
  minimum<-(min(min(df_all$lo,na.rm=TRUE),min(df_women$lo,na.rm=TRUE),min(df_men$lo,na.rm=TRUE),
                min(df_01_04$lo,na.rm=TRUE),min(df_05_14$lo,na.rm=TRUE),min(df_15_24$lo,na.rm=TRUE),
                min(df_25_34$lo,na.rm=TRUE),min(df_35_44$lo,na.rm=TRUE),min(df_45_54$lo,na.rm=TRUE),
                min(df_55_64$lo,na.rm=TRUE),min(df_65_74$lo,na.rm=TRUE),min(df_75plus$lo,na.rm=TRUE)))*0.95
  maximum<-(max(max(df_all$hi,na.rm=TRUE),max(df_women$hi,na.rm=TRUE),max(df_men$hi,na.rm=TRUE),
                max(df_01_04$hi,na.rm=TRUE),max(df_05_14$hi,na.rm=TRUE),max(df_15_24$hi,na.rm=TRUE),
                max(df_25_34$hi,na.rm=TRUE),max(df_35_44$hi,na.rm=TRUE),max(df_45_54$hi,na.rm=TRUE),
                max(df_55_64$hi,na.rm=TRUE),max(df_65_74$hi,na.rm=TRUE),max(df_75plus$hi,na.rm=TRUE)))*1.05
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_all)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_all)
  years <- seq(min(as.numeric(df_all$year)), max(as.numeric(df_all$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_all<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_all, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "All participants, all ages",
      tag = "A",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_women)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_women)
  years <- seq(min(as.numeric(df_women$year)), max(as.numeric(df_women$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_women<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_women, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "Women only, all ages",
      tag = "B",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_men)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_men)
  years <- seq(min(as.numeric(df_men$year)), max(as.numeric(df_men$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_men<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_men, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "Men only, all ages",
      tag = "C",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_01_04)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_01_04)
  years <- seq(min(as.numeric(df_01_04$year)), max(as.numeric(df_01_04$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_01_04<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_01_04, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "1-4 years",
      tag = "D",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_05_14)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_05_14)
  years <- seq(min(as.numeric(df_05_14$year)), max(as.numeric(df_05_14$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_05_14<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_05_14, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "5-14 years",
      tag = "E",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_15_24)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_15_24)
  years <- seq(min(as.numeric(df_15_24$year)), max(as.numeric(df_15_24$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_15_24<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_15_24, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "15-24 years",
      tag = "F",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_25_34)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_25_34)
  years <- seq(min(as.numeric(df_25_34$year)), max(as.numeric(df_25_34$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_25_34<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_25_34, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "25-34 years",
      tag = "G",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_35_44)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_35_44)
  years <- seq(min(as.numeric(df_35_44$year)), max(as.numeric(df_35_44$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_35_44<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_35_44, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "35-44 years",
      tag = "H",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_45_54)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_45_54)
  years <- seq(min(as.numeric(df_45_54$year)), max(as.numeric(df_45_54$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_45_54<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_45_54, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "45-54 years",
      tag = "I",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_55_64)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_55_64)
  years <- seq(min(as.numeric(df_55_64$year)), max(as.numeric(df_55_64$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_55_64<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_55_64, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "55-64 years",
      tag = "J",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_65_74)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_65_74)
  years <- seq(min(as.numeric(df_65_74$year)), max(as.numeric(df_65_74$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_65_74<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_65_74, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "65-74 years",
      tag = "K",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_75plus)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_75plus)
  years <- seq(min(as.numeric(df_75plus$year)), max(as.numeric(df_75plus$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_75plus<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_75plus, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "75 years and older",
      tag = "L",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  figure_combined <- figure_all + figure_women + figure_men + 
    figure_01_04 + figure_05_14 + figure_15_24 + 
    figure_25_34 + figure_35_44 + figure_45_54 + 
    figure_55_64 + figure_65_74 + figure_75plus + 
    plot_layout(ncol = 3, nrow = 4)
  
  namefile<-paste("C:/Users/Alvaro/Documents/Documentos/Artículos/Gender_Age/Outputs/",vars01[i],".jpg",sep="")
  ggsave(filename=namefile,units="px", width=12000, height=16000, dpi=1200, bg="white")
  par(las=1,cex=1.2,mar=c(6,6,2,0),bty="n",lheight=0.9)
  figure_combined
  dev.off()
  
  diff_all<-guapa((tail(df_all$mean[!is.na(df_all$mean)],1)-head(df_all$mean[!is.na(df_all$mean)],1))/head(df_all$mean[!is.na(df_all$mean)],1)*100)
  diff_women<-guapa((tail(df_women$mean[!is.na(df_women$mean)],1)-head(df_women$mean[!is.na(df_women$mean)],1))/head(df_women$mean[!is.na(df_women$mean)],1)*100)
  diff_men<-guapa((tail(df_men$mean[!is.na(df_men$mean)],1)-head(df_men$mean[!is.na(df_men$mean)],1))/head(df_men$mean[!is.na(df_men$mean)],1)*100)
  diff_01_04<-guapa((tail(df_01_04$mean[!is.na(df_01_04$mean)],1)-head(df_01_04$mean[!is.na(df_01_04$mean)],1))/head(df_01_04$mean[!is.na(df_01_04$mean)],1)*100)
  diff_05_14<-guapa((tail(df_05_14$mean[!is.na(df_05_14$mean)],1)-head(df_05_14$mean[!is.na(df_05_14$mean)],1))/head(df_05_14$mean[!is.na(df_05_14$mean)],1)*100)
  diff_15_24<-guapa((tail(df_15_24$mean[!is.na(df_15_24$mean)],1)-head(df_15_24$mean[!is.na(df_15_24$mean)],1))/head(df_15_24$mean[!is.na(df_15_24$mean)],1)*100)
  diff_25_34<-guapa((tail(df_25_34$mean[!is.na(df_25_34$mean)],1)-head(df_25_34$mean[!is.na(df_25_34$mean)],1))/head(df_25_34$mean[!is.na(df_25_34$mean)],1)*100)
  diff_35_44<-guapa((tail(df_35_44$mean[!is.na(df_35_44$mean)],1)-head(df_35_44$mean[!is.na(df_35_44$mean)],1))/head(df_35_44$mean[!is.na(df_35_44$mean)],1)*100)
  diff_45_54<-guapa((tail(df_45_54$mean[!is.na(df_45_54$mean)],1)-head(df_45_54$mean[!is.na(df_45_54$mean)],1))/head(df_45_54$mean[!is.na(df_45_54$mean)],1)*100)
  diff_55_64<-guapa((tail(df_55_64$mean[!is.na(df_55_64$mean)],1)-head(df_55_64$mean[!is.na(df_55_64$mean)],1))/head(df_55_64$mean[!is.na(df_55_64$mean)],1)*100)
  diff_65_74<-guapa((tail(df_65_74$mean[!is.na(df_65_74$mean)],1)-head(df_65_74$mean[!is.na(df_65_74$mean)],1))/head(df_65_74$mean[!is.na(df_65_74$mean)],1)*100)
  diff_75plus<-guapa((tail(df_75plus$mean[!is.na(df_75plus$mean)],1)-head(df_75plus$mean[!is.na(df_75plus$mean)],1))/head(df_75plus$mean[!is.na(df_75plus$mean)],1)*100)
  tab<-rbind(tab,cbind(diff_all,diff_women,diff_men,diff_01_04,diff_05_14,diff_15_24,
                       diff_25_34,diff_35_44,diff_45_54,diff_55_64,diff_65_74,diff_75plus))
}

rownames(tab)<-vars01
write.table(t(tab),file="C:/Users/Alvaro/Documents/Documentos/Artículos/Gender_Age/Outputs/percentages.csv",
            sep=";",col.names=TRUE,row.names=TRUE)


########################################################################
### TRAJECTORIES OF FOOD GROUPS - COUNTRY OF ORIGIN AND SOCIAL CLASS ###
########################################################################

### WEIGHTED SUMS OF FOOD GROUP INTAKE IN ENALIA/2 ###
######################################################

# Sum of the means, squared root of the ^2 of standard errors

setwd("C:/Users/Alvaro/Documents/Documentos/Artículos/Socioeconomic/Data")

dat<-read.csv2("./datos_enalia.csv",header=TRUE,sep=";",dec=".")
names(dat)<-c("nombre","categoria",
              "mean_01_03","se_01_03",
              "mean_04_09","se_04_09",
              "mean_10_17","se_10_17",
              "mean_18_39","se_18_39", 
              "mean_40_64","se_40_64",
              "mean_65_74","se_65_74")

enalia <- function(means, ses) {
  weighted_mean <- sum(means)
  weighted_se <- sqrt(sum(ses^2))
  return(c(mean = weighted_mean, se = weighted_se))
}

vars01<-c("verdura","fruta","legumbre","pan_cer","pap","pescado","carne",
          "embutido","lacteos","dulces","refrescos","snacks","zumos","huevos")
tab<-NULL

for(i in 1:length(vars01))
  
{
  datos<-subset(dat,categoria==vars01[i])
  calc01<-enalia(datos$mean_01_03, datos$se_01_03)
  calc02<-enalia(datos$mean_04_09, datos$se_04_09)
  calc03<-enalia(datos$mean_10_17, datos$se_10_17)
  calc04<-enalia(datos$mean_18_39, datos$se_18_39)
  calc05<-enalia(datos$mean_40_64, datos$se_40_64)
  calc06<-enalia(datos$mean_65_74, datos$se_65_74)
  tab<-rbind(tab,cbind(calc01[1],calc01[2],calc02[1],calc02[2],calc03[1],calc03[2],
                       calc04[1],calc04[2],calc05[1],calc05[2],calc06[1],calc06[2]))
}

colnames(tab)<-c("mean_01_03","se_01_03","mean_04_09","se_04_09","mean_10_17","se_10_17",
                 "mean_18_39","se_18_39","mean_40_64","se_40_64","mean_65_74","se_65_74")
rownames(tab)<-NULL
tab<-as.data.frame(tab)
tab$categoria<-vars01

tab$w1_01_04<-0.75
tab$w2_01_04<-0.25
tab$w1_05_14<-0.5
tab$w2_05_14<-0.5
tab$w1_15_24<-0.3
tab$w2_15_24<-0.7
tab$w1_35_44<-0.5
tab$w2_35_44<-0.5

tab$mean_0104<-(tab$mean_01_03*tab$w1_01_04)+(tab$mean_04_09*tab$w2_01_04)
tab$se_0104<-sqrt((((tab$se_01_03^2)*tab$w1_01_04)+((tab$se_04_09^2)*tab$w2_01_04))/(tab$w1_01_04+tab$w2_01_04))
tab$mean_0514<-(tab$mean_04_09*tab$w1_05_14)+(tab$mean_10_17*tab$w2_05_14)
tab$se_0514<-sqrt((((tab$se_04_09^2)*tab$w1_05_14)+((tab$se_10_17^2)*tab$w2_05_14))/(tab$w1_05_14+tab$w2_05_14))
tab$mean_1524<-(tab$mean_10_17*tab$w1_15_24)+(tab$mean_18_39*tab$w2_15_24)
tab$se_1524<-sqrt((((tab$se_10_17^2)*tab$w1_15_24)+((tab$se_18_39^2)*tab$w2_15_24))/(tab$w1_15_24+tab$w2_15_24))
tab$mean_2534<-tab$mean_18_39
tab$se_2534<-tab$se_18_39
tab$mean_3544<-(tab$mean_18_39*tab$w1_35_44)+(tab$mean_40_64*tab$w2_35_44)
tab$se_3544<-sqrt((((tab$se_18_39^2)*tab$w1_35_44)+((tab$se_40_64^2)*tab$w2_35_44))/(tab$w1_35_44+tab$w2_35_44))
tab$mean_4554<-tab$mean_40_64
tab$se_4554<-tab$se_40_64
tab$mean_5564<-tab$mean_40_64
tab$se_5564<-tab$se_40_64
tab$mean_6574<-tab$mean_65_74
tab$se_6574<-tab$se_65_74
tab$mean_7599<-tab$mean_65_74
tab$se_7599<-tab$se_65_74
tab$categoria<-c("verdura","fruta","legumbres","pan_cereales","pasta_arroz_patatas","pescado","carne",
                 "embutido","lacteos","dulces","refrescos","aperitivos","zumos","huevos")

intake<-tab[,c("categoria","mean_0104","se_0104","mean_0514","se_0514",
               "mean_1524","se_1524","mean_2534","se_2534","mean_3544","se_3544",
               "mean_4554","se_4554","mean_5564","se_5564","mean_6574","se_6574",  
               "mean_7599","se_7599")]

write.table(intake,file="./consumo_grupos_alimentos.csv",sep=";",col.names=TRUE,row.names=FALSE)

intake[,2:19]<-apply(intake[, 2:19], 2, guapa)

write.table(intake,file="./consumo_grupos_alimentos_guapa.csv",sep=";",col.names=TRUE,row.names=FALSE)


### CREATION OF CLEAN DATABASES ###
###################################

setwd("C:/Users/Alvaro/Documents/Documentos/Artículos/Socioeconomic/Data")
intake<-read.csv2("./consumo_grupos_alimentos.csv",header=TRUE,sep=";",dec=".")

weighted <- function(means, ses, ns) {
  means <- as.numeric(means)
  ses <- as.numeric(ses)
  ns <- as.numeric(ns)
  weighted_mean <- sum(means * ns, na.rm = TRUE) / sum(ns, na.rm = TRUE)
  weighted_var_sum <- sum((ses^2) * ns, na.rm = TRUE)
  total_sample_size <- sum(ns, na.rm = TRUE)
  se_corrected <- sqrt(weighted_var_sum / total_sample_size)
  return(c(mean = weighted_mean, se = se_corrected))
}

vars01<-c("verdura","fruta","legumbres","pan_cereales","pasta_arroz_patatas","pescado","carne",
          "embutido","lacteos","dulces","refrescos","aperitivos","zumos","huevos")

tab<-NULL

for(i in 1:length(vars01))
  
{
  tab<-NULL
  namefile2017<-paste("C:/Users/Alvaro/Documents/Documentos/Artículos/Gender_Age/Data/csv/2017/",vars01[i],"2017.csv",sep="")
  dat<-read.csv2(namefile2017,header=TRUE,sep=";",dec=".")
  dat[4:12,3:4]<-as.data.frame(pivot_longer(intake[intake$categoria==vars01[i],], 
                                            cols = -categoria, 
                                            names_to = c(".value", "age_range"),
                                            names_pattern = "(mean|se)_(\\d+)"))[,3:4]
  datx<-dat[4:12,3:5]
  weighted_coef<-weighted(datx$serv_mean, datx$serv_se, datx$p_total)
  
  namefile2017<-paste("./csv/2017/2017_",vars01[i],".csv",sep="")
  dat<-read.csv2(namefile2017,header=TRUE,sep=",",dec=".")
  dat$serv_mean<-weighted_coef[1]
  dat$serv_se<-weighted_coef[2]
  dat$mean2017<-with(dat,serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  factor<-with(dat,serv_mean/mean2017)
  
  # Factor para hacer que las curvas describan un valor promedio de consumo en 2017
  # igual al consumo estimado para cada franja de edad en ENALIA (estudio de 2016)
  
  namefile2003<-paste("./csv/2003/2003_",vars01[i],".csv",sep="")
  dat<-read.csv2(namefile2003,header=TRUE,sep=",",dec=".")
  dat$serv_mean<-weighted_coef[1]
  dat$serv_se<-weighted_coef[2]
  dat$factor<-factor
  dat$mean2003<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2003<-with(dat,mean2003-(z*serv_se_adj))
  dat$hi2003<-with(dat,mean2003+(z*serv_se_adj))
  tab<-rbind(tab,cbind(dat$x,dat$mean2003,dat$lo2003,dat$hi2003))
  
  namefile2006<-paste("./csv/2006/2006_",vars01[i],".csv",sep="")
  dat<-read.csv2(namefile2006,header=TRUE,sep=",",dec=".")
  dat$serv_mean<-weighted_coef[1]
  dat$serv_se<-weighted_coef[2]
  dat$factor<-factor
  dat$mean2006<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2006<-with(dat,mean2006-(z*serv_se_adj))
  dat$hi2006<-with(dat,mean2006+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2006,dat$lo2006,dat$hi2006))
  
  namefile2011<-paste("./csv/2011/2011_",vars01[i],".csv",sep="")
  dat<-read.csv2(namefile2011,header=TRUE,sep=",",dec=".")
  dat$serv_mean<-weighted_coef[1]
  dat$serv_se<-weighted_coef[2]
  dat$factor<-factor
  dat$mean2011<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2011<-with(dat,mean2011-(z*serv_se_adj))
  dat$hi2011<-with(dat,mean2011+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2011,dat$lo2011,dat$hi2011))
  
  namefile2014<-paste("./csv/2014/2014_",vars01[i],".csv",sep="")
  dat<-read.csv2(namefile2014,header=TRUE,sep=",",dec=".")
  dat$serv_mean<-weighted_coef[1]
  dat$serv_se<-weighted_coef[2]
  dat$factor<-factor
  dat$mean2014<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2014<-with(dat,mean2014-(z*serv_se_adj))
  dat$hi2014<-with(dat,mean2014+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2014,dat$lo2014,dat$hi2014))
  
  namefile2017<-paste("./csv/2017/2017_",vars01[i],".csv",sep="")
  dat<-read.csv2(namefile2017,header=TRUE,sep=",",dec=".")
  dat$serv_mean<-weighted_coef[1]
  dat$serv_se<-weighted_coef[2]
  dat$factor<-factor
  dat$mean2017<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2017<-with(dat,mean2017-(z*serv_se_adj))
  dat$hi2017<-with(dat,mean2017+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2017,dat$lo2017,dat$hi2017))
  
  namefile2020<-paste("./csv/2020/2020_",vars01[i],".csv",sep="")
  dat<-read.csv2(namefile2020,header=TRUE,sep=",",dec=".")
  dat$serv_mean<-weighted_coef[1]
  dat$serv_se<-weighted_coef[2]
  dat$factor<-factor
  dat$mean2020<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2020<-with(dat,mean2020-(z*serv_se_adj))
  dat$hi2020<-with(dat,mean2020+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2020,dat$lo2020,dat$hi2020))
  
  namefile2023<-paste("./csv/2023/2023_",vars01[i],".csv",sep="")
  dat<-read.csv2(namefile2023,header=TRUE,sep=",",dec=".")
  dat$serv_mean<-weighted_coef[1]
  dat$serv_se<-weighted_coef[2]
  dat$factor<-factor
  dat$mean2023<-with(dat,factor*serv_mean*(freq1+(freq2*4.5/7)+(freq3*1.5/7)+(freq4*0.5/7)))
  dat$serv_se_adj<-with(dat,serv_se*sqrt((p_freq1+p_freq2+p_freq3+p_freq4)/p_total))
  dat$lo2023<-with(dat,mean2023-(z*serv_se_adj))
  dat$hi2023<-with(dat,mean2023+(z*serv_se_adj))
  tab<-cbind(tab,cbind(dat$mean2023,dat$lo2023,dat$hi2023))
  
  namefile<-paste("./clean/",vars01[i],".csv",sep="")
  colnames(tab)<-c("category","mean2003","lo2003","hi2003","mean2006","lo2006","hi2006",
                   "mean2011","lo2011","hi2011",
                   "mean2014","lo2014","hi2014","mean2017","lo2017","hi2017",
                   "mean2020","lo2020","hi2020","mean2023","lo2023","hi2023")
  write.table(tab,file=namefile,sep=";",col.names=TRUE,row.names=FALSE)
  
  namefile<-paste("./clean/export/",vars01[i],".csv",sep="")
  tab<-as.data.frame(tab)
  tab$coef2003<-ic_guapa2(guapa(as.numeric(tab$mean2003)),guapa(as.numeric(tab$lo2003)),
                          guapa(as.numeric(tab$hi2003)))
  tab$coef2003<-with(tab,ifelse(coef2003=="NA (NA to NA)",NA,coef2003))
  tab$coef2006<-ic_guapa2(guapa(as.numeric(tab$mean2006)),guapa(as.numeric(tab$lo2006)),
                          guapa(as.numeric(tab$hi2006)))
  tab$coef2006<-with(tab,ifelse(coef2006=="NA (NA to NA)",NA,coef2006))
  tab$coef2011<-ic_guapa2(guapa(as.numeric(tab$mean2011)),guapa(as.numeric(tab$lo2011)),
                          guapa(as.numeric(tab$hi2011)))
  tab$coef2011<-with(tab,ifelse(coef2011=="NA (NA to NA)",NA,coef2011))
  tab$coef2014<-ic_guapa2(guapa(as.numeric(tab$mean2014)),guapa(as.numeric(tab$lo2014)),
                          guapa(as.numeric(tab$hi2014)))
  tab$coef2014<-with(tab,ifelse(coef2014=="NA (NA to NA)",NA,coef2014))
  tab$coef2017<-ic_guapa2(guapa(as.numeric(tab$mean2017)),guapa(as.numeric(tab$lo2017)),
                          guapa(as.numeric(tab$hi2017)))
  tab$coef2017<-with(tab,ifelse(coef2017=="NA (NA to NA)",NA,coef2017))
  tab$coef2020<-ic_guapa2(guapa(as.numeric(tab$mean2020)),guapa(as.numeric(tab$lo2020)),
                          guapa(as.numeric(tab$hi2020)))
  tab$coef2020<-with(tab,ifelse(coef2020=="NA (NA to NA)",NA,coef2020))
  tab$coef2023<-ic_guapa2(guapa(as.numeric(tab$mean2023)),guapa(as.numeric(tab$lo2023)),
                          guapa(as.numeric(tab$hi2023)))
  tab$coef2023<-with(tab,ifelse(coef2023=="NA (NA to NA)",NA,coef2023))
  tab<-tab[,c("category","coef2003","coef2006","coef2011",
              "coef2014","coef2017","coef2020","coef2023")]
  write.table(tab,file=namefile,sep=";",col.names=TRUE,row.names=FALSE)
}


### CREATION OF TREND FIGURES ###
#################################

setwd("C:/Users/Alvaro/Documents/Documentos/Artículos/Socioeconomic/Data")

vars01<-c("verdura","fruta","legumbres","pan_cereales","pasta_arroz_patatas","pescado","carne",
          "embutido","lacteos","huevos","dulces","aperitivos","refrescos","zumos")
vars02<-c("Intake of vegetables (g/day)","Intake of fruits (g/day)",
          "Intake of legumes (g/day)","Intake of bread and cereals (g/day)",
          "Intake of pasta, rice and potatoes (g/day)","Intake of fish (g/day)",
          "Intake of unprocessed meat (g/day)","Intake of processed meat (g/day)",
          "Intake of dairy products (g/day)","Intake of eggs (g/day)",
          "Intake of sweets (g/day)","Intake of salty snacks (g/day)",
          "Intake of soft drinks (g/day)","Intake of fresh juices (g/day)")

tab<-NULL

for(i in 1:length(vars01))
  
{
  namefile<-paste("./clean/",vars01[i],".csv",sep="")
  dat<-read.csv2(namefile, header = TRUE, sep = ";", dec = ".")[, c(2:22)]  
  df_all<-pivot_longer(dat[1,], 
                       cols = everything(), 
                       names_to = c(".value", "year"), 
                       names_pattern = "(mean|lo|hi)(\\d+)")
  df_spain<-pivot_longer(dat[2,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_foreign<-pivot_longer(dat[3,], 
                       cols = everything(), 
                       names_to = c(".value", "year"), 
                       names_pattern = "(mean|lo|hi)(\\d+)")
  df_class01<-pivot_longer(dat[4,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_class02<-pivot_longer(dat[5,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_class03<-pivot_longer(dat[6,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_class04<-pivot_longer(dat[7,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_class05<-pivot_longer(dat[8,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")
  df_class06<-pivot_longer(dat[9,], 
                         cols = everything(), 
                         names_to = c(".value", "year"), 
                         names_pattern = "(mean|lo|hi)(\\d+)")

  minimum<-(min(min(df_all$lo,na.rm=TRUE),min(df_spain$lo,na.rm=TRUE),min(df_foreign$lo,na.rm=TRUE),
                min(df_class01$lo,na.rm=TRUE),min(df_class02$lo,na.rm=TRUE),min(df_class03$lo,na.rm=TRUE),
                min(df_class04$lo,na.rm=TRUE),min(df_class05$lo,na.rm=TRUE),min(df_class06$lo,na.rm=TRUE)))*0.95
  maximum<-(max(max(df_all$hi,na.rm=TRUE),max(df_spain$hi,na.rm=TRUE),max(df_foreign$hi,na.rm=TRUE),
                max(df_class01$hi,na.rm=TRUE),max(df_class02$hi,na.rm=TRUE),max(df_class03$hi,na.rm=TRUE),
                max(df_class04$hi,na.rm=TRUE),max(df_class05$hi,na.rm=TRUE),max(df_class06$hi,na.rm=TRUE)))*1.05
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_all)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_all)
  years <- seq(min(as.numeric(df_all$year)), max(as.numeric(df_all$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_all<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_all, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "All participants, all ages",
      tag = "A",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_spain)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_spain)
  years <- seq(min(as.numeric(df_spain$year)), max(as.numeric(df_spain$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_spain<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_spain, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "Born in Spain, all ages",
      tag = "B",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_foreign)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_foreign)
  years <- seq(min(as.numeric(df_foreign$year)), max(as.numeric(df_foreign$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_foreign<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_foreign, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "Not born in Spain, all ages",
      tag = "C",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_class01)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_class01)
  years <- seq(min(as.numeric(df_class01$year)), max(as.numeric(df_class01$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_class01<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_class01, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "Social class - Level I",
      tag = "D",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_class02)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_class02)
  years <- seq(min(as.numeric(df_class02$year)), max(as.numeric(df_class02$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_class02<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_class02, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "Social class - Level II",
      tag = "E",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_class03)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_class03)
  years <- seq(min(as.numeric(df_class03$year)), max(as.numeric(df_class03$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_class03<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_class03, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "Social class - Level III",
      tag = "F",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_class04)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_class04)
  years <- seq(min(as.numeric(df_class04$year)), max(as.numeric(df_class04$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_class04<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_class04, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "Social class - Level IV",
      tag = "G",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_class05)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_class05)
  years <- seq(min(as.numeric(df_class05$year)), max(as.numeric(df_class05$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_class05<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_class05, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "Social class - Level V",
      tag = "H",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  lo_smooth <- loess(lo ~ as.numeric(year), data = df_class06)
  hi_smooth <- loess(hi ~ as.numeric(year), data = df_class06)
  years <- seq(min(as.numeric(df_class06$year)), max(as.numeric(df_class06$year)), by = 0.1)
  lo_pred <- predict(lo_smooth, newdata = data.frame(year = years))
  hi_pred <- predict(hi_smooth, newdata = data.frame(year = years))
  shade_df <- data.frame(year = years, lo = lo_pred, hi = hi_pred)
  figure_class06<-ggplot() +
    geom_ribbon(data = shade_df, aes(x = year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.5) +
    geom_smooth(data = df_class06, aes(x = as.numeric(year), y = mean, group = 1), method = "loess", color = "black", se = FALSE) +
    theme_classic() +
    labs(
      title = "Social class - Level VI",
      tag = "I",
      x = "Year",
      y = vars02[i]) +
    scale_x_continuous(breaks = c(2003, 2006, 2011, 2014, 2017, 2020, 2023),
                       expand = c(0, 0.2)) +
    scale_y_continuous(limits = c(minimum, maximum)) +
    theme(plot.tag = element_text(face = "bold", size = 20))
  
  figure_combined <- figure_all + figure_spain + figure_foreign + 
    figure_class01 + figure_class02 + figure_class03 + 
    figure_class04 + figure_class05 + figure_class06  + 
    plot_layout(ncol = 3, nrow = 3)
  
  namefile<-paste("C:/Users/Alvaro/Documents/Documentos/Artículos/Socioeconomic/Outputs/",vars01[i],".jpg",sep="")
  ggsave(filename=namefile,units="px", width=12000, height=12000, dpi=1200, bg="white")
  par(las=1,cex=1.2,mar=c(6,6,2,0),bty="n",lheight=0.9)
  figure_combined
  dev.off()
  
  diff_all<-guapa((tail(df_all$mean[!is.na(df_all$mean)],1)-head(df_all$mean[!is.na(df_all$mean)],1))/head(df_all$mean[!is.na(df_all$mean)],1)*100)
  diff_spain<-guapa((tail(df_spain$mean[!is.na(df_spain$mean)],1)-head(df_spain$mean[!is.na(df_spain$mean)],1))/head(df_spain$mean[!is.na(df_spain$mean)],1)*100)
  diff_foreign<-guapa((tail(df_foreign$mean[!is.na(df_foreign$mean)],1)-head(df_foreign$mean[!is.na(df_foreign$mean)],1))/head(df_foreign$mean[!is.na(df_foreign$mean)],1)*100)
  diff_class01<-guapa((tail(df_class01$mean[!is.na(df_class01$mean)],1)-head(df_class01$mean[!is.na(df_class01$mean)],1))/head(df_class01$mean[!is.na(df_class01$mean)],1)*100)
  diff_class02<-guapa((tail(df_class02$mean[!is.na(df_class02$mean)],1)-head(df_class02$mean[!is.na(df_class02$mean)],1))/head(df_class02$mean[!is.na(df_class02$mean)],1)*100)
  diff_class03<-guapa((tail(df_class03$mean[!is.na(df_class03$mean)],1)-head(df_class03$mean[!is.na(df_class03$mean)],1))/head(df_class03$mean[!is.na(df_class03$mean)],1)*100)
  diff_class04<-guapa((tail(df_class04$mean[!is.na(df_class04$mean)],1)-head(df_class04$mean[!is.na(df_class04$mean)],1))/head(df_class04$mean[!is.na(df_class04$mean)],1)*100)
  diff_class05<-guapa((tail(df_class05$mean[!is.na(df_class05$mean)],1)-head(df_class05$mean[!is.na(df_class05$mean)],1))/head(df_class05$mean[!is.na(df_class05$mean)],1)*100)
  diff_class06<-guapa((tail(df_class06$mean[!is.na(df_class06$mean)],1)-head(df_class06$mean[!is.na(df_class06$mean)],1))/head(df_class06$mean[!is.na(df_class06$mean)],1)*100)
  tab<-rbind(tab,cbind(diff_all,diff_spain,diff_foreign,diff_class01,diff_class02,diff_class03,
                       diff_class04,diff_class05,diff_class06))
}

rownames(tab)<-vars01
write.table(t(tab),file="C:/Users/Alvaro/Documents/Documentos/Artículos/Socioeconomic/Outputs/percentages.csv",
            sep=";",col.names=TRUE,row.names=TRUE)


