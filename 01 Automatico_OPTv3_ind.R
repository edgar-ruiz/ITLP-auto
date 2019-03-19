### Descarga bases

rm(list=ls())
ult.anio <- 18
ult.trim <- 4
todas.bases <- FALSE

setwd("C:/retrospect/ITLP R/")
### RECUERDA ! ACTUALIZA LAS BASES CA e INPC

destino <- "C:/Base de datos/ENOE2"
dest.dir <- "C:/retrospect/ITLP R/coe2"
dest.dir2 <- "C:/retrospect/ITLP R/sdem"

library(stringr)
library(data.table)
library(purrr)
library(haven)
library(foreign)
library(tidyverse)
library(dplyr)
library(srvyr)
library(doBy)
library(lubridate)
library(gdata)
library(grid)
library(gtable)
library(gridExtra)


if(T){
  n.periodo <- {c("I 2005",
                  "II 2005",
                  "III 2005",
                  "IV 2005",
                  "I 2006",
                  "II 2006",
                  "III 2006",
                  "IV 2006",
                  "I 2007",
                  "II 2007",
                  "III 2007",
                  "IV 2007",
                  "I 2008",
                  "II 2008",
                  "III 2008",
                  "IV 2008",
                  "I 2009",
                  "II 2009",
                  "III 2009",
                  "IV 2009",
                  "I 2010" ,
                  "II 2010" ,
                  "III 2010" ,
                  "IV 2010",
                  "I 2011",
                  "II 2011",
                  "III 2011",
                  "IV 2011",
                  "I 2012",
                  "II 2012",
                  "III 2012",
                  "IV 2012",
                  "I 2013",
                  "II 2013",
                  "III 2013",
                  "IV 2013",
                  "I  2014" ,
                  "II 2014",
                  "III 2014",
                  "IV 2014" ,
                  "I 2015",
                  "II 2015",
                  "III 2015",
                  "IV 2015",
                  "I 2016",
                  "II 2016",
                  "III 2016",
                  "IV 2016",
                  "I 2017",
                  "II 2017",
                  "III 2017",
                  "IV 2017",
                  "I 2018",
                  "II 2018",
                  "III 2018","IV 2018"
  )}
  tx <- {c("t105","t205","t305","t405",
           "t106","t206","t306","t406",
           "t107","t207","t307","t407",
           "t108","t208","t308","t408",
           "t109","t209","t309","t409",
           "t110","t210","t310","t410",
           "t111","t211","t311","t411",
           "t112","t212","t312","t412",
           "t113","t213","t313","t413",
           "t114","t214","t314","t414",
           "t115","t215","t315","t415",
           "t116","t216","t316","t416",
           "t117","t217","t317","t417",
           "t118","t218","t318", "t418"
  )}
  n.itlp <-{ c("periodo",
               "Nacional",
               "Urbano",
               "Rural",
               "Aguascalientes",
               "Baja_California",
               "Baja_California_Sur",
               "Campeche",
               "Coahuila",
               "Colima",
               "Chiapas",
               "Chihuahua",
               "Ciudad_de_Mexico",
               "Durango",
               "Guanajuato",
               "Guerrero",
               "Hidalgo",
               "Jalisco",
               "Estado_de_Mexico",
               "Michoacan",
               "Morelos",
               "Nayarit",
               "Nuevo_Leon",
               "Oaxaca",
               "Puebla",
               "Queritaro",
               "Quintana_Roo",
               "San_Luis_Potosi",
               "Sinaloa",
               "Sonora",
               "Tabasco",
               "Tamaulipas",
               "Tlaxcala",
               "Veracruz",
               "Yucatan",
               "Zacatecas"
  )}
  rt105=492.78
  rt205=520.05
  rt305=516.84
  rt405=510.03
  rt106=526.53
  rt206=518.1
  rt306=533.35
  rt406=565.69
  rt107=576.48
  rt207=563.7
  rt307=567.7
  rt407=582.76
  rt108=585.91
  rt208=601.28
  rt308=614.67
  rt408=640.53
  rt109=649.04
  rt209=672.97
  rt309=686.43
  rt409=686.09
  rt110=705.17
  rt210=695.86
  rt310=684.99
  rt410=705.71
  rt111=717.4
  rt211=717.83
  rt311=716.93
  rt411=740.43
  rt112=765.39
  rt212=770.93
  rt312=805.72
  rt412=820.38
  rt113=828.61
  rt213=837.18
  rt313=833.37
  rt413=853.72
  rt114=870.81
  rt214=854.08
  rt314=869.82
  rt414=899.27
  rt115=896.17
  rt215=901.34
  rt315=911.38
  rt415=926.32
  rt116=959.7
  rt216=947.13
  rt316=942.81
  rt416=970.61
  rt117=975.82
  rt217=1003.88
  rt317=1053.26
  rt417=1054.84
  rt118=1052.56
  rt218=1046.25
  rt318=1068.21
  ut105=712.54
  ut205=741.49
  ut305=740.37
  ut405=736.48
  ut106=754.24
  ut206=748.55
  ut306=764.24
  ut406=797.76
  ut107=814.7
  ut207=803.92
  ut307=810.79
  ut407=830.52
  ut108=839.18
  ut208=858.24
  ut308=875.52
  ut408=906.75
  ut109=922
  ut209=949.84
  ut309=967.6
  ut409=968.9
  ut110=992.68
  ut210=987.38
  ut310=979.48
  ut410=1003.59
  ut111=1021.5
  ut211=1022.29
  ut311=1023.17
  ut411=1049.22
  ut112=1079.48
  ut212=1089.02
  ut312=1130.09
  ut412=1151.75
  ut113=1166.22
  ut213=1177.4
  ut313=1177.99
  ut413=1201.99
  ut114=1234.8
  ut214=1223.42
  ut314=1243.83
  ut414=1276.56
  ut115=1264.53
  ut215=1268.44
  ut315=1282.51
  ut415=1302.59
  ut116=1338.64
  ut216=1329.36
  ut316=1323.88
  ut416=1357.24
  ut117=1376.88
  ut217=1410.21
  ut317=1469.65
  ut417=1479.05
  ut118=1482.13
  ut218=1477.31
  ut318=1510.45
  ut418=1533.46
  rt418=1091.92
}

#### Calculo cambiar

df_ind <- read_dta("base_mun_ind.dta")
df_ind <- dplyr::filter(df_ind, pobi >= 0.4)
mun_ind <- paste(df_ind$ent, df_ind$mun, sep="")

df.inpc <- read.csv("inpc-base.csv", stringsAsFactors = FALSE)
df.inpc <- dplyr::filter(df.inpc, !is.na(inpc))
df.inpc <- colMeans(matrix(df.inpc$inpc, nrow=3))

df.inpc <- data.table(inpc_t = df.inpc, tx)
df.inpc$def <- df.inpc$inpc_t / df.inpc$inpc_t[df.inpc$tx=="t110"]

fx.ingreso <- function(x) {
  nombre <- paste0("lp", x, sep ="")
  num <- as.numeric(substr(x,2,5))
  df <- fread(paste0("coe2/coe2", x, ".csv", sep=""))
  colnames(df) <- tolower(colnames(df))
  df <- df[, c("cd_a", "ent", "con", "v_sel", "n_ren") :=
             .(str_pad(cd_a, 2, "left", "0"),
               str_pad(ent, 2, "left", "0"),
               str_pad(con, 5, "left", "0"),
               str_pad(v_sel, 2, "left", "0"),
               str_pad(n_ren, 2, "left", "0"))]
  df <- df[,foliop := paste0(cd_a, ent, con, v_sel,n_hog, h_mud, n_ren, sep="")
           ][, .(foliop, p6c, p6b2, p6_9, p6a3)]
  
  df2 <- fread(paste0("coe2/sdem", x, ".csv", sep=""))
  colnames(df2) <- tolower(colnames(df2))
  df2 <- df2[, c("cd_a", "ent", "con", "v_sel", "n_ren") :=
             .(str_pad(cd_a, 2, "left", "0"),
               str_pad(ent, 2, "left", "0"),
               str_pad(con, 5, "left", "0"),
               str_pad(v_sel, 2, "left", "0"),
               str_pad(n_ren, 2, "left", "0"))]
  
  df2 <- df2[r_def==0 & (c_res==1 | c_res==3)
             ][,
               c("folioh", 
                 "foliop") := 
                 .(paste0(cd_a, ent, con, v_sel, n_hog, h_mud, sep=""),
                   paste0(cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, sep=""))][
                     , .(folioh, foliop, salario, sex, t_loc, fac, clase1, clase2, ent, ingocup, mun)]
  
  df <- df[df2, on = .(foliop)
           ][, c("ocupado","p6b2","p6c") := 
               .(ifelse(clase1 == 1 & clase2 == 1, 1, 0),
                 as.numeric(p6b2),
                 as.numeric(p6c))
             ][p6b2==999998 | p6b2==999999,  p6b2 := NA
               ][, ingreso := if_else(ocupado == 0 | (is.na(p6b2) & (p6_9==9 | p6a3==3)), 0, p6b2)
                 ][is.na(p6b2) & (p6c==1), ingreso := salario * 0.5
                   ][is.na(p6b2) & p6c==2, ingreso := salario * 1
                     ][is.na(p6b2) & p6c==3, ingreso := salario * 1.5
                       ][is.na(p6b2) & p6c==4, ingreso := salario * 2.5
                         ][is.na(p6b2) & p6c==5, ingreso := salario * 4
                           ][is.na(p6b2) & p6c==6, ingreso := salario * 7.5
                             ][is.na(p6b2) & p6c==7, ingreso := salario * 10
                               ][,
                                 c("factor", "tamh", "rururb", "ent", "mv") :=
                                   .(fac,
                                     1,
                                     if_else((as.numeric(t_loc)>=1 & as.numeric(t_loc)<=3), 0, 1),
                                     as.numeric(ent),
                                     if_else(is.na(ingreso) & ocupado == 1, 1, 0))][
                                       , .(foliop, tamh, ingreso, sex, rururb, factor, ent, mun, mv, ocupado)
                                       ][, .(tamh = sum(tamh),
                                             ingreso = sum(ingreso),
                                             mv = sum(mv), 
                                             sex=sex[1],
                                             ocupado = sum(ocupado),
                                             rururb = rururb[1], 
                                             factor = factor[1], 
                                             ent = ent[1], 
                                             mun = mun[1]), by=.(foliop)
                                         ][, .(foliop, tamh, ingreso, mv, sex,ocupado, rururb, factor, ent, mun)
                                           ][, mv := if_else(!is.na(mv) & mv > 0, 1, 0)
                                             ][mv != 1
                                               ][, mun_i := str_c(str_pad(ent, 2, "left", "0"),str_pad(mun, 3, "left", "0"), sep="")
                                                 ][,indigena := mun_i %in% mun_ind][ocupado==1]
  
  
  ####################################################
  #
  # Parte III COMPARACISN DEL INGRESO DEL HOGAR CON 
  # EL PROMEDIO DE LA LINEA DE BIENESTAR MMNIMO :
  # 
  ####################################################  
  nombre <- paste0("lp", x, sep ="")
  df <- mutate(df, factorp = df$factor * df$tamh,
               pob = if_else(rururb == 0,
                             if_else((ingreso / tamh) < get(paste0("u", x, sep="")),1,0),
                             if_else((ingreso / tamh) < get(paste0("r", x, sep="")),1,0)),
               ingpc = ingreso / tamh)
                                     
  df$ingpcdef <- NA
  num <- as.numeric(substr(x,2,5))

  #Antes del disenio
  sd <- as_survey_design(df, weights = factorp)
  df_i_tlp <- sd %>% group_by(indigena) %>%
    summarise(pob_mean = survey_mean(pob, na.rm=TRUE))
  df_i_ingpcdf <- sd %>% group_by(indigena) %>%
    summarise(ingpcdef = survey_mean(ingpc, na.rm=TRUE))
  
  df_s_tlp <- sd %>% group_by(sex) %>%
    summarise(pob_mean = survey_mean(pob, na.rm=TRUE))
  df_s_ingpcdf <- sd %>% group_by(sex) %>%
    summarise(ingpcdef = survey_mean(ingpc, na.rm=TRUE))
  
  
  df_r <- data.table(x = 1)
  df_r$TLP_i <- df_i_tlp$pob_mean[df_i_tlp$indigena==T] *100
  df_r$TLP_ni <- df_i_tlp$pob_mean[df_i_tlp$indigena==F] *100
  
  df_r$ingreso_i <- df_i_ingpcdf$ingpcdef[df_i_ingpcdf$indigena==T] 
  df_r$ingreso_ni <- df_i_ingpcdf$ingpcdef[df_i_ingpcdf$indigena==F] 
  
  df_r$TLP_h <- df_s_tlp$pob_mean[as.numeric(df_s_tlp$sex)==1] *100
  df_r$TLP_m <- df_s_tlp$pob_mean[as.numeric(df_s_tlp$sex)==2] *100
  
  df_r$ingreso_h <- df_s_ingpcdf$ingpcdef[as.numeric(df_s_ingpcdf$sex)==1] 
  df_r$ingreso_m <- df_s_ingpcdf$ingpcdef[as.numeric(df_s_ingpcdf$sex)==2] 
  
 
  print(paste("Fin del calculo para el periodo ", x, sep =""))
  return(df_r)
  
}
if(todas.bases){
  f<-map(tx, fx.ingreso)
  df <- f[[1]]
  for (i in 2:length(f)) {
    df <- bind_rows(df, f[[i]])
  }
  df$x <- tx
  write.dbf(df, "temp/def_ca_i.dbf")
} else {
  df <- read.dbf("temp/def_ca_i.dbf", as.is = TRUE)
  df <- df[1:(length(tx))-1,]
  f <- fx.ingreso(tx[length(tx)])
  f$x <- as.character(f$x)
  df <- bind_rows(df, f)
  df$x <- tx
  write.dbf(df, "temp/temporal_i.dbf")
}



### df <- read.dbf("temp/temporal_i.dbf", as.is = TRUE)
df$def <- df.inpc$def[1:length(df$ingreso_i)]
df <- data.table(df)
df <- df[,c("ingreso_i", "ingreso_ni",
            "ingreso_h", "ingreso_m") := .(ingreso_i/def, ingreso_ni/def,
                                           ingreso_h/def, ingreso_m/def)]

df_indigenas <- df
fwrite(df_indigenas, "temp/cuadro_indigenas.csv")

save(df_indigenas,file="presentaciones/datos_final_i.RData")
