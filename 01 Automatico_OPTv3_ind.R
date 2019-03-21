### Descarga bases
# Cambiar a TRUE si se corre solo
if(FALSE) {
  rm(list=ls())
  ult.anio <- 18
  ult.trim <- 4
  todas.bases <- FALSE
  
  setwd("C:/retrospect/ITLP R/")
  ### RECUERDA ! ACTUALIZA LAS BASES CA e INPC
  
  destino <- "C:/Base de datos/ENOE2"
  dest.dir <- "C:/retrospect/ITLP R/coe2"
  
  library(pacman)
  p_load("data.table", "haven", "foreign",
         "tidyverse", "srvyr", "lubridate",
         "gdata", "grid", "gtable", "gridExtra")
  
  
  if(T){
    n.periodo <- head(paste(rep(c("I", "II", "III", "IV"), (ult.anio - 05)),
                            sort(rep(2005:(2000+ult.anio), 4)), sep = " "),
                      ((ult.anio-4)*4)-(4-ult.trim))
    
    tx <- head(paste("t", rep(1:4, (ult.anio - 05)),
                     str_sub(sort(rep(2005:(2000+ult.anio), 4)), -2), sep = ""),
               ((ult.anio-4)*4)-(4-ult.trim))
    
    n.itlp <-{ c("periodo",
                 "Nacional",
                 "Urbano",
                 "Rural",
                 "Aguascalientes",
                 "Baja California",
                 "Baja California Sur",
                 "Campeche",
                 "Coahuila",
                 "Colima",
                 "Chiapas",
                 "Chihuahua",
                 "Ciudad de México",
                 "Durango",
                 "Guanajuato",
                 "Guerrero",
                 "Hidalgo",
                 "Jalisco",
                 "Estado de México",
                 "Michoacán",
                 "Morelos",
                 "Nayarit",
                 "Nuevo León",
                 "Oaxaca",
                 "Puebla",
                 "Querétaro",
                 "Quintana Roo",
                 "San Luis Potosí",
                 "Sinaloa",
                 "Sonora",
                 "Tabasco",
                 "Tamaulipas",
                 "Tlaxcala",
                 "Veracruz",
                 "Yucatán",
                 "Zacatecas")}
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
  
  df.ca <- read.csv("ca-base.csv", stringsAsFactors = FALSE)
  
  df.ca.u <- colMeans(matrix(df.ca$Urbano, nrow=3))
  df.ca.r <- colMeans(matrix(df.ca$Rural, nrow=3))
  
  df.ca <- data.frame(cbind(df.ca.r, df.ca.u))
  
  df.ca$df.ca.r <- df.ca$df.ca.r / df.ca$df.ca.r[21]
  df.ca$df.ca.u <- df.ca$df.ca.u / df.ca$df.ca.u[21]
  
  df.inpc <- read.csv("inpc-base.csv", stringsAsFactors = FALSE)
  df.inpc <- dplyr::filter(df.inpc, !is.na(inpc))
  v_inpc <- colMeans(matrix(df.inpc$inpc, nrow=3))
}
#### Calculo cambiar

df_ind <- read_dta("base_mun_ind.dta")
df_ind <- dplyr::filter(df_ind, pobi >= 0.4)
mun_ind <- paste(df_ind$ent, df_ind$mun, sep="")
df.inpc <- data.frame(inpc_t = v_inpc, tx)
df.inpc$def <- df.inpc$inpc_t / df.inpc$inpc_t[df.inpc$tx=="t110"]
options(survey.lonely.psu="adjust")

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
                     , .(folioh, foliop, salario, sex, t_loc, fac, clase1, clase2, ent, ingocup, mun, dur_est, rama_est2, est_d, upm)]
  
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
                                       , .(foliop, tamh, ingreso, sex, rururb, factor, ent, mun, mv, ocupado, dur_est, rama_est2, est_d, upm)
                                       ][, .(tamh = sum(tamh),
                                             ingreso = sum(ingreso),
                                             mv = sum(mv), 
                                             sex=sex[1],
                                             ocupado = sum(ocupado),
                                             rururb = rururb[1], 
                                             factor = factor[1], 
                                             ent = ent[1], 
                                             mun = mun[1],
                                             horas = dur_est[1],
                                             rama = rama_est2[1],
                                             est_d = est_d[1],
                                             upm = upm[1]), by=.(foliop)
                                         ][, .(foliop, tamh, ingreso, mv, sex,ocupado, rururb, factor, ent, mun, horas, rama, est_d, upm)
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
  
  df <- mutate(df, factorp = df$factor * df$tamh,
               pob = if_else(rururb == 0,
                             if_else((ingreso / tamh) < get(paste0("u", x, sep="")),1,0),
                             if_else((ingreso / tamh) < get(paste0("r", x, sep="")),1,0)),
               ingpc = ingreso / tamh)
  
  
  #Antes del disenio
  
  sd <- as_survey_design(df, weights = factorp, id=upm, strata = est_d, nest=TRUE)
  df_i_tlp <- sd %>% group_by(indigena) %>%
    summarise(pob_mean = survey_mean(pob, na.rm=TRUE, vartype = "cv"))
  df_i_ingpc <- sd %>% group_by(indigena) %>%
    summarise(ingpc = survey_mean(ingpc, na.rm=TRUE, vartype = "cv"))
  df_s_tlp <- sd %>% group_by(sex) %>%
    summarise(pob_mean = survey_mean(pob, na.rm=TRUE, vartype = "cv"))
  df_s_ingpc <- sd %>% group_by(sex) %>%
    summarise(ingpc = survey_mean(ingpc, na.rm=TRUE, vartype = "cv"))
  df_sex_hrs <- sd %>% group_by(sex, horas) %>%
    summarise(ingpc = survey_mean(ingpc, na.rm=TRUE, vartype = "cv"))
  df_ind_act <- sd %>% group_by(indigena, rama) %>%
    summarise(pob_mean = survey_mean(pob, na.rm=TRUE, vartype = "cv"))
  
  df_r <- data.table(x = 1)
  df_r$TLP_i <- df_i_tlp$pob_mean[df_i_tlp$indigena==T] *100
  df_r$TLP_ni <- df_i_tlp$pob_mean[df_i_tlp$indigena==F] *100
  
  df_r$ingreso_i <- df_i_ingpc$ingpc[df_i_ingpc$indigena==T] 
  df_r$ingreso_ni <- df_i_ingpc$ingpc[df_i_ingpc$indigena==F] 
  
  df_r$TLP_h <- df_s_tlp$pob_mean[as.numeric(df_s_tlp$sex)==1] *100
  df_r$TLP_m <- df_s_tlp$pob_mean[as.numeric(df_s_tlp$sex)==2] *100
  
  df_r$ingreso_h <- df_s_ingpc$ingpc[as.numeric(df_s_ingpc$sex)==1] 
  df_r$ingreso_m <- df_s_ingpc$ingpc[as.numeric(df_s_ingpc$sex)==2] 
  
  df_sex_hrs$id <- paste("S", df_sex_hrs$sex, "H", df_sex_hrs$horas, sep="")
  sex_hrs <- as.data.frame(t(df_sex_hrs$ingpc))
  colnames(sex_hrs) <- df_sex_hrs$id
  
  df_ind_act$id <- paste("I", as.numeric(df_ind_act$indigena), "A", df_ind_act$rama, sep="")
  ind_act <- as.data.frame(t(df_ind_act$pob_mean))
  colnames(ind_act) <- df_ind_act$id
  
  df_r <- bind_cols(df_r, sex_hrs, ind_act)
  
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


### df <- read.dbf("temp/def_ca_i.dbf", as.is = TRUE)
df$def <- df.inpc$def[1:length(df$ingreso_i)]
df <- data.table(df)
df <- df[,c("ingreso_i", "ingreso_ni",
            "ingreso_h", "ingreso_m") := .(ingreso_i/def, ingreso_ni/def,
                                           ingreso_h/def, ingreso_m/def)]

df_indigenas <- df
fwrite(df_indigenas, "temp/cuadro_indigenas.csv")

save(df_indigenas, file="presentaciones/datos_final_i.RData")



