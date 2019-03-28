### Descarga bases
# Cambiar a TRUE si se corre solo
if(FALSE) {
  rm(list=ls())
  tiempo <- Sys.time()
  ult.anio <- 18
  ult.trim <- 4
  todas.bases <- TRUE
  setwd("D:/Retrospect/ITLP R/")
  ### RECUERDA ! ACTUALIZA LAS BASES CA e INPC
  
  destino <- "D:/Retrospect/EN C/Base de datos/ENOE2"
  dest.dir <- "D:/Retrospect/ITLP R/coe2"
  
  library(pacman)
  p_load("data.table", "haven", "foreign",
         "tidyverse", "srvyr", "lubridate",
         "gdata", "grid", "gtable", "gridExtra", "httr", "jsonlite")
  
  
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
  }
  
  
  ultim.mes <- paste(case_when(ult.trim==1 ~ "mar", ult.trim==2 ~ "jun" , ult.trim==3 ~ "sep", ult.trim==4 ~ "dic"), 
                     paste("20", ult.anio, sep = ""), sep = "")
  
  url <- paste("https://www.coneval.org.mx/Informes/Pobreza/Datos_abiertos/lineas_de_pobreza_por_ingresos/lineas_pobreza_ingresos_ene1992_",
               ultim.mes,".csv", sep = "")
  
  download.file(url, destfile = "lineas.csv")
  
  lineas <- read.csv("lineas.csv", stringsAsFactors = FALSE)
  
  lineas <- data.table(lineas)
  lineas <- dcast(lineas, anio + mes ~ desagregacion, value.var=c("lpei", "lpi"))
  lineas <- data.frame(lineas)
  
  lineas <- mutate(lineas, mes =case_when(mes=="Ene" ~ 1 ,mes=="Feb" ~ 2, mes=="Mar" ~ 3, mes=="Abr" ~ 4, mes=="May" ~ 5, mes=="Jun" ~ 6,
                                          mes=="Jul" ~ 7, mes=="Ago" ~ 8, mes=="Sep" ~ 9, mes=="Oct" ~ 10, mes=="Nov" ~ 11, mes=="Dic" ~ 12))
  lineas <- arrange(lineas, anio, mes)
  lineas <- filter(lineas, anio>=2005)
  
  lineas <- mutate(lineas, trim = case_when(mes==1 |mes==2 | mes==3 ~ 1, mes==4 |mes==5 | mes==6 ~ 2, 
                                            mes==7 |mes==8 | mes==9 ~ 3, mes==10 |mes==11 | mes==12 ~ 4))
  
  lineas <- lineas %>% group_by(anio, trim) %>%
    summarise(lpei_r = mean(lpei_Rural), lpei_u = mean(lpei_Urbano))
  
  lineas[,3:4] <- round(lineas[,3:4], digits = 2)
  
  lineas <- mutate(lineas, periodo= paste("t",trim, str_sub(anio, -2,-1), sep=""))
  
  df.ca <- dplyr::select(lineas, anio, lpei_r , lpei_u )
  names(df.ca) <- c("periodo","Rural","Urbano")
  
  df.ca.u <- df.ca$Urbano
  df.ca.r <- df.ca$Rural
  
  df.ca <- data.frame(cbind(df.ca.r, df.ca.u))
  
  df.ca$df.ca.r <- df.ca$df.ca.r / df.ca$df.ca.r[21]
  df.ca$df.ca.u <- df.ca$df.ca.u / df.ca$df.ca.u[21]
  df.ca <- dplyr::filter(df.ca, !is.na(df.ca.r))
  df.ca$periodo <- as.numeric(substr(tx,2,4))
  
  url <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1/Indicador/"
  url1 <- "/01/es/false/json/"
  auth <- "9e0b148e-428f-0ffa-4508-6cf5f04c5854"
  
  inpc_gen <- 583766
  
  series <- function(i){
    
    raw <- httr::GET(paste0(url,i, url1,auth, sep=""))
    txt.c <- content(raw, as = "text", encoding = "UTF-8")  %>% fromJSON
    print(paste0("Terminó la serie ",i, sep = " "))
    txt.c$Data$Serie 
    df <- data.frame(serie=i, txt.c$Data$Serie$TimePeriod , txt.c$Data$Serie$CurrentValue)
  }
  
  df.inpc <- map(inpc_gen,series)
  
  df.inpc <- tibble(df.inpc) %>% unnest(df.inpc) 
  colnames(df.inpc) <- c("serie", "periodo", "inpc")
  df.inpc$periodo <-  as.character(df.inpc$periodo)
  df.inpc$inpc    <- as.numeric(as.character(df.inpc$inpc))
  df.inpc <- mutate(df.inpc, anio = str_sub(periodo, 1, 4), mes = str_sub(periodo, -2, -1))
  df.inpc <- dplyr::filter(df.inpc, anio>=2005)
  
  df.inpc<- mutate(df.inpc, trim = case_when(mes=="01" |mes=="02" | mes=="03" ~ 1, mes=="04" |mes=="05" | mes=="06" ~ 2, 
                                             mes=="07" |mes=="08" | mes=="09" ~ 3, mes=="10" |mes=="11" | mes=="12" ~ 4))
  df.inpc <- dplyr::select(df.inpc, anio, trim, inpc)
  
  df.inpc <- df.inpc %>% group_by(anio, trim) %>%
    dplyr::summarise(inpc_trim = mean(inpc))
  
  v_inpc <- df.inpc$inpc_trim
}
#### Calculo cambiar

df_ind <- read_dta("base_mun_ind.dta")
df_ind <- dplyr::filter(df_ind, pobi >= 0.4)
mun_ind <- paste(df_ind$ent, df_ind$mun, sep="")
df.inpc <- data.frame(inpc_t = v_inpc, tx)
df.inpc$def <- df.inpc$inpc_t / df.inpc$inpc_t[df.inpc$tx=="t110"]
#options(survey.lonely.psu="adjust")

fx.ingreso <- function(x) {
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
  write.dbf(df, "temp/def_ca_i.dbf")
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