### Descarga bases

rm(list=ls())
ult.anio <- 18
ult.trim <- 4
todas.bases <- TRUE
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

url2 <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/20"

descargas2 <- function(i, j){
  download.file(paste0(url2, i,"trim",j,"_csv.zip", sep=""),
                paste0(destino, "enoe_15ymas_20", i, j, "_csv.zip"), mode="wb")
  inicio <- paste0(destino, "enoe_15ymas_20", i, sep="")
  unzip(paste0(destino, "enoe_15ymas_20", i, j, "_csv.zip"), exdir = dest.dir)
}

url3 <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/20"

descargas3 <- function(i, j){
  download.file(paste0(url3, i,"trim",j,"_csv.zip", sep=""),
                paste0(destino, "enoe_15ymas_20", i, j, "_csv.zip"), mode="wb")
  inicio <- paste0(destino, "enoe_15ymas_20", i, sep="")
  unzip(paste0(destino, "enoe_15ymas_20", i, j, "_csv.zip"), exdir = dest.dir)
}

if(!todas.bases){
descargas3(str_pad(ult.anio, width = 2, pad = "0"), ult.trim)
}

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

if(todas.bases){
  anios2 <- str_pad(5:18,2,pad="0")
  perm <- 4 - 2
  
  walk2(head(sort(rep(anios2, times=4)), 
             n=length(rep(anios2, times=4))-perm), 
        head(rep(1:4, times=(ult.anio-4)), 
             n=length(rep(1:4, times=(ult.anio-4)))-perm), 
        descargas2)
  
  ### Dos bases se descargan con mayusculas
  anios3 <- str_pad(18:ult.anio,2,pad="0")
  perm3 <- 4 - ult.trim
  
  walk2(head(sort(rep(anios3, times=4)), 
             n=length(rep(anios3, times=4))-perm3)[-c(1,2)], 
        head(rep(1:4, times=(ult.anio-17)), 
             n=length(rep(1:4, times=(ult.anio-17)))-perm3)[-c(1,2)], 
        descargas3)
}
source("01 Automatico_OPTv3_ind.R")
#### Calculo cambiar

df.ca <- read.csv("ca-base.csv", stringsAsFactors = FALSE)

df.ca.u <- colMeans(matrix(df.ca$Urbano, nrow=3))
df.ca.r <- colMeans(matrix(df.ca$Rural, nrow=3))

df.ca <- data.frame(cbind(df.ca.r, df.ca.u))

df.ca$df.ca.r <- df.ca$df.ca.r / df.ca$df.ca.r[21]
df.ca$df.ca.u <- df.ca$df.ca.u / df.ca$df.ca.u[21]

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
                     , .(folioh, foliop, salario, t_loc, fac, clase1, clase2, ent, ingocup, mun)]
  
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
                                       , .(folioh, tamh, ingreso, rururb, factor, ent, mun, mv, ocupado)
                                       ][, .(tamh = sum(tamh),
                                             ingreso = sum(ingreso),
                                             mv = sum(mv), 
                                             ocupado = sum(ocupado),
                                             rururb = rururb[1], 
                                             factor = factor[1], 
                                             ent = ent[1], 
                                             mun = mun[1]), by=.(folioh)
                                         ][, .(folioh, tamh, ingreso, mv, ocupado, rururb, factor, ent, mun)
                                           ][, mv := if_else(!is.na(mv) & mv > 0, 1, 0)][mv != 1]
  
  
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
                                     
  df.ca <- dplyr::filter(df.ca, !is.na(df.ca.r))
  df.ca$periodo <- as.numeric(substr(tx,2,4))
  
  df$ingpcdef <- NA
  num <- as.numeric(substr(x,2,5))
  df$ingpcdef[df$rururb==0] <- df$ingpc[df$rururb==0] / df.ca$df.ca.u[df.ca$periodo==num]
  df$ingpcdef[df$rururb==1] <- df$ingpc[df$rururb==1] / df.ca$df.ca.r[df.ca$periodo==num]
  #Antes del disenio
  sd <- as_survey_design(df, weights = factorp)
  df.rururb <- sd %>% group_by(rururb) %>%
    summarise(pob_mean = survey_mean(pob, na.rm=TRUE))
  df.def_n <- summarise(sd, ingpcdef = survey_mean(ingpcdef, na.rm=TRUE))
  df.def_e <- sd %>% group_by(ent) %>%
    summarise(ingpcdef_mean = survey_mean(ingpcdef, na.rm=TRUE))
  
  df_r <- data.table(x = 1)
  df_r$TLP <- summarise(sd, pob_mean = survey_mean(pob))$pob_mean[1] * 100
  df_r$defN <- df.def_n$ingpcdef[1]
  df_r$TLPu <- df.rururb$pob_mean[1] * 100
  df_r$TLPr <- df.rururb$pob_mean[2] * 100
  
  df.ent <- group_by(sd, ent) %>%
    summarise(pob_mean = survey_mean(pob))
  df.ingpc <- group_by(sd, ent) %>%
    summarise(ingpc_mean = survey_mean(ingpc))
  
  
  for(y in 1:32) {
    nombre <- paste0("TLP", y, sep ="")
    df_r[[nombre]] <- df.ent$pob_mean[y] * 100
    nombre2 <- paste0("ingpcx", y, sep ="")
    df_r[[nombre2]] <- df.ingpc$ingpc_mean[y]
    nombre4 <- paste0("ingpcdef", y, sep ="")
    df_r[[nombre4]] <- df.def_e$ingpcdef_mean[y]
  }
  
  df_r <- dplyr::select(df_r, starts_with("TLP"), starts_with("ingpcx"), starts_with("ingpcdef"))
  df_r$periodo <- x
  df_r$ingpcx0 <- summarise(sd, ingpc_mean = survey_mean(ingpc))$ingpc_mean[1]
  df_r$ingpcdef0 <- summarise(sd, ingpcdef_mean = survey_mean(ingpcdef))$ingpcdef_mean[1]  
  df_r <- dplyr::select(df_r, periodo, starts_with("TLP"), starts_with("ingpcx"), starts_with("ingpcdef"))
  
  df_r$ingpc <- df_r$ingpcx0
  df_r$ingpcdef <- df_r$ingpcdef0
  print(paste("Fin del calculo para el periodo ", x, sep =""))
  return(df_r)
  
}
if(todas.bases){
  f<-map(tx, fx.ingreso)
  df <- f[[1]]
  for (i in 2:length(f)) {
    df <- bind_rows(df, f[[i]])
  }
  write.dbf(df, "temp/def_ca.dbf")
} else {
  df <- read.dbf("temp/def_ca.dbf", as.is = TRUE)
  df <- filter(df, periodo != tx[length(tx)])
  f <- fx.ingreso(tx[length(tx)])
  df <- bind_rows(df, f)
  
  write.dbf(df, "temp/temporal.dbf")
}



### df <- read.dbf("temp/temporal.dbf", as.is = TRUE)

df4 <-dplyr::select(df, periodo, starts_with("ingpcdef")) %>% 
  mutate(periodo = substr(periodo, 2, 5))
df2 <-dplyr::select(df, periodo, starts_with("ingpcx")) %>% 
  mutate(periodo = substr(periodo, 2, 5))
df <-dplyr::select(df, periodo, starts_with("TLP"))
df7 <- df

df <- filter(df, !is.na(TLP)) %>% 
  mutate(periodo = substr(periodo, 2, 5))
df$periodo <- as.numeric(df$periodo)
df2$periodo <- df$periodo

df$base <- mean(df$TLP[df$periodo==110])
df$ITLP <- NA
df$ITLP <- df$TLP / df$base

df$baseu <- mean(df$TLPu[df$periodo==110])
df$ITLPu <- NA
df$ITLPu <- df$TLPu / df$baseu

df$baser <- mean(df$TLPr[df$periodo==110])
df$ITLPr <- NA
df$ITLPr <- df$TLPr / df$baser

for(y in 1:32) {
  nombre <- paste0("base", y, sep ="")
  nombre2 <- paste0("TLP", y, sep ="")
  df[[nombre]] <- NA
  df[[nombre]] <- mean(df[[nombre2]][df$periodo==110])
  nombre3 <- paste0("ITLP", y, sep ="")
  df[[nombre3]] <- NA
  df[[nombre3]] <- df[[nombre2]] / df[[nombre]]
}

df <-dplyr::select(df, periodo, starts_with("ITLP"))


rownames(df) <- n.periodo
rownames(df2) <- n.periodo

names(df) <- n.itlp
n.ingpc <- n.itlp[-c(3:4)]
names(df2) <- c(n.ingpc[-2], "Nacional")

rownames(df) <- n.periodo
rownames(df2) <- n.periodo

#El INPC tiene problemas cuando ponemos un periodo incompleto. (Por ejemplo, si añadimos enero)
df.inpc <- read.csv("inpc-base.csv", stringsAsFactors = FALSE)
df.inpc <- dplyr::filter(df.inpc, !is.na(inpc))
df.inpc <- colMeans(matrix(df.inpc$inpc, nrow=3))

df3 <- df2
df3$inpc <- df.inpc
df3$base <- df3$inpc / df3$inpc[df3$periodo==110]
df3[,2:34] <- df3[,2:34] / df3$base

names(df4) <- c("periodo", n.ingpc[-c(1,2)], "Nacional", "otro" )
df4 <- df4[,-35]
rownames(df4) <- n.periodo

df <- data.frame(sapply(df, FUN=round, digits=4))
df2 <- data.frame(sapply(df2, FUN=round, digits=2))
df3 <- data.frame(sapply(df3, FUN=round, digits=2))
df4 <- data.frame(periodo = df4[,1],sapply(df4[,-1], FUN=round, digits=2))
names(df) <- c("periodo", "Nacional", "Urbano", "Rural", n.ingpc[-c(1,2)])
names(df7) <- c("periodo", "Nacional", "Urbano", "Rural", n.ingpc[-c(1,2)])

write.csv(df, "temp/ITLP IS.csv")
write.csv(df2, "temp/IL-Corriente.csv")
write.csv(df3, "temp/IL-INPC.csv")
write.csv(df4, "temp/IL-CA.csv")
write.csv(df7, "temp/TLP.csv")

ult.trim.l <- if_else(ult.trim == 1, "primer",
                      if_else(ult.trim==2, "segundo",
                              if_else(ult.trim==3, "tercer", "cuarto")))

add_sublabs <- function(plot, sublabs){
  
  gg <- ggplotGrob(plot)
  
  axis_num <- which(gg$layout[,"name"] == "axis-b")
  
  xbreaks <- gg[["grobs"]][[axis_num]][["children"]][[2]][["grobs"]][[2]][["children"]][[1]]$x
  if(length(xbreaks) != length(sublabs)) stop("Sub-labels must be the same length as the x-axis breaks")
  
  to_breaks <- c(as.numeric(xbreaks),1)[which(!duplicated(sublabs, fromLast = TRUE))+1]
  sublabs_x <- diff(c(0,to_breaks))
  sublabs_labels <- sublabs[!duplicated(sublabs, fromLast = TRUE)]
  
  tg <- tableGrob(matrix(sublabs_labels, nrow = 1))
  tg$widths = unit(sublabs_x, attr(xbreaks,"unit"))
  
  pos <- gg$layout[axis_num,c("t","l")]
  
  gg2 <- gtable_add_rows(gg, heights = sum(tg$heights)+unit(4,"mm"), pos = pos$t)
  gg3 <- gtable_add_grob(gg2, tg, t = pos$t+1, l = pos$l)
  
  return(gg3)
}
df.ca$inc_r <- (df.ca$df.ca.r - lag(df.ca$df.ca.r))/lag(df.ca$df.ca.r)
df.ca$inc_u <- (df.ca$df.ca.u - lag(df.ca$df.ca.u))/lag(df.ca$df.ca.u)
df.ca$per <- paste("t",df3$periodo, sep="")
gdata::keep(ult.anio, ult.trim.l, ult.trim, n.periodo, n.ingpc,df,df2,df3,df4,df7, df.ca,add_sublabs, sure=T)
save.image(file="datos_final.RData")


library("rmarkdown")
#rmarkdown::render("presentaciones\\Ing-laboral.Rmd", encoding="UTF-8",
#                  output_options = 
#                    list(pandoc_args = 
#                           c(paste("--metadata=subtitle:\"",
#                                   paste(stringr::str_to_title(ult.trim.l)," trimestre de 20", ult.anio, sep=""),
#                                   "\"", sep=""))))

rmarkdown::render("presentaciones\\Comunicado de prensa.Rmd", encoding="UTF-8")

