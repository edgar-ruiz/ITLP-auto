### Descarga bases

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
       "gdata", "grid", "gtable", "gridExtra", "httr", "jsonlite", "rmarkdown")


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

#### Calculo cambiar

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

source("01 Automatico_OPTv3_ind.R")

fx.ingreso <- function(x) {
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
  df <- mutate(df, factorp = df$factor * df$tamh,
               pob = if_else(rururb == 0,
                             if_else((ingreso / tamh) < lineas$lpei_u[lineas$periodo== x],1,0),
                             if_else((ingreso / tamh) < lineas$lpei_r[lineas$periodo== x],1,0)),
               ingpc = ingreso / tamh)
                                     
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
  fwrite(df, "temp/temporal.csv")
} else {
  df <- fread("temp/temporal.csv")
  df <- filter(df, periodo != tx[length(tx)])
  f <- fx.ingreso(tx[length(tx)])
  df <- bind_rows(df, f)
  
  fwrite(df, "temp/temporal.csv")
}



### df <- fread("temp/temporal.csv")

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

df3 <- df2
df3$inpc <- head(v_inpc, length(df3$periodo))
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

fwrite(df, "temp/ITLP IS.csv")
fwrite(df2, "temp/IL-Corriente.csv")
fwrite(df3, "temp/IL-INPC.csv")
fwrite(df4, "temp/IL-CA.csv")
fwrite(df7, "temp/TLP.csv")

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
Sys.time() - tiempo
