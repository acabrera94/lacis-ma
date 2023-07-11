

####  1. Peparamos ambiente de trabajo ####
library(pacman)
pacman::p_load(tidyverse,
               haven,
               magrittr,
               sjPlot,
               sjmisc,
               sjlabelled,
               stargazer,
               summarytools,
               expss,
               car,
               foreign,
               coefplot,
               fastDummies)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica


####  2. Abrimos dataset ####
setwd("C:/Users/Alvaro C/Dropbox/1. Escritos/1. Artículos/1 2023_Estallido Social, malestar material y protesta")
#setwd("C:/Users/alvar/Dropbox/1. Escritos/1. Artículos/1 2023_Estallido Social, malestar material y protesta")

db<-read_dta("C:/Users/Alvaro C/Dropbox/Statistic/Data/Datasets/ISSP/ISSP 2014 - Citizenship II - No. 6670/ISSP_2014_CITZENSHIP.dta")
#db<-read_dta("C:/Users/alvar/Dropbox/Statistic/Data/Datasets/ISSP/ISSP 2014 - Citizenship II - No. 6670/ISSP_2014_CITZENSHIP.dta")


####  3. Seleccionamos variables de interés ####

#1 Recordar: find_var(data = YYY,"XXX") para buscar variables

db<-select(db,
           SEX,
           AGE,
           DEGREE,
           EMPREL, #employee, self-emplyed or working for your own's family
           NEMPLOY, #Self-employed, how many employe
           WRKSUP, #Supervise?
           TOPBOT, #Subjective Social Class
           V17, #Political: Sign a petition
           V18, #Collective Action: Boycot certain products
           V19, #Collective: Take part in demonstration
           V20, #Collective: Attend political meeting
           V21, #Political: Contact a politician
           V22, #Political: Donate money or raise funds
           V23, #Political: Contact media
           V24, #Political: Express views on the internet
           V25, #Political: Using media to get political news
           V27, #Member: Political Party
           V28, #Trade Union
           V48, #Political Self-Placement
           ISCO08 #Occupation (ISCO 2008)
)


####  4. Resumen de datos ####
sjlabelled::get_label(db) #Vemos los labels
summarytools::dfSummary(db, plain.ascii = FALSE)
view(dfSummary(db, headings=FALSE))


####  4.1 Frecuencia de variables (Solo si ex necesario)
frq(db$NEMPLOY) 

####  5. Eliminamos NA ####

recoding_specs <- list(
  SEX = 9,
  AGE = 999,
  DEGREE = 9,
  EMPREL = c(0, 9),
  NEMPLOY = c(9995, 9998, 9999),
  WRKSUP = c(0, 8, 9),
  TOPBOT = c(0,98, 99),
  V17 = c(8, 9),
  V18 = c(8, 9),
  V19 = c(8, 9),
  V20 = c(8, 9),
  V21 = c(8, 9),
  V22 = c(8, 9),
  V23 = c(8, 9),
  V24 = c(8, 9),
  V25 = c(8, 9),
  V27 = c(8, 9),
  V28 = c(8, 9),
  V48 = c(98, 99),
  ISCO08= c(0, 9998, 9999, 0110, 0210, 0310))

for (var in names(recoding_specs)) {
  if (!is.null(recoding_specs[[var]])) {
    recoding_values <- recoding_specs[[var]]
    db[[var]] <- recode(db[[var]], paste("c(", paste(recoding_values, collapse = ","), ")=NA"))
  }
}



####  4. Resumen de datos ####
sjlabelled::get_label(db) #Vemos los labels
summarytools::dfSummary(db, plain.ascii = FALSE)
view(dfSummary(db, headings=FALSE))









