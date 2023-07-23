
############## PARTE 1 #############
# 1. Peparamos ambiente de trabajo ####
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
               fastDummies,
               knitr,
               dcolumn)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica


# 2. Abrimos dataset ####
setwd("C:/Users/Alvaro C/Dropbox/1. Escritos/1. Artículos/1 2023_Estallido Social, malestar material y protesta")
#setwd("C:/Users/alvar/Dropbox/1. Escritos/1. Artículos/1 2023_Estallido Social, malestar material y protesta")

db<-read_dta("C:/Users/Alvaro C/Dropbox/Statistic/Data/Datasets/ISSP/ISSP 2014 - Citizenship II - No. 6670/ISSP_2014_CITZENSHIP.dta")
#db<-read_dta("C:/Users/alvar/Dropbox/Statistic/Data/Datasets/ISSP/ISSP 2014 - Citizenship II - No. 6670/ISSP_2014_CITZENSHIP.dta")


# 3. Seleccionamos variables de interés ####

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
           UNION, #Trade Union
           V48, #Political Self-Placement
           ISCO08, #Occupation (ISCO 2008)
           PARTLIV # Living in steady partnership
)


# 4. Resumen de datos ####
#sjlabelled::get_label(db) #Vemos los labels
#summarytools::dfSummary(db, plain.ascii = FALSE)
#view(dfSummary(db, headings=FALSE))


##4.1 Frecuencia de variables (Solo si ex necesario)
frq(db$NEMPLOY) 

# 5. Eliminamos NA ####

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
  UNION = c(8, 9),
  V48 = c(98, 99),
  ISCO08= c(0, 9998, 9999, 0110, 0210, 0310),
  PARTLIV= c(7, 9, 0))

for (var in names(recoding_specs)) {
  if (!is.null(recoding_specs[[var]])) {
    recoding_values <- recoding_specs[[var]]
    db[[var]] <- recode(db[[var]], paste("c(", paste(recoding_values, collapse = ","), ")=NA"))
  }
}



# 6. Resumen de datos ####
#sjlabelled::get_label(db) #Vemos los labels
#summarytools::dfSummary(db, plain.ascii = FALSE)
#view(dfSummary(db, headings=FALSE))



############## PARTE 2 #############




# 1. Construccion de variable####

## 1.1 Construcción de variable Clase Social (Erik Wright)

#Construcción de autoempleado
db$auto_emp <- ifelse(db$EMPREL == 1, 2, 1) #f value 1 (employee) then the value is 2, if not (self-employee), the value is 1.
frq(db$auto_emp) 

#Construcción de credencial ## DE MOMENTO: AQUELLOS QUE SE SALEN QUEDAN COMO NO CREDENCIALIZADOS
db$credencial <- car::recode(db$DEGREE,
                             "0:3=1; 4=2; 5:6=3")

frq(db$credencial)

#Construcción de variable "skills_2digit"
db$skills_2digit <- car::recode(db$ISCO08,
                                "1000=10; 1100:1120=11; 1200:1223=12; 1300:1349=13;
                                1400:1439=14; 2000=20; 2100:2166=21; 2200:2269=22;
                                2300:2359=23; 2400:2434=24; 2500:2529=25; 2600:2659=26;
                                3000=30; 3100:3155=31; 3200:3259=32; 3300:3359=33; 3400:3435=34;
                                3500:3522=35; 4000=40; 4100:4132=41; 4200:4229=42; 4300:4323=43;
                                4400:4419=44; 5000=50; 5100:5169=51; 5200:5249=52; 5300:5329=53;
                                5400:5419=54; 6000=60; 6100:6130=61; 6200:6224=62; 6300:6340=63;
                                7000=70; 7100:7133=71; 7200:7234=72; 7300:7323=73; 7400:7422=74;
                                7500:7549=75; 8000=80; 8100:8189=81; 8200:8219=82; 8300:8350=83;
                                9000=90; 9100:9129=91; 9200:9216=92; 9300:9334=93; 9400:9412=94;
                                9500:9520=95; 9600:9629=96")

frq(db$skills_2digit)


#Construcción de variable Skills:
db$skills <- car::recode(db$skills_2digit,
                         recodes = "10:26=1; c(30,31,32,33,34,35,36,60,61,72)=2;
                                    c(40,41,42,43,44,50,51,52,53,54,62,63,70,71,73,74,75,76,76,77,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96)=3")

frq(db$skills)
#Construcción de variable "skills_2digit"

frq(db$skills_1digit)





#Construcción de num_empleados
db$num_empleados <- car::recode(db$NEMPLOY,
                                "1:9=1; 10:6000=2; 0=3")

frq(db$num_empleados)


#Construcción de participa de unions o 
db$unionized <- car::recode(db$UNION,
                                "1:2=1; 3:4=0")




#Construcción de Clase Social
db$css <- NA

# Condition 1: Burguesía tradicional (Value: 1)
db$css[db$auto_emp == 1 & db$num_empleados == 2 & db$WRKSUP == 1] <- 1

# Condition 2: Pequeño empleador (Value: 2)
db$css[db$auto_emp == 1 & db$num_empleados == 1 & db$WRKSUP == 1] <- 2

# Condition 3: Pequeña burguesía (Value: 3)
db$css[db$auto_emp == 1 & db$num_empleados == 3] <- 3

# Condition 4: Directivo/Supervisor Experto (BA) (Value: 4) - (Skills grupo 10 al 26 (isco) + Correción por credencial)
db$css[db$auto_emp == 2 & db$WRKSUP == 1 & db$skills == 1 & db$credencial == 3 ] <- 4

# Condition 5: Experto (BA+) No directivo (Value: 5)
db$css[db$auto_emp == 2 & db$WRKSUP == 2 & db$skills == 1 & db$credencial == 3] <- 5

# Condition 6: Directivo/Supervisor Semi-Credencializado (Value: 6)
db$css[db$auto_emp == 2 & db$WRKSUP == 1 & db$skills == 2] <- 6

# Condition 7: Obrero semi-credencializado (Value: 7)
db$css[db$auto_emp == 2 & db$WRKSUP == 2 & db$skills == 2] <- 7

# Condition 8: Directivo/Supervisor no credencializado (Value: 8)
db$css[db$auto_emp == 2 & db$WRKSUP == 1 & db$skills == 3] <- 8

# Condition 9: Proletariado tradicional (Value: 9)
db$css[db$auto_emp == 2 & db$WRKSUP == 2 & db$skills == 3] <- 9

frq(db$css)




## 6.2 Construcción de variables de control



#Clase Social subjetiva
db$cs_sub <- car::recode(db$TOPBOT,
                         "1:4=2; 5:6=1; 7:10=0") # 2= WK-Class / 2=Middle Groups / 0=Upper Class

frq(db$cs_sub) 



## Construcción variable working class else = wk_else
db$wk_else <- car::recode(db$css,
                          "1:6=0; 7:9=1") #0 = Not working class / 1= Working class 

## Construcción variable: correctly class identity (correct_ci)
db$correct_ci <- NA

# Con comando ifelse -> 1 = correct_class_identity / 0 = incorrect_class_identity
db$correct_ci<- ifelse(db$wk_else == 1 & db$cs_sub == 2, 1,
                       ifelse(db$wk_else == 1 & db$cs_sub %in% c(0, 1), 0, NA))


#Posición Política
db$pos_pol <- car::recode(db$V48,
                          "0:4=1; 5=2; 6:10=3")

frq(db$pos_pol) 



# Situación marital
db$partner <- car::recode(db$PARTLIV,
                          "1:2=1; 3=0")



# Construcción de variable acción colectiva (sumatoria)
#Loop that recodes, 1:2 = 1 (Yes), 3:4 = 2 (No)
variables <- c("V17", "V18", "V19", "V22")  # List of variables to recode

for (var in variables) {
  db[[var]] <- ifelse(db[[var]] %in% c(1, 2), 1, ifelse(db[[var]] %in% c(3, 4), 2, db[[var]]))
}
#Loop para reemplazar 2 por 0 y que 1 sea participar
replace_values <- function(data, vars) {
  for (var in vars) {
    data[[var]] <- ifelse(data[[var]] == 2, 0, data[[var]])
  }
  return(data)
}
variables <- variables <- c("V17", "V18", "V19", "V22") 
db <- replace_values(db, variables)


#Segundo: Sumatoria
# Create a new variable named 'new_variable' as the summation of other variables
db$acc <- rowSums(db[, c("V17", "V18", "V19", "V22")])


#Construcción de acc2
db$acc2 <- car::recode(db$acc,
                       "0=0; 1:16=1")

frq(db$acc2)



############## PARTE 3 #############
db$css <- set_labels(db$css,
                     labels=c( "Burguesía tradicional"=1,
                               "Pequeño empleador"=2,
                               "Pequeña Burguesía"=3,
                               "Directivo/Supervisor Experto"=4,
                               "Experto no directivo"=5,
                               "Directivo/Supervisor semi-credencializado"=6,
                               "Obrero semi-credencializado"=7,
                               "Directivo/Supervisor no credencializado"=8,
                               "Proletario tradicional"=9)) #Clase Social

db$cs_sub <- set_labels(db$cs_sub,
                        labels=c("Clase Subjetiva baja"=2, "Clase Subjetiva Media"=3,
                                 "Clase Subjetiva Alta"=0)) #Clase Social Subjetiva


db$pos_pol <- set_labels(db$pos_pol,
                         labels=c("Izquierda"=1, "Centro"=2,
                                  "Derecha"=3)) #Posición Política


db$correct_ci <- set_labels(db$correct_ci,
                         labels=c("Corresponding Class Identity"=1,
                                  "Not corresponding Class Identity" = 0)) #Correct Class identity

db$unionized <- set_labels(db$unionized,
                            labels=c("Have participated in unions"=1,
                                     "Haven't participated in unions"=0)) #Unionizedorrect Class identityC

db$partner <- set_labels(db$partner,
                           labels=c("In partnership"=1,
                                    "Not in partnership"=0)) #Partnership




#Seleccionamos otra bdd
db2<-select(db,
            SEX,
            AGE,
            acc,
            pos_pol,
            css,
            cs_sub)



# Export Data base

# Set the folder path
folder_path <- "C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/base"

# Create the folder if it doesn't exist
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

# Replace 'your_data' with the name of your database object
save(db, file = paste0(folder_path, "/db.RData"))


# Load the data from the RData file
#load("C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/base/db.RData")




############## PARTE 4 #############


#### Summary descriptive tables ####

## 1. Descriptives
plot_frq(db2$css)
  
########### 3.1 Multivariate Analysis #############


#### Análisis exploratorio y conversción en factor
tab_xtab(var.row = db$css,db$acc,show.cell.prc = T,show.summary = F) #Tabla de contingencia de variable de interés

#Pasamos a factor para regresión
db$css<-as_factor(db$css) #Clase Social Marx
db$sex<-as_factor(db$SEX) #Sexo
db$cs_sub2<-as_factor(db$cs_sub) #Clase Social Subj.
db$pos_pol2<-as_factor(db$pos_pol) #Posición política



##### MODELOS ######

#Modelo 1: Variable dependiente Acción Colectiva  / #Independiente = css

# Modelo A
reg1<-lm(acc ~ css, data = db)
sjPlot::tab_model(list(reg1), show.ci=FALSE, show.se = TRUE,
                  show.aic = TRUE, show.r2 = TRUE, collapse.se = TRUE,
                  title = "Predictores de XXX",
                  p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1"))

# Modelo A.2
reg2<-lm(acc ~ css+cs_sub2+sex, data = db)
sjPlot::tab_model(list(reg1, reg2), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2"))

# Modelo A.3
reg3<-lm(acc ~ css+cs_sub2+AGE, data = db)
sjPlot::tab_model(list(reg1, reg2, reg3), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"))
# Modelo A.4
reg4<-lm(acc ~ css+AGE+partner, data = db)
sjPlot::tab_model(list(reg1, reg2, reg3, reg4), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"))
# Modelo A.5
reg5<-lm(acc ~ css+cs_sub2+AGE+sex+partner, data = db)
sjPlot::tab_model(list(reg1, reg2, reg3, reg4, reg5), show.ci=FALSE, show.se = TRUE,
                  show.aic = TRUE, show.r2 = TRUE, collapse.se = TRUE,
                  show.reflvl = TRUE,
                  title = "Predictores de XXX",
                  p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"))


####

# Modelo B.1
mod1<-lm(acc ~ correct_ci, data = db)
sjPlot::tab_model(list(mod1), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1"))

# Modelo B.2
mod2<-lm(acc ~ correct_ci+unionized, data = db)
sjPlot::tab_model(list(mod1, mod2), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2"))

# Modelo B.3
mod3<-lm(acc ~ correct_ci+unionized+pos_pol2+AGE, data = db)
sjPlot::tab_model(list(mod1, mod2, mod3), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"))

# Modelo B.4
mod4<-lm(acc ~ correct_ci+unionized+pos_pol2+sex, data = db)
sjPlot::tab_model(list(mod1, mod2, mod3, mod4), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"))

# Modelo B.5
mod5<-lm(acc ~ correct_ci+unionized+pos_pol2+AGE+sex, data = db)
sjPlot::tab_model(list(mod1, mod2, mod3, mod4, mod5), show.ci=FALSE, show.se = TRUE,
                  show.aic = TRUE, show.r2 = TRUE, collapse.se = TRUE,
                  title = "Predictores de XXX",
                  p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"))



#. Visualización


# Step 4: Create the stargazer table
stargazer(reg1, reg2, title = "Regression Results", align = TRUE, type = "html")

# Alternatively, you can create the table and save it as a LaTeX or HTML table for further use.
# For LaTeX:
# stargazer(model1, model2, title = "Regression Results", align = TRUE, out = "regression_table.tex")

# For HTML:
stargazer(reg1, reg2, reg3, reg4, title = "Regression Results", align = TRUE)

stargazer(reg1 = "Regression Results", align = TRUE, type = "text")

stargazer(reg1, type = "html")
