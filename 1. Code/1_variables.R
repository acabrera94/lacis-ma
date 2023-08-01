
########### Parte 1: Variable Construction #########


# Preamble

library(pacman)
pacman::p_load(car,
               sjlabelled,
               dplyr)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

db1<-readRDS("C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/3. Dataframes/0_preamble.RDS")



# 1. Preámbulo Clase Social ####

#Construcción de autoempleado
db1$auto_emp <- ifelse(db1$EMPREL == 1, 2, 1) #f value 1 (employee) then the value is 2, if not (self-employee), the value is 1.
frq(db1$auto_emp) 

#Construcción de credencial ## DE MOMENTO: AQUELLOS QUE SE SALEN QUEDAN COMO NO CREDENCIALIZADOS
db1$credencial <- car::recode(db1$DEGREE,
                             "0:3=1; 4=2; 5:6=3")

frq(db1$credencial)

#Construcción de variable "skills_2digit"
db1$skills_2digit <- car::recode(db1$ISCO08,
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

frq(db1$skills_2digit)


#Construcción de variable Skills:
db1$skills <- car::recode(db1$skills_2digit,
                         recodes = "10:26=1; c(30,31,32,33,34,35,36,60,61,72)=2;
                                    c(40,41,42,43,44,50,51,52,53,54,62,63,70,71,73,74,75,76,76,77,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96)=3")

frq(db1$skills)
#Construcción de variable "skills_2digit"


#Construcción de num_empleados
db1$num_empleados <- car::recode(db1$NEMPLOY,
                                "1:9=1; 10:6000=2; 0=3")

frq(db1$num_empleados)


#Construcción de participa de unions o sindocatos
db1$unionized <- car::recode(db1$UNION,
                            "1:2=1; 3:4=0")
#Labels
db1$unionized <- set_labels(db1$unionized,
                           labels=c("Have participated in unions"= 1,
                                    "Haven't participated in unions"= 0)) #Unions 



# 2. Construcción Clase Social (E. Wright) ####
db1$css <- NA

# Condition 1: Burguesía tradicional (Value: 1)
db1$css[db1$auto_emp == 1 & db1$num_empleados == 2 & db1$WRKSUP == 1] <- 1

# Condition 2: Pequeño empleador (Value: 2)
db1$css[db1$auto_emp == 1 & db1$num_empleados == 1 & db1$WRKSUP == 1] <- 2

# Condition 3: Pequeña burguesía (Value: 3)
db1$css[db1$auto_emp == 1 & db1$num_empleados == 3] <- 3

# Condition 4: Directivo/Supervisor Experto (BA) (Value: 4) - (Skills grupo 10 al 26 (isco) + Correción por credencial)
db1$css[db1$auto_emp == 2 & db1$WRKSUP == 1 & db1$skills == 1 & db1$credencial == 3 ] <- 4

# Condition 5: Experto (BA+) No directivo (Value: 5)
db1$css[db1$auto_emp == 2 & db1$WRKSUP == 2 & db1$skills == 1 & db1$credencial == 3] <- 5

# Condition 6: Directivo/Supervisor Semi-Credencializado (Value: 6)
db1$css[db1$auto_emp == 2 & db1$WRKSUP == 1 & db1$skills == 2] <- 6

# Condition 7: Obrero semi-credencializado (Value: 7)
db1$css[db1$auto_emp == 2 & db1$WRKSUP == 2 & db1$skills == 2] <- 7

# Condition 8: Directivo/Supervisor no credencializado (Value: 8)
db1$css[db1$auto_emp == 2 & db1$WRKSUP == 1 & db1$skills == 3] <- 8

# Condition 9: Proletariado tradicional (Value: 9)
db1$css[db1$auto_emp == 2 & db1$WRKSUP == 2 & db1$skills == 3] <- 9


#Aplicamos labels a la variable
db1$css <- set_label(x = db1$css,label = "Social Class")

get_label(db1$css)

#Aplicamos labels valores
db1$css <- set_labels(db1$css,
                     labels=c( "Burguesía tradicional"=1,
                               "Pequeño empleador"=2,
                               "Pequeña Burguesía"=3,
                               "Directivo/Supervisor Experto"=4,
                               "Experto no directivo"=5,
                               "Directivo/Supervisor semi-credencializado"=6,
                               "Obrero semi-credencializado"=7,
                               "Directivo/Supervisor no credencializado"=8,
                               "Proletario tradicional"=9)) #Clase Social

frq(db1$css)


# Grouped Social Class

db1$css_grouped<- car::recode(db1$css,
                          "1:3=1; 4:6=2; 7:9=3") # 1 = Upper classes / 2=Middle Groups / 3 = Working Class

db1$css_grouped <- set_labels(db1$css_grouped,
                      labels=c( "Upper classes"=1,
                                "Middle Groups"=2,
                                "Working Class"=3)) #Clase Social




# 3. Variables de control ####

## 3.1 Clase Social subjetiva ####

db1$cs_sub <- car::recode(db1$TOPBOT,
                         "1:4=2; 5:6=1; 7:10=0") # 2= WK-Class / 2=Middle Groups / 0=Upper Class

#Inclumimos labels
db1$cs_sub <- set_labels(db1$cs_sub,
                        labels=c("Clase Subjetiva baja"=2, "Clase Subjetiva Media"=3,
                                 "Clase Subjetiva Alta"=0)) #Clase Social Subjetiva
frq(db1$cs_sub) 


## 3.2 Construcción variable working class else = wk_else ####
db1$wk_else <- car::recode(db1$css,
                          "1:6=0; 7:9=1") #0 = Not working class / 1= Working class 

## 3.3 Construcción variable: correctly class identity (correct_ci) ####
db1$correct_ci <- NA
# Con comando ifelse -> 1 = correct_class_identity / 0 = incorrect_class_identity
db1$correct_ci<- ifelse(db1$wk_else == 1 & db1$cs_sub == 2, 1,
                       ifelse(db1$wk_else == 1 & db1$cs_sub %in% c(0, 1), 0, NA))
#Incluimos labels
db1$correct_ci <- set_labels(db1$correct_ci,
                            labels=c("Corresponding Class Identity"=1,
                                     "Not corresponding Class Identity" = 0)) #Correct Class identity

## 3.4 Posición Política ####
db1$pos_pol <- car::recode(db1$V48,
                          "0:4=1; 5=2; 6:10=3")
#Incluimos labels
db1$pos_pol <- set_labels(db1$pos_pol,
                         labels=c("Izquierda"=1, "Centro"=2,
                                  "Derecha"=3)) #Posición Política
frq(db1$pos_pol) 


## 3.5 Situación Marital ####
db1$partner <- car::recode(db1$PARTLIV,
                          "1:2=1; 3=0")
#Incluimos labels
db1$partner <- set_labels(db1$partner,
                         labels=c("In partnership"=1,
                                  "Not in partnership"=0)) #Partnership


# 4. Construcción de variable acción colectiva (sumatoria) ####

## 4.1 Loop that recodes, 1:2 = 1 (Yes), 3:4 = 2 (No) ####


variables <- c("V17", "V18", "V19", "V22")  # List of variables to recode

for (var in variables) {
  db1[[var]] <- ifelse(db1[[var]] %in% c(1, 2), 1, ifelse(db1[[var]] %in% c(3, 4), 2, db1[[var]]))
}
#Loop para reemplazar 2 por 0 y que 1 sea participar
replace_values <- function(data, vars) {
  for (var in vars) {
    data[[var]] <- ifelse(data[[var]] == 2, 0, data[[var]])
  }
  return(data)
}
variables <- variables <- c("V17", "V18", "V19", "V22") 
db1 <- replace_values(db1, variables)


## 4.2 Segunda Sumatoria ####

# Create a new variable named 'new_variable' as the summation of other variables
db1$acc <- rowSums(db1[, c("V17", "V18", "V19", "V22")])

#Labels para sumatoria
#Aplicamos labels a la variable
db1$acc <- set_label(x = db1$acc,label = "Collective Action Participation")



## 4.3 Construcción de acc2 ####
db1$acc2 <- car::recode(db1$acc,
                       "0=0; 1:16=1")
db1$acc2 <- set_label(x = db1$acc2,label = "Dummy Collective Action")
frq(db1$acc2)


# 5. Asimilamos al n de css ####

#Filtrar la base de datos para mantener solo las filas donde "clase_social" no sea NA
db1 <- db1 %>%
  filter(!is.na(css))


# 7. Exportar y guardar ####

export_dataframe_as_RDS <- function(dataframe, folder_path, file_name) {
  # Create the folder if it doesn't exist
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
  
  # Prepare the full file path
  file_path <- file.path(folder_path, paste0(file_name, ".RDS"))
  
  # Save the dataframe as RDS, overwriting if the file exists
  saveRDS(dataframe, file = file_path)
}

# Usage example:
# Assuming you have a dataframe called "db1" and want to export it to the specified folder
dataframe_to_export <- db1
folder_path <- "C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/3. Dataframes"
file_name <- "1_variables"

# Call the function to export the dataframe as RDS
export_dataframe_as_RDS(dataframe_to_export, folder_path, file_name)
