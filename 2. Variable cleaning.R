######## Master's Thesis / Article #############
####: Álvaro Cabrera M.
#Título: "Class, Capitalism, and Collective Action".
#Dataset: ISSP 2014.
#Fecha de inicio: 21-06-2023
#Fecha de término:



################  PARTE 2: Variable cleaning ##########################################




####  6. Construccion de variable####

## 6.1 Construcción de variable Clase Social (Erik Wright)

#Construcción de autoempleado
db$auto_emp <- ifelse(db$EMPREL == 1, 2, 1) #f value 1 (employee) then the value is 2, if not (self-employee), the value is 1.
frq(db$auto_emp) 

#Construcción de credencial ## DE MOMENTO: AQUELLOS QUE SE SALEN QUEDAN COMO NO CREDENCIALIZADOS
db$credencial <- car::recode(db$DEGREE,
                             "0:3=1; 4=2; 5:6=3")

frq(db$credencial)

#Construcción de variable "skills_2digit"
db$skills_2digit <- car::recode(db$ISCO08,
                             "1000:1120=11; 1200:1223=12; 1300:1349=13;
                         1400:1439=14; 2100:2166=21; 2200:2269=22; 2300:2359=23;
                         2400:2434=24; 2500:2529=25; 2600:2659=26; 3100:3155=31;
                         3200:3259=32; 3300:3359=33; 3400:3435=34; 3500:3522=35;
                         4100:4132=41; 4200:4229=42; 4300:4323=43; 4400:4419=44;
                         5100:5169=51; 5200:5249=52; 5300:5329=53; 5400:5419=54;
                         6100:6130=61; 6200:6224=62; 6300:6340=63; 7100:7133=71; 
                         7200:7234=72; 7300:7323=73; 7400:7422=74; 7500:7549=75;
                             8100:8189=81; 8200:8219=82; 8300:8350=83; 9100:9129=91;
                             9200:9216=92; 9300:9334=93; 9400:9412=94; 9500:9520=95;
                             9600:9629=96")

frq(db$skills_2digit)


#Construcción de variable "skills_2digit"
db$skills_1digit <- car::recode(db$ISCO08,
                                "1000:1439=1; 2000:2659=2; 3000:3522=3; 4000:4419=4; 5000:5419=5;
                                6000:6340=6; 7000:7549=7; 8000:8350=8; 9000:9629=9")

frq(db$skills_1digit)




#Construcción de num_empleados
db$num_empleados <- car::recode(db$NEMPLOY,
                                "1:9=1; 10:6000=2; 0=3")

frq(db$num_empleados)


#Construcción de Clase Social
db$css <- NA

# Condition 1: Burguesía tradicional (Value: 1)
db$css[db$auto_emp == 1 & db$num_empleados == 2 & db$WRKSUP == 1] <- 1

# Condition 2: Pequeño empleador (Value: 2)
db$css[db$auto_emp == 1 & db$num_empleados == 1 & db$WRKSUP == 1] <- 2

# Condition 3: Pequeña burguesía (Value: 3)
db$css[db$auto_emp == 1 & db$num_empleados == 3] <- 3

# Condition 4: Directivo/Supervisor Experto (BA) (Value: 4) - (Skills grupo 10 al 26 (isco) + Correción por credencial)
db$css[db$auto_emp == 2 & db$WRKSUP == 1 & db$skills_1digit %in% c(1, 2) & db$credencial == 3 ] <- 4

# Condition 5: Experto (BA+) No directivo (Value: 5)
db$css[db$auto_emp == 2 & db$WRKSUP == 2 & db$skills_1digit %in% c(1, 2) & db$credencial == 3] <- 5

# Condition 6: Directivo/Supervisor Semi-Credencializado (Value: 6)
db$css[db$auto_emp == 2 & db$WRKSUP == 1 & db$skills_1digit %in% c(3, 6)] <- 6

# Condition 7: Obrero semi-credencializado (Value: 7)
db$css[db$auto_emp == 2 & db$WRKSUP == 2 & db$skills_1digit %in% c(3, 6)] <- 7

# Condition 8: Directivo/Supervisor no credencializado (Value: 8)
db$css[db$auto_emp == 2 & db$WRKSUP == 1 & db$skills_1digit %in% c(7, 9)] <- 8

# Condition 9: Proletariado tradicional (Value: 9)
db$css[db$auto_emp == 2 & db$WRKSUP == 2 & db$skills_1digit %in% c(7, 9)] <- 9

frq(db$css)



## 6.2 Construcción de variables de control
#Clase Social subjetiva
db$cs_sub <- car::recode(db$TOPBOT,
                         "1:4=1; 5:6=2; 7:10=3")

frq(db$cs_sub) 

#Posición Política
db$pos_pol <- car::recode(db$V48,
                          "0:4=1; 5=2; 6:10=3")

frq(db$pos_pol) 

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



############################## PARTE 3: LABELS DE LAS VARIABLES ##################################
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
                        labels=c("Clase Subjetiva baja"=1, "Clase Subjetiva Media"=2,
                                 "Clase Subjetiva Alta"=3)) #Clase Social Subjetiva


db$pos_pol <- set_labels(db$pos_pol,
                         labels=c("Izquierda"=1, "Centro"=2,
                                  "Derecha"=3)) #Posición Política




