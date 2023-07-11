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
db$auto_emp <- ifelse(db$EMPREL == 1, 2, 1) # 5 = 1 (autoempleado), else = 2
frq(db$auto_emp) 

#Construcción de credencial ## DE MOMENTO: AQUELLOS QUE SE SALEN QUEDAN COMO NO CREDENCIALIZADOS
db$credencial <- car::recode(db$DEGREE,
                             "0:3=1; 4=2; 5:6=3")

frq(db$credencial)


#Construcción de num_empleados
db$num_empleados <- car::recode(db$NEMPLOY,
                                "1:2=1; 3:10=2; 11:6000=3")

frq(db$num_empleados)


#Construcción de Clase Social
db$css <- NA

# Condition 1: Burguesía tradicional (Value: 1)
db$css[db$auto_emp == 1 & db$num_empleados == 3 & db$WRKSUP == 1] <- 1

# Condition 2: Pequeño empleador (Value: 2)
db$css[db$auto_emp == 1 & db$num_empleados == 2 & db$WRKSUP == 1] <- 2

# Condition 3: Pequeña burguesía (Value: 3)
db$css[db$auto_emp == 1 & db$num_empleados == 1 & db$WRKSUP == 1] <- 3

# Condition 4: Directivo/Supervisor Experto (BA) (Value: 4)
db$css[db$auto_emp == 2 & db$WRKSUP == 1 & db$credencial == 3] <- 4

# Condition 5: Experto (BA+) No directivo (Value: 5)
db$css[db$auto_emp == 2 & db$WRKSUP == 2 & db$credencial == 3] <- 5

# Condition 6: Directivo/Supervisor Semi-Credencializado (Value: 6)
db$css[db$auto_emp == 2 & db$WRKSUP == 1 & db$credencial == 2] <- 6

# Condition 7: Obrero semi-credencializado (Value: 7)
db$css[db$auto_emp == 2 & db$WRKSUP == 2 & db$credencial == 2] <- 7

# Condition 8: Directivo/Supervisor no credencializado (Value: 8)
db$css[db$auto_emp == 2 & db$WRKSUP == 1 & db$credencial == 1] <- 8

# Condition 9: Proletariado tradicional (Value: 9)
db$css[db$auto_emp == 2 & db$WRKSUP == 2 & db$credencial == 1] <- 9

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
variables <- c("V17", "V18", "V19", "V20",
               "V21", "V22", "V23", "V24", "V25",
               "V27", "V28")  # List of variables to recode

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
variables <- variables <- c("V17", "V18", "V19", "V20",
                            "V21", "V22", "V23", "V24", "V25",
                            "V27", "V28") 
db <- replace_values(db, variables)
#Segundo: Sumatoria
# Create a new variable named 'new_variable' as the summation of other variables
db$acc <- rowSums(db[, c("V17", "V18", "V19", "V20",
                         "V21", "V22", "V23", "V24", "V25",
                         "V27", "V28")])
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




