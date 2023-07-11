######## Master's Thesis / Article #############
####: Álvaro Cabrera M.
#Título: "Class, Capitalism, and Collective Action".
#Dataset: ISSP 2014.
#Fecha de inicio: 21-06-2023
#Fecha de término:



################  PARTE 3: ANALISIS ##########################################




#### Summary descriptive tables ####







#.1 Análisis exploratorio y conversción en factor
tab_xtab(var.row = db$css,db$acc,show.cell.prc = T,show.summary = F) #Tabla de contingencia de variable de interés

#Pasamos a factor para regresión
db$css<-as_factor(db$css) #Clase Social Marx
db$sex<-as_factor(db$SEX) #Sexo
db$cs_sub2<-as_factor(db$cs_sub) #Clase Social Subj.
db$pos_pol2<-as_factor(db$pos_pol) #Posición política

db$acc<-as_factor(db$acc) #Acción Colectiva
db$acc2<-as_factor(db$acc2) #Acción Colectiva2


##### MODELOS ######

#Modelo 1: Variable dependiente Acción Colectiva  / #Independiente = css

# Modelo A
reg1<-lm(acc ~ css, data = db)
sjPlot::tab_model(list(reg1), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1"))

# Modelo A.2
reg2<-lm(acc ~ css+cs_sub2, data = db)
sjPlot::tab_model(list(reg1, reg2), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2"))

# Modelo A.3
reg3<-lm(acc ~ css+cs_sub2+pos_pol2, data = db)
sjPlot::tab_model(list(reg1, reg2, reg3), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"))
# Modelo A.4
reg4<-lm(acc ~ css+cs_sub2+pos_pol2+AGE+sex, data = db)
sjPlot::tab_model(list(reg1, reg2, reg3, reg4), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"))