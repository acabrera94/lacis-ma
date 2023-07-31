library(pacman)
pacman::p_load(sjPlot)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

db2<-readRDS("C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/3. Dataframes/1_variables.RDS")




# 1. Multivariate Analysis ####


## 1.1 Análisis exploratorio y conversción en factor ####

tab_xtab(var.row = db2$css,db2$acc,show.cell.prc = T,show.summary = F) #Tabla de contingencia de variable de interés
#Pasamos a factor para regresión
db2$css<-as_factor(db2$css) #Clase Social Marx
db2$sex<-as_factor(db2$SEX) #Sexo
db2$cs_sub2<-as_factor(db2$cs_sub) #Clase Social Subj.
db2$pos_pol2<-as_factor(db2$pos_pol) #Posición política



# 2. Modelos ####

## 2.1 Modelo A: Variable dependiente Acción Colectiva  / #Independiente = css ####

# Modelo A
reg1<-lm(acc ~ css, data = db2)

# Modelo A.2
reg2<-lm(acc ~ css+cs_sub2+sex, data = db2)

# Modelo A.3
reg3<-lm(acc ~ css+cs_sub2+AGE, data = db2)

# Modelo A.4
reg4<-lm(acc ~ css+AGE+partner, data = db2)

# Modelo A.5
reg5<-lm(acc ~ css+cs_sub2+AGE+sex+partner, data = db2)
sjPlot::tab_model(list(reg1, reg2, reg3, reg4, reg5), show.ci=FALSE, show.se = TRUE,
                  show.aic = TRUE, show.r2 = TRUE, collapse.se = TRUE,
                  title = "Multivariate regression models predicting determinants of collective action within capitalist structure (SE on parenthesis)",
                  p.style = "stars",
                  string.pred = "Predictors", 
                  string.est = "β", 
                  digits = 2,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"))


## 2.2 Modelo B: Dependent: collective action / idependent (correct_class-identity) ####

# Modelo B.1
mod1<-lm(acc ~ correct_ci, data = db2)
sjPlot::tab_model(list(mod1), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1"))

# Modelo B.2
mod2<-lm(acc ~ correct_ci+unionized, data = db2)
sjPlot::tab_model(list(mod1, mod2), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2"))

# Modelo B.3
mod3<-lm(acc ~ correct_ci+unionized+pos_pol2+AGE, data = db2)
sjPlot::tab_model(list(mod1, mod2, mod3), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"))

# Modelo B.4
mod4<-lm(acc ~ correct_ci+unionized+pos_pol2+sex, data = db2)
sjPlot::tab_model(list(mod1, mod2, mod3, mod4), show.ci=FALSE, p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"))

# Modelo B.5
mod5<-lm(acc ~ correct_ci+unionized+pos_pol2+AGE+sex, data = db2)
sjPlot::tab_model(list(mod1, mod2, mod3, mod4, mod5), show.ci=FALSE, show.se = TRUE,
                  show.aic = TRUE, show.r2 = TRUE, collapse.se = TRUE,
                  title = "Collective Action determinants in contemporary capitalism",
                  p.style = "stars",
                  string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"))






# 4. Modelling ####


stargazer()



# 4. Exportar y guardar ####

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
dataframe_to_export <- db2
folder_path <- "C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/3. Dataframes"
file_name <- "2_analyses"

# Call the function to export the dataframe as RDS
export_dataframe_as_RDS(dataframe_to_export, folder_path, file_name)
