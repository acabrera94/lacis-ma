library(pacman)
pacman::p_load(sjPlot,
               xtable,
               stargazer)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

db2<-readRDS("C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/3. Dataframes/2_analyses.RDS")




# 1. Visualización descriptiva ####

## 1. Generamos una base clon para realizar descr con sjmisc ####

desc_db<-select(db2,
            acc,
            acc2,
            css,
            cs_sub,
            AGE,
            SEX,
            partner)

## 2. Generamos la tabla ####

descriptivo1<-sjmisc::descr(desc_db,
                            show = c("label","range", "mean", "sd", "NA.prc", "n"))

## 2. Pasamos a LaTeX con xtable ####

xtable(descriptivo1,
       type = "latex",
       caption = "Descriptives Statistics")


# Paso 2: Calcular frecuencia y frecuencia acumulada para todas las columnas numéricas




# 3. Exportar y guardar ####

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




