############## PARTE 1 #############
# 1. Peparamos ambiente de trabajo ####

library(pacman)
pacman::p_load(tidyverse,
               haven,
               sjPlot,
               sjmisc,
               sjlabelled,
               summarytools,
               car,
               openxlsx)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica


# 2. Abrimos dataset ####
setwd("C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA")
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
#frq(db$NEMPLOY) 




# 5. Eliminamos NA ####


db$SEX <- ifelse(db$SEX == 9, NA, db$SEX)
db$AGE <- ifelse(db$AGE == 999, NA, db$AGE)
db$DEGREE <- ifelse(db$DEGREE == 9, NA, db$DEGREE)
db$EMPREL <- ifelse(db$EMPREL %in% c(0, 9), NA, db$EMPREL)
db$NEMPLOY <- ifelse(db$NEMPLOY %in% c(9995, 9998, 9999), NA, db$NEMPLOY)
db$WRKSUP <- ifelse(db$WRKSUP %in% c(0, 8, 9), NA, db$WRKSUP)
db$TOPBOT <- ifelse(db$TOPBOT %in% c(0, 98, 99), NA, db$TOPBOT)
db$V17 <- ifelse(db$V17 %in% c(8, 9), NA, db$V17)
db$V18 <- ifelse(db$V18 %in% c(8, 9), NA, db$V18)
db$V19 <- ifelse(db$V19 %in% c(8, 9), NA, db$V19)
db$V20 <- ifelse(db$V20 %in% c(8, 9), NA, db$V20)
db$V21 <- ifelse(db$V21 %in% c(8, 9), NA, db$V21)
db$V22 <- ifelse(db$V22 %in% c(8, 9), NA, db$V22)
db$V23 <- ifelse(db$V23 %in% c(8, 9), NA, db$V23)
db$V24 <- ifelse(db$V24 %in% c(8, 9), NA, db$V24)
db$V25 <- ifelse(db$V25 %in% c(8, 9), NA, db$V25)
db$V27 <- ifelse(db$V27 %in% c(8, 9), NA, db$V27)
db$UNION <- ifelse(db$UNION %in% c(7, 8, 9), NA, db$UNION)
db$V48 <- ifelse(db$V48 %in% c(98, 99), NA, db$V48)
db$ISCO08 <- ifelse(db$ISCO08 %in% c(0, 9998, 9999, 0110, 0210, 0310), NA, db$ISCO08)
db$PARTLIV <- ifelse(db$PARTLIV %in% c(7, 9, 0), NA, db$PARTLIV)


# 6. Resumen de datos ####

#sjlabelled::get_label(db) #Vemos los labels
#summarytools::dfSummary(db, plain.ascii = FALSE)
#view(dfSummary(db, headings=FALSE))

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
# Assuming you have a dataframe called "db" and want to export it to the specified folder
dataframe_to_export <- db
folder_path <- "C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/3. Dataframes"
file_name <- "0_preamble"

# Call the function to export the dataframe as RDS
export_dataframe_as_RDS(dataframe_to_export, folder_path, file_name)


