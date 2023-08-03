library(pacman)

pacman::p_load(sjPlot,
               sjmisc,
               haven,
               stargazer,
               effects,
               dplyr,
               margins,
               ggeffects,
               modelsummary,
               sandwich,
               lmtest,
               arm)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

db2<-readRDS("C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/3. Dataframes/1_variables.RDS")




# Preambulo #### 

#Filtramos por submuestra de variables deseadas




# 1. Multivariate Analysis ####




#Pasamos a factor para regresión
db2$css<-as_factor(db2$css) #Clase Social Marx
db2$css_grouped<-as_factor(db2$css_grouped) #Clase Social Marx
db2$SEX<-as_factor(db2$SEX) #Sexo
db2$cs_sub<-as_factor(db2$cs_sub) #Clase Social Subj.
db2$unionized<-as_factor(db2$unionized) # unionized.
db2$pos_pol<-as_factor(db2$pos_pol) #Posición política



# 2. Modelos ####

#More info at.

## 2.1 Modelo A: Variable dependiente Acción Colectiva  / #Independiente = css ####

# Modelo A
reg1<-glm(acc2 ~ css+unionized, data = db2,
          family = binomial (link = "logit"))

# Modelo A.2
reg2<-glm(acc2 ~ css+unionized+cs_sub+SEX+partner, data = db2,
          family = binomial (link = "logit"))

# Modelo A.3
reg3<-glm(acc2 ~ css+unionized+cs_sub+AGE, data = db2,
          family = binomial (link = "logit"))

# Modelo A.4
reg4<-glm(acc2 ~ css+unionized+AGE, data = db2,
          family = binomial (link = "logit"))

# Modelo A.5
reg5<-glm(acc2 ~ css+unionized+cs_sub+AGE+SEX+partner, data = db2,
          family = binomial (link = "logit"))

#sjPlot::tab_model(list(reg1, reg2, reg3, reg4, reg5), show.ci=FALSE, show.se = TRUE,
                  #show.aic = TRUE, show.r2 = TRUE, collapse.se = TRUE,
                  #title = "Multivariate model measuring collective action (SE on parenthesis)",
                  #p.style = "stars",
                  #string.pred = "Predictors", 
                  #string.est = "β", 
                  #digits = 2,
                  #dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"))

modelo_a<-list(reg1, reg2, reg3, reg4, reg5)


## 2.2 Modelo A: With robust SE ####

#Reg 1
reg1_robust<-coeftest(reg1, vcov = vcovHC(reg1))
print(reg1_robust)

#Reg 2
reg2_robust<-coeftest(reg2, vcov = vcovHC(reg2))
print(reg2_robust)

#Reg 3
reg3_robust<-coeftest(reg3, vcov = vcovHC(reg3))
print(reg3_robust)

#Reg 4
reg4_robust<-coeftest(reg4, vcov = vcovHC(reg4))
print(reg4_robust)


# Modelo A.5
reg5_robust<-coeftest(reg5, vcov = vcovHC(reg5))
print(reg5_robust)


modelo_a_robust<-list(reg1_robust, reg2_robust, reg3_robust,
                      reg4_robust, reg5_robust)


## 2.3 Calculating predicted values and residuals ####
#df_inst<-select(db2, acc2, css, unionized) # DF instrumental

#reg1_pred_res<-glm(acc2 ~ css+unionized, data = df_inst,
          #family = binomial (link = "logit"))






#df_inst$res<-residuals(reg1_pred_res) #Residuals

## 2.3 Calcullatin multicolineallity ####

vif_model_5<-car::vif(reg5)



# 3. Ploting Logit models ####

## 3.1 Modelo (no SE ROBUST) ####

modelo_a_star<-stargazer(modelo_a,
                    header = FALSE,
                    column.sep.width = "0.5pt",
                    title = "Logit model measuring collective action participation (SE on parenthesis)",
                    covariate.labels = c("Small Employers (Ref: Burgeoisie)", "Petty Bourgeoisie", "Expert Managers",
                                         "Expert non-managers", "Semi-credentialled managers", "Semi-credentialled worker",
                                         "Non-credentialled manager", "Traditional proletariat","Trade union membership: Yes (ref: No)",
                                         "Subjective Middle Class (ref: Upper class)", "Subjective Working Class",
                                         "Female (ref: Male)", "Age", "Has a steady partner (ref: No)"),
                    digits = 2,
                    dep.var.caption  = "Dependant Variable: Collective Action Participation",
                    dep.var.labels.include = FALSE,
                    no.space = TRUE,
                    font.size = "small",
                    align = TRUE,
                    label = "modelo-a",
                    type = "latex"
                    ) 
#Guardamos Modelo A
save(modelo_a_star,
     file = "C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/4. Data_Tables/modelo_a_star.RData")



## 3.2 Modelo (SE ROBUST) ####



modelo_a_star_robust<-stargazer(modelo_a,
                         header = FALSE,
                         column.sep.width = "0.5pt",
                         title = "Logit model measuring collective action participation (Robust SE on parenthesis)",
                         covariate.labels = c("Small Employers (Ref: Burgeoisie)", "Petty Bourgeoisie", "Expert Managers",
                                              "Expert non-managers", "Semi-credentialled managers", "Semi-credentialled worker",
                                              "Non-credentialled manager", "Traditional proletariat","Trade union membership: Yes (ref: No)",
                                              "Subjective Middle Class (ref: Upper class)", "Subjective Working Class",
                                              "Female (ref: Male)", "Age", "Has a steady partner (ref: No)"),
                         digits = 2,
                         dep.var.caption  = "Dependant Variable: Collective Action Participation",
                         dep.var.labels.include = FALSE,
                         no.space = TRUE,
                         font.size = "small",
                         align = TRUE,
                         label = "modelo-a-robust",
                         type = "latex"
                         ) 
print(modelo_a_star_robust)

#Guardamos Modelo A Robust
save(modelo_a_star_robust,
     file = "C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/4. Data_Tables/modelo_a_star_robust.RData")


## 3.3 Modelo (SE ROBUST) ####

modelo_a_star_vif<-stargazer(vif_model_5,
                                header = FALSE,
                                column.sep.width = "0.5pt",
                                title = "VIF Analysis of the Logistic Model",
                                digits = 2,
                                dep.var.caption  = "VIF Analysis",
                                dep.var.labels.include = FALSE,
                                no.space = TRUE,
                                font.size = "small",
                                align = TRUE,
                                label = "modelo-a-vif",
                                type = "latex"
) 
print(modelo_a_star_vif)

#Guardamos Modelo A Robust
save(modelo_a_star_vif,
     file = "C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/4. Data_Tables/modelo_a_star_vif.RData")





# 4. Plot Marginal Effects margins #### 

# Addittional resources:
# https://strengejacke.github.io/ggeffects/articles/introduction_plotmethod.html



# Effects modelo A (effects)


# Union and Class
dat <- ggpredict(reg5, terms = c("unionized", "css"))

  






modelo_a_plot<-plot(allEffects(reg1)) #Plot modelo A


save(reg1,
     file = "C:/Users/Alvaro C/Dropbox/3. Educacion/1. MA_LACIS/Lacis_MA/5. Plots/reg1.RData")



# 4. Plotting residuals and predicted values ####

#Using binned residual plot: must read about this: 
# https://bookdown.org/jefftemplewebb/IS-6489/logistic-regression.html#assessing-logistic-model-fit

binnedplot(fitted(reg1), 
           residuals(reg1, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")



# 6. Exportar y guardar ####

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
