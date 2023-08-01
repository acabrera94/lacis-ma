# Writing

tinytex::install_tinytex()
install.packages("rticles")

library(pacman)
pacman::p_load(rticles,
               tinytex,
               kableExtra)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

