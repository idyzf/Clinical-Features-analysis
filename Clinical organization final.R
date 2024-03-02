library(openxlsx)


rm(list=ls())
clinical_features <- read.xlsx("clinico_atualizado.xlsx")

'%ni%' <- Negate("%in%") 
BD_v <- clinical_features %>% 
  dplyr::select(c(ID), contains('Locali')) %>% 
  tidyr::pivot_longer(!ID) %>% 
  dplyr::filter(value == 'Checked')

BD_v %>% 
  dplyr::count(ID)

## Ideli voltou
summary(as.factor(BD_v$name))
BD_v$loc <- BD_v$name
BD_v <- BD_v %>% mutate(loc = recode(loc,  "Localização.(choice=Frontal)" = "Frontal", 
                                     "Localização.(choice=Multicêntrico)" = "Multicentrico",
                                     "Localização.(choice=Multifocal)" = "Miltifocal",
                                     "Localização.(choice=Occipital)"= "Occipital",
                                     "Localização.(choice=Outro)" = "Outro",
                                     "Localização.(choice=Parietal)"="Parietal",
                                     "Localização.(choice=Temporal)" = "Temporal"))

summary(as.factor(BD_v$loc))

contagem <- BD_v %>%  dplyr::count(ID)
contagem$ene <- contagem$n
contagem1 <- subset(contagem, contagem$ene != 1)
contagem1$ene <- "More Than one"
contagem2<- subset(BD_v, BD_v$ID %ni% contagem1$ID)
contagem1$loc <- contagem1$ene
contagem1 <- contagem1[,-2:-3]
contagem2 <- contagem2[,-2:-3]
Loc <- rbind(contagem1, contagem2)

dados<- left_join(clinical_features, Loc)
clinical_features_unique <- distinct(clinical_features)
dados <- left_join(clinical_features_unique, Loc, by = "ID")
names(dados)
write.xlsx(dados, "clinico_atualizado_umloc.xlsx")

names(dados)
clinical_stepwise <- dados[,c(1, 30, 53, 39, 83, 34, 18, 10, 86, 87, 20, 9)]
write.xlsx(clinical_stepwise, "clinical_variables_oficial.xlsx")


### tabela clinico patologico
library(dplyr)
library(flextable)
library(gtsummary)
rm(list=ls())
clinical<- read.csv("clinical_data_OS_quartile_ID.csv", sep = ";")
clinical<- read.xlsx("clinical_variables_oficial.xlsx")
names(clinical)

#clin <- clinical[, c(1, 8, 27, 28, 51, 37, 79, 32, 28, 18, 35, 16, 7, 6, 73)]
variables <- names(clinical)
paste(variables,collapse = ",")

###no groups ###
clinical %>%
  dplyr::select(VC_quartile,
                Gender,
                Histology,
                Side,
                OS,
                Craniotomy,
                Ki.67.Positive.Percent,
                Atrx.Status,
                Vital.Status,
                Lobe.Location,
                MGMT.Status) %>%
  gtsummary::tbl_summary(
    by = NULL,
    type = where(is.numeric) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd}), ({min} : {max})")
  ) %>%
  gtsummary::as_flex_table() %>%
  save_as_docx(path = 'tabela_clinico_patologico.docx')

###groups ####

clinical %>%
  dplyr::select(VC_quartile,
                Gender,
                Histology,
                Side,
                OS,
                Craniotomy,
                KPS,
                Ki67,
                Atrx.Status,
                Vital.Status,
                Lobe.Location,
                MGMT.Status) %>%
  gtsummary::tbl_summary(
    by = VC_quartile,
    type = where(is.numeric) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd}), ({min} : {max})")
  ) %>%
  gtsummary::add_p(simulate.p.value = TRUE) %>%  # Use simulation for p-value
  gtsummary::as_flex_table() %>%
  save_as_docx(path = 'tabela_clinico_patologico_groups.docx')

# Header ------------------------------------------------------------------
# Hospital de Amor [https://hospitaldeamor.com.br/site/]
# Núcleo de Epidemiologia e Bioestatística (NEB) [https://iep.hospitaldeamor.com.br/plataformas-de-apoio/neb/]
# Bioestatístico: Welinton Yoshio Hirai [welinton.hirai@hospitaldeamor.com.br]
rm(list = ls())

# version 1 -  ter, 27/02/2024

#library used
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyselect)
library(tidyr)
library(readxl)

# data --------------------------------------------------------------------
#setwd("C:/Users/HANEB02/Downloads/Ideli")

clinical_data <- clinical
#rm(list = ls())

# version 1 -  ter, 27/02/2024

#library used
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyselect)
library(tidyr)
library(readxl)

# data --------------------------------------------------------------------
#setwd("C:/Users/HANEB02/Downloads/Ideli")

clinical_data <- clinical
#var_clinical <- names(clinical_data)[c(1, #ID 20, 75, #Surv. var.30, 39, 53)] #Var. Clinical
var_clinical <- names(clinical_data)

ruv_data <- read_excel("ruvcorrected.xlsx")

#merge data
data_merge <- 
  clinical_data %>% 
  dplyr::select_at(var_clinical) %>% 
  dplyr::mutate_all(as.factor) %>%
  dplyr::left_join(ruv_data, by = join_by(ID == Name)) %>% 
  dplyr::select(-ID) %>% 
  dplyr::mutate(Sobrevida.Global = Sobrevida.Global %>% 
                  as.character() %>% 
                  as.numeric())

# REg. COX Multi - sem step -----------------------------------------------

library(survival)

#formula da Reg. Cox
formula_suv <- Surv(time = Sobrevida.Global,
                    event = Status == "obito por câncer") ~ .

survival_fit <- 
  survival::coxph(data = data_merge,
                  formula = formula_suv)

library(gtsummary)

tbl_regression(survival_fit, exponentiate = T) %>% 
  gtsummary::as_gt() %>% 
  gt::tab_header('Regressão de Cox multivariada')

# REg. COX Multi - com step -----------------------------------------------

library(StepReg)

data_surv <-
  data_merge %>% 
  dplyr::mutate(STATUS_NEW = if_else(Status == "obito por câncer", 1, 0)) %>% 
  na.omit() %>% 
  #convertendo variável de fator para numericas
  dplyr::mutate_if(is.factor, as.numeric) %>% 
  dplyr::select(-Status)

# haven::write_sav(data = data_surv, path = 'BD.sav')

formula_suv = Surv(Sobrevida.Global, STATUS_NEW) ~ . 

model_stepfit <- 
  StepReg::stepwiseCox(formula = formula_suv,
                       data = data_surv,
                       selection = 'bidirection',
                       select = 'AIC',
                       method ="efron")

# Melhorando a saída

var_selected <- 
  unlist(model_stepfit$`Selected Varaibles`) %>% 
  as.vector()

data_step <- 
  data_merge %>% 
  dplyr::select(all_of(c('Sobrevida.Global', 'Status',
                         var_selected)))

formula_suv <- Surv(time = Sobrevida.Global,
                    event = Status == "obito por câncer") ~ .

survival_fit <- 
  survival::coxph(data = data_step,
                  formula = formula_suv)

library(gtsummary)

tbl_regression(survival_fit, exponentiate = T) %>% 
  gtsummary::as_gt() %>% 
  gt::tab_header('Regressão de Cox multivariada')
