# Links ----
# SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. 
# Microdatasus: pacote para download e pré-processamento de microdados do 
# Departamento de Informática do SUS (DATASUS). Cad. Saúde Pública, Rio de 
# Janeiro , v. 35, n. 9, e00032419, 2019 . Available from 
# http://ref.scielo.org/dhcq3y.

# Wiki: https://github.com/rfsaldanha/microdatasus/wiki

# Sistemas: https://datasus.saude.gov.br/wp-content/uploads/2019/08/Catalogo-de-Produtos-DATASUS.pdf

# Opções: https://github.com/danicat/datasus/

# Pacotes ----
#install.packages("devtools")
library(chisq.posthoc.test)
#devtools::install_github("rfsaldanha/microdatasus")

library(devtools)
#https://cran.r-project.org/web/packages/chisq.posthoc.test/vignettes/chisq_posthoc_test.html

library(microdatasus)
# dados = fetch_datasus(year_start = 2013, 
#                       year_end = 2014, 
#                       uf = "CE", 
#                       information_system = "SINASC")
# SIM-DO - Óbitos

dados <- fetch_datasus(year_start = 2019, #month_start = 1, 
                       year_end = 2019, #month_end = 2,
                       uf = "PE", 
                       information_system = "SINASC")

dados <- process_sinasc(dados) #View(dados)

# Consultas ----
a = dados$CONSULTAS

# Consultas vs Estado Civil
b = dados$ESTCIVMAE
plot(a); plot(b)

tb = table(a, b); tb
chisq.test(tb)
postHoc = chisq.posthoc.test(tb, method = "bonferroni")
View(postHoc)

chisq.test(tb)$stdres
s = 0.05
sAdj = s/(nrow(tb)*ncol(tb)); sAdj
qnorm(sAdj/2)

# Consultas vs Esc. de mãe
b = dados$ESCMAE
plot(a); plot(b)

tb = table(a, b); tb
chisq.test(tb)
postHoc = chisq.posthoc.test(tb, method = "bonferroni")
View(postHoc)

# Consultas vs Raca/cor
b = dados$RACACOR
plot(a); plot(b)

tb = table(a, b); tb
chisq.test(tb)
postHoc = chisq.posthoc.test(tb, method = "bonferroni")
View(postHoc)

#KOTELCHUCK
View(dados)
# Consultas vs Local de nascimento
b = dados$LOCNASC
plot(a); plot(b)

tb = table(a, b); tb
chisq.test(tb)
postHoc = chisq.posthoc.test(tb, method = "bonferroni")
View(postHoc)

# Estado civil ----
a = dados$ESTCIVMAE

# Estado civil vs Escolaridade da mae
b = dados$ESCMAE
plot(a); plot(b)

tb = table(a, b); tb
chisq.test(tb)
postHoc = chisq.posthoc.test(tb, method = "bonferroni")
View(postHoc)

chisq.test(tb)$stdres
s = 0.05
sAdj = s/(nrow(tb)*ncol(tb)); sAdj
qnorm(sAdj/2)
