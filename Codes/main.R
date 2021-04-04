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
library(dplyr)
library(rstatix)
library(corrplot)

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

dados <- fetch_datasus(year_start = 2010, #month_start = 1, 
                       year_end = 2019, #month_end = 2,
                       uf = "PE", 
                       information_system = "SINASC")

dados <- process_sinasc(dados) #View(dados)
glimpse(dados)

plot(dados$CODOCUPMAE)

# Qnt. de cons. PN vs Ocorr. anom. Cong.  ----
a = dados$CONSULTAS
b = dados$IDANOMAL; plot(a); plot(b)
tb = table(a, b); tb = t(tb)
chi = chisq.test(tb); chi 

name = "NumConsPN_OcorAnomCong"
write.csv2(tb, file = paste("Results/", name, '.csv', sep=""))

# Analise post-hoc para o teste chi^2
postHoc = chisq.posthoc.test(tb, method = "bonferroni")
#View(postHoc)

# Calcula o V de Cramer
cramer_v(tb); chi[2]

# Exibe a analise dos residuos
#jpeg(filename = paste("Results/", name, '.jpeg', sep=""), width = 1200, height = 800, quality = 100, res = 150)
corrplot(chi$stdres, is.cor = FALSE,
         method = "color",
         tl.col = "black", tl.srt = 0)
#dev.off()

# Qnt. de cons. PN vs Est. civ. mae  ----
a = dados$CONSULTAS
b = dados$ESTCIVMAE; plot(a); plot(b)
tb = table(a, b); tb = t(tb); tb
chi = chisq.test(tb); chi 

# Salva os resultados
name = "NumConsPN_EstCivMae"
write.csv2(tb, file = paste("Results/", name, '.csv', sep=""))

# Analise post-hoc para o teste chi^2
postHoc = chisq.posthoc.test(tb, method = "bonferroni")
#View(postHoc)

# Calcula o V de Cramer
cramer_v(tb); chi[2]

# Exibe a analise dos residuos
#jpeg(filename = paste("Results/", name, '.jpeg', sep=""), width = 1250, height = 1100, quality = 100, res = 150)
corrplot(chi$stdres, is.cor = FALSE,
         method = "color",
        tl.col = "black", tl.srt = 0)
#dev.off()

# Qnt. de cons. PN vs Esc. mae  ----
a = dados$CONSULTAS
b = dados$ESCMAE; plot(a); plot(b)
tb = table(a, b); #tb = t(tb); tb
chi = chisq.test(tb); chi 

name = "NumConsPN_EscolMae"
write.csv2(tb, file = paste("Results/", name, '.csv', sep=""))

# Analise post-hoc para o teste chi^2
postHoc = chisq.posthoc.test(tb, method = "bonferroni")
#View(postHoc)

# Calcula o V de Cramer
cramer_v(tb); chi[2]

# Exibe a analise dos residuos
#jpeg(filename = paste("Results/", name, '.jpeg', sep=""), width = 1600, height = 800, quality = 75, res = 150)
corrplot(chi$stdres, is.cor = FALSE,
         method = "color",
         tl.col = "black", tl.srt = 0)
#dev.off()





# Qnt. de cons. PN vs Raxa/cor mae  ----
a = dados$CONSULTAS
b = dados$RACACORMAE; plot(a); plot(b)
tb = table(a, b); tb; #tb = t(tb); tb
tb = tb[,c(-1)]
chi = chisq.test(tb); chi 

name = "NumConsPN_RacaCorMae"
write.csv2(tb, file = paste("Results/", name, '.csv', sep=""))

# Analise post-hoc para o teste chi^2
postHoc = chisq.posthoc.test(tb, method = "bonferroni")
#View(postHoc)

# Calcula o V de Cramer
cramer_v(tb); chi[2]

# Exibe a analise dos residuos
#jpeg(filename = paste("Results/", name, '.jpeg', sep=""), width = 1600, height = 1000, quality = 75, res = 150)
corrplot(chi$stdres, is.cor = FALSE,
         method = "color",
         tl.col = "black", tl.srt = 0)
#dev.off()





# Esc. mae vs Tipo de parto  ----
a = dados$ESCMAE
b = dados$PARTO; plot(a); plot(b)
tb = table(a, b); tb; tb = t(tb); tb
tb = tb[,c(-1)]
chi = chisq.test(tb); chi 

name = "EscMae_TipoParto"
write.csv2(tb, file = paste("Results/", name, '.csv', sep=""))

# Analise post-hoc para o teste chi^2
postHoc = chisq.posthoc.test(tb, method = "bonferroni")
#View(postHoc)

# Calcula o V de Cramer
cramer_v(tb); chi[2]

# Exibe a analise dos residuos
#jpeg(filename = paste("Results/", name, '.jpeg', sep=""), width = 1600, height = 1000, quality = 75, res = 150)
corrplot(chi$stdres, is.cor = FALSE,
         method = "color",
         tl.col = "black", tl.srt = 0)
#dev.off()
# Idade mae vs Ocorr. anom. Cong  ----
a = dados$IDADEMAE
b = as.numeric(dados$IDANOMAL);  #1 - Não; 2 - Sim
matriz = data.frame(a = a, b = b)
matriz = na.omit(matriz)
str(matriz)

cor.test(matriz$a, matriz$b) 
plot(matriz$b[1:10000]~matriz$a[1:10000])

# Qnt. sem.gest. vs Ocorr. anom. Cong  ----
a = dados$SEMAGESTAC 
b = as.numeric(dados$IDANOMAL); #1 - Não; 2 - Sim
matriz = data.frame(a = a, b = b)
matriz = na.omit(matriz)
str(matriz)

cor.test(matriz$a, matriz$b) 
plot(matriz$b[1:10000]~matriz$a[1:10000])
