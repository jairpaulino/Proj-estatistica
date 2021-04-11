# KOTELCHUCK vs Ocorr. anom. cong ----
a = dados$KOTELCHUCK
b = dados$IDANOMAL; plot(a); plot(b)
tb = table(a, b); tb; tb = t(tb); tb
#tb = tb[,c(-1)]
chi = chisq.test(tb); chi 

name = "IndK_OcorrAmonCong"
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

## KOTELCHUCK vs Estado Civil Mãe ----
a = dados$KOTELCHUCK
b = dados$ESTCIVMAE; plot(a); plot(b)
tb = table(a, b); tb; tb = t(tb); tb
#tb = tb[,c(-1)]
chi = chisq.test(tb); chi 

name = "IndK_EstCivilMae"
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

## KOTELCHUCK vs Raça/CorMãe ----
a = dados$KOTELCHUCK
b = dados$RACACOR; plot(a); plot(b)
tb = table(a, b); tb; tb = t(tb); tb
#tb = tb[,c(-1)]
chi = chisq.test(tb); chi 

name = "IndK_RacaCorMae"
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

# Qnt. sem.gest. vs Ocorr. anom. Cong  ----
a = dados$SEMAGESTAC 
b = as.numeric(dados$IDANOMAL); #1 - Não; 2 - Sim
matriz = data.frame(a = a, b = b)
matriz = na.omit(matriz)
str(matriz)

cor.test = cor.test(matriz$a, matriz$b); cor.test
plot(matriz$b[1:10000]~matriz$a[1:10000])

name = "QntSemGest_OcorrAnomCong"
sink(paste("Results/", name, '.txt', sep=""))
cor.test
sink()

