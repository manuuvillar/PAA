data<-read.csv('data_tratado.csv')
setwd('C:/Users/Utilizador/OneDrive - Universidade do Minho/2-ano/aprendizage_automatica/trabalho')
attach(data)

names(data)
lista_categoricas<-c('brand','model','fuel_type','engine','transmission','ext_col','int_col','accident','clean_title')
lista_numericas <-c('model_year','milage')

  

for (i in 1:(length(lista_categoricas) - 1)) {
  for (j in (i + 1):length(lista_categoricas)) {
    variavel_i <- lista_categoricas[i]
    variavel_j <- lista_categoricas[j]
    
    # Criar uma tabela de contingência
    tabela_contingencia <- table(data[, variavel_i], data[, variavel_j])
    
    # Realizar o teste qui-quadrado
    resultado_teste <- chisq.test(tabela_contingencia)
    
    # Imprimir os resultados
    print(paste("Variáveis:", variavel_i, "e", variavel_j))
    print(resultado_teste)
  }
}

# H0: os testes são provenientes do modelo normal vs H1: os dados não são:
shapiro.test(milage)
#p-value < 2.2e-16 < 0.05(alpha)- rejeita H0
shapiro.test(price)
#p-value < 2.2e-16 < 0.05(alpha)- rejeita H0

#teste correlação
#H0: p=0 vs h1: p != 0
cor.test(milage,price, method='spearman')
# p-value < 2.2e-16 - rejeita H0


# Verificar se posso fazer a moda para fuel_type:
# H0: as categorias estão divididas proporcionalmente- os dados seguem modelo uniforme
#vs H1: Os dados não seguem o modelo uniforme

pi <-c(1/8,1/8,1/8,1/8,1/8,1/8,1/8,1/8)
chisq.test(table(fuel_type), p=pi)

#p-value < 2.2e-16 rejeita-se H0
# Logo, posso fazer a moda

# Para acidente:
table(accident)
p<- c(1/3,1/3,1/3)
chisq.test(table(accident), p=p)
#p-value < 2.2e-16

#verificar se posso fazer moda para engine:
table(engine)
p<- c(rep(1/1015,1015))
chisq.test(table(engine), p=p)
# p-value < 2.2e-16, rejeita-se H0
