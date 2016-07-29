# Análise dos Afastamentos no CBMDF - CID F

getwd()
afastamentos <- read.csv("Relatório CID F DITIC - só LTSP, prorrog e VAF.csv", dec = ",", 
                         sep = ";", stringsAsFactors = FALSE)
str(afastamentos)
View(afastamentos)

library(ggplot2)
library(ggthemes)
library(gridExtra)
library(stringr)
install.packages("zoo")
library(zoo)


# trabalhando com data de nascimento

data_de_nascimento <- afastamentos$Data.nascimento #clonando para não perder

which(is.na(data_de_nascimento)) #retirando NA

afastamentos$Data.nascimento[4187]
afastamentos <- afastamentos[- 4187, ] # retirando outlier
data_de_nascimento[4187]

data_de_nascimento <- data_de_nascimento[-4187]


d <- as.Date(data_de_nascimento, format = "%d/%m/%y" ) # formatando data

data_de_nascimento_format <- as.Date(ifelse(d > Sys.Date(),
                                            format(d, "19%y-%m-%d"),
                                            format(d)))
head(data_de_nascimento_format)
idade <- (as.yearmon(data_de_nascimento) - as.yearmon(hoje)) * 12                                 

afastamentos$Data.nascimento <- data_de_nascimento_format # consertando no df origem                                 
                         
head(afastamentos$Data.nascimento)

# criando coluna idade

idade <- 2016 - as.numeric(format(as.Date(afastamentos$Data.nascimento,
                                   format="%m/%d/%y"), format="%Y")) 
head(idade)

afastamentos$Idade <- idade



# gráficos
#histograma de afastamento por sexo
ggplot(afastamentos, aes(x = Qtd.dias)) +
  geom_histogram(binwidth = 0.4) +
  facet_grid(Sexo~.)

# histograma de afastamento por quartel com mais ocorrências
xtabs(~Órgão, afastamentos)
quarteis_mais_afast <- afastamentos[afastamentos$Órgão == "GRUPAMENTO BOMBEIRO MILITAR DO RECANTO DAS EMAS CENTRAL" |
                                      afastamentos$Órgão == "GRUPAMENTO BOMBEIRO MILITAR DO LAGO SUL" |
                                      afastamentos$Órgão == "DIRETORIA DE SAÚDE" | 
                                      afastamentos$Órgão == "DIRETORIA DE INATIVOS E PENSIONISTAS", ]

ggplot(quarteis_mais_afast, aes(x = Qtd.dias)) +
  geom_histogram(binwidth = 0.7) +
  facet_grid(Órgão~.)

# gráfico de barras quartel mais afastado
ggplot(quarteis_mais_afast, aes(Qtd.dias, Idade, color= Órgão )) + geom_point()




