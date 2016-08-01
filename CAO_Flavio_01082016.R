getwd()
afastamentos <- read.csv("Relatório CID F DITIC - só LTSP, prorrog e VAF.csv", dec = ",", 
                                       sep = ",", stringsAsFactors = FALSE)
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


d <- as.Date(data_de_nascimento, format = "%Y-%m-%d" ) # formatando data

#não precisou porque importei diferente data_de_nascimento_format <- as.Date(ifelse(d > Sys.Date(),
                                            format(d, "19%y-%m-%d"),
                                            format(d)))


afastamentos$Data.nascimento <- data_de_nascimento # consertando no df origem                                 

head(afastamentos$Data.nascimento)

# criando coluna idade

idade <- 2016 - as.numeric(format(as.Date(afastamentos$Data.nascimento,
                                          format="%Y-%m-%d"), format="%Y")) 
head(idade)

afastamentos$Idade <- idade



# gráficos

# cálculo da amostra

media_afastamentos_cidF <- mean(afastamentos$Qtd.dias) 
dp_afastamentos_cidF <- sd(afastamentos$Qtd.dias)
afastamentos$Qtd.dias <- as.numeric(afastamentos$Qtd.dias)
total_afast_CIDF <- sum(afastamentos$Qtd.dias)
fx <- dnorm(afastamentos$Qtd.dias, media_afastamentos_cidF, dp_afastamentos_cidF)
plot(afastamentos$Qtd.dias, fx)


#gráfico da função densidade
ggplot(data = afastamentos, aes(x = afastamentos$Qtd.dias)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.7,
                 fill = "lightblue", col = "black") +
  geom_rug() +
  stat_function(fun = dnorm, args = list(mean = media_afastamentos_cidF,
                                         sd = dp_afastamentos_cidF),
                color = "red") +
  xlab("Dias afastamento") +
  ylab("Densidade") +
  ggtitle("Histograma Dias de Afastamentos") +
  theme_bw()

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







######################################################
##### PLANILHA COM TODOS OS CID'S ####################
######################################################

cidgeral <- read.csv("Planilhas - afastamentos gerais.csv", dec = ",", 
                         sep = ",", stringsAsFactors = FALSE)
str(cidgeral)

# convertendo pra numérico
cidgeral$Quantidade.de.Dias <- as.numeric(cidgeral$Quantidade.de.Dias)
which(is.na(cidgeral$Quantidade.de.Dias))
head(cidgeral$Quantidade.de.Dias)
cidgeral$Quantidade.de.Dias[1] <- 0
cidgeral <- cidgeral[-1, ]

cidgeral$Quantidade.de.Atendimentos.por.Finalidade <- as.numeric(cidgeral$Quantidade.de.Atendimentos.por.Finalidade)
which(is.na(cidgeral$Quantidade.de.Atendimentos.por.Finalidade))

View(cidgeral)

# fazendo filtro pra CIDF

CIDF <- grep(pattern = "F", cidgeral$CID )

length(CIDF)
length(cidgeral$CID)

# estatísticas com colunas numéricas
# consertando o df

sum(cidgeral$Quantidade.de.Atendimentos.por.Finalidade) 
which(cidgeral$Quantidade.de.Atendimentos.por.Finalidade %% 1 != 0)

cidgeral$Quantidade.de.Atendimentos.por.Finalidade[866] <- 2362

str(cidgeral$Quantidade.de.Dias)
which(cidgeral$Quantidade.de.Dias %% 1 !=0)
sum(cidgeral$Quantidade.de.Dias)

# comparando com CIDF
sum(cidgeral$Quantidade.de.Atendimentos.por.Finalidade[CIDF])
sum(cidgeral$Quantidade.de.Dias[CIDF])

ggplot(