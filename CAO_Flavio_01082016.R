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

data_de_nascimento <- afastamentos$Data.nascimento #clonando para n?o perder

which(is.na(afastamentos$Data.nascimento)) #retirando NA

afastamentos$Data.nascimento[4187]
afastamentos <- afastamentos[- 4187, ] # retirando outlier
data_de_nascimento[4187]

data_de_nascimento <- data_de_nascimento[-4187]
View(afastamentos)

d <- as.Date(data_de_nascimento, format = "%d/%m/%y" ) # formatando data

#n?o precisou porque importei diferente 
data_de_nascimento <- as.Date(ifelse(d > Sys.Date(),
                                            format(d, "19%y-%m-%d"),
                                            format(d)))


afastamentos$Data.nascimento <- data_de_nascimento # consertando no df origem                                 

head(afastamentos$Data.nascimento)

# criando coluna idade

idade <- 2016 - as.numeric(format(as.Date(afastamentos$Data.nascimento,
                                          format="%d-%m-%y"), format="%Y")) 
head(idade)

afastamentos$Idade <- idade

# trocando nome de gbm por comar
# fazendo filtros
COMARES <- afastamentos
COMARI <- c("BRASÍLIA","LAGO SUL","SIA","ABASTECIMENTO","GUARÁ","ASA SUL","SUDOESTE")
COMARII <- c("TAGUATINGA","BRAZLÂNDIA","CEILÂNDIA", "SAMAMBAIA")
COMARIII <- c("PLANALTINA", "SEBASTIÃO","SOBRADINHO", "LAGO NORTE")
COMARIV <- c("BANDEIRANTE", "MARIA","GAMA","CANDANGOLÂNDIA", "RIACHO", "RECANTO")

COMARES$Órgão[grep(paste(COMARI, collapse = "|") , COMARES$Órgão)] <- "COMAR I"
COMARES$Órgão[grep(paste(COMARII, collapse = "|") , COMARES$Órgão)] <- "COMAR II"
COMARES$Órgão[grep(paste(COMARIII, collapse = "|") , COMARES$Órgão)] <- "COMAR III"
COMARES$Órgão[grep(paste(COMARIV, collapse = "|") , COMARES$Órgão)] <- "COMAR IV"
COMARES$Órgão[grep("ÁREA I" , COMARES$Órgão)] <- "COMAR I"
COMARES$Órgão[grep("ÁREA II" , COMARES$Órgão)] <- "COMAR II"
COMARES$Órgão[grep("ÁREA III" , COMARES$Órgão)] <- "COMAR III"

unique(COMARES$Órgão)             

# retirando os vafs
semvaf <- grep("Verificação", afastamentos$Finalidade)
afastamentos_sem_vaf <- afastamentos[-semvaf, ]

head(afastamentos[semvaf, ])
# gráficos

# histogramas das variáveis numéricas
# com vaf
media_afastamentos_cidF <- mean(afastamentos$Qtd.dias) 
dp_afastamentos_cidF <- sd(afastamentos$Qtd.dias)

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

# histograma sem VAF

media_afastamentos_cidF_semvaf <- mean(afastamentos_sem_vaf$Qtd.dias) 
dp_afastamentos_cidF_semvaf <- sd(afastamentos_sem_vaf$Qtd.dias)

media_afastamentos_cidF_semvaf
dp_afastamentos_cidF_semvaf



ggplot(data = afastamentos_sem_vaf, aes(x = log(Qtd.dias)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.7,
                 fill = "lightblue", col = "black") +
  geom_rug() +
  stat_function(fun = dnorm, args = list(mean = media_afastamentos_cidF_semvaf,
                                         sd = dp_afastamentos_cidF_semvaf),
                color = "red") +
  xlab("Dias afastamento") +
  ylab("Densidade") +
  ggtitle("Histograma Dias de Afastamentos - Exceto VAF") +
  theme_bw()


#histograma de afastamento por sexo
ggplot(afastamentos, aes(x = Qtd.dias)) +
  geom_histogram(binwidth = 0.9) +
  facet_grid(Sexo~.)+ coord_cartesian(xlim=c(0, 200), ylim=c(0, 400))+
  ylab("Ocorrências") + xlab("Quantidade de dias do afastamento")


# histograma de afastamento por quartel com mais ocorr?ncias
xtabs(Órgão, afastamentos)
quarteis_mais_afast <- afastamentos[afastamentos$Órgão == "GRUPAMENTO BOMBEIRO MILITAR DO RECANTO DAS EMAS CENTRAL" |
                                      afastamentos$Órgão == "GRUPAMENTO BOMBEIRO MILITAR DO LAGO SUL" |
                                      afastamentos$Órgão == "DIRETORIA DE SAÚDE" | 
                                      afastamentos$Órgão == "DIRETORIA DE INATIVOS E PENSIONISTAS", ]

ggplot(quarteis_mais_afast, aes(x = Qtd.dias)) +
  geom_histogram(binwidth = 0.7) +
  facet_grid(Órgão~.)+ coord_cartesian(xlim=c(0, 200), ylim=c(0, 30)) +
  ylab("Ocorrências")


# barras de CID's mais frequentes

# CIDF MAIS AFASTADOS
qplot(CID, data= afastamentos, geom="bar", 
      weight= Qtd.dias,
      ylab="TEMPO DE AFASTAMENTO") + 
  theme(axis.text.x = element_text(angle = 90,
  hjust = 1))

# CIDF MAIS OCORRÊNCIAS
# calculando as frequencias
afastamentos$Identidade <- rep(1, times = 4245 )
View(afastamentos)
library(dplyr)
frequencia_afast <- afastamentos %>% group_by(CID) %>% summarise( Identidade = sum(Identidade))

head(frequencia_afast)
qplot(CID, data= frequencia_afast, geom="bar", 
      ylab="OCORRÊNCIAS", width=1) +
  scale_x_discrete(limits=c("F412", "F32","F41","F432","F411","F43")) +
  
 


  ggplot(frequencia_afast, aes(x= CID,  y= Identidade, width= 0.5)) + 
    geom_bar(stat = "identity", fill= "steelblue" )+ 
    geom_text(aes(label= Identidade), 
              vjust=-0.25) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_discrete(limits=c("F412", "F32","F41","F432","F411","F43"))
+ coord_cartesian(xlim=c("F412", "F43"), ylim=c(0, 600))

  
  
  ggplot(CIDF_mais_FREQ_dplyr, aes(x= CID,  y= Identidade, width= 0.5)) + 
    geom_bar(stat = "identity", fill= "steelblue" )+ 
    geom_text(aes(label= Identidade), 
              vjust=-0.25) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ylab("Frequência do evento")#FUNCIONOU
  
 
  
  
  
  
  
   head(CIDF_mais_FREQ_dplyr)
  
  install.packages("scales")
library("scales")

# CIDF COMPARAÇÃO COM MAIORES FREQUENCIAS

CIDF_mais_FREQ <- afastamentos[afastamentos$CID == "F412" |
                                      afastamentos$CID == "F32" |
                                      afastamentos$CID == "F322" | 
                                      afastamentos$CID == "F41" |
                                 afastamentos$CID == "F432" |
                                 afastamentos$CID == "F411" |
                                 afastamentos$CID == "F43", ]

CIDF_mais_FREQ_dplyr <- frequencia_afast[CID == "F412" |
                                 CID == "F32" |
                                 CID == "F322" | 
                                 CID == "F41" |
                                 CID == "F432" |
                                 CID == "F411" |
                                 CID == "F43", ]


# GRÁFICO COM CID'S MAIS FREQ
head(CIDF_mais_FREQ)
qplot(CID, data= CIDF_mais_FREQ, geom="bar", 
            ylab="OCORRÊNCIAS") 




# colocando mais variáveis

ggplot(data=CIDF_mais_FREQ, aes(x=CID, y=Qtd.dias)) + 
  geom_bar(stat = "identity")+scale_fill_brewer(palette="Dark2")


ggplot(data = CIDF_mais_FREQ, aes(CID, Qtd.dias, fill= Finalidade )) + 
  geom_bar(stat = "identity")

######################################################
##### PLANILHA COM TODOS OS CID'S ####################
######################################################

getwd()

cidgeral <- read.csv("Planilhas - afastamentos gerais 050816.csv", dec = ",", 
                         sep = ";", stringsAsFactors = FALSE)


str(cidgeral)

# convertendo pra numérico
cidgeral$Quantidade.de.Dias <- as.numeric(cidgeral$Quantidade.de.Dias)

which(is.na(cidgeral$Quantidade.de.Dias))
head(cidgeral$Quantidade.de.Dias)

cidgeral$Quantidade.de.Atendimentos.por.Finalidade <- as.numeric(cidgeral$Quantidade.de.Atendimentos.por.Finalidade)
which(is.na(cidgeral$Quantidade.de.Atendimentos.por.Finalidade))

View(cidgeral)

# fazendo filtro pra CIDF

CIDF <- grep(pattern = "F", cidgeral$CID )

length(CIDF)
length(cidgeral$CID)


# estat?sticas com colunas num?ricas
# consertando o df

sum(cidgeral$Quantidade.de.Atendimentos.por.Finalidade) 
which(cidgeral$Quantidade.de.Atendimentos.por.Finalidade %% 1 != 0)

cidgeral$Quantidade.de.Atendimentos.por.Finalidade[866] <- 2362
cidgeral$Quantidade.de.Atendimentos.por.Finalidade[720] <- 1353
str(cidgeral$Quantidade.de.Dias)
which(cidgeral$Quantidade.de.Dias %% 1 !=0)
sum(cidgeral$Quantidade.de.Dias)

# comparando com CIDF
sum(cidgeral$Quantidade.de.Atendimentos.por.Finalidade[CIDF])
sum(cidgeral$Quantidade.de.Dias[CIDF])

# tentando fazer CID como fator
head(cidgeral$CID)
head(cidgeral)

install.packages("stringr")

CID_ESTRAT <- cidgeral
CID_ESTRAT$CID <-  str_replace(CID_ESTRAT$CID, "[0-9]{1}", "") 
CID_ESTRAT$CID <-  str_replace(CID_ESTRAT$CID, "Não possui CID", "NI")
unique(CID_ESTRAT$CID)

# GRÁFICO
# CIDF VS DEMAIS CIDS: Y ATENDIMENTOS
qplot(CID, data=CID_ESTRAT, geom="bar", 
      weight= Quantidade.de.Atendimentos.por.Finalidade,
      ylab="ATENDIMENTOS") + 
  geom_text(aes(label= Identidade), vjust=-0.25) 

CID_ESTRAT$Identidade <- rep(1, times = 2288 )
frequencia_afast_CIDESTRAT <- CID_ESTRAT %>% group_by(CID) %>% 
  summarise( Quantidade.de.Atendimentos.por.Finalidade = sum(Quantidade.de.Atendimentos.por.Finalidade))

head(frequencia_afast_CIDESTRAT)
ggplot(frequencia_afast_CIDESTRAT, aes(x= CID,  y= Quantidade.de.Atendimentos.por.Finalidade, width= 0.5)) + 
  geom_bar(stat = "identity", fill= "steelblue" )+ 
  geom_text(aes(label= Quantidade.de.Atendimentos.por.Finalidade), 
            vjust=-0.25) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Atendimentos por Finalidade")

library(ggplot2)


# CIDF VS DEMAIS CIDS: AFASTAMENTOS
qplot(CID, data=CID_ESTRAT, geom="bar", 
      weight= Quantidade.de.Dias,
      ylab="TEMPO DE AFASTAMENTO")
library(ggplot2)


ggplot(CID_ESTRAT, aes(Quantidade.de.Atendimentos.por.Finalidade)))
+ geom_line()

unique(CID_ESTRAT$CID)
CID_ESTRAT$CID[1974] <- "NI"  
CID_ESTRAT$CID[865] <- "NI"   

str(CID_ESTRAT)
head(CID_ESTRAT)

