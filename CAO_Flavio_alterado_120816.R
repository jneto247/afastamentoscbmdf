getwd()
setwd( "C:/Users/1667124/Documents/afastamentoscbmdf")
afastamentos <- read.csv("Relatório CID F DITIC - só LTSP, prorrog e VAF v5.csv", dec = ",", 
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

data_de_nascimento <- afastamentos$Data.nascimento #clonando para n?o perder

which(is.na(afastamentos$Data.nascimento)) #retirando NA


d <- as.Date(data_de_nascimento, format = "%d/%m/%Y" ) # formatando data
View(d)
#n?o precisou porque importei diferente 
data_de_nascimento <- as.Date(ifelse(d > Sys.Date(),
                                     format(d, "19%Y/%m/%d"),
                                     format(d)))


afastamentos$Data.nascimento <- data_de_nascimento # consertando no df origem                                 

head(afastamentos$Data.nascimento)

# criando coluna idade

idade <- 2016 - as.numeric(format(as.Date(afastamentos$Data.nascimento,
                                          format="%Y-%m-%d"), format="%Y")) 

View(idade)


afastamentos$Idade <- idade

# trocando nome de gbm por comar
# fazendo filtros
COMARES <- afastamentos
COMARI <- c("BRASÍLIA","LAGO SUL","SIA","ABASTECIMENTO","GUARÁ","ASA SUL","SUDOESTE")
COMARII <- c("TAGUATINGA","BRAZLÂNDIA","CEILÂNDIA", "SAMAMBAIA")
COMARIII <- c("PLANALTINA", "SEBASTIÃO","SOBRADINHO", "LAGO NORTE", "PARANO")
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
View(afastamentos_sem_vaf)
head(afastamentos[semvaf, ])

# graficos

# histogramas das variaveis numÃ©ricas
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



ggplot(data = afastamentos_sem_vaf, aes(x = Qtd.dias)) +
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
         geom_histogram(binwidth = 0.9)+stat_bin(binwidth= 1500, geom="text", aes(label=..count..) , 
                                                 vjust = -1) + coord_cartesian(xlim=c(0, 200), ylim=c(0, 200))+
         ylab("Ocorrências") + xlab("Quantidade de dias do afastamento")+
         facet_grid(Sexo~.)
       
       idade_cidf <- factor(afastamentos$Sexo) #tentando via fator
       cid_cidf <- factor(afastamentos$CID)
       table(cid_cidf)
       table(idade_cidf)
       barplot(table(cid_cidf, idade_cidf), beside= TRUE, las=2, legend = TRUE, args.legend = c("top"))
       pie(table(idade_cidf))
       str(afastamentos$Data.atendimento)
       
       
       rm(lt)
       
       # histograma de afastamento por quartel com mais ocorr?ncias
       xtabs(afastamentos$Ã“rgÃ£o, afastamentos)
       quarteis_mais_afast <- afastamentos[afastamentos$Ã“rgÃ£o == "GRUPAMENTO BOMBEIRO MILITAR DO RECANTO DAS EMAS CENTRAL" |
                                             afastamentos$Ã“rgÃ£o == "GRUPAMENTO BOMBEIRO MILITAR DO LAGO SUL" |
                                             afastamentos$Ã“rgÃ£o == "DIRETORIA DE SAÃšDE" | 
                                             afastamentos$Ã“rgÃ£o == "DIRETORIA DE INATIVOS E PENSIONISTAS", ]
       
       ggplot(quarteis_mais_afast, aes(x = Qtd.dias)) +
         geom_histogram(binwidth = 0.7) +
         facet_grid(Ã“rgÃ£o~.)+ coord_cartesian(xlim=c(0, 200), ylim=c(0, 30)) +
         ylab("OcorrÃªncias")
       
       
       # barras de CID's mais frequentes
       
      
       # afastamentos sem vaf por paciente (código)
       unique(afastamentos_sem_vaf$Código)
       length(unique(afastamentos_sem_vaf$Código))
       length(unique(afastamentos$Código))
       
       # quantidade posto e graduação # VER COM CALLLLLLLLLLLLLLMA
library(dplyr)
quantidade_pacientes_cidf <- afastamentos%>% group_by(Código) %>% summarise( Qtd.dias = sum(Qtd.dias))
quantidade_pacientes_cidf_semvaf <- afastamentos_sem_vaf %>% group_by(Código) %>% summarise( Qtd.dias)   
qtddias_unicos_cidf_semvaf <- afastamentos_sem_vaf %>% group_by(Código) %>% 
  summarise( Qtd.dias = sum(Qtd.dias) / length(unique(Qtd.dias)))
head(qtddias_unicos_cidf_semvaf, 20)




str(quantidade_pacientes_cidf_semvaf)  
quantidade_pacientes_cidf_semvaf <- as.numeric(quantidade_pacientes_cidf_semvaf)
  

ggplot(quantidade_pacientes_cidf_semvaf, aes(x= Código, y= Qtd.dias)) +
  geom_bar(stat = "identity", fill= "steelblue" )+ 
  geom_text(aes(label= Qtd.dias), 
            vjust=-0.25) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
table(quantidade_pacientes_cidf_semvaf)

max(quantidade_pacientes_cidf_semvaf)
min(quantidade_pacientes_cidf_semvaf)
which(quantidade_pacientes_cidf_semvaf, 1307)


 # CIDF MAIS AFASTADOS
       qplot(CID, data= afastamentos, geom="bar", 
             weight= Qtd.dias,
             ylab="TEMPO DE AFASTAMENTO") + 
         theme(axis.text.x = element_text(angle = 90,
                                          hjust = 1))
       
       # CIDF MAIS OCORRENCIAS
       # calculando as frequencias
       
       afastamentos$Identidade <- rep(1, times = 2549 )
       View(afastamentos)
       library(dplyr)
       frequencia_afast <- afastamentos %>% group_by(CID) %>% summarise( Identidade = sum(Identidade))
       
       head(frequencia_afast)
         
         
         
         
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
       
       # CIDF COMPARAÃ‡ÃƒO COM MAIORES FREQUENCIAS
       
       CIDF_mais_FREQ <- afastamentos[afastamentos$CID == "F412" |
                                        afastamentos$CID == "F32" |
                                        afastamentos$CID == "F322" | 
                                        afastamentos$CID == "F41" |
                                        afastamentos$CID == "F432" |
                                        afastamentos$CID == "F411" |
                                        afastamentos$CID == "F43", ]
       
       CIDF_mais_FREQ_dplyr <- frequencia_afast[frequencia_afast$CID == "F412" |
                                                  frequencia_afast$CID== "F32" |
                                                  frequencia_afast$CID == "F322" | 
                                                  frequencia_afast$CID == "F41" |
                                                  frequencia_afast$CID == "F432" |
                                                  frequencia_afast$CID == "F411" |
                                                  frequencia_afast$CID == "F43", ]
       
       
       # GRÁFICO COM CID'S MAIS FREQ
       head(CIDF_mais_FREQ)
       qplot(CID, data= CIDF_mais_FREQ, geom="bar", 
             ylab="OCORRÊNCIAS") 
       
       
       
       
       # colocando mais variÃ¡veis
       
       ggplot(data=CIDF_mais_FREQ, aes(x=CID, y=Qtd.dias + fill(Posto.Graduação)))+ 
         geom_bar(stat = "identity")+scale_fill_brewer(palette="Dark2")
       
       
       ggplot(data = CIDF_mais_FREQ, aes(CID, Quadro )) + 
         geom_point(stat = "identity")
       
       # cidf posto / quadro
      
       
       unique(afastamentos_sem_vaf$Posto.Graduação)
2sgt <- grep("SEGUNDO SARGENTO", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf[grep("[TERCEIRO SARGENTO]", afastamentos_sem_vaf)]
       
       
       
afastamentos_sem_vaf[grep("SEGUNDO SARGENTO")]              
afastamentos_sem_vaf$Posto.Grad.fator <- afastamentos_sem_vaf$Posto.Graduação 
afastamentos_sem_vaf$Posto.Grad.fator <- gsub(pattern = "SEGUNDO SARGENTO", replacement = "1", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <- gsub(pattern = "CABO", replacement = "2", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <-  gsub("PRIMEIRO SARGENTO", "3", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <-  gsub("TERCEIRO SARGENTO", "4", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <-  gsub("CAPITÃO", "5", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <-  gsub("TENENTE-CORONEL", "6", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <-  gsub("MAJOR", "7", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <-  gsub("8ENENTE", "8", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <-  gsub("SOLDADO SEGUNDA CLASSE", "9", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <-  gsub("PRIMEIRO TENENTE", "10", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <-  gsub("ASPIRANTE A OFICIAL", "11", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <-  gsub("CORONEL", "12", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <-  gsub("SEGUNDO TENENTE", "13", afastamentos_sem_vaf$Posto.Grad.fator)
afastamentos_sem_vaf$Posto.Grad.fator <- as.integer(afastamentos_sem_vaf$Posto.Grad.fator)
unique(afastamentos_sem_vaf$Posto.Grad.fator)
postogradlabs <- c("SEGUNDO SARGENTO","CABO", "PRIMEIRO SARGENTO", "TERCEIRO SARGENTO", "CAPITÃO", "TENENTE-CORONEL",
 "MAJOR", "SUBTENENTE", "SOLDADO", "PRIMEIRO TENENTE", "ASPIRANTE", "CORONEL", "SEGUNDO TENENTE")
postogradfator <- factor(afastamentos_sem_vaf$Posto.Grad.fator, labels = postogradlabs)
table(postogradfator)
pie(table(postogradfator), col=gray(seq(.2,1,.2)))
barplot(table(postogradfator), horiz= TRUE, las= 1, main = "Distribuição dos Postos/Graduações")




       ######################################################
       ##### PLANILHA COM TODOS OS CID'S ####################
       ######################################################
       
       getwd()
       
       cidgeral <- read.csv("Planilhas - afastamentos gerais.csv", dec = ",", 
                            sep = ",", stringsAsFactors = FALSE)
       
       
       str(cidgeral)
       
       # convertendo pra numÃ©rico
       cidgeral$Quantidade.de.Dias <- as.numeric(cidgeral$Quantidade.de.Dias)
       
       which(is.na(cidgeral$Quantidade.de.Dias))
       head(cidgeral$Quantidade.de.Dias)
       
       cidgeral$Quantidade.de.Atendimentos.por.Finalidade <- as.numeric(cidgeral$Quantidade.de.Atendimentos.por.Finalidade)
       which(is.na(cidgeral$Quantidade.de.Atendimentos.por.Finalidade))
       
       cidgeral <- cidgeral[-1, ]
       
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
       
       # GRÃFICO
       # CIDF VS DEMAIS CIDS: Y ATENDIMENTOS
       qplot(CID, data=CID_ESTRAT, geom="bar", 
             weight= Quantidade.de.Atendimentos.por.Finalidade,
             ylab="ATENDIMENTOS")  
       
       
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
       CID_ESTRAT <- CID_ESTRAT[ - 158, ]
       View(CID_ESTRAT)
       library(ggplot2)
       
       
       # CIDF VS DEMAIS CIDS: AFASTAMENTOS
       qplot(CID, data=CID_ESTRAT, geom="bar", 
             weight= Quantidade.de.Dias,
             ylab="TEMPO DE AFASTAMENTO")
       library(ggplot2)
       
       
       
       
       ggplot(CID_ESTRAT, aes(Quantidade.de.Atendimentos.por.Finalidade)))
+ geom_line()

unique(CID_ESTRAT$CID)

# CIDGERAL SEM OS VAFS

CID_ESTRAT[semvaf, ]

#############################################
## TRABALHANDO SEM OS VAF'S #################
#############################################

head(afastamentos[-semvaf, ])

head(afastamentos_sem_vaf)

semvaf_maisfreq <- afastamentos_sem_vaf[afastamentos_sem_vaf$CID == "F412" |
                                          afastamentos_sem_vaf$CID== "F32" |
                                          afastamentos_sem_vaf$CID == "F322" | 
                                          afastamentos_sem_vaf$CID == "F41" |
                                          afastamentos_sem_vaf$CID == "F432" |
                                          afastamentos_sem_vaf$CID == "F411" |
                                          afastamentos_sem_vaf$CID == "F43", ]
ggplot(data = semvaf_maisfreq, aes(CID, Qtd.dias, fill= Finalidade )) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90,
                                                              hjust = 1))



frequencia_semvaf_maisfreq <- semvaf_maisfreq %>% group_by(CID) %>% summarise( Identidade = sum(Identidade))
head(frequencia_semvaf_maisfreq)
str(frequencia_semvaf_maisfreq)
library(dplyr)

ggplot(data = frequencia_semvaf_maisfreq, aes(CID, y = Identidade, width = 0.5 )) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90,
                                                                 hjust = 1)) + ylab("Quantidade de Atendimentos")
summary(frequencia_semvaf_maisfreq)






# estatística variáveis numéricas
summary(afastamentos_sem_vaf)
ggplot(data = afastamentos_sem_vaf, aes(Idade)) + 
  geom_histogram()

# histograma idade
media_idade_semvasf <- mean(afastamentos_sem_vaf$Idade)
dp_idade_semvasf <- sd(afastamentos_sem_vaf$Idade)
ggplot(data = afastamentos_sem_vaf, aes(x = Idade)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.7,
                 fill = "lightblue", col = "black") +
  geom_rug() +
  stat_function(fun = dnorm, args = list(mean = media_idade_semvasf,
                                         sd = dp_idade_semvasf),
                color = "red") +
  xlab("Idade") +
  ylab("Densidade") +
  ggtitle("Histograma Idade") +
  theme_bw()

# histograma dias de afastamento
media_afastamentos_cidF_semvaf
dp_afastamentos_cidF_semvaf
ggplot(data = afastamentos_sem_vaf, aes(x = Idade)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.7,
                 fill = "lightblue", col = "black") +
  geom_rug() +
  stat_function(fun = dnorm, args = list(mean = media_idade_semvasf,
                                         sd = dp_idade_semvasf),
                color = "red") +
  xlab("Idade") +
  ylab("Densidade") +
  ggtitle("Histograma Idade") +
  theme_bw()


# Posto e graduação / quadro  vs CIDF, Órgão
