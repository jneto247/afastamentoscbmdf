# Afastamentos gerais
# formatando o data frame
geralafast <- read.csv("Planilhas - afastamentos gerais.csv",  dec = ",", 
         sep = ";", stringsAsFactors = FALSE)
str(geralafast)
head(geralafast)
geralafast$Qtd.dias <- as.numeric(geralafast$Qtd.dias)
geralafast$Qtd.atendimentos <- as.numeric(geralafast$Qtd.atendimentos)

# anaslisando as cids
indice_cidf <- grep(pattern = "F", geralafast$CID)
length(indice_cidf)
length(geralafast$CID)

xtabs(~CID, geralafast)

