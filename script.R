###############################################################
#       Trabalho Final de Métodos Estatísticos II - 1/2021    #
#                                                             #
#                                                             #
#           Estudo do impacto dos afazeres domésticos         #
#           no rendimento acadêmico segundo o SAEB 2017       #
#                                                             #
#             Autores: Aline de Almeida Ramos                 #
#                   Lucas Loureiro Lino da Costa              #
#                       Luiz Felippe Enéas                    #
#                                                             #
#                                                             #
#                                                             #
# Script para análise dos dados                               #
# Data: 14/05/2021                                            #
###############################################################


###############################################################
### Configuração Inicial ###
###############################################################
# Instalação, carregamento dos pacotes necessários e configuração do #
# diretório de trabalho e não utilização de notação científica #

packages = c("ggplot2", "readr", "tidyverse",
             "reshape2", "ggforce", "devtools", "DescTools", "moments",
             "PropCIs", "nortest", "car", "ggpubr", "nortest", "rstatix",
             "classit")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

devtools::install_github(c("guiastrennec/ggplus", "r-spatial/classInt"))

library(ggplus)
library(ggplot2)
library(readr)
library(tidyverse)
library(reshape2)
library(DescTools)
library(classInt)
library(moments)
library(PropCIs)
library(nortest)
library(car)
library("ggpubr")
library(nortest)
library("rstatix")
library(scales)

setwd("/cloud/project")
options(scipen=999)


###############################################################
### DataSets e Ajustes ###
###############################################################
# Carregando os dados da pesquisa no sistema #
# As três amostras inciais, foram unificadas em um único arquivo #
df_list = list.files(path = "/cloud/project/DataBases/", pattern = ".csv")

df_total = data.frame()

for (i in 1:length(df_list)){
  file = read_csv(paste("/cloud/project/DataBases/", df_list[i], sep = ""))
  df_total = rbind(df_total, file)
  rm(file) # removendo o db temporário
}


# Remoção das entradas nulas e/ou vazias #
# Remoção das entradas duplicas, se existirem #
df_total = df_total[complete.cases(df_total), ]

df_total = df_total %>% distinct() # existiam duas entradas idênticas


# Criação dos rótulos das variáveis categóricas de análise #
df_total$REGIAO = factor(df_total$REGIAO,
                           labels = c("Norte", "Nordeste", "Sudeste",
                                      "Sul", "Centro-Oeste"),
                           levels = c(1, 2, 3, 4, 5))

df_total$AREA = factor(df_total$AREA,
                         labels = c("Capital", "Interior"), levels = c(1,2))

df_total$DEPENDENCIA_ADM = factor(df_total$DEPENDENCIA_ADM,
                                    labels = c("Federal", "Estadual",
                                               "Municipal"),
                                    levels = c(1:3))

df_total$SEXO = factor(df_total$SEXO,
                         labels = c("Masculino", "Feminino"),
                         levels = c("A", "B"))

df_total$RACA_COR = factor(df_total$RACA_COR,
                             labels = c("Branca", "Preta", "Parda",
                                        "Amarela","Indígena",
                                        "Não quero declarar"),
                             levels = c("A", "B", "C", "D", "E", "F"))

df_total$IDADE = factor(df_total$IDADE,
                          labels = c("8 anos ou menos",
                                     "9 anos", "10 anos",
                                     "11 anos", "12 anos","13 anos",
                                     "14 anos", "15 anos ou mais"),
                          levels = c("A", "B", "C", "D",
                                     "E", "F", "G", "H"))

df_total$MORA_MÃE = factor(df_total$MORA_MÃE,
                             labels = c("SIM", "Não",
                                        "Não, mas moro com outra mulher responsável por mim"),
                             levels = c("A", "B", "C"))

df_total$LOCALIZACAO = factor(df_total$LOCALIZACAO, 
                                labels = c("Urbana", "Rural"),
                                levels = c(1, 2))

df_total$USO_TEMPO_TELAS = factor(df_total$USO_TEMPO_TELAS,
                                    labels = c("Menos de 1 hora",
                                               "Entre 1 e 2 horas",
                                               "Mais de 2 horas, até 3 horas",
                                               "Mais de 3 horas",
           "Não vejo TV, não navego na internet e não jogo jogos eletrônicos"),
                                    levels =c(LETTERS[1:5]))

df_total$AFAZERES_DOM = factor(df_total$AFAZERES_DOM,
                               labels = c("Menos de 1 hora",
                                          "Entre 1 e 2 horas",
                                          "Mais de 2 horas, até 3 horas",
                                          "Mais de 3 horas",
                                          "Não faço trabalhos domésticos"),
                               levels = c(LETTERS[1:5]))

# Escolha da seed específica, para replicação #
# Criação da Amostra Aleatória Simples de tamanho 1000 #
set.seed(322)
df_amostra = df_total[sample(nrow(df_total), 1000),]


###############################################################
### Medidas de Posição e Dispersão ###
###############################################################
# NOTA_MT e LOCALIZACAO #
tapply(df_amostra$NOTA_MT, df_amostra$LOCALIZACAO,Freq)
tapply(df_amostra$NOTA_MT, df_amostra$LOCALIZACAO,summary)
tapply(df_amostra$NOTA_MT, df_amostra$LOCALIZACAO,var)
tapply(df_amostra$NOTA_MT, df_amostra$LOCALIZACAO,sd)

# NOTA_MT e REGIAO #
tapply(df_amostra$NOTA_MT, df_amostra$REGIAO,Freq)
tapply(df_amostra$NOTA_MT, df_amostra$REGIAO,summary)
tapply(df_amostra$NOTA_MT, df_amostra$REGIAO,var)
tapply(df_amostra$NOTA_MT, df_amostra$REGIAO,sd)

# NOTA_MT e SEXO #
tapply(df_amostra$NOTA_MT, df_amostra$SEXO,Freq)
tapply(df_amostra$NOTA_MT, df_amostra$SEXO,summary)
tapply(df_amostra$NOTA_MT, df_amostra$SEXO,var)
tapply(df_amostra$NOTA_MT, df_amostra$SEXO,sd)

# NOTA_MT e RAÇA #
tapply(df_amostra$NOTA_MT, df_amostra$RACA_COR,Freq)
tapply(df_amostra$NOTA_MT, df_amostra$RACA_COR,summary)
tapply(df_amostra$NOTA_MT, df_amostra$RACA_COR,var)
tapply(df_amostra$NOTA_MT, df_amostra$RACA_COR,sd)

# NOTA_MT e IDADE #
tapply(df_amostra$NOTA_MT, df_amostra$IDADE,Freq)
tapply(df_amostra$NOTA_MT, df_amostra$IDADE,summary)
tapply(df_amostra$NOTA_MT, df_amostra$IDADE,var)
tapply(df_amostra$NOTA_MT, df_amostra$IDADE,sd)

# NOTA_MT e AFAZERES_DOM #
tapply(df_amostra$NOTA_MT, df_amostra$AFAZERES_DOM,Freq)
tapply(df_amostra$NOTA_MT, df_amostra$AFAZERES_DOM,summary)
tapply(df_amostra$NOTA_MT, df_amostra$AFAZERES_DOM,var)
tapply(df_amostra$NOTA_MT, df_amostra$AFAZERES_DOM,sd)


# AFAZERES_DOM e LOCALIZACAO #
table(df_amostra$AFAZERES_DOM, df_amostra$LOCALIZACAO)
tapply(df_amostra$AFAZERES_DOM, df_amostra$LOCALIZACAO, Freq)
tapply(df_amostra$AFAZERES_DOM, df_amostra$LOCALIZACAO, summary)

# AFAZERES_DOM e REGIAO #
table(df_amostra$AFAZERES_DOM, df_amostra$REGIAO)
tapply(df_amostra$AFAZERES_DOM, df_amostra$REGIAO, Freq)
tapply(df_amostra$AFAZERES_DOM, df_amostra$REGIAO, summary)

# AFAZERES_DOM e SEXO #
table(df_amostra$AFAZERES_DOM, df_amostra$SEXO)
tapply(df_amostra$AFAZERES_DOM, df_amostra$SEXO, Freq)
tapply(df_amostra$AFAZERES_DOM, df_amostra$SEXO, summary)

# AFAZERES_DOM e RAÇA #
table(df_amostra$AFAZERES_DOM, df_amostra$RACA_COR)
tapply(df_amostra$AFAZERES_DOM, df_amostra$RACA_COR, Freq)
tapply(df_amostra$AFAZERES_DOM, df_amostra$RACA_COR, summary)

# AFAZERES_DOM e IDADE #
table(df_amostra$AFAZERES_DOM, df_amostra$IDADE)
tapply(df_amostra$AFAZERES_DOM, df_amostra$IDADE, Freq)
tapply(df_amostra$AFAZERES_DOM, df_amostra$IDADE, summary)


###############################################################
### Testes de Normalidade ###
# Anderson-Darling #
###############################################################
# NOTA_MT #
ad.test(df_amostra$NOTA_MT) # p-valor muito pequeno (não normal)


###############################################################
### Testes de Independência ###
# Teste do Qui-Quadrado #
###############################################################
# AFAZERES_DOM e LOCALIZACAO #
chisq.test(df_amostra$AFAZERES_DOM, df_amostra$LOCALIZACAO)
# aceita H0, não são diferentes os tempos dedicados a afazeres domésticos entre as localizações

# AFAZERES_DOM e REGIAO #
chisq.test(df_amostra$AFAZERES_DOM, df_amostra$REGIAO)
ContCoef(df_amostra$AFAZERES_DOM, df_amostra$REGIAO, correct = TRUE)
# rejeita H0, são diferentes os tempos dedicados a afazeres domésticos entre as regiões
# associação fraca para moderada ver o valor do coeficiente acima

# AFAZERES_DOM e SEXO #
chisq.test(df_amostra$AFAZERES_DOM, df_amostra$SEXO)
ContCoef(df_amostra$AFAZERES_DOM, df_amostra$SEXO)
# rejeita H0, são diferentes os tempos dedicados a afazeres domésticos entre as regiões
# associação fraca para moderada ver o valor do coeficiente acima

# AFAZERES_DOM e RAÇA #
chisq.test(df_amostra$AFAZERES_DOM, df_amostra$RACA_COR)
# aceita H0, não são diferentes os tempos dedicados a afazeres domésticos entre a raça/cor


# AFAZERES_DOM e IDADE #
chisq.test(df_amostra$AFAZERES_DOM, df_amostra$IDADE)
# aceita H0, não são diferentes os tempos dedicados a afazeres domésticos entre as idades


###############################################################
### Identificação de Outliers ###
###############################################################
# NOTA_MT e LOCALIZACAO #
df_amostra %>%
  group_by(LOCALIZACAO) %>%
  identify_outliers(NOTA_MT) # não possui

# NOTA_MT e REGIAO #
df_amostra %>%
  group_by(REGIAO) %>%
  identify_outliers(NOTA_MT) # 4 - sendo 2 superiores nordeste
                              # 1 superior sudeste e 1 inferior sudeste

# NOTA_MT e SEXO #
df_amostra %>%
  group_by(SEXO) %>%
  identify_outliers(NOTA_MT) # 3 para feminino

# NOTA_MT e RAÇA #
df_amostra %>%
  group_by(RACA_COR) %>%
  identify_outliers(NOTA_MT) # 1 para não informado

# NOTA_MT e IDADE #
df_amostra %>%
  group_by(IDADE) %>%
  identify_outliers(NOTA_MT) # 1 para 9 anos, 2 para 11 anos, 3 para 13 e 1 para 15 anos

##### NOTA_MT e USO_TEMPO_TELAS #
##### df_amostra %>%
#####  group_by(USO_TEMPO_TELAS) %>%
#####  identify_outliers(NOTA_MT) # 2 para menos de 1 hora
##### Fiz a mais #####

# como foram observados poucos outliers, optou-se por não retirar-los da amostra


###############################################################
### Testes de Homocedasticidade ###
# Testes robustos de Levene (usando mediana - variante de Brown) #
###############################################################
# NOTA_MT e LOCALIZACAO #
LeveneTest(NOTA_MT ~ LOCALIZACAO, df_amostra) # homogênas

# NOTA_MT e REGIAO #
LeveneTest(NOTA_MT ~ REGIAO, df_amostra) # homogêneas

# NOTA_MT e SEXO #
LeveneTest(NOTA_MT ~ SEXO, df_amostra) # heterogêneas (mesma coisa pra oteste normal de levene)

# NOTA_MT e RAÇA #
LeveneTest(NOTA_MT ~ RACA_COR, df_amostra) # homogêneas

# NOTA_MT e IDADE #
LeveneTest(NOTA_MT ~ IDADE, df_amostra) # homogêneas


#### NOTA_MT e USO_TEMPO_TELAS #
#### LeveneTest(NOTA_MT ~ USO_TEMPO_TELAS, df_amostra) # homogêneas
#### fiz a mais #####

###############################################################
### Teste de Kruskal-Wallis ###
###############################################################
# NOTA_MT e REGIAO #
kruskal.test(NOTA_MT ~ REGIAO, data = df_amostra) # p-valor muito muito baixo

# NOTA_MT e RAÇA #
kruskal.test(NOTA_MT ~ RACA_COR, df_amostra) # p-valor muito muito baixo

# NOTA_MT e IDADE #
kruskal.test(NOTA_MT ~ IDADE, df_amostra) # p-valor muito muito baixo

#### NOTA_MT e USO_TEMPO_TELAS #
#### kruskal.test(NOTA_MT ~ USO_TEMPO_TELAS, df_amostra) # p-valor muito muito baixo
#### fiz a mais #####


# para todas acima, ao menos uma das médias difere das demais (H0 falsa)


###############################################################
### Teste de Mann-Whitney - Wilcoxon ###
###############################################################
# NOTA_MT e LOCALIZACAO #
wilcox.test(NOTA_MT ~ LOCALIZACAO, df_amostra) # p-valor muito muito baixo

# NOTA_MT e SEXO #
wilcox.test(NOTA_MT ~ SEXO, df_amostra) # p-valor muito muito baixo

# para todas acima, a média difere entre os dois grupos (H0 falsa)



###############################################################
### Análise Ad-Hoc das Médias Diferentes dos Grupos ###
# Teste de Dunn com ajuste do valor de p por Bonferroni #
###############################################################
# NOTA_MT e LOCALIZACAO #
dunn_test(NOTA_MT ~ LOCALIZACAO, data = df_amostra,
          p.adjust.method = "bonferroni")

# NOTA_MT e REGIAO #
dunn_test(NOTA_MT ~ REGIAO, data = df_amostra,
          p.adjust.method = "bonferroni")

# NOTA_MT e SEXO #
dunn_test(NOTA_MT ~ SEXO, data = df_amostra,
          p.adjust.method = "bonferroni")

# NOTA_MT e RAÇA #
dunn_test(NOTA_MT ~ RACA_COR, data = df_amostra,
          p.adjust.method = "bonferroni")

# NOTA_MT e IDADE #
print(dunn_test(NOTA_MT ~ IDADE, data = df_amostra,
          p.adjust.method = "bonferroni"), n= 45) # função print adicional, para não omitir as entradas


#### NOTA_MT e USO_TEMPO_TELAS #
#### dunn_test(NOTA_MT ~ USO_TEMPO_TELAS, data = df_amostra,
####          p.adjust.method = "bonferroni")
#### Fiz a mais ####



###############################################################
### Gráficos ###
# Boxplots #
###############################################################
# NOTA_MT e LOCALIZACAO #
ggplot(df_amostra, aes(x = LOCALIZACAO, y = NOTA_MT, group = LOCALIZACAO)) +
  geom_boxplot(aes(fill = LOCALIZACAO)) +
  scale_fill_discrete(name = "Localização da escola \n do estudante") +
  labs(y = "Proficiência em matemática", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# NOTA_MT e REGIAO #
ggplot(df_amostra, aes(x = REGIAO, y = NOTA_MT, group = REGIAO)) +
  geom_boxplot(aes(fill = REGIAO)) +
  scale_fill_discrete(name = "Região da escola \n do estudante") +
  labs(y = "Proficiência em matemática", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# NOTA_MT e SEXO #
ggplot(df_amostra, aes(x = SEXO, y = NOTA_MT, group = SEXO)) +
  geom_boxplot(aes(fill = SEXO)) +
  scale_fill_discrete(name = "Sexo do estudante") +
  labs(y = "Proficiência em matemática", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# NOTA_MT e RAÇA #
ggplot(df_amostra, aes(x = RACA_COR, y = NOTA_MT, group = RACA_COR)) +
  geom_boxplot(aes(fill = RACA_COR)) +
  scale_fill_discrete(name = "Raça/Cor declarada \n pelo estudante") +
  labs(y = "Proficiência em matemática", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# NOTA_MT e IDADE #
ggplot(df_amostra, aes(x = IDADE, y = NOTA_MT, group = IDADE)) +
  geom_boxplot(aes(fill = IDADE)) +
  scale_fill_discrete(name = "Idade do estudante") +
  labs(y = "Proficiência em matemática", x = "") +
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#### NOTA_MT e USO_TEMPO_TELAS #
#### ggplot(df_amostra, aes(x = USO_TEMPO_TELAS, y = NOTA_MT,
####                       group = USO_TEMPO_TELAS)) +
####  geom_boxplot(aes(fill = USO_TEMPO_TELAS)) +
####  scale_fill_discrete(name = "Tempo de uso de telas") +
####  labs(y = "Proficiência em matemática", x = "") +
####  theme(panel.background = element_blank(),
####        axis.title.x=element_blank(),
####        axis.text.x=element_blank(),
####        axis.ticks.x=element_blank())
#### Fiz a mais ####

###############################################################
### Gráficos ###
# Histogrmas #
###############################################################
# AFAZERES_DOM e REGIAO #
df_total %>%
  ggplot(aes(x = AFAZERES_DOM, fill = REGIAO, group = REGIAO)) +
  geom_bar(stat="count", position = position_dodge(),
           aes(y = (..prop..))) +
  labs(y = "Porcentagem", binwidth = 5, fill = "Região", x = "") +
  scale_y_continuous(labels = percent) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 15, vjust = 0.5))

# AFAZERES_DOM e SEXO #
df_total %>%
  ggplot(aes(x = AFAZERES_DOM, fill = SEXO, group = SEXO)) +
  geom_bar(stat="count", position = position_dodge(),
           aes(y = (..prop..))) +
  labs(y = "Porcentagem", binwidth = 5, fill = "Sexo do estudante", x = "") +
  scale_y_continuous(labels = percent) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 15, vjust = 0.5))

