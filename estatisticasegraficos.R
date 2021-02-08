#instalando pacotes e abrindo bibliotecas
library(foreign)
library(xtable)
library(stargazer)


# CRIANDO O GRÁFICO DE ÓBITOS POR ANO
# base de óbitos por município

pop_ano<-pop%>%
  group_by(ano)%>%
  summarise(pop = sum(pop))%>%
  mutate(ano = as.numeric(ano))

obitos_totais_porano <- obitoind_agregada_dsei%>%
  group_by(ano)%>% 
  summarize(mortes_totais = sum(mortes_totais),
            suicidio = sum(suicidio),
            agressao = sum(agressao),
            saneamento = sum(saneamento),
            respiratorio = sum(resp),
            prev = sum(prev),
            prev1 = sum(prevum),
            prev2 = sum(prevdois),
            prev3 = sum(prevtres),
            cardiaco = sum(cardiaco),
            naoespec = sum(naoespec))%>%
  ungroup()

grafico_mortes_por_ano <- obitos_totais_porano%>%
  inner_join(pop_ano, by = "ano")%>%
  mutate(txmgeral = (mortes_totais/pop)*1000,
         txmsuicidio = (suicidio/pop)*1000,
         txmsaneamento = (saneamento/pop)*1000,
         txmagressao = (agressao/pop)*1000,
         txmresp = (respiratorio/pop)*1000,
         tx_prev_1 = (prev1/ pop)*1000,
         tx_prev_2 = (prev2/ pop)*1000,
         tx_prev_3 = (prev3/ pop)*1000,
         tx_prev_geral = (prev/ pop)*1000,
         tx_card = (cardiaco/ pop)*1000,
         tx_naoespec = (naoespec/pop)*1000)%>%
  select(ano,txmsaneamento,txmgeral,tx_naoespec,tx_card,tx_prev_geral,txmsuicidio,
         txmagressao, txmresp)
  

write.csv(grafico_mortes_por_ano, "grafico1.csv")












#ESTATÍSTICAS PARA TODOS OS DSEI 
##mortalidade

#PRINCIPAL CAUSA DE MORTE GERAL
obitoind%>% 
  count(causa)%>%
  arrange(desc(n))%>%
  mutate(total = sum(n))%>%
  mutate(porcentagem = n/total)

#PRINCIPAL CAUSA DE MORTE POR DSEI
statobitobas <- obitoind%>%
  group_by(dsei)%>%
  count(causa)%>%
  arrange(desc(n))%>%
  mutate(total = sum(n))%>%
  mutate(porcentagem = n/total)%>%
  ungroup()%>%
  group_by(dsei)%>%
  top_n(1)%>%
  ungroup()


statobitobas%>%
  count(causa)%>%
  arrange(desc(n))
#####

basecompleta161 <- basecompleta16%>%
  select(ibge, ano, dsei,regiao,pibpcapita,popind ,part_nas_mortes, txmsaneamento ,tx_mortalidade_dsei, 
         txmresp, txmsuicidio,txmagressao, txmcard, txmnaoespec,txmprev, cardiaco, resp, cardiaco_total_dsei,
         resp_total_dsei, naoespec_total_dsei, naoespec, suicidio, suicidio_total_dsei, agressao,
         agressao_total_dsei, saneamento, saneamento_total_dsei, prev, prev_total_dsei, netnias,poloporaldeia)%>%
  mutate(partcard = (cardiaco/cardiaco_total_dsei)*100,
         partresp = (resp/resp_total_dsei)*100,
        partnaoespec= (naoespec/naoespec_total_dsei)*100,
        partsuicidio = (suicidio/suicidio_total_dsei)*100,
        partsaneamento= (saneamento/ saneamento_total_dsei)*100,
        partagressao = (agressao/ agressao_total_dsei)*100,
        partprev = (prev/prev_total_dsei)*100)

write.dta(basecompleta161, "C:/Users/Matheus/Desktop/base1615.dta")
describeBy(basecompleta161, group = 'regiao')

#estatisticas descritivas mortalidade
















#PRINCIPAL CAUSA DE MORTE POR ANO

statobitobasano <- obitoind%>%
  group_by(ano)%>%
  count(causa)%>%
  arrange(desc(n))%>%
  mutate(total = sum(n))%>%
  mutate(porcentagem = n/total)%>%
  ungroup()%>%
  group_by(ano)%>%
  top_n(1)
##NAO COLOQUEI AINDA: COUNT NAS PRINCIPAIS PRIMEIRAS CAUSAS
statobitobas2<- statobitobas%>%
  count(n)


#TENTATIVA DE GERAR TABELA PRO LATEX

# write.xlsx(statobitobas, file="C:/Users/Matheus/Desktop/statobitobas3.xlsx" )
# print(xtable(statobitobas, type = "latex",  tabular.environment="longtable"), file = "C:/Users/Matheus/Desktop/statobitobas3.tex")
# 
# write.xlsx(x = statobitobas, file = "C:/Users/Matheus/Desktop/statobitobas1.xlsx"
#               , sheetName = "Sheet1")
# #MODELINHO PARA EXPLICAR DOENCAS DO SANEAMENTO
# 
# 
# modelinearsaneam <- lm(saneamento ~  m_integridade_territorial+ m_governanciati + 
#                anoscommtimposto + media_part_morb_recente
#              + m_estab_legal + cand_ind ,data = basecompleta16)
# 
# summary(modelinearsaneam)



##VARIAÇÃO DA TAXA DE MORTALIDADE PARA CADA DSEI
#ajustando a variavel tx de mortalidade
basecompleta16estats <- basecompleta16%>%
  select(ibge, dsei, ano, mortes_totais, 
         saneamento, resp, agressao, suicidio, prev, 
         prevum, prevdois, prevtres, regiao, popind, cardiaco,
         naoespec)


# baseauxregiao <- basecompleta16%>%
#   mutate(m=1)%>%
#   group_by(dsei,regiao)%>%
#   summarize(m = sum(m))%>%
#   select(-m)

#JUNTANDO OS DSEIS E OLHANDO POR ANO
graficosgeraisano <- basecompleta16estats%>%
  group_by(ano)%>%
  summarize(mortes_por_ano = sum(mortes_totais),
            suicidio = sum(suicidio),
            agressao = sum(agressao),
            saneamento = sum(saneamento),
            respiratorio = sum(resp),
            popind = sum(popind, na.rm =T),
            prev = sum(prev),
            prev1 = sum(prevum),
            prev2 = sum(prevdois),
            prev3 = sum(prevtres),
            card = sum(cardiaco),
            naoespec = sum(naoespec))%>%
  ungroup()%>%
  mutate(txmgeral = (mortes_por_ano/popind)*1000,
    txmsuicidio = (suicidio/popind)*1000,
         txmsaneamento = (saneamento/popind)*1000,
         txmagressao = (agressao/popind)*1000,
         txmresp = (respiratorio/popind)*1000,
         tx_prev_1 = (prev1/ popind)*1000,
         tx_prev_2 = (prev2/ popind)*1000,
         tx_prev_3 = (prev3/ popind)*1000,
         tx_prev_geral = (prev/ popind)*1000,
         tx_card = (card/ popind)*1000,
    tx_naoespec = (naoespec/popind)*1000)%>%
  ungroup()%>%
  filter(ano>2013)

#TRAÇANDO O GRAFICO DA EVOLUCAO DA MORTALIDADE PARA DIFERENTES CAUSAS DE ÓBITO


colors <- c("Respiratorio" = "blue", "Agressão" = "red", "Condições de Saneamento" = "orange", "Suicídio" = "yellow",
            "Causas Evitáveis" = 'grey',"Não-Especificado" )

ggplot(graficosgeraisano, aes(x=ano, group=1)) + 
  geom_line(aes(y = txmresp, color = "Respiratório"), size=1.7) +
  geom_line(aes(y = txmagressao, color = "Agressão"), size=1.7) +
  geom_line(aes(y = txmsuicidio, color = "Suicídio"), size=1.7) + 
  geom_line(aes(y = txmsaneamento, color="Condições de Saneamento"), size=1.7) +
  geom_line(aes(y = tx_prev_geral, color="Causas Evitáveis"), size=1.7)+
  geom_line(aes(y = tx_card, color="Cardio-Vascular"), size=1.7)+
  geom_line(aes(y = tx_naoespec, color="Não-Especificado"), size=1.7)+
 labs(x = "Ano",
       y = "Taxa da mortalidade a cada 1000 indígenas",
       title = "Avanço da mortalidade por diferentes causas entre 2010 e 2019 ") +
  scale_color_manual(name = "Causa do Óbito",
values = c( "Respiratório" = "blue", "Agressão"  = "red", "Condições de Saneamento" = "lightsalmon4", 
      "Suicídio" = "yellow", "Causas Evitáveis" = "grey", "Cardio-Vascular" = "mediumorchid4",
                                 "Não-Especificado" = "seagreen1"),
labels = c("Agressão", "Cardio-Vascular", "Causas Evitáveis", 
           "Condições de Saneamento Inadequadas" ,"Não-Especificado","Respiratório", "Suicídio"))+ theme_classic()




#OLHANDO POR ANO E POR DSEI
graficosgeraisdsei <- basecompleta16estats%>%
  group_by(dsei,ano)%>%
  summarize(mortes_total_dsei = sum(mortes_totais),
            suicidio = sum(suicidio),
            agressao = sum(agressao),
            saneamento = sum(saneamento),
            respiratorio = sum(resp),
            popind = mean(popind),
            tx_mortalidade_dsei = mean(tx_mortalidade_dsei))%>%
  mutate(txmsuicidio = (suicidio/popind)*1000,
         txmsaneamento = (saneamento/popind)*1000,
         txmagressao = (agressao/popind)*1000,
         txmresp = (respiratorio/popind)*1000 )%>%
  select(-agressao, -respiratorio, -saneamento, -suicidio)
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#para criar médias medianas e quartis por DSEI já que vamos fazer uma tabela


mediasmedianasdseis <- graficosgeraisdsei%>%
  group_by(dsei)%>%
  summarize(
         mediamort=mean(tx_mortalidade_dsei),
         meanamort=median(tx_mortalidade_dsei),
         dpmort=sd(tx_mortalidade_dsei),
         mediatxsuicidio = mean(txmsuicidio),
         meanasuicidio = median(txmsuicidio),
         dpsuicidio=sd(txmsuicidio),
         mediatxsaneamento = mean(txmsaneamento),
         meanasaneamento = median(txmsaneamento),
         dpsaneamento = sd(txmsaneamento),
         mediatxresp = mean(txmresp),
         meanaresp = median(txmresp),
         sdresp=sd(txmresp),
         mediatxagressao = mean(txmagressao),
         meanaagressao = median(txmagressao),
         sdagressao = sd(txmagressao))


basecompleta16


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#ONDE CAIU MAIS E ONDE CAIU MENOS A MORTALIDADE

# a base comparacaodseicomeco terá valores de mortalidade de 2010 e 2011 e faremos uma média entre ambos
comparacaodseicomeco <- graficosgeraisdsei %>%
  filter(ano == '2010' | ano=='2011' )%>%
  group_by(dsei)%>%
  summarize(mortalidadeinicial = mean(tx_mortalidade_dsei),
            suicidioinicial = mean(txmsuicidio),
            agressaoinicial = mean(txmagressao),
            respinicial = mean(txmresp),
            saneamentoinicial = mean(txmsaneamento))
  
#a base comparacaodseifinal terá valores de 2016 e 2017

comparacaodseifinal <- graficosgeraisdsei %>%
  filter(ano=='2016' | ano == '2017')%>%
  group_by(dsei)%>%
  summarize(mortalidadefinal = mean(tx_mortalidade_dsei),
            suicidiofinal = mean(txmsuicidio),
            agressaofinal = mean(txmagressao),
            respfinal = mean(txmresp),
            saneamentofinal = mean(txmsaneamento))
  

            
variacaodsei <- comparacaodseicomeco %>%
  inner_join(comparacaodseifinal, by = 'dsei')%>%
  mutate(variacao_mortalidade = (mortalidadefinal - mortalidadeinicial))%>%
  mutate(perc_mortalidade = ((mortalidadefinal - mortalidadeinicial)/ mortalidadeinicial)*100)%>%
  mutate(variacao_suicidio = (suicidiofinal - suicidioinicial))%>%
  mutate(variacao_agressao = (agressaofinal - agressaoinicial))%>%
  mutate(variacao_resp = (respfinal - respinicial))%>%
  mutate(variacao_saneamento = (saneamentofinal - saneamentoinicial))%>%
  #médias e mediana e etc
  select(1,12,13,14,15,16,17)

quedadsei <- variacaodsei%>%
  filter(perc_mortalidade < -37)
write.xlsx(quedadsei, 'C:/Users/Matheus/Desktop/quedadsei.xlsx')

sobedsei <- variacaodsei%>%
  filter(perc_mortalidade > 94)
write.xlsx(sobedsei, 'C:/Users/Matheus/Desktop/sobedsei.xlsx')



#regressao linear do que explica mortalidade total

modeloresp <- lm(variacao_mortalidade ~ variacao_suicidio +variacao_agressao+ variacao_resp+variacao_saneamento ,data = variacaodsei)

summary(modeloresp)










variacaodosdsei2 <- variacaodsei%>%
  filter(regiao== 'Sudeste' | regiao == 'Sul')

# #tentativa de plot 1
# ggplot(variacaodsei, aes(x=dsei,y=variacao_mortalidade, group=1)) + geom_col(size=2) +
#   labs(x = "DSEI", y = "Variação da Mortalidade em %", 
#        title = "Variação da Mortalidade entre 2010-2011 e 2016-2017 por DSEI")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   facet_wrap(~regiao)
# 
# #tentativa de plot 2 - selecionando algumas regioes
# ggplot(variacaodosdsei2, aes(x=variacao_mortalidade,y=dsei, group=1)) + geom_col(size=2) +
#   labs(x = "DSEI", y = "Variação da Mortalidade em %", 
#        title = "Variação da Mortalidade entre 2010-2011 e 2016-2017 por DSEI")+
#   # theme(axis.text.y = element_text(angle = 60, hjust = 1))+
#   facet_wrap(~regiao)
# 
# 
# 
# 
# #grafico 1 se pa - tipos de morte e evolucao durante os anos - talvez usando facet
# ggplot(graficosgeraisano, aes(x=ano,y=mortes_por_ano, group=1)) + geom_line(size=2) +
#   geom_point()+
#   labs(x = "Ano", y = "Número de Mortes Totais no País", 
#        title = "Evolução das Mortes Totais por Ano")
# 
# #evolucao da taxa de mortalidade (media de todos os dsei e facet por dsei)
# 
# ggplot(graficosgeraisano, aes(x=ano,y=tx_mort_dsei_100, group=1)) + geom_line(size=2)  +
#   labs(x = "Ano", y = "Taxa de Mortalidade Média do País", 
#        title = "Evolução Taxa de Mortalidade em todos os distritos, por ano")
# 
# 
#grafico 2 evolucao de mortes por dsei - 1) ao inves de facet talvez tracar varias
#linhas - esse aqui é o maior gráfico que já ta no doc

ggplot(graficosgeraisdsei, aes(x=ano,y= tx_mortalidade_dsei, group=1)) + 
  geom_line(color= 'red3', size= 2) + geom_point(size=4.5, color='red4') +
  labs(x = "Ano", y = "Taxa de Mortalidade a cada 1000 Indígenas",
       title = "Evolução da Taxa de Mortalidade por Ano em Diferentes DSEIs") +
  theme_classic()+theme(axis.text.x = element_text(angle = 50, hjust = 1))+
facet_wrap(~dsei)
# 



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#estatisticas sumarias

#descrevendo para darmos uma olhada
#por ano
describeBy(basecompleta16estats, group = 'ano')

#por dsei
describeBy(basecompleta16estats, group = 'dsei')





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##grafico para os DSEI que saem da estabilidade
#yanomami

graficosgeraisdseixingu <- graficosgeraisdsei%>%
  filter(dsei == 'XINGU')

ggplot(graficosgeraisdseiyanomami, aes(x=ano, group=1)) + 
  geom_line(aes(y = txmresp), color = "cornflowerblue", size=1.2) +
  geom_line(aes(y = txmagressao), color = "darkred", size=1.2) +
  geom_line(aes(y = txmsuicidio), color = "blueviolet", size=1.2) + 
  geom_line(aes(y = txmsaneamento), color="chocolate4", size=1.2) +
  labs(x = "Ano", y = "Taxas de Mortalidade por 100 indígenas", 
       title = "Evolução da taxa de mortalidade de diferentes complicações no DSEI Yanomami") 
  
  

#altamira


graficosgeraisdseialtoriopurus <- graficosgeraisdsei%>%
  filter(dsei == 'ALTO RIO PURUS')
  
ggplot(graficosgeraisdseiriopurus, aes(x=ano, group=1)) + 
  geom_line(aes(y = txmresp), color = "cornflowerblue", size=1.2) +
  geom_line(aes(y = txmagressao), color = "darkred", size=1.2) +
  geom_line(aes(y = txmsuicidio), color = "blueviolet", size=1.2) + 
  geom_line(aes(y = txmsaneamento), color="chocolate4", size=1.2)  +
  labs(x = "Ano", y = "Taxas de Mortalidade por 100 indígenas", 
       title = "Evolução da taxa de mortalidade de diferentes complicações no DSEI Altamira")


#alto rio jurua


graficosgeraisdseiriojurua <- graficosgeraisdsei%>%
  filter(dsei == 'ALTO RIO JURUÁ')

ggplot(graficosgeraisdseiriojurua, aes(x=ano, group=1)) + 
  geom_line(aes(y = txmresp), color = "cornflowerblue", size=1.2) +
  geom_line(aes(y = txmagressao), color = "darkred", size=1.2) +
  geom_line(aes(y = txmsuicidio), color = "blueviolet", size=1.2) + 
  geom_line(aes(y = txmsaneamento), color="chocolate4", size=1.2) +
  labs(x = "Ano", y = "Taxas de Mortalidade por 100 indígenas", 
       title = "Evolução da taxa de mortalidade de diferentes complicações no DSEI Kaiapó do MT")


#medio rio purus


graficosgeraisdseimediopurus <- graficosgeraisdsei%>%
  filter(dsei == 'MÉDIO RIO PURUS')

ggplot(graficosgeraisdseimediopurus, aes(x=ano, group=1)) + 
  geom_line(aes(y = txmresp), color = "cornflowerblue", size=1.2) +
  geom_line(aes(y = txmagressao), color = "darkred", size=1.2) +
  geom_line(aes(y = txmsuicidio), color = "blueviolet", size=1.2) + 
  geom_line(aes(y = txmsaneamento), color="chocolate4", size=1.2) +
  labs(x = "Ano", y = "Taxas de Mortalidade por 100 indígenas", 
       title = "Evolução da taxa de mortalidade de diferentes complicações no DSEI Médio Purus")


#medio solimoes e afluentes

graficosgeraisdseimediosolimoes <- graficosgeraisdsei%>%
  filter(dsei == 'MÉDIO RIO SOLIMÕES E AFLUENTES')

ggplot(graficosgeraisdseimediosolimoes, aes(x=ano, group=1)) + 
  geom_line(aes(y = txmresp), color = "cornflowerblue", size=1.2) +
  geom_line(aes(y = txmagressao), color = "darkred", size=1.2) +
  geom_line(aes(y = txmsuicidio), color = "blueviolet", size=1.2) + 
  geom_line(aes(y = txmsaneamento), color="chocolate4", size=1.2) +
  labs(x = "Ano", y = "Taxas de Mortalidade por 100 indígenas", 
       title = "Evolução da taxa de mortalidade de diferentes complicações no DSEI Médio Solimões")


#medio solimoes e afluentes

graficosgeraisdseiparintins <- graficosgeraisdsei%>%
  filter(dsei == 'PARINTINS')

ggplot(graficosgeraisdseiparintins, aes(x=ano, group=1)) + 
  geom_line(aes(y = txmresp), color = "cornflowerblue", size=1.2) +
  geom_line(aes(y = txmagressao), color = "darkred", size=1.2) +
  geom_line(aes(y = txmsuicidio), color = "blueviolet", size=1.2) + 
  geom_line(aes(y = txmsaneamento), color="chocolate4", size=1.2) +
  labs(x = "Ano", y = "Taxas de Mortalidade por 100 indígenas", 
       title = "Evolução da taxa de mortalidade de diferentes complicações no DSEI Parintins")





#juntando para fazer um facet desses anteriores - em duvida em como fazer

graficosgerais4dsei <- graficosgeraisdseialtamira%>%
  add_row(graficosgeraisdseikaiapomt)%>%
  add_row(graficosgeraisdseikaiapopa)%>%
  add_row(graficosgeraisdseiyanomami)%>%
  #agora criando uma dummy para cada tipo de mortalidade
  mutate(sanenam = if_else())

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#summary statistics

latextable <- describeBy(basecompleta16estats, group = 'ano')

write.xlsx(x = tibble(latextable[['2010']]), 
           file ="C:/Users/Matheus/Desktop/coisasparalatex/latexmorb10.xlsx" )   

write.xlsx(x= tibble(latextable[['2011']]),
           file ="C:/Users/Matheus/Desktop/coisasparalatex/latexmorb11.xlsx" )   

write.xlsx(x=tibble(latextable[['2012']]),
           file ="C:/Users/Matheus/Desktop/coisasparalatex/latexmorb12.xlsx" )   

write.xlsx(x=tibble(latextable[['2013']]),
           file ="C:/Users/Matheus/Desktop/coisasparalatex/latexmorb13.xlsx" )   

write.xlsx(x=tibble(latextable[['2014']]),
           file ="C:/Users/Matheus/Desktop/coisasparalatex/latexmorb14.xlsx" )   

write.xlsx(x=tibble(latextable[['2015']]),
           file ="C:/Users/Matheus/Desktop/coisasparalatex/latexmorb15.xlsx" )   

write.xlsx(x=tibble(latextable[['2016']]),
           file ="C:/Users/Matheus/Desktop/coisasparalatex/latexmorb16.xlsx" )   

write.xlsx(x=tibble(latextable[['2017']]),
           file ="C:/Users/Matheus/Desktop/coisasparalatex/latexmorb17.xlsx" )  


