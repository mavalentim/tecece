# CRIANDO UMA BASE NO FORMATO WIDE PARA VARIAS VARIAVEIS DIFERNTES
######################################################################

#11)ABRINDO UMA BASE DE OBITO E ADAPTANDO ELA PRO FORMATO WIDE

obitoind <-
  
  obitoind <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/obitoind.xlsx")



#nome das colunas
names(obitoind) <- c('dsei', 'polobase', 'municipio' , 'ano' , 'cid' , 'causa')



#base intermediaria para reduzir os cids
mortind10a17b <-obitoind%>%
  mutate(cid3 = str_sub(cid, end= 3))%>%
  mutate(cid2 = str_sub(cid, end= 2))%>%
  mutate(cap = str_sub(cid, end= 1))%>%
  mutate(morb = 1)%>%
  mutate(resp = case_when(cap == 'J' ~ 1,
                          cap != 'J' ~ 0))%>%
  #definindo suicidio
  mutate(suicid = case_when(cid2=="X6" | cid2=="X7" | cid2 == "X8" ~ 1,
                            cid2!="X6" | cid2!="X7" | cid2 != "X8"~0))%>%
  #definindo morte por conflito
  mutate(agressao = case_when(cid3 == "X85" |cid3 == "X86" |cid3 == "X87" 
                              |cid3 == "X88" |cid3 == "X89"|
                                cid2 == "X9" | cid2 == "Y0" ~ 1,
                              cid3 != "X85" |cid3 != "X86" |cid3 != "X87"
                              |cid3 != "X88" |cid3 != "X89"|
                                cid2 != "X9" | cid2 != "Y0" ~0 ))%>%
  #definindo mortes por ruins condicoes sanitarias
  mutate(saneamento = case_when(cid2 == 'A0'~ 1, cid2!= 'A0' ~ 0 ))%>%
  ungroup()



#sumarizando para juntar as mortes e tal
mortind10a17c <- mortind10a17b%>%
  #aqui pus para agrupar as mortes por municipio
  group_by(municipio,ano)%>%
  summarize(mort_totais = sum(morb), suicidio = sum(suicid),
            agressao = sum(agressao), resp = sum(resp), saneamento = sum(saneamento))


#para morb_geral
mortind10a17c_geral <- mortind10a17c%>%
  select(1:3)
mortind10a17c_gerals <- spread(mortind10a17c_geral, ano, mort_totais)%>%
  rename(mort_total10 = '2010',mort_total11 = '2011',mort_total12 = '2012',mort_total13 = '2013',
         mort_total14 = '2014',mort_total15 = '2015',mort_total16 = '2016', mort_total17 = '2017')

#para suicidio
mortind10a17c_suicidio<- mortind10a17c%>%
  select(1,2,4)
mortind10a17c_suicidios <- spread(mortind10a17c_suicidio, ano, suicidio)%>%
  rename(suicid10 = '2010',suicid11 = '2011',suicid12 = '2012',suicid13 = '2013',
         suicid14 = '2014',suicid15 = '2015',suicid16 = '2016', suicid17 = '2017')


#para agressao

mortind10a17c_agressao<- mortind10a17c%>%
  select(1,2,5)
mortind10a17c_agressaos <- spread(mortind10a17c_agressao, ano, agressao)%>%
  rename(agressao10 = '2010',agressao11 = '2011',agressao12 = '2012',agressao13 = '2013',
         agressao14 = '2014',agressao15 = '2015',agressao16 = '2016', agressao17 = '2017')


#para respiratorio

mortind10a17c_resp<- mortind10a17c%>%
  select(1,2,6)
mortind10a17c_resps <- spread(mortind10a17c_resp, ano, resp)%>%
  rename(resp10 = '2010',resp11 = '2011',resp12 = '2012',resp13 = '2013',
         resp14 = '2014',resp15 = '2015',resp16 = '2016', resp17 = '2017')


#para saneamento

mortind10a17c_saneamento<- mortind10a17c%>%
  select(1,2,7)
mortind10a17c_saneamentos <- spread(mortind10a17c_saneamento, ano, saneamento)%>%
  rename(saneamento10 = '2010',saneamento11 = '2011',saneamento12 = '2012',saneamento13 = '2013',
         saneamento14 = '2014',saneamento15 = '2015',saneamento16 = '2016', saneamento17 = '2017')


## vamos agora juntar essas bases spread com base em um agregado de valores de municipios que já
#temos 

mortind10a17d <- mortind10a17c_gerals%>%
  left_join(mortind10a17c_suicidios, by = 'municipio' )%>%
  left_join(mortind10a17c_agressaos, by = 'municipio' )%>%
  left_join(mortind10a17c_resps , by = 'municipio')%>%
  left_join(mortind10a17c_saneamentos , by = 'municipio')


#12) COLOCANDO OS CONTROLES JA GERADOS - COMECANDO COM PIB FAZENDO MORTINDA


#IBGE
mortwidea <- mortind10a17d%>%
  left_join(mundsei, by = 'municipio')



#PIB
mortwideb <- mortwidea%>%
  mutate(ibge = as.character(ibge))%>%
  left_join(pib, by ='ibge')

#TERRAS INDIGENAS
mortwidec <- mortwideb%>%
  left_join(munti_g, by = 'ibge')

#INFRAESTRUTURA DO DSEI
saudeinfram <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/saudeinf/saudeinfra.xlsx", 
                          sheet = "polos base")
saudeinfram2 <- saudeinfram%>%
  select(1:9)%>%
  rename(polosbase10 = '2010',polosbase11 = '2011',polosbase12 = '2012',polosbase13 = '2013',
         polosbase14 = '2014',polosbase15 = '2015',polosbase16 = '2016', polosbase17 = '2017')


mortwided <- mortwidec%>%
  left_join(saudeinfram2, by =c('dsei' = 'DSEI'))

#POPULACAO DO DSEI
popwide <- read_excel("C:/Users/Matheus/Desktop/Recurso Demografico 2010-2019.xlsx", 
                      sheet = "DEMOGRAFICO")
popwide_a <- popwide%>%
  rename(pop2010 = '2010', pop2011= '2011', pop2012= '2012',
         pop2013 = '2013', pop2014= '2014', pop2015 = '2015',
         pop2016 = '2016', pop2017= '2017', pop2019 = '2019')


mortwidee <- mortwided%>%
  left_join(popwide_a, by = c('dsei' = 'DSEI'))

#CRIANDO MORTES TOTAIS NO DSEI E PARTICIPACAO DAS MORTES DO MUNICIPIO

baseauxdseimorte <- mortwidee%>%
  group_by(dsei)%>%
  summarize(morttotaldsei10 = sum(mort_total10, na.rm =T ),
            morttotaldsei11 = sum(mort_total11, na.rm =T),
            morttotaldsei12 = sum(mort_total12, na.rm =T), 
            morttotaldsei13 = sum(mort_total13, na.rm =T),
            morttotaldsei14 = sum(mort_total14, na.rm =T),
            morttotaldsei15 = sum(mort_total15, na.rm =T),
            morttotaldsei16 = sum(mort_total16, na.rm =T),
            morttotaldsei17 = sum(mort_total17, na.rm =T),
            #definindo suicidio e recortes totais para o dsei por ano
            suicidtotaldsei16 = sum(suicid16, na.rm =T),
            suicidtotaldsei15 = sum(suicid15, na.rm =T),
            suicidtotaldsei14 =sum(suicid14, na.rm =T),
            saneamtotaldsei16 =sum(saneamento16, na.rm =T),
            saneamtotaldsei15 =sum(saneamento15, na.rm =T),
            saneamtotaldsei14 = sum(saneamento14, na.rm =T),
            resptotaldsei16 =sum(resp16, na.rm =T),
            resptotaldsei15 = sum(resp15, na.rm =T),
            resptotaldsei14 =sum(resp14, na.rm =T),
            agrestotaldsei16 =sum(agressao16, na.rm =T),
            agrestotaldsei15 =sum(agressao15, na.rm =T),
            agrestotaldsei14 = sum(agressao14, na.rm =T))%>%
  #saneamento e outras específicas podem ser adicionadas
  mutate(morttotal13dsei = as.numeric(morttotaldsei13))






#adding total morbidities per district and creating a share of deaths variable
mortwidef <- mortwidee %>%
  left_join(baseauxdseimorte, by= 'dsei')%>%
  #adicionando participacao nas morbidades por municipio - agora será em porcentagem
  mutate(part_nas_morts10 = (mort_total10/morttotaldsei10)*100, 
         part_nas_morts11 = (mort_total11/morttotaldsei11)*100,
         part_nas_morts12 = (mort_total12/morttotaldsei12)*100, 
         part_nas_morts13 = (mort_total13/morttotaldsei13)*100,
         part_nas_morts14 = (mort_total14/morttotaldsei14)*100,
         part_nas_morts15 = (mort_total15/morttotaldsei15)*100,
         part_nas_morts16 = (mort_total16/morttotaldsei16)*100, 
         part_nas_morts17 = (mort_total17/morttotaldsei17)*100,
         #taxa mortalidade dsei - agora por 1000 habitantes
         tx_mortidade_dsei2017 = (mort_total17/pop2017)*1000,
         tx_mortidade_dsei2016 = (mort_total16/pop2016)*1000,
         tx_mortidade_dsei2015 = (mort_total15/pop2015)*1000,
         tx_mortidade_dsei2014 = (mort_total14/pop2014)*1000,
         #taxas especificas de mortalidade dsei - agora por 100 habitantes
         tx_suicidio_dsei2016 = (suicidtotaldsei16/pop2016)*1000,
         tx_suicidio_dsei2015 =  (suicidtotaldsei15/pop2015)*1000,
         tx_saneamento_dsei2016 = (saneamtotaldsei16/pop2016)*1000,
         tx_saneamento_dsei2015 = (saneamtotaldsei15/pop2015)*1000,
         tx_agressao_dsei2016 = (agrestotaldsei16/pop2016)*1000,
         tx_agressao_dsei2015 =(agrestotaldsei15/pop2015)*1000,
         tx_resp_dsei2016 = (resptotaldsei16/pop2016)*1000,
         tx_resp_dsei2015 =   (resptotaldsei15/pop2015)*1000,
         #criando variaveis para esses valores recentes   
         popmedia = (pop2016+pop2017)/2)%>%
  mutate(media_part_mort_recente = (part_nas_morts17+part_nas_morts16+part_nas_morts15)/3,
         tx_mortidade_dseirecente = (tx_mortidade_dsei2016 + tx_mortidade_dsei2015)/2,
         notamedia_ti = mean(c(m_estab_legal, m_integridade_amb, m_integridade_territorial, m_governanciati)), na.rm=T)

#adicionando politica de 2016

mortwideg <- mortwidef%>%
  left_join(poli16, by = 'ibge')%>%
  mutate(muntevecand = if_else(cand_ind > 0,1,0))%>%
  mutate(munelegeucand = if_else(ind_eleito>0,1,0))%>%
  mutate(percentil_eleito = ind_eleito/cand_ind)

# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #MODELOS
# 
# logitwidemort1 <- glm(munelegeucand ~ mort_total17 + pop2017 +
#                      notamedia_ti + anossendorural + situ_ti_mun,
#                      family = binomial(link = "logit"), 
#                      data = mortwideg)
# summary(logitwidemort1)
# 
# 
# #resultado interessante - talvez entre
# logitwidemort2 <- glm(munelegeucand ~ media_part_mort_recente + popmedia +  
#                           anossendorural 
#                        + notamedia_ti + situ_ti_mun,
#                        family = binomial(link = "logit"), 
#                        data = mortwideg)
# summary(logitwidemort2)
# 
# 
# #ADICIONANDO PROPORCAO DE VEREADORES INDIGENAS
# logitwidemort3 <- glm(munelegeucand ~ media_part_mort_recente + popmedia +  
#                         anossendorural 
#                       + notamedia_ti + situ_ti_mun + prop_vereador_ind,
#                       family = binomial(link = "logit"), 
#                       data = mortwideg)
# summary(logitwidemort3)
# 
# 









# # MODELOS COM RECORTES DE MUNICIPIOS - PARECE TER POUCA VARIABIIDADES
# 
# descedsei1 <- mortwideg%>%
#   filter(dsei == 'ALTO RIO JURUÁ' | dsei == 'ALTO RIO PURUS' | dsei == 'XINGU' )
# 
# logitwidemort_especifico1 <- glm(munelegeucand ~ media_part_mort_recente + popmedia +  
#                         anossendorural 
#                       + notamedia_ti + situ_ti_mun,
#                       family = binomial(link = "logit"), 
#                       data = descedsei1)
# summary(logitwidemort_especifico1)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #significativo mas negativo?
# modeloef <- lm(cand_ind ~ part_nas_morts16 + pop2016 +  m_integridade_amb+ 
#                  m_integridade_territorial + anossendorural + m_estab_legal + anoscommtimposto
#                + m_pressao_infra + ibge - 1,data = mortwideg)
# 
# summary(modeloef)
# 
# 
# 
# modeloef <- lm(part_nas_morts17 ~ ind_eleito +   notamedia_ti+ anossendorural  + anoscommtimposto
#                 + dsei - 1,data = mortwideg)
# 
# summary(modeloef)
# 
# 
# 
# #10)IDEIAS
# #10.1) O QUE AJUDA A LEVAR A MORTALIDADE ESPECIFICA
# 
# modeloresp <- lm(txm_resp ~  densidadepopulacional da terra indigena ,data = morbindh)
# 
# summary(modeloresp)
# 
# 
# 
# 
# modelo cardio <- lm(polosbase17 ~  m_integridade_territorial+ m_governanciati +
#                anoscommtimposto + media_part_morb_recente
#              + m_estab_legal + cand_ind ,data = morbindh)
# 
# summary(modelo cardio)
# 
# modelo saneamento <- lm(txm_saneamento ~  CONDICOES AMBIENTAIS + CFEM + m_integridade_territorial+ m_governanciati +
#                           anoscommtimposto + media_part_morb_recente
#                         + m_estab_legal + cand_ind ,data = morbindh)
# 
# modelo suicidio <- instabilidade da terra indigena + ameacas externas + obitos por alcool?
# 
# modelo agressao <- municipios agro + condicao legal + ratio de homem pra mulher do dsei usando consultas de morbidade(talvez homem vao menos as consultas mas pode dar certo
#                                                                                                                       )

#mudando o formato da base só para tentar ver o efeito de cada ano de mortalidade, ver ate onde o efeito vai
#e criar uma base mais organizada para o propensity score matching





# basecompleta16wide <-basecompleta16%>%
#   select(municipio, dsei, mortes_totais, ano.x, m_integridade_amb,  m_integridade_territorial,
#          anossendorural, m_estab_legal, anoscommtimposto, m_governanciati, popind)
#  
# testewide<- dcast(basecompleta16wide, formula =  municipio + mortes_totais +
#                     m_integridade_amb + m_integridade_territorial+ anossendorural+ ~ ano.x)
# 













#9.4) AGLOMERANDO POR DSEI
#qual dsei tem o que

# basepordsei <- basecompleta16%>%
#   group_by(dsei, ano.x)%>%
#   summarize(mortes_totais = sum(mortes_totais), saneamento = sum(saneamento), resp = sum(resp),
#             agressao = sum(agressao), popind = mean(popind), cfem = sum(cfem), 
#             anosimposto = sum(anoscommtimposto), anosrural = sum(anossendorural), polobase = mean(polobase),
#             naldeias = mean(naldeias), netnias = mean(netnias), cand_ind_el = sum(ind_eleito),
#             #parte das terras indigenas presentes em cada dsei, dados da terramais
#             m_estab_legal = mean(`Legal stability`, na.rm = T), 
#             m_integridade_amb = mean(`Environmental integrity`, na.rm = T),
#             m_integridade_territorial = mean(`Territorial integrity`, na.rm = T), 
#             m_pressao_infra = mean(`An absence of pressure from infrastructure projects`, na.rm = T),
#             m_ameaca_infra = mean( `An absence of threats due to infrastructure projects`, na.rm = T),
#             m_governanciati = mean(Governance, na.rm = T))%>%
#   #criando as taxas de mortalidade geral e para os cids selecionados
#   mutate(txm_geral = (mortes_totais/popind)*1000, txm_saneamento = (saneamento/popind)*1000
#          , txm_agressao = (agressao/popind)*1000,
#          txm_resp = (resp/popind)*1000, polobasepormil = (polobase/popind)*1000,
#          tevecand  = if_else (cand_ind_el>0 ,1,0) )

































# basepordseitrienio <- basepordsei%>%
#   filter(ano.x == '2017')
# #filter(ano.x == '2016' | ano.x == '2017' | ano.x == '2018')
# asmortes <- lm(mortes_totais ~ anosrural + anosimposto + polobasepormil + , data = basepordseitrienio)
# 
# summary(asmortes)
# 
# novoprobit2 <- glm(muntevecand ~ txm_geral + polobasepormil + netnias + cfem + anosrural + anosimposto,
#                   family = binomial(link = "robit"), 
#                   data = basepordsei)
# summary(novoprobit)
# 
# 
# 
# 
# 
# 
# #modelo probit com mortes totais/populacao e candidatos eleitos por dsei
# #caso 2016
# 
# 
# 
# novoprobit <- glm(tevecand ~ txm_geral + polobasepormil + netnias + cfem + anosrural + anosimposto,
#                 family = binomial(link = "lobit"), 
#                 data = basepordsei)
# summary(novoprobit)
# 
# 
# 
# #uau
# myprobit <- glm(tevecand ~ mortes_totais + cfem + anossendorural + anoscommtimposto,
#                 family = binomial(link = "probit"), 
#                 data = baseibge16)
# #uau2
# myprobit2 <- glm(tevecand ~ suicidio + cfem + anossendorural + anoscommtimposto,
#                 family = binomial(link = "probit"), 
#                 data = baseibge16)
# 
# 
# summary(myprobit)
# summary(myprobit2)
# 
# 
# #adicionando desmatamento - resulta na baseibge
# 
# #coisas a fazer: primeiro, conseguir infraestrutura de saude indigena que estao nos dados de dsei
# # e vincular de alguma forma dsei a municipio
# 
# #conseguir dados de mortalidade de 2019 e checar para quando nao temos dados de morbidade pois
# #seria bom ter tambem
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(baseibgetestefed, aes(x=proporcao_cand_ind , y=suicidio , group=1)) + geom_point()  +
#   labs(x = "Candidatos Indígenas", y = "Mortes Totais", 
#        title = "candidatos e mortes") + geom_smooth(method = 'lm')
# 
# 
# 
# 






############################
# #Estatisticas descritivas de mortalidade
# 
# mortwideg2 <- mortwideg%>%
#   select(part_nas_morts16, part_nas_morts15, tx_mortidade_dsei2016, tx_mortidade_dsei2015,
#          tx_suicidio_dsei2016, tx_suicidio_dsei2015, tx_agressao_dsei2016, 
#          tx_resp_dsei2016, tx_resp_dsei2015)%>%
#   replace_na(list(part_nas_morts16 = 0, part_nas_morts15 = 0,tx_mortidade_dsei2016=0, tx_mortidade_dsei2015 = 0,
#                   tx_suicidio_dsei2016=0, tx_suicidio_dsei2015 = 0, tx_agressao_dsei2016 = 0, 
#                   tx_resp_dsei2016 = 0 , tx_resp_dsei2015=0))
# 
# 
# write.xlsx(data.frame(describeBy(mortwideg2)), file = "C:/Users/Matheus/Desktop/descritivamort.xlsx") 
# 
