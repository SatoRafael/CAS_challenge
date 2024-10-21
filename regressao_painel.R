library(plm)
library(sf)
library(spdep)
library(gstat)
library(pglm)
library(spatialreg)
library(splm)
library(fastDummies)
library(dplyr)

gdf <- read.csv('C:\\Users\\Lenovo\\OneDrive\\Área de Trabalho\\Programação\\CAS\\gdf.csv', 
                encoding = 'utf-8')

df_painel <- pdata.frame(gdf, index = c('ID', 'Ano.da.Apólice')) 
colnames(df_painel) <- c('X', 'ID', 'Ano', 'Importancia', 'Premio', 'ID.Evento', 'Mês', 'Duracao_Enchente', 'Severidade', 
                         'Precipitacao', 'Nivel_Agua', 'Valor_Sinistro', 'Latitude', 'Longitude', 'geometry')

df_painel <- fastDummies::dummy_cols(df_painel, select_columns = "Severidade") %>%
             select(-Severidade_NA) %>%
             filter(Valor_Sinistro > 0)
### Referência: Sem severidade

estimador_ols <- Valor_Sinistro ~ #Importancia +
                                     Premio +  
                                     Duracao_Enchente +
                                     #Severidade_2 + 
                                     Severidade_3 +
                                     Severidade_4 +
                                     #Severidade_5 +
                                     Precipitacao  
                                     #Nivel_Agua 


modelo_pool <- plm(estimador_ols, data = df_painel, model = "pooling")                                      
modelo_fixo <- plm(estimador_ols, data = df_painel, model = "within")
modelo_rand <- plm(estimador_ols, data = df_painel, model = "random")

summary(modelo_pool)
  
  
        