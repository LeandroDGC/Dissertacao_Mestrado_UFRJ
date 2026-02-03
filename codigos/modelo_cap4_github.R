######################
#                    #
# INICIANDO O MODELO #
#                    #
######################
#Adj.matrix é a matriz de adjancência
#municipios é apenas para ter os municipios de acordo
#dados_final é a base de dados
args = list()
structure = polynomial_block(V = 1, D = 0.999, order = 1)

input = as.list(rep(1,length(municipios)))
names(input) = paste0("PM25.",municipios)

#Bloco polinomial para a media
input$D = 0.999
input$order = 1
structure = structure + do.call(polynomial_block,input)


#bloco harmonico anual
input$D = 0.99
input$order = 1
input$period = 365.25636
input$name = "Sazonalidade_anual"
structure = structure + do.call(harmonic_block,input)


#bloco harmonico semanal
input$D = 0.99
input$order = 1
input$period = 7
input$name = "Sazonalidade_semanal"
structure = structure + do.call(harmonic_block,input)


#Bloco da regressora igual para todos TEMPERATURA
input = dados_final %>%
  dplyr::select(temp_prad,DT_INTER,municipio) %>%
  pivot_wider(names_from = municipio,values_from = temp_prad) %>%
  arrange(DT_INTER) %>% dplyr::select(-DT_INTER) %>% as.list()
input$D = 0.96
input$name = "Temperaturaglobal"
structure = structure + do.call(regression_block,input) %>% block_rename(.,paste0("PM25.",.$pred.names))

# #Bloco da regressora igual para todos PRECIPITACAO INDICADORA
input = dados_final %>% 
  dplyr::select(rain,DT_INTER,municipio) %>%
  pivot_wider(names_from = municipio,values_from = rain) %>%
  arrange(DT_INTER) %>% dplyr::select(-DT_INTER) %>% as.list()
input$D = 0.97
input$name = "rainglobal"
structure = structure + do.call(regression_block,input) %>% block_rename(.,paste0("PM25.",.$pred.names))


#FUNÇÃO DE TRANSFERÊNCIA VELOCIDADE DO VENTO
input = as.list(rep(1,length(municipios)))
names(input) = paste0("PM25.",sort(municipios))

input$pulse = dados_final %>%
  dplyr::select(vento_velocidade_prad,DT_INTER,municipio) %>%
  pivot_wider(names_from = municipio,values_from = vento_velocidade_prad) %>%
  arrange(DT_INTER) %>% dplyr::select(-DT_INTER) %>% as.matrix()
input$multi.states = TRUE
input$order = 1
input$noise.disc = 1
input$name = "TF_ventoglobal"
structure = structure + do.call(TF_block,input) 


convert_NG_Normal = kDGLM:::convert_NG_Normal
convert_Normal_NG = kDGLM:::convert_Normal_NG
update_NG = kDGLM:::update_NG
generic_smoother = kDGLM:::generic_smoother
format_ft = kDGLM:::format_ft
format_param = kDGLM:::format_param
generic_param_names = kDGLM:::generic_param_names
any_na = kDGLM:::any_na
check.block.status = kDGLM:::check.block.status

structure = list(structure)

for (i in municipios) {
  data = log(dados_final$pm25_ugm3[which(dados_final$municipio==i)])
  label = paste0("PM25.",i)
  structure = append(structure,list((polynomial_block(mu = 1,R1 = 9, D = 1, order = 1,name=paste0("trend.",label))) %>% #+
                                      block_rename(label)))
}

structure = do.call(block_superpos,structure)

coef.names <- rep(NA, structure$n)
for (name in names(structure$var.names)) {
  if (length(names(structure$var.names[[name]])) > 1) {
    coef.names[structure$var.names[[name]]] <- paste0(name, ".", names(structure$var.names[[name]]))
  } else {
    coef.names[structure$var.names[[name]]] <- name
  }
}
structure$var.labels <- coef.names 

structure = structure %>% 
  CAR_prior(.,scale="tau_level",rho=1,adj.matrix=adj.matrix,var.index=which(grepl("trend",.$var.labels))) %>% 
  zero_sum_prior(.,var.index=which(grepl("trend",.$var.labels)))
args$tau_level = 0.26

args$structure = structure

for (i in municipios) {
  data = log(dados_final$pm25_ugm3[which(dados_final$municipio==i)])#[1:1458]
  label = paste0("PM25.",i)
  outcome = Normal(mu = label , V = "V", data = data)
  
  args[[label]] = outcome
}
system.time({
  fitted.model = do.call(fit_model,args)
})
fitted.model = do.call(fit_model,args)
summary(fitted.model)