##########################
#                        #
#   INICIANDO O MODELO   #
#                        #
##########################
#Adj.matrix é a matriz de adjancência
#bairros é apenas para ter os bairros de acordo
#dados_final é a base de dados para o sexo masculino ou feminino
func_model = function(variavel){
  input = as.list(rep(1,length(bairros)))
  names(input) = bairros
  args = list()
  structure = list()
  
  #Primeiro bloco compartilhado - bloco polinomial
  input_i = input
  input_i$D = c(0.95,0.99)
  input_i$order = 2
  input_i$a1 = 0.013
  input_i$R1 = 1
  input_i$name = "Level"
  structure = append(structure,list(do.call(polynomial_block,input_i)))
  
  #bloco de regressora para IVS ou IDH
  input_i = dados_final %>% dplyr::select(IVS_2010,IDADE,bairros) %>% pivot_wider(names_from = bairros, values_from = IVS_2010) %>%
    arrange(IDADE) %>% dplyr::select(-IDADE) %>% as.list()
  input_i$D = 1
  input_i$name = "IVSglobal"
  structure = append(structure,list(do.call(regression_block,input_i)))
  
  #Bloco GLOBAL referente ao PM2.5
  input_i = dados_final %>% dplyr::select(pm25_p90,IDADE,bairros) %>% pivot_wider(names_from = bairros, values_from = pm25_p90) %>%
    arrange(IDADE) %>% dplyr::select(-IDADE) %>% as.list()
  input_i$D = 0.99
  input_i$name = "PM2.5_media_global"
  structure = append(structure,list(do.call(regression_block,input_i)))
  
  # #Bloco GLOBAL referente a Temperatura (Mínima(noite), Máxima(dia) ou Média) - Mínima no caso
  input_i = dados_final %>% dplyr::select(variavel,IDADE,bairros) %>% pivot_wider(names_from = bairros,
                                                                                  values_from = variavel) %>%
    arrange(IDADE) %>% dplyr::select(-IDADE) %>% as.list()
  input_i$D = 1
  input_i$name = "Temperatura_noite_max_global"
  structure = append(structure,list(do.call(regression_block,input_i)))

  
  # #Bloco GLOBAL referente ao Noise Block (Sobredispersao)
  input_i = input
  input_i$D = 1
  input_i$R1 = 0.002
  input_i$name = "Noiseglobal"
  structure = append(structure,list(do.call(noise_block,input_i)))
  
  for(bairro in bairros){
    input_i = list()
    input_i[[bairro]] = 1
    input_i$D = 0.99
    input_i$name = paste0("Nivel_Local-",bairro)
    structure = append(structure,list(do.call(polynomial_block,input_i)))
    
    args[[paste0("Óbitos_Homem - ",bairro)]] = Poisson(lambda = bairro,data=dados_final$OT_20mais[which(dados_final$bairros==bairro)],offset = dados_final$pop_mean[which(dados_final$bairros==bairro)])
    args[[paste0("Óbitos_Homem - ",bairro)]]$update = update_Poisson_laplace
    args[[paste0("Óbitos_Homem - ",bairro)]]$alt.method=TRUE

  }
  
  structure = do.call(block_superpos,structure)
  indice_car = c()
  
  #Local onde eu estou inserindo a priori car - Nivel Local
  for(name in names(structure$var.names)){
    if(grepl("Nivel_Local",name)){
      indice_car = c(indice_car,as.numeric(structure$var.names[[name]]))
    }
  }
  
  # #Adicionando a priori car na estrutura
  structure = structure %>% CAR_prior(var.index = indice_car,rho=1,scale="tau_nivel",adj.matrix=adj.matrix)
  structure = structure %>% zero_sum_prior(var.index = indice_car)
  args$tau_nivel = 0.05

  #Rodando o modelo
  structure$status = kDGLM:::check.block.status(structure)
  
  args = append(args,list(structure))
  
  #Ajustando o modelo
  modelos_masc = do.call(fit_model,args)
  return(modelos_masc)
}

modelos_masc = func_model("temperatura_noite_max")
summary(modelos_masc)