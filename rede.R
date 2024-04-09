# Carrega pacotes ----
## Instala pacote se nao presente
if (!require(igraph)) {
  install.packages("igraph")
  library(igraph)
}

library(igraph)

# Carrega dados ----
## Dados brutos ----
trello_bruto = read.csv("RedeONI/monitoramentoDemandas.csv")

## Membros dos cards ----
card_membros = trello_bruto$Members

## Loop para extracao das relacoes dos membros do card ----
relacao_bruto = purrr::map_dfr(card_membros, 
                               function(x){
                                 y = stringr::str_split(x, ', ')
                                 
                                 y = unlist(y)
                                 
                                 z = expand.grid(colaborador_1 = y,
                                                 colaborador_2 = y)
                                 
                                 z = z[z$colaborador_1 != z$colaborador_2,]
                                 
                                 z
                               })

# Limpeza dos dados ----
relacao_contagem = dplyr::count(relacao_bruto, colaborador_1, colaborador_2)
relacao_filtro = dplyr::filter(relacao_contagem, n > 0)

relacao_binario = relacao_filtro %>% 
  dplyr::mutate(n = ifelse(n > 0, 1, 0))

## Gera matriz de colaboradores ----
matriz_colaboradores =tidyr::pivot_wider(relacao_filtro,
                                         names_from = colaborador_2,
                                         values_from = n,
                                         values_fn = ~sum(.x, na.rm = TRUE),
                                         values_fill = 0
)

## Ordena matriz 
matriz_colaboradores = matriz_colaboradores[, c('colaborador_1', as.character(matriz_colaboradores$colaborador_1))]

## Transforma em formato de matriz
colab = data.matrix(matriz_colaboradores)

## Remove coluna nao numerica (nomes dos colaboradores
colab = colab[,-1]

## Transforma em rede
network = graph_from_adjacency_matrix(colab, weighted = TRUE, mode = c("undirected"))

layout_grafico = layout_with_fr(network)

# Avaliacao e Visualizacao
## Grafico da rede
plot(network)
plot(network, layout= layout_grafico)

     
