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
relacao_filtro = dplyr::filter(relacao_contagem, n > 9)

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
matriz_colaboradores = matriz_colaboradores[, 2:30]

## Transforma em formato de matriz
colab = data.matrix(matriz_colaboradores)

## Remove coluna nao numerica (nomes dos colaboradores)
# colab = rbind(colab, c(-1))
# colab = colab[,-1]

## Transforma em rede
network = graph_from_adjacency_matrix(colab, weighted = TRUE, mode = c("undirected"))

layout_grafico = layout_as_star(network)

# Avaliacao e Visualizacao
## Grafico da rede

plot(
  network,
  layout = layout_grafico,
  vertex.size = log(colSums(colab), 1.5),
  edge.arrow.size = 500,
  vertex.label.cex = 0.8,
  vertex.label.color = "black",
  vertex.frame.color = adjustcolor("#1b1740", alpha.f = 700),
  vertex.color = adjustcolor("blue", alpha.f = 0),
  edge.color = adjustcolor("#893395", alpha.f = 0.7),
  display.isolates = FALSE,
  vertex.label = ifelse(colSums(colab) > 100 , colnames(colab), NA)
)

# Clusterizando grupos
## Construcao e Plot dos Clusteres do Observatorio

cluster = cluster_label_prop(network)

plot(cluster, network)

## Testes

# colSums(colab)
# 
# contagem = relacao_contagem %>% 
#   group_by(colaborador_1) %>% 
#   summarise(n = sum(n)) %>% 
#   arrange(-n)
# 
# ggplot(contagem, aes(x = fct_reorder(colaborador_1, n), n)) +
#   geom_col() +
#   coord_flip()

# plot(network, layout= layout_grafico)
