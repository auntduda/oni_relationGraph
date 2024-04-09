# install.packages('igraph')

# entrada = scan()
# entrada2 = readline()

# entrada
# entrada2

library(igraph)

trello = read.csv("RedeONI/monitoramentoDemandas.csv")

observatorio = trello$Members

t = purrr::map_dfr(observatorio, 
                   function(x){
                     y = stringr::str_split(x, ', ')
                     
                     y = unlist(y)
                     
                     z = expand.grid(colaborador_1 = y,
                                     colaborador_2 = y)
                     
                     z = z[z$colaborador_1 != z$colaborador_2,]
                     
                     z
                   })


tt = dplyr::count(t, colaborador_1, colaborador_2)
ttt = dplyr::filter(tt, n > 7)

tttt = ttt %>% 
  dplyr::mutate(n = ifelse(n > 0, 1, 0))
  
matriz_colaboradores =tidyr::pivot_wider(tttt,
                                         names_from = colaborador_2,
                                         values_from = n,
                                         values_fn = ~sum(.x, na.rm = TRUE),
                                         values_fill = 0
)

matriz_colaboradores = matriz_colaboradores[, c('colaborador_1', as.character(matriz_colaboradores$colaborador_1))]

colab = data.matrix(matriz_colaboradores)

#colnames(colab) = rownames(colab) = matriz_colaboradores$colaborador_1

colab = colab[,-1]

network = graph_from_adjacency_matrix(colab)

plot(network)

# graph = layout_in_circle(network)
# plot(network, layout = graph)


# gd = graph(c(observatorio))
# plot(gd)
# 
# gu = graph(c(observatorio), directed=FALSE)
# plot(gu, vertex.label =NA)
