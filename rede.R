# Carrega pacotes ----
## Instala pacote se nao presente
if (!require(igraph)) {
  install.packages("igraph")
  library(igraph)
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require(arcdiagram)) {
  install.packages("remotes")
  remotes::install_github("gastonstat/arcdiagram")
  library(arcdiagram)
}

# Carrega dados ----
## Dados brutos ----
trello_bruto = read.csv("RedeONI/monitoramentoDemandas.csv", encoding = 'UTF-8')

## Membros dos cards ----
card_membros = trello_bruto$Members

## Loop para extracao das relacoes dos membros do card ----
relacao_bruto = purrr::map_dfr(
  card_membros,
  function(x) {
    y = stringr::str_split(x, ', ')
    y = unlist(y)
    
    z = expand.grid(colaborador_1 = y,
                    colaborador_2 = y)
    z = z[z$colaborador_1 != z$colaborador_2, ]
    
    return(z)
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

## Transforma em rede
network = graph_from_adjacency_matrix(colab, weighted = TRUE, mode = c("undirected"))

layout_grafico = layout_with_dh(network)

# Avaliacao e Visualizacao ----
## Grafico da rede ----

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

## - Gráfico dos 5 colaboradores com mais conexões ----

cols = colSums(colab)

col = data.frame(cols) %>% tibble::rownames_to_column() %>% arrange(-cols)

c = col[1:5, ]

ggplot(c, aes(x = fct_reorder(rowname, cols), y = cols)) +
  geom_col() +
  coord_flip() +
  labs(title = "Quantas conexoes possui cada colaborador?", x = "Conexoes", y = "Colaboradores")

## - Para cada integrante avalie as 3 pessoas que mais interagem com eles ----

# matriz_2x2 = como ela era?
# 
# matriz_2x2
# colaborador_1 x colaborador_2
# quem tem relacao com quem e cada linha so aparece 1x pra cada relacao

valores = seq_along(matriz_2x2$colaborador_1)

for (i in seq_along(matriz_2x2$colaborador_1))  {
  colaborar_1_sel = as.character(matriz_2x2$colaborador_1[i])
  colaborar_2_sel = as.character(matriz_2x2$colaborador_2[i])
  
  y = relacao_contagem[relacao_contagem$colaborador_1 == colaborar_1_sel & relacao_contagem$colaborador_2 == colaborar_2_sel,]
  
  z = y$n
  
  valores[i] = z
  
}

arcplot(as.matrix(matriz_2x2), lwd.arcs = 0.2 * valores, cex.labels=0.7, sorted=TRUE, col.arcs=hsv(runif(9,0.6,0.8),alpha=0.3))

## - Avaliar a distribuição dos integrantes com mais conexões; ----



## Criar medida de centralidade de cada integrante, considerando a rede pura e outra tornando a rede mais esparsa ----
### Rede pura ----



### Rese esparsa ----



## - Gerar avaliacao de clusters da rede, considerando a rede pura e outra tornando a rede mais esparsa ----
### Rede pura ----

cluster = cluster_label_prop(network)

plot(cluster, network)

### Rede esparsa ----




## Qual eh a dispersao de interacoes desta pessoa com outros colaboradores? Quanto mais disperso, menor a proporcao ----

rel = relacao_contagem %>% 
  rename(vinculos = n) %>% 
  group_by(colaborador_1) %>% 
  mutate(prop = vinculos/sum(vinculos)) %>% 
  slice_max(prop, n = 3) %>% 
  summarise(prop = sum(prop))

rela = arrange(rel, prop)

ggplot(rela, aes(x = fct_reorder(colaborador_1, prop), y = prop)) +
  geom_col() +
  coord_flip()

## Quantas vezes essa pessoa eh o elo principal entre outros colaboradores? ----

ranksklva = relacao_contagem %>%
  rename(vinculos = n) %>% 
  group_by(colaborador_1) %>% 
  slice_max(order_by = vinculos, n = 3) %>% 
  ungroup() %>% 
  count(colaborador_2)

ggplot(ranksklva, aes(x = fct_reorder(colaborador_2, n), y = n)) +
  geom_col() +
  coord_flip()

## Relação de Cards Não-Concluidos ----

status_colab = trello_bruto[,c("Members", "List.Name")]

r1 = status_colab %>%
  separate_rows(Members, sep = ",\\s*")
  
r2 = r1 %>%
  group_by(Members, List.Name) %>%
  count()

r3 = r2 %>%
  rename(count = n, status = List.Name, colab = Members) %>%
  filter((colab != "" | is.na(colab)==TRUE) & (status != "" | is.na(status)==TRUE))

nao_concluidos = r3 %>% 
              filter(status != "Concluído")
  


# Testes ----

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
