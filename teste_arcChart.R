rel = relacao_contagem %>% rename(vinculos = n) %>% group_by(colaborador_1) %>% mutate(prop = vinculos/sum(vinculos)) %>% slice_max(prop, n = 3) %>% summarise(prop = sum(prop))
rela = arrange(rel, prop)

ggplot(rela, aes(x = fct_reorder(colaborador_1, prop), y = prop)) +
  geom_col() +
  coord_flip()

ranksklva = relacao_contagem %>% rename(vinculos = n) %>% group_by(colaborador_1) %>% slice_max(order_by = vinculos, n = 3) %>% ungroup() %>% count(colaborador_2)

ggplot(ranksklva, aes(x = fct_reorder(colaborador_2, n), y = n)) +
  geom_col() +
  coord_flip()

############################################################

get_top_3 = function(column) {
  top_indices = order(column, decreasing = TRUE)[1:3]
  return(list(values = column[top_indices], indices = top_indices))
}

# Apply the function to each column of the dataframe
top_values = apply(colab, 2, get_top_3)

# Output the results
for (i in 1:ncol(colab)) {
  cat("Person", i, "has interacted:\n")
  for (j in 1:3) {
    cat(top_values[[i]]$values[j], "times with Person", top_values[[i]]$indices[j], "\n")
  }
  cat("\n")
}

###############################################################

edgelist = colnames(colab)

for(i in 1:ncol(colab))
{
  edgelist[i]
  top_values[i]
}

# # Output the results
for (i in 1:ncol(colab)) {
  cat(top_values[i], "has interacted:\n")
  for (j in 1:3) {
    # cat(top_values[[i]]$values[j], "times with Person", top_values[[i]]$indices[j], "\n")
    
  }
  cat("\n")
}

matriz = data.matrix(top_values)

conexoes = graph_from_incidence_matrix(matriz)

plot(conexoes)

# get edgelist - tem que passar um grafo ja
edgelist = get.edgelist(conexoes)

# get vertex labels
vlabels = get.vertex.attribute(conexoes, "colaboradores")

# get vertex groups
vgroups = get.vertex.attribute(conexoes, "grupos?")

# get vertex fill color
vfill = get.vertex.attribute(conexoes, "preenchendo?")

# get vertex border color
vborders = get.vertex.attribute(conexoes, "limites?")

# get vertex degree
degrees = degree(conexoes)

# get edges value
values = get.edge.attribute(conexoes, "conexoes")

# install.packages("reshape")

library(data.table)

library(reshape)

library(tidyverse)

# data frame with vgroups, degree, vlabels and ind
x = data.frame(vgroups, degrees, vlabels, ind=1:vcount(conexoes))

# arranging by vgroups and degrees
y = arrange(x, desc(vgroups), desc(degrees))

# get ordering 'ind'
new_ord = y$ind


arcplot(top_values$, ordering=new_ord, labels=vlabels, cex.labels=0.8,
        show.nodes=TRUE, col.nodes=vborders, bg.nodes=vfill,
        cex.nodes = log(degrees)+0.5, pch.nodes=21,
        lwd.nodes = 2, line=-0.5,
        col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * values)