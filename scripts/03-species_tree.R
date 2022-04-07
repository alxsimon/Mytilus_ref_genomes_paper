library(treedataverse)

tree <- read.newick("data/SpeciesTree_rooted_node_labels.txt")

ggtree(tree) + 
	geom_tiplab() +
	geom_treescale(x=0, y=10) +
	xlim(c(0,0.1)) +
	theme_tree()
