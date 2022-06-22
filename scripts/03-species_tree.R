library(treedataverse)
library(forcats)
library(ggtext)

tree <- read.newick("data/SpeciesTree_rooted_node_labels.txt")

tree$tip.label[5] = "bold(MgalMED)"
tree$tip.label[6] = "bold(MeduEUN)"
tree$tip.label[8] = "bold(MeduEUS)"

lineages = as_factor(c(
	'M. californianus',
	'M. trossulus',
	'M. galloprovincialis Atlantic',
	'M. galloprovincialis Mediterranean Sea',
	'M. galloprovincialis Mediterranean Sea',
	'M. edulis Europe North',
	'M. edulis Europe South',
	'M. edulis Europe South',
	'M. edulis America',
	'M. edulis America',
	'M. coruscus'
))
lineages_2 = fct_collapse(lineages, 'M. edulis Europe'=c('M. edulis Europe North', 'M. edulis Europe South'))
color_pal = c("#999999", "#009E73", "#F0E442", "#D55E00",
							"#0072B2", "#56B4E9", "#CC79A7")
cols = factor(as.integer(lineages_2), labels = color_pal)



p = ggtree(tree) + 
	geom_tiplab(align=T, linesize=.5, offset=.002, parse=T) +
	geom_treescale(x=0, y=10) +
	xlim(c(0,0.1)) +
	geom_tippoint(color=cols, size=4) +
	theme_tree()

# p + geom_text(aes(label=node), hjust=-.3)
# 
# p + geom_highlight(node=20, fill='blue') +
# 	geom_highlight(node=21, fill='gold') +
# 	geom_highlight(node=16, fill='red') +
# 	geom_highlight(node=2, fill='green')

ggsave("manuscript/figures/Fig3_tree.pdf", width = 8, height=4)
