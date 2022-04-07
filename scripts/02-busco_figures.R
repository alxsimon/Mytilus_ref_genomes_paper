library(tidyverse)
library(patchwork)

busco <- read_tsv("data/busco_summary.tsv", col_types = cols()) %>% 
	mutate(asm = str_replace(asm, "_metazoa_odb10|_mollusca_odb10", ""))

busco$asm <- str_replace_all(
	busco$asm, 
	c("gallo_v"="MgalMED_v", 
		"edu_v"="MeduEUS_v",
		"tros_v"="MeduEUN_v",
		"coruscus"="Mcor",
		"gallo_GCA"="MgalATL_GCA",
		"edu_GCA"="MeduEUS_GCA",
		"eduam"="MeduAM")
)

asm_show <- c(
	"Mcor_GCA017311375",
	"MgalATL_GCA900618805",
	"MeduEUS_GCA905397895",
	"MeduAM_GCA019925415",
	"MgalMED_v7",
	"MeduEUS_v7",
	"MeduEUN_v7"
)

busco <- busco %>% # clean asm names
	mutate(cat = fct_relevel(cat, "M", "F", "CD", "CS")) %>%
	mutate(asm = fct_relevel(asm, asm_show) %>% 
				 	fct_rev()) %>% 
	group_by(asm, db) %>% 
	mutate(proportion = N/cur_data()$N[cur_data()$cat=="T"]) %>% 
	mutate(pct_fmt = paste(format(round(proportion*100, digits = 2)), "%"))


fig_mollusca <- busco %>% 
	filter(db == "mollusca_odb10") %>% 
	filter(cat %in% c("CS", "CD", "F", "M")) %>%
	filter(asm %in% asm_show) %>% 
	ggplot(., aes(x = N, y = asm, fill = cat)) + 
	geom_bar(position = "fill", stat = "identity", width = 0.9) +
	scale_fill_manual(values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73"),
										guide = guide_legend(reverse=T),
										name = "categories", 
										labels = c("Missing", "Fragmented", "Complete duplicated", "Complete single copy")) +
	scale_x_continuous(expand=c(0,0), labels = scales::percent, breaks = c(0,0.2,0.4,0.6,0.8,1)) + 
	scale_y_discrete(expand=c(0,0)) +
	geom_text(aes(label = N), position = position_fill(vjust = .5), size = 3) +
	labs(x = "BUSCO percentage", y = NULL, title = "BUSCO scores Mollusca_odb10 (5295 genes)") +
	theme_minimal() +
	theme(axis.ticks.x = element_line(),
				panel.grid = element_blank(),
				legend.position = "bottom",
				plot.margin = margin(t=5,r=15,b=0,l=5))

fig_metazoa <- busco %>% 
	filter(db == "metazoa_odb10") %>% 
	filter(cat %in% c("CS", "CD", "F", "M")) %>%
	filter(asm %in% asm_show) %>% 
	ggplot(., aes(x = N, y = asm, fill = cat)) + 
	geom_bar(position = "fill", stat = "identity") +
	scale_fill_manual(values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73"),
										guide = guide_legend(reverse=T),
										name = "categories", 
										labels = c("Missing", "Fragmented", "Complete duplicated", "Complete single copy")) +
	scale_x_continuous(expand=c(0,0), labels = scales::percent, breaks = c(0,0.2,0.4,0.6,0.8,1)) + 
	scale_y_discrete(expand=c(0,0)) +
	geom_text(aes(label = N), position = position_fill(vjust = .5), size = 3) +
	labs(x = "BUSCO percentage", y = NULL, title = "BUSCO scores Metazoa_odb10 (954 genes)") +
	theme_minimal() +
	theme(axis.ticks.x = element_line(),
				panel.grid = element_blank(),
				legend.position = "bottom",
				plot.margin = margin(t=5,r=15,b=0,l=5))


Fig1 <- (fig_metazoa + fig_mollusca) + plot_layout(nrow = 2, guides = "collect") & theme(legend.position = "bottom")
ggsave("manuscript/figures/Fig2_busco.pdf", Fig1, width = 12, height = 6)
