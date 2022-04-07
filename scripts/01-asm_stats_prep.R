library(tidyverse)
library(xtable)

asm_stats <- read_csv("data/assembly_stats.csv", col_types = cols())

# name replacements
asm_stats$asm <- str_replace_all(
	asm_stats$asm, 
	c("gallo_v"="MgalMED_v", 
		"edu_v"="MeduEUS_v",
		"tros_v"="MeduEUN_v",
		"coruscus"="Mcor",
		"gallo_GCA"="MgalATL_GCA",
		"edu_GCA"="MeduEUS_GCA",
		"eduam"="MeduAM")
)

write_tsv(asm_stats, "manuscript/supplementary_data/assembly_stats.tsv")

asm_show <- c(
	"Mcor_GCA017311375",
	"MgalATL_GCA900618805",
	"MeduEUS_GCA905397895",
	"MeduAM_GCA019925415",
	"MgalMED_v7",
	"MeduEUS_v7",
	"MeduEUN_v7"
)

asm_stats_table <- asm_stats %>% 
	select(asm,
				 C.L50, C.N50, C.median, C.sequence_count,
				 S.L50, S.N50, S.gc_content, S.median, S.sequence_count, S.total_bps) %>% 
	filter(asm %in% asm_show) %>% 
	mutate(asm = fct_relevel(asm, asm_show)) %>% 
	rename(assembly = asm)
	

tab_caption <- "
Assembly statistics comparisons. C for contigs and S for scaffolds.
"

print(xtable(asm_stats_table,
						 caption = tab_caption,
						 label = "tab:asm_stats",
						 digits = c(0,0,0,0,0,0,0,0,1,0,0,0)),
	booktabs = TRUE,
	caption.placement = "top",
	include.rownames = FALSE,
	scalebox = 0.7,
	file = "manuscript/figures/Tab1_asm_stats.tex"
)
