library(tidyverse)
library(radiant)
supp2 <- read.csv("SupplDataFile2.csv", stringsAsFactors = FALSE)
head(supp2)

supp2_test <- supp2 %>% recode(GeneGroup, "Nuc-OXPHOS" = "NucOXPHOS")

supp2_test <- supp2 %>% mutate(GeneGroup = recode(GeneGroup, "Nuc-OXPHOS" = "NucOXPHOS"))

supp2_test %>% filter(PAMLmodel == 1 & GeneGroup != "Nuc-Glycolysis") %>%
  pivot_wider(names_from = GeneGroup, values_from = dS) %>%
  group_by(Species.3.letter.codes.follow.KEGG.nomenclature.) %>%
  ggplot(aes(x=Mt/NucOXPHOS, y=omega..dN.dS.)) + geom_point()

#Figure S4. Differences in transcript abundance between 
#mt and N-mt OXPHOS and ribosomal subunits in three different 
#Arabidopsis thaliana datasets. Error bars show ± SEM.

supp1 <- read.csv("SupplDataFile1.csv", stringsAsFactors = FALSE)
head(supp1)
arabidopsis <- c("Arabidopsis_dataset1", "Arabidopsis_dataset2", "Arabidopsis_dataset3")
genes <- c("I", "II", "III", "IV", "RPL", "RPS", "V")
supp1 %>% slice(1:387) %>% filter(Complex %in% genes) %>%
  ggplot(aes(Complex, FPKM, fill = Genome)) + 
  geom_bar(stat="summary", position= "dodge") +
  facet_wrap(~ Species) + 
  scale_y_continuous(trans = "log10", breaks=c(100,1000,10000,100000,1000000)) +
  geom_errorbar(stat="summary", width=0.5, position="dodge") + 
  scale_fill_grey() + 
  theme_bw()


