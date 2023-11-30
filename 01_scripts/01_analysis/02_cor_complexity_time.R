require(tidyverse)
require(readr)
require(ggthemes)

### This part of the code relies on the EUPLEX dataset see:

### Steffen Hurka, Maximilian Haag & Constantin Kaplaner (2022)
### Policy complexity in the European Union, 1993-today: introducing the EUPLEX dataset,
### Journal of European Public Policy, 29:9, 1512-1527, DOI: 10.1080/13501763.2021.1938174

### Link to the data: https://www.euplex.org/data/

euplex <-
  read_csv("https://www.euplex.org/assets/datasets/euplex_dataset_20231102.csv")
load(
  "~/2023-mapping-policy-integration/00_data/ceps_full_mapped_all_versions.Rdata"
)

# Match datasets based on CELEX
ceps_full_mapped_subset_1977_no0 <-
  ceps_full_mapped_subset_1977 %>%
  filter(integration > 0)

ceps_matched <-
  ceps_full_mapped_subset_1977_no0[ceps_full_mapped_subset_1977_no0$CELEX %in%
                                     euplex$e_doc_celexs, ]

# select relevant variables from EUPLEX
match <- euplex %>%
  filter(doc_complete_complexity == T) %>%
  select(
    event,
    e_doc_celexs,
    doc_lix,
    doc_word_entropy_l,
    doc_struct_size_enacting,
    doc_avg_article_depth,
    doc_ref_ext_enacting_rel,
    doc_ref_int_enacting_rel
  )


ceps_matched <- merge(ceps_matched,
                      match,
                      by.x = "CELEX",
                      by.y = "e_doc_celexs",
                      all.x = T)

ceps_matched <- ceps_matched %>%
  filter(!is.na(event))

# Table of correlation
ceps_matched %>%
  summarize(
    `Word Entropy` = round(cor(integration, doc_word_entropy_l), 3),
    `Lix Score` = round(cor(integration, doc_lix), 3),
    `Ext. References` = round(cor(integration, doc_ref_ext_enacting_rel), 3),
    `Int. References` = round(cor(integration, doc_ref_int_enacting_rel), 3),
    `Size` = round(cor(integration, doc_struct_size_enacting), 3),
    `Depth` = round(cor(integration, doc_avg_article_depth), 3)
  ) %>%
  flextable::flextable()

# Create decade variable
ceps_matched <- ceps_matched %>%
  mutate(year = lubridate::year(Date_document),
         decade = year - year %% 10)


ceps_matched$decade_written[ceps_matched$decade == 1990] <-
  "1990 - 1999"
ceps_matched$decade_written[ceps_matched$decade == 2000] <-
  "2000 - 2009"
ceps_matched$decade_written[ceps_matched$decade == 2010] <-
  "2010 - 2019"

# Figure in Appendix of correlation to Word Entropy
set.seed(123)
ceps_matched %>%
  group_by(decade) %>%
  filter(doc_word_entropy_l > 5 & doc_word_entropy_l < 9) %>%
  mutate(correlation = paste("r =", round(
    cor(integration, doc_word_entropy_l, method = "pearson"), 2
  )),
  N = paste("n =", n())) %>%
  ggplot(aes(
    x = doc_word_entropy_l,
    y = integration,
    color = factor(decade),
    fill = factor(decade)
  )) +
  geom_jitter(alpha = .2, size = .6) +
  geom_smooth(method = "lm") +
  ggthemes::theme_base() +
  scale_color_manual(values = c("red", "blue", "black")) +
  scale_fill_manual(values = c("red", "blue", "black")) +
  xlab("Word entropy") +
  ylab("Degree of intersectorality") +
  theme(legend.position = "none",
        plot.background = element_rect(color = "white")) +
  facet_wrap(~ decade_written + correlation + N)

ggsave("02_figures/figureA5.png",
       height = 5,
       width = 8)

# Relative importance over time
ceps_full_mapped_subset_1977_long %>%
  mutate(year = lubridate::year(Date_document)) %>%
  group_by(year, variable) %>%
  count() %>%
  group_by(year) %>%
  mutate(n_relative = n / sum(n)) %>%
  ggplot(aes(x = year, y = n_relative, fill = variable)) +
  geom_area(color = "grey20") +
  ggthemes::theme_base() +
  theme(plot.background = element_rect(color = "white")) +
  scale_fill_viridis_d(option = "magma") +
  theme(legend.title = element_blank()) +
  xlab("Year") +
  ylab("Relative frequency")

ggsave("02_figures/domains_time.pdf",
       height = 5,
       width = 10)


'Äöas<DF GÄ#+
_0K9rm(list = ls())
