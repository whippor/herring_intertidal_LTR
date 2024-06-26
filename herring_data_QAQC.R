#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Herring Data QAQC                                                           ##
# Script created 2024-06-24                                                   ##
# Data source: Terrie Klinger - University of Washington                      ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2024-06-24                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Data tidying of csv data for long-term intertidal plots from the Herring
# Islands in Kachemak Bay, Alaska


# Required Files (check that script is loading latest version):
# 1999.csv : 2022.csv

# Associated Scripts:
# FILE.R

# TO DO 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# RECENT CHANGES TO SCRIPT                                                     +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE DATA                                                              +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(janitor)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data1999 <- data.frame(t(read_csv("data/1999.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 1999) %>%
  clean_names()
data2000 <- data.frame(t(read_csv("data/2000.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2000) %>%
  clean_names()
data2001 <- data.frame(t(read_csv("data/2001.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2001) %>%
  clean_names()
data2002 <- data.frame(t(read_csv("data/2002.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2002) %>%
  clean_names()
data2003 <- data.frame(t(read_csv("data/2003.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  mutate_all(~ replace(., . == "tr", "0.5")) %>%
  replace(is.na(.), 0) %>%
  mutate_all(~ str_replace_all(., "\\*1", "1")) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2003) %>%
  clean_names()
data2004 <- data.frame(t(read_csv("data/2004.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2004) %>%
  clean_names()
data2005 <- data.frame(t(read_csv("data/2005.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2005) %>%
  clean_names()
data2006 <- data.frame(t(read_csv("data/2006.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2006) %>%
  clean_names()
data2007 <- data.frame(t(read_csv("data/2007.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2007) %>%
  clean_names()
data2008 <- data.frame(t(read_csv("data/2008.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2008) %>%
  clean_names()
data2009 <- data.frame(t(read_csv("data/2009.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2009) %>%
  clean_names()
data2010 <- data.frame(t(read_csv("data/2010.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2010) %>%
  clean_names()
data2011 <- data.frame(t(read_csv("data/2011.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2011) %>%
  clean_names()
data2012 <- data.frame(t(read_csv("data/2012.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2012) %>%
  clean_names()
data2013 <- data.frame(t(read_csv("data/2013.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2013) %>%
  clean_names()
data2014 <- data.frame(t(read_csv("data/2014.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2014) %>%
  clean_names()
data2015 <- data.frame(t(read_csv("data/2015.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2015) %>%
  clean_names()
data2016 <- data.frame(t(read_csv("data/2016.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  mutate_all(~ replace(., . == ",5", "0.5")) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2016) %>%
  clean_names()
data2017 <- data.frame(t(read_csv("data/2017.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2017) %>%
  clean_names()
data2018 <- data.frame(t(read_csv("data/2018.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2018) %>%
  clean_names()
data2019 <- data.frame(t(read_csv("data/2019.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2019) %>%
  clean_names()
data2020 <- data.frame(t(read_csv("data/2020.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  replace(is.na(.), 0) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2020) %>%
  clean_names()
data2021 <- data.frame(t(read_csv("data/2021.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2021) %>%
  clean_names()
data2022 <- data.frame(t(read_csv("data/2022.csv"))) %>%
  rownames_to_column() %>%
  row_to_names(row_number = 1) %>%
  replace(is.na(.), 0) %>%
  mutate_at(c(7:ncol(.)), as.numeric) %>%
  filter(rowSums(across(where(is.numeric)))!=0) %>%
  mutate(QUAD = as.character(as.numeric(QUAD))) %>%
  unite("plotID", QUAD, TRIPLET, sep = "_", remove = FALSE) %>%
  mutate(across('plotID', str_replace, "\\..*", "")) %>%
  mutate(year = 2022) %>%
  clean_names()


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



rm(all_data, data_list, all_fucus, dfs, objs, all_mussel, all_barns, test, columnation)
objs <- mget(ls(envir = globalenv()))
dfs <- objs[map_lgl(objs, is.data.frame)]

test <- dfs %>%
  reduce(bind_rows)
columnation <- data.frame(colnames(test))

all_test <- dfs %>%
  reduce(bind_rows) %>%
  replace(is.na(.), 0) %>%
  mutate(across('triplet', str_replace, "\\..*", ""))

write_csv(all_test, "data/all_data_QAQC.csv")

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####


library(viridis)

# test plot, spatial visualization on plot level of fucus

all_test %>%
  mutate(fucus_percent_total = as.numeric(fucus_percent_total)) %>%
  mutate(triplet = factor(triplet, levels = c(1:12))) %>%
  ggplot(aes(x = year, y = fucus_percent_total, fill = triplet,
             color = triplet)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  facet_wrap(.~triplet)

# test block, spatial visualization on plot level of fucus

all_test %>%
  mutate(fucus_percent_total = as.numeric(fucus_percent_total)) %>%
  mutate(triplet = factor(triplet, levels = c(1:12))) %>%
  mutate(block = case_when(triplet %in% c(1:6) ~ 'A',
                           triplet %in% c(7:12) ~ 'B')) %>%
  ggplot(aes(x = year, y = fucus_percent_total, fill = block,
             color = block)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  scale_color_viridis(discrete = TRUE, option = 'magma', 
                      begin = 0.4, end = 0.7) +
  scale_fill_viridis(discrete = TRUE, option = 'magma', 
                     begin = 0.4, end = 0.7) +
  theme_minimal() +
  facet_wrap(.~block)


# test plot, spatial visualization on plot level of mussels

all_test %>%
  mutate(triplet = factor(triplet, levels = c(1:12))) %>%
  ggplot(aes(x = year, y = mytilus, fill = triplet,
             color = triplet)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  facet_wrap(.~triplet)

# test block, spatial visualization on plot level of mussels

all_test %>%
  mutate(triplet = factor(triplet, levels = c(1:12))) %>%
  mutate(block = case_when(triplet %in% c(1:6) ~ 'A',
                           triplet %in% c(7:12) ~ 'B')) %>%
  ggplot(aes(x = year, y = mytilus, fill = block,
             color = block)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  scale_color_viridis(discrete = TRUE, option = 'magma', 
                      begin = 0.4, end = 0.7) +
  scale_fill_viridis(discrete = TRUE, option = 'magma', 
                     begin = 0.4, end = 0.7) +
  theme_minimal() +
  facet_wrap(.~block)





# test plot, spatial visualization on plot level of barnacles

all_test %>%
  mutate(triplet = factor(triplet, levels = c(1:12))) %>%
  ggplot(aes(x = year, y = barnacles, fill = triplet,
             color = triplet)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  facet_wrap(.~triplet)

# test block, spatial visualization on plot level of barnacles

all_test %>%
  mutate(triplet = factor(triplet, levels = c(1:12))) %>%
  mutate(block = case_when(triplet %in% c(1:6) ~ 'A',
                           triplet %in% c(7:12) ~ 'B')) %>%
  ggplot(aes(x = year, y = barnacles, fill = block,
             color = block)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  scale_color_viridis(discrete = TRUE, option = 'magma', 
                      begin = 0.4, end = 0.7) +
  scale_fill_viridis(discrete = TRUE, option = 'magma', 
                     begin = 0.4, end = 0.7) +
  theme_minimal() +
  facet_wrap(.~block)

# identify rare species
 test1 <- all_test %>%
  select_if(is.numeric) %>%
  map_dbl(mean) 
 test1 <- data.frame(test1)
test1 <-  rownames_to_column(test1) %>%
  filter(rowname != 'year')
  ggplot(test1, aes(x = rowname, y = test1)) +
  geom_col()


  # test rare spp
  
  all_test %>%
    select(#fucus_percent_total,
           siphonaria,
           nucella,
           gloiopeltis,
           amphiporus,
           leptasterias,
           soranthera,
           porphyra,
           year,
           plot_id) %>%
 #   mutate(fucus_percent_total = as.numeric(fucus_percent_total)) %>%
    pivot_longer(siphonaria:porphyra)  %>%
    ggplot(aes(x = year, y = value, fill = name,
               color = name, group = name)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
    scale_color_viridis(discrete = TRUE, option = 'magma') +
    scale_fill_viridis(discrete = TRUE, option = 'magma') +
    theme_minimal() +
    facet_wrap(.~plot_id, scales = "free")
  
 #NMDS ALL
  
  alldata_summary <- all_data %>%
    relocate(year, .before = fucus_percent_total) %>%
    group_by(year) %>%
    select(!(c(fucus_number_adults:fucus_sporelings_number,
               antho_artemesia,
               katharina,
               spirorbidae,
               petrocelia,
               ralfsia_hild,
               petrocelis))) %>%
    mutate(across(fucus_percent_total:idotea, as.numeric)) %>%
    summarise(across(fucus_percent_total:idotea, ~mean(.x)))
  
  yearallMDS <- metaMDS(alldata_summary[,2:59], distance = "altGower")
  
  
  # extract the 'points' from the nMDS that you will plot in ggplot2
  yearall_points <- yearallMDS$points
  # turn those plot points into a dataframe that ggplot2 can read
  yearall_points <- data.frame(yearall_points)
  # join your plot points with your summed species observations from each habitat type
  plot_data_tax <- data.frame(yearall_points, alldata_summary[,1])
  
  
  
  # run the ggplot
  ggplot(plot_data_tax, aes(x=MDS1, y=MDS2, 
                            color = year, label = year)) + 
    labs(x = "nMDS1", y = "nMDS2") +
    theme_classic() + 
    geom_point(size =  4) + 
    scale_color_viridis(discrete = FALSE, begin = 0.2, end = 0.9, option = "G", name = "region") +
    geom_text(hjust=0, vjust=-.5) +
    geom_path(arrow = arrow(angle = 15, ends = "last", type = "closed"))
  
  # NMDS BLOCKS
  
   alldata_summary <- all_data %>%
    relocate(year, .before = fucus_percent_total) %>%
    mutate(block = case_when(triplet %in% 1:6 ~ "A",
                             triplet %in% 7:12 ~ "B"),
           .before = year) %>%
    group_by(year, block) %>%
    select(!(c(fucus_number_adults:fucus_sporelings_number,
               antho_artemesia,
               katharina,
               spirorbidae,
               petrocelia,
               ralfsia_hild,
               petrocelis))) %>%
    mutate(across(fucus_percent_total:idotea, as.numeric)) %>%
    summarise(across(fucus_percent_total:idotea, ~mean(.x)))
  
  yearallMDS <- metaMDS(alldata_summary[,3:60], distance = "altGower")
  
  
  # extract the 'points' from the nMDS that you will plot in ggplot2
  yearall_points <- yearallMDS$points
  # turn those plot points into a dataframe that ggplot2 can read
  yearall_points <- data.frame(yearall_points)
  # join your plot points with your summed species observations from each habitat type
  plot_data_tax <- data.frame(yearall_points, alldata_summary[,1:2])
  
  
  
  # run the ggplot
  ggplot(plot_data_tax, aes(x=MDS1, y=MDS2, 
                            color = block, label = year)) + 
    labs(x = "nMDS1", y = "nMDS2") +
    theme_classic() + 
    geom_point(size =  4) + 
    scale_color_viridis(discrete = TRUE, begin = 0.4, end = 0.7, option = "G", name = "region") +
    geom_text(hjust=0, vjust=-.5) +
    geom_path(arrow = arrow(angle = 15, ends = "last", type = "closed"))

