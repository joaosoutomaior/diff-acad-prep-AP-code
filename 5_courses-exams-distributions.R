#==============================================================
# File description
#==============================================================
# contents: 
#  distribution of number of courses and exams taken
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#==============================================================
# Set up
#==============================================================

rm(list=ls())
# usual header
source("header_basic.R")
source("header_plotting-functions.R")

# load data
load(paste0(DOE_wd,"/dataset_final.Rdata"))

# ---> Questions to address: 
#      Check on alternative operationalization of AP success.
#      What kind of AP courses do students take? How many? How does that vary across racial groups?
#      How many exams do students take? How does that vary across racial groups?

# data
dat_all <- dat_all %>%
  mutate(AP_n_courses_calc = AP_n_courses_calc_09 + AP_n_courses_calc_10 + AP_n_courses_calc_11 + AP_n_courses_calc_12,
         AP_n_courses_stats = AP_n_courses_stats_09 + AP_n_courses_stats_10 + AP_n_courses_stats_11 + AP_n_courses_stats_12,
         AP_n_courses_tech = AP_n_courses_tech_09 + AP_n_courses_tech_10 + AP_n_courses_tech_11 + AP_n_courses_tech_12)

#==============================================================
# Two samples of interest
#==============================================================

# AP takers
dat_all_ap_takers <- dat_all %>%
  filter(AP_math_enroll ==  1)

# AP takers and exam takers
dat_all_exam_takers <- dat_all %>%
  filter(AP_math_enroll ==  1,
         AP_math_exam == 1)

# Drop race = other from both samples
dat_all_ap_takers <- dat_all_ap_takers %>%
  filter(race != "Other")
dat_all_ap_takers$race <- droplevels(dat_all_ap_takers$race)

dat_all_exam_takers <- dat_all_exam_takers %>%
  filter(race != "Other")
dat_all_exam_takers$race <- droplevels(dat_all_exam_takers$race)

#==============================================================
# Distribution of number of exams taken
#==============================================================

# ------------- ap takers

comp = dat_all_ap_takers %>%
  select(AP_math_exam_n_total, race) %>%
  group_by(race, AP_math_exam_n_total) %>%
  summarise(N = n()) %>%
  mutate(value = 100 * prop.table(N))

# print results
# ---> do not print because of small cell values
# sink("output/n-exams-dist_ap-takers.txt")
# print(comp, n = Inf)
# sink()

p <- comp %>%
  ggplot() +
  geom_bar(stat="identity", 
           aes(x = AP_math_exam_n_total, y = value),
           fill =  grayfill,
           color = graycolor) +
  scale_y_continuous(' ', limits=c(0,100), 
                     labels = function(x) paste0(x, "%"),
                     expand = c(0,0.01)) +
  scale_x_continuous(name ="Total number of AP math exams taken")+
  ggtheme +
  theme(legend.position="top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid")) +
  facet_wrap(~race)
p
ggsave("figures/fig_exams-distribution.png", width = 20, height = 20, units = "cm")

# ------------- exam takers

dat_all_exam <- dat_all_exam_takers %>%
  mutate(AP_math_pass_exam_n = AP_math_pass_exam_n_09 + 
           AP_math_pass_exam_n_10 + 
           AP_math_pass_exam_n_11 +
           AP_math_pass_exam_n_12) %>%
  filter(AP_math_exam == 1)

comp = dat_all_exam %>%
  select(AP_math_pass_exam_n, race) %>%
  group_by(race, AP_math_pass_exam_n) %>%
  summarise(N = n()) %>%
  mutate(value = 100 * prop.table(N))

# print results
# ---> do not print because of small cell values
# sink("output/n-exams-dist_exam-takers.txt")
# print(comp, n = Inf)
# sink()

#==============================================================
# Distribution of number of courses taken
#==============================================================

# ------------- ap takers

comp = dat_all_ap_takers %>%
  select(AP_n_courses_total, race) %>%
  group_by(race, AP_n_courses_total) %>%
  summarise(N = n()) %>%
  mutate(value = 100 * prop.table(N))

# print results
# ---> do not print because of small cell values
# sink("output/n-courses-dist_ap-takers.txt")
# print(comp, n = Inf)
# sink()

p <- comp %>%
  filter(AP_n_courses_total <= 7) %>%
  ggplot() +
  geom_bar(stat="identity", 
           aes(x = AP_n_courses_total, y = value),
           fill =  grayfill,
           color = graycolor) +
  scale_y_continuous(' ', limits=c(0,100), 
                     labels = function(x) paste0(x, "%"),
                     expand = c(0,0.01)) +
  scale_x_continuous(name ="Number of AP math courses taken")+
  ggtheme +
  theme(legend.position="top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid")) +
  facet_wrap(~race)
p
ggsave("figures/fig_courses-distribution.png", width = 20, height = 20, units = "cm")

# ------------- exam takers

comp = dat_all_exam_takers %>%
  select(AP_n_courses_total, race) %>%
  group_by(race, AP_n_courses_total) %>%
  summarise(N = n()) %>%
  mutate(value = 100 * prop.table(N))

# print results
# ---> do not print because of small cell values
# sink("output/n-courses-dist_exam-takers.txt")
# print(comp, n = Inf)
# sink()

