library(ggplot2)
library(tidyverse)
library(broom)
library(car)
library(dplyr)
library(modelr)
library(haven)
library(forcats)
library(gt)
library(tibble)
library(tidyr)
library(gtsummary)
library(magrittr)
library(modelsummary)
library(fixest)
library(robust)
library(fit.models)
library(patchwork)


### Creating the Main Dataframe ###
italy <- read_dta("data/Replication_Dataset.dta")

### Giving Ranks to Categorical Variables ###

df <- italy |>
  mutate(rank_edu = case_when(education_level_it_original < 7 ~ 1,
                             education_level_it_original == 7 |education_level_it_original == 9 ~ 2,
                             education_level_it_original == 8 | education_level_it_original == 10 | education_level_it_original == 11 | education_level_it_original == 12 ~ 3,
                             education_level_it_original == 13 | education_level_it_original == 14 ~ 4),
         rank_inc=case_when(profile_gross_personal_eu == 1 | profile_gross_personal_eu == 2 | profile_gross_personal_eu == 3 ~ 1,
                            profile_gross_personal_eu==4 | profile_gross_personal_eu==5 | profile_gross_personal_eu == 6 ~ 2,
                            profile_gross_personal_eu==7 | profile_gross_personal_eu==8 | profile_gross_personal_eu == 9 ~ 3,
                            profile_gross_personal_eu==10| profile_gross_personal_eu==11| profile_gross_personal_eu == 12 ~ 4,
                            profile_gross_personal_eu==13| profile_gross_personal_eu==14 ~ 5,
                            profile_gross_personal_eu==98| profile_gross_personal_eu==99 ~ 6))


### For factor variables ###
df <- df |>
  mutate(age_cat = factor(case_when(
      age <= 24 ~ "18-24",
      age >= 25 & age <= 34 ~ "25-34",
      age >= 35 & age <= 44 ~ "35-44",
      age >= 45 & age <= 54 ~ "45-54",
      age >= 55 ~ "55+")),
      gender = factor(case_when(
        female == 1 ~ "Female",
        female == 0 ~ "Male")),
      cat_education = factor(case_when(
        EDU1 == 1 ~ "High School Diploma",
        EDU2 == 1 ~ "Bachelors",
        EDU3 == 1 ~ "MA or Higher",
        EDU4 == 1 ~ "Unknown")),
      cat_income = factor(case_when(
        profile_gross_personal_eu == 1 ~ "Less than 15,000€ per year",
        profile_gross_personal_eu == 2 ~ "Less than 15,000€ per year",
        profile_gross_personal_eu == 3 ~ "Less than 15,000€ per year",
        profile_gross_personal_eu == 4 ~ "From 15,000€ to 29,999€ per year",
        profile_gross_personal_eu == 5 ~ "From 15,000€ to 29,999€ per year",
        profile_gross_personal_eu == 6 ~ "From 15,000€ to 29,999€ per year",
        profile_gross_personal_eu == 7 ~ "From 30,000€ to 44,999€ per year",
        profile_gross_personal_eu == 8 ~ "From 30,000€ to 44,999€ per year",
        profile_gross_personal_eu == 9 ~ "From 30,000€ to 44,999€ per year",
        profile_gross_personal_eu == 10 ~ "From 45,000€ to 69,999€ per year",
        profile_gross_personal_eu == 11 ~ "From 45,000€ to 69,999€ per year",
        profile_gross_personal_eu == 12 ~ "From 45,000€ to 69,999€ per year",
        profile_gross_personal_eu == 13 ~ "Above 70,000€ per year",
        profile_gross_personal_eu == 14 ~ "Above 70,000€ per year",
        profile_gross_personal_eu == 98 ~ "No Answer/DK",
        profile_gross_personal_eu == 99 ~ "No Answer/DK")),
      engine_cat = factor(case_when(
        dummy_euro_4==1 & dummy_diesel==1~"Diesel Euro 4",
        dummy_euro_5==1 & dummy_diesel==1~"Diesel Euro 5",
        dummy_euro_4==1 & dummy_petrol==1~"Petrol Euro 4",
        dummy_euro_5==1 & dummy_petrol==1~"Petrol Euro 5")
      ))

### Separate Age Table ###
age_tab <- df |>
  group_by(engine_cat,age_cat) |>
  summarise(n= n()) |>
  mutate (proportion = 100 * n/ sum(n)) |>
  pivot_wider(names_from = engine_cat,
              values_from = proportion, 
              id_cols = age_cat,
              values_fill = 0) |>
  mutate(across(starts_with("Diesel") | starts_with("Petrol"), ~ round(., 2))) |>
  rename(Category = age_cat)
  
age_tab <- age_tab[, !colnames(age_tab) %in% "NA"]


### Separate Gender Table ###
gen_tab <- df |>
  group_by(engine_cat, gender) |>
  summarise(n= n()) |>
  mutate (proportion = 100 * n/ sum(n), decimal = 2) |>
  pivot_wider(names_from = engine_cat,
              values_from = proportion, 
              id_cols = gender,
              values_fill = 0) |>
  mutate(across(starts_with("Diesel") | starts_with("Petrol"), ~ round(., 2))) |>
  rename(Category = gender)

gen_tab <- gen_tab[, !colnames(gen_tab) %in% "NA"]


### Separate Education Table ###
edu_tab <- df |>
  group_by(engine_cat, cat_education) |>
  summarise(n= n()) |>
  mutate (proportion = 100 * n/ sum(n), decimal = 2) |>
  pivot_wider(names_from = engine_cat,
              values_from = proportion, 
              id_cols = cat_education,
              values_fill = 0) |>
  mutate(across(starts_with("Diesel") | starts_with("Petrol"), ~ round(., 2))) |>
  rename(Category = cat_education)

edu_tab <- edu_tab[, !colnames(edu_tab) %in% "NA"]

      
### Separate Income Table ###
inc_tab <- df |>
  group_by(engine_cat, cat_income) |>
  summarise(n = n()) |>
  mutate (proportion = 100 * n/ sum(n), decimal = 2) |>
  pivot_wider(names_from = engine_cat,
              values_from = proportion, 
              id_cols = cat_income,
              values_fill = 0) |>
  mutate(across(starts_with("Diesel") | starts_with("Petrol"), ~ round(., 2))) |>
  rename(Category = cat_income)

inc_tab <- inc_tab[, !colnames(inc_tab) %in% "NA"]

### Combining the above tables for a Summary Statistics Table ###

stacked_table <- rbind(age_tab, gen_tab, edu_tab, inc_tab) |>
  mutate(name = case_when(
    Category %in% c("18-24", "25-34", "35-44", "45-54", "55+") ~ "Age",
    Category %in% c("Male", "Female") ~ "Gender",
    Category %in% c("Bachelors", "High School Diploma", "MA or Higher", "Unknown") ~ "Education",
    Category %in% c("Above 70,000€ per year", "From 15,000€ to 29,999€ per year",
                    "From 30,000€ to 44,999€ per year", "From 45,000€ to 69,999€ per year",
                    "Less than 15,000€ per year", "No Answer/DK") ~ "Income"
    )
  )

gt_stack <- stacked_table |>
  gt(rowname_col = "category", groupname_col = "name") |>
  tab_header(title = "Summary Statistics (Proportion of Data in Each Category)")

### Grouping Age Variable ###
df <- df |>
  mutate(young45 = case_when(age <= 45 ~ 1,
                           age > 45 ~ 0),
         old45 = case_when(age <= 45 ~ 0,
                         age > 45 ~1))

df <- df |>
  mutate(low_edu = case_when(EDU1 == 1 ~ 1, TRUE ~ 0),
         high_edu = case_when(EDU2 == 1 | EDU3 ==1 ~ 1,
                              TRUE ~ 0))
                     


### Table 2 Replication ###

### Vote for Lega in Absolute Terms ###

### Keeping data relevant to only 4 car categories- Diesel euro 4 and 5, petrol euro 4 & 5 ###
df_reg1 <- df |>
  filter(target!=3 & target!=4 & no_answer_euro==0)

### Replicating Table 1 Reg 1 ###
tab1_reg1 <- lm(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + diesel_euro4, df_reg1) |>
  modelsummary()

###
fe <- df_reg1 |> 
  select(starts_with("EDU"),
         starts_with("INC"),
         -starts_with("education_level")) |> 
  names()

reg_form <- as.formula(
  paste0("vote_lega_euro ~ dummy_diesel + dummy_euro_4 + diesel_euro4 + age + female |", 
         paste(fe, collapse = " + ")))

tab1_reg2 <- feols(reg_form, df_reg1) |>
  modelsummary(gof_map = c("nobs"))

###
df_reg3 <- df |>
  filter(target!= 3 & no_answer_euro == 0)

reg_form_3 <- as.formula(
  paste0("vote_lega_euro ~ dummy_diesel_ass + dummy_euro_4_ass + diesel_euro4_ass + dummy_car_unknown + age + female |", 
         paste(fe, collapse = " + ")))

tab1_reg3 <- feols(reg_form_3, df_reg3) |>
  modelsummary(gof_map = c("nobs"))

###
df_reg4 <- df |>
  filter(target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0)

reg_form4 <- as.formula(
  paste0("vote_lega_euro ~ dummy_diesel + dummy_euro_4 + diesel_euro4 + age + female + vote_lega_2018|", 
         paste(fe, collapse = " + ")))

tab1_reg4 <- feols(reg_form4, df_reg4) |>
  modelsummary(gof_map = c("nobs"))

###
df_reg5 <- df |>
  filter(target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0)

reg_form5 <- as.formula(
  paste0("vote_lega_euro ~ dummy_diesel + dummy_euro_4 + diesel_euro4 + age + female + vote_lega_regional|", 
         paste(fe, collapse = " + ")))

tab1_reg5 <- feols(reg_form5, df_reg5) |>
  modelsummary(gof_map = c("nobs"))


###
df_reg6 <- df |>
  filter(target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0)

reg_form6 <- as.formula(
  paste0("vote_lega_euro ~ dummy_diesel + dummy_euro_4 + diesel_euro4 + age + female + vote_lega_municipal|", 
         paste(fe, collapse = " + ")))

tab1_reg6 <- feols(reg_form6, df_reg6) |>
  modelsummary(gof_map = c("nobs"))


#################################################################################################################
### Replicating Figure 4 Coefficient Plot for Legislative ###
#################################################################################################################

df_fig1a <- df |>
  filter(target!=3 & target!=4 & no_answer_euro == 0 & no_answer_2018 == 0)

fig1a_reg <- lm(sw_to_lega_18_19 ~ dummy_diesel + dummy_euro_4 + diesel_euro4, df_fig1a) |>
  tidy(vcov = sandwich::vcovHC(type = "HC1"))

reg_form1b <- as.formula(
  paste0("sw_to_lega_18_19 ~ dummy_diesel + dummy_euro_4 + diesel_euro4 + age + female |", 
         paste(fe, collapse = " + ")))

fig1b_reg <- feols(reg_form1b, df_fig1a) |>
  tidy(vcov = sandwich::vcovHC(type = "HC1"))

df_fig1c <- df |>
  filter(target!=3 & no_answer_euro == 0 & no_answer_2018 == 0)

reg_form1c <- as.formula(
  paste0("sw_to_lega_18_19 ~ dummy_diesel_ass + dummy_euro_4_ass + diesel_euro4_ass + age + female + dummy_car_unknown|", 
         paste(fe, collapse = " + ")))

fig1c_reg <- feols(reg_form1c, df_fig1c) |>
  tidy(vcov = sandwich::vcovHC(type = "HC1"))


model_names <- c("No Controls", "With Controls", "Non-Categorized Cars")

panel_a_bind <- bind_rows(fig1a_reg, fig1b_reg, fig1c_reg, .id = "Model")

panel_a_select <- subset(panel_a_bind, term =="dummy_diesel"|term =="dummy_euro_4"|term =="diesel_euro4"|
                                 term == "dummy_diesel_ass"|term =="dummy_euro_4_ass"|term =="diesel_euro4_ass")

panel_a_select <- panel_a_select |>
  mutate(term2 = case_when(term=="dummy_diesel" | term=="dummy_diesel_ass" ~ "Diesel",
                           term=="dummy_euro_4" | term=="dummy_euro_4_ass" ~ "Euro 4",
                           term=="diesel_euro4" | term=="diesel_euro4_ass" ~ "Diesel Euro 4"))

coeff_plot_1 <- 
  ggplot(panel_a_select, aes(term2, estimate,colour=term2, fill=term2)) +
  scale_fill_manual(values = c("grey","darkred","grey"))+
  scale_colour_manual(values = c("grey","darkred","grey"))+
  geom_hline(yintercept=0, linetype="longdash", lwd=0.9, size=2, 
             colour = "lightgrey", alpha=1) +
  geom_errorbar(stat = "identity", alpha = 1, 
                position = position_dodge(width = 0.15),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.4, width=0) +
  geom_point(stat = "identity", alpha = 1,  size = 3, 
             position = position_dodge(width = 0.15)) + coord_flip() +
  facet_wrap(~Model) +
  theme_minimal() + theme(legend.position = "none", 
                        plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=14.5),
                        axis.text.y=element_text(size=14.5),
                        strip.text.x = element_text(size = 14.5),
                        panel.spacing = unit(1.7, "lines"),
                        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = " ", title="Legislative Elections",  
       colour="", fill="", shape="", group="" )


#################################################################################################################
### Replicating Figure 4 Coefficient Plot for Regional ###
#################################################################################################################

df_fig2a <- df |>
  filter(target!=3 & target!=4 & no_answer_euro == 0 & no_answer_regional == 0)

fig2a_reg <- lm(sw_to_lega_reg_19 ~ dummy_diesel + dummy_euro_4 + diesel_euro4, df_fig2a) |>
  tidy(vcov = sandwich::vcovHC(type = "HC1"))

reg_form2b <- as.formula(
  paste0("sw_to_lega_reg_19 ~ dummy_diesel + dummy_euro_4 + diesel_euro4 + age + female |", 
         paste(fe, collapse = " + ")))

fig2b_reg <- feols(reg_form2b, df_fig2a) |>
  tidy(vcov = sandwich::vcovHC(type = "HC1"))

df_fig2c <- df |>
  filter(target!=3 & no_answer_euro == 0 & no_answer_regional == 0)

reg_form2c <- as.formula(
  paste0("sw_to_lega_reg_19 ~ dummy_diesel_ass + dummy_euro_4_ass + diesel_euro4_ass + age + female + dummy_car_unknown|", 
         paste(fe, collapse = " + ")))

fig2c_reg <- feols(reg_form2c, df_fig2c) |>
  tidy(vcov = sandwich::vcovHC(type = "HC1"))


panel_2a_bind <- bind_rows(fig2a_reg, fig2b_reg, fig2c_reg, .id = "Model")

panel_2a_select <- subset(panel_2a_bind, term =="dummy_diesel"|term =="dummy_euro_4"|term =="diesel_euro4"|
                           term == "dummy_diesel_ass"|term =="dummy_euro_4_ass"|term =="diesel_euro4_ass")

panel_2a_select <- panel_2a_select |>
  mutate(term2 = case_when(term=="dummy_diesel" | term=="dummy_diesel_ass" ~ "Diesel",
                           term=="dummy_euro_4" | term=="dummy_euro_4_ass" ~ "Euro 4",
                           term=="diesel_euro4" | term=="diesel_euro4_ass" ~ "Diesel Euro 4"))

coeff_plot_2 <- 
  ggplot(panel_2a_select, aes(term2, estimate,colour=term2, fill=term2)) +
  scale_fill_manual(values = c("grey","darkred","grey"))+
  scale_colour_manual(values = c("grey","darkred","grey"))+
  geom_hline(yintercept=0, linetype="longdash", lwd=0.9, size=2, 
             colour = "lightgrey", alpha=1) +
  geom_errorbar(stat = "identity", alpha = 1, 
                position = position_dodge(width = 0.15),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.4, width=0) +
  geom_point(stat = "identity", alpha = 1,  size = 3, 
             position = position_dodge(width = 0.15)) + coord_flip() +facet_wrap(~Model)+
  theme_minimal()+theme(legend.position = "none", 
                        plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=14.5),
                        axis.text.y=element_text(size=14.5),
                        strip.text.x = element_text(size = 14.5),
                        panel.spacing = unit(1.7, "lines"),
                        plot.title = element_text(hjust = 0.5))+
  labs(x = "", y = " ", title="Regional Elections",  
       colour="", fill="", shape="", group="" ) 



#################################################################################################################
### Replicating Figure 4 Coefficient Plot for Municipal ###
#################################################################################################################

df_fig3a <- df |>
  filter(target!=3 & target!=4 & no_answer_euro == 0 & no_answer_municipal == 0)

fig3a_reg <- lm(sw_to_lega_16_19 ~ dummy_diesel + dummy_euro_4 + diesel_euro4, df_fig3a) |>
  tidy()

reg_form3b <- as.formula(
  paste0("sw_to_lega_16_19 ~ dummy_diesel + dummy_euro_4 + diesel_euro4 + age + female |", 
         paste(fe, collapse = " + ")))

fig3b_reg <- feols(reg_form3b, df_fig3a) |>
  tidy()

df_fig3c <- df |>
  filter(target!=3 & no_answer_euro == 0 & no_answer_municipal == 0)

reg_form3c <- as.formula(
  paste0("sw_to_lega_16_19 ~ dummy_diesel_ass + dummy_euro_4_ass + diesel_euro4_ass + age + female + dummy_car_unknown|", 
         paste(fe, collapse = " + ")))

fig3c_reg <- feols(reg_form3c, df_fig3c) |>
  tidy()


panel_3a_bind <- bind_rows(fig3a_reg, fig3b_reg, fig3c_reg, .id = "Model")

panel_3a_select <- subset(panel_3a_bind, term =="dummy_diesel"|term =="dummy_euro_4"|term =="diesel_euro4"|
                           term == "dummy_diesel_ass"|term =="dummy_euro_4_ass"|term =="diesel_euro4_ass")

panel_3a_select <- panel_3a_select |>
  mutate(term2 = case_when(term=="dummy_diesel" | term=="dummy_diesel_ass" ~ "Diesel",
                           term=="dummy_euro_4" | term=="dummy_euro_4_ass" ~ "Euro 4",
                           term=="diesel_euro4" | term=="diesel_euro4_ass" ~ "Diesel Euro 4"))

coeff_plot_3 <- 
  ggplot(panel_3a_select, aes(term2, estimate,colour=term2, fill=term2)) +
  scale_fill_manual(values = c("grey","darkred","grey"))+
  scale_colour_manual(values = c("grey","darkred","grey"))+
  geom_hline(yintercept=0, linetype="longdash", lwd=0.9, size=2, 
             colour = "lightgrey", alpha=1) +
  geom_errorbar(stat = "identity", alpha = 1, 
                position = position_dodge(width = 0.15),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.4, width=0) +
  geom_point(stat = "identity", alpha = 1,  size = 3, 
             position = position_dodge(width = 0.15)) + coord_flip() +facet_wrap(~Model)+
  theme_minimal()+theme(legend.position = "none", 
                        plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=14.5),
                        axis.text.y=element_text(size=14.5),
                        strip.text.x = element_text(size = 14.5),
                        panel.spacing = unit(1.7, "lines"),
                        plot.title = element_text(hjust = 0.5)
                        ) +
  labs(x = "", y = " ", title="Municipal Elections",  
       colour="", fill="", shape="", group="" ) 


coeff_plot_combined <- (coeff_plot_1 | coeff_plot_2 | coeff_plot_3) +
  plot_layout(nrow = 3) 


ggsave("combined_plot.png", coeff_plot_combined)


########################################################################
#######################################################################
Age
#######################################################################
##############################################
df_reg1 <- df |>
  filter(target!=3 & target!=4 & no_answer_euro == 0 & no_answer_2018 == 0)

fe <- df_reg1 |> 
  select(starts_with("EDU"),
         starts_with("INC"),
         -starts_with("education_level")) |> 
  names()

reg_form_age <- as.formula(
  paste0("sw_to_lega_18_19 ~ dummy_diesel + dummy_euro_4 + old45*diesel_euro4 + young45*diesel_euro4 + female |", 
         paste(fe, collapse = " + ")))

reg_age_leg <- feols(reg_form_age, df_reg1) 
  
##############################################
df_reg2 <- df |>
  filter(target!=3 & target!=4 & no_answer_euro == 0 & no_answer_regional == 0)

fe <- df_reg2 |> 
  select(starts_with("EDU"),
         starts_with("INC"),
         -starts_with("education_level")) |> 
  names()

reg_form_age_2 <- as.formula(
  paste0("sw_to_lega_reg_19 ~ dummy_diesel + dummy_euro_4 + old45*diesel_euro4 + young45*diesel_euro4 + female |", 
         paste(fe, collapse = " + ")))

reg_age_regn <- feols(reg_form_age_2, df_reg2)
##################################################
df_reg3 <- df |>
  filter(target!=3 & target!=4 & no_answer_euro == 0 & no_answer_municipal == 0)

fe <- df_reg3 |> 
  select(starts_with("EDU"),
         starts_with("INC"),
         -starts_with("education_level")) |> 
  names()

reg_form_age_3 <- as.formula(
  paste0("sw_to_lega_16_19 ~ dummy_diesel + dummy_euro_4 + old45*diesel_euro4 + young45*diesel_euro4 + female |", 
         paste(fe, collapse = " + ")))

reg_age_mun <- feols(reg_form_age_3, df_reg3)

age_hetero <- modelsummary(
  list(
    Legislative = reg_age_leg,
    Regional = reg_age_regn,
    Municipal = reg_age_mun
  ),
  stars = TRUE,
  coef_rename = c("Dummy Diesel", "Dummy Euro 4", "Dummy Old", "Diesel Euro 4", "Female", "Old*Diesel Euro 4"),
  gof_map =c("r.squared", "nobs"),
  output = "gt") |>
  tab_header(title = "Age Heterogeneity") 
 
?modelsummary
  
gtsave(age_hetero, filename = "age_hetero_tab_2.png")


#######################################################################
#Different Age Cutoffs Coefficient Plot (Legislative)
#######################################################################
#######################################################################
df_coeff_age_1 <- df |>
  mutate(young35 = case_when(age <= 35 ~ 1,
                           age > 35 ~ 0),
         old35 = case_when(age <= 35 ~ 0,
                         age > 35 ~1))

fe <- df_coeff_age_1 |> 
  select(starts_with("EDU"),
         starts_with("INC"),
         -starts_with("education_level")) |> 
  names()

reg_form_age_coeff_1 <- as.formula(
  paste0("sw_to_lega_18_19 ~ dummy_diesel + dummy_euro_4 + old35*diesel_euro4 + young35*diesel_euro4 + female |", 
         paste(fe, collapse = " + ")))

reg_age_leg_1 <- feols(reg_form_age_coeff_1, df_coeff_age_1) |>
  tidy()
##########################################################################

df_coeff_age_2 <- df |>
  mutate(young30 = case_when(age <= 30 ~ 1,
                             age > 30 ~ 0),
         old30 = case_when(age <= 30 ~ 0,
                           age > 30 ~1))

fe <- df_coeff_age_2 |> 
  select(starts_with("EDU"),
         starts_with("INC"),
         -starts_with("education_level")) |> 
  names()

reg_form_age_coeff_2 <- as.formula(
  paste0("sw_to_lega_18_19 ~ dummy_diesel + dummy_euro_4 + old30*diesel_euro4 + young30*diesel_euro4 + female |", 
         paste(fe, collapse = " + ")))

reg_age_leg_2 <- feols(reg_form_age_coeff_2, df_coeff_age_2) |>
  tidy()

#######################################################################
df_coeff_age_3 <- df |>
  mutate(young55 = case_when(age <= 55 ~ 1,
                             age > 55 ~ 0),
         old55 = case_when(age <= 55 ~ 0,
                           age > 55 ~1))

fe <- df_coeff_age_3 |> 
  select(starts_with("EDU"),
         starts_with("INC"),
         -starts_with("education_level")) |> 
  names()

reg_form_age_coeff_3 <- as.formula(
  paste0("sw_to_lega_18_19 ~ dummy_diesel + dummy_euro_4 + old55*diesel_euro4 + young55*diesel_euro4 + female |", 
         paste(fe, collapse = " + ")))

reg_age_leg_3 <- feols(reg_form_age_coeff_3, df_coeff_age_3) |>
  tidy()

#########################################################################
df_coeff_age_4 <- df |>
  mutate(young40 = case_when(age <= 40 ~ 1,
                             age > 40 ~ 0),
         old40 = case_when(age <= 40 ~ 0,
                           age > 40 ~1))

fe <- df_coeff_age_4 |> 
  select(starts_with("EDU"),
         starts_with("INC"),
         -starts_with("education_level")) |> 
  names()

reg_form_age_coeff_4 <- as.formula(
  paste0("sw_to_lega_18_19 ~ dummy_diesel + dummy_euro_4 + old40*diesel_euro4 + young40*diesel_euro4 + female |", 
         paste(fe, collapse = " + ")))

reg_age_leg_4 <- feols(reg_form_age_coeff_4, df_coeff_age_4) |>
  tidy()
#####################################################################################################
reg_age_leg_2$model <- "Cut Off 30"
reg_age_leg_1$model <- "Cut Off 35"
reg_age_leg_4$model <- "Cut Off 40"
reg_age_leg$model <- "Cut Off 45"
reg_age_leg_3$model <- "Cut Off 55"

model_list <- list(reg_age_leg_1, reg_age_leg_2, reg_age_leg, reg_age_leg_4, reg_age_leg_3)

data_for_plot <- model_list |>
  list_rbind() |>
  filter(term %in% c ("old30:diesel_euro4", "old35:diesel_euro4","old40:diesel_euro4", "old45:diesel_euro4", "old55:diesel_euro4"))

# Calculate standard errors from confidence intervals


age_hetero <- ggplot(data_for_plot, aes(x = model, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(stat = "identity", alpha = 1, 
                position = position_dodge(width = 0.15),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error),
                lwd=1.4, width=0, color = "darkgreen" ) +
  labs(x = "Model", y = "Coefficient (old*diesel_euro4)")

ggsave("age_errorplot.png", age_hetero)

#######################################################################
Education
#######################################################################

df_reg4 <- df |>
  filter(target!=3 & target!=4 & no_answer_euro == 0 & no_answer_2018 == 0 & EDU4 != 1)

fe_noedu <- df_reg4 |> 
  select(starts_with("INC"),
         age) |> 
  names()

reg_form_edu_4 <- as.formula(
  paste0("sw_to_lega_18_19 ~ dummy_diesel + dummy_euro_4 + high_edu*diesel_euro4 + female |", 
         paste(fe_noedu, collapse = " + ")))

reg_edu_leg <- feols(reg_form_edu_4, df_reg4)


##############################################
df_reg5 <- df |>
  filter(target!=3 & target!=4 & no_answer_euro == 0 & no_answer_regional == 0 & EDU4 != 1)

fe_noedu <- df_reg5 |> 
  select(starts_with("INC"),
         age) |> 
  names()

reg_form_edu_5 <- as.formula(
  paste0("sw_to_lega_reg_19 ~ dummy_diesel + dummy_euro_4 + high_edu*diesel_euro4 + female |", 
         paste(fe_noedu, collapse = " + ")))

reg_edu_regn <- feols(reg_form_edu_5, df_reg5)

################################################## 
df_reg6 <- df |>
  filter(target!=3 & target!=4 & no_answer_euro == 0 & no_answer_regional == 0 & EDU4 != 1)

fe_noedu <- df_reg6 |> 
  select(starts_with("INC"),
         age) |> 
  names()

reg_form_age_6 <- as.formula(
  paste0("sw_to_lega_16_19 ~ dummy_diesel + dummy_euro_4 + high_edu*diesel_euro4 + female |", 
         paste(fe_noedu, collapse = " + ")))

reg_edu_mun <- feols(reg_form_age_6, df_reg6)

edu_hetero <- modelsummary(
  list(
    Lesgislative = reg_edu_leg,
    Regional = reg_edu_regn,
    Municipal = reg_edu_mun
  ),
  coef_rename = c("Dummy Diesel", "Dummy Euro 4", "Dummy High Education", "Diesel Euro 4", "Female", "High Education*Diesel Euro 4"),
  gof_map =c("r.squared", "nobs"),
  stars = TRUE,
  output = "gt")

gtsave(edu_hetero, filename = "edu_hetero.png")


#################################################################################################################################
### Evidence from European Countries #########################################################################################
#################################################################################################################################

europe_cses <- cses5 |>
  filter(E1003 %in% c("00802017", "04002017", "05612019", "05622019", "20302021", "20802019", 
         "24602019", "25002017", "27602021", "30002019", "34802018", "35202017", "37202016", 
         "38002018", "42802018", "44002020", "52802021", "57802017", "61602019", "62002019",
         "64202016", "70302020", "75202018", "75602019", "82602019"))

europe_cses <- europe_cses |>
  rename(political_ideology = "E3013_LR_CSES",
         age = "E2001_A",
         vote_right = "E3013_LR_MARPOR",
         party_type = "E3013_IF_CSES",
         education = "E2003",
         gender = "E2002",
         vote_switch = "E3013_VS_1",
         values = "E3022",
         paid_job = "E2006",
         job_type = "E2007",
         family_inc = "E2010",
         districts = "E2020") |>
  filter(vote_right == 1) 

europe_cses <- europe_cses |>
  filter(education != "96",
         education != "97",
         education != "98",
         education != "99")

europe_cses <- europe_cses |>
  filter(age != "9997",
         age != "9998",
         age != "9999")

europe_cses <- europe_cses |>
  filter(family_inc != "6",
         family_inc != "7",
         family_inc != "8",
         family_inc != "9")

europe_age_df <- europe_cses |>
  mutate(young = case_when(age <= 80 ~ 1,
                           age > 80 ~ 0),
         old = case_when(age <= 80 ~ 0,
                         age > 80 ~ 1))


europe_reg1 <- feols(vote_switch ~ family_inc + values + education + age + political_ideology| gender,
                       europe_age_df) |>
  modelsummary(
    coeff_rename = c("Family Income", "Values", "Education", "Age", "Political Ideology"),
    gof_map = c()
  )


###############################################################################################

italy_cses <- cses5 |>
  filter (E1003 == "38002018")

italy_cses <- italy_cses |>
  rename(political_ideology = "E3013_LR_CSES",
         age = "E2001_A",
         vote_right = "E3013_LR_MARPOR",
         party_type = "E3013_IF_CSES",
         education = "E2003",
         gender = "E2002",
         vote_switch = "E3013_VS_1",
         values = "E3022",
         paid_job = "E2006",
         job_type = "E2007",
         family_inc = "E2010",
         districts = "E2020") |>
  filter(vote_right == 1)



italy_cses <- italy_cses |>
  filter(education != "96",
         education != "97",
         education != "98",
         education != "99")

italy_cses <- italy_cses |>
  filter(age != "9997",
         age != "9998",
         age != "9999")

italy_cses <- italy_cses |>
  filter(family_inc != "6",
         family_inc != "7",
         family_inc != "8",
         family_inc != "9")


italy_reg1 <- feols(vote_switch ~ family_inc + education + age + political_ideology| gender,
                     italy_cses)




  

