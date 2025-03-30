#### load packages

library(tidyverse)
library(readxl)
library(easystats)
library(glue)

#### load data

data_de <- data_read("./data_de.xlsx")
data_us <- data_read("./data_us.xlsx")

data_de <- data_de |> slice(-1)
data_us <- data_us |> slice(-1)

### save data as csv

data_write(data_de, "./data_de.csv")
data_write(data_us, "./data_us.csv")

### load data as csv

data_de <- data_read("./data_de.csv")
data_us <- data_read("./data_us.csv")

### remove empty columns

data_de <- remove_empty_columns(data_de)
data_us <- remove_empty_columns(data_us)

#### set missing values to NA

replace_invalid <- function(x) {
  replace(x, x %in% c(-1, -9, -99), NA)
}

data_de <- data_de |>
  mutate(across(where(is.numeric), replace_invalid))

data_us <- data_us |>
  mutate(across(where(is.numeric), replace_invalid))

#### Rename variables: de

data_de <- data_de |>
  rename(
    case_id        = CASE,
    questionnaire  = QUESTNNR,
    interview_mode = MODE,
    started_at     = STARTED,
    consent        = D102,
    exclusion      = D103,
    age            = D202_01,
    citizenship    = D203)

data_de <- data_de |>
  rename(
    ethnicity_count         = D204,
    eth_white               = D204_01,
    eth_black               = D204_02,
    eth_latinx              = D204_03,
    eth_asian               = D204_04,
    eth_indigenous_native   = D204_05,
    eth_pacific             = D204_06,
    eth_unspecified         = D204_07,
    gender_count            = D205,
    gender_female           = D205_01,
    gender_male             = D205_02,
    gender_cis              = D205_03,
    gender_dyadic           = D205_04,
    gender_inter            = D205_05,
    gender_nonbinary        = D205_06,
    gender_questioning      = D205_07,
    gender_trans            = D205_08,
    gender_na               = D205_09,
    gender_other            = D205_10,
    gender_other_text       = D205_10a)

data_de <- data_de |>
  rename(
    partnered           = D206,
    has_children        = D207,
    position_count      = D208,
    pos_professor       = D208_01,
    pos_junior_prof     = D208_02,
    pos_priv_docent     = D208_03,
    pos_group_leader    = D208_04,
    pos_postdoc         = D208_05,
    pos_akad_council    = D208_06,
    pos_other           = D208_07,
    pos_other_text      = D208_07a)

data_de <- data_de |>
  rename(
    tenure              = D209,
    field_count         = D210,
    field_biology       = D210_01,
    field_chemistry     = D210_02,
    field_engineering   = D210_03,
    field_aerospace     = D210_04,
    field_biomedicine   = D210_05,
    field_comp_eng      = D210_06,
    field_chem_eng      = D210_07,
    field_civil_eng     = D210_08,
    field_electrical    = D210_09,
    field_env_eng       = D210_10,
    field_nuclear       = D210_11,
    field_materials     = D210_12,
    field_math          = D210_13,
    field_physics       = D210_14,
    field_cs            = D210_15,
    field_other         = D210_16,
    field_other_text    = D210_16a,
    rank_category       = D211)

data_de <- data_de |>
  rename(
    study_field_count    = D212,
    study_biology        = D212_15,
    study_chemistry      = D212_16,
    study_aerospace      = D212_17,
    study_biomedicine    = D212_18,
    study_comp_eng       = D212_19,
    study_chem_eng       = D212_20,
    study_civil_eng      = D212_21,
    study_electrical     = D212_22,
    study_env_eng        = D212_23,
    study_nuclear        = D212_24,
    study_materials      = D212_25,
    study_math           = D212_26,
    study_physics        = D212_27,
    study_cs             = D212_28,
    study_other          = D212_29,
    study_other_text     = D212_29a,
    degree_location      = D213,
    degree_abroad_text   = D213_02)

data_de <- data_de |>
  rename(
    teach_hours_bachelor   = D302_01,
    teach_hours_master     = D302_02,
    teach_hours_contact    = D302_03,
    mentees_bachelor       = D303_01,
    mentees_master         = D303_02,
    mentees_phd            = D303_03,
    mentees_postdoc        = D303_04,
    mentees_staff          = D303_05,
    mentees_junior_prof    = D303_06,
    mentoring_hours        = D304_01,
    advising_services      = D305_01)

data_de <- data_de |>
  rename(
    # WHO-5 (Wohlbefinden)
    who5_positive_mood      = D402_01,
    who5_relaxed            = D402_02,
    who5_energetic          = D402_03,
    who5_restful_sleep      = D402_04,
    who5_interest_life      = D402_05,
    
    # GHQ-12 (psych. Belastung)
    ghq12_sleep_worries     = D403_01,
    ghq12_pressure          = D406_02,
    ghq12_concentration     = D407_02,
    ghq12_useful            = D408_02,
    ghq12_deal_problems     = D409_02,
    ghq12_decision_difficulty = D410_02,
    ghq12_overwhelmed       = D411_02,
    ghq12_contentment       = D412_02,
    ghq12_daily_duties      = D413_02,
    ghq12_depressed         = D414_03,
    ghq12_low_confidence    = D415_04,
    ghq12_worthless         = D416_05,
    
    # PHQ-4 (Depression/Angst, ultrakurz)
    phq4_low_interest       = D404_01,
    phq4_feeling_down       = D404_02,
    phq4_nervous            = D404_03,
    phq4_worry_control      = D404_04,
    
    # GAD-7 (Angststörung)
    gad7_excess_worry       = D405_03,
    gad7_relax_difficulty   = D405_04,
    gad7_restlessness       = D405_05,
    gad7_irritability       = D405_06,
    gad7_fear_bad_happen    = D405_07,
    
    # F-SozU K-6 (soziale Unterstützung)
    soz_support_understood  = D502_01,
    soz_support_trusted     = D502_02,
    soz_support_borrow      = D502_03,
    soz_support_activities  = D502_04,
    soz_support_illness     = D502_05,
    soz_support_distress    = D502_06,
    
    # BRS (Resilienzskala)
    brs_recover_quickly     = D503_01,
    brs_handle_stress       = D503_02,
    brs_recover_fast        = D503_03,
    brs_back_to_normal      = D503_04,
    brs_get_through         = D503_05,
    brs_take_long_to_recover = D503_06)

data_de <- data_de %>%
  rename(
    # CERQ-S
    cerq_akz_1 = D504_01,
    cerq_ref_2 = D504_02,
    cerq_lern_3 = D504_03,
    cerq_selfblame_4 = D504_04,
    cerq_akz_5 = D504_05,
    cerq_rum_6 = D504_06,
    cerq_ablenk_7 = D504_07,
    cerq_stark_8 = D504_08,
    cerq_gruebeln_9 = D504_09,
    cerq_blame_10 = D504_10,
    cerq_posi_11 = D504_11,
    cerq_loesung_12 = D504_12,
    cerq_vergl_13 = D504_13,
    cerq_selfcause_14 = D504_14,
    cerq_plan_15 = D504_15,
    cerq_relat_16 = D504_16,
    cerq_katast_17 = D504_17,
    cerq_othercause_18 = D504_18,
    
    # MHLQ-YA
    mhlq_sport = D602_01,
    mhlq_depr_symp = D602_02,
    mhlq_schizo_wahn = D602_03,
    mhlq_help_fam = D602_04,
    mhlq_help_psy = D602_05,
    mhlq_no_behav = D602_06,
    mhlq_sleep = D602_07,
    mhlq_self_help_psy = D602_08,
    mhlq_anx_panik = D602_09,
    mhlq_income = D602_10,
    mhlq_nonjdm = D602_11,
    mhlq_alc = D602_12,
    mhlq_no_affect = D602_13,
    mhlq_early = D602_14,
    mhlq_adults_only = D602_15,
    mhlq_brain = D602_16,
    mhlq_psych_help = D602_17,
    mhlq_friend_help = D602_18,
    mhlq_nutri = D602_19,
    mhlq_interest = D602_20,
    mhlq_helpless = D602_21,
    mhlq_duration = D602_22,
    mhlq_depr_real = D602_23,
    mhlq_drugs = D602_24,
    mhlq_thoughts = D602_25,
    mhlq_fun = D602_26,
    mhlq_schizo_hall = D602_27,
    mhlq_stress = D602_28,
    mhlq_self_psych = D602_29,
    
    # MHLS1
    mhls1_socphob = D603_01,
    mhls1_gad = D603_02,
    mhls1_depr = D603_03,
    mhls1_pers = D603_04,
    mhls1_dysth = D603_05,
    mhls1_agora = D603_06,
    mhls1_bipol = D603_07,
    mhls1_subs = D603_08,
    mhls1_fem_more = D603_09,
    mhls1_male_anx = D603_10,
    
    # MHLS2
    mhls2_sleep = D604_01,
    mhls2_avoid = D604_02,
    
    # MHLS3
    mhls3_kvt = D605_01,
    mhls3_break_danger = D605_02,
    mhls3_break_support = D605_03,
    
    # MHLS4
    mhls4_info = D606_01,
    mhls4_digital = D606_02,
    mhls4_apt = D606_03,
    mhls4_sources = D606_04,
    mhls4_control = D606_05,
    mhls4_weakness = D606_06,
    mhls4_not_med = D606_07,
    mhls4_danger = D606_08,
    mhls4_avoidance = D606_09,
    mhls4_disclose = D606_10,
    
    # MHLS5
    mhls5_neighbor = D607_01,
    mhls5_evening = D607_02,
    mhls5_friend = D607_03,
    mhls5_job = D607_04,
    mhls5_family = D607_05,
    mhls5_vote = D607_06,
    mhls5_hire = D607_07,
    
    # ressources
    resources = D608,
    
    # MHSAS
    mhsas_useful = D610_10,
    mhsas_important = D610_11,
    mhsas_healthy = D610_12,
    mhsas_effective = D610_13,
    mhsas_good = D610_14,
    mhsas_healing = D610_15,
    mhsas_empowering = D610_16,
    mhsas_satisfying = D610_17,
    mhsas_desirable = D610_18)

data_de <- data_de %>%
  rename(
    # Abteilungsklima1 (D702)
    abtkl_freundlich = D702_01,
    abtkl_rassistisch = D702_02,
    abtkl_vielfalt = D702_03,
    abtkl_respekt = D702_04,
    abtkl_kollegial = D702_05,
    abtkl_sexismus = D702_06,
    abtkl_kollab = D702_07,
    abtkl_kooperativ = D702_08,
    abtkl_homophob = D702_09,
    abtkl_support = D702_10,
    
    # Abteilungsklima2 (D703)
    abtkl_forsch_geschaetzt = D703_01,
    abtkl_anpassdruck = D703_02,
    abtkl_festanstellung_druck = D703_03,
    abtkl_leistung_fragen = D703_04,
    abtkl_prob_ansprechen = D703_05,
    abtkl_gender_view = D703_06,
    abtkl_ethnie_view = D703_07,
    abtkl_erwartungen = D703_08,
    abtkl_beobachtet = D703_09,
    abtkl_harter_weg = D703_10,
    abtkl_regeln = D703_11,
    abtkl_dazugehoeren = D703_12,
    
    # Abteilungsklima3 (D704)
    abtkl_lehrplan = D704_01,
    abtkl_gehalt = D704_02,
    abtkl_reisekosten = D704_03,
    abtkl_geraete = D704_04,
    abtkl_promiwahl = D704_05,
    abtkl_fakwahl = D704_06,
    abtkl_festanstellung = D704_07,
    abtkl_leitung = D704_08,
    abtkl_klima = D704_09,
    
    # Mentorship (D705–D707)
    mentorship = D705,
    mentor_m = D706_01,
    mentor_w = D706_02,
    mentor_div = D706_03,
    mentor_rolle = D707_01_CN)

data_de <- data_de %>%
  rename(
    # Mentorship3M: Rollenmodell
    mentor_rolle_gl = D707_01_1,
    mentor_rolle_af = D707_01_2,
    mentor_rolle_ai = D707_01_3,
    mentor_rolle_au = D707_01_4,
    
    # Mentorship3M: Networking
    mentor_net_gl = D707_02_1,
    mentor_net_af = D707_02_2,
    mentor_net_ai = D707_02_3,
    mentor_net_au = D707_02_4,
    mentor_net_n = D707_02_CN,
    
    # Mentorship3M: Aufstieg
    mentor_aufstieg_gl = D707_03_1,
    mentor_aufstieg_af = D707_03_2,
    mentor_aufstieg_ai = D707_03_3,
    mentor_aufstieg_au = D707_03_4,
    mentor_aufstieg_n = D707_03_CN,
    
    # Mentorship3M: Veröffentlichung
    mentor_pub_gl = D707_04_1,
    mentor_pub_af = D707_04_2,
    mentor_pub_ai = D707_04_3,
    mentor_pub_au = D707_04_4,
    mentor_pub_n = D707_04_CN,
    
    # Mentorship3M: Abteilungspolitik
    mentor_politik_gl = D707_05_1,
    mentor_politik_af = D707_05_2,
    mentor_politik_ai = D707_05_3,
    mentor_politik_au = D707_05_4,
    mentor_politik_n = D707_05_CN,
    
    # Mentorship3M: Ressourcen
    mentor_res_gl = D707_06_1,
    mentor_res_af = D707_06_2,
    mentor_res_ai = D707_06_3,
    mentor_res_au = D707_06_4,
    mentor_res_n = D707_06_CN,
    
    # Mentorship3M: Einsatz
    mentor_einsatz_gl = D707_07_1,
    mentor_einsatz_af = D707_07_2,
    mentor_einsatz_ai = D707_07_3,
    mentor_einsatz_au = D707_07_4,
    mentor_einsatz_n = D707_07_CN,
    
    # Mentorship3M: Vereinbarkeit
    mentor_fam_gl = D707_08_1,
    mentor_fam_af = D707_08_2,
    mentor_fam_ai = D707_08_3,
    mentor_fam_au = D707_08_4,
    mentor_fam_n = D707_08_CN,
    
    # Mentorship3M: Anderes
    mentor_and_gl = D707_09_1,
    mentor_and_af = D707_09_2,
    mentor_and_ai = D707_09_3,
    mentor_and_au = D707_09_4,
    mentor_and_n = D707_09_CN)

## Recode tenure

data_de <- data_de %>% mutate (tenure = as.factor(tenure))

data_de <- data_de %>%
  mutate(tenure = fct_recode(tenure,"yes" = "1","no" = "2"))

## Recode mentorship

data_de <- data_de %>% mutate (mentorship = as.factor(mentorship))

data_de <- data_de %>%
  mutate(mentorship = fct_recode(mentorship,"yes" = "1","no" = "2"))

#### Rename variables: us

data_us <- data_us |>
  rename(
    consent = U104,                  
    exclusion_criteria = U105,       
    age = U202_01,                   
    citizenship = U203)

data_us <- data_us |>
  rename(
    ethnicity_count         = U204,
    eth_white               = U204_01,
    eth_black               = U204_02,
    eth_latinx              = U204_03,
    eth_asian               = U204_04,
    eth_indigenous_native   = U204_05,
    eth_pacific             = U204_06,
    eth_unspecified         = U204_07)

data_us <- data_us |>
  rename(
    gender_count       = U205,
    gender_female      = U205_01,
    gender_male        = U205_02,
    gender_cis         = U205_03,
    gender_dyadic      = U205_04,
    gender_inter       = U205_05,
    gender_nonbinary   = U205_06,
    gender_questioning = U205_07,
    gender_trans       = U205_08,
    gender_na          = U205_09,
    gender_other       = U205_10,
    partnered          = U206,
    has_children       = U207)

data_us <- data_us |>
  rename(
    position_count         = U208,
    # Vergleichbar mit pos_junior_prof (Juniorprofessor:in)
    pos_assist_prof        = U208_02,
    # Vergleichbar mit pos_priv_docent / mid-career
    pos_assoc_prof         = U208_03,
    # Vergleichbar mit pos_professor
    pos_full_prof          = U208_04,
    pos_dist_prof          = U208_05,
    pos_prof               = U208_15,  # Falls einfach "Professor" ausgewählt wird
    # US-spezifische Teaching Track Positionen
    pos_lecturer           = U208_07,
    pos_instructor         = U208_08,
    pos_assist_teach_prof  = U208_09,
    pos_assoc_teach_prof   = U208_10,
    pos_teach_prof         = U208_11,
    # US-spezifische Research Track Positionen
    pos_assist_res_prof    = U208_12,
    pos_assoc_res_prof     = U208_13,
    pos_res_prof           = U208_14,
    # "Other" Kategorie analog zu DE
    pos_other              = U208_16,
    pos_other_text         = U208_16a)

data_us <- data_us |>
  rename(
    tenure               = U209,
    field_count          = U210,
    field_biology        = U210_01,
    field_chemistry      = U210_02,
    field_materials      = U210_03,
    field_math           = U210_04,
    field_physics        = U210_05,
    field_aerospace      = U210_06,
    field_biomedicine    = U210_07,
    field_comp_eng       = U210_08,
    field_chem_eng       = U210_09,
    field_civil_eng      = U210_10,
    field_electrical     = U210_11,
    field_env_eng        = U210_12,
    field_nuclear        = U210_13,
    field_cs             = U210_15,
    field_other          = U210_14,
    field_other_text     = U210_14a,
    # US-spezifisch: Mechanical Engineering gibt's im DE-Set nicht explizit
    field_mechanical     = U210_16,  # Extra-Feld, falls du später vergleichen willst
    rank_category        = U211
  )

data_us <- data_us |>
  rename(
    study_field_count    = U212,
    study_biology        = U212_01,
    study_chemistry      = U212_02,
    study_materials      = U212_03,
    study_math           = U212_04,
    study_physics        = U212_05,
    study_aerospace      = U212_06,
    study_biomedicine    = U212_07,
    study_comp_eng       = U212_08,
    study_chem_eng       = U212_09,
    study_civil_eng      = U212_10,
    study_electrical     = U212_11,
    study_env_eng        = U212_12,
    study_nuclear        = U212_13,
    study_other          = U212_14,
    study_other_text     = U212_14a,
    degree_location      = U213,
    degree_abroad_text   = U213_02)

data_us <- data_us |>
  rename(
    teach_hours_bachelor   = U302_01,
    teach_hours_master     = U302_02,
    teach_hours_contact    = U302_03,
    mentees_bachelor       = U303_01,
    mentees_master         = U303_02,
    mentees_phd            = U303_03,
    mentees_postdoc        = U303_04,
    mentees_resident       = U303_05,  # US-spezifisch: nicht in data_de
    mentees_junior_prof    = U303_06,
    mentoring_hours        = U304_01,
    advising_services      = U305_01)

data_us <- data_us |>
  rename(
    who5_positive_mood      = U402_01,
    who5_relaxed            = U402_02,
    who5_energetic          = U402_03,
    who5_restful_sleep      = U402_04,
    who5_interest_life      = U402_05,
    ghq12_concentration     = U403_01,
    ghq12_sleep_worries     = U403_02,
    ghq12_useful            = U403_03,
    ghq12_decision_difficulty = U403_04,
    ghq12_pressure          = U403_05,
    ghq12_deal_problems     = U403_06,
    ghq12_daily_duties      = U403_07,
    ghq12_contentment       = U403_08,
    ghq12_depressed         = U403_09,
    ghq12_low_confidence    = U403_10,
    ghq12_worthless         = U403_11,
    ghq12_overwhelmed       = U403_12,
    gad7_excess_worry       = U404_03,
    gad7_relax_difficulty   = U404_04,
    gad7_restlessness       = U404_05,
    gad7_irritability       = U404_06,
    gad7_fear_bad_happen    = U404_07,
    phq4_nervous            = U405_01,
    phq4_worry_control      = U405_02,
    phq4_feeling_down       = U405_03,
    phq4_low_interest       = U405_04,
    soz_support_understood  = U502_01,
    soz_support_trusted     = U502_02,
    soz_support_borrow      = U502_03,
    soz_support_activities  = U502_04,
    soz_support_illness     = U502_05,
    soz_support_distress    = U502_06,
    brs_recover_quickly     = U503_01,
    brs_handle_stress       = U503_02,
    brs_recover_fast        = U503_03,
    brs_back_to_normal      = U503_04,
    brs_get_through         = U503_05,
    brs_take_long_to_recover = U503_06)

data_us <- data_us |>
  rename(
    cerq_akz_1         = U504_01,
    cerq_ref_2         = U504_02,
    cerq_lern_3        = U504_03,
    cerq_selfblame_4   = U504_04,
    cerq_akz_5         = U504_05,
    cerq_rum_6         = U504_06,
    cerq_ablenk_7      = U504_07,
    cerq_stark_8       = U504_08,
    cerq_gruebeln_9    = U504_09,
    cerq_blame_10      = U504_10,
    cerq_posi_11       = U504_11,
    cerq_loesung_12    = U504_12,
    cerq_vergl_13      = U504_13,
    cerq_selfcause_14  = U504_14,
    cerq_plan_15       = U504_15,
    cerq_relat_16      = U504_16,
    cerq_katast_17     = U504_17,
    cerq_othercause_18 = U504_18)

data_us <- data_us |>
  rename(
    mhlq_thoughts      = U602_01,
    mhlq_schizo_hall   = U602_02,
    mhlq_depr_real     = U602_03,
    mhlq_stress        = U602_04,
    mhlq_brain         = U602_05,
    mhlq_duration      = U602_06,
    mhlq_adults_only   = U602_07,
    mhlq_no_affect     = U602_08,
    mhlq_no_behav      = U602_09,
    mhlq_self_help_psy = U602_10,
    mhlq_help_fam      = U602_11,
    mhlq_help_psy      = U602_12,
    mhlq_sleep         = U602_13,
    mhlq_nutri         = U602_14,
    mhlq_sport         = U602_15,
    mhlq_fun           = U602_16)

data_us <- data_us |>
  rename(
    mhls1_socphob       = U603_01,
    mhls1_gad           = U603_02,
    mhls1_depr          = U603_03,
    mhls1_pers          = U603_04,
    mhls1_dysth         = U603_05,
    mhls1_agora         = U603_06,
    mhls1_bipol         = U603_07,
    mhls1_subs          = U603_08,
    mhls1_fem_more      = U603_09,
    mhls1_male_anx      = U603_10,
    mhls3_kvt           = U603_11,
    mhls3_break_danger  = U603_12,
    mhls3_break_support = U603_14,
    mhls2_sleep         = U603_16,
    mhls2_avoid         = U603_17,
    mhls4_info          = U604_01,
    mhls4_digital       = U604_02,
    mhls4_apt           = U604_03,
    mhls4_sources       = U604_04,
    mhls4_control       = U604_05,
    mhls4_weakness      = U604_06,
    mhls4_not_med       = U604_07,
    mhls4_danger        = U604_08,
    mhls5_neighbor      = U605_01,
    mhls5_friend        = U605_02,
    mhls5_family        = U605_03,
    mhls5_job           = U605_04,
    resources     = U606)

data_us <- data_us |>
  rename(
    mhsas_useful       = U607_01,
    mhsas_important    = U607_02,
    mhsas_healthy      = U607_03,
    mhsas_effective    = U607_04,
    mhsas_good         = U607_05,
    mhsas_healing      = U607_06,
    mhsas_empowering   = U607_07,
    mhsas_satisfying   = U607_08,
    mhsas_desirable    = U607_09)

data_us <- data_us |>
  rename(
    dep_climate_friendly         = U702_01,
    dep_climate_racist           = U702_02,
    dep_climate_diverse          = U702_03,
    dep_climate_respectful       = U702_04,
    dep_climate_collegial        = U702_05,
    dep_climate_sexist           = U702_06,
    dep_climate_individualistic  = U702_07,
    dep_climate_competitive      = U702_08,
    dep_climate_homophobic       = U702_09,
    dep_climate_supportive       = U702_10)

data_us <- data_us |>
  rename(
    dep_climate_research_valued        = U703_01,
    dep_climate_pressure_fitin         = U703_02,
    dep_climate_pressure_tenure        = U703_03,
    dep_climate_questions_ok           = U703_04,
    dep_climate_fear_speakup           = U703_05,
    dep_climate_gender_stereotype      = U703_06,
    dep_climate_ethnicity_stereotype   = U703_07,
    dep_climate_low_expectations       = U703_08,
    dep_climate_under_scrutiny         = U703_09,
    dep_climate_work_harder            = U703_10,
    dep_climate_unwritten_rules        = U703_11,
    dep_climate_fit_in                 = U703_12)

data_us <- data_us |>
  rename(
    dep_climate_curriculum             = U704_01,
    dep_climate_salary_increase        = U704_02,
    dep_climate_travel_funding         = U704_03,
    dep_climate_research_resources     = U704_04,
    dep_climate_select_students        = U704_05,
    dep_climate_select_faculty         = U704_06,
    dep_climate_tenure_decision        = U704_07,
    dep_climate_select_leadership      = U704_08,
    dep_climate_overall_culture        = U704_09)

data_us <- data_us |>
  rename(
    mentorship = U705)

data_us <- data_us |>
  rename(
    mentor_role_model_same_unit         = U706_01_1,
    mentor_role_model_diff_unit         = U706_01_2,
    mentor_role_model_other_inst        = U706_01_3,
    mentor_role_model_outside_acad      = U706_01_4,
    mentor_networking_same_unit         = U706_02_1,
    mentor_networking_diff_unit         = U706_02_2,
    mentor_networking_other_inst        = U706_02_3,
    mentor_networking_outside_acad      = U706_02_4,
    mentor_advancement_same_unit        = U706_03_1,
    mentor_advancement_diff_unit        = U706_03_2,
    mentor_advancement_other_inst       = U706_03_3,
    mentor_advancement_outside_acad     = U706_03_4,
    mentor_publishing_same_unit         = U706_04_1,
    mentor_publishing_diff_unit         = U706_04_2,
    mentor_publishing_other_inst        = U706_04_3,
    mentor_publishing_outside_acad      = U706_04_4,
    mentor_politics_same_unit           = U706_05_1,
    mentor_politics_diff_unit           = U706_05_2,
    mentor_politics_other_inst          = U706_05_3,
    mentor_politics_outside_acad        = U706_05_4,
    mentor_resources_same_unit          = U706_06_1,
    mentor_resources_diff_unit          = U706_06_2,
    mentor_resources_other_inst         = U706_06_3,
    mentor_resources_outside_acad       = U706_06_4,
    mentor_advocates_same_unit          = U706_07_1,
    mentor_advocates_diff_unit          = U706_07_2,
    mentor_advocates_other_inst         = U706_07_3,
    mentor_advocates_outside_acad       = U706_07_4,
    mentor_worklife_same_unit           = U706_08_1,
    mentor_worklife_diff_unit           = U706_08_2,
    mentor_worklife_other_inst          = U706_08_3,
    mentor_worklife_outside_acad        = U706_08_4,
    mentor_other_same_unit              = U706_09_1,
    mentor_other_diff_unit              = U706_09_2,
    mentor_other_other_inst             = U706_09_3,
    mentor_other_outside_acad           = U706_09_4)

data_us <- data_us |>
  rename(
    mentor_other_description    = U708_01,
    mentor_count_male           = U710_01,
    mentor_count_female         = U710_02,
    mentor_count_non_binary     = U710_03)

## Recode tenure

data_us <- data_us %>% mutate (tenure = as.factor(tenure))

data_us <- data_us %>%
  mutate(tenure = fct_recode(tenure,"yes" = "1","no" = "2"))

## Recode mentorship

data_us <- data_us %>% mutate (mentorship = as.factor(mentorship))

data_us <- data_us %>%
  mutate(mentorship = fct_recode(mentorship,"yes" = "1","no" = "2"))

#### Sample-Variable hinzufügen

data_de <- data_de  |>
  mutate(sample = "de")

data_us <- data_us |>
  mutate(sample = "us")

#### save dfs

data_write(data_de, "data_tidy_de.csv")
data_write(data_us, "data_tidy_us.csv")

#### df mergen

### Variablen nur in data_us, aber nicht in data_de
vars_only_in_us <- setdiff(names(data_us), names(data_de))
vars_only_in_us

### mergen

df_all <- bind_rows(data_de, data_us)

### save df

data_write(df_all, "data_all.csv")

