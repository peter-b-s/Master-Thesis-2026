

# Creating test data set

library(dplyr)
library (tidyverse) 
library(ggplot2)
library(haven)
library(zoo)
library(stargazer)
library(corrplot)
library(psych)
library(modelsummary)


#loading data
ppathl <- readRDS("[...] SOEP v40/R_EN/soepdata/ppathl.rds")

cognit <- readRDS("[...]SOEP v40/R_EN/soepdata/cognit.rds")

#pl <- readRDS("[...] SOEP v40/R_EN/soepdata/pl.rds")
small_pl <- readRDS("[...] /small_pl.rds")

pgen <- readRDS("[...] SOEP v40/R_EN/soepdata/pgen.rds")
 
health <- readRDS("[...] SOEP v40/R_EN/soepdata/health.rds")

biojob <- readRDS("[...] /SOEP v40/R_EN/soepdata/biojob.rds")


## creating PPATHL dataframe

ppathl_cut <- ppathl %>% filter(syear >= 2006 & syear <= 2016) %>%
                         select(cid, hid, pid, syear, sex, gebjahr, gebmonat, eintritt, erstbefr, austritt, letztbef, todjahr, piyear, 
                                parid, partner, germborn, corigin, immiyear, migback, arefback, birthregion_ew, loc1989, sampreg) 

## creating PL dataframe

pl_raw <- small_pl %>% select(cid, hid, pid, syear, plb0022_h, plb0018, plb0282_h, plc0013_h, plc0014_h, plb0304_h, pld0146,
                        pld0047, 
                        pli0090_h, pli0091_h, pli0093_h, 
                        pli0043_h, 
                        pli0047_h, 
                        pli0044_h, 
                        pli0049_h,
                        pli0092_h,
                        pli0096_h
                        ) %>%      
                        filter(syear >= 2000 & syear <= 2016)

#saveRDS(pl_raw, "small_pl.rds")

#View(pl_raw)


pl_unimpt <- pl_raw %>%
  semi_join(cognit, by = "pid") %>%   # only IDs with test results but more years included
  left_join(cognit, by = c("pid", "syear"))  %>%  # joining test results by year
  select(hid, pid, syear, 
         f96t90s, f096, f99z90r, f099, 
         pld0047, 
         pli0090_h, pli0091_h, pli0093_h, 
         pli0043_h, 
         pli0047_h, 
         pli0044_h, 
         pli0049_h,
         pli0092_h,
         pli0096_h) %>%
         mutate(across(everything(), ~replace(., . < 0, NA)))


## Imputing, Last Observation Carry Forward (LOCF) and Next Observation Carry Backward (NOCB)

cont_vars <- c("pli0049_h",  
               "pli0043_h", 
               "pli0047_h",  
               "pli0044_h"
                )        # continuous

disc_vars <- c("pld0047", 
               "pli0090_h", "pli0091_h", "pli0093_h",
               "pli0092_h",
               "pli0096_h"
               )        # discrete

vars_orig <- c(cont_vars, disc_vars)            # all

pl_impt <- pl_unimpt %>%
           arrange(pid, syear) %>%
           group_by(pid) %>%
           mutate(across(all_of(cont_vars), 
           ~if(all(is.na(.))) NA_real_ else na.approx(., na.rm = FALSE, maxgap = 2), 
           .names = "{.col}_n"),
    
            across(all_of(disc_vars), 
           ~if(all(is.na(.))) NA_real_ else na.locf(na.locf(., na.rm = FALSE), fromLast = TRUE), 
           .names = "{.col}_n")) %>%
           ungroup()



pl_impt <- pl_impt %>% select(hid, pid, syear, f096, f96t90s, f099, f99z90r,
                       sort(c(vars_orig, paste0(vars_orig, "_n")), method = "radix")  # sort alphabetic
                        )%>% 
                       filter(f96t90s >= 0 | f99z90r >= 0) %>%
                       rename(nr_close_friends = pld0047_n,
                              hrs_chores_weekd = pli0043_h_n, 
                              hrs_childcare_weekd = pli0044_h_n, 
                              hrs_edu_weekd = pli0047_h_n, 
                              hrs_repair_weekd = pli0049_h_n, 
                              frq_classics = pli0090_h_n,
                              frq_cindisc = pli0091_h_n,
                              frq_sport = pli0092_h_n, 
                              frq_arts = pli0093_h_n,
                              frq_voluntr = pli0096_h_n 
                              ) 


pl_impt <- pl_impt %>% mutate(across(c(frq_classics, frq_cindisc, frq_sport, frq_arts, frq_voluntr),
                                        ~ dplyr::recode(as.numeric(.), 
                                                    '1' = 5,
                                                    '2' = 4, 
                                                    '3' = 3, 
                                                    '4' = 2,
                                                    '5' = 1))) %>% 
                      select(hid, pid, syear, f096, f96t90s, f099, f99z90r, 
                             nr_close_friends, 
                             hrs_chores_weekd, hrs_childcare_weekd, hrs_edu_weekd, hrs_repair_weekd,
                             frq_classics, frq_cindisc, frq_sport, frq_arts, frq_voluntr)
                  



## create final PL dataframe
##

pl_cut <- pl_raw %>% select(cid, pid, syear, plb0022_h, plb0018, plb0282_h, plc0013_h, plc0014_h, plb0304_h, pld0146) %>% 
                     mutate(pid = zap_labels(pid)) %>%
                     left_join(pl_impt, by = c("pid", "syear")) %>%
                     mutate(across(c(plb0022_h, plb0018, plb0282_h, plc0013_h, plc0014_h, plb0304_h, pld0146), as.numeric)) %>%                 
                     mutate(employment_status = ifelse(plb0022_h < 1, NA, plb0022_h)) %>%
                     mutate(paid_work_l7d = ifelse(plb0018 < 1, NA, recode(plb0018, '1' = 1, '2' = 0))) %>%
                     mutate(retired_beg_prevy = ifelse(plb0282_h < 1, NA, recode(plb0282_h, '1' = 1, '2' = 0))) %>% 
                     mutate(typ_job_end = ifelse(plb0304_h < 1, NA, plb0304_h)) %>% 
                     mutate(partner_died = ifelse(pld0146 < 1, NA, recode(pld0146, '1' = 1))) %>%
                     select(-plb0022_h, -plb0018, -plb0282_h, -plb0304_h, -pld0146, -f096, -f96t90s, -f099, -f99z90r)


## creating PGEN dataframe

pgen_cut <- pgen %>% filter(syear >= 2004 & syear <= 2017) %>%
                      select(cid, hid, pid, syear, imonth, #including imonth (interview month) here for getting age in df_big
                            pgstib, pgemplst, pglfs,   #employment and labor force
                            pgegp88,
                            pgisco88,
                            pgbilzeit, pgisced11,     #years of schooling and degree (education) 
                            pgjobend,                 #reason job change
                            pgpartz, pgpartnr, pgfamstd, #partner and relation status
                            pglabgro, pglabnet) %>%      #income
                            mutate(pid = zap_labels(pid)) %>% 
                  mutate(pgegp88 = ifelse(pgegp88 < 1, NA, pgegp88)) %>% 
                  mutate(pgisco88 = ifelse(pgisco88 < 1, NA, pgisco88))


# testing for availability of job classification, at least n=1500 missing and not able to impute for cog_test_sgl
null_ids <- pgen_cut %>%
  group_by(pid) %>%
  summarise(
    n_obs = sum(!is.na(pgisco88)),
    n_years = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs == 0) %>%
  pull(pid)

final_ids <- cog_test_sgl$pid

length(intersect(final_ids, null_ids))

missing_ids <- intersect(final_ids, null_ids)

final_occ_ids <- cog_test_sgl %>%
  group_by(pid) %>%
  summarise(
    n_obs = sum(!is.na(occljob)),
    n_years = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs == 1) %>%
  pull(pid)
  
length(intersect(missing_ids, final_occ_ids))




## creating HEALTH dataframe

health_cut <- health %>% select(cid, pid, syear, #mcs=mental, pcs=physical 
                                mcs, pcs) %>%
                         filter(syear >= 2000 & syear <= 2016)


## creating BIOJOB dataframe

biojob_cut <- biojob %>% select(cid, pid, 
                                bioyear, agefjob, occfjob, fjblue, fjselfe, fjwhite, fjcivs, ageatmv, 
                                yearlast, scopelj, occljob, ljblue, ljselfe, ljwhite, ljcivs)
  
#anti_join(check, biojob_cut, by = c("pid"))



## ## creating FULL dataset

df_cog_full <- cognit %>% left_join(ppathl_cut) %>%
                          left_join(pgen_cut) %>% 
                          left_join(health_cut) %>% 
                          left_join(biojob_cut) %>% 
                          left_join(pl_cut) 

#saveRDS(df_cog_full, "df_cog_full.rds")



corevars <- c("f096", "f96t90s", "f099", "f99z90r", "sex", "age", "todjahr", 
              "pcs", "mcs", 
              "germborn", "birthregion_ew", "loc1989", "sampreg",
              "pdatt", "pdatm", "eintritt", "austritt", "letztbef", "parid", "partner", "partner_died",
              "pgstib", "employment_status", "paid_work_l7d", "retired_beg_prevy", "pgemplst", "pglfs",   #employment and labor force
              "pgbilzeit", "pgisced11",     #years of schooling and degree (education) 
              "pgjobend", "typ_job_end",                #reason job change
              "pgpartz", "pgpartnr", "pgfamstd", #partner and relation status
              "pglabgro", "pglabnet", 
              "bioyear", "agefjob", "occfjob", "fjblue", "fjselfe", "fjwhite", "fjcivs", "ageatmv", 
              "yearlast", "scopelj", "occljob", "ljblue", "ljselfe", "ljwhite", "ljcivs",
              "nr_close_friends", 
              "hrs_chores_weekd", "hrs_childcare_weekd", "hrs_edu_weekd", "hrs_repair_weekd",
              "frq_classics", "frq_cindisc", "frq_sport", "frq_arts", "frq_voluntr")

df_cog_test <- df_cog_full %>% filter(f96t90s >= 0 | f99z90r >= 0) %>%
                               filter(f096 != 2 & f099 != 3) %>%
                                mutate(age = round(((syear * 12 + pdatm) - (gebjahr * 12 + gebmonat)) / 12, 2)) %>% 
                                select(cid, pid, syear, f096, f96t90s, f099, f99z90r, sex, age, gebjahr, todjahr, 
                                       pcs, mcs, 
                                       pdatt, pdatm, eintritt, austritt, letztbef, parid, partner, partner_died,
                                       pgstib, employment_status, paid_work_l7d, retired_beg_prevy, pgemplst, pglfs,   #employment and labor force
                                       germborn,birthregion_ew, loc1989, sampreg,
                                       pgegp88,
                                       pgisco88,
                                       pgbilzeit, pgisced11,     #years of schooling and degree (education) 
                                       pgjobend, typ_job_end,                #reason job change
                                       pgpartz, pgpartnr, pgfamstd, #partner and relation status
                                       pglabgro, pglabnet, 
                                       bioyear, agefjob, occfjob, fjblue, fjselfe, fjwhite, fjcivs, ageatmv, 
                                       yearlast, scopelj, occljob, ljblue, ljselfe, ljwhite, ljcivs,
                                       nr_close_friends, 
                                       hrs_chores_weekd, hrs_childcare_weekd, hrs_edu_weekd, hrs_repair_weekd,
                                       frq_classics, frq_cindisc, frq_sport, frq_arts, frq_voluntr) %>%
                                  mutate(across(corevars, as.numeric)) %>%
                                  mutate(across(where(is.numeric), ~ ifelse(. < 0, NA, .))) %>%
                                  rename(start_ant = f096, 
                                         result_ant = f96t90s, 
                                         start_sdt = f099, 
                                         result_sdt = f99z90r, 
                                         birthyear = gebjahr,
                                         deathyear = todjahr, 
                                         surveymonth = pdatm, 
                                         surveyday = pdatt, 
                                         entry = eintritt, 
                                         exit = austritt, 
                                         finsurvey = letztbef, 
                                         partner_status = partner, 
                                         occ_status = pgstib, 
                                         labforce_status = pglfs, 
                                         edu_time = pgbilzeit, 
                                         occ_class = pgegp88,
                                         typ_degree = pgisced11, 
                                         typ_jobchange = pgjobend, 
                                         partner_indc = pgpartz, 
                                         partner_nr = pgpartnr, 
                                         marit_status = pgfamstd,
                                         gross_labinc = pglabgro, 
                                         net_labinc = pglabnet,
                                         occsyear = bioyear,
                                         age_occhange = ageatmv,
                                         year_lastjob = yearlast 
                                         ) 


#saveRDS(df_cog_test, "df_cog_test.rds")

cog_test_sgl <- df_cog_test %>% distinct(pid, .keep_all = TRUE) %>%  # check for correct filter! 
                                filter(age >= 55 & age <= 70) %>% 
                                filter(((occ_status == 13 | occ_status == 11 | occ_status >= 100) | 
                                       (employment_status == 1 | employment_status == 3 | employment_status == 5)) &
                                        employment_status != 2 & employment_status != 4)  %>% #filter for retired, in education or fulltime, excluding parttime
                                mutate(retired = ifelse((occ_status == 13 & paid_work_l7d == 0) | 
                                                        (employment_status == 5 & paid_work_l7d == 0), 1, 0)) %>% #condition on retired or partial retired with zero work
                                mutate(manual_worker = case_when(
                                  occ_class %in% 5:6 & occljob == 2 ~ NA_real_,
                                  occ_class %in% 5:6 & occljob == 1 ~ 1,
                                  occ_class %in% 5:6 & occljob %in% 3:4 ~ 0,       # check EPG Scale and match to bluecollar/whitecollar workers, define blue/whitecollar
                                  occljob == 2 & occ_class %in% 7:10 ~ 1,         # check <- cog_test_sgl %>% select(pid, syear, occ_class, pgisco88, ljblue, ljselfe, ljwhite, ljcivs, occljob, retired)
                                  occljob == 2 & occ_class %in% 1:4 ~ 0,          # pgisco88: NA = 67%, occ_class(EPG): NA = 45% -- focus on EPG class          
                                  occ_class %in% 7:10 | occljob == 1 ~ 1,
                                  occ_class %in% 1:4 | occljob %in% 3:4 ~ 0)) %>% 
                                mutate(sex = ifelse(sex < 0, NA, dplyr::recode(sex, '1' = 0, '2' = 1))) %>% #binary variable for eligibility for retirement (earliest possible)
                                mutate(sra = case_when(
                                  birthyear < 1947 ~ 63,
                                  birthyear == 1947 ~ 63 ,
                                  birthyear == 1948 ~ 63 ,
                                  birthyear == 1949 ~ 63 ,
                                  birthyear == 1950 ~ 63 ,
                                  birthyear == 1951 ~ 63 ,
                                  birthyear == 1952 ~ 63 , # retirement option at age 63 through long contribution until birthyear=1952
                                  birthyear == 1953 ~ 65 + 7/12,
                                  birthyear == 1954 ~ 65 + 8/12,
                                  birthyear == 1955 ~ 65 + 9/12,
                                  birthyear == 1956 ~ 65 + 10/12,
                                  birthyear == 1957 ~ 65 + 11/12,
                                  birthyear == 1958 ~ 66,
                                  birthyear == 1959 ~ 66 + 2/12,
                                  birthyear == 1960 ~ 66 + 4/12,
                                  birthyear == 1961 ~ 66 + 6/12)) %>%
                                mutate(early = ifelse(birthyear < 1954, 60, 63)) %>% 
                                mutate(eligible_sra = ifelse(age >= sra, 1, 0)) %>% 
                                mutate(eligible_early = ifelse(age >= early, 1, 0)) %>% 
                                mutate(retired_early = ifelse(retired == 1 & age < sra, 1, 0)) %>% 
                                mutate(long_retired = ifelse(retired == 1 & retired_beg_prevy != 1, 1, 0)) %>% 
                                mutate(born_abroad = ifelse(germborn == 2, 1, 0)) %>% 
                                mutate(live_east = ifelse(sampreg == 2, 1, 0)) %>% 
                                mutate(live_west = ifelse(sampreg == 1, 1, 0)) %>% 
                                mutate(married = ifelse(marit_status == 1, 1, 0))
                                  



#saveRDS(cog_test_sgl, "cog_test_sgl.rds")



cog_test_svrl <- df_cog_test %>% #distinct(pid, .keep_all = TRUE) %>%  # check for correct filter! 
                                filter(age >= 55 & age <= 70) %>% 
                                filter(((occ_status == 13 | occ_status == 11 | occ_status >= 100) | 
                                          (employment_status == 1 | employment_status == 3 | employment_status == 5)) &
                                         employment_status != 2 & employment_status != 4)  %>% #filter for retired, in education or fulltime, excluding parttime
                                mutate(retired = ifelse((occ_status == 13 & paid_work_l7d == 0) | 
                                                          (employment_status == 5 & paid_work_l7d == 0), 1, 0)) %>% #condition on retired or partial retired with zero work
                                mutate(manual_worker = case_when(
                                  occ_class %in% 5:6 & occljob == 2 ~ NA_real_,
                                  occ_class %in% 5:6 & occljob == 1 ~ 1,
                                  occ_class %in% 5:6 & occljob %in% 3:4 ~ 0,       # check EPG Scale and match to bluecollar/whitecollar workers, define blue/whitecollar
                                  occljob == 2 & occ_class %in% 7:10 ~ 1,         # check <- cog_test_sgl %>% select(pid, syear, occ_class, pgisco88, ljblue, ljselfe, ljwhite, ljcivs, occljob, retired)
                                  occljob == 2 & occ_class %in% 1:4 ~ 0,          # pgisco88: NA = 67%, occ_class(EPG): NA = 45% -- focus on EPG class          
                                  occ_class %in% 7:10 | occljob == 1 ~ 1,
                                  occ_class %in% 1:4 | occljob %in% 3:4 ~ 0)) %>% 
                                mutate(sex = ifelse(sex < 0, NA, dplyr::recode(sex, '1' = 0, '2' = 1))) %>% #binary variable for eligibility for retirement (earliest possible)
                                mutate(sra = case_when(
                                  birthyear < 1947 ~ 63,
                                  birthyear == 1947 ~ 63 ,
                                  birthyear == 1948 ~ 63 ,
                                  birthyear == 1949 ~ 63 ,
                                  birthyear == 1950 ~ 63 ,
                                  birthyear == 1951 ~ 63 ,
                                  birthyear == 1952 ~ 63 , # retirement option at age 63 through long contribution until birthyear=1952
                                  birthyear == 1953 ~ 65 + 7/12,
                                  birthyear == 1954 ~ 65 + 8/12,
                                  birthyear == 1955 ~ 65 + 9/12,
                                  birthyear == 1956 ~ 65 + 10/12,
                                  birthyear == 1957 ~ 65 + 11/12,
                                  birthyear == 1958 ~ 66,
                                  birthyear == 1959 ~ 66 + 2/12,
                                  birthyear == 1960 ~ 66 + 4/12,
                                  birthyear == 1961 ~ 66 + 6/12)) %>%
                                mutate(early = ifelse(birthyear < 1954, 60, 63)) %>% 
                                mutate(eligible_sra = ifelse(age >= sra, 1, 0)) %>% 
                                mutate(eligible_early = ifelse(age >= early, 1, 0)) %>% 
                                mutate(retired_early = ifelse(retired == 1 & age < sra, 1, 0)) %>% 
                                mutate(long_retired = ifelse(retired == 1 & retired_beg_prevy != 1, 1, 0)) %>% 
                                mutate(born_abroad = ifelse(germborn == 2, 1, 0)) %>% 
                                mutate(live_east = ifelse(sampreg == 2, 1, 0)) %>% 
                                mutate(live_west = ifelse(sampreg == 1, 1, 0)) %>% 
                                mutate(married = ifelse(marit_status == 1, 1, 0))

                              
                            

#saveRDS(cog_test_svrl, "cog_test_svrl.rds")





# plot share of retired by age

ggplot(data = cog_test_sgl, aes(x = age, fill=factor(retired))) + 
                            geom_histogram(bins=16, color="navy") 

ggplot(data = cog_test_sgl, aes(x = age, y = retired)) +
  stat_summary_bin(fun = mean, binwidth = 0.25, geom = "point") +
  stat_summary_bin(fun = mean, binwidth = 0.25, geom = "line") +
  scale_y_continuous(limits = c(0,1)) + 
  geom_vline(xintercept = 61, linetype = "dashed") +
  geom_vline(xintercept = 65, linetype = "dashed")


# more advanced retired by age
plot_data <- cog_test_sgl %>%
  mutate(age_bin = floor(age / 0.25) * 0.25) %>%   # 3-Monats-Bins
  group_by(age_bin) %>%
  summarise(
    share_retired = mean(retired),
    n = n()
  )

ggplot(plot_data, aes(x = age_bin, y = share_retired)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  labs(
    x = "Age",
    y = "Share retired"
  ) +
  theme_minimal() + 
  geom_vline(xintercept = 60, linetype = "dashed") 
  

ggplot(plot_data, aes(x = age_bin, y = share_retired)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal()


ggplot(plot_data, aes(x = age_bin, y = share_retired)) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_line(alpha = 0.6) +
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal()


# other plots: results by age and retired
ggplot(data = cog_test_svrl, 
       aes(x = age, y = result_ant)) + 
  geom_point(aes(colour = retired)) +
  geom_vline(xintercept = 60) +
    geom_smooth(method=lm, se=F)

check <- cog_test_svrl %>% filter(retired == 0 & age >= 60 & age <= 70)

ggplot(data = check, 
       aes(x = factor(manual_worker), y = result_sdt)) + 
      geom_boxplot() 
+ 
  facet_wrap(vars(manual_worker))

# difference in results retired vs. working is (highly) significant
t.test(result_sdt ~ retired, var.equal = TRUE, data = cog_test_sgl)
t.test(result_ant ~ retired, var.equal = TRUE, data = cog_test_sgl)



# difference by sex
t.test(result_sdt ~ sex, var.equal = TRUE, data = cog_test_sgl) #weakly significant, lower for women, low t

# difference by occupation
t.test(result_sdt ~ manual_worker, var.equal = TRUE, data = cog_test_sgl) #highly significant, lower for manual worker, low t




# barcharts and other descriptives

ggplot(data = cog_test_sgl, aes(x=factor(syear))) + geom_bar(fill="grey") # 2006 is most frequent year, then chronologically descending

ggplot(data = cog_test_sgl, aes(x=factor(syear), fill=factor(manual_worker))) + geom_bar(position = "fill") + ylab("proportion")

ggplot(data = cog_test_sgl, aes(x=birthyear, fill= factor(retired))) +
                            geom_bar() + 
                            scale_x_continuous(breaks = seq(1936, 1961, by = 5))
  
ggplot(data = cog_test_sgl, aes(x=retired)) + geom_bar(fill="lightblue4") # working: n=1000, retired: n=2000

ggplot(data = cog_test_sgl, aes(x=sex)) + geom_bar(fill="purple4") # men: n=1750, women:n=1250

ggplot(data = cog_test_sgl, aes(x=partner_status)) + geom_bar(fill="purple4") # partner: n = 2250, no partner: n = 700

ggplot(data = cog_test_sgl, aes(x= factor(marit_status))) + geom_bar(fill="purple4") # most married, rest divorced/widowed: n = 500, few singles, partner died: n = 25

ggplot(data = cog_test_sgl, aes(x=nr_close_friends)) + geom_bar(fill="pink")

ggplot(data = cog_test_sgl, aes(x= factor(manual_worker, exclude = NULL))) + geom_bar(fill="darkred") # manual workers n = 1000

ggplot(data = cog_test_sgl, aes(x= factor(edu_time, exclude = NULL))) + geom_bar(fill="darkblue") # number of those with uni degree is equal to those with only lower school degree!

ggplot(data = cog_test_sgl, aes(x= factor(typ_degree, exclude = NULL))) + geom_bar(fill="darkblue") # confirms, most frequent is ISCED2011 = 3 (upper secondary education), NA: n = 1500 

ggplot(data = cog_test_sgl, aes(x= factor(edu_time), fill=factor(manual_worker))) + geom_bar() # manual workers are found with persons having <13 years edutime



ggplot(data = cog_test_sgl, aes(x= factor(sex), y=age)) + geom_boxplot(fill="lightblue1") + facet_wrap(vars(sex)) # women are older

ggplot(data = cog_test_sgl, aes(x= factor(sex), y=pcs)) + geom_boxplot(fill="lightblue1") + facet_wrap(vars(sex)) # women pcs is slightly worse

ggplot(data = cog_test_sgl, aes(x= factor(sex), y=mcs)) + geom_boxplot(fill="lightblue1") + facet_wrap(vars(sex)) # mental health mcs seems to be the same

ggplot(data = cog_test_sgl, aes(x= factor(sex), y=edu_time)) + geom_boxplot(fill="lightblue3") + facet_wrap(vars(sex)) # women with slightly less education

ggplot(data = cog_test_sgl, aes(x= factor(sex), y=result_sdt)) + geom_boxplot(fill="lightgreen") + facet_wrap(vars(sex)) # no difference in test results




ggplot(data = cog_test_sgl, aes(x= hrs_chores_weekd, fill= factor(retired))) + 
                            geom_histogram(binwidth = 1) + 
                            scale_x_continuous(breaks = seq(0, 24, by = 1)) 

cog_test_sgl %>% pivot_longer(cols = c(hrs_chores_weekd, hrs_childcare_weekd, hrs_edu_weekd, hrs_repair_weekd),
                              names_to = "variable",
                              values_to = "value") %>%
  ggplot(aes(x = value, fill = factor(retired))) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 24, by = 1)) +
  facet_wrap(~ variable)


cog_test_sgl %>% pivot_longer(cols = c(frq_classics, frq_cindisc, frq_sport, frq_arts, frq_voluntr),
                              names_to = "variable",
                              values_to = "value") %>%
  ggplot(aes(x = value, fill = factor(retired))) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  facet_wrap(~ variable)

ggplot(data = cog_test_sgl, aes(x= factor(manual_worker), fill=factor(retired))) + 
                            geom_bar(position="fill") +
                            ylab("proportion")

ggplot(data = cog_test_sgl, aes(x= factor(sex), fill=factor(retired))) + 
                            geom_bar(position="fill") +
                            ylab("proportion")




# correlation matrix
cor_set <- cog_test_sgl %>% select(result_ant, result_sdt, age, pcs, mcs, surveyday, surveymonth, edu_time, gross_labinc, net_labinc, 
                                   agefjob, age_occhange, nr_close_friends, hrs_chores_weekd, hrs_childcare_weekd, hrs_edu_weekd, hrs_repair_weekd,
                                   frq_classics, frq_cindisc, frq_cindisc, frq_sport, frq_arts, frq_voluntr, retired, manual_worker, sex)
#round(cor(cor_set, use = "pairwise.complete.obs"), 2)

cormatrix <- cor(cor(cor_set, use = "pairwise.complete.obs"))
corrplot(cormatrix, diag = F, method="ellipse", order="hclust", hclust.method="average")


cordetail <- corr.test(cor_set,
                  use = "pairwise",
                  method = "spearman")

corrplot(cordetail$r,
         p.mat = cordetail$p,
         sig.level = 0.05,
         method = "color",
         type = "lower",
         tl.col = "black",
         insig = "blank")

datasummary_correlation(cor_set, method = "spearman")



