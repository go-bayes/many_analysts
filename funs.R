# funs.R
# functions used in this analysis
data_read_mymachine <- function() {
  out <-
    read.csv("/Users/jbul176/Documents/GitHub/many_analysts/MARP_data_blinded.csv")
}

get_rel_vars <- function(x) {
  df <- x %>% dplyr::select(c(starts_with("rel_"), "country"))
}

get_wb_gen <- function(x) {
  df <- x %>% dplyr::select(c(starts_with("wb_gen"), "country"))
}

get_wb_phys <- function(x) {
  df <- x %>% dplyr::select(c(starts_with("wb_phys"), "country"))
}

get_wb_psych <- function(x) {
  df <- x %>% dplyr::select(c(starts_with("wb_psych"), "country"))
}


get_wb_soc <- function(x) {
  df <- x %>% dplyr::select(c(starts_with("wb_soc"), "country"))
}




# this code is for citing packages
cite_packages <- function() {
  citation(package = "base", lib.loc = "/Users/josephbulbulia/Dropbox/BIBS")
  toLatex(sessionInfo(), locale = FALSE)
  sapply(names(sessionInfo()$otherPkgs), function(x)
    print(citation(x), style = "Bibtex"))
  out <- sapply(names(sessionInfo()$otherPkgs),
                function(x)
                  print(citation(x), style = "Bibtex"))
  print(out)
}
#
# corrmat_rel() <- d %>%
#   dplyr::select(
#     R_attendance,
#     R_religiousID,
#     R_member, # can lose this
#     R_god,
#     R_afterlife,
#     R_prayer)%>%
#   dplyr::mutate(R_attendance.S = scale(R_attendance),
#                 R_religiousID.S =scale(R_religiousID),
#                 R_member.S = scale(R_member), # not used
#                 R_god.S=scale(R_god),
#                 R_prayer.S =scale(R_prayer),
#                 R_afterlife.S =scale(R_afterlife))%>%
#   rowwise() %>%
#   mutate(REL= mean(c(R_attendance, R_prayer,R_god,R_religiousID,R_afterlife), na.rm=T))%>% # we don't use this, but my sense is R_member isn't going to be worth including.
#   mutate(REL.S = scale(REL))%>% # we don't use this
#   select(R_attendance.S,R_religiousID.S,R_member.S,R_god.S,R_afterlife.S,R_prayer.S) # don't select religiosity for PCA
# # corrplot
# res <- cor(corrmat, method="pearson")
# corrplot::corrplot(res, method= "color", order = "hclust") # tl.pos = 'n'
#
#







#function for cleaning this dataset for any arbitrary number of waves
# data clean
data_clean <- function(x) {
  x %>%
    dplyr::filter(YearMeasured == 1) %>%
    dplyr::filter(Wave == 2017 | Wave == 2018) %>%
    dplyr::mutate(Postattack = as.factor(ifelse(TSCORE > 3545, 1, 0))) %>%
    dplyr::select(
      TSCORE,
      Wave,
      years,
      Id,
      Warm.Muslims,
      Warm.Immigrants,
      Warm.Asians,
      KESSLER6,
      Age,
      Male,
      Relid,
      Pol.Orient,
      Urban,
      GenCohort,
      Edu,
      Terrorism.Anxiety,
      YearMeasured,
      Postattack,
      LIFESAT,
      PWI,
      EthnicCats,
      NZdep,
      Partner,
      Pol.Orient,
      Urban,
      Euro,
      Religious,
      NZdep,
      WSCORE,
      Muslim
    ) %>%
    dplyr::filter(Muslim == 0) %>%  # filter 31 Muslims
    dplyr::group_by(Id) %>% filter(n() > 1) %>%
    dplyr::filter(n() != 0) %>%
    dplyr::ungroup(Id) %>%
    dplyr::mutate(
      Urban = (Urban),
      Euro = (Euro),
      Religious = as.factor(Religious),
      Postattack = as.factor(Postattack)
    ) %>%
    dplyr::mutate(
      WSCORE = as.factor(WSCORE),
      EduC = scale(as.numeric(Edu), scale = F, center = TRUE),
      AgeDecadeC = scale(Age, scale = F, center = T) / 10,
      K6sum = as.integer(KESSLER6 * 6),
      NZdepS = scale(NZdep),
      Condition = factor(
        ifelse(Wave == 2017, -1, ifelse(Wave == 2018 &
                                          Postattack == 0, 0, 1)),
        labels = c("Baseline", "PreAttack", "PostAttack")
      ),
      PolOrientS = scale(Pol.Orient, scale = T, center = T),
      RelidS = scale(Relid, scale = T, center = T),
      Conservative = Pol.Orient,
      Conservative_S = PolOrientS
    )
  
}


data_clean_long <- function(x) {
  x %>%
    dplyr::filter(YearMeasured == 1) %>%
    dplyr::filter(Wave == 2012 |
                    Wave == 2013 |
                    Wave == 2014 |
                    Wave == 2015 |
                    Wave == 2016 |
                    Wave == 2017 |
                    Wave == 2018) %>%
    dplyr::filter(Muslim == 0) %>%  # filter 31 Muslims
    dplyr::group_by(Id) %>%
    filter(n() > 6) %>%
    dplyr::filter(n() != 0) %>%
    dplyr::ungroup(Id) %>%
    dplyr::mutate(Postattack = as.factor(ifelse(TSCORE > 3545, 1, 0))) %>%
    dplyr::select(
      TSCORE,
      Wave,
      years,
      Id,
      Warm.Muslims,
      Warm.Immigrants,
      Warm.Asians,
      KESSLER6,
      Age,
      Male,
      Relid,
      Pol.Orient,
      Urban,
      GenCohort,
      Edu,
      Terrorism.Anxiety,
      YearMeasured,
      Postattack,
      LIFESAT,
      PWI,
      EthnicCats,
      NZdep,
      Partner,
      Pol.Orient,
      Urban,
      Euro,
      Religious,
      NZdep,
      WSCORE,
      Muslim
    ) %>%
    dplyr::mutate(
      Urban = (Urban),
      Euro = (Euro),
      Religious = as.factor(Religious),
      Postattack = as.factor(Postattack)
    ) %>%
    dplyr::mutate(Condition = factor(
      ifelse(Wave != 2018,-1, ifelse(Wave == 2018 &
                                       Postattack == 0, 0, 1)),
      labels = c("Baseline", "PreAttack", "PostAttack")
    )) %>%
    dplyr::filter(Condition != "PostAttack") %>%  # want to assess trend prior to attack%>%
    dplyr::mutate(
      WSCORE = as.factor(WSCORE),
      EduC = scale(as.numeric(Edu), scale = F, center = TRUE),
      AgeDecadeC = scale(Age, scale = F, center = T) / 10,
      K6sum = as.integer(KESSLER6 * 6),
      NZdepS = scale(NZdep),
      
      PolOrientS = scale(Pol.Orient, scale = T, center = T),
      RelidS = scale(Relid, scale = T, center = T),
      Conservative = Pol.Orient,
      Conservative_S = PolOrientS,
      yearsR = years - min(years)
    )
}


data_model_3 <- function(x) {
  x %>%
    dplyr::filter(YearMeasured == 1) %>%
    dplyr::filter(Wave == 2012 |
                    Wave == 2013 |
                    Wave == 2014 |
                    Wave == 2015 |
                    Wave == 2016 |
                    Wave == 2017 |
                    Wave == 2018) %>%
    dplyr::filter(Muslim == 0) %>%  # filter 31 Muslims
    dplyr::group_by(Id) %>%
    filter(n() > 6) %>%
    dplyr::filter(n() != 0) %>%
    dplyr::ungroup(Id) %>%
    dplyr::filter(Wave == 2017 | Wave == 2018) %>%
    dplyr::mutate(Postattack = as.factor(ifelse(TSCORE > 3545, 1, 0))) %>%
    dplyr::select(
      TSCORE,
      Wave,
      years,
      Id,
      Warm.Muslims,
      Warm.Immigrants,
      Warm.Asians,
      KESSLER6,
      Age,
      Male,
      Relid,
      Pol.Orient,
      Urban,
      GenCohort,
      Edu,
      Terrorism.Anxiety,
      YearMeasured,
      Postattack,
      LIFESAT,
      PWI,
      EthnicCats,
      NZdep,
      Partner,
      Pol.Orient,
      Urban,
      Euro,
      Religious,
      NZdep,
      WSCORE,
      Muslim
    ) %>%
    dplyr::mutate(
      Urban = (Urban),
      Euro = (Euro),
      Religious = as.factor(Religious),
      Postattack = as.factor(Postattack)
    ) %>%
    dplyr::mutate(Condition = factor(
      ifelse(Wave != 2018,-1, ifelse(Wave == 2018 &
                                       Postattack == 0, 0, 1)),
      labels = c("Baseline", "PreAttack", "PostAttack")
    )) %>%
    dplyr::mutate(
      WSCORE = as.factor(WSCORE),
      EduC = scale(as.numeric(Edu), scale = F, center = TRUE),
      AgeDecadeC = scale(Age, scale = F, center = T) / 10,
      K6sum = as.integer(KESSLER6 * 6),
      NZdepS = scale(NZdep),
      PolOrientS = scale(Pol.Orient, scale = T, center = T),
      RelidS = scale(Relid, scale = T, center = T),
      Conservative = Pol.Orient,
      Conservative_S = PolOrientS,
      yearsR = years - min(years)
    )
}




# This is the code to show hos many muslims there were in 2017. We remove them from the analysis because we are interested in anti-Muslim prejudice among non-muslims
# km<-ldf.5%>%
#   dplyr::filter(YearMeasured==1)%>%
#   dplyr::filter(Wave ==2017 |Wave ==2018)%>%
#   dplyr::mutate(Postattack=as.factor(ifelse(TSCORE>3545,1,0)))%>%
#   dplyr::select(TSCORE,Wave,years, Id, Warm.Muslims,KESSLER6,Age,
#                 Male,Relid,Pol.Orient,Urban,GenCohort,Edu,Terrorism.Anxiety,YearMeasured,Postattack,
#                 LIFESAT,PWI,EthnicCats,NZdep,Partner,Pol.Orient,Urban,Euro,Religious,NZdep,WSCORE,Muslim)
#
# library(table1)  # make pretty tables quickly
# table1::table1(~Muslim|Wave, data = km, overall=F)# there are 31 muslims in 2017

#
# # home ownership dataset
# home_owner_data_clean <- function(x){
#   out <- x %>%
#     dplyr::select(HomeOwner,Wave,Issue.IncomeRedistribution,Id,YearMeasured, Pol.Orient,years, Age, NZdep)%>%
#     #  filter(Wave != 2009| Wave != 2010| Wave != 2011| Wave != 2010| Wave != 2012| Wave != 2013| Wave != 2014| Wave != 2016| Wave != 2017) %>%
#     dplyr::filter(YearMeasured == 1)%>%
#     filter(Wave == 2015| Wave == 2018) %>%
#     droplevels()%>%
#     dplyr::group_by(Id) %>%
#     dplyr::filter(n() > 1)%>% # select those who have responded to at least y waves #
#     dplyr::ungroup(Id)%>%
#     filter(!is.na(HomeOwner))%>%
#     dplyr::mutate(Age.10yrs = (Age/10))%>%
#     filter(!is.na(Issue.IncomeRedistribution))%>%
#     dplyr::group_by(Id) %>%
#     dplyr::filter(n() > 1)%>% # select those who have responded to at least y waves #
#     dplyr::ungroup(Id) %>%
#     dplyr::mutate(Political_Conservativism_S = scale(Pol.Orient, center = TRUE, scale = TRUE))%>%
#     dplyr::mutate(years_R = years - min(years))%>%
#     dplyr::mutate(Income_Redistribution = Issue.IncomeRedistribution) %>%
#     dplyr::mutate(Political_Conservativism = Pol.Orient)
#
#   return(out)
# }
#
#
#
# # function for showing unique ids
# show_unique_id <- function(x){
#   numb <- length(unique(x)) # count # of ids
#   print(numb)
# }
#
# # Function to tally number of responses for the ids
# count_waves_participants <- function(x){
#   out<-dplyr::count(tally(group_by(x, Id), sort = TRUE, name="number_waves"), number_waves)
#   print(out)
# }
#
#
# # demographic table
# demographic_table <- function(x){
#   table1::table1(~ Age +
#                    NZdep +
#                    Education +
#                    Employed +
#                    Ethnic_Categories +
#                    Male  +
#                    Has_Partner  +
#                    Political_Conservativism +
#                    Urban +
#                    Income_Redistribution|Wave, data = x,
#                  overall = F)
# }
#
#
# graph_predictions_one <- function( x, y, t){
#   out <- ggeffects::ggpredict(model = x, terms = c(y),facet=TRUE)
#   plot(out, facets = F) +  theme_pubr() + ggtitle(t) # title to be suppled  +  gghighlight::gghighlight()  d
# }
#
# graph_predictions <- function( x, y, z, t ){
#   out <- ggeffects::ggpredict(model = x, terms = c(y,z),facet=TRUE)
#   plot(out, facets = F) +  theme_pubr() + ggtitle(z) # title to be suppled  +  gghighlight::gghighlight()  d
# }
#
# get_predictions <- function(x,y){
#   out <- ggeffects::ggpredict(model = x, terms = c(y),
#                               ci.lvl = 0.95,
#                               type = "fe",
#                               typical = "mean",
#                               back.transform = TRUE,
#                               ppd = FALSE,
#                               interval = "confidence")
#   return(out)
# }
#
#
#
# # markov model
# library("msm")
# run_markov_model_two <- function(x,y,z,id,dat){
#   # create Q matrix
#   Q0 <- rbind(c(.9,.1),
#               c(.1,.9))
#   crudeinR<-crudeinits.msm(x ~ y, Id, data=dat, qmatrix=Q0)
#   # Rmod.mnm.0  <- msm(BelieveGod1 ~ years,  Id, data=ldf.5, qmatrix=crudeinG, covariates = ~Age, gen.inits=TRUE)
#
#   out  <- msm(x ~ y, id,
#               data=df,
#               covariates =  ~ z,
#               qmatrix = crudeinR,
#               ematrix = rbind(c(.1,.1),c(.1,.1)),
#               est.initprobs = TRUE)
#   return(out)
# }
#
# # latex model function
# table_model_latex_pwi <- function(x){ # x is a model
#   xtract <-texreg::extract(
#     x,
#     level = 0.95,
#     include.random = TRUE,
#     include.rsquared = F,
#     include.nobs = T,
#     include.loo.ic = F,
#     include.waic = F)
#   texreg(list(xtract),
#          custom.model.names = c("PWI"),
#          caption = "Personal Wellbeing",
#          sideways = F,
#          scalebox = .5,
#          #fontsize= "footnotesize",
#          label = "tab:REGRESS_PWI",
#          ci.force.level = 0.95, bold = 0.05,
#          settingstars = 0,
#          booktabs = TRUE,
#          custom.note ="")
# }
# # table function
# table_model_latex_ls <- function(x){ # x is a model
#   xtract <-texreg::extract(
#     x,
#     level = 0.95,
#     include.random = TRUE,
#     include.rsquared = F,
#     include.nobs = T,
#     include.loo.ic = F,
#     include.waic = F)
#   texreg(list(xtract),
#          custom.model.names = c("Life Sat"),
#          caption = "Life Satisfaction",
#          sideways = F,
#          scalebox = .5,
#          #fontsize= "footnotesize",
#          label = "tab:REGRESS_LS",
#          ci.force.level = 0.95, bold = 0.05,
#          settingstars = 0,
#          booktabs = TRUE,
#          custom.note ="")
# }
#
# # imputation functions
# amelia_imputation_clean <- function(x){
#   set.seed(1234)
#   # x is the longitudional dataframe (here 'd_3')
#   prep <- x %>% # Remove what we do not need anymore
#     dplyr::select(c(Beliefs,   # to predict missinglness
#                     Age,
#                     Id,
#                     Education,
#                     NZdep,
#                     KESSLER6,
#                     LIFESAT,
#                     PWI,
#                     Political_Conservativism,
#                     Relid,
#                     Ethnic_Categories,
#                     Partner,
#                     Employed,
#                     Male,
#                     Urban,
#                     years,
#                     Wave))
#   # impute missing
#   prep <- as.data.frame(prep) # tibble won't run in amelia
#   prep2 <- Amelia::amelia(
#     prep, #dataset to impute
#     m = 10, # number of imputations
#     cs= c("Id"),
#     ts= c("years"),
#     noms = c("Ethnic_Categories",
#              "Urban",
#              "Partner",
#              "Male",
#              "Employed",
#              "Beliefs"),
#     idvars=c("Wave","PWI","LIFESAT"), # not imputing outcomes
#     polytime = 3)  #https://stackoverflow.com/questions/56218702/missing-data-warning-r
#   prep3<- transform.amelia(prep2,
#                            Age.10yrs = (Age/10),
#                            yearsC = scale(years, center=TRUE,scale=FALSE),
#                            Political_Conservativism_S = scale(Political_Conservativism,center=TRUE,scale=TRUE),
#                            Employed = factor(Employed),
#                            Ethnic_Categories = as.factor(Ethnic_Categories),
#                            Urban = as.factor(Urban),
#                            Deprivation_S = scale(NZdep, scale=TRUE, center=TRUE),
#                            Education_S = scale(Education, scale =TRUE,center=TRUE),
#                            Male = as.factor(Male),
#                            PWI = as.numeric(PWI),
#                            LIFESAT = as.numeric(LIFESAT),
#                            Has_Partner = as.factor(Partner),
#                            Beliefs = as.factor(Beliefs),
#                            Id =as.factor(Id))
#   # center an d scale age
#   out <- transform.amelia(prep3,Age_in_Decades_C = scale(Age.10yrs,scale =FALSE, center=TRUE))
#   return(out)
# }
#
#
#
# # run models iterating over imputed data
# loop_lmer_model <- function(x,y){
#   m <- 10
#   mod <- NULL
#   for(i in 1:m) {
#     mod[[i]] <- lmer(x, data=y$imputations[[i]])
#   }
#   return(mod)
# }
#
#
# # table of effects
# loop_lmer_model_tab <- function(x){
#   mp<-lapply(x, model_parameters)
#   out<- parameters::pool_parameters(mp)
#   return(out)
# }
#
#
#
# ## imputation prediction plot
# ## note we just pick the tenth iteration until there is something better
#
#
# graph_predictions_imputed <-function( x, y){  # x = model objects
#   m<-10
#   out<-NULL
#   for(i in 1:m) {
#     out[[i]] <- ggpredict(x[[i]], terms =c("Beliefs","years [0:9]"))
#   }
#   plots<-NULL
#   for(i in 1:m) {
#     plots[[i]] <- plot(out[[i]], facets = T) # + scale_y_continuous(limits=c(6.35,6.85) )
#   }
#   plots[[10]] + gghighlight() + ggtitle(y)
# }
#
#
# create_hux_table<-function(x){
#   as_hux(x)%>%
#     select("Parameter", "Coefficient", "CI_low","CI_high", "p") %>%
#     set_number_format(3)%>%
#     set_left_padding(20)%>%
#     set_bold(1,everywhere)#%>% # to create lates
#   #quick_latex()
# }
#
#
