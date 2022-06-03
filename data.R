path = "C:/Users/a/Documents/R/Projet/TerraformingMars/"

## Liste des cartes ----
cartes = openxlsx::read.xlsx(paste0(path,"cartes.xlsx"),sheet = "cartes",
                             startRow=1,colNames=TRUE) %>% as.data.frame()
nrow = nrow(cartes)
cartes = cartes %>%
  mutate(
    joueur = rep("",nrow),
    where = rep("P",nrow),
    sur_cette_carte_type = rep("",nrow),
    sur_cette_carte_nombre = rep(0,nrow),
    check= TRUE,
    ok = FALSE) #%>%
 #  filter(type_carte %in% c(21,22))

effet_cartes = openxlsx::read.xlsx(paste0(path,"cartes.xlsx"),sheet = "effet_immediat",
                                   startRow=1,colNames=TRUE)%>%as.data.frame()

prerequis = openxlsx::read.xlsx(paste0(path,"cartes.xlsx"),sheet = "prerequis",
                                startRow=1,colNames=TRUE) %>%
  as.data.frame()



actions21 = openxlsx::read.xlsx(paste0(path,"cartes.xlsx"),
                            sheet = "actions21",
                            startRow=1,colNames=TRUE)%>%
  as.data.frame() %>%
  mutate(vise = "self")%>%
  mutate(en_clair = case_when(
              # nombre == 0 ~ "gratuit",
              TRUE ~ paste(nombre,unite,type)),
         id_idaction = paste0(id,"_",idaction))%>%
  group_by(id_idaction)


effet22 = openxlsx::read.xlsx(paste0(path,"cartes.xlsx"),sheet = "effets22_bleues",
                                     startRow=1,colNames=TRUE)%>%  as.data.frame() %>%
  mutate(vise = "self")

restrictions_placement = openxlsx::read.xlsx(paste0(path,"cartes.xlsx"),sheet = "restrictions_placement",
                                             startRow=1,colNames=TRUE)%>%  as.data.frame()

points_speciaux = openxlsx::read.xlsx(paste0(path,"cartes.xlsx"),sheet = "points_speciaux",
                                      startRow=1,colNames=TRUE)%>%  as.data.frame()

## PLATEAU ----

plateau = openxlsx::read.xlsx(paste0(path,"cartes.xlsx"),sheet = "plateau",
                             startRow=1,colNames=TRUE)%>%  as.data.frame()
plateau = plateau %>%
  mutate(Ocean = case_when(is.na(Ocean)~FALSE,
                           Ocean==1 ~ TRUE,
                           TRUE ~ FALSE),
         Libre = TRUE,
         Proprio = "",
         Type_tuile = "",
         Libre_ville = TRUE,
         Libre_foret_J1 = TRUE,
         Libre_foret_J2 = TRUE,
         Num = paste(Lat,Long,sep="/"),
         Source = "" 
  )

bonus_placement = openxlsx::read.xlsx(paste0(path,"cartes.xlsx"),
                                      sheet = "bonus_placement",
                                       startRow=1,colNames=TRUE)%>%  as.data.frame()%>%
  mutate(Num = paste(Lat,Long,sep="/"))

# Projets standards
projets_std = openxlsx::read.xlsx(paste0(path,"cartes.xlsx"),
                                      sheet = "std",
                                      startRow=1,colNames=TRUE)%>%
  as.data.frame()%>%
  mutate(vise = "self")

objectifs = openxlsx::read.xlsx(paste0(path,"cartes.xlsx"),
                                sheet = "obj",
                                startRow=1,colNames=TRUE)%>%
  as.data.frame()%>%
  mutate(
    joueur="J",
    possible = FALSE,
    valide = "",
    vise = "self")






### SUMS ----
# prerequis
sum_prerequis = prerequis %>%
  group_by(id)%>%
  mutate(prerequis = paste0(nombre," ",type," ",valeur))%>%
  select(-c(valeur,type,nombre))%>%
  group_by(id)%>%
  summarise(prerequis = paste(prerequis, collapse = ", "))

sum_prerequis_icone = prerequis %>%
  group_by(id)%>%
  f_icone_prerequis()%>%
  summarise(icone_pr = paste(icone_pr, collapse = ", "))


# effets immédiats
sum_effet = effet_cartes %>%
  group_by(id)%>%
  mutate(effets_immediats = paste0(nombre," ",type," ",unite))%>%
  select(-c(unite,type,nombre))%>%
  group_by(id)%>%
  summarise(immediat = paste(effets_immediats, collapse = ", "))

sum_effet_icone = effet_cartes%>%
  group_by(id) %>%
  f_icone()%>%
  summarise(icone = paste(icone, collapse = ""))

# cartes à effet22
sum_effet22 = effet22 %>%
  group_by(id)%>%
  select(c(id,effet_en_clair))%>%
  group_by(id)%>%
  summarise(effet = paste(effet_en_clair, collapse = ", "))
sum_effet22_icone = effet22 %>%
  f_icone_effets22()%>%
  group_by(id)%>%
  summarise(icone_e = paste(icone_e, collapse = ""))


# # cartes actions
sum_actions1 = actions21 %>%
  group_by(id_idaction,id)%>%
  summarise(clair = paste(en_clair, collapse = ", "),
            .groups = "keep")
sum_actions = sum_actions1 %>%
  as.data.frame()%>%
  group_by(id)%>%
  summarise(action = paste(clair, collapse = " OU "),
            .groups = "keep")


sum_actions_ic = actions21%>%
  f_icone()%>%
  f_icone_action21_fleche()

sum_actions_icone = sum_actions_ic %>%
  group_by(id)%>%
  summarise(icone = paste(icone, collapse = " OU "), .groups = "keep")%>%
  mutate(icone_a = icone)%>%
  select(-icone)

sum_actions_icone_id_idaction = sum_actions_ic %>%
  group_by(id_idaction)%>%
  summarise(icone = paste(icone, collapse = " OU "), .groups = "keep")%>%
  mutate(icone_a = icone)%>%
  select(-icone)


# icones des badges
sum_badges_icone = cartes %>%
  f_icone_badges()%>%
  group_by(id)%>%
  summarise(icone_b = paste(icone_b, collapse = ""), .groups = "keep")

