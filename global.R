# all_cartes = cartes %>% full_join(effet_cartes)

# Opérateur spersos
'%!in%' <- function(x,y)!('%in%'(x,y))
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
`%-=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 - e2))
## x %+=% 2 # équivalent à x=x+2

options(encoding = "UTF-8")



# display debugging messages in R (if local) 
# and in the console log (if running in shiny)
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}



# fonction qui va chercher le Nième élément d'une liste en wrapant !
f_cherche_wrap = function(liste,numero){
  len = length(liste)
  num_temp = numero
  if(num_temp <= len){
    return(liste[num_temp])
  }else{
    while(num_temp > len){
      num_temp = num_temp-len
    }
    return(liste[num_temp])
  }
}


## Initialisation des variables réactives mdf$ ----
f_init_jeu = function(mdf, nombre){
  # nb_joueurs = 2
  mdf$nb = nombre
  mdf$statut = "actif"
  mdf$plateau = plateau #f_init_plateau()
  
  mdf$plat_en_main = plateau %>% filter(Lat==0) %>%
    mutate(id_carte=0)
  
  mdf$navire = f_init_tous(nombre_joueurs=isolate(mdf$nb),
                          fonction = f_init_navire())
  
  mdf$tuile=""         # indique si implique de placer une tuile, et si oui laquelle
  mdf$tuile_nb = 1
  mdf$tuile_source = ""
  # mdf$tuile = "vi"
  # mdf$tuile_nb = 1
  # mdf$tuile_source = 60
  mdf$sur_carte = data.frame()
 # mdf$stand_by = FALSE # indique si le jeu est en attente du placement d'une tuile
  mdf$cartes = f_debut(session,
                      cartes=cartes,
                      navire=isolate(mdf$navire),
                      gratuit=TRUE,
                      nb_cartes=10)
  mdf$cartes_tuile = isolate(mdf$cartes) %>%
    left_join(effet_cartes, by = "id")%>%
    filter(type =="tuile")%>%
    left_join(restrictions_placement, by = "id")
  # mdf$params_joueur = data.frame(
  #   nom=unique(isolate(mdf$navire)$joueur),
  #   NT=20,
  #   statut="actif",
  #   nom_complet="",
  #   couleur=liste_couleurs$hex[1])
  mdf$params_joueur = data.frame(
    nom=c("J1","J2"),
    NT=20,
    statut="actif",
    nom_complet="",
    couleur=c(liste_couleurs$hex[1],liste_couleurs$hex[2])
  )
  
  
  mdf$actions21_prepa = isolate(mdf$cartes)# %>% filter(id_carte==0)
  mdf$nb_generations = 1
  # mdf$global = data.frame(
  #   oxyg = 13,
  #   temp = 7,
  #   ocean = 8)#,
  ###   venus = 0
  mdf$global = data.frame(
    oxyg = 0,
    temp = -30,
    ocean = 0)#,
  
  mdf$j_actif = isolate(mdf$params_joueur)[1,1]
  
  mdf$cartes_en_main_achetables = isolate(mdf$cartes) %>% filter(id==0)
  mdf$cartes_en_main = isolate(mdf$cartes) %>% filter(id==0)
  mdf$effet22_jeu = isolate(mdf$cartes) %>% filter(id==0)
  mdf$actions21_en_jeu = isolate(mdf$cartes) %>% filter(id==0)
  mdf$actions21_pour_affich = isolate(mdf$cartes) %>% filter(id==0)
  mdf$badges_joueurs = f_init_tous(nombre_joueurs=isolate(mdf$nb),
                                  fonction = f_init_badge())
  mdf$objectifs = f_init_tous(nombre_joueurs=isolate(mdf$nb),
                             fonction = f_init_objectifs())
  mdf$objectifs_persos = isolate(mdf$objectifs)
  
  mdf$bouton = h6("")
  mdf$PV = data.frame(
    joueur = "J0",
    PV = 0
  )
  mdf$test = reactiveValues(n = 0)
  mdf$testt = 0
  mdf$cartes_devant = isolate(mdf$cartes) %>% filter(id ==0)
  return(mdf)
}


# Liste des actions possibles ----
moves  <- data.frame(
  move = c("crt","std","obj","rcp","act","8pl","8ch","pss","end"),
  move_name = c("Jouer une carte de sa main",
                "Réaliser un projet standard",
                "Valider un objectif",
                "Financer une récompense",
                "Utiliser l'action d'une carte Action (bleue)",
                "Convertir 8 plantes pour placer une tuile forêt",
                "Convertir 8 chaleur pour faire augmenter la température",
                "Passer son tour pour l'instant",
                "Ne plus rien faire de la génération")
  )

# liste des objectifs
f_init_objectifs = function(){
return(objectifs)
}

# Liste des projets standards ----
# projets_std <- data.frame(
#   std = c("vi","fo","en","tp","oc"),
#   std_name = c("Poser une ville","Poser une forêt","Energie","Température","Océan"),
#   cost = c(25,23,11,14,18)
# )


### Initialisation de la partie ----

# Fonction pour remettre les navires à 0
f_init_navire= function(){
  nom = c("Argent","Acier","Titane",
          "Plantes","Energie","Chaleur")
  #flux = rep(0,6)
  flux = rep(1,6)
  stock = c(42,rep(0,5))
#  stock=rep(50,6)
  joueur = "J"
  navire = data.frame(nom,flux,stock,joueur)%>% 
    mutate_if(is.numeric, round, digits = 0)

  return(navire)
}

f_init_tous = function(nombre_joueurs=2,
                       fonction){
  mdf = fonction %>%
    mutate(joueur = paste0(joueur,0))
  for(i in 1:as.numeric(nombre_joueurs)){
    temp = fonction %>%
      mutate(joueur = paste0(joueur,i))
    mdf = mdf %>%
      full_join(temp) 
    rm(temp)
  }
  mdf = mdf %>%
    filter(joueur != "J0")
  return(mdf)
}
f_init_badge = function(){
  badge = c("Es","Co","Vi","Jo","Sc",
            "En","Te","Pl","Mi","Ve","An")
  joueur = "J"
  nombre = 0
  badge_joueur = data.frame(badge,joueur,nombre)%>% 
    mutate_if(is.numeric, round, digits = 0)
  return(badge_joueur)
}

# Fonction pour mettre toutes les cartes dans la pioche
f_reinit_cartes = function(cartes){
  cartes = cartes %>%
    mutate(where = "P", joueur="")  # toutes vont dans la pioche
 #   slice(sample(1:n())) # mélange aléatoirement les lignes = les cartes
}

f_shuffle_defausse = function(cartes){
  cartes[which(cartes$where=="D"),]$where ="P"
  return(cartes)
}


f_cartes_temp = function(cartes,
                         nb_cartes=1,
                         gratuit=TRUE,
                         joueur){
  player=joueur
  #f_cartes_temp permet à un joueur particulier de piocher
  #nb_cartes gratuites ou payantes
  #et renvoie le grand tableau cartes actualisé
  
  a = cartes %>% filter(where=="P") #cartes piochables
  b = cartes %>% filter(where!="P") #cartes pas piochables
  
  sample = sample(nrow(a),nb_cartes)
  a[(sample),]$joueur=joueur
  # on "pioche" nb_cartes, c'est-à-dire qu'on change son "where"
  if(gratuit == TRUE){
    #si gratuite, les cartes vont dans la main
    a[(sample),]$where="M"
    a[(sample),]$joueur=player
  } else {
    #si payantes, elle sont stockées en T pour temp (en attente d'être en main OU défaussées)
    a[(sample),]$where="T"
  }
  c = a %>%
    full_join(b, by = c("id", "nom", "cost", "points", "badges",
                        "type_carte", "joueur", "where", "sur_cette_carte_type",
                        "sur_cette_carte_nombre", "check", "ok"))
  # on récupère dans le tableau complet les cartes mises de côtés (dans b)
  # car elles n'étaient pas dispos à la pioche
  return(c)
}


f_check_badge = function(cartes,id_carte,badge){
  # Fonction qui renvoie TRUE si UNE carte numéro id_carte du grand tableau cartes porte un badge 
  ligne = cartes[which(cartes$id == id_carte),]
  if(nrow(ligne)>0){
    if(grepl(badge, ligne$badges[[1]], fixed = TRUE) == TRUE){
      return(TRUE)
    }else{return(FALSE)}
  }
}

# # Fonction qui modifie mdf$badges_joueur selon les badges présents (ou pas) sur la carte qui vient d'être jouée
# f_badges_carte_vers_joueur=function(cartes,badges_joueurs,badges_carte,joueur){
#   
#   if(length(badges_carte)>0){
#     
#     a = gsub('([[:upper:]])', ' \\1', badges_carte)
#     b = as.list(strsplit(a, " ")[[1]])[-1]
#     for(i in b){
#       badges_joueurs[which(badges_joueurs$joueur==joueur & badges_joueurs$badge == i),]$nombre %+=% 1
#     }
#   }
#   
#   return(badges_joueurs)
# }
# Fonction qui modifie mdf$badges_joueur selon les badges présents (ou pas) sur la carte qui vient d'être jouée
f_badges_carte_vers_joueur=function(mdf, id_carte){
  
  # Variables à consulter seulement
  cartes = mdf$cartes
  carte_p = cartes[which(cartes$id==id_carte),]
  joueur = mdf$j_actif
  
  # variable à modifier : mdf$badges_joueurs
  
  # Si la carte posée n'est pas un évènement
  if(carte_p$type_carte != 3){
    
    # et si elle possède des badges
    if(length(carte_p$badges)>0){
      
      a = gsub('([[:upper:]])', ' \\1', carte_p$badges)
      b = as.list(strsplit(a, " ")[[1]])[-1]
      
      # alors, pour chaque badge nouveau, on le rajoute à la liste des badges du joueur actif
      for(i in b){
        mdf$badges_joueurs[which(mdf$badges_joueurs$joueur==joueur &
                                 mdf$badges_joueurs$badge == i),]$nombre %+=% 1
        print(paste("Le joueur",joueur,"a gagné un badge",i))
      }
      rm(a,b)
    }
  }
  rm(joueur,cartes, carte_p)
  
  return(mdf)
}

# Fonction qui calcule le minimum d'acier/titane dispo pour les sliders (input$crt_poser = id_carte)
# elle renvoie un minimum
f_min_max_slider = function(navire, cartes, id_carte, joueur, badge){
  
  cout_carte = cartes[which(cartes$id==id_carte),]$cost

  stock_argent = navire[which(navire$joueur==joueur & navire$nom=="Argent"),]$stock
  
  if(badge=="Es"){
    stock_max = navire[which(navire$joueur==joueur & navire$nom=="Titane"),]$stock
    }else if(badge=="Co"){
    stock_max = navire[which(navire$joueur==joueur & navire$nom=="Acier"),]$stock
    }

  # Minimum : minimum de matériaux pour couvrir un manque d'argent
  reste_a_payer = stock_argent - cout_carte
  if(reste_a_payer >=0 ){
    minimum = 0
  }else if(reste_a_payer < 0){
    if(badge=="Es"){
      minimum = round(-reste_a_payer/3,digits=0)
    }else if(badge=="Co"){
      minimum =  round(-reste_a_payer/2,digits=0)
    }
  }
  # Maximum : le minimum entre le stock d'acier/titane réel
  # et l'acier/titane nécessaire pour payer en matériaux sans dépenser trop d'argent pour l'appoint
  if(badge=="Es"){
    max_materiaux = ceiling(cout_carte/3)
    valeur = cout_carte %/% 3      # division euclidienne
  }else if(badge=="Co"){
    max_materiaux =  ceiling(cout_carte/2)
    valeur = cout_carte %/% 2      # division euclidienne
  }
  maximum = min(stock_max, max_materiaux)
  
  liste = list(minimum, maximum, valeur)
  return(liste)
}


f_check_poser_cartes_de_sa_main = function(mdf){
  print("début f_check_poser_cartes")
  cartes = mdf$cartes %>%
    filter(joueur == mdf$j_actif,
           where == "M") %>% 
    mutate(ok = FALSE)
  
  if(length(cartes)>0){
   
    # a renvoie, à partir des cartes en main, la liste des cartes en main que le joueur actif peut se payer
    a = f_check_poser_argent(mdf, cartes = cartes)
    print("f_check_poser_cartes : fin check argent")
    
    # b renvoie, à partir de a, la liste des cartes dont les prérequis sont remplis
    b = f_check_poser_prerequis(mdf = mdf, cartes = a)
    print("f_check_poser_cartes : fin check prerquis")
    
    # c renvoie la liste des cartes dont on peut assumer les effets immédiats
    c = f_check_poser_effets(mdf = mdf, cartes = b)
    print("f_check_poser_cartes : fin check effet")
    
    
    # d renvoie mdf (pour mdf$plat_en_main)
    d = f_check_poser_tuile(mdf = mdf, cartes = c)#cartes=c, mdf$plateau, player = mdf$j_actif)
    print("f_check_poser_cartes : fin check tuile")
    
    e = d %>%
      group_by(id)%>%
      filter(joueur == mdf$j_actif, where == "M")%>%
      f_colonne_en_plus()%>%
      arrange(-ok)%>%
      select(-c(where, check, sur_cette_carte_nombre,
                sur_cette_carte_type, badges,
                joueur, id,
                prerequis, points, badges))%>%
      rename(prerequis = icone_pr,
             points = icone_p,
             badges = icone_b,
             immediat = icone,
             action = icone_a,
             effet = icone_e)
  }
  print("fin f_check_poser_cartes")
  return(e)
}
f_appliquer_reduction = function(mdf){
  # Variables observées
  # mdf$cartes_en_jeu = mdf$cartes[which(mdf$cartes$joueur==mdf$j_actif &
  #                                        mdf$cartes$where =="J"),]
  # 
  # mdf$effet22_jeu =  effet22[which(effet22$id %in% mdf$cartes_en_jeu$id),]
  # effet22_reduction = mdf$effet22_jeu[which(mdf$effet22_jeu$type_observe == "reduction"),]
  # 
  effet22_reduction = effet22 %>%
    filter(id %in% (mdf$cartes %>%
                      filter(joueur == mdf$j_actif)%>%
                      filter(where =="J"))$id)%>%
    filter(type_observe == "reduction")
  
  print(paste("effet22_reduction",effet22_reduction$id))
  
  # variable retournée cartes
  cartes2 = mdf$cartes

  if(nrow(effet22_reduction)>0){
    print(paste("Aujourd'hui, grande réduction spéciale !"))
    
    # # pour chaque carte dans sa main
    # for(i in effet22_reduction$id){
      
      reduction = 0
      
      # Etape 1 : réductions qui s'appliquent quelle que soit la carte ----
      # ici effet22_observe est liste des cartes effet22 en jeu
      # qui appliquent une reduction sur une carte sans tenir compte des badges
      effet22_observe_sans = effet22_reduction %>%
        filter(is.na(badge_observe))
      
      if(nrow(effet22_observe_sans)>0){
        for(i in 1:nrow(effet22_observe_sans)){
          reduction %+=% effet22_observe_sans[i,]$nombre_gain
        # reduction additionne toutes les réductions des cartes effet22 posées devant le j_actif
        }
        print(paste("reduction sans condition de",reduction))
      }
      
      cartes2 = cartes2 %>%
        mutate(cost = case_when(cost + reduction < 0 ~ 0,
                                TRUE ~ cost + reduction))
  }
      
      # Etape 2 : réductions conditionnées à un badge ----
      # id_carte est le numéro de la carte qui vient d'être posée
      # badges_carte est la liste des badges 
     # badges_cartes = cartes %>% filter(!is.na(badges))
      
      # effet22_observe est la liste des cartes effet22 en jeu
      # qui appliquent une reduction sur une carte à BADGE
      effet22_observe = effet22_reduction %>%
        filter(!is.na(badge_observe))

      # si il existe de telles cartes effet22
      if(nrow(effet22_observe)>0){
          for(i in 1:nrow(effet22_observe)){
            reduction = 0
            reduction %+=% effet22_observe[i,]$nombre_gain
            print(paste("badge cherché :",effet22_observe[i,]$badge_observe,
                        "par la carte ",effet22_observe[i,]$id,
                        "qui confère une réduction de",reduction))
            
            cartes2 = cartes2 %>%
              mutate(flag = grepl(pattern = effet22_observe[i,]$badge_observe, x=badges))%>%
              group_by(id)%>%
           #   filter(grepl(pattern = effet22_observe[i,]$badge_observe, x=d$badges) == TRUE)%>%
              mutate(cost = case_when(flag == TRUE & cost + reduction < 0 ~ 0,
                                      flag == TRUE & cost + reduction >= 0 ~ cost + reduction,
                                      TRUE ~ cost))
          }
        
      }
          
  return(cartes2)
}
# f_appliquer_reduction = function(mdf){
#   # Variables observées
#   # mdf$cartes_en_jeu = mdf$cartes[which(mdf$cartes$joueur==mdf$j_actif &
#   #                                        mdf$cartes$where =="J"),]
#   # 
#   # mdf$effet22_jeu =  effet22[which(effet22$id %in% mdf$cartes_en_jeu$id),]
#   # effet22_reduction = mdf$effet22_jeu[which(mdf$effet22_jeu$type_observe == "reduction"),]
#   # 
#   effet22_reduction = effet22 %>%
#     filter(id %in% (mdf$cartes %>%
#                       filter(joueur == mdf$j_actif)%>%
#                       filter(where =="J"))$id)%>%
#     filter(type_observe == "reduction")
#   
#   print(paste("effet22_reduction",effet22_reduction$id))
#   
#   # variable retournée cartes
#   cartes = mdf$cartes
#   
#   if(nrow(cartes)>0){
#     
#     # pour chaque carte dans sa main
#     for(i in cartes$id){
#       
#       # cout est le cout de posage de la carte id =i
#       cout = cartes[which(cartes$id == i),]$cost
# 
#       # # réduction sur le coût grâce à des cartes effet22
#       reduction = f_reduction_cout_i(effet22_reduction, cartes, id_carte=i)
#       
#       cout %+=% reduction
#       
#       # on peut changer la valeur du cout de la carte directement dans le tableau cartes
#       # car 1- les cartes ne changent jamais de main
#       # 2- les cartes qui offrent une réduction ne sont jamais détruites
#       if(reduction > 0){
#         cartes[which(cartes$id == i),]$cost = cout
#       }
#     }
#   }
#   
#   return(cartes)
# }

f_check_poser_argent = function(mdf, cartes){
  # la fonction renvoie la liste des cartes qu'on peut s'acheter
  # elle màj une colonne "ok" = TRUE ou FALSE
  
  # Variables observées
  navire = mdf$navire %>%
    filter(joueur == mdf$j_actif)
  global = mdf$global

  if(nrow(cartes)>0){
    
    # pour chaque carte dans sa main
    for(i in cartes$id){

      ### Etape 1 : regarder si on peut se la payer
      # cout est le cout de posage de la carte id =i
      cout = cartes[which(cartes$id == i),]$cost
      # # 
      # # # réduction sur le coût grâce à des cartes effet22
      # reduction = f_reduction_cout_i(effet22_reduction, cartes, id_carte=i)
      # 
      # cout %+=% reduction
      # 
      # # on peut changer la valeur du cout de la carte directement dans le tableau cartes
      # # car 1- les cartes ne changent jamais de main
      # # 2- les cartes qui offrent une réduction ne sont jamais détruites
      # cartes[which(cartes$id == i),]$cost = cout
     
      
      reduc_acier = 0
      reduc_titane = 0
      
      if(f_check_badge(cartes=cartes, id_carte=i,badge="Co")==TRUE){
        reduc_acier = navire[which(navire$nom=="Acier"),]$stock # nombre d'acier à dépenser
        cout = cout - 2*reduc_acier # réduction du prix qui restera à payer en argent
      }
      if(f_check_badge(cartes=cartes, id_carte=i,badge="Es")==TRUE){
        reduc_titane = navire[which(navire$nom=="Titane"),]$stock # nombre d'acier à dépenser
        cout = cout - 3*reduc_titane # réduction du prix qui restera à payer en argent
      }
    
     
      if(cout <= navire[which(navire$nom=="Argent"),]$stock){
        cartes[which(cartes$id == i),]$ok = TRUE      # carte achetable
      }
    }
  }
  
  #r = cartes[which(cartes$ok == TRUE),]

  return(cartes)
}

f_reduction_cout_i = function(effet22_reduction, cartes, id_carte){
  # initialisation de la réduction
  reduction = 0
  # if(!is.na(effet22_reduction[1,1])){
  
  if(nrow(effet22_reduction)>0){
    
    print(paste("Aujourd'hui, grande réduction spéciale !"))
    
    
    # Etape 1 : réductions qui s'appliquent quelle que soit la carte ----
    # ici effet22_observe est liste des cartes effet22 en jeu
    # qui appliquent une reduction sur une carte sans tenir compte des badges
    effet22_observe_sans = effet22_reduction %>%
      filter(is.na(badge_observe))
    
    if(nrow(effet22_observe_sans)>0){
      for(i in 1:nrow(effet22_observe_sans )){
        reduction %+=% effet22_observe_sans[i,]$nombre_gain
        print(paste("effet22_observe_sans (i in nrow)",i))
      }
    }
    # Etape 2 : réductions conditionnées à un badge ----
    # id_carte est le numéro de la carte qui vient d'être posée
    # badges_carte est la liste des badges de la carte qui vient d'être posée
    badges_carte = cartes[which(cartes$id==id_carte),]$badges
    
    # effet22_observe est la liste des cartes effet22 en jeu
    # qui appliquent une reduction sur une carte à BADGE
    effet22_observe = effet22_reduction %>%
      filter(!is.na(badge_observe))
    # si il existe de telles cartes effet22
    if(nrow(effet22_observe)>0){
      # et si carte qu'on vient de poser a des badges
      if(nrow(badges_carte)>0){
        # on commence par parcourir la liste des badges que porte la carte qu'on vient de poser
        # et qui est contenue dans badges_carte
        a = gsub('([[:upper:]])', ' \\1', badges_carte)
        b = as.list(strsplit(a, " ")[[1]])[-1]
        for(i in b){
          # pour chaque carte qui offre des réductions sur des badges
          for(j in effet22_observe$id){
            # on regarde si le badge posé correspond aux badges attendus par les cartes effet22
            if(grepl(i,  effet22_observe[which(effet22_observe$id==j),]$badge_observe[[1]], fixed = TRUE)==TRUE){
              # si oui, on en applique les conséquences
              reduction %+=% effet22_observe[which(effet22_observe$id==j),]$nombre_gain
              print(paste("effet22_observe avec badge (j in id)",j))
            }
          }
        }
      }
    }
  }
  if(reduction !=0 ){
  print(paste("reduction de", reduction,"pour la carte",cartes[which(cartes$id==id_carte),]$nom))
  }
  # fin ----
  return(reduction)
}


f_check_poser_prerequis = function(mdf,cartes){
# vérifie les prérequis globaux (temp, ox, océan)
# vérifie les badges nécessaires
# vérifie les tuiles nécessaires
    
  # Variables observées
  badges = mdf$badges_joueurs
  global = mdf$global
  joueur = mdf$j_actif
  plateau = mdf$plateau

  
  # Variable modifiée : cartes
  
  for(i in cartes$id){
    ### Etape 2 : on regarde si les éventuels prérequis sont remplis
    liste_pre = prerequis[which(prerequis$id == i),] # liste des prérequis pour la carte i
    if(nrow(liste_pre)>0){
      for(j in 1:nrow(liste_pre)){
        pre = liste_pre[j,]
      #  print(pre)
        
        if(pre$type %in% c("oxyg","temp","ocean")){
          if(pre$valeur == "min"){
            # si le paramètre global est inférieur au prerequis de la carte, la carte est éliminée
            if(global[pre$type][1,] < pre$nombre){
              cartes[which(cartes$id == i),]$ok = FALSE
              
            }
          }else if(pre$valeur =="max"){
            if(global[pre$type][1,] > pre$nombre){
              cartes[which(cartes$id == i),]$ok = FALSE
            }
          }
        }else if(pre$type == "badge"){
          # si le nombre de badges correspondant accumulés par le joueur actif est inférieur strictement à la valeur prérequise,
          # alors la carte est éliminée des cartes qu'on peut poser
          if(badges[which(badges$joueur == mdf$j_actif &
                          badges$badge == pre$valeur),]$nombre < pre$nombre){
            cartes[which(cartes$id == i),]$ok = FALSE
          }
        }else if(pre$type == "tuile"){
          tab = plateau[which(plateau$Type_tuile == pre$valeur),] # tableau des cases qui contiennent ce type de tuile
          if(nrow(tab) < pre$nombre){
            cartes[which(cartes$id == i),]$ok = FALSE
          }else{
            if(pre$vise == "self"){
              if(nrow(tab[which(tab$Proprio == mdf$j_actif),]) < pre$nombre){
                cartes[which(cartes$id == i),]$ok = FALSE
              }
            }
          }
        }
      }
    }
  }
  return(cartes)
  #return(cartes[which(cartes$ok == TRUE),])
}


f_check_poser_effets = function(mdf, id_carte=0,cartes){   
  # id_carte ne sert pas quand on 
    
  # Variables observées seulement
  navire = mdf$navire %>%
    filter(joueur == mdf$j_actif) %>%
    as.data.frame()


  ### Etape 3 : si la carte implique de perdre un stock/flux,
  # il faut être en mesure de le perdre.
  for(i in cartes$id){      
    effet_carte_i = effet_cartes[which(effet_cartes$id == i),]

    if(nrow(effet_carte_i)!=0){
      for(j in 1:nrow(effet_carte_i)){ #pour toutes les lignes de effet_cartes qui concerne cette carte en particulier
        unite = effet_carte_i[j,]$unite
        type = effet_carte_i[j,]$type
        nombre = effet_carte_i[j,]$nombre
        vise = effet_carte_i[j,]$vise
        
        
        # si l'effet est une modification NEGATIVE
        if(nombre < 0){
          if(type %in% c("stock","flux") & vise == "self"){
            
            a = navire[which(navire$nom==unite),][type]
            
            #et si le joueur n'a pas ce stock ou ce flux, on élimine cette carte
            if(as.numeric(a) < (-nombre)){
              cartes[which(cartes$id == i),]$ok = FALSE
              
            }
          }
          else if(type == "flux" & vise == "adv"){
            # b est la somme des flux de tous les joueurs de l'unité concernée
              b = sum(mdf$navire[which(navire$nom==unite),][type])
              
              # et si il n'y a pas suffisamment de flux, même avec tous les joeuurs
              if(as.numeric(b) < (-nombre)){
                cartes[which(cartes$id == i),]$ok = FALSE
              }
              
          }
        }}}}
  # c = cartes[which(cartes$ok == TRUE),]
  return(cartes)
}

f_check_poser_tuile = function(mdf,cartes){
    # la liste des cartes qui impliquent un placement de tuile
    # mdf$cartes_tuile = mdf$cartes %>%
    #   filter(joueur == mdf$j_actif,
    #          where == "M") %>%
    #   left_join(effet_cartes, by = "id")%>%
    #   filter(type =="tuile")%>%
    #   left_join(restrictions_placement, by = "id")
    
    cartes_tuile = cartes %>%
      left_join(effet_cartes, by = "id")%>%
      filter(type =="tuile")%>%
      left_join(restrictions_placement, by = "id")
    
    cartes_tuile_restr = cartes_tuile %>%
      filter(!is.na(restriction))
    
    cartes_tuile_normal = cartes_tuile %>%
      filter(is.na(restriction))
 
    if(nrow(cartes_tuile)>0){
      for(i in cartes_tuile_restr$id){
        if(i %in% mdf$plat_girafe$id_source){
        }else{
          cartes[which(cartes$id == i),]$ok = FALSE
        }
      }
      for(i in cartes_tuile_normal$id){
        a=mdf$plat_girafe %>%
          filter(id_source ==0)
        if(cartes_tuile_normal[which(cartes_tuile_normal$id==i),]$unite %in% a$tuile){
        }else{
          cartes[which(cartes$id == i),]$ok = FALSE
        }
      }   
  }
  return(cartes)
}

f_plat_girafe = function(mdf){
  print("Début Calcul plat_girafe")

    plat_girafe = mdf$plateau %>%
      filter(Lat == 0) %>%
      mutate(id_source = as.character(0),
             tuile="")
    
    
  # POUR TOUTES LES CARTES EN MAIN
  # QUI IMPLIQUENT UN PLACEMENT DE TUILE particulier
  mdf$cartes_tuile = mdf$cartes %>%
    filter(joueur == mdf$j_actif,
           where == "M") %>%
    left_join(effet_cartes, by = "id")%>%
    select(c(id,unite,type,nombre))%>%
    filter(type =="tuile")%>%
    left_join(restrictions_placement, by = "id")%>%
    filter(!is.na(restriction))

  # pour chaque carte (en main) qui placent une tuile,
  # on regarde les cases dispos pour lesdites tuiles
  # on en profite pour les garder en mémoire dans mdf$plat_en_main
  for(i in mdf$cartes_tuile$id){
    cases_possibles_i = f_plat_tot_girafe(
      id_carte = i,
      plat1 = mdf$plateau,
      player= mdf$j_actif
    ) %>%
      mutate(id_source = as.character(i),
             tuile = mdf$cartes_tuile[which(mdf$cartes_tuile$id==i),]$unite)
    
    plat_girafe = plat_girafe %>%
      full_join(cases_possibles_i, by = c("Lat", "Long", "Ocean", "Mars", "Reserve",
                                          "Libre", "Proprio", "Type_tuile", "Libre_ville",
                                          "Volcanique","Source", "tuile",
                                          "Libre_foret_J1", "Libre_foret_J2", "Num", "id_source"))
  }
  
  # POUR TOUS LES PROJETS STANDARDS QUI IMPLIQUENT UN PLACEMENT DE TUILE
 # for(j in (projets_std %>% filter(type=="tuile"))$id){
  for(j in c("oc","vi","fo","special")){
    cases_possibles_j = f_plat_tot_girafe(
      id_carte = 0,
      plat1 = mdf$plateau,
      type_tuile = j,
      player = mdf$j_actif
    ) %>%
      mutate(id_source = as.character(0),
             tuile = j)
    
    plat_girafe = plat_girafe %>%
      full_join(cases_possibles_j, by = c("Lat", "Long", "Ocean", "Mars", "Reserve",
                                          "Libre", "Proprio", "Type_tuile", "Libre_ville",
                                          "Volcanique","Source","tuile",
                                          "Libre_foret_J1", "Libre_foret_J2", "Num", "id_source"))
  }
  
  print("fin Calcul plat_girafe")
#  View(plat_girafe)
  return(plat_girafe)
}

f_plat_en_main = function(mdf){
  
  # POUR TOUTES LES CARTES EN MAIN QUI IMPLIQUENT UN PLACEMENT DE TUILE
  mdf$cartes_tuile = mdf$cartes %>%
    filter(joueur == mdf$j_actif,
           where == "M") %>%
    left_join(effet_cartes, by = "id")%>%
    filter(type =="tuile")%>%
    left_join(restrictions_placement, by = "id")
  
  
  plat_en_main = mdf$plateau %>%
    filter(Lat == 0) %>%
    mutate(id_carte = as.character(0))
  # pour chaque carte (en main) qui placent une tuile,
  # on regarde les cases dispos pour lesdites tuiles
  # on en profite pour les garder en mémoire dans mdf$plat_en_main
  for(i in mdf$cartes_tuile$id){
    cases_possibles_i = f_plat_tot_girafe(
      id_carte = i,
      plat1 = mdf$plateau,
      player= mdf$j_actif
      ) %>%
      mutate(id_carte = as.character(i))
    
    plat_en_main = plat_en_main %>%
      full_join(cases_possibles_i, by = c("Lat", "Long", "Ocean", "Mars", "Reserve",
                                          "Libre", "Proprio", "Type_tuile", "Libre_ville",
                                          "Libre_foret_J1", "Libre_foret_J2", "Num", "id_carte"))
  }
  
  
  
  # POUR TOUS LES PROJETS STANDARDS QUI IMPLIQUENT UN PLACEMENT DE TUILE
  for(j in (projets_std %>% filter(type=="tuile"))$id){
    cases_possibles_j = f_plat_tot_girafe(
      id_carte = 0,
      plat1 = mdf$plateau,
      type_tuile = j,
      player = mdf$j_actif
      ) %>%
      mutate(id_carte = as.character(j))
      
      plat_en_main = plat_en_main %>%
        full_join(cases_possibles_j, by = c("Lat", "Long", "Ocean", "Mars", "Reserve",
                                            "Libre", "Proprio", "Type_tuile", "Libre_ville",
                                            "Libre_foret_J1", "Libre_foret_J2", "Num", "id_carte"))
  }
  return(plat_en_main)
}


f_check_actions21 = function(a, mdf){
 # a = mdf$actions21_prepa

  if(nrow(a)>0){
    a$ok = "TRUE"

  
  # a = mdf$cartes %>%
  #   filter(joueur == mdf$j_actif,
  #          type_carte==21,
  #          where =="J"
  #   ) %>%
  #   left_join(actions21,by="id")
  
  for(i in unique(a$id)){   
    
    effet_i = a[which(a$id==i),]

    if(nrow(effet_i)!=0){
      for(j in 1:nrow(effet_i)){ #pour toutes les lignes de effet_cartes qui concerne cette carte en particulier
        unite = effet_i[j,]$unite
        type = effet_i[j,]$type
        nombre = effet_i[j,]$nombre
        vise = effet_i[j,]$vise
     #   print(paste(i,j,unite,type,nombre,vise))
        # si l'effet est une modification NEGATIVE
        if(nombre < 0){
          if(type %in% c("stock","flux") & vise == "self"){
              nav = mdf$navire[which(mdf$navire$nom==unite &
                                     mdf$navire$joueur==mdf$j_actif),][type]
            #et si le joueur n'a pas ce stock ou ce flux, on élimine cette carte
              if(as.numeric(nav) < (-nombre)){
                a[which(a$id_idaction == effet_i[j,]$id_idaction),]$ok = FALSE
                }
              }else if(type == "sur_cette_carte"){
                sur = effet_i[1,]$sur_cette_carte_nombre
            #et si le joueur n'a pas ces ressources sur la carte, on élimine cette carte
                if(as.numeric(sur) < (-nombre)){
                  a[which(a$id_idaction == effet_i[j,]$id_idaction),]$ok = FALSE
                }
              }
          }
        }
    }
  }
  }

  return(a)
}


#######
f_debut = function(session,cartes,navire,gratuit=TRUE,nb_cartes){
  #f_debut applique f_cartes_temp pour l'ensemble des joueurs
  for(i in unique(navire$joueur)){
    cartes = f_cartes_temp(cartes=cartes,
                           nb_cartes=nb_cartes,
                           gratuit=gratuit,
                           joueur=i)
  }
  return(cartes)
}

# fonction qu'un joueur en particulier règle l'achat de nb_cartes cartes
f_achat_cartes =function(navire,nb_cartes,player){
  cout_tot = 3*nb_cartes
  
  navire[which(navire$joueur==player & navire$nom=="Argent"),]$stock %+=% cout_tot

  return(navire)
}

# fonction pour actualiser le tableau cartes en fonction des cartes **achetées**
# (les cartes piochées gratuitement sont normalement comptées dans f_cartes_temp)
f_new_cartes_en_main = function(cartes,player,new_cartes,choix_cartes){
  #new_cartes doit être une $$$liste$$$ des id des cartes précédemment achetées
  new_cartes=as.integer(new_cartes)
 
  #cartes2 = les cartes achetées et qui arrivent dans la main
  cartes2 = cartes %>%
    filter(id %in% new_cartes)%>%
    mutate(joueur = player,
           where="M")
  
  # choix_cartes est ***un tableau**** l'ensemble des cartes "T" présentées à l'achat
  # cartes3 = les cartes rejetées (leur id n'est pas dans new_cartes MAIS est dans choix_cartes) et qui arrivent dans la défausse
  cartes3 = cartes %>%
    filter(id %!in% new_cartes & id %in% choix_cartes$id & joueur == player)%>%
    mutate(where="D", joueur="")

  # cartes4 = toutes les autres qui n'étaient pas concernées par le tirage
  cartes4 = cartes %>%
    filter(id %!in% cartes3$id & id %!in% cartes2$id)%>%
    full_join(cartes2, by = c("id", "nom", "cost", "points", "badges",
                              "type_carte", "joueur", "where", "sur_cette_carte_type",
                              "sur_cette_carte_nombre", "check", "ok"))%>%
    full_join(cartes3, by = c("id", "nom", "cost", "points", "badges",
                              "type_carte", "joueur", "where", "sur_cette_carte_type",
                              "sur_cette_carte_nombre", "check", "ok"))
  

   # la fonction revoie l'ensemble du tableau cartes
  return(cartes4)
}

f_maj_navire = function(navire,params_joueur){
  navire2 = navire
  
  # Tout stock d'énergie restant est converti en chaleur
  navire2[which(navire2$nom=="Chaleur"),]$stock %+=%
    navire2[which(navire2$nom=="Energie"),]$stock
  navire2[which(navire2$nom=="Energie"),]$stock = 0
  
  # Chaque ressource ajoute la valeur de son flux à son stock existant
  navire2 = navire2 %>%
    group_by(joueur)%>%
    mutate(stock = stock + flux)
  
  # Le NT s'additionne à l'argent
  for(i in params_joueur$nom){
  navire2[which(navire2$nom=="Argent" & navire2$joueur==i),]$stock %+=%
    params_joueur[which(params_joueur$nom == i),]$NT
  }
  
  return(navire2)
}

f_switch_tab <- function(tab) {
  switch(tab,
         crt = effet_cartes,
         std = projets_std,
         obj = data.frame(
           type = "stock",
           unite = "Argent",
           nombre = (-8),
           id=0
           ),
         action21 = actions21
         )
}
# f_switch_tab(id_radio) ####
# f_switch_tab("std") = projets_std
# f_switch_tab("crt") = effet_cartes
# f_switch_tab("obj") = data.frame() (tous au même prix)
# f_switch_tab("action21") = 


f_switch_id <- function(tab) {
  switch(tab,
         crt = "id",
         std = "id",
         obj = "obj",
         action21 = "id_idaction"
  )
}


# généralisation de f_effets_immediats_carte
f_effets_immediats = function(choix, mdf, session, tab,numero="id"){
  # tab = effet_cartes -> id = id
  # tab = projets_std -> id = id
  # tab = actions21 -> id = id_idaction

  print(paste("début effets :",choix))

  tab_i = tab[which(tab[[numero]] == choix),]
 # print("tab_i")
 # print(tab_i)
  
  if(nrow(tab_i)>0){
    print(paste("Ce choix entraîne",nrow(tab_i),"effets immédiats."))
    for(i in 1:nrow(tab_i)){ #pour toutes les lignes de effet_cartes qui concerne cette carte en particulier
      
      mdf = f_consequence_i_mdf(unite = tab_i[i,]$unite,
                                type = tab_i[i,]$type,
                                nombre = tab_i[i,]$nombre,
                                mdf,
                                id_carte = choix,
                                vise = tab_i[i,]$vise)
    }
  }else{print("Aucun effet immédiat.")}
  print("fin effets")
  return(mdf)
}

# Fonction pour réaliser les conséquences immédiates d'UNE SEULE carte posée n° id_carte
f_effets_immediats_carte = function(id_carte, mdf, session){#navire,id_carte,tab_effets,global,params_joueur,player, cartes){
 
  print(paste("f_effets_immediats_carte ",id_carte))
  
  # ###
  # navire = mdf$navire,
  # id_carte = 56 id_carte,
  tab_effets = effet_cartes[which(effet_cartes$id==id_carte),]
  # global = mdf$global,
  # params_joueur = mdf$params_joueur,
  # player = mdf$j_actif,
  # cartes = mdf$cartes
  # ###
  # 
 # if(id_carte %in% effet_cartes$id){
    
    
    # Etape 1 : appliquer les effets immédiats de la carte----
    # grâce à f_consequence_i
    if(nrow(tab_effets)>0){
      print(paste("La carte va avoir",nrow(tab_effets),"effets immédiats."))
      for(i in 1:nrow(tab_effets)){ #pour toutes les lignes de effet_cartes qui concerne cette carte en particulier

        mdf = f_consequence_i_mdf(unite = tab_effets[i,]$unite,
                                  type = tab_effets[i,]$type,
                                   nombre = tab_effets[i,]$nombre,
                                  mdf, id_carte =id_carte,
                                  vise = tab_effets[i,]$vise)
        print("f_consequence_i_mdf : fin.")
      } 
    
    }
  print("fin effets_immediats_carte")
  return(mdf)
}

f_activation_effets22_carte = function(id_carte, mdf, session){
  
  # Etape 2 : regarder si un des joueurs a des cartes effets posées devant lui ----
  # et qui s'activent au posage d'un badge
  # cette liste de cartes qui observent est cartes_observe

  # badges_carte est la liste des badges de la carte qui vient d'être posée
  badges_carte = mdf$cartes[which(mdf$cartes$id==id_carte),]$badges
  
  cartes_observe = mdf$cartes %>%
     filter(where=="J")%>%
     left_join(effet22, by="id")%>%
     filter(type_observe=="badge")%>%
    filter((qui == "tous") | (qui=="actif"&joueur==mdf$j_actif))

  # si carte qu'on vient de poser a des badges
  if(length(badges_carte)>0){
    if(nrow(cartes_observe)>0){
      print(paste("debut activation effets22 carte",cartes_observe$nom))
      
    # id_carte est le numéro de la carte qui vient d'être posée

    # si la carte qu'on vient de poser n'est pas un évènement
    # (car certains effets ne ciblent que les évènements)
    if(mdf$cartes[which(mdf$cartes$id==id_carte),]$type_carte!=3){
      cartes_observe = cartes_observe %>%
        filter(type_carte_observe != 3)
    }
      

    # on commence par parcourir la liste des badges que porte la carte qu'on vient de poser
    # et qui est contenue dans badges_carte
      a = gsub('([[:upper:]])', ' \\1', badges_carte)
      b = as.list(strsplit(a, " ")[[1]])[-1]
      print(paste("Balise :",a))
      for(i in b){
        # puis, pour chaque carte à effet22 en jeu (qui s'activent au posage d'un badge)
        for(j in cartes_observe$id){
          for(k in 1:length(j)){
            # on regarde si le badge posé correspond aux badges attendus par les cartes effet22
            if(grepl(i,  cartes_observe[which(cartes_observe$id==j),]$badge_observe[[k]], fixed = TRUE)==TRUE){
               # si oui, on en applique les conséquences
              temp = cartes_observe
             print("Conséquences d'effet22 !")
              mdf = f_consequence_i_mdf(unite=temp$unite_gain,
                                         type=temp$type_gain,
                                         nombre=temp$nombre_gain,
                                        mdf, id_carte = j,
                                        vise = temp$vise)
            }
          }
        }
      }
      print("fin activation effets22 carte")
      rm(cartes_observe)
    }
  } # fin étape 2 (badges)



  # liste = list(navire,global,params_joueur,cartes)
  # return(liste)
return(mdf)
}

# Actualise les navires et les cartes en fonction des effets 22 des cartes posées observantes (cartes_observe)
f_cartes_observe_i = function(cartes_observe,
                            navire,
                            cartes,
                            joueur){
    
      for(j in 1:nrow(cartes_observe)){#cartes_observe$id){
        if( # si la carte concerne les tuiles posées par le joueur actif seulement,
          # ou si la carte concerne tous les joueurs
          (cartes_observe[j,]$qui == "actif" &&
           cartes_observe[j,]$joueur == joueur) ||
          cartes_observe[j,]$qui == "tous"){
          
          type =          cartes_observe[j,]$type_gain
          unite =         cartes_observe[j,]$unite_gain
          nombre =        cartes_observe[j,]$nombre_gain
          proprio_carte = cartes_observe[j,]$joueur
          id_carte =      cartes_observe[j,]$id
          print(paste("id_carte de cartes_observe[j,] = ",id_carte,"et bonus",nombre,unite,type,"pour",proprio_carte))
          
          # alors on applique l'effet de la carte
          if(type %in% c("stock","flux")){
            navire[which(navire$joueur==proprio_carte & navire$nom==unite),][type] %+=%
              nombre
          }else if(type == "sur_cette_carte"){
            cartes[which(cartes$id == id_carte),]$sur_cette_carte_type = unite
            cartes[which(cartes$id == id_carte),]$sur_cette_carte_nombre %+=% nombre
          }else{print("Mogadiscio")}
        }
      }
          
liste = list(navire,cartes)
return(liste)
}

# fonction pour noter le placement d'une tuile sur le plateau
f_maj_plateau = function(plat, tuile, joueur, case){
 
  print(paste("tuile pour maj_plateau",tuile))
  # on place une tuile (Libre = FALSE) + si ce n'est pas un océan on note à qui elle appartient (Proprio= j_actif)
  plat[which(plat$Num==case),]$Libre = FALSE
  plat[which(plat$Num==case),]$Type_tuile = tuile
  
  if(tuile=="oc"){
    plat[which(plat$Num==case),]$Proprio = "Ocean"
  }else{
    plat[which(plat$Num==case),]$Proprio = joueur
    
    if(plat[which(plat$Num==case),]$Mars==1){
      # on en profite pour recalculer la disponibilité actuelle des cases aux futures Villes et Forêt
      plat = f_cases_conditionnees(plat,
                                   tuile,
                                   joueur,
                                   case)
    }
  }
  
  print("fin màj plateau")
  return(plat)
}

f_bonus_placement = function(mdf,
                             num,
                             rivage = 2 #nombre de pièces touchées quand placement océans
){
  # num="8/14"
  
  # variables observées
  
  
  print(paste("Num case pour bonus de placement :",num))
  
  # Etape 1 : gérer le bonus de placement proprement dit----
  bonus = bonus_placement %>% filter(Num == num)
 # print(paste("Bonus estimé : ",bonus))
  
  #rownames(navire) <- c()
  if(nrow(bonus)>0){
    for(i in 1:nrow(bonus)){ #pour toutes les lignes de effet_cartes qui concerne cette carte en particulier
      unite = bonus[i,]$unite_bonus
      type = bonus[i,]$type_bonus
      nombre = bonus[i,]$nombre_bonus
      print(paste("Bonus placement : ",nombre,type,unite))
      # si l'effet est une modification des stocks/flux du navire actif
      if(type %in% c("stock","flux")){
        mdf$navire[which(mdf$navire$nom==unite & mdf$navire$joueur==mdf$j_actif),][type] %+=%
          nombre
      }else if(type == "carte"){
        temp = f_cartes_temp(cartes=mdf$cartes,
                             nb_cartes=nombre,
                             gratuit=TRUE,
                             joueur=mdf$j_actif)
        mdf$cartes = temp
      }else{print("On the road again")}
    }
  }else{print("Pas de bonus de placement.")}
  # Etape 2 : gérer l'argent gagné en posant une tuile près d'océans ----
  plat=mdf$plateau
  case_tot = plat[which(plat$Num==num),]
  
  cases_adj = f_cases_adjacentes(plateau = mdf$plateau, case = case_tot)
  
  nb_ocean_adj = cases_adj %>%
    filter(Type_tuile == "oc",
           Libre = FALSE)%>%
    nrow()
  bonus_ocean = rivage*nb_ocean_adj
  print(paste("Nombre d'océans adjacents : ",nb_ocean_adj, "bonus",bonus_ocean))
  if(bonus_ocean > 0){
    mdf$navire[which(mdf$navire$nom=="Argent" & mdf$navire$joueur==mdf$j_actif),]$stock %+=% bonus_ocean
  }
  
  # Fin----
  print("fin bonus de placement")
  return(mdf)
  
}  
# f_bonus_placement = function(navire,
#                                    num,
#                                    player,
#                                    cartes,
#                                    plateau,
#                                    rivage = 2 #nombre de pièces touchées quand placement océans
#                                    ){
#   # num="8/14"
#   # case = num
#   
#   print(paste("Num case pour bonus de placement :",num))
#   
#   # Etape 1 : gérer le bonus de placement proprement dit----
#   bonus = bonus_placement %>% filter(Num == num)
#   
#   
#   rownames(navire) <- c()
#   if(nrow(bonus)>0){
#     for(i in 1:nrow(bonus)){ #pour toutes les lignes de effet_cartes qui concerne cette carte en particulier
#         unite = bonus[i,]$unite_bonus
#         type = bonus[i,]$type_bonus
#         nombre = bonus[i,]$nombre_bonus
#         print(paste("Bonus placement : ",nombre,type,unite))
#         # si l'effet est une modification des stocks/flux du navire actif
#         if(type %in% c("stock","flux")){
#           navire[which(navire$nom==unite & navire$joueur==player),][type] %+=%
#             nombre
#         }else if(type == "carte"){
#           temp = f_cartes_temp(cartes=cartes,
#                                  nb_cartes=nombre,
#                                  gratuit=TRUE,
#                                  joueur=player)
#           cartes = temp
#         }
#     }
#   }
#   # Etape 2 : gérer l'argent gagné en posant une tuile près d'océans ----
#   plat=plateau
#   case_tot = plateau[which(plateau$Num==case),]
#   
#   cases_adj = f_cases_adjacentes(plateau = plateau, case = case)
#   
#   # cases_adj1 = plat[which(plat$Lat == case_tot$Lat &
#   #                (plat$Long == case_tot$Long +2 | plat$Long == case_tot$Long -2) ),]
#   # cases_adj2 = plat[which((plat$Lat == case_tot$Lat - 1 | plat$Lat == case_tot$Lat +1) &
#   #                (plat$Long == case_tot$Long +1 | plat$Long == case_tot$Long -1) ),]
# 
#   # nb_ocean_adj = cases_adj1 %>%
#   #   full_join(cases_adj2)%>%
#   #   filter(Proprio ==  "Ocean")%>%
#   #   nrow()
#   
#   nb_ocean_adj = cases_adj %>%
#     filter(Type_tuile == "oc",
#            Libre = FALSE)%>%
#     nrow()
#   print(paste("Nombre d'océans adjacents : ",nb_ocean_adj))
#   navire[which(navire$nom=="Argent" & navire$joueur==player),]$stock %+=% rivage*nb_ocean_adj
#     
#     
#   # Fin----
#   print("fin bonus de placement")
#   liste = list(navire,cartes)
#   return(liste)
#   
# }  
# Fonction qui fait les conséquences ----
f_consequence_i_mdf = function(unite,type,nombre, vise, mdf, id_carte){
  
  print(paste("début f_consequence_i_mdf",nombre,type,unite,"pour",mdf$j_actif))
  
  
  # si l'effet est une modification des stocks/flux du navire actif
  if(type %in% c("stock","flux")){
    if(vise == "self"){
      mdf$navire[which(mdf$navire$nom==unite & mdf$navire$joueur==mdf$j_actif),][type] %+=% nombre
      
      # si l'effet vise "n'importe quel joueur"
    }else if(vise == "adv"){
      # si on joue à plus de 1
      if(mdf$nb > 1){
        # a = l'état du flux/stock chez l'adversaire
        a = mdf$navire[which(mdf$navire$nom==unite & mdf$navire$joueur!=mdf$j_actif),][type]
        if(type == "stock"){
          if(a < (-nombre)){ # l'adversaire perd entre tout son stock et "nombre", selon son stock
            mdf$navire[which(mdf$navire$nom==unite & mdf$navire$joueur!=mdf$j_actif),][type] = 0
          }else{
            mdf$navire[which(mdf$navire$nom==unite & mdf$navire$joueur!=mdf$j_actif),][type] %+=% nombre
            }
        }else if(type == "flux"){
          if(a < (-nombre)){ # si l'adversaire n'a pas assez de flux pour perdre l'intégralité de "nombre"
            mdf$navire[which(mdf$navire$nom==unite & mdf$navire$joueur!=mdf$j_actif),][type] = 0
            mdf$navire[which(mdf$navire$nom==unite & mdf$navire$joueur==mdf$j_actif),][type] %+=% (nombre + a)
          }else{ # si l'adversaire a assez de flux, il perd "nombre"
            mdf$navire[which(mdf$navire$nom==unite & mdf$navire$joueur!=mdf$j_actif),][type] %+=% nombre
          }
      }
      }else if(mdf$nb==1 & nombre < 0){
        mdf$navire[which(mdf$navire$nom==unite & mdf$navire$joueur==mdf$j_actif),][type] %+=% nombre
      }
    }
    
    # si l'effet est directement un paramètre global (temp OU oxyg, pas océan car il faut placer la tuile)
  }else if(type == "global"){
    if(unite == "temp"){
      mdf$global[unite] %+=% (2*nombre)
      mdf$params_joueur[which(mdf$params_joueur$nom == mdf$j_actif),]$NT %+=% nombre
    }else if(unite == "oxyg"){
      mdf$global[unite] %+=% nombre
      mdf$params_joueur[which(mdf$params_joueur$nom == mdf$j_actif),]$NT %+=% nombre
    }
    #}else{print("à débugger plus tard.")}
    
    # si l'effet est un gain de NT
  }else if(type =="NT"){
    mdf$params_joueur[which(mdf$params_joueur$nom == mdf$j_actif),]$NT %+=% 1
    
  }else if(type =="sur_cette_carte"){  

    if(typeof(id_carte)=="character"){
      nouveau_id = (actions21 %>%
        filter(id_idaction == id_carte))$id[1]
    }else{nouveau_id=id_carte}

    mdf$cartes[which(mdf$cartes$id == nouveau_id),]$sur_cette_carte_type = unite
    mdf$cartes[which(mdf$cartes$id == nouveau_id),]$sur_cette_carte_nombre = nombre
    print("nouvelle ressource sur une carte")
    
  }else if(type == "sur_carte"){
    
    a = mdf$cartes %>%
      filter(where == "J", joueur == mdf$j_actif)%>%
      left_join(actions21, by = "id")%>%
      left_join(effet22, by = "id")
    
    b = a[which((a$type == "sur_cette_carte" & a$unite == unite) |
                 a$type_gain == "sur_cette_carte" & a$unite_gain == unite),
          ]
    
    if(nrow(b)==1){
      mdf$cartes[which(mdf$cartes$id == b$id[1]),]$sur_cette_carte_type = unite
      mdf$cartes[which(mdf$cartes$id == b$id[1]),]$sur_cette_carte_nombre = nombre
      showNotification(paste("Vous avez gagné une ressource (",nombre,unite,") sur la carte 'b$nom[1]'"))
    }else if(nrow(b)==0){
      showNotification(paste("Vous avez gagné une ressource (",nombre,unite,"), mais vous n'avez pas de carte en jeu sur laquelle la stocker."))
    }else if(nrow(b)>1){
      c = b %>% select(c(id, nom)) %>% distinct()
      mdf$sur_carte = c
      showNotification(paste("Vous avez gagné une ressource (",nombre,unite,"), vous choisissez la carte."))
    }
    
  }else if(type == "carte"){
    mdf$cartes = f_cartes_temp(cartes=mdf$cartes,
                           nb_cartes=nombre,
                           gratuit=TRUE,
                           joueur=mdf$j_actif)
    
   }else if(type == "tuile"){
      # mdf$tuile=effet_p[which(effet_p$type == "tuile"),]$unite
      # mdf$tuile_nb = effet_p[which(effet_p$type == "tuile"),]$nombre
      # mdf$tuile_source = carte_p$id
     mdf$tuile= unite
     mdf$tuile_nb = nombre
     
     if(id_carte %in% restrictions_placement$id){
       mdf$tuile_source = id_carte
     }else{
       mdf$tuile_source = 0
       
       }
      print(paste("TUILE A L'HORIZON : ",mdf$tuile,mdf$tuile_nb,mdf$tuile_source))
  }else{
    print("Tout un tas d'autres conséquences !")}
  
  print("fin consequence i")
 
  return(mdf)
  
}  


# Fonction qui fait les conséquences ----
f_consequence_i = function(unite,type,nombre,
                           navire, player,
                           global,
                           params_joueur,
                           cartes,id_carte){
  # si l'effet est une modification des stocks/flux du navire actif
  if(type %in% c("stock","flux")){
    navire[which(navire$nom==unite & navire$joueur==player),][type] %+=%
      nombre
    
    # si l'effet est directement un paramètre global (temp OU oxyg, pas océan car il faut placer la tuile)
  }else if(type == "global"){
    if(unite == "temp"){
    global[unite] %+=% 2*nombre
    params_joueur[which(params_joueur$nom == player),]$NT %+=% 1
    }else{print("à débugger plus tard.")}
    
    # si l'effet est un gain de NT
  }else if(type =="NT"){
    params_joueur[which(params_joueur$nom == player),]$NT %+=% 1
    
  }else if(type =="sur_cette_carte"){  
    cartes[which(cartes$id == id_carte),]$sur_cette_carte_type = unite
    cartes[which(cartes$id == id_carte),]$sur_cette_carte_nombre = nombre
  }else if(type == "carte"){
    cartes = f_cartes_temp(cartes=cartes,
                         nb_cartes=nombre,
                         gratuit=TRUE,
                         joueur=player)
     

  }else{
    print("Tout un tas d'autres conséquences !")}
  
  print("fin consequence i")
  liste = list(navire,global,params_joueur,cartes)
  return(liste)
  
}  


# Fonction pour générer les navires (kableExtra) ----
f_kable_navire = function(navire,params_joueur) {
  if(nrow(navire)>0){
    rownames(navire) <- c()
    # names(navire)[1] <- cell_spec(names(navire)[1],
    #                               background = params_joueur[which(params_joueur$nom==unique(navire$joueur)),]$couleur)
    # 
    kable=navire %>%
    #  select(-joueur)%>%
      knitr::kable("html") %>%
      kable_styling(#"striped", 
        full_width = F)%>%
      column_spec(3, color = spec_color(navire$stock[1:6]))%>% #stocks
      column_spec(2, color = spec_color(navire$flux[1:6]))%>% #flux

      column_spec(4, color = "black", #joueur
                  background = params_joueur[which(params_joueur$nom==unique(navire$joueur)),]$couleur)

  }else{kable=data.frame()%>%kable("html")}
  return(kable)
}

f_kable_cartes = function(cartes,params_joueur) {
  if(nrow(cartes)>0){
    
  #  rownames(cartes) <- c()
    couleur_rouge <- which(cartes$type_carte ==3)
    couleur_bleue <- which(cartes$type_carte > 20)
    couleur_verte <- which(cartes$type_carte == 1)
    
    # mdf %>% 
    #   kable(booktabs = T) %>%
    #   kable_styling() %>%
    #   row_spec(color.me, bold = T, color = "white", background = "red")
    kable=cartes %>%
      select(-c(type_carte,where,joueur))%>%
      knitr::kable("html") %>%
      kable_styling(#"striped", 
        full_width = F)%>%
      row_spec(couleur_rouge, color = "red")%>%
      row_spec(couleur_bleue, color = "blue")%>%
      row_spec(couleur_verte, color = "green")
    #       popover = paste("am:", mtcars$am[1:8]))
  }else{kable=data.frame()%>%kable("html")}
  return(kable)
}

f_colonne_en_plus = function(cartes){
  # total
  cartes_tot = cartes %>%
    group_by(id)%>%
    left_join(sum_badges_icone, by=c("id")) %>%
    left_join(sum_prerequis, by="id")%>%
    left_join(sum_prerequis_icone , by=c("id")) %>%
    left_join(sum_effet_icone , by="id") %>%
    left_join(sum_actions_icone , by=c("id")) %>%
    left_join(sum_effet22_icone, by = c("id"))%>%
    f_icone_points() %>%
    mutate(icone = as.list(icone),
           icone_a = as.list(icone_a),
           icone_b = as.list(icone_b),
           icone_pr = as.list(icone_pr),
           icone_e = as.list(icone_e),
           icone_p = as.list(icone_p))%>%
    left_join(sum_effet, by="id")%>%
    left_join(sum_effet22, by="id")%>%
    left_join(sum_actions , by="id")%>%
    
    
    group_by(id)%>%
    mutate(immediat2 = case_when(!is.na(immediat)~ paste0("Immédiat : ",immediat),
                                 TRUE ~ ""),
           action2 = case_when(!is.na(action)~ paste0("Action : ",action),
                                 TRUE ~ ""),
           effet2 = case_when(!is.na(effet)~ paste0("Effet : ",effet),
                                 TRUE ~ "")
    ) %>%
    mutate(consequences = 
             paste0(immediat2, " ", action2," ",effet2))%>%
    select(-c(immediat, effet, action))%>%
    select(-c(immediat2, effet2, action2))
    
  
  return(cartes_tot)
}

### Fonctions tuile ----

# rendre le girafe Plot interactif
f_plot_girafe = function(plat){
  plot=plot+
      geom_point_interactive(data=plat,
                             aes(x=Long, y=Lat,data_id = Num,fill=Ocean),
                             shape=21, size = 9)#,alpha = 5/10)+
  return(plot)
}


# générer la liste des cases dispos pour une tuile
# qui provient d'une carte id_carte
# ou pas du tout d'une carte (id_carte=0)
f_plat_tot_girafe = function(
    id_carte=0,
    plat1=plateau,
    type_tuile="",
    player="J1"){

  a = effet_cartes %>%
     filter(id == id_carte)
  
  if(nrow(a)>0){
    if("tuile" %in% a$type){
    type_tuile = (a %>% filter(type=="tuile"))$unite
    }
  }
 
  restriction_i = restrictions_placement %>%
    filter(id == id_carte)
  
  # SI UNE RESTRICTION DE PLACEMENT EXISTE
  if(nrow(restriction_i)>0){
  
    if(restriction_i$restriction == "hors_plateau"){
      case_reservee = restriction_i$reservee
      plat = plat1 %>%
        filter(Reserve == case_reservee)
    }else{

    # si c'est sur une case non réservée pour un océan
    # (seules les cartes qui posent des océans ont ce type de condition)
    if(restriction_i$restriction == "sur_non_ocean"){
      plat = plat1 %>%
        filter(Libre==TRUE)%>%
        filter(Ocean==FALSE)
    }else if(restriction_i$restriction == "sur_ocean"){
      plat = plat1 %>%
        filter(Libre==TRUE)%>%
        filter(Ocean==TRUE)
    }else if(restriction_i$restriction == "volcanique"){
      plat = plat1 %>%
        filter(Volcanique == 1,
               Libre == TRUE)
    
    }else if(restriction_i$restriction == "adjacent"){
      
      # si la nouvelle tuile doit être adjacente à rien
      if(restriction_i$adjacent_nb == 0){
        plat = plat1 %>%
          mutate(test = 0)
        for(j in plat$Num){
          # j="1/5"
          adj_j = f_cases_adjacentes(plat, j)
          # pour chaque case j du plateau, 
          # on regarde si toutes ses voisines sont Libre==TRUE
          if(length(unique(adj_j$Libre))==1){
            if(adj_j[1,]$Libre==TRUE){
        
            plat[which(plat$Num == j),]$test = 1
          }}
        }
        
        plat = plat %>%
          filter(Libre==TRUE,
                 Ocean==FALSE,
                 test == 1)%>%
          select(-test)
        
        # si la nouvelle tuile doit être adjacente à une tuile en particulier
      }else{
        # plat1 = plateau
        # plat1[which(plat1$Num=="4/4"),]$Libre=FALSE
        # plat1[which(plat1$Num=="4/4"),]$Type_tuile="fo"
        # restriction_i = restrictions_placement[which(restrictions_placement$id == 57),]
        # 
        type_observe = restriction_i$adjacent_type
        
        # on sort la liste de toutes les tuiles répondant à ce type_observe (déjà placées)
        tuiles_type_observe = plat1[which(plat1$Type_tuile==type_observe),]
        
        if(restriction_i$adjacent_nb == 1){
          plat = plat1 %>% filter(Lat==0)
          # on fait le tour de toutes les voisines  de cette liste de tuiles
          # et on les prend toutes
          for(j in tuiles_type_observe$Num){
            plat  = plat %>%
              full_join(f_cases_adjacentes(plat1, j))
          }
        }else if(restriction_i$adjacent_nb == 2){
          # par exemple, quand une ville doit être adjacente à 2 villes existantes
          plat = plat1 %>%
            filter(Lat==0) %>%
            mutate(test = 0)
          # on fait le tour de toutes les voisines  de cette liste de tuiles
          for(j in tuiles_type_observe$Num){
            # les cases éventuellement dispos sont celles autour de chaque j
            plat = plat %>%
              full_join(f_cases_adjacentes(plat1, j))%>%
              filter(Libre==TRUE)
            # on regarde parmi ces cases éventuellement dispos si elles ont des voisines du même type_observe
            for(k in plat$Num){
              if(type_observe %in% f_cases_adjacentes(plat1, k)$Type_tuile){
                plat[which(plat$Num == j),]$test = 1
              }
            }
          }
          plat = plat %>% filter(test == 1) %>% select(-test)
        }
        # puis on les filtre
        plat = plat %>%
          filter(Libre==TRUE,
                 Ocean==FALSE)
      }
    }
    }
    # S'IL N'Y A PAS DE RESTRICTION
  }else{
    plat1 = plat1 %>%
      filter(is.na(Reserve))
    
    if(type_tuile==""){
      plat = plat1 %>% filter(Lat==0)
      
    }else if(type_tuile=="special"){
      plat = plat1 %>%
        filter(Libre==TRUE)%>%
        filter(Ocean==FALSE)
      
    }else if(type_tuile=="oc"){
      plat = plat1 %>%
        filter(Libre==TRUE)%>%
        filter(Ocean==TRUE)

    }else if(type_tuile=="vi"){
      plat = plat1 %>%
        filter(Libre==TRUE)%>%
        filter(Ocean==FALSE)%>%
        filter(Libre_ville==TRUE)
     
    }else if(type_tuile=="fo"){
      if(player=="J1"){
        plat = plat1 %>%
          filter(Libre==TRUE)%>%
          filter(Ocean==FALSE)%>%
          filter(Libre_foret_J1==TRUE)
        
      }else if(player=="J2"){
        plat = plat1 %>%
          filter(Libre==TRUE)%>%
          filter(Ocean==FALSE)%>%
          filter(Libre_foret_J2==TRUE)
       
      }
    }else{
      plat = plat1 %>%
        filter(Libre==TRUE)%>%
        filter(Ocean==FALSE)
     
    }
  }
 # }else{}
  return(plat)
}


# f_plat_en_main = function(type_tuile, plat1, player){
#   
#   plat1 = plat1 %>%
#     filter(Mars == 1)
#   
# if(type_tuile=="oc"){
#   plat = plat1 %>%
#     filter(Libre==TRUE)%>%
#     filter(Ocean==TRUE)
#   
#   }else if(type_tuile=="vi"){
#   plat = plat1 %>%
#     filter(Libre==TRUE)%>%
#     filter(Ocean==FALSE)%>%
#     filter(Libre_ville==TRUE)
#   
#   }else if(type_tuile=="fo"){
#   if(player=="J1"){
#    plat = plat1 %>%
#      filter(Libre==TRUE)%>%
#      filter(Ocean==FALSE)%>%
#      filter(Libre_foret_J1==TRUE)
#    }else if(player=="J2"){
#     plat = plat1 %>%
#       filter(Libre==TRUE)%>%
#       filter(Ocean==FALSE)%>%
#       filter(Libre_foret_J2==TRUE)
#     }
#   }else{
#     plat = plat1 %>%
#       filter(Libre==TRUE)
#   }
#   
# return(plat)
# }
# 
# # générer un plateau avec restrictions de placement
# f_plat_restreint_girafe = function(id_carte,  plat1){
#   # id_carte
#   restriction_i = restrictions_placement %>%
#       filter(id == id_carte)
#   # plat1 = plateau
# 
#   # if(restriction_i$restriction == "hors_plateau")
#   # 
#   # plat1 = plat1 %>%
#   #   filter(Mars == 1)
#   
#   # si c'est sur une case non réservée pour un océan
#   # (seules les cartes qui posent des océans ont ce type de condition)
#   if(restriction_i$restriction == "sur_non_ocean"){
#     plat = plat1 %>%
#       filter(Libre==TRUE)%>%
#       filter(Ocean==FALSE)
#   
#   }else if(restriction_i$restriction == "sur_ocean"){
#     plat = plat1 %>%
#       filter(Libre==TRUE)%>%
#       filter(Ocean==TRUE)
#     
#   }else if(restriction_i$restriction == "adjacent"){
#     
#     # si la nouvelle tuile doit être adjacente à rien
#     if(restriction_i$adjacent_nb == 0){
#       plat = plat1 %>%
#         mutate(test = 0)
#       for(j in plat1$Num){
#         adj_j = f_cases_adjacentes(plat1, j)
#         # pour chaque case j du plateau, 
#         # on regarde si toutes ses voisines sont Libre==TRUE
#         if(length(unique(adj_j$Libre))==1 & adj_j[1,]$Libre==TRUE){
#           plat[which(plat$Num == j),]$test = 1
#         }
#       }
#       plat = plat1 %>%
#         filter(Libre==TRUE,
#                Ocean==FALSE,
#                test == 1)
#     
#       # si la nouvelle tuile doit être adjacente à une tuile en particulier
#       }else if(restriction_i$adjacent_nb == 1){
#         
#         plat1 = plateau
#         plat1[which(plat1$Num=="4/4"),]$Libre=FALSE
#         plat1[which(plat1$Num=="4/4"),]$Type_tuile="fo"
#         restriction_i = restrictions_placement[which(restrictions_placement$id == 57),]
# 
#         type_observe = restriction_i$adjacent_type
#         
#         # on sort la liste de toutes les tuiles répondant à ce type_observe (déjà placées)
#         tuiles_type_observe = plat1[which(plat1$Type_tuile==type_observe),]
#         
#         if(restriction_i$adjacent_nb == 1){
#           plat = plat1 %>% filter(Lat==0)
#           # on fait le tour de toutes les voisines  de cette liste de tuiles
#           # et on les prend toutes
#           for(j in tuiles_type_observe$Num){
#             plat  = plat %>%
#               full_join(f_cases_adjacentes(plat1, j))
#           }
#         
#         }else if(restriction_i$adjacent_nb == 2){
#           # par exemple, quand une ville doit être adjacente à 2 villes existantes
#           plat = plat1 %>%
#             filter(Lat==0) %>%
#             mutate(test = 0)
#           # on fait le tour de toutes les voisines  de cette liste de tuiles
#           for(j in tuiles_type_observe$Num){
#             # les cases éventuellement dispos sont celles autour de chaque j
#             plat = plat %>%
#               full_join(f_cases_adjacentes(plat1, j))%>%
#               filter(Libre==TRUE)
#             # on regarde parmi ces cases éventuellement dispos si elles ont des voisines du même type_observe
#             for(k in plat$Num){
#               if(type_observe %in% f_cases_adjacentes(plat1, k)$Type_tuile){
#                 plat[which(plat$Num == j),]$test = 1
#               }
#             }
#           }
#         }
#         # puis on les filtre
#               plat = plat %>%
#           filter(Libre==TRUE,
#                  Ocean==FALSE)
#         }
#       }
#   
#   return(plat)
# }

f_pose_tuile = function(mdf, selection){
  print(paste("tuile pour f_pose_tuile :",mdf$tuile))

  if(length(selection)>0){
    for(i in 1:length(selection)){
      case = f_choix_case_tuile(selection_manuelle=selection[i],
                            type_tuile=mdf$tuile,
                            plat=mdf$plateau,
                            joueur=joueur)
      print(paste("Case : ",case))
       # Actualisation du plateau selon la tuile posée (+ calculs des cases conditionnées pour les Ville/Forêt à l'avenir)
      temp = mdf$plateau
      temp = f_maj_plateau(plat= mdf$plateau,
                           tuile=mdf$tuile,
                           joueur=mdf$j_actif,
                           case=case)
      mdf$plateau = temp
      rm(temp)
      
      mdf$plateau[which(mdf$plateau$Num == case),]$Source = mdf$tuile_source
      
      
      
      # si c'est un océan ou une forêt, global augmente
      if(mdf$tuile=="oc"){
        mdf$params_joueur[which(mdf$params_joueur$nom==mdf$j_actif),]$NT %+=% 1
        mdf$global$ocean %+=% 1}
      if(mdf$tuile=="fo"){
        mdf$params_joueur[which(mdf$params_joueur$nom==mdf$j_actif),]$NT %+=% 1
        mdf$global$oxyg %+=% 1}
      
      # on gagne le bonus de placement (dont 2 argent / océan adjacent)
      mdf = f_bonus_placement(num = case,  mdf = mdf)
      
      
      
      # on regarde si un joueur a en jeu une carte effet
      # qui s'active au placement d'une tuile du même type qui vient d'être posée
      # et si oui, on en applique les conséquences
      # via la fonction f_effet22_tuile()
      liste_temp = f_effet22_tuile(cartes = mdf$cartes,
                                   tuile_posee = mdf$tuile,
                                   joueur = mdf$j_actif,
                                   navire = mdf$navire)
      
      mdf$navire = liste_temp[[1]]
      mdf$cartes = liste_temp[[2]]
      
      rm(liste_temp)
      
      print(paste("fin conséquence placement",case))
    }
  }

  return(mdf)
}


# choix d'une case libre = par défaut ou par input$plateauPlot
f_choix_case_tuile = function(selection_manuelle,type_tuile,plateau,joueur){
  # si on a rien sélectionné de base
  if(is.null(selection_manuelle)){
    print("Sélection manuelle est nulle. Une valeur par défaut va être trouvée.")
     case = f_case_libre_defaut(type_tuile=type_tuile,
                                plateau=plateau,
                                joueur=joueur)
  }else{ #if(selection_manuelle!=""){
    # ou si la case sélectionnée est prise (ce qui arrive quand on a rien sélectionné alors qu'on a déjà placé une tuile cette partie, car je ne sais pas comment faire retourner input$plateauPlotselected à une valeur par défaut "")
    if(plateau[which(plateau$Num==selection_manuelle),]$Libre == FALSE){
      case = f_case_libre_defaut(type_tuile=type_tuile,
                                 plateau=plateau,
                                 joueur=joueur)
    }else{
      case = selection_manuelle
    }
  }
  return(case)
}

f_cases_adjacentes = function(plateau,case){
  plat=plateau
  case_tot = plateau[which(plateau$Num==case),]
  
  cases = plat[
    which(
       (plat$Lat == case_tot$Lat & (plat$Long == case_tot$Long +2 | plat$Long == case_tot$Long -2)) |
      ((plat$Lat == case_tot$Lat - 1 | plat$Lat == case_tot$Lat +1) & (plat$Long == case_tot$Long +1 | plat$Long == case_tot$Long -1) )
      ),]

  return(cases)
}

# fonction qui renvoie la première case dispo pour une nouvelle tuile
f_case_libre_defaut = function(plateau,type_tuile,joueur,id_carte=0){
  print(paste("type tuile pour défaut: ",type_tuile))
  print(paste("nrow plateau f_case_libre ",nrow(plateau)))
  cases_possibles = f_plat_tot_girafe(
    id_carte = id_carte,
    plat1 = plateau,
    player= joueur,
    type_tuile=type_tuile
  )
  print(paste("nrow cases possibles :",nrow(cases_possibles)))
  num_case = cases_possibles[1,]$Num
  return(num_case)
}
# Fonction pour calculer les cases qui deviennent indisponibles pour un type de tuile
# en fonction d'une tuile qu'on vient de poser
# suite, donc, à un stand_by = TRUE + input$do_move, suivi d'une sélection manuelle ou par défaut, peu importe
f_cases_conditionnees = function(plateau,type_tuile,joueur,case){
  plat=plateau
  case_tot = plateau[which(plateau$Num==case),]
  
  if(type_tuile =="vi"){
    plat[which(plat$Lat == case_tot$Lat &
                    (plat$Long == case_tot$Long +2 | plat$Long == case_tot$Long -2) ),]$Libre_ville = FALSE
    plat[which((plat$Lat == case_tot$Lat - 1 | plat$Lat == case_tot$Lat +1) &
                 (plat$Long == case_tot$Long +1 | plat$Long == case_tot$Long -1) ),]$Libre_ville = FALSE
  }
  
  # quoi que ce soit (ville, forêt ou autre SAUF océan)
  # la suite de la fonction NE dépend PLUS de case (la case qu'on vient de remplir ni du type de tuile)
  # Elle calcule automatiquement toutes les cases dispos pour une forêt.
  if(joueur=="J1"){
    plat$Libre_foret_J1 = FALSE
  for(i in 1:nrow(plateau[which(plateau$Proprio==joueur),])){
    row <- plateau[which(plateau$Proprio==joueur),][i,]
    plat[which(plat$Lat == row$Lat &
               (plat$Long == row$Long +2 | plat$Long == row$Long -2) ),]$Libre_foret_J1 = TRUE
    plat[which((plat$Lat == row$Lat - 1 | plat$Lat == row$Lat +1) &
               (plat$Long == row$Long +1 | plat$Long == row$Long -1) ),]$Libre_foret_J1 = TRUE
  }}
  if(joueur=="J2"){
  plat$Libre_foret_J2 = FALSE
  for(i in 1:nrow(plateau[which(plateau$Proprio==joueur),])){
    row <- plateau[which(plateau$Proprio==joueur),][i,]
    plat[which(plat$Lat == row$Lat &
                 (plat$Long == row$Long +2 | plat$Long == row$Long -2) ),]$Libre_foret_J2 = TRUE
    plat[which((plat$Lat == row$Lat - 1 | plat$Lat == row$Lat +1) &
                 (plat$Long == row$Long +1 | plat$Long == row$Long -1) ),]$Libre_foret_J2 = TRUE
  }}
  return(plat)  
}

f_effet22_tuile = function(cartes, tuile_posee, joueur, navire){
  # cartes_observe_tuiles contient l'ensemble des cartes effet22
  # posées devant n'importe quel joueur
  # et qui s'activent au posage d'une tuile
  cartes_observe_tuiles = cartes %>%
    filter(where=="J")%>%
    left_join(effet22, by="id")%>%
    filter(type_observe=="tuile",
           param == tuile_posee)#%>% View()
  
  
  if(nrow(cartes_observe_tuiles)>0){
    liste_temp = f_cartes_observe_i(cartes_observe = cartes_observe_tuiles,
                                  navire=navire,
                                  cartes=cartes,
                                  joueur=joueur)
    
  
    navire = liste_temp[[1]]
    cartes = liste_temp[[2]]
  }
  
  liste = list(navire,cartes)
  return(liste)
}

# Objectifs ----
# fonction qui renvoie le tableau des objectifs persos actualisés (en termes de possible = TRUE/FALSE)
f_verif_objectifs = function(objectifs,
                             params_joueur,
                             plateau,
                             cartes,
                             badges_joueurs){
#  print(paste("nrow objectifs",nrow(objectifs)))
  for(i in 1:nrow(objectifs)){
    player = objectifs[i,]$joueur

    if(objectifs[i,]$obj=="Terraformeur"){
      # si l'objectif est rempli
      if(params_joueur[which(params_joueur$nom==player),]$NT >= 35){
        objectifs[i,]$possible = TRUE
        print("OUIIII")
      }
    }else if(objectifs[i,]$obj=="Maire"){
      p = plateau %>%
        filter(Proprio==player)%>%
        filter(Type_tuile=="vi")
      # si l'objectif est rempli
      if(nrow(p) >= 3){
        objectifs[i,]$possible = TRUE
        print("Vlaboum")
      }
      
    }else if(objectifs[i,]$obj=="Jardinier"){
      p = plateau %>%
        filter(Proprio==player)%>%
        filter(Type_tuile=="fo")
      # si l'objectif est rempli
      if(nrow(p) >= 3){
        objectifs[i,]$possible = TRUE
        print("Tchou tchou!")
      }
      
    }else if(objectifs[i,]$obj=="Bâtisseur"){
      # si l'objectif est rempli
      if((badges_joueurs %>% filter(joueur==player,
                                   badge=="Co"))$nombre >= 8){
        objectifs[i,]$possible = TRUE
        print("Vers l'infini et au-delà")
      }
      
    }else if(objectifs[i,]$obj=="Planificateur"){
      # si l'objectif est rempli
      if(cartes %>% filter(joueur==player, where == "M") %>% nrow() >= 16){
        objectifs[i,]$possible = TRUE
        print("I'm a poor lonesome cowboy")
      }
      
    }
  }
  
  obj_return = objectifs %>%
    filter(joueur == player,
           possible==TRUE,
           valide == "")
  
  return(obj_return)
}


# Cartes actions ----

#Renvoie la liste des actions ----
f_all_actions = function(actions21_jeu){
 #  # # à suppr
 #  cartes2=cartes
 #  cartes2 = cartes2 %>%
 # #   filter(where=="M") %>%
 #    mutate(where="J") %>%
 #    filter(type_carte == "21")
 #  actions21_jeu = cartes2
 #  #

  
  actions21_pour_affich = actions21_jeu %>%
    left_join(actions21_cout,by="id")%>%
    left_join(actions21_gain%>%
                summarise_all(funs(if(all(.==0)) "0" else toString(unique(.[.!=0])))),
              by=c("id_idaction"))%>%#("id2"))%>%
    mutate(en_clair = paste0(nom," : ",cout_en_clair,", ",gain_en_clair))
#    filter(ok == TRUE)

  actions21_jeu = actions21_jeu %>%
    left_join(actions21_cout,by="id")%>%
    left_join(actions21_gain, by=c("id_idaction","idaction","id"))

    liste = list(actions21_pour_affich, actions21_jeu)
  return(liste)

}

# Fonction pour réaliser les conséquences d'une carte Action bleue 21
f_consequence_action = function(navire,
                                global,
                                params_joueur,
                                player,
                                actions21_jeu,
                                id_input,
                                cartes){
  
  # id_idaction = input$crt_action, choicecValues = mdf$actions21_pour_affich$id_idaction; sous la forme 15_1 = action 1 de la carte 15
  # attention,  length(b$id_idaction) !=  length(a$id_idaction)
  # a = actions21_pour_affich
  # b = actions21_jeu
  
 # id_input = "22_1"
  
  # Etape 1 : payer le COUT de l'action ----
  # NORMALEMENT d n'a qu'une seule ligne
  d = actions21_jeu %>%
    select(-c(unite_gain,type_gain,nombre_gain,gain_en_clair))%>%
    distinct()%>%
    filter(id_idaction == id_input)%>%
    mutate(nombre_cout = -nombre_cout)
  
  idcarte = d$id
  
  if(nrow(d)>1){print("Les sanglots longs des violons de l'automne...")}
  
  liste_temp = f_consequence_i(
    unite = d$unite_cout,
    type = d$type_cout,
    nombre = d$nombre_cout,
    
    navire = navire,
    id_carte = d$id,
    global = global,
    params_joueur = params_joueur,
    player = player,
    cartes = cartes)
  
  navire = liste_temp[[1]]
  global = liste_temp[[2]]
  params_joueur = liste_temp[[3]]
  cartes = liste_temp[[4]]
  
  
  # Etape 2 : collecter les GAINS de l'action ----
  
  c = actions21_jeu %>%
    filter(id_idaction == id_input)
  
    for(i in 1:nrow(c)){ #pour toutes les lignes qui concerne cette action en particulier
      liste_temp = f_consequence_i(
        unite = c[i,]$unite_gain,
        type = c[i,]$type_gain,
        nombre = c[i,]$nombre_gain,
        
        navire = navire,
        id_carte = c[i,]$id,
        global = global,
        params_joueur = params_joueur,
        player = player,
        cartes = cartes)
      
      navire = liste_temp[[1]]
      global = liste_temp[[2]]
      params_joueur = liste_temp[[3]]
      cartes = liste_temp[[4]]
    }
  
  
  
  # FIN ----
  
  # on note que cette carte a déjà servi, on ne peut plus la réutiliser.
  cartes[which(cartes$id == idcarte),]$check = FALSE
  
  print("fin consequence action 21")
  liste = list(navire,global,params_joueur,cartes)
  return(liste)
}

# Vérif fin de partie ----
# si fin de partie, on revoie mdf$statut = fin_partie
# sinon, on renvoie le statut d'entrée
# f_verif_fin = function(statut, global){
#   
#   if(global$ocean >= 9 &&
#      global$oxyg >= 14 &&
#      global$temp >= 8){
#     return(TRUE)
#   }else{return(FALSE)}
# }
f_verif_fin = function(statut, global){
  
  if(global$ocean >= 9 &&
     global$oxyg >= 14 &&
     global$temp >= 8){
    statut = "fin_partie"
  }
    return(statut)
}

# Slider ----
f_slider = function(id_carte, cartes, navire,session){
# cartes = cartes_en_main
# navire = navire_actif
  
  ns <- session$ns

  # cartes_en_main = cartes %>%
  #   filter(where == "M",
  #          joueur == joueur)
  # 
  badgeCo = f_check_badge(cartes=cartes,
                id_carte=id_carte,
                badge="Co")
  

  badgeEs = f_check_badge(cartes=cartes,
                         id_carte=id_carte,
                         badge="Es")
  
  # deux conditions à l'apparition du slider :
  # 1- la carte qu'on veut poser a un badge correspondant
  # 2- le stock d'acier/titane n'est pas nul
  if(badgeCo == TRUE){
    if(navire[which(navire$nom=="Acier"),]$stock >0){

      liste_temp = f_min_max_slider(
        navire = navire,
        cartes=cartes,
        id_carte = id_carte,
        badge="Co",
        joueur = unique(navire$joueur))
      
      min1 = liste_temp[[1]]
      max1 = liste_temp[[2]]
      valeur = liste_temp[[3]]
      rm(liste_temp)
    
      slider = sliderInput(ns("slider_acier"),
            label = h6("Payer en acier"),
            min = min1,
            max = max1,
            value = valeur)
   #   print(paste("Slider acier pour la carte",id_carte))
    }else{slider = h6("Pas d'acier en stock.")}
  
  }else if(badgeEs == TRUE){
    if(navire[which(navire$nom=="Titane"),]$stock >0){
      

      liste_temp = f_min_max_slider(
        navire = navire,
        cartes=cartes,
        id_carte = id_carte,
        badge="Es",
        joueur = unique(navire$joueur))
      
      min1 = liste_temp[[1]]
      max1 = liste_temp[[2]]
      valeur = liste_temp[[3]]
      
      slider =  sliderInput(ns("slider_titane"),
                  label = h6("Payer en titane"),
                  min = min1,
                  max = max1,
                  value = valeur)
    #  print(paste("Slider titane pour la carte",id_carte))
    }else{slider = h6("Pas de titane en stock.")}
  }else{slider = h6("Pas de badge = pas de réduction.")}
    
  return(slider) 
}
# AFFICHAGE, UI ----
f_cartes_en_main = function(cartes, player){
  cartes_en_main = cartes %>%
    filter(joueur == player,
           where =="M")%>%
    select(-c(sur_cette_carte_type, sur_cette_carte_nombre, check,ok))
  return(cartes_en_main)
}

f_radio = function(id="inputId", label="Mon label",
                   names, values,
                   data,
                   str_else="Pas possible."){
  
  # names = "nom"
  # values = "id"
  # data = cartes
 
  if(nrow(data)>0){
   radio = radioButtons(
      inputId = id,
      label = label,
      choiceNames = data[[names]],
      choiceValues = data[[values]]
    #  selected = character(0))
   )
  }else{
    radio = h6(str_else)
  }
  
  return(radio)
}

f_bouton = function(mdf, session, input){
  
  if(mdf$statut == "actif"){
    
    # 8 plantes en forêt
    if(input$move == "8pl"){
      if(mdf$navire_actif[which(mdf$navire_actif$nom=="Plantes"),]$stock >= 8){
        mdf$tuile="fo"
        mdf$bouton = h6("Choisissez une case pour cette nouvelle tuile forêt.")
        #mdf$bouton = actionButton(inputId = "do_move", label = "TEMP", icon = icon("play"))
        #    validate(need(input$plateauPlot_selected, "Choisissez une case pour cette nouvelle tuile."))
        #   mdf$bouton = actionButton(inputId = "do_move", label = "Convertir 8 plantes", icon = icon("play"))
      }else{h6("Pas assez de plantes.")}
    }else if(input$move == "8ch"){
      mdf$tuile=""
      if(mdf$navire_actif[which(mdf$navire_actif$nom=="Chaleur"),]$stock >= 8){
        mdf$bouton = actionButton(inputId = "do_move", label = "Convertir 8 chaleur", icon = icon("play"))
      }else{h6("Pas assez de chaleur.")}
      
      # poser une carte de la main 
    } else if(input$move == "crt"){
      if(nrow(mdf$cartes_en_main)>0){ # si on a des cartes en main
        if(nrow(mdf$cartes_en_main_achetables)>0){ # et qu'on peut en acheter au moins une
          
          mdf$tuile=""
          req(input$crt_poser)
          #   print(paste("id crt_poser : ",input$crt_poser))
          
          c = effet_cartes[which(effet_cartes$id == input$crt_poser),]
          
          if(nrow(c)>0){
            print("La carte aura des effets.")
            if("tuile" %in% c$type){
              #  print(paste("OK va pour une tuile",c[which(c$type == "tuile"),]$unite))
              mdf$tuile=c[which(c$type == "tuile"),]$unite
              mdf$tuile_nb = c[which(c$type == "tuile"),]$nombre
              mdf$bouton = h6(paste("Choisissez ", mdf$tuile_nb ,
                                   " case(s) pour la/les nouvelle(s) tuile(s) ",mdf$tuile))
              # validate(need(input$plateauPlot_selected, "Choisissez une case pour cette nouvelle tuile."))
              # mdf$bouton = actionButton(inputId = "do_move", label = "Jouer cette carte", icon = icon("play"))
            }else{
              #  print("La carte n'implique pas de tuile.")
              mdf$bouton = actionButton(inputId = "do_move", label = "Jouer cette carte", icon = icon("play"))
            }
          }else{
            #  print("La carte n'a pas d'effets immédiats.")
            mdf$bouton = actionButton(inputId = "do_move", label = "Jouer cette carte", icon = icon("play"))
          }
          rm(c)
          
          
          
          
        }else{h6("Vous n'avez pas assez d'argent.")}
      }else{h6("Vous n'avez pas de cartes en main, voyons !")}
      
      # projet standard
    }else if(input$move=="std"){
      
      mdf$tuile=""
      req(input$controls_std)
      # si on peut se payer le projet standard sélectionné
      if(projets_std[which(projets_std$std == input$controls_std),]$cost <=  mdf$navire_actif[which(mdf$navire_actif$nom=="Argent"),]$stock){
        
        if(input$controls_std %in% c("oc","vi","fo")){
          mdf$tuile = input$controls_std
          mdf$bouton = h6(paste("Choisissez une case pour la nouvelle tuile ",mdf$tuile))
          
          # validate(need(input$plateauPlot_selected, "Choisissez une case pour cette nouvelle tuile."))
          
          # # si on choisit de poser un océan
          # if(mdf$tuile == "oc"){
          #   mdf$bouton = actionButton(inputId = "do_move", label = "Poser un océan", icon = icon("play"))
          #   }else if(mdf$tuile  == "vi"){
          #     mdf$bouton = actionButton(inputId = "do_move", label = "Poser une ville", icon = icon("play"))
          #   }else if(mdf$tuile == "fo"){
          #     mdf$bouton = actionButton(inputId = "do_move", label = "Poser une forêt", icon = icon("play"))
          #   }
          
        }else if(input$controls_std == "en"){
          
          mdf$tuile=""
          mdf$bouton = actionButton(inputId = "do_move", label = "Construire une centrale", icon = icon("play"))
        }else if(input$controls_std == "tp"){
          
          mdf$tuile=""
          mdf$bouton = actionButton(inputId = "do_move", label = "Faire monter la température", icon = icon("play")) 
          
        }else{ # autres projets standards
          h6("Pas possible pour l'instant.")
          
          mdf$tuile=""
          #mdf$bouton = actionButton(inputId = "do_move", label = "Jouer ce projet", icon = icon("play"))
        }
      }else{h6("Vous n'avez pas assez d'argent pour ce projet standard.")}
      
      # action de carte bleue 21  
    }else if(input$move=="act"){
      
      mdf$tuile=""
      
      c = mdf$actions21_jeu[which(mdf$actions21_jeu$id_idaction == input$crt_action),]
      
      if("tuile" %in% c$type_gain){
        mdf$tuile=c[which(c$type_gain == "tuile"),]$unite_gain
        print(paste("Une tuile comme conséquence de carte 21 :",mdf$tuile))
        mdf$bouton = h6(paste("Choisissez une case pour la nouvelle tuile.",mdf$tuile))
        # validate(need(input$plateauPlot_selected, "Choisissez une case pour cette nouvelle tuile."))
        # mdf$bouton = actionButton(inputId = "do_move", label = "Jouer cette action (+ tuile)", icon = icon("play"))
        # 
      }else{
        
        mdf$bouton = actionButton(inputId = "do_move", label = "Jouer cette action", icon = icon("play"))
      }
      rm(c)
      # valider un objectif
    }else if(input$move=="obj"){
      
      mdf$tuile=""
      if(nrow(mdf$objectifs_persos)>0){
        if(mdf$navire_actif[which(mdf$navire_actif$nom=="Argent"),]$stock >= 8){
          mdf$bouton = actionButton(inputId="do_move",label="Valider cet objectif", icon = icon("play"))
        }else{bouton = h5("Pas assez d'argent")}
      }else{bouton = h5("Aucun objectif disponible.")}
    }else if(input$move=="end"){
      
      mdf$tuile=""
      mdf$bouton = actionButton(inputId="do_move",label="Fin de génération", icon = icon("play"))
      
      # tout autre résultat
    }else{h5("Comment êtes-vous arrivé là ?")#
      # mdf$bouton = actionButton(inputId="do_move",label="Go !", icon = icon("play"))
    }
    
  }
  
  
  return(mdf)
}


f_new_params = function(session, input, mdf,nb_joueurs){
  print("début f_new_params")
  mdf$params_joueur[which(mdf$params_joueur$nom=="J1"),]$nom_complet = input$nom_J1
  mdf$params_joueur[which(mdf$params_joueur$nom=="J1"),]$couleur = input$color_J1
  if(nb_joueurs >= 2){
    mdf$params_joueur[which(mdf$params_joueur$nom=="J2"),]$nom_complet = input$nom_J2
    mdf$params_joueur[which(mdf$params_joueur$nom=="J2"),]$couleur = input$color_J2
  }
  if(nb_joueurs >= 3){
    mdf$params_joueur[which(mdf$params_joueur$nom=="J3"),]$nom_complet = input$nom_J3
    mdf$params_joueur[which(mdf$params_joueur$nom=="J3"),]$couleur = input$color_J3
  }
  print("fin f_new_params")
  return(mdf$params)
}


# MODULES ----
# SCORES module ----


f_calculs_score = function(mdf){
  # observeEvent(mdf$statut,{
  #   if(mdf$statut == "fin_partie"){
  print("Début calculs scores")
  # points sur les cartes ----
  cartes = mdf$cartes
  PV_cartes = cartes %>%
    filter(where == "J")%>%
    mutate(points = as.numeric(points))%>%
    group_by(joueur)%>%
    summarise(PV_cartes = sum(points))
  
 print(paste("PV_cartes",PV_cartes))
  
  # points spéciaux----
  PV_spe_data = points_speciaux %>%
    left_join(cartes, by="id")%>%
    filter(where == "J")
  PV_spe = data.frame(
    joueur = c("J1","J2"),
    PV = c(0,0)
  )
  liste_badges = mdf$badges_joueurs
  liste_ressources = mdf$cartes %>%
    filter(sur_cette_carte_nombre > 0)
  liste_tuile = mdf$plateau

  if(nrow(PV_spe_data)>0){
    for(i in 1:nrow(PV_spe_data)){
      type = PV_spe_data[i,]$type_points
      player = PV_spe_data[i,]$joueur
      nb = PV_spe_data[i,]$nb_points
      detail = PV_spe_data[i,]$details_points
      id_carte = PV_spe_data[i,]$id_carte
      
      # i parcourt la liste des cartes en jeu qui ont des PV spéciaux

      if(type=="badge"){
        # si l'une d'elle dépend du nombre de badges en jeu d'un joeur
        l_badges = liste_badges %>%
          filter(joueur == player) %>%
          filter(badge == detail)
          PV = (nb*l_badges$nombre)
          
          PV_spe[which(PV_spe$joueur == player),]$PV %+=% PV
          rm(PV)
        
      }else if(type=="sur_cette_carte"){
        if(nrow(liste_ressources)>0){
          l_ressources = liste_ressources %>%
              filter(id == id_carte)
          PV = (nb*l_ressources$sur_cette_carte_nombre)
          PV_spe[which(PV_spe$joueur == player),]$PV %+=% PV
        }else{print("Oulà, quelle chaleur")}
      }else if(type =="tuile" & detail == "adjacent"){
        # on va chercher la case où l'on a mis la tuile associée à la carte dont on calcule les points
        case = liste_tuile %>% filter(Source == id)
        cases_adj = f_cases_adjacentes(plateau = liste_tuile, case = case) %>%
          filter(Type_tuile == PV_spe_data[i,]$adjacent_type)
        PV = (nb*nrow(cases_adj))    
        PV_spe[which(PV_spe$joueur == player),]$PV %+=% PV
        
      }else if(type =="tuile" & detail != "adjacent"){
        l_tuile = liste_tuile %>%
          filter(Type_tuile == detail)
        PV = (nb*nrow(l_tuile))    
        PV_spe[which(PV_spe$joueur == player),]$PV %+=% PV
    }
    }
  }
#  print(paste("PV_spe",PV_spe))
  
  # points de forêt----
  plateau = mdf$plateau
  PV_fo = plateau %>%
    filter(Type_tuile == "fo")%>%
    group_by(Proprio)%>%
    summarise(PV_fo = n())%>%
    mutate(joueur = Proprio)%>%
    select(-Proprio)
  
 print(paste("PV_fo",PV_fo))
  
  # points de ville----
  params = mdf$params_joueur
  PV_vi = data.frame(
    joueur = "J0",
    PV_vi = 0
  )
  for(i in 1:nrow(params)){
    PV_vi[i,]$joueur = params$nom[i]
    PV_vi[i,]$PV_vi = 0
    villes_i = plateau %>% filter(Proprio == PV_vi[i,]$joueur, Type_tuile == "vi")
    for(j in villes_i$Num){
      cases_ij = f_cases_adjacentes(plateau, j)
      foret_ij = cases_ij %>% filter(Type_tuile == "fo")
      PV_vi[which(joueur==PV_vi[i,]$joueur),]$PV_vi %+=% nrow(foret_ij)
    }
  }
 print(paste("PV_vi",PV_vi))
  # points d'objectifs----
  objectifs = mdf$objectifs
  PV_obj = objectifs%>%
    filter(valide != "")%>%
    group_by(valide)%>%
    summarise(PV_obj = 5*n())%>%
    mutate(joueur = valide)%>%
    select(-valide)
#  print(paste("PV_obj",PV_obj))
  #-----
  PV = 
  #   data.frame(joueur = c(""), PV = c(0)) %>%
  #  full_join(
      PV_cartes %>%
   #   ,by="joueur") %>%
    full_join(PV_fo, by = "joueur")%>%
    full_join(PV_vi, by = "joueur")%>%
    full_join(PV_obj, by = "joueur")%>%
    full_join(PV_spe, by = "joueur")%>%
    replace(is.na(.), 0)%>%
    dplyr::mutate_if(is.numeric, round, digits=0)%>%
    as.data.frame()
    # group_by(joueur)%>%
    # mutate(PV_tot = )

  print("Fin màj scores")
  return(PV)
}



# fonction pour générer les couleurs des joueurs
f_color_widget = function(nb_joueurs){
  if(nb_joueurs == 1){
    colorWidget = tagList(
        h4("Premier joueur"),
        textInput("nom_J1", label="Nom :",value="Joueur 1"),
        colourInput("color_J1", NULL, "yellow", palette = "limited")
      )
    
  }else if(nb_joueurs == 2){
    colorWidget = tagList(
        h4("Premier joueur"),
        textInput("nom_J1", label="Nom :",value="Joueur 1"),
        colourInput("color_J1", NULL, "yellow", palette = "limited"),
        
        h4("Deuxième joueur"),
        textInput("nom_J2", label="Nom :",value="Joueur 2"),
        colourInput("color_J2", NULL, "red", palette = "limited")
      )
  }

  return(colorWidget)
}



