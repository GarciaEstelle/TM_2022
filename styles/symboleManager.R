# Couleurs de l'appli ----

bg_color = "#FAF5E8"
fg_color = "black"

# couleurs autorisées par le dashboard
liste_couleurs = data.frame(
  hex = c(
    "#dd4b39", #"red",
    "#f39c12", #"yellow",
    "#00c0ef",#"aqua",
    "#0073b7",#"blue",
    "#3c8dbc", #light-blue
    "#00a65a", # "green",
    "#001f3f",## "navy",
    "#39cccc",## "teal",
    "#3d9970",# "olive",
    "#01ff70",#"lime",
    "#ff851b",#"orange", 
    "#f012be",#"fuchsia", 
    "#605ca8",#"purple",
    "#d81b60",#"maroon",
    "#111111" #"black")
  ),
  col = c("red", "yellow", "aqua", "blue", "light-blue", "green", "navy", "teal", "olive", "lime", "orange", "fuchsia", "purple", "maroon", "black")
)

# couleurs des tuiles (plot) ----
couleurs_plot = data.frame(
  hex = c("#808080","#003399","#003399","#009900","#996633","#ffffff"),
  rgb = c("rgb(128, 128, 128)","rgb(0, 51, 153)","rgb(0, 51, 153)","rgb(0, 153, 0)","rgb(153, 102, 51)","rgb(255, 255, 255)"),
  nom = c("vi","oc","Ocean","fo","special","")
)


# Tableaux d'îcones ----

# Ressources (standards + sur les cartes)
data_icones_hex <- data.frame(
  nom =    c("Argent","Acier","Titane","Plantes","Energie","Chaleur",
             "minus","Chasseur","Animal","Microbe","Science"),
             
  fa_nom = c("coins", "hammer","star","leaf","bolt","fire",
             "minus","space-shuttle","paw","bacterium","atom"),
  hex = c("#ffcc00", "#996600", "#ffffff", "#00cc00", "#cc0099", "#ff0000",
          "#c60000","#4d4d4d","#339933","#66ff66","#e6e6e6")
)
data_icones_sans_hex <- data.frame(
  nom = c("An","Jo","Es","Vi","En","Pl","Co","Sc","Mi","Ve","Te",
          "NT","points",
          "fo","oc","vi","special",
          "fleche","carte","asterisk",
          "oxyg","temp",
          "adv","min","max","colon","action21","effet22"),
  fa_nom = c( "paw","rocket","star","city","bolt","leaf", "hammer","atom", "bacterium", "venus", "globe",
              "trophy","shield",
              "tree","water","city","question",
              "arrow-right","file","asterisk",
              "wind","thermometer-three-quarters",
              "users","compress","expand","ellipsis-v","redo","play")
)
  
icon_data_fa <- data_icones_hex %>%
  group_by(nom)%>%
  mutate(
    icones = c(fa(name = fa_nom, fill = hex))
  )

data_icones = data_icones_hex %>%
#  select(-hex)%>%
  full_join(data_icones_sans_hex, by = c("nom", "fa_nom"))%>%
  mutate(hex = case_when(is.na(hex) ~ "#d9d9d9",
                         TRUE ~ hex))

f_switch_icon_name <- function(name){
  fa_nom = (data_icones %>%
    filter(nom == name))$fa_nom
  return(fa_nom)
}

f_switch_col <- function(name) {
  hex = (data_icones %>%
              filter(nom == name))$hex
  return(hex)
}

f_switch_icon <- function(nom) {
  fa = fa_i(name = f_switch_icon_name(nom),
          fill = f_switch_col(nom))
  return(fa)
}

# f_switch_icon <- function(nom) {
#   fa = paste0("<i class='fa-solid fa-",f_switch_icon_name(nom),"'></i>")
#   
#   return(fa)
# }

# Fonctions qui génèrent les icônes ----

f_icone_badges <- function(df){
  #df = cartes
  df$icone_b = ""
  
  for(i in 1:length(df$badges)){
    str_i = ""
    a = gsub('([[:upper:]])', ' \\1', df$badges[i])
    b = as.list(strsplit(a, " ")[[1]])[-1]
    if(length(b)>0){
      for(j in 1:length(b)){
        
        #  str_i = paste0(str_i, fa(name = f_switch_icon_name(b[[j]])))
        str_i = paste0(str_i,
                       "<span class='fa-stack'> <i class='fa fa-circle fa-stack-2x' role='presentation' aria-label='square icon' style='color: rgb(89, 89, 89)'></i><i class='fa fa-",f_switch_icon_name(b[[j]])," fa-stack-1x' role='presentation' aria-label='",f_switch_icon_name(b[[j]])," icon' style='color: rgb(242, 242, 242)'></i></span>"
        )
      }
    }
    df$icone_b[i] = str_i
  }
  return(df)
}

f_icone_points <- function(df){
  df2 = df %>%
    mutate(icone_p = case_when(
      points > 0 ~ paste0(
          "<span class='fa-layers'> <i class='fa-solid fa-",
          f_switch_icon_name("points"),"'></i> <span class='fa-layers-counter'>",
          points," </span> </span>"
          ),
      points < 0 ~ paste0(
          f_switch_icon("minus"),
          "<span class='fa-layers'> <i class='fa-solid fa-",
          f_switch_icon_name("points"),"'></i> <span class='fa-layers-counter'>",
          points," </span> </span>"
          ),
      TRUE ~ ""
    ))
  
  
  return(df2)
}

f_icone_effets22 <- function(df){
  df = effet22
  df$icone_e = ""
  
  # Etape 1 : Avant l'observé : si vise = adv
  for(i in 1:nrow(df)){
    if(df$qui[i] == "tous"){
      str1 = f_switch_icon("adv")
    }else{str1 = ""}
    
    
  # Etape 2 : Observé
    
 #   print(df$type_observe[i])
    if(df$type_observe[i]=="badge"){
   #   for(j in 1:length(df$badge_observe[i])){
        str = ""
        a = gsub('([[:upper:]])', ' \\1', df$badge_observe[i])
        b = as.list(strsplit(a, " ")[[1]])[-1]
        if(length(b)>0){
          for(k in 1:length(b)){
            
            #  str_i = paste0(str_i, fa(name = f_switch_icon_name(b[[j]])))
            str = paste0(str, "<span class='fa-stack'> <i class='fa fa-circle fa-stack-2x' role='presentation' aria-label='square icon' style='color: rgb(89, 89, 89)'></i><i class='fa fa-",
                         f_switch_icon_name(b[[k]])," fa-stack-1x' role='presentation' aria-label='",
                         f_switch_icon_name(b[[k]])," icon' style='color: rgb(242, 242, 242)'></i></span>"
            )
          }}
     # }
    }else if(df$type_observe[i]=="tuile"){
      str = paste0(
        "<span class='fa-stack'>
                <i class='fa fa-circle fa-stack-2x' role='presentation' aria-label='square icon' style='color:",
                couleurs_plot[which(couleurs_plot$nom == df$param[i]),]$rgb ,"'></i>
                <i class='fa fa-",f_switch_icon_name(df$param[i])," fa-stack-1x' role='presentation' aria-label='",
                f_switch_icon_name(df$param[i])," icon' style='color: rgb(217, 217, 217)'></i>
                </span>"
      )
    }else if(df$type_observe[i]=="slider"){
      str = f_switch_icon(df$unite_gain[i])
    }
    
    # Etape 3 : conséquences
    str2 = f_icone_i(nombre = df$nombre_gain[i],
                     unite = df$unite_gain[i],
                     type = df$type_gain[i],
                     vise = "self", opt_minus = TRUE,
                     opt_vise = TRUE)
    
    # Fin
    df$icone_e[i] = paste0(f_switch_icon("effet22"),str1,str,f_switch_icon("colon"),str2)

  }
  return(df)
}

# f_icone travaille sur les colonnes nombre, unite, type
f_icone <- function(df){
  if(nrow(df)>0){
    df$icone = ""
    
    for(i in 1:nrow(df)){
  #     print(df$id[i])
      new = f_icone_i(nombre = df$nombre[i],
                              unite = df$unite[i],
                              type = df$type[i],
                              vise = df$vise[i],
                              opt_vise = TRUE,
                              opt_minus = TRUE)
      print(length(new))
      print(length(df$icone[i]))
      df$icone[i] = new
   #    print(paste("end",df$id[i]))
    }
  }
  return(df)  
}

f_icone_prerequis <- function(df){

      df$icone_pr = ""
    
    for(i in 1:nrow(df)){
       #    print(df$id[i])
      df$icone_pr[i] = f_icone_i(nombre = df$nombre[i],
                              unite = df$valeur[i],
                              type = df$type[i],
                              vise = df$vise[i],
                              opt_vise = FALSE,
                              opt_minus = FALSE)
      
       #   print(paste("end",df$id[i]))
    }
  
  return(df)
}

f_icone_action21_fleche <- function(df){
# df = actions21 %>% f_icone()

  df2 = df %>%
    select(id, idaction, id_idaction)%>%
    distinct()%>%
    mutate(icone = "")
  
  for(i in unique(df$id_idaction)){
  #  i="14_1"
    # print(i)
    str_i = ""
    str_1 = ""
    str_2 = ""
    
    dfi = df[which(df$id_idaction == i),]

    sens1 = dfi %>% filter(sens==1)
    sens2 = dfi %>% filter(sens==2)
    
    if(nrow(sens1)>0){
      str_1 = paste0(# "<span class='fa-layers' style='background:MistyRose'>",
                      sens1$icone)
                     #  "</span>")
      # str_1 = paste0( "<div> <span style='background-color:red'>",
      #                 sens1$icone,
      #                 "</span></div>")
      
    }
    if(nrow(sens2)>0){
        str_2 = paste0(
       #   "<span class='fa-layers' style='background:MistyRose'>",
          ( sens2 %>% summarise(icone = paste(icone, collapse ="")))$icone)
         # "</span>")
    }
    # print(str_1)
    # print(str_2)
    
  
    str_i = paste0(f_switch_icon("action21"),
                   str_1, f_switch_icon("fleche"), str_2,"<br>")
  
    df2[which(df2$id_idaction == i),]$icone = str_i
  }
  
 
  return(df2)
}

f_icone_i <- function(nombre, unite, type, vise,
                      opt_vise, opt_minus){
  
  
  str_i = ""
  if(opt_minus == TRUE){
    if(nombre<0){
      str_i = paste0(str_i, f_switch_icon("minus"))
    }
  }
  if(opt_vise == TRUE){
    if(vise=="adv"){
      str_i = paste0(str_i, f_switch_icon("adv"))
    }
  }
  
  str = f_icone_i_str(nombre = nombre, unite = unite,
                      type = type, vise = vise)
  if(abs(nombre)==1){
    str_i = str
  }else if(abs(nombre)>=4){
    str_i = paste0("<span class='fa-layers-counter'>",nombre,
                   "<span class='fa-layers'>", str,
                   "</span>")
  } else if(abs(nombre)<4){
    str_j = ""
    for(j in 1:abs(nombre)){
      str_j = paste0(str_j,str)
    }
    str_i = str_j
  }
  return(str_i)
}

f_icone_i_str <- function(nombre, unite, type, vise){
  if(type == "flux"){
    str = paste0(
      "<span class='fa-stack'>
                <i class='fa fa-square fa-stack-2x' role='presentation' aria-label='square icon' style='color: rgb(102, 51, 0)'></i>
                <i class='fa fa-",f_switch_icon_name(unite)," fa-stack-1x' role='presentation' aria-label='",f_switch_icon_name(unite)," icon' style='color: ",f_switch_col(unite),"'></i>
                </span>"
    )
  }else if(type %in% c("stock", "NT","global",
                       "sur_cette_carte")){
    str = f_switch_icon(unite)
    
    # }else if(type == "NT"){
    # #   str = f_switch_icon(unite)
    # }else if(type == "global"){
    #   str = f_switch_icon(unite)  
    
  }else if(type =="badge"){
    str = paste0( "<span class='fa-stack'> <i class='fa fa-circle fa-stack-2x' role='presentation' aria-label='square icon' style='color: rgb(89, 89, 89)'></i><i class='fa fa-",
                  f_switch_icon_name(unite)," fa-stack-1x' role='presentation' aria-label='",
                  f_switch_icon_name(unite)," icon' style='color: rgb(242, 242, 242)'></i></span>"
    )  
  
  }else if(type == "carte"){
    str = f_switch_icon(type)
    
    # }else if(type == "sur_cette_carte"){
    #   str = f_switch_icon(unite)
    
  }else if(type == "sur_carte"){
    str = paste0(f_switch_icon(unite),
                 f_switch_icon("asterisk"))
    
  }else if(type ==  "tuile"){
    # str = f_switch_icon(unite)
    str = paste0(
      "<span class='fa-stack'>
                <i class='fa fa-circle fa-stack-2x' role='presentation' aria-label='square icon' style='color:",couleurs_plot[which(couleurs_plot$nom == unite),]$rgb ,"'></i>
                <i class='fa fa-",f_switch_icon_name(unite)," fa-stack-1x' role='presentation' aria-label='",f_switch_icon_name(unite)," icon' style='color: rgb(217, 217, 217)'></i>
                </span>"
    )
  }else if(type %in% c("oxyg","temp")){
    str = paste0(
      f_switch_icon(type),
      f_switch_icon(unite)
    )
  }else if(type == "oc"){
    str = paste0(
      "<span class='fa-stack'>
                <i class='fa fa-circle fa-stack-2x' role='presentation' aria-label='square icon' style='color:",couleurs_plot[which(couleurs_plot$nom == "oc"),]$rgb ,"'></i>
                <i class='fa fa-",f_switch_icon_name("oc")," fa-stack-1x' role='presentation' aria-label='",f_switch_icon_name("oc")," icon' style='color: rgb(217, 217, 217)'></i>
                </span>"
    )
    # }else if(type == "badge"){
    #   str = f_switch_icon(unite)
  }else{
    str = ""
  }
return(str)
}
