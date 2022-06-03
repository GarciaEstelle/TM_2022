library(shiny)
library(shinydashboard)
library(dashboardthemes)

# # MODULES ----
# # Cartes ----
cartes_UI <- function(id){
  ns = NS(id)
  tagList(
  #  radioButton(ns("crt_poser")),
    uiOutput(ns("crt_poser_validate_ui"), label = "Poser la carte"),
    #actionButton(ns("crt_poser_validate"), label = "Poser la carte"),
    uiOutput(ns("sliders")),
    DTOutput(ns("table_poser")),
    verbatimTextOutput(ns("render_selected_line"))
  )
}
cartes_Server <- function(id,mdf){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(mdf$test$n,{
      if(mdf$statut=="actif"){
        #mdf$plat_en_main = f_plat_en_main(mdf = mdf)
        mdf$cartes = f_appliquer_reduction(mdf=mdf)
        mdf$plat_girafe = f_plat_girafe(mdf=mdf)
        mdf$cartes_en_main = f_check_poser_cartes_de_sa_main(mdf = mdf)
        print("Recalcul des cartes en main")
      }
   })
   
    # RenderDF pour le tableau des cartes en main
     output$table_poser = renderDT({
       mdf$cartes_en_main %>%
         datatable(caption = "Votre main",
           rownames = FALSE,
           selection = list(mode = 'single',
                            target = 'row',
                            selectable = which(mdf$cartes_en_main$ok == TRUE)),
           options=list(columnDefs = list(list(visible=FALSE, targets=c("ok","type_carte")))))%>%
         formatStyle(columns = 'type_carte',
                     target="row",
                     color = styleEqual(
                       c(21,22,1,3),
                       c('#00ccff', '#99ccff', '#99ff99', '#ff9999')
                       ))%>%
         formatStyle(columns = 'ok',
                     target="row",
                     color = styleEqual(
                       c(FALSE),
                       c('gray')
                     ))
#   })    #} 
     })
   
     a = reactiveValues()

   # à chaque fois qu'on clique sur une ligne
   # affichage éventuel des sliders
   observeEvent(input$table_poser_row_last_clicked,{
    # print(paste("input table : ",input$table_poser_row_last_clicked))
     
     carte_p = mdf$cartes_en_main[input$table_poser_row_last_clicked,]
   #  print(paste("carte_p",carte_p$nom,"ok",carte_p$ok,"typeok",typeof(carte_p$ok)))
     
     if(carte_p$ok %in% c(TRUE,"true")){
      # print("BOUTON")
         a$slider =  f_slider(
           id_carte = carte_p$id,
            session = session,
            #  joueur = mdf$j_actif,
            cartes = mdf$cartes,
            navire = mdf$navire[which(mdf$navire$joueur==mdf$j_actif),]
           )
         a$bouton = actionButton(ns("crt_poser_validate"), label = "Poser la carte")
     }else{
       a$slider = h6("")
       a$bouton = h6("")
     }
   })
   output$sliders <- renderUI({
     a$slider
     # f_slider(id_carte = carte_p$id,
     #          session = session,
     #        #  joueur = mdf$j_actif,
     #          cartes = mdf$cartes,
     #          navire = mdf$navire[which(mdf$navire$joueur==mdf$j_actif),])
   })
   output$crt_poser_validate_ui <- renderUI({
      a$bouton
   })
   
   # Conséquences d'appuyer sur le bouton de confirmation "crt_poser_validate"
  #observeEvent(input$table_poser_row_last_clicked,{
   observeEvent(input$crt_poser_validate,{
      
      carte_p = mdf$cartes_en_main[input$table_poser_row_last_clicked,]
      cout = carte_p$cost
      print(paste("Début traitement carte posée",carte_p$nom))
      # mise à jour du stock d'acier et de titane
      if(typeof(input$slider_acier) == "integer"){
        if(input$slider_acier > 0){
          reduc_acier = input$slider_acier # nombre d'acier à dépenser
          cout = (cout - 2*reduc_acier) # réduction du prix qui reste à payer en argent
          mdf$navire[which(mdf$navire$joueur==mdf$j_actif & mdf$navire$nom=="Acier"),]$stock %-=% reduc_acier # mise à jour du stock d'acier

        }}
      if(typeof(input$slider_titane) == "integer"){
        if(input$slider_titane > 0){
          reduc_titane = input$slider_titane
          cout = (cout - 3*reduc_titane) # si on a dépensé plus de titane que nécessaire, on ne rend pas la monnaie en argent !
          mdf$navire[which(mdf$navire$joueur==mdf$j_actif & mdf$navire$nom=="Titane"),]$stock %-=% reduc_titane # mise à jour du stock de titane

        }}
      if(cout > 0){
        # Régler le coût pour poser la carte
        mdf$navire[which(mdf$navire$joueur == mdf$j_actif &
                         mdf$navire$nom == "Argent"),]$stock %-=% cout
      }
      # Mise à jour du tableau de cartes
      mdf$cartes[which(mdf$cartes$id == carte_p$id),]$where = "J"   # la carte passe de la main 'M' à devant le joueur 'J'
      mdf$cartes[which(mdf$cartes$id == carte_p$id),]$joueur = mdf$j_actif # et devient associée au joueur j_actif
      
      # Ajout de nouveaux badges à la liste du joueur
      mdf = f_badges_carte_vers_joueur(mdf = mdf, id_carte = carte_p$id)
      
      # déclenche l'effet immédiat de la carte id_carte
      #mdf = f_effets_immediats_carte(id_carte = carte_p$id, mdf = mdf, session)
      mdf = f_effets_immediats(choix = carte_p$id,
                               mdf = mdf,
                               session = session,
                               tab = effet_cartes)
      
      # et regarde si le posage des badges de id_carte a des effets sur des cartes effets22
      mdf = f_activation_effets22_carte(id_carte =  carte_p$id, mdf, session)
      
      # mdf$test$n %+=% 1
      # mdf$tuile = ""
      # mdf$tuile_nb = 0
      # mdf$tuile_source = ""
      print(paste(mdf$j_actif,"a posé une carte depuis sa main :",
                  carte_p$nom))
      if(mdf$tuile == ""){
        mdf$test$n %+=% 1
        print(paste(mdf$j_actif,"a fini son tour. Nouveau test =",mdf$test$n))
      }
   }) 
  return(mdf)
  })
}

actions21_UI <- function(id){
  ns = NS(id)
  tagList(
    DTOutput(ns("table_actions21"))
  )
}
actions21_Server <- function(id,mdf){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # RenderDF pour le tableau des cartes en main
    output$table_actions21 = renderDT({
      mdf$actions21_pour_affich %>%
        datatable(caption = "Actions disponibles",
                  rownames = FALSE,
                  selection = list(mode = 'single',
                                   target = 'row',
                                   selectable = which(mdf$actions21_pour_affich$ok == TRUE)
                  ),
                  options=list(
                    columnDefs = list(list(visible=FALSE, targets=c("ok","id","id_idaction"))),
                    dom = "t",
                    search.smart = FALSE,
                    ordering=F,
                    autoWidth = FALSE,
                    scrollX = TRUE
                    )
        
                  )%>%
 # stripeClasses = FALSE,
          #
          # columnDefs = list(
          #   list(width = '20px', targets = "_all"),
          #   list(className = "dt-center", targets = "_all")
          # )
        formatStyle(columns = 'ok',
                    target = "row",
                    color = styleEqual(
                      c(FALSE),
                      c('gray')
                    ))
    })
    
    observeEvent(input$table_actions21_rows_selected,{
      print(input$table_actions21_rows_selected)
      choix = mdf$actions21_pour_affich$id_idaction[input$table_actions21_rows_selected]
      id_carte = 
      print(paste0(mdf$j_actif," a choisi de jouer une action (",mdf$cartes[which(mdf$cartes$id==choix),]$nom,")"))
      tab = actions21
      numero = "id_idaction"

      
       
      mdf = f_effets_immediats(choix = input$my_radio,
                               mdf = mdf,
                               session = session,
                               tab = tab,
                               numero=numero)
      # si l'action n'implique pas de tuile, on change de joueur
      if(mdf$tuile == ""){
        mdf$test$n %+=% 1
        print(paste(mdf$j_actif,"a fait une action. Nouveau test :",mdf$test$n))
      }#else{
      #   mdf$tuile_source = input$my_radio
      # }

    })
  })
}


# Plateau ----
plateau_UI <- function(id){
  ns = NS(id)
  tagList(
    girafeOutput(ns("plateauPlot"))
    # plotOutput(ns("plateau"))
  )
}
plateau_Server <- function(id,mdf){
  moduleServer(id, function(input, output, session){
    
      mdf$plot = eventReactive(mdf$plateau,{
        mdf$plateau %>%
          ggplot()+
          geom_point(#data = mdf$plateau,
                     aes(x=Long, y=Lat,fill=Type_tuile,color=Ocean),
                     shape=21, size = 12)+
          
          geom_point(#mdf$plateau,
                     aes(x=Long, y=Lat,fill=Proprio),
                     shape=21, size = 8)+
          
          lims(x= c(0,22), y = c(0,12))+
          scale_fill_manual(
            breaks = c(mdf$params_joueur$nom,couleurs_plot$nom),
            values= c(mdf$params_joueur$couleur,couleurs_plot$hex))
          # theme(
          #   panel.background = element_rect(fill="black"),#fill='transparent', color=NA), #transparent panel bg
          #   plot.background = element_rect(fill="black"),#, color=NA), #transparent plot bg
          #   panel.grid.major = element_blank(), #remove major gridlines
          #   panel.grid.minor = element_blank(), #remove minor gridlines
          #   legend.background = element_rect(fill='transparent'), #transparent legend bg
          #   legend.box.background = element_rect(fill='transparent') #transparent legend panel
          # )
    })
    
    output$plateauPlot <- renderGirafe({
      girafe(ggobj = #mdf$plateau %>%
                #ggplot()+
               mdf$plot())
    })
  })
}
    
# Nouvelle partie ----
nouvellePartie_UI <- function(id, label = "Paramètres pour une nouvelle partie"){
  ns = NS(id)

  tagList(
  sliderInput(ns("new_nb_joueurs"), label = h5("Nombre de joueurs"), min = 0, 
                max = 2, value = 2, step=1),
  textInput(ns("nom_J1"), label="Nom :",value="Joueur 1"),
  colorSelectorInput(ns("color_J1"), label=NULL,
                     choices = liste_couleurs$hex,
                     selected = liste_couleurs$hex[1]),
  
  textInput(ns("nom_J2"), label="Nom :",value="Joueur 2"),
  colorSelectorInput(ns("color_J2"), label=NULL,
                     choices = liste_couleurs$hex,
                     selected = liste_couleurs$hex[2]),
  actionButton(ns("new_game"), "New game", icon = icon("redo"))
  )
}

nouvellePartie_Server <- function(id,mdf,parent){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Màj des paramètres pour la nouvelle partie :
    # (nombre de joueurs, nom complet et couleur associée)
    observeEvent(input$new_game,{
      mdf = reactiveValues()
      print(paste("Vous avez demandé une nouvelle partie à",input$new_nb_joueurs))
      temp = f_init_jeu(mdf=mdf, nombre = input$new_nb_joueurs)
      mdf = temp
      mdf$j_actif = "J0"
    
      if(input$new_nb_joueurs == 1){
        mdf$params_joueur = data.frame(
          nom = "J1",
          nom = input$nom_J1,
          couleur = input$color_J1,
          NT = 20,
          statut = "actif"
        )
      }else if(input$new_nb_joueurs ==2){
        mdf$params_joueur = data.frame(
          nom = c("J1","J2"),
          nom_complet = c(input$nom_J1,input$nom_J2),
          couleur = c(input$color_J1, input$color_J2),
          NT = 20,
          statut = "actif"
        )
      }
      updateTabItems(session=parent, "tabs", "jeu")
      })
    
    return(mdf)
    })   
}

nouvellePartiePopup_Server <- function(id,df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shinyalert(
      title = "Nouvelle partie",
      size = "m",
      html = TRUE,
      text =  tagList(
        #----
        sliderInput(ns("new_nb_joueurs"), label = h5("Nombre de joueurs"), min = 0, 
                    max = 2, value = 2, step=1),
        textInput(ns("nom_J1"), label="Nom :",value="Joueur 1"),
        colorSelectorInput(ns("color_J1"), label=NULL,
                           choices = liste_couleurs$hex,
                           selected = liste_couleurs$hex[1]),
        
        textInput(ns("nom_J2"), label="Nom :",value="Joueur 2"),
        colorSelectorInput(ns("color_J2"), label=NULL,
                           choices = liste_couleurs$hex,
                           selected = liste_couleurs$hex[2]),
        actionButton(ns("new_game"), "New game", icon = icon("redo"))
        #----
      )
      
    )
    observeEvent(input$shinyalert,{
      mdf = reactiveValues()
   #   mdf$test$n = 42 # on force mdf$test$n à changer pour actualiser le tableau des cartes en main
   #   print(paste("Vous avez demandé une nouvelle partie à",input$new_nb_joueurs))
      temp = f_init_jeu(mdf=mdf, nombre = input$new_nb_joueurs)
      mdf = temp
      mdf$test$n = 0
      mdf$j_actif = "J0"
      
      
      if(input$new_nb_joueurs == 1){
        mdf$params_joueur = data.frame(
          nom = "J1",
          nom = input$nom_J1,
          couleur = input$color_J1,
          NT = 20,
          statut = "actif"
        )
      }else if(input$new_nb_joueurs ==2){
        mdf$params_joueur = data.frame(
          nom = c("J1","J2"),
          nom_complet = c(input$nom_J1,input$nom_J2),
          couleur = c(input$color_J1, input$color_J2),
          NT = 20,
          statut = "actif"
        )
      }
      
      df(1)
      return(mdf)
    })
    
  })
}


# The notification module ----
notificationUI = function(id) {
  
  ns = NS(id)
  
  dropdownMenuOutput(ns('notifications'))
}
notificationServer = function(input, output, session) {
  notification_list = reactiveVal()
  output$notifications = renderMenu({
    validate(need(notification_list(), message = FALSE))
    dropdownMenu(type = 'notifications', badgeStatus = 'warning', .list = notification_list())
  })
  
  return(list(
    push_notification = function(message) {
      pf = parent.env(environment())
      pf$notification_list(c(pf$notification_list(), list(message)))
    },
    pop_notification = function() {
      pf = parent.env(environment())
      pf$notification_list(notification_list()[-length(pf$notification_list())])
    }
  ))
}
tab2_UI = function(id) {
  ns = NS(id)
  tagList(
    actionButton(ns('send_message'), 'Send a message'),
    actionButton(ns('remove_message'), 'Remove most recent message')
  )
}
tab2_Server = function(input, output, session, notificationModule) {
  observeEvent(input$send_message, {
    notifficationModule$push_notification(notificationItem(sprintf('Tab2: Pushed a notification at %s', Sys.time())))
  }) 
  observeEvent(input$remove_message, {
    notifficationModule$pop_notification()
  })
}




# Affichage des paramètres globaux ----
global_UI <- function(id){
  ns = NS(id)
  
  tagList(
    valueBoxOutput(ns("oxygBox"), width=3),
    valueBoxOutput(ns("tempBox"), width=3),
    valueBoxOutput(ns("oceanBox"), width=3),
    infoBoxOutput(ns("generationsBox"), width=3),
    infoBoxOutput(ns("j_actifBox"), width=3)
  )
}
global_Server <- function(id, mdf){
  moduleServer(id, function(input, output, session){
    
    output$generationsBox <- renderInfoBox({
      infoBox(
        value = paste0(mdf$nb_generations, "e"),
        title = "Génération",
        icon = icon(name="hourglass",lib="font-awesome"),
        color = "black",
        width = 2
      )
    })
    output$j_actifBox <- renderInfoBox({
      infoBox(
        value = (mdf$params_joueur %>% filter(nom==mdf$j_actif))$nom_complet,
        title = "Actif",
        subtitle = mdf$j_actif,
        icon = icon(name="user",lib="font-awesome"),
        color = (mdf$params_joueur %>%
                   filter(nom == mdf$j_actif) %>%
                   left_join(liste_couleurs,by=c("couleur"="hex")))$col,
        width = 1
      )
    })
    output$oxygBox <- renderValueBox({
      valueBox(
        value = paste0(mdf$global$oxyg, " % O2"),
        subtitle = "Oyxgène",
        icon = icon(f_switch_icon_name("oxyg")),#"atom"),
        color = "green"
      )
    })
    output$tempBox <- renderValueBox({
      valueBox(
        value = paste0(mdf$global$temp, " °C"),
        subtitle = "Température",
        
        icon = icon(name=f_switch_icon_name("temp"),lib="font-awesome"),
        color = "red",
        width = 1
      )
    })
    output$oceanBox <- renderValueBox({
      valueBox(
        value = paste0(mdf$global$ocean, ""),
        subtitle = "Océans",
        icon = icon(name=f_switch_icon_name("oc"),lib="font-awesome"),
        color = "blue",
        width = 1
      )
    })
  })
}


# Visualisation des données pour l'adversaire ---
adv_UI <- function(id){
  ns = NS(id)
  tagList(
    infoBoxOutput(ns("jBox"), width=3),
    tableOutput(ns("navire_adv"))
  )
}
adv_Server <- function(id, mdf){
  moduleServer(id, function(input, output, session){
    output$jBox <- renderInfoBox({
      infoBox(
        value = (mdf$params_joueur %>% filter(nom!=mdf$j_actif))$nom_complet,
        title = "Passif",
        subtitle = (mdf$params_joueur %>% filter(nom!=mdf$j_actif))$nom,
        icon = icon(name="user",lib="font-awesome"),
        color = (mdf$params_joueur %>%
                   filter(nom != mdf$j_actif) %>%
                   left_join(liste_couleurs,by=c("couleur"="hex")))$col,
        width = 1
      )
    })
    output$navire_adv <- function(){
      f_kable_navire(navire = mdf$navire[which(mdf$navire$joueur != mdf$j_actif),],
                     params_joueur = mdf$params_joueur)
    }
  })
}

# Visualisation des données ---
stats_UI <- function(id){
  ns = NS(id)
  tagList(
    infoBoxOutput(ns("NTBox")),
   fluidRow(
     box(title="Scores",
         status = "danger", solidHeader = TRUE,
         collapsible = TRUE,
          tableOutput(ns("points"))),
     ),
   fluidRow(
     #infoBoxOutput("approvalBox")
     box(title="Ressources sur les cartes",
         status = "primary", solidHeader = TRUE,
         collapsible = TRUE,
         tableOutput(ns("cartes_avec_ressources")))
   ),
 fluidRow(
     box(title="Badges en jeu",
         status = "primary", solidHeader = TRUE,
         collapsible = TRUE,
         tableOutput(ns("badges_joueurs")))
  )
  )
}

stats_Server <- function(id, mdf){
  moduleServer(id, function(input, output, session){

    player = reactive({
      switch(
      id,
      actif = mdf$j_actif,
      adv = (mdf$params_joueur %>% filter(nom != mdf$j_actif))[1]$nom
    )
    })
    mdf$PV = reactive({
      f_calculs_score(mdf=mdf)
    })
    
    output$NTBox <- renderInfoBox({
      infoBox(
        value = paste0(mdf$params_joueur[which(mdf$params_joueur$nom == mdf$j_actif),]$NT),
        title = "NT",
        icon = icon(name="trophy",lib="font-awesome"),
        color = (mdf$params_joueur %>%
                           filter(nom == mdf$j_actif) %>%
                           left_join(liste_couleurs,by=c("couleur"="hex")))$col,
        width = 2
      )
    })
    output$points <- renderTable({
      mdf$PV() %>%
        as.data.frame()
    })
    output$badges_joueurs = renderTable({
      mdf$badges_joueurs %>%
        as.data.frame() %>%
        filter(
         # joueur == player(),
          nombre > 0,
        ) %>%
        pivot_wider(names_from = joueur,
                    values_from = nombre)%>%
        replace(is.na(.), 0)%>%
        dplyr::mutate_if(is.numeric, round, digits=0)
    })
    output$cartes_avec_ressources = renderTable({
      mdf$cartes %>%
        filter(
          joueur == player(),
          sur_cette_carte_nombre > 0,
          )%>%
        select(c(nom,sur_cette_carte_type,sur_cette_carte_nombre#,
                # joueur
                 ))%>%
        rename(type = sur_cette_carte_type,
               nombre = sur_cette_carte_nombre)
     #   datatable(rownames = FALSE) #%>%
        # formatStyle(columns = 'sur_cette_carte_type',
        #             target="row",
        #             color = styleEqual(
        #               unique(mdf$cartes$sur_cette_carte_type)
        #             ),
        #             values = c("")
        #             )
      
    })
  })
}

# Boutons radios choix des actions ----
radio_UI <- function(id){
  ns = NS(id)
  tagList(
      uiOutput(ns("radio_actions")),#radioButtons(ns("radio_actions")),
      uiOutput(ns("radio_actions_validate"))
      )
}

radio_Server <- function(id, mdf,
                         id_radio = "obj", label_radio="Objectifs", data = "objectifs",
                         names_radio="obj_name", values_radio="obj"){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
  
    data_radio = reactive({ mdf[[data]] })
    
  #  print(paste("id_radio :",id_radio,", nrowdata_radio",nrow(data_radio)))
    
    # Affichage du bouton de validation de radio
    observe({#Event(mdf$j_actif,{
      # print(paste("data_radio()",id_radio))
      # print(data_radio())
      if(nrow(data_radio())>0){
        output$radio_actions_validate <- renderUI({
          actionButton(inputId = ns("validate"),
                       label = "Valider")
        })
      }
    })
    
    # Affichage du radio
    output$radio_actions = renderUI({
        f_radio(id=ns("my_radio"),
              label=label_radio,
              data = data_radio(),
              names = names_radio,
              values = values_radio)
    })
    
  
    r <- reactiveValues()
    observe({
      r$validate = input$validate
    })

    # Observateur du bouton de validation
    # et conséquences
    observeEvent(r$validate,{
      print(paste("id_radio VALIDATE",id_radio))
      tab = f_switch_tab(id_radio)
      numero = f_switch_id(id_radio)
      print(paste("type d'id :",numero))
      
      # print("tab pour switch tab :")
      # print(tab %>% head())
      print(paste("Input my_radio",input$my_radio))
      
      mdf = f_effets_immediats(choix = input$my_radio,
                               mdf = mdf,
                               session = session,
                               tab = tab,
                               numero=numero)
      # si l'action n'implique pas de tuile, on change de joueur
      if(mdf$tuile == ""){
        mdf$test$n %+=% 1
        print(paste(mdf$j_actif,"a fait une action. Nouveau test :",mdf$test$n))
      }#else{
     #   mdf$tuile_source = input$my_radio
     # }
      
    })
  })
}


# POP-UP PLATEAU si mdf$tuile (PAS D'UI)
modal_plateau_Server <- function(id, mdf){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    data_girafe <- reactiveVal()
    observe({#mdf$plat_girafe,{
      new = mdf$plat_girafe %>%
        filter(tuile == mdf$tuile)%>%
        filter(id_source == mdf$tuile_source)%>%
        left_join(bonus_placement, by=c("Lat","Long","Num"))%>%
        unite(., col = "clair",  nombre_bonus, type_bonus, unite_bonus, na.rm=TRUE, sep = " ")
      data_girafe(new)
    })
    
    
    # Boîte de dialogue qui s'ouvre quand mdf$tuile
    observeEvent(mdf$tuile,{
     if(nchar(mdf$tuile)>0){
       
       print(paste(
         #"nrow plat main : ",nrow(mdf$plat_en_main),
         "nrow plat main pour cette tuile : ",nrow(data_girafe()),
         "tuile_source :",mdf$tuile_source))
    
       
       print("ALERTE")
        shinyalert(
          title = "Plateau",
          size = "m",
          html = TRUE,
          text = tagList(
            girafeOutput(ns("modal_girafe"))
          )
        )
     }
    })
    
   

    # prépration du plateau interactif
    girafe_obj = reactiveVal()
    
    observe({
      new = geom_point_interactive(data = data_girafe(),
                                 aes(x=Long, y=Lat,data_id = Num,fill=Ocean),
                                 shape=21, size = 9,
                                 tooltip = data_girafe()$clair)
      girafe_obj(new)
    
    })


    # Rendu output pour boîte dialogue
    output$modal_girafe <- renderGirafe({
      x= girafe(ggobj = (mdf$plot() + girafe_obj()),
                options = list(
                   opts_tooltip(css = "background-color:black;"),
                   opts_sizing(width = .7) )
                )
      
      # output$plateauPlot <- renderGirafe({
      #   x = girafe(ggobj = mdf$plot,
      #              options = list(
      #                opts_tooltip(css = "background-color:black;"),
      #                opts_sizing(width = .7) ) )
        if(mdf$tuile_nb > 1){
          x <- girafe_options(x, opts_selection(type = "multiple"))
        }else{
          x <- girafe_options(x, opts_selection(type = "single"))
        }
      # })
    })
    
    # Conséquences 
    observeEvent(input$shinyalert,{
      print(paste("input modal girafe",input$modal_girafe_selected))

       mdf = f_pose_tuile(mdf = mdf, selection = input$modal_girafe_selected)
     # # if(mdf$plateau[which(mdf$plateau$Num == input$modal_girafe_selected),]$Libre == TRUE){

        # et on sort du stand_by
        mdf$tuile = ""
        mdf$tuile_source = ""
        mdf$tuile_nb = 0
        
        # et on change de joueur
        mdf$test$n %+=% 1
      #}
    })
    return(mdf)
    
    
  })
}

# Alternance des tours et des générations
tours_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("terminer"), "Terminer", icon = icon("stop")),
    uiOutput(ns("passer_UI"))
  )
  
}
tours_Server <- function(id, mdf){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Conséquences appuyer sur "passer"
    observeEvent(input$passer,{
      print(paste(mdf$j_actif,"passe."))
      mdf$test$n = 2
      print(paste("test : ",mdf$test$n))
    })

    # Conséquences appuyer sur "terminer"
    observeEvent(input$terminer,{
      mdf$params_joueur[which(mdf$params_joueur$nom==mdf$j_actif),]$statut = "inactif"
      print(paste(mdf$j_actif,"a passé son tour. La génération continue sans lui."))
      mdf$test$n = 2
    })
    
    # Vérification fin de partie (générations)
    observeEvent(mdf$nb_generations,{
      mdf$statut = f_verif_fin(statut = mdf$statut, global = mdf$global)
      if(mdf$statut == "fin_partie"){
        showNotification("Terraformation réussie !")
        mdf$statut = "fin"
        

      }
      
    })

    # Alternance mdf$statut = "actif"/"choix_cartes" et reboot de début de tour
    observeEvent(mdf$params_joueur,{
      # si tous les joueurs sont dans le même état inactif (suite à "passer son tour")
      if(length(unique(mdf$params_joueur$statut))==1 &
         mdf$params_joueur$statut[1]=="inactif" &
         mdf$statut=="actif"){
        
        print("Tous les joueurs sont inactifs. On passe au choix des cartes de la génération suivante.")
        # alors on met en place la génération suivante
        # avec mise à jour des navires (tous)
        navire_temp = f_maj_navire(navire=mdf$navire,
                                   params_joueur = mdf$params_joueur)
        mdf$navire = navire_temp
        
        ## Piocher 4 cartes payantes/joueur
        mdf$cartes = f_debut(cartes=mdf$cartes,
                              navire= mdf$navire,
                              nb_cartes=4,
                              gratuit=FALSE)
        print(paste("Cartes T :",nrow(mdf$cartes %>%
                                        filter(where=="T"))))
        rm(navire_temp)
        
        mdf$nb_generations %+=% 1
        mdf$params_joueur$statut = "actif"
        mdf$statut = "choix_cartes"
        # remise à 0 de test + nb générations pour commencer une fois sur 2
        mdf$test$n = 0 
        
        
        #   #si tous les joueurs sont dans le même statut de "choix_cartes" = TRUE (ils ont choisi leurs cartes)
        #   # et qu'on est dans la phase de choix_cartes
      }else if(length(unique(mdf$params_joueur$statut))==1 &
               mdf$params_joueur$statut[1]=="inactif" &
               mdf$statut=="choix_cartes"){
        # alors on passe à la phase active et tous les joueurs redeviennent actifs pour cette génération
        print("Tous les joueurs ont leurs nouvelles cartes pour cette nouvelle génération. Retour à la phase active.")
        mdf$statut = "actif"
        mdf$params_joueur$statut = "actif"
        
      }
    })
    
  #  alert <- reactiveValues(response = FALSE)
  
    # Boîte de dialogue qui s'ouvre statut = choix_cartes
    # + si test = 1, on peut "passer"
    observeEvent(mdf$test$n,{
      if(mdf$statut == "choix_cartes"){ #& nrow(mdf$params_joueur[which(mdf$params_joueur$actif == "actif"),])>0){
         shinyalert(
          title = paste("Nouvelles cartes"),
          # callbackR = function(x) {
          #   alert$response <- x
          # },
          size = "l",
          html = TRUE,
          text = tagList(#div(style = "overflow: auto; max-height: 50vh;",
                    
                     DTOutput(ns("choix_cartes"),height=600)
                     )
        )
      }
      if(mdf$test$n == 1){
       output$passer_UI <- renderUI({
         actionButton(ns("passer"), "Passer pour l'instant", icon = icon("pause"))
       })
      }
    })
    # tableau de cartes achetables =f(j_actif)
    choix_cartes_prepa = eventReactive(mdf$j_actif,{
      mdf$cartes %>%
        filter(where=="T",
               joueur == mdf$j_actif)%>%
        f_colonne_en_plus() %>%
        select(-c(sur_cette_carte_nombre,sur_cette_carte_type,
                  where,joueur,check,ok))
    })
    output$choix_cartes <- renderDT({
      choix_cartes_prepa() %>%
        datatable(
          caption = paste("Nouvelles cartes pour",mdf$actif,mdf$params_joueur[mdf$params_joueur$nom==mdf$j_actif,]$nom_complet),
          rownames = FALSE,
          selection = list(target = 'row')
        )%>%
        formatStyle(columns = 'type_carte',
                    target="row",
                    color = styleEqual(
                      c(21,22,1,3),
                      c('#00ccff', '#99ccff', '#99ff99', '#ff9999')
                    ))
    })
    
    
    nouvelles_cartes <- reactiveVal(
      isolate(mdf$cartes) %>% filter(id==0)
    )
    
    observeEvent(input$shinyalert,{
      req(input$choix_cartes_rows_selected)
      new = choix_cartes_prepa()[input$choix_cartes_rows_selected,]
      nouvelles_cartes(new)
    })
    
  # Conséqunces alerte : Achat des cartes
  observeEvent(input$shinyalert,{
 #   nouvelles_cartes = choix_cartes_prepa()[input$choix_cartes_rows_selected,]
    print(paste("nrow nouvelles_cartes",nrow(nouvelles_cartes())))
    
 #   if(nrow(nouvelles_cartes())>0){
      mdf$navire = f_achat_cartes(navire=mdf$navire,
                                  nb_cartes=nrow(nouvelles_cartes()),
                                  player=mdf$j_actif)
      
      # f_new_cartes_en_main permet de prendre en main les cartes sélectionnées (radiobutton input$nouvelles_cartes)
      # et de défausser celles qui n'ont pas été retenues (ni achetées)
      mdf$cartes = f_new_cartes_en_main(cartes=mdf$cartes,
                                        player=mdf$j_actif,
                                        new_cartes=as.list(nouvelles_cartes()$id),
                                        choix_cartes=mdf$cartes[which(mdf$cartes$joueur==mdf$j_actif &
                                                                        mdf$cartes$where =="T"),])
   # }
    print(paste(mdf$j_actif,"a pioché",nrow(nouvelles_cartes()),"nouvelles cartes."))
    print(paste("Nombre de cartes T restantes :",nrow(mdf$cartes %>% filter(where=="T"))))
    mdf$params_joueur[which(mdf$params_joueur$nom == mdf$j_actif),]$statut = "inactif" # il devient inactif
    
    mdf$test$n = 2
  })
  
    
  
    # Observateur pour changer de joueur actif
    observeEvent(mdf$test$n, priority=1,{
      
      # mdf$test$n %+=% 1
      # print(paste("testt :",mdf$test$n))
      
      if(mdf$test$n == 2){
        temp = mdf$params_joueur[which(mdf$params_joueur$statut=="actif"),]$nom # liste des joueurs encore actifs
        temp2 = temp[which(temp!= mdf$j_actif)]
        
        mdf$test$n = 0
        print(paste0(#"testt = ",mdf$test$n,
          "statut général = ", mdf$statut," generation = ",mdf$nb_generations))#," stand_by =",mdf$stand_by))
        print("Liste des joueurs encore actifs : ")
        print(temp)
        
        if(length(temp2)==1){
          mdf$j_actif = temp2
          #mdf$j_actif = f_cherche_wrap(liste=as.list(temp2), numero=mdf$test$n)
          
          # # définit le j_actif
          # mdf$j_actif = f_cherche_wrap(liste=temp,numero=mdf$test$n)
        print("*-______-*-______-*-______-*-______-*-______-")
        print(paste0("Joueur actif : ",mdf$j_actif))
        mdf$tuile=""
        # updateRadioButtons(session, "move", selected = "pss")
        # mdf$bouton = h6("")
        rm(temp)
        }else{print("Tant va la cruche à l'eau")}
        
      }
    })

  })
}

# DEBUG ----
debug_UI <- function(id){
  ns = NS(id)
  tagList(
    textOutput(ns("variables")),
    tableOutput(ns("cartes_cartes_cartes"))
  )
}
debug_Server <- function(id, mdf){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$cartes_cartes_cartes <- renderTable({
      mdf$cartes %>%
        filter(where != "P")%>%
        select(-c(sur_cette_carte_nombre,sur_cette_carte_type))
    })
    
    output$variables <- renderText({
      paste("statut :",mdf$statut,
            ", générations :",mdf$nb_generations,
            ", tuile : ",mdf$tuile,
            ", tuile_source : ",mdf$tuile_source,
            ", test :",mdf$test$n,
            ", typeof(cartes$where)",typeof(mdf$cartes$where),
            ", nombre where==J",nrow(mdf$cartes %>% filter(where=="J"))
            )
    })
  })
}

# calculs des variables en temps réel
# pour le choix des actions
calculs_pour_affich_Server <- function(id, mdf){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observe({
      mdf$objectifs_persos = f_verif_objectifs(
        objectifs = mdf$objectifs,
        params_joueur = mdf$params_joueur,
        plateau = mdf$plateau,
        cartes = mdf$cartes,
        badges_joueurs = mdf$badges_joueurs
        )
    })
    
    observe({
      mdf$actions21_pour_affich = mdf$cartes %>%
        filter(joueur == mdf$j_actif,
               type_carte==21,
               where =="J"
        ) %>%
    #    a = cartes %>%
        left_join(actions21,by="id")%>%
        f_check_actions21(mdf=mdf)%>%
        select(id,id_idaction,nom,ok)%>%
        left_join(
          sum_actions_icone_id_idaction %>%
          mutate(icone_a = as.list(icone_a))
        )%>%
        distinct()%>%
        rename(action = icone_a)
        #group_by(id_idaction)%>%
        View(mdf$actions21_pour_affich)
        #summarise_all(funs(toString(unique(.[.!=0]))))
      
      #print(paste("nrow actions21_pour_affich",nrow(mdf$actions21_pour_affich)))
    })
    
    observe({
      mdf$pj_std = projets_std %>%
        filter(
          cost <= (mdf$navire %>%
                     filter(joueur == mdf$j_actif,
                            nom == "Argent")
                   )$stock)%>%
        select(c(id,std_name))%>%
        group_by(id) %>%
        slice(1)

    })
    
    return(mdf)
  })
}


# affichage des 8pl/8ch
convertir_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("convertir"))
  )
}
convertir_Server <- function(id, mdf, param){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ressource = reactive({
      switch(param,
           plantes = "Plantes",
           chaleur = "Chaleur")
    })
    label = reactive({
      switch(param,
           plantes = "8 plantes en forêt",
           chaleur = "8 chaleur en température")
    })
    icone = reactive({
      switch(param,
             plantes = icon("pagelines"),
             chaleur = icon("thermometer-half"))
    })

    bouton <- reactiveVal(h6(""))

    # dès que le joueur actif peut convertir, on affiche le bouton
    observeEvent((mdf$navire %>% filter(joueur==mdf$j_actif)),{
      if((mdf$navire %>%
          filter(joueur == mdf$j_actif,
                 nom == ressource())
          )$stock > 7){

        bouton(actionButton(ns("convertir_button"),
                            label = label(),
                            icon = icone()))
      }else{
        bouton(h6(""))
      }
    })
    output$convertir <- renderUI({
       bouton()
     # h6("")
    })
    
    observeEvent(input$convertir_button,{
      mdf$navire[which(mdf$navire$joueur==mdf$j_actif & mdf$navire$nom==ressource()),]$stock %-=% 8 
      if(ressource()=="Plantes"){
        mdf$tuile="fo"
        mdf$tuile_source="fo"
    
      }else if(ressource()=="Chaleur"){

        mdf$global$temp %+=% 1
        mdf$params_joueur[which(mdf$params_joueur$nom==mdf$j_actif),]$NT %+=% 1
        mdf$test$n %+=% 1
      }
    })
  })
}


navire_UI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("navi"))

  )
}
navire_Server <- function(id, mdf){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    navi <- reactiveVal()
    
    observe({
      new = mdf$navire %>%
        filter(joueur==mdf$j_actif)%>%
        ungroup()%>%
        select(-joueur)%>%
        left_join(icon_data_fa, by = "nom")%>%
        mutate(icones = as.list(icones))%>%
        relocate(icones) %>%
        mutate(flux = paste0("+",flux)) %>%
        select(c(icones,stock,flux))%>%
        as.data.frame()
    navi(new)
    })


    output$navi <- renderDT({
        datatable(
          data = navi(),
          rownames = FALSE,
          colnames = c("", "", ""),
          #width = 100,
          # filter ="none",
           selection="none",
           options = list(
             dom = "t",
              search.smart = FALSE,
             # stripeClasses = FALSE,
             ordering=F,
             autoWidth = FALSE,
             scrollX = TRUE,
             columnDefs = list(
               list(width = '20px', targets = "_all"),
               list(className = "dt-center", targets = "_all")
             )
           )
        )
                         #autoWidth = TRUE,
                         # scrollX=TRUE,
                         # columnDefs = list(list(width = '10px', targets = "_all"))),
       #   rownames = FALSE,
          #width = 100,
          # filter ="none",
          # selection="none",
          # escape = FALSE)
        
    })
    
  })
}


# Cartes posées
cartes_devant_soi_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
    textOutput(ns("evenements_devant")),
    box(title="Cartes posées",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width=12,
        DTOutput(ns("cartes_devant")))
  )
  )
}
cartes_devant_soi_Server <- function(id, mdf){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # mdf$cartes_devant <- reactiveVal({#eventReactive(input$crt_poser_validate,{
    #   mdf$cartes %>%
    #     filter(joueur == mdf$j_actif,
    #            where == "J")%>%
    #     as.data.frame()%>%
    #     select(-c(sur_cette_carte_nombre,sur_cette_carte_type))
    # })
    mdf$cartes_devant <- reactiveVal()
    observe({
      new = mdf$cartes %>%
        filter(joueur == mdf$j_actif,
               where == "J")%>%
        as.data.frame()%>%
        select(-c(sur_cette_carte_nombre,sur_cette_carte_type))
      
      mdf$cartes_devant(new)
    })
    output$cartes_devant = renderDT({
      
      if(nrow(mdf$cartes_devant())>0){
        mdf$cartes_devant() %>%
          datatable(caption = "Cartes déjà posées",
                    rownames = FALSE,
                    selection = list(mode = 'none'))%>%
          formatStyle(columns = 'type_carte',
                      target="row",
                      color = styleEqual(
                        c(21,22,1,3),
                        c('#00ccff', '#99ccff', '#99ff99', '#ff9999')
                      ))
      }
    })
    
    mdf$nb_evenement <- reactiveVal()
    observe({
      new = mdf$cartes %>%
        filter(joueur == mdf$j_actif,
               where == "J",
               type_carte == 3)%>%
        nrow()
      mdf$nb_evenement(new)
    })
    output$evenements_devant <- renderText({
      paste(mdf$j_actif,"a joué",as.character(mdf$nb_evenement()),"évènements depuis le début de la partie.")
    })
  })
}


test_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("test2")),
    DTOutput(ns("test3"))
  )
}

test_Server <- function(id, mdf){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    a = effet_cartes %>%
      f_icone() %>%
      mutate(icone = as.list(icone))
    
    b = cartes %>%
      f_colonne_en_plus()%>%
      select(-c(sur_cette_carte_nombre,
                sur_cette_carte_type,
                ok, where, check, type_carte, joueur))
    
    output$test3 <- renderDT({
      b %>% datatable(escape=FALSE)
    })
    
    output$test2 <- renderUI({
      tagList(

        actionButton(inputId = "ClickonMe5", label = div(
          tags$span(f_switch_icon("Argent"),
                    f_switch_icon("Plantes"),
                    f_switch_icon("Chaleur")))),
        
        actionButton("sgh",label = div(
          tags$span(class = "fa-stack",
                    icon("square", class = "fa-stack-2x", style = "color: rgb(102, 51, 0)"),
                    icon(f_switch_icon_name("Argent"),class = "fa-stack-1x", style = "color: rgb(255, 204, 0)")
          )
          
        ))
      )
    })
      
  })
}

# POPUP = PAS d'UI
popup_Server <- function(id, mdf){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Boîte de dialogue qui s'ouvre quand mdf$tuile
    observeEvent(mdf$sur_carte,{
      if(nrow(mdf$sur_carte)>0){

        shinyalert(
          title = "Gain d'une ressource",
          size = "s",
          html = TRUE,
          text = tagList(
            DTOutput(ns("modal"))
          )
        )
      }
    })
    
    output$modal <- renderDT({
      mdf$sur_carte %>%
        datatable(caption = "Choisissez une carte :",
                  rownames = FALSE,
                  selection = list(mode = 'single',
                                   target = 'row')
        )
    })
    
    observeEvent(input$shinyalert,{
      req(input$modal_rows_selected)
      carte = mdf$sur_carte[input$modal_row_last_clicked,]
      print(carte)
        
    })
})
}