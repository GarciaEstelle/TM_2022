
## MAIN UI ----

#Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);")

ui <- dashboardPage(
  
  # TITRE #
  dashboardHeader(title ="Terraforming Mars"),
                  #notificationUI('notificationUI')),
  # SIDEBAR # ----
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Jeu", tabName = "jeu",
               icon = icon("tachometer-alt")),
      menuItem("Actions standards", tabName = "std",
               icon = icon("coins",lib = "font-awesome")),
      #badgeLabel = "",
      #badgeColor = "blue"),
      navire_UI("nav"),
   #   tableOutput("navire"),
      convertir_UI("plantes"),
      convertir_UI("chaleur"),
      tours_UI("ledger"),
      menuItem("Adversaire", tabName = "adversaire",
               icon = icon("users"),
               badgeLabel = "état",
               badgeColor = "black"),
      
      menuItem("Plateau",tabName="plateau",
               icon =icon("globe")),
      menuItem("Stats",tabName = "stats",
               icon = icon("info")),
      
      br(),
      br(),
      menuItem("Nouvelle partie",  icon = icon("redo"),
               tabName = "nouvelle_partie",
               badgeLabel = "new", badgeColor = "purple"),
      
      menuItem("Debug", tabName="debug")
      
    )
  ),
  # BODY # ----
  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    # modal_plateau_UI("modal"),
    tabItems(
      ##### tabItem JEU #####
      tabItem(tabName = "jeu",
              
              h2("Tableau de bord"),
              #    tab2UI('tab2UI'),
              
          #    test_UI("test"),
              fluidRow(global_UI("global")),
              cartes_UI("gestion_cartes")
      ),
      
      ##### tabItem Actions standards #####
      tabItem(tabName = "std",
              h2("Actions"),
              fluidRow(
                column(width = 3,
                       h5("Projets standards"),
                       radio_UI("pj_std")),
                
                column(width = 3,
                       h5("Objectifs"),
                       radio_UI("objectifs"))
              ),
              fluidRow(
                column(width = 6,
                       h5("Actions de cartes déjà posées"),
                       actions21_UI("actions21")
                       #radio_UI("act21")
                )
              )
      ),
      ##### tabItem ADVERSAIRE #####
      tabItem(tabName = "adversaire",
              h2("Adversaire"),
              adv_UI("adversaire")#,
              #  stats_UI("adv")
      ),
      tabItem(tabName ="plateau",
              h2("Plateau"),
              plateau_UI("plateau")),
      
      ### tabItem Statistiques ####
      tabItem(tabName = "stats",
              stats_UI("actif"),
              cartes_devant_soi_UI("devant")),
      
      ##### tabItem Nouvelle PARTIE #####
      tabItem(tabName = "nouvelle_partie",
              h2("Nouvelle partie"),
              nouvellePartie_UI("new")
      ),
      ##### tabItem Debug #####
      tabItem(tabName = "debug",
              h2("Debug"),
              debug_UI("debug")
      )
      
    )
  )
)

