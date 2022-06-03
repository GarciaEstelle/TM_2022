

# Server ----

server <- function(input, output, session){
  #  notificationModule = callModule(notificationServer, 'notificationUI')
  # callModule(tab2Server, 'tab2UI', notificationModule)
  # df=reactiveVal()
  # mdf = nouvellePartiePopup_Server("new_pop",df=df)#, params)
  # 
  # # observeEvent(df(),{
  # 
  nombre=2
  mdf = reactiveValues()
  mdf = f_init_jeu(mdf = mdf, nombre=nombre)
#
  nouvellePartie_Server("new",mdf=mdf,parent = session)#, params)

  debug_Server("debug",mdf=mdf)
  calculs_pour_affich_Server("calculs",mdf=mdf)

  cartes_Server("gestion_cartes",mdf)
  cartes_devant_soi_Server("devant",mdf)
  global_Server("global",mdf)
  plateau_Server("plateau",mdf)
  radio_Server("objectifs", mdf,
               id_radio = "obj",
               label_radio = "Objectifs (8 crédits) : ",
               data = "objectifs_persos",
               names_radio = "obj_name",
               values_radio = "obj")

  # radio_Server("act21", mdf,
  #              id_radio = "action21",
  #              label_radio = "Actions disponibles (1 fois par génération) : ",
  #              data = "actions21_pour_affich",
  #              names_radio = "en_clair",
  #              values_radio = "id_idaction")

  radio_Server("pj_std", mdf,
               id_radio = "std",
               label_radio = "",
               data = "pj_std",
               names_radio = "std_name",
               values_radio = "id")
  #

  adv_Server("adversaire",mdf=mdf)

  tours_Server("ledger", mdf=mdf)
  modal_plateau_Server("modal",mdf=mdf)

  stats_Server("actif",mdf=mdf)
  # stats_Server("adv",mdf=mdf)
# 
#   output$navire <- function(){
#     f_kable_navire(navire = mdf$navire[which(mdf$navire$joueur == mdf$j_actif),],
#                    params_joueur = mdf$params_joueur)
#   }
navire_Server("nav", mdf = mdf)

  convertir_Server("plantes",mdf=mdf,param="plantes")
  convertir_Server("chaleur",mdf=mdf,param="chaleur")
  # })
  
  popup_Server("sur_carte", mdf = mdf)
  actions21_Server("actions21", mdf = mdf)
 # test_Server("test")
}

