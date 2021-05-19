#'/////////////////////////////////////////////////////////////////////////////
#' FILE: "MTDP Rio Tupiza v.3"
#' AUTHOR: Angelica Moncada, SEI Latinoamerica
#' CREATED: 2021
#' MODIFIED: 
#' STATUS: working
#' PURPOSE: Proyecto Bolivia WATCH
#' COMMENTS: click `Run App` to view the app in the viewer pane
#'/////////////////////////////////////////////////////////////////////////////
#'
#'
options(shiny.reactlog=TRUE) 
options(show.error.messages = TRUE)
cat("\014")  
rm(list=ls())

###############
###############
###############
library(sf)
library(shinydashboard)
library(dplyr)
library(shiny)
library(reshape)
library(plotly)
library(DT)
library(ggplot2)
library(lubridate)
library(leaflet)
library(leaflet.minicharts)
library(data.table)
library(shinyjs)
library(rgdal)
library(stringr)
library(raster)
library(shinyWidgets)
library(tidyr)
library(shinyhelper)
library(htmltools)

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#datos
######################################
######################################

#Escenarios
a1=c("Reference", "Reference1", "Reference2", "Reference3")
#Nombres Escenarios
a2=c("Pob 2021 Clima Hist","Pob 2022 Clima Hist","Pob 2021 Clima CC", "Pob 2022 Clima CC")
#a2=c("Pob 2021 Clima Hist","Pob 2022 Clima Cambio Climatico","Pob 2021 Clima Cambio Climatico", "Pob 2022 Clima Cambio Climatico")

#WASHfiles="Estrategias/Indicadores/WASH/"
#REVAMPfiles="Estrategias/Indicadores/REVAMP/"
#WaTTfiles="Estrategias/Indicadores/WaTT/"
#ScriptIndicadoresFiles=""
#FolderAcciones="Estrategias/Acciones/"
#FolderInventario="Estrategias/"

WASHfiles=""
REVAMPfiles=""
WaTTfiles=""
ScriptIndicadoresFiles=""
FolderAcciones=""
FolderInventario=""

#InvAcciones = read.csv(paste0("Inventario de acciones.csv"), check.names = F, stringsAsFactors = F,header = T, encoding="UTF-8-BOM")
#colnames(InvAcciones)[1] = gsub('^...','',colnames(InvAcciones)[1])
#InvIndicadores = read.csv(paste0("Inventario de indicadores.csv"), check.names = F, stringsAsFactors = F,header = T, encoding="UTF-8-BOM")
#colnames(InvIndicadores)[1] = gsub('^...','',colnames(InvIndicadores)[1])
InvAcciones = read.csv(paste0(FolderInventario,"Inventario de acciones.csv"), check.names = F, stringsAsFactors = F,header = T)
InvIndicadores = read.csv(paste0(FolderInventario,"Inventario de indicadores.csv"), check.names = F, stringsAsFactors = F,header = T)

######################################
######################################

TabsAcciones=c(unique(InvAcciones$Categoria),"Resumen y Correr")
imagenAcc=unique(InvAcciones[,c("Categoria", "Imagen")])

data=unique(InvAcciones[,c("Categoria","Accion","Descripcion Largo","Tipo","Key assumption")])
data$N=1:nrow(data)
data$Opcion=""

TabsIndicadores=unique(InvIndicadores$`Tema_Central`)
imagen=subset(InvIndicadores,InvIndicadores$Imagen!="NO")[,c("Nombre")]
mapa=subset(InvIndicadores,InvIndicadores$Visualizacion_Mapa!="NO")[,c("Nombre")]
mapaWASH=subset(InvIndicadores,InvIndicadores$Visualizacion_MapaWASH!="NO")[,c("Nombre")]
grafico=subset(InvIndicadores,InvIndicadores$Visualizacion_Grafico!="NO")[,c("Nombre")]
tabla=subset(InvIndicadores,InvIndicadores$Visualizacion_Tabla!="NO")[,c("Nombre")]
compuesto=subset(InvIndicadores,InvIndicadores$Visualizacion_Compuesta!="NO")[,c("Nombre")]


#reiniciar MTDP 
#fn = paste0(FolderAcciones,c("Estrategia_cuenca.csv","Estrategia_cuenca.txt","Lista acciones.csv"))
#for (i in 1:length(fn)) {if (file.exists(fn[i])) {file.remove(fn[i])}}

acciones=data.frame(matrix(NA,1,nrow(data)+1))
colnames(acciones)=c("N_Corrida",data$`Accion`)
acciones[1,]=colnames(acciones)
#if (file.exists(paste0(FolderAcciones,"Lista acciones.csv"))==FALSE) {
#  write.table(na.exclude(acciones),paste0(FolderAcciones,"Lista acciones.csv"),row.names=F, col.names=T ,sep=",")
#}

runsScript=unique(substring(list.files(pattern = "_Indicadores.csv$"), 1,1))
#runsScript=1

imagenblanca=paste0("blanco.png")
#funciones
######## 
# show handler
show_loading = function(elem) {
  session = shiny::getDefaultReactiveDomain()
  session$sendCustomMessage("show_loading", elem)
}
# hide handler
hide_loading = function(elem) {
  session = shiny::getDefaultReactiveDomain()
  session$sendCustomMessage("hide_loading", elem)
}
Previous_Button=tags$div(actionButton("Prev_Tab",HTML('<div class="col-sm-4"> <strong><font color=\"mediumseagreen\">Anterior </font></strong>  <i class="fa fa-arrow-circle-left fa-2x" style="color:mediumseagreen"></i></div>                                                               ')))
Next_Button=div(actionButton("Next_Tab",HTML('<div class="col-sm-4"> <strong><font color=\"mediumseagreen\">Siguiente </font></strong> <i class="fa fa-arrow-circle-right fa-2x" style="color:mediumseagreen"></i></div>')))
########

#UI
########
ui = dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "MTDP Rio Tupiza v.2"),
  
  #https://getbootstrap.com/docs/3.4/components/#glyphicons
  #https://fontawesome.com/icons?d=gallery&p=2&s=solid&m=free
  #https://fontawesome.com/v4.7.0/icons/
  #https://www.w3schools.com/icons/fontawesome5_icons_audio_video.asp
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "Inicio", icon = icon("home")),   
      menuItem("MTDP", tabName = "MTDP", icon = icon("road")),
      menuItem("Indicadores", tabName = "Indicadores", icon = icon("list-alt")),
      menuItem("Mapas", tabName = "Mapas", icon = icon("globe"))
    )),
  
  dashboardBody(
    tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
    tabItems(
      #inicio
      #######################
      tabItem("Inicio",
              fluidPage(
                wellPanel(style = "background: white",
                          #
                          wellPanel(style = "background: white",
                                    h1("Modelo de toma de decisiones Cuenca Rio Tupiza (MTDP Rio Tupiza)", style = "color:mediumseagreen", align = "center"),
                                    h3("Version 2.0, Abril 2021", align = "center"),
                                    fluidRow(
                                      column(3,
                                             img(src="logo1.png", align = "center",width='100%'), #height='250px',width='500px'
                                      ),
                                      column(3,
                                             img(src="logo2.png", align = "center",width='100%'), #height='250px',width='500px'
                                      ),
                                      column(3,
                                             img(src="logo3.png", align = "center",width='100%'), #height='250px',width='500px'
                                      ),
                                      column(3,
                                             img(src="logo4.png", align = "center",width='100%'), #height='250px',width='500px'
                                      ))
                          ),
                         h4(p(br(""), #texto explicando que es el MTDP 
                               br("se pueden incluir tambien las intrucciones"),
                               hr(),
                               em(strong(br("1. "))),
                               br(img(src="01.png")),
                               #br("explicacion"),
                               hr(),
                               em(strong(br("2. "))),
                               br(img(src="022.png")),
                               #br("explicacion"),
                               hr(),
                               em(strong(br("3. "))),
                               br(img(src="03.png")),
                               #br("explicacion"),
                               hr(),
                               em(strong(br("4. "))),
                               br(img(src="04.png")),
                               #br("explicacion"),
                               hr(),
                               em(strong(br("5. "))),
                               br(img(src="05.png")),
                               #br("explicacion"),
                               hr(),
                               em(strong(br("6. "))),
                               br(img(src="06.png")),
                               #br("explicacion"),
                               hr(),
                               em(strong(br("7. "))),
                               br(img(src="07.png")),
                               #br("explicacion"),
                               hr(),
                               em(strong(br("8. "))),
                               br(img(src="08.png")),
                               #br("explicacion"),
                               hr()
                          )
                          ))
              )),
      #######################
      #MTDP
      #######################
      tabItem("MTDP",
         fluidPage(
           tags$style(HTML("
            .tabbable > .nav > li[class=active]    > a {font-weight: bold;background-color: #6694F6; color:white}
          ")),
           useShinyjs(),
          wellPanel(style = "background: white",
            #aqui la parte superior
            hr(),
            wellPanel(style = "background: white",
              fluidRow(
                h4(strong("Cada set de acciones se modelara para los escenarios: ", style="color:mediumseagreen")),
                column(3,
                       selectInput(inputId = paste0("DPoblacion1_Out"),
                                   label = "Escenario 1",
                                   choices=a2[1]
                       )
                       ),
                column(3,
                       selectInput(inputId = paste0("DPoblacion2_Out"),
                                   label = "Escenario 2",
                                   choices=a2[2]
                       )
                ),
                column(3,
                       selectInput(inputId = paste0("DPoblacion3_Out"),
                                   label = "Escenario 3",
                                   choices=a2[3]
                       )
                ),
                column(3,
                       selectInput(inputId = paste0("DPoblacion4_Out"),
                                   label = "Escenario 4",
                                   choices=a2[4]
                       )
                )
              ),
              h4(strong("seleccione la opcion deseada en cada accion. Por favor verifique una a una.", style="color:mediumseagreen")),
              br(),
              fluidRow(
                uiOutput("Next_Previous"),
              ),
              do.call(tabsetPanel, c(id='tabBox_next_previous',lapply(1:length(TabsAcciones), function(i) {
                tabPanel(
                  title=TabsAcciones[i], 
                  wellPanel(style = "background: white",
                            imageOutput(paste0("imagenA",TabsAcciones[i]),width = "30%",height = "auto"),
                            br(),
                            uiOutput(paste0("ID_A_",TabsAcciones[i])),
                            div(DT::dataTableOutput(paste0("tablaA",TabsAcciones[i])), style = "font-size: 100%; width: 100%"),
                            br(),
                            h4(strong(htmlOutput(paste0("Text_A",TabsAcciones[i])))),
                            br(),
                            div(actionButton(paste0("CorrerWEAP",TabsAcciones[i]),HTML('<div class="col-sm-4"> <i class="fa fa-play-circle fa-2x" style="color:mediumseagreen"></i> <strong><font color=\"mediumseagreen\">Simular conjunto de acciones </font></strong> </div>'))),
                            br(),
                            fluidRow(
                              column(4,
                                     h4(strong(htmlOutput(paste0("Text_Arun",TabsAcciones[i])))),
                              ),
                              column(8,
                                     infoBoxOutput(paste0("infobox",TabsAcciones[i]), width = 8)
                                     )
                            )
                            #tags$head(tags$style(paste0("#",paste0("Text_Arun",TabsAcciones[i]),"{color: red;font-size: 20px}"))) #;font-style: italic;
                            
                  ),
                  tags$script("
                       $('body').mouseover(function() {
                       list_tabs=[];
                       $('#tabBox_next_previous li a').each(function(){
                       list_tabs.push($(this).html())
                       });
                       Shiny.onInputChange('List_of_tab', list_tabs);})
                       "
                  ))
              })))
            )
          ))),
      #######################
      #indicadores
      #######################
      tabItem("Indicadores",
              tags$style(HTML("
                  .tabbable > .nav > li[class=active]    > a {font-weight: bold;background-color: #6694F6; color:white}
                ")),
              fluidPage(
                useShinyjs(),
                wellPanel(style = "background: white",
                          # aqui la parte superior
                wellPanel(style = "background: white",
                          fluidPage(
                            fluidRow(
                              column(2,
                                     actionButton("ActualizarRuns", "Actualizar corridas")
                              ),
                              column(10,
                                     selectInput("Nrun", "Seleccione el numero de corrida", runsScript,runsScript[length(runsScript)])
                                     )
                            ),
                            actionButton("test", label = "Esconder/Mostrar Tabla resumen de acciones segun corrida seleccionada"),
                            wellPanel(id="panel",style = "background: white",
                                      DT::dataTableOutput("TableRun")
                            ),   
                            hr(),
                           wellPanel(style = "background: white",
                                      do.call(tabsetPanel, c(id='tab',lapply(1:length(TabsIndicadores), function(i) {
                                        TabsIndicadores1=unique(subset(InvIndicadores,InvIndicadores$`Tema_Central`==TabsIndicadores[i])[,"Variable"])
                                        tabPanel(
                                          title=TabsIndicadores[i], 
                                          do.call(tabsetPanel, c(id='tab1',lapply(1:length(TabsIndicadores1), function(j) {
                                            tabPanel(
                                              title=TabsIndicadores1[j], 
                                              fluidPage(
                                                hr(),
                                                fluidRow(
                                                  wellPanel(style = "background: white",
                                                            radioButtons(
                                                              inputId=paste0("out_",TabsIndicadores[i],"_",TabsIndicadores1[j]),
                                                              label="Opciones",
                                                              choices = subset(InvIndicadores, InvIndicadores$Variable==TabsIndicadores1[j] & InvIndicadores$Tema_Central==TabsIndicadores[i])[,"Nombre"] #
                                                            )),
                                                  wellPanel(style = "background: white",
                                                            h4(strong(htmlOutput(paste0("text",TabsIndicadores1[j],"_",TabsIndicadores[i])))),
                                                            hr(),
                                                            #h3(em(strong(br("Descripcion"))),
                                                            h4(strong(htmlOutput(paste0("text1",TabsIndicadores1[j],"_",TabsIndicadores[i])))),
                                                            imageOutput(paste0("mapa_imagen",TabsIndicadores1[j],"_",TabsIndicadores[i]),width='100%',height='100%'),
                                                            leafletOutput(paste0("mapa_shapes1",TabsIndicadores1[j],"_",TabsIndicadores[i]),width='100%', height ='500' ),
                                                            leafletOutput(paste0("mapa_WASH",TabsIndicadores1[j],"_",TabsIndicadores[i]),width='100%', height ='500' ),
                                                            br(),
                                                            fluidRow(
                                                              column(2,
                                                                     checkboxInput(paste0("graficolog",TabsIndicadores1[j],"_",TabsIndicadores[i]), label = "Eje Y en escala logaritmica", value = FALSE)
                                                              ),
                                                              column(5,
                                                                     uiOutput(paste0("umbral_alertaIn",TabsIndicadores1[j],"_",TabsIndicadores[i]))
                                                              ),
                                                              column(5,
                                                                     uiOutput(paste0("umbral_criticoIn",TabsIndicadores1[j],"_",TabsIndicadores[i]))
                                                                                                                                   )
                                                            ),
                                                            
                                                            fluidRow(
                                                              column(3,
                                                                     strong(htmlOutput((paste0("textU01",TabsIndicadores1[j],"_",TabsIndicadores[i])))),
                                                                     htmlOutput((paste0("textU11",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU11_1",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU21",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU21_1",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU31",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU31_1",TabsIndicadores1[j],"_",TabsIndicadores[i])))
                                                              ),
                                                              column(3,
                                                                     strong(htmlOutput((paste0("textU02",TabsIndicadores1[j],"_",TabsIndicadores[i])))),
                                                                     htmlOutput((paste0("textU12",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU12_1",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU22",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU22_1",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU32",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU32_1",TabsIndicadores1[j],"_",TabsIndicadores[i])))
                                                              ),
                                                              column(3,
                                                                     strong(htmlOutput((paste0("textU03",TabsIndicadores1[j],"_",TabsIndicadores[i])))),
                                                                     htmlOutput((paste0("textU13",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU13_1",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU23",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU23_1",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU33",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU33_1",TabsIndicadores1[j],"_",TabsIndicadores[i])))
                                                              ),
                                                              column(3,
                                                                     strong(htmlOutput((paste0("textU04",TabsIndicadores1[j],"_",TabsIndicadores[i])))),
                                                                     htmlOutput((paste0("textU14",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU14_1",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU24",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU24_1",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU34",TabsIndicadores1[j],"_",TabsIndicadores[i]))),
                                                                     htmlOutput((paste0("textU34_1",TabsIndicadores1[j],"_",TabsIndicadores[i])))
                                                              )
                                                            ),
                                                            plotlyOutput(paste0("grafico",TabsIndicadores1[j],"_",TabsIndicadores[i])), #,width='100%'
                                                            verbatimTextOutput(paste0("summary",TabsIndicadores1[j],"_",TabsIndicadores[i])),
                                                            plotlyOutput(paste0("graficoWaTT",TabsIndicadores1[j],"_",TabsIndicadores[i])), #,width='100%'
                                                            br(),
                                                            div(DT::dataTableOutput(paste0("tabla",TabsIndicadores1[j],"_",TabsIndicadores[i])), style = "font-size: 100%; width: 100%")
                                                            
                                                  )
                                                  
                                                )))
                                          })))
                                        )
                                      }))))
                            #)
                            
                          )
                ))
              )
      ),
      #######################
      #Mapas
      #######################
      tabItem("Mapas",
              fluidPage(
                useShinyjs(),
                wellPanel(style = "background: white",
                          # aqui la parte superior
                    wellPanel(style = "background: white",
                          fluidRow(
                            column(2,
                                   actionButton("ActualizarRuns1", "Actualizar corridas")
                            ),
                            column(10,
                                   selectInput("Nrun1", "Seleccione el numero de corrida", runsScript,runsScript[length(runsScript)])
                                   )
                          )),
                actionButton("test1", label = "Esconder/Mostrar Tabla resumen de acciones segun corrida seleccionada"),
                wellPanel(id="panel1",style = "background: white",
                          div(DT::dataTableOutput("TableRun1"), style = "font-size: 100%; width: 100%")
                ),   
                hr(),
                wellPanel(style = "background: white",
                          selectInput("yearpop1",label="Escenario", choices = a2),
                          tagList(
                            tags$head(
                              tags$style(
                                ".map-container {
                                    height: 100%;
                                    width: 100%;
                                    position: relative;
                                  }",
                                ".map-loading {
                                    position: absolute;
                                    display: flex;
                                    justify-content: center;
                                    align-items: center;
                                    top: 0;
                                    left: 0;
                                    width: 100%;
                                    height: 100%;
                                    background-color: #bdbdbd;
                                    text-align: center;
                                  }"
                              )
                            ),
                            tags$main(
                              tags$div(
                                class = "map-container",
                                tags$div(
                                  id = "leafletBusy",
                                  class = "map-loading",
                                  tags$p("Cargando... por favor espere") #aqui el mensaje
                                ),
                                leafletOutput("mapaTodos",width='100%', height ='1000' ) #, height = '100%'
                              )
                            ),
                            tags$script(
                              "
                              Shiny.addCustomMessageHandler('hide_loading', function(value) {
                                const el = document.getElementById(value);
                                el.style.display = 'none';
                              });
                              Shiny.addCustomMessageHandler('show_loading', function(value) {
                                const el = document.getElementById(value);
                                el.style.display = 'flex';
                              });
                              "
                            )
                          )
                 ))
                ))
                                        
      #######################
    )
  )
)
########

########
server = function(input, output, session) {
  
  observe_helpers() 
  
  session$onSessionEnded(stopApp)
  
  #MTDP
  ##################################

  output$Next_Previous=renderUI({
    tab_list=input$List_of_tab 
    nb_tab=length(tab_list)
    if (which(tab_list==input$tabBox_next_previous)==nb_tab)
      column(1,Previous_Button)
    else if (which(tab_list==input$tabBox_next_previous)==1)
      column(1,offset=2,Next_Button)
    else
      div(column(1,Previous_Button),column(1,offset=1,Next_Button))
    
  })
  
  observeEvent(input$Prev_Tab, {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab-1])
               })
  
  observeEvent(input$Next_Tab, {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab+1])
               })
  
  observe({
    lapply(1:(length(TabsAcciones)-1), function(i) {
      
      output[[paste0("imagenA",TabsAcciones[i])]]= renderImage({
        
        map=as.character(subset(imagenAcc,imagenAcc$Categoria==TabsAcciones[i],"Imagen"))
        
       if (isTRUE(subset(imagenAcc,imagenAcc$Categoria==TabsAcciones[i],"Imagen")!="NO") && file.exists(map)) {
          try(
            list(
              src = paste0(map),
              alt ="" 
            )
          )
        }else {
          shinyjs::hide(paste0("imagenA",TabsAcciones[i]))
          list(src=imagenblanca)
        }
        
      }, deleteFile = FALSE)
      
        output[[paste0("ID_A_",TabsAcciones[i])]] = renderUI({
            lapply(1:length(data[data$Categoria==TabsAcciones[i],"Accion"]), function(k) {
              #k=1 
              if (data[data$Categoria==TabsAcciones[i],"Tipo"][k]=="Opciones") {
                 selectInput(inputId = paste0("D",data[data$Categoria==TabsAcciones[i],"N"][k],"_Out"),
                             label = data[data$Categoria==TabsAcciones[i],"Accion"][k],
                             choices=InvAcciones[InvAcciones$Categoria==TabsAcciones[i] & InvAcciones$`Accion`==data[data$Categoria==TabsAcciones[i],"Accion"][k],"Opcion"]
                             ) %>% 
                  helper(#icon = "question", 
                    #colour = "orange",
                    size = "s",
                    type = "inline",
                    title = "Descripcion",
                    content = c(paste("<b>Categoria:</b>", TabsAcciones[i]),
                                paste("<b>Variable:</b>", data[data$Categoria==TabsAcciones[i],"Accion"][k]),
                                paste("<b>Informacion:</b>", data[data$Categoria==TabsAcciones[i],"Descripcion Largo"][k]),
                                "</br>Aqui se puede agregar lo que se quiera!"),
                    easyClose = TRUE,
                    buttonLabel = "Ok")
               } else {
                #k=5
                  num=InvAcciones[InvAcciones$Categoria==TabsAcciones[i] & InvAcciones$`Accion`==data[data$Categoria==TabsAcciones[i],"Accion"][k],"Opcion"]
                  num=as.numeric(gsub("[[:punct:]]","",unlist(strsplit(num,"-"))))
                  num1=num[1]
                  num2=num[2]
                  sliderInput(paste0("D",data[data$Categoria==TabsAcciones[i],"N"][k],"_Out"),
                             label = data[data$Categoria==TabsAcciones[i],"Accion"][k], 
                             min = num1, max = num2, value = num1) %>% 
                    helper(#icon = "question", 
                      #colour = "orange",
                      size = "s",
                      type = "inline",
                      title = "Descripcion",
                      content = c(paste("<b>Categoria:</b>", TabsAcciones[i]),
                                  paste("<b>Variable:</b>", data[data$Categoria==TabsAcciones[i],"Accion"][k]),
                                  paste("<b>Informacion:</b>", data[data$Categoria==TabsAcciones[i],"Descripcion Largo"][k]),
                                  "</br>Aqui se puede agregar lo que se quiera!"),
                      easyClose = TRUE,
                      buttonLabel = "Ok")
                  
                 
               }
            })
        })
        shinyjs::hide(paste0("tablaA",TabsAcciones[i]))
        shinyjs::hide(paste0("Text_A",TabsAcciones[i]))
        shinyjs::hide(paste0("CorrerWEAP",TabsAcciones[i]))
        shinyjs::hide(paste0("Text_Arun",TabsAcciones[i]))
        shinyjs::hide(paste0("infobox",TabsAcciones[i]))
      })
    })

  data.sel=reactive({
    data.sel=data
    for (i in seq(data$Categoria)) {
      data.sel$Opcion[i] = as.character(input[[paste0("D",as.character(i),"_Out")]])
    }
    #write.csv(data.sel,paste0(FolderAcciones,"data.sel.csv"),row.names=F)
    data.sel
  })
  
  output[[paste0("tablaA",TabsAcciones[length(TabsAcciones)])]] = DT::renderDataTable({
    data.sel()[,c("Categoria","Accion","Opcion")]
  },rownames = FALSE, options = list(searching = TRUE, pageLength = 10,scrollX = T))
  
  output[[paste0("Text_A",TabsAcciones[length(TabsAcciones)])]] = renderText({ 
    text=paste0("Haga clic en el boton para simular las estrategias")
    sprintf('<font color="%s">%s</font>',"mediumseagreen",text)})
  
  output[[paste0("Text_Arun",TabsAcciones[length(TabsAcciones)])]] = renderText({ 
    text=paste0("Una vez la modelacion termine puede explorar los resultados")
    sprintf('<font color="%s">%s</font>',"mediumseagreen",text)})
  
  observeEvent(input[[paste0("CorrerWEAP",TabsAcciones[length(TabsAcciones)])]], {
    ask_confirmation(
      inputId = "Confirmar",
      title = "Se va a modelar el conjunto de acciones seleccionadas. Si desea confirmar haga clic en SI, de lo contrario haga clic en NO para seleccionar otras acciones",
      btn_labels = c("SI", "NO"),
      btn_colors = c("mediumseagreen","red")
    )
  })
  
  observeEvent(input[["Confirmar"]],{ 
    if (input[["Confirmar"]]==FALSE) {
      
      shinyjs::hide(paste0("infobox",TabsAcciones[length(TabsAcciones)]))
      
      output[[paste0("Text_Arun",TabsAcciones[length(TabsAcciones)])]] = renderText({ 
        text=paste0("Una vez la modelacion termine puede explorar los resultados")
        sprintf('<font color="%s">%s</font>',"mediumseagreen",text)})
      
      start = Sys.time()
      
      acciones= read.csv(paste0(FolderAcciones,"Lista acciones.csv"), check.names = F, stringsAsFactors = F,header = T)
      #colnames(acciones)[1] = gsub('^...','',colnames(acciones)[1])
      data.sel=data.sel()
      row=t(data.frame(c(nrow(acciones)+1,data.sel$Opcion)))
      colnames(row)=colnames(acciones)
      rownames(row)=NULL
      acciones1=rbind(acciones,row)

      if (duplicated(acciones1[,c(2:ncol(acciones1))])[nrow(acciones1)]==TRUE){
        output[[paste0("Text_Arun",TabsAcciones[length(TabsAcciones)])]] = renderText({ 
          outTxt = ""
          
          text=paste0("La corrida numero: ")
          formatedFont = sprintf('<font color="%s">%s</font>',"red",text)
          outTxt = paste(outTxt, formatedFont,collapse=' ')
          
          text=acciones1[duplicated(acciones1[,2:ncol(acciones1)],fromLast=TRUE),"N_Corrida"]
          formatedFont = sprintf('<font color="%s">%s</font>',"red",text)
          outTxt = paste(outTxt, formatedFont,collapse=' ')
          
          text=paste0(" corresponde a las mismas acciones seleccionadas. Cambie las opciones para modelar un conjunto de acciones diferente.")
          formatedFont = sprintf('<font color="%s">%s</font>',"mediumseagreen",text)
          outTxt = paste(outTxt, formatedFont,collapse=' ')
          
          outTxt
          
        })
      } else {
        
        acciones=rbind(acciones,row)
        #write.table(na.exclude(acciones),paste0(FolderAcciones,"Lista acciones.csv"),row.names=F, col.names=T ,sep=",")
        
        key=data.sel[,c("Categoria","N","Key assumption",	"Opcion")]
        key$`Key assumptionOpcionCategoria`=paste0(as.character(key$`Key assumption`),as.character(key$Opcion),as.character(key$Categoria))
        InvAcciones$`Key assumptionOpcionCategoria`=paste0(as.character(InvAcciones$`Key assumption`),as.character(InvAcciones$Opcion),as.character(InvAcciones$Categoria))         
        key=merge(key,InvAcciones[,c("Valor","Key assumptionOpcionCategoria")],"Key assumptionOpcionCategoria",all.x=TRUE)
        key=key[order(as.numeric(key$N)),c("N","Key assumption","Valor","Opcion")]
        key[is.na(key$Valor),"Valor"]=key[is.na(key$Valor),"Opcion"]
        
        row=t(data.frame(c(0,"Key\\NumRun",nrow(acciones),nrow(acciones))))
        colnames(row)=colnames(key)
        rownames(row)=NULL
        key=rbind(row,key)
        
        #write.csv(na.exclude(key),paste0(FolderAcciones,"Estrategia_cuenca.csv"),row.names=F)
        #write.table(na.exclude(key), file = paste0(FolderAcciones,"Estrategia_cuenca.txt"), sep = "\t",row.names = FALSE,quote=FALSE)
        
        output[[paste0("Text_Arun",TabsAcciones[length(TabsAcciones)])]] = renderText({ 
          outTxt = ""
          
          text=paste0("Se ha modelado el conjunto de acciones numero: ")
          formatedFont = sprintf('<font color="%s">%s</font>',"mediumseagreen",text)
          outTxt = paste(outTxt, formatedFont,collapse=' ')
          
          text=nrow(acciones)
          formatedFont = sprintf('<font color="%s">%s</font>',"red",text)
          outTxt = paste(outTxt, formatedFont,collapse=' ')
          
          text=paste0(". En la seccion de Indicadores puede explorar los resultados")
          formatedFont = sprintf('<font color="%s">%s</font>',"mediumseagreen",text)
          outTxt = paste(outTxt, formatedFont,collapse=' ')
          
          outTxt
          
        })
        
        output[[paste0("infobox",TabsAcciones[length(TabsAcciones)])]] = renderInfoBox({
          infoBox("Simulacion completada.",a("Ir a explorar Indicadores", onclick = "openTab('Indicadores')", href="#"),
                  icon = icon("thumbs-o-up"), color = "green" )
        })
        
        shinyjs::show(paste0("infobox",TabsAcciones[length(TabsAcciones)]))
        
      }
 
    } else if (input[["Confirmar"]]==TRUE) {
      
      output[[paste0("Text_Arun",TabsAcciones[length(TabsAcciones)])]] = renderText({ 
        text=paste0("Una vez la modelacion termine puede explorar los resultados")
        sprintf('<font color="%s">%s</font>',"mediumseagreen",text)})
      
      
      shinyjs::hide(paste0("infobox",TabsAcciones[length(TabsAcciones)]))
    }
  })
  
  ##################################
  
  #Indicadores
  ##################################
  
  acciones=reactive({
    acciones=read.csv(paste0(FolderAcciones,"Lista acciones.csv"), check.names = F, stringsAsFactors = F,header = T)
    #colnames(acciones)[1] = gsub('^...','',colnames(acciones)[1])
    acciones
  })
  
  output$TableRun = DT::renderDataTable({
    acciones=acciones()
    acciones=subset(acciones,acciones$N_Corrida==input$Nrun)
    t_acciones=transpose(acciones)
    t_acciones$Accion=colnames(acciones)
    colnames(t_acciones) = c("Expresion","Accion")
    t_acciones[,c(2,1)]
  },rownames = FALSE, options = list(searching = TRUE, pageLength = 10,scrollX = T))
  
  observeEvent(input$ActualizarRuns,{ 
    runsScript=unique(substring(list.files(pattern = "_Indicadores.csv$"), 1,1))
    updateSelectInput(session, "Nrun",
                      label = "Seleccione el numero de corrida" ,
                      choices = runsScript, runsScript[length(runsScript)])

  })
  
  observeEvent(input$test, {
    shinyjs::toggle(id= "panel")
  })
  
  observe({
    lapply(1:length(TabsIndicadores), function(i) {
      
      #i=1
      TabsIndicadores1=unique(subset(InvIndicadores,InvIndicadores$`Tema_Central`==TabsIndicadores[i])[,"Variable"])
      
      lapply(1:length(TabsIndicadores1), function(j) {
        
        #j=1
        TI=TabsIndicadores[i]
        TI1=TabsIndicadores1[j]
        #var="Caudal-Media" #"Area de estudio" #Area_" #"Poblacion_CoberturaPor_Media-AnioSequia" # "Poblacion" #_CoberturaPor_Media" # "JMP Agua_seco" #  "Flujo de retorno-Media" #Poblacion_CoberturaPor_Media" #JMP Agua_seco" #"Mineria" #  #"Carga contaminante al ambiente" #"Poblacion_CoberturaPor_Media" #"Poblacion_DemandaInsatisfecha-Media" "Poblacion" "Irrigacion_AreaRiego" "Area_" "Caudal-Media"
        #Nrun=1
        var=input[[paste0("out_",TI,"_",TI1)]]
        Nrun= input$Nrun
        
        #for (l in seq(InvIndicadores$Nombre)) {
        #var=InvIndicadores$Nombre[l]
        DatosVar=subset(InvIndicadores,InvIndicadores$Nombre==var)
        if (isTRUE(DatosVar$Fuente_Informacion == "Script Indicadores")){
            file=NULL
            for (k in 1:length(a1)){
              f1=read.csv(paste0(ScriptIndicadoresFiles,Nrun,"_",a1[k],"_Indicadores.csv"), check.names = F, stringsAsFactors = F,header = T)
              #colnames(f1)[1] = gsub('^...','',colnames(f1)[1])
              f1$Nrun=k
              f1$Escenario=a2[k]
              file=rbind(file,f1)
            }
            py=unique(file$Nrun)
            remove(f1)
            
            file=subset(file,file$Prefix==var)
            file$NameDef=gsub(paste0("^",gsub("\\\\", "-", DatosVar$LimpiarNombre)),"",gsub("\\\\", "-",file[,DatosVar$ColumnaInformacion]))
            file2=subset(file,file$Prefix==var & file$Nrun==py[1], c("NameDef", "Value"))
            file2=unique(as.vector(file2[order(file2$Value, decreasing = TRUE),"NameDef"]))
            #file[,DatosVar$ColumnaInformacion] = factor(file[,DatosVar$ColumnaInformacion], labels=file2, levels = unique(file[,DatosVar$ColumnaInformacion]))
            file$NameDef = factor(file$NameDef, levels = file2)
            if (!is.na(DatosVar$RedondearDecimales)){
              file$Value=round(file$Value,DatosVar$RedondearDecimales)
            }
            remove(file2)
            
            #str(file)
            
            if (!is.na(DatosVar$RedondearDecimales)){
              file$Value=round(file$Value,DatosVar$RedondearDecimales)
            }
            
            file3=file
            
            file4=file[,c("NameDef","Value", "Escenario")]
            file4 = file4 %>% pivot_wider(names_from ="Escenario" , values_from = "Value")
            file4=file4[order(file4[,2],decreasing=TRUE),]
            colnames(file4)[1]=DatosVar$Nombre
            file4=as.data.frame(file4)
            
            file5=file[,c(DatosVar$ColumnaInformacion,"Value", "Escenario")]
            file5 = file5 %>% pivot_wider(names_from ="Escenario" , values_from = "Value")
            file5=as.data.frame(file5)
            #str(file)
            
        } else if (isTRUE(DatosVar$Fuente_Informacion == "WASH Flows")) {
          
            file=read.csv(paste0(WASHfiles,Nrun,"_WASHtest.csv"), check.names = F, stringsAsFactors = F,header = T)
            #colnames(file)[1] = gsub('^...','',colnames(file)[1])
            file$Nrun=NA
            file$Escenario=NA
            for (k in 1:length(a1)){
              file[file$WEAPEscenario==a1[k],"Nrun"]=k
              file[file$WEAPEscenario==a1[k],"Escenario"]=a2[k]
            }
            file=subset(file,file$Prefix==var)
            
            if (!is.na(DatosVar$RedondearDecimales)){
              file$Value=round(file$Value,DatosVar$RedondearDecimales)
            }
            
            file$NameDef=gsub(paste0("^",gsub("\\\\", "-", DatosVar$LimpiarNombre)),"",gsub("\\\\", "-",file[,DatosVar$ColumnaInformacion]))
            
            file$Grupo=paste0(file$Escenario," - ", file$Categoria)
            
            file=as.data.frame(file)
            fileT=file
            
            file1 = aggregate(x =file[,c("Value")],by = list(Name=file[,DatosVar$ColumnaInformacion], Escenario=file$Escenario), FUN = sum)
            colnames(file1)[3]="Value"
            file2 = aggregate(x =file[,c("Value")],by = list(Name=file[,DatosVar$ColumnaInformacion], Escenario=file$Escenario), FUN = min)
            colnames(file2)[3]="Value"
            file3=rbind(file1,file2)
            remove(file1)
            remove(file2)
            
            categ=unlist(strsplit(DatosVar$Umbral_Categorias,","))
            if (unique(file$Categoria)!="" && length(categ)!=0){
              remove(file)
              file=NULL
              for (k in seq(categ)){
                file2=subset(fileT, fileT$Categoria==categ[k])
                file=rbind(file,file2)
              }
              file = as.data.frame(aggregate(x =file[,c("Value")],by = list(NameDef=file$NameDef, Escenario=file$Escenario), FUN = sum))
              colnames(file)[3]="Value"
              remove(file2)
            }
            file1=file
            
         # }
        } 
        
        output[[paste0("text",TI1,"_",TI)]] = renderText({
          text=as.character(paste0("Se esta mostrando la opcion: ",input[[paste0("out_",TI,"_",TI)]],". Variable: ",TI,". Categoria: ",TI))
          sprintf('<font color="%s">%s</font>',"mediumseagreen",text)
        })
        
        output[[paste0("text1",TI1,"_",TI)]] = renderText({
          text1=subset(InvIndicadores,InvIndicadores$Nombre==input[[paste0("out_",TI,"_",TI1)]],"Descripcion")
          text=as.character(paste0("Descripcion: ",text1))
          sprintf('<font color="%s">%s</font>',"mediumseagreen",text)
        })
        
        output[[paste0("mapa_imagen",TI1,"_",TI)]] = renderImage({
 
          map=as.character(subset(InvIndicadores,InvIndicadores$Nombre==var,"Imagen"))
          
           if (isTRUE(is.element(input[[paste0("out_",TI,"_",TI1)]],imagen)) && (file.exists(map))){
              try(
                list(
                  src = map,
                  width = "80%"
                  #height = "100%"
                ))
            } else {
              shinyjs::hide(paste0("mapa_imagen",TI1,"_",TI))
              list(src=imagenblanca)
            }
        }, deleteFile = FALSE)
        
        output[[paste0("umbral_alertaIn",TI1,"_",TI)]]= renderUI({
          
            shinyjs::show(paste0("umbral_alertaIn",TI1,"_",TI))
            if (isTRUE(is.element(var,grafico)) && isTRUE(DatosVar$Critico=="MAX") && isTRUE(DatosVar$Umbral_Alerta=="NO") && isTRUE(DatosVar$Umbral_Critico=="NO")) {

                alerta=max(file3$Value)
                critico=max(file3$Value)
                
                #alerta
                #critico
                
                #numericInput  sliderInput
                sliderInput(paste0("umbral_alerta",TI1,"_",TI), "Seleccione el umbral Alerta:", 
                            min =min(file3$Value) , max = max(file3$Value), value=alerta)
              
            } else if (isTRUE(is.element(var,grafico)) && isTRUE(DatosVar$Critico=="MAX") && isTRUE(DatosVar$Umbral_Alerta!="NO") && isTRUE(DatosVar$Umbral_Critico!="NO"))  {
              
                alerta1=as.numeric(DatosVar$Umbral_Alerta)
                critico1=as.numeric(DatosVar$Umbral_Critico)
                if (alerta1<min(file3$Value)){alerta=min(file3$Value)} else {alerta=alerta1}
                if (critico1<alerta){critico=alerta} else if (critico1>max(file3$Value)) {critico=max(file3$Value)} else {critico=critico1}
                
                #alerta
                #critico
                
                #numericInput  sliderInput
                sliderInput(paste0("umbral_alerta",TI1,"_",TI), "Seleccione el umbral Alerta:", 
                            min =min(file3$Value)*0.9 , max = max(file3$Value)*1.1, value=alerta)
              
            } else if (isTRUE(is.element(var,grafico)) && isTRUE(DatosVar$Critico=="MIN") && isTRUE(DatosVar$Umbral_Alerta=="NO") && isTRUE(DatosVar$Umbral_Critico=="NO")) {
              
                alerta=min(file3$Value)
                critico=min(file3$Value)
                
                sliderInput(paste0("umbral_critico",TI1,"_",TI), "Seleccione el umbral Critico:", 
                            min =min(file3$Value) , max = max(file3$Value) , value=alerta)
                
                     
            } else if (isTRUE(is.element(var,grafico)) && isTRUE(DatosVar$Critico=="MIN") && isTRUE(DatosVar$Umbral_Alerta!="NO") && isTRUE(DatosVar$Umbral_Critico!="NO"))  {
              
                critico1=as.numeric(DatosVar$Umbral_Alerta)
                alerta1=as.numeric(DatosVar$Umbral_Critico)
                
                if (alerta1<min(file3$Value)){alerta=min(file3$Value)} else {alerta=alerta1}
                if (critico1<alerta){critico=alerta} else if (critico1>max(file3$Value)) {critico=max(file3$Value)} else {critico=critico1}
                
                
                sliderInput(paste0("umbral_critico",TI1,"_",TI), "Seleccione el umbral Critico:", 
                            min =min(file3$Value)*0.9 , max = max(file3$Value)*1.1 , value=alerta)
              
            } else {
                
              shinyjs::hide(paste0("umbral_alertaIn",TI1,"_",TI))
          }
  
        })
        
        output[[paste0("umbral_criticoIn",TI1,"_",TI)]]= renderUI({
          
          shinyjs::show(paste0("umbral_criticoIn",TI1,"_",TI))
          if (isTRUE(is.element(var,grafico)) &&  isTRUE(DatosVar$Umbral_Alerta=="NO") && isTRUE(DatosVar$Umbral_Critico=="NO") && isTRUE(DatosVar$Critico=="MAX")) {
            
            req(input[[paste0("umbral_alerta",TI1,"_",TI)]])
            alerta=max(file3$Value)
            critico=max(file3$Value)
            
            #numericInput  sliderInput
            sliderInput(paste0("umbral_critico",TI1,"_",TI), "Seleccione el umbral Critico:", 
                        min =as.numeric(input[[paste0("umbral_alerta",TI1,"_",TI)]]) , max = max(file3$Value) , value=critico)
              
          } else if (isTRUE(is.element(var,grafico)) &&  isTRUE(DatosVar$Umbral_Alerta!="NO") && isTRUE(DatosVar$Umbral_Critico!="NO") && isTRUE(DatosVar$Critico=="MAX")) {
              
              req(input[[paste0("umbral_alerta",TI1,"_",TI)]])
              alerta1=as.numeric(DatosVar$Umbral_Alerta)
              critico1=as.numeric(DatosVar$Umbral_Critico)
              if (alerta1<min(file3$Value)){alerta=min(file3$Value)} else {alerta=alerta1}
              if (critico1<alerta){critico=alerta} else if (critico1>max(file3$Value)) {critico=max(file3$Value)} else {critico=critico1}
              
              #numericInput  sliderInput
              sliderInput(paste0("umbral_critico",TI1,"_",TI), "Seleccione el umbral Critico:", 
                          min =as.numeric(input[[paste0("umbral_alerta",TI1,"_",TI)]]) , max = max(file3$Value)*1.1 , value=critico)
           
           } else if (isTRUE(is.element(var,grafico)) &&  isTRUE(DatosVar$Umbral_Alerta=="NO") && isTRUE(DatosVar$Umbral_Critico=="NO") && isTRUE(DatosVar$Critico=="MIN")) {
              
              req(input[[paste0("umbral_critico",TI1,"_",TI)]])
              alerta=min(file3$Value)
              critico=min(file3$Value)
              
             #alerta
             #critico

              sliderInput(paste0("umbral_alerta",TI1,"_",TI), "Seleccione el umbral Alerta:", 
                          min =as.numeric(input[[paste0("umbral_critico",TI1,"_",TI)]]) , max = max(file3$Value), value=critico)
            
          } else if (isTRUE(is.element(var,grafico)) && isTRUE(DatosVar$Umbral_Alerta!="NO") && isTRUE(DatosVar$Umbral_Critico!="NO") && isTRUE(DatosVar$Critico=="MIN")) {
            
            req(input[[paste0("umbral_critico",TI1,"_",TI)]])
            critico1=as.numeric(DatosVar$Umbral_Alerta)
            alerta1=as.numeric(DatosVar$Umbral_Critico)
            if (alerta1<min(file3$Value)){alerta=min(file3$Value)} else {alerta=alerta1}
            if (critico1<alerta){critico=alerta} else if (critico1>max(file3$Value)) {critico=max(file3$Value)} else {critico=critico1}
            
            sliderInput(paste0("umbral_alerta",TI1,"_",TI), "Seleccione el umbral Alerta:", 
                        min =as.numeric(input[[paste0("umbral_critico",TI1,"_",TI)]]) , max = max(file3$Value)*1.1 , value=critico)
            
          } else {
            shinyjs::hide(paste0("umbral_criticoIn",TI1,"_",TI))
          }
          
        })

        output[[paste0("grafico",TI1,"_",TI)]]= renderPlotly({
          
            
          if (isTRUE(is.element(input[[paste0("out_",TI,"_",TI1)]],grafico))) {
          #var="ExtraccionTransmision-Media" #"JMP Agua" #JMP Agua" #"Irrigacion_AreaRiego"  #"Carga contaminante al ambiente" # #"JMP Agua" #"Irrigacion_AreaRiego" #"Poblacion" #"Poblacion_CoberturaPor_Media" #  #"Poblacion_CoberturaPor_Media" #"Poblacion_DemandaInsatisfecha-Media" "Poblacion" "Irrigacion_AreaRiego" "Area_" "Caudal-Media"
            
            #Nrun=1
            
            Ualerta=as.numeric(input[[paste0("umbral_alerta",TI1,"_",TI)]])
            Ucritico=as.numeric(input[[paste0("umbral_critico",TI1,"_",TI)]])
            
            #ejey=TRUE  
            ejey=input[[paste0("graficolog",TI1,"_",TI)]]
            
            
            m = list(
              l = 100,
              r = 50,
              b = 100,
              t = 100,
              pad = 4
            )
            
            
            hlineAlerta = function(y = 0, color = "orange") {
              list(
                type = "line", 
                x0 = 0, 
                x1 = 1, 
                xref = "paper",
                y0 = y, 
                y1 = y, 
                line = list(color = color)
              )
            }
            
            hlineCritico = function(y = 0, color = "red") {
              list(
                type = "line", 
                x0 = 0, 
                x1 = 1, 
                xref = "paper",
                y0 = y, 
                y1 = y, 
                line = list(color = color)
              )
            }
            
            pal=colorNumeric(DatosVar$ColorEscenarios, domain = NULL)
            color=NULL
            for (k in seq(a1)){
              color=c(color,rep(pal(1:(length(a1)+1))[k+1],nrow(subset(file,file$Escenario==a2[k]))))
            }
            
            if (isTRUE(ejey)) {
              y = list(title = paste0(DatosVar$Variable,"(",DatosVar$Unidad,")"),  type = "log")
            } else {
              y = list( title = paste0(DatosVar$Variable,"(",DatosVar$Unidad,")"))
            }
            
            if (isTRUE(is.element(input[[paste0("out_",TI,"_",TI1)]],grafico)) && DatosVar$Critico=="MAX" && DatosVar$Umbral_Alerta!="NO" && DatosVar$Umbral_Critico!="NO"){
              
              #Ualerta=as.numeric(quantile(file$Value,0.5))
              #Ucritico=as.numeric(quantile(file$Value,0.75))

              if (is.na(DatosVar$RedondearDecimales)){
                DatosVar$RedondearDecimales=2
              }
              
              k=1
              output[[paste0("textU01",TI1,"_",TI)]] = renderText({
                text=as.character(paste0("Escenario: ",a2[1]))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              file11=subset(file,file$Escenario==a2[k])
              output[[paste0("textU1",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file11$Value>=Ucritico))/length(file11$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos iguales o mayores al umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),"): ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU1",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file11$NameDef[which(file11$Value>=Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU2",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file11$Value>=Ualerta & file11$Value<Ucritico))/length(file11$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores o iguales al umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") y menores que el umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU2",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file11$NameDef[which(file11$Value>=Ualerta & file11$Value<Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU3",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file11$Value<Ualerta))/length(file11$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos menores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              output[[paste0("textU3",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file11$NameDef[which(file11$Value<Ualerta)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              k=2
              output[[paste0("textU02",TI1,"_",TI)]] = renderText({
                text=as.character(paste0("Escenario: ",a2[2]))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              file2=subset(file,file$Escenario==a2[k])
              output[[paste0("textU1",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file2$Value>=Ucritico))/length(file2$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos iguales o mayores al umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),"): ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU1",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file2$NameDef[which(file2$Value>=Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU2",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file2$Value>=Ualerta & file2$Value<Ucritico))/length(file2$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores o iguales al umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") y menores que el umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU2",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file2$NameDef[which(file2$Value>=Ualerta & file2$Value<Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU3",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file2$Value<Ualerta))/length(file2$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos menores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              output[[paste0("textU3",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file2$NameDef[which(file2$Value<Ualerta)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              k=3
              output[[paste0("textU03",TI1,"_",TI)]] = renderText({
                text=as.character(paste0("Escenario: ",a2[3]))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              file33=subset(file,file$Escenario==a2[k])
              output[[paste0("textU1",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file33$Value>=Ucritico))/length(file33$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos iguales o mayores al umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),"): ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU1",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file33$NameDef[which(file33$Value>=Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU2",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file33$Value>=Ualerta & file33$Value<Ucritico))/length(file33$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores o iguales al umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") y menores que el umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU2",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file33$NameDef[which(file33$Value>=Ualerta & file33$Value<Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU3",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file33$Value<Ualerta))/length(file33$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos menores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              output[[paste0("textU3",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file33$NameDef[which(file33$Value<Ualerta)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              k=4
              output[[paste0("textU04",TI1,"_",TI)]] = renderText({
                text=as.character(paste0("Escenario: ",a2[4]))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              file44=subset(file,file$Escenario==a2[k])
              output[[paste0("textU1",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file44$Value>=Ucritico))/length(file44$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos iguales o mayores al umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),"): ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU1",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file44$NameDef[which(file44$Value>=Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU2",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file44$Value>=Ualerta & file44$Value<Ucritico))/length(file44$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores o iguales al umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") y menores que el umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU2",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file44$NameDef[which(file44$Value>=Ualerta & file44$Value<Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU3",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file44$Value<Ualerta))/length(file44$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos menores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              output[[paste0("textU3",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file44$NameDef[which(file44$Value<Ualerta)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              shinyjs::show(paste0("textU01",TI1,"_",TI))
              shinyjs::show(paste0("textU11",TI1,"_",TI))
              shinyjs::show(paste0("textU11_1",TI1,"_",TI))
              shinyjs::show(paste0("textU21",TI1,"_",TI))
              shinyjs::show(paste0("textU21_1",TI1,"_",TI))
              shinyjs::show(paste0("textU31",TI1,"_",TI))
              shinyjs::show(paste0("textU31_1",TI1,"_",TI))
              shinyjs::show(paste0("textU02",TI1,"_",TI))
              shinyjs::show(paste0("textU12",TI1,"_",TI))
              shinyjs::show(paste0("textU12_1",TI1,"_",TI))
              shinyjs::show(paste0("textU22",TI1,"_",TI))
              shinyjs::show(paste0("textU22_1",TI1,"_",TI))
              shinyjs::show(paste0("textU32",TI1,"_",TI))
              shinyjs::show(paste0("textU32_1",TI1,"_",TI))
              shinyjs::show(paste0("textU03",TI1,"_",TI))
              shinyjs::show(paste0("textU13",TI1,"_",TI))
              shinyjs::show(paste0("textU13_1",TI1,"_",TI))
              shinyjs::show(paste0("textU23",TI1,"_",TI))
              shinyjs::show(paste0("textU23_1",TI1,"_",TI))
              shinyjs::show(paste0("textU33",TI1,"_",TI))
              shinyjs::show(paste0("textU33_1",TI1,"_",TI))
              shinyjs::show(paste0("textU04",TI1,"_",TI))
              shinyjs::show(paste0("textU14",TI1,"_",TI))
              shinyjs::show(paste0("textU14_1",TI1,"_",TI))
              shinyjs::show(paste0("textU24",TI1,"_",TI))
              shinyjs::show(paste0("textU24_1",TI1,"_",TI))
              shinyjs::show(paste0("textU34",TI1,"_",TI))
              shinyjs::show(paste0("textU34_1",TI1,"_",TI))
              
              color[which(file$Value>=Ualerta)]="orange"
              color[which(file$Value>=Ucritico)]="red"
              
              
            } else if (isTRUE(is.element(input[[paste0("out_",TI,"_",TI1)]],grafico)) && DatosVar$Critico=="MIN" && DatosVar$Umbral_Alerta!="NO" && DatosVar$Umbral_Critico!="NO"){
              
              #Ualerta=as.numeric(quantile(file$Value,0.75))
              #Ucritico=as.numeric(quantile(file$Value,0.5))
              
              if (is.na(DatosVar$RedondearDecimales)){
                DatosVar$RedondearDecimales=2
              }
              
              k=1
              output[[paste0("textU01",TI1,"_",TI)]] = renderText({
                text=as.character(paste0("Escenario: ",a2[1]))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              file11=subset(file,file$Escenario==a2[k])
              output[[paste0("textU1",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file11$Value<=Ucritico))/length(file11$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos iguales o menores al umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),"): ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU1",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file11$NameDef[which(file11$Value<=Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU2",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file11$Value<=Ualerta & file11$Value>Ucritico))/length(file11$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores que el umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") e iguales o menores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU2",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file11$NameDef[which(file11$Value<=Ualerta & file11$Value>Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU3",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file11$Value>Ualerta))/length(file11$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              output[[paste0("textU3",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file11$NameDef[which(file11$Value>Ualerta)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              k=2
              output[[paste0("textU02",TI1,"_",TI)]] = renderText({
                text=as.character(paste0("Escenario: ",a2[2]))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              file22=subset(file,file$Escenario==a2[k])
              output[[paste0("textU1",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file22$Value<=Ucritico))/length(file22$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos iguales o menores al umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),"): ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU1",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file22$NameDef[which(file22$Value<=Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU2",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file22$Value<=Ualerta & file22$Value>Ucritico))/length(file22$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores que el umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") e iguales o menores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU2",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file22$NameDef[which(file22$Value<=Ualerta & file22$Value>Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU3",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file22$Value>Ualerta))/length(file22$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              output[[paste0("textU3",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file22$NameDef[which(file22$Value>Ualerta)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              k=3
              output[[paste0("textU03",TI1,"_",TI)]] = renderText({
                text=as.character(paste0("Escenario: ",a2[3]))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              file33=subset(file,file$Escenario==a2[k])
              output[[paste0("textU1",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file33$Value<=Ucritico))/length(file33$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos iguales o menores al umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),"): ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU1",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file33$NameDef[which(file33$Value<=Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU2",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file33$Value<=Ualerta & file33$Value>Ucritico))/length(file33$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores que el umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") e iguales o menores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU2",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file33$NameDef[which(file33$Value<=Ualerta & file33$Value>Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU3",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file33$Value>Ualerta))/length(file33$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              output[[paste0("textU3",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file33$NameDef[which(file33$Value>Ualerta)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              k=4
              output[[paste0("textU04",TI1,"_",TI)]] = renderText({
                text=as.character(paste0("Escenario: ",a2[4]))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              file44=subset(file,file$Escenario==a2[k])
              output[[paste0("textU1",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file44$Value<=Ucritico))/length(file44$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos iguales o menores al umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),"): ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU1",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file44$NameDef[which(file44$Value<=Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"red",text)
              })
              output[[paste0("textU2",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file44$Value<=Ualerta & file44$Value>Ucritico))/length(file44$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores que el umbral critico (",formatC(Ucritico, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") e iguales o menores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU2",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file44$NameDef[which(file44$Value<=Ualerta & file44$Value>Ucritico)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"orange",text)
              })
              output[[paste0("textU3",k,TI1,"_",TI)]] = renderText({
                p=round(length(which(file44$Value>Ualerta))/length(file44$Value)*100,2)
                text=as.character(paste0("Porcentaje de datos mayores que el umbral alerta (",formatC(Ualerta, format="f", big.mark=",", digits=DatosVar$RedondearDecimales),") : ",formatC(p, format="f", big.mark=",", digits=2), "%"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              output[[paste0("textU3",k,"_1",TI1,"_",TI)]] = renderText({
                p=paste(file44$NameDef[which(file44$Value>Ualerta)], collapse = ", ")
                if (p==""){p="-"}
                text=as.character(paste0("(",p, ")"))
                sprintf('<font color="%s">%s</font>',"black",text)
              })
              
              shinyjs::show(paste0("textU01",TI1,"_",TI))
              shinyjs::show(paste0("textU11",TI1,"_",TI))
              shinyjs::show(paste0("textU11_1",TI1,"_",TI))
              shinyjs::show(paste0("textU21",TI1,"_",TI))
              shinyjs::show(paste0("textU21_1",TI1,"_",TI))
              shinyjs::show(paste0("textU31",TI1,"_",TI))
              shinyjs::show(paste0("textU31_1",TI1,"_",TI))
              shinyjs::show(paste0("textU02",TI1,"_",TI))
              shinyjs::show(paste0("textU12",TI1,"_",TI))
              shinyjs::show(paste0("textU12_1",TI1,"_",TI))
              shinyjs::show(paste0("textU22",TI1,"_",TI))
              shinyjs::show(paste0("textU22_1",TI1,"_",TI))
              shinyjs::show(paste0("textU32",TI1,"_",TI))
              shinyjs::show(paste0("textU32_1",TI1,"_",TI))
              shinyjs::show(paste0("textU03",TI1,"_",TI))
              shinyjs::show(paste0("textU13",TI1,"_",TI))
              shinyjs::show(paste0("textU13_1",TI1,"_",TI))
              shinyjs::show(paste0("textU23",TI1,"_",TI))
              shinyjs::show(paste0("textU23_1",TI1,"_",TI))
              shinyjs::show(paste0("textU33",TI1,"_",TI))
              shinyjs::show(paste0("textU33_1",TI1,"_",TI))
              shinyjs::show(paste0("textU04",TI1,"_",TI))
              shinyjs::show(paste0("textU14",TI1,"_",TI))
              shinyjs::show(paste0("textU14_1",TI1,"_",TI))
              shinyjs::show(paste0("textU24",TI1,"_",TI))
              shinyjs::show(paste0("textU24_1",TI1,"_",TI))
              shinyjs::show(paste0("textU34",TI1,"_",TI))
              shinyjs::show(paste0("textU34_1",TI1,"_",TI))
              
              color[which(file$Value<=Ualerta)]="orange"
              color[which(file$Value<=Ucritico)]="red"
              
            } else {
              shinyjs::hide(paste0("textU01",TI1,"_",TI))
              shinyjs::hide(paste0("textU11",TI1,"_",TI))
              shinyjs::hide(paste0("textU11_1",TI1,"_",TI))
              shinyjs::hide(paste0("textU21",TI1,"_",TI))
              shinyjs::hide(paste0("textU21_1",TI1,"_",TI))
              shinyjs::hide(paste0("textU31",TI1,"_",TI))
              shinyjs::hide(paste0("textU31_1",TI1,"_",TI))
              shinyjs::hide(paste0("textU02",TI1,"_",TI))
              shinyjs::hide(paste0("textU12",TI1,"_",TI))
              shinyjs::hide(paste0("textU12_1",TI1,"_",TI))
              shinyjs::hide(paste0("textU22",TI1,"_",TI))
              shinyjs::hide(paste0("textU22_1",TI1,"_",TI))
              shinyjs::hide(paste0("textU32",TI1,"_",TI))
              shinyjs::hide(paste0("textU32_1",TI1,"_",TI))
              shinyjs::hide(paste0("textU03",TI1,"_",TI))
              shinyjs::hide(paste0("textU13",TI1,"_",TI))
              shinyjs::hide(paste0("textU13_1",TI1,"_",TI))
              shinyjs::hide(paste0("textU23",TI1,"_",TI))
              shinyjs::hide(paste0("textU23_1",TI1,"_",TI))
              shinyjs::hide(paste0("textU33",TI1,"_",TI))
              shinyjs::hide(paste0("textU33_1",TI1,"_",TI))
              shinyjs::hide(paste0("textU04",TI1,"_",TI))
              shinyjs::hide(paste0("textU14",TI1,"_",TI))
              shinyjs::hide(paste0("textU14_1",TI1,"_",TI))
              shinyjs::hide(paste0("textU24",TI1,"_",TI))
              shinyjs::hide(paste0("textU24_1",TI1,"_",TI))
              shinyjs::hide(paste0("textU34",TI1,"_",TI))
              shinyjs::hide(paste0("textU34_1",TI1,"_",TI))
            }
            
            if (isTRUE(is.element(input[[paste0("out_",TI,"_",TI1)]],grafico)) && DatosVar$Fuente_Informacion == "Script Indicadores") {
              
              x = list(
                title = DatosVar$Etiqueta_EjeX
              )
              
              output[[paste0("summary",TI1,"_",TI)]] = renderPrint({
                with( file , aggregate(Value , by=list(Escenario) , FUN=summary))
              })
              

              
              fig = plot_ly(file, x = ~NameDef, y = ~Value, type = 'bar', split=~Escenario, #color=~Value, colors=pal,
                            marker = list(color = color)) %>%
                layout(autosize = T, margin = m)  %>%
                layout(title = var, font=list(size = 14)) %>% 
                layout(xaxis = x, yaxis = y) %>%
                layout(shapes = list(hlineCritico(Ucritico), hlineAlerta(Ualerta)))
              
              
              shinyjs::show(paste0("grafico",TI1,"_",TI))
              shinyjs::show(paste0("graficolog",TI1,"_",TI))
              shinyjs::show(paste0("summary",TI1,"_",TI))
              
              fig
              

              
              
            } else if (isTRUE(is.element(input[[paste0("out_",TI,"_",TI1)]],grafico)) &&  DatosVar$Fuente_Informacion == "WASH Flows" &&  DatosVar$Apiladas != "SI") {
              
              output[[paste0("summary",TI1,"_",TI)]] = renderPrint({
                with( fileT , aggregate(Value , by=list(Grupo) , FUN=summary))
              })
              
              fileT

              x = list(
                title = DatosVar$Etiqueta_EjeX
              )
              
              fig = plot_ly(fileT, x = ~NameDef, y = ~Value, type = 'bar', split=~Escenario, #color=~Value, colors=pal,
                            marker = list(color = color)) %>%
                layout(autosize = T, margin = m)  %>%
                layout(title = var, font=list(size = 14)) %>% 
                layout(xaxis = x, yaxis = y)%>%
                layout(shapes = list(hlineCritico(Ucritico), hlineAlerta(Ualerta)))
              
              shinyjs::show(paste0("grafico",TI1,"_",TI))
              shinyjs::show(paste0("graficolog",TI1,"_",TI))
              shinyjs::show(paste0("summary",TI1,"_",TI))
              
              fig  

              
            } else if (isTRUE(is.element(input[[paste0("out_",TI,"_",TI1)]],grafico)) &&  DatosVar$Fuente_Informacion == "WASH Flows" &&  DatosVar$Apiladas == "SI") {
                
              output[[paste0("summary",TI1,"_",TI)]] = renderPrint({
                with( fileT , aggregate(Value , by=list(Grupo) , FUN=summary))
              })
              
              categ=unique(fileT$Categoria)
                colorcateg= unlist(strsplit(DatosVar$Colores_CategoriasApiladas,","))
                #colorcateg=c("blue","lightskyblue","yellow","#FAE5A2","orange") 
                
                plot_list = vector("list", length(a1))
                lab_list = vector("list", length(a1))
                #
                
                for (h in seq(a1)){
                  
                  file1=fileT
                  #h=1
                  file1=file1[file1$Nrun==h,]
                  file1 = as.data.frame(file1[, c("NameDef","Categoria","Value")] %>% pivot_wider(names_from ="Categoria" , values_from = "Value"))
                  x = list( title = paste0(a2[h]," - ",DatosVar$Etiqueta_EjeX))
                  
                  
                  if (h==1){
                    k=1
                    fig = plot_ly(file1, x = ~NameDef, y = as.vector(file1[,c(categ[k])]), type = 'bar',name=categ[1], marker = list(color=rep(colorcateg[k],nrow(file1))), showlegend=TRUE) #marker = list(color = color[nd:(nd+ndata-1)])
                    for (k in 2:length(categ)){
                      fig = fig %>% add_trace(y = as.vector(file1[,c(categ[k])]), name=categ[k],marker = list(color=rep(colorcateg[k],nrow(file1))), showlegend=TRUE) #,marker = list(color = color[nd:(nd+ndata-1)])
                    }
                    
                  } else {
                    k=1
                    fig = plot_ly(file1, x = ~NameDef, y = as.vector(file1[,c(categ[k])]), type = 'bar',name=categ[1], marker = list(color=rep(colorcateg[k],nrow(file1))), showlegend=FALSE) #marker = list(color = color[nd:(nd+ndata-1)])
                    for (k in 2:length(categ)){
                      fig = fig %>% add_trace(y = as.vector(file1[,c(categ[k])]), name=categ[k],marker = list(color=rep(colorcateg[k],nrow(file1))), showlegend=FALSE) #,marker = list(color = color[nd:(nd+ndata-1)])
                    }
                    
                  }
                  
                  fig = fig %>%
                    layout(shapes = list(hlineCritico(Ucritico), hlineAlerta(Ualerta)))
                  
                  #fig
                  
                  fig = fig %>% layout(autosize = T, margin = m)  %>%
                    layout(title = var, font=list(size = 14)) %>% 
                    layout(xaxis = x, yaxis = y, barmode = 'stack')
                  
                  plot_list[[h]] = fig
                  lab_list[[h]] = list(x=x, y=y)
                  
                }
                
                shinyjs::show(paste0("grafico",TI1,"_",TI))
                shinyjs::show(paste0("graficolog",TI1,"_",TI))
                shinyjs::show(paste0("summary",TI1,"_",TI))
                
                subplot(plot_list,shareY = TRUE, titleX=TRUE) %>%
                  layout(annotations = lab_list)
                

                
              } else {
                shinyjs::hide(paste0("grafico",TI1,"_",TI))
                shinyjs::hide(paste0("graficolog",TI1,"_",TI))
                shinyjs::hide(paste0("summary",TI1,"_",TI))
                
            }
            
          } else {
            shinyjs::hide(paste0("grafico",TI1,"_",TI))
            shinyjs::hide(paste0("graficolog",TI1,"_",TI))
            shinyjs::hide(paste0("summary",TI1,"_",TI))
            shinyjs::hide(paste0("textU01",TI1,"_",TI))
            shinyjs::hide(paste0("textU11",TI1,"_",TI))
            shinyjs::hide(paste0("textU11_1",TI1,"_",TI))
            shinyjs::hide(paste0("textU21",TI1,"_",TI))
            shinyjs::hide(paste0("textU21_1",TI1,"_",TI))
            shinyjs::hide(paste0("textU31",TI1,"_",TI))
            shinyjs::hide(paste0("textU31_1",TI1,"_",TI))
            shinyjs::hide(paste0("textU02",TI1,"_",TI))
            shinyjs::hide(paste0("textU12",TI1,"_",TI))
            shinyjs::hide(paste0("textU12_1",TI1,"_",TI))
            shinyjs::hide(paste0("textU22",TI1,"_",TI))
            shinyjs::hide(paste0("textU22_1",TI1,"_",TI))
            shinyjs::hide(paste0("textU32",TI1,"_",TI))
            shinyjs::hide(paste0("textU32_1",TI1,"_",TI))
            shinyjs::hide(paste0("textU03",TI1,"_",TI))
            shinyjs::hide(paste0("textU13",TI1,"_",TI))
            shinyjs::hide(paste0("textU13_1",TI1,"_",TI))
            shinyjs::hide(paste0("textU23",TI1,"_",TI))
            shinyjs::hide(paste0("textU23_1",TI1,"_",TI))
            shinyjs::hide(paste0("textU33",TI1,"_",TI))
            shinyjs::hide(paste0("textU33_1",TI1,"_",TI))
            shinyjs::hide(paste0("textU04",TI1,"_",TI))
            shinyjs::hide(paste0("textU14",TI1,"_",TI))
            shinyjs::hide(paste0("textU14_1",TI1,"_",TI))
            shinyjs::hide(paste0("textU24",TI1,"_",TI))
            shinyjs::hide(paste0("textU24_1",TI1,"_",TI))
            shinyjs::hide(paste0("textU34",TI1,"_",TI))
            shinyjs::hide(paste0("textU34_1",TI1,"_",TI))
          } 
            
          
        })
        
        output[[paste0("mapa_shapes1",TI1,"_",TI)]] <- renderLeaflet({
          
          if (!isTRUE(is.element(var,mapa))) {
            shinyjs::hide(paste0("mapa_shapes1",TI1,"_",TI))
            shinyjs::hide(paste0("DPoblacion11_Out",TI1,"_",TI))
            shinyjs::hide(paste0("DPoblacion21_Out",TI1,"_",TI))
            shinyjs::hide(paste0("DPoblacion31_Out",TI1,"_",TI))
            shinyjs::hide(paste0("testmap",TI1,"_",TI))
            hide_loading(elem = "leafletBusy")
            
            
          } else {
            
            y=length(a1)
            
            shapesContexto=unlist(strsplit(DatosVar$ShapesContexto,","))
            NshapesContexto=unlist(strsplit(DatosVar$NombresShapesContexto,","))
            CshapesContexto=unlist(strsplit(DatosVar$ColorShapesContexto,","))
            
            map = leaflet()%>%
              addMapPane("principal", zIndex = 430) %>%  
              addMapPane("puntos", zIndex = 420) %>% 
              addMapPane("lineas", zIndex = 410) %>% 
              addMapPane("poligonos", zIndex = 400) 
            

            #map
            shape =sf::read_sf(dsn=paste0(DatosVar$Shape_Visualizacion))
            Nshape=paste0(DatosVar$Tema_Central," - ", var)
            shape=st_zm(shape, drop = T, what = "ZM")
            colnames(file5)[which(colnames(file5)==DatosVar$ColumnaInformacion)]=DatosVar$CampoShapeUnion
            
            #shape@data[,DatosVar$CampoShapeUnion]=str_replace(shape@data[,DatosVar$CampoShapeUnion], " \\s*\\([^\\)]+\\)", "")
            shape[[DatosVar$CampoShapeUnion]]=gsub(" \\s*\\([^\\)]+\\)", "",shape[[DatosVar$CampoShapeUnion]])
            shape = merge(shape, file5, by=DatosVar$CampoShapeUnion ,all.x=FALSE)
            
            if (DatosVar$Shape_Michart=="BARRAS"){
              
              pal=colorNumeric(DatosVar$Shape_ColoresMinichart, domain = NULL)
              #pal=colorNumeric("Greens", domain = NULL)
              color1=pal(1:(length(a1)+1))[2:(length(a1)+1)]
              
              #d <- as.data.frame(reshape2::melt(as.data.frame(shape),DatosVar$CampoShapeUnion,a2))
              #colnames(d)[1]=DatosVar$ColumnaInformacion
              #d$Name=gsub(paste0("^",DatosVar$LimpiarNombre),"",d$Name, fixed=TRUE)
              #d$Nombre=DatosVar$Nombre
              #d$Unidad=DatosVar$Unidad
              #str(d)  
              #my_popups <- d %>% 
              #  group_by(Name) %>% 
              #  mutate(popup = paste0("<h4>", 
              #                       Name, 
              #                       "</h5><br>",
              #                        paste(variable, ": ", value, " ",Unidad, collapse = "<br>"))) %>% 
              #  pull(popup)
              
              
              map = map %>%
                #basemap %>%
                addMinicharts(
                  st_coordinates(st_centroid(st_geometry(shape)))[,"X"], st_coordinates(st_centroid(st_geometry(shape)))[,"Y"],
                  chartdata = as.data.frame(shape[a2])[,seq(a2)],
                  showLabels = TRUE,
                  type="bar",
                  #layerId = lID ,
                  width = 45,
                  height = 45,
                  colorPalette = color1,
                  layerId = gsub(paste0("^",gsub("\\\\", "-", DatosVar$LimpiarNombre)),"",as.data.frame(shape[DatosVar$CampoShapeUnion])[,DatosVar$CampoShapeUnion], fixed=TRUE),
                  #popup=popupArgs(
                  #  html=my_popups
                  #)
                  #width = 60 * sqrt(d$value) / sqrt(max(d$value))
                )
              
            } else if (DatosVar$Shape_Michart=="PUNTOS"){
              
              pal <- colorNumeric(palette = unlist(strsplit(DatosVar$Colores,",")),domain = NULL)
              
              shape1 =sf::read_sf(dsn=paste0(DatosVar$Shape_Visualizacion))
              shape1=st_zm(shape1, drop = T, what = "ZM")
              colnames(file3)[which(colnames(file3)==DatosVar$ColumnaInformacion)]=DatosVar$CampoShapeUnion
              shape1[[DatosVar$CampoShapeUnion]]=gsub(" \\s*\\([^\\)]+\\)", "",shape1[[DatosVar$CampoShapeUnion]])
              #is.element(unique(file3$Name), shape1$Name)
              shape1 = merge(shape1, file3, by=DatosVar$CampoShapeUnion ,all.y=TRUE)
              shape1$color=pal(shape1$Value)
              shape1$tam=60 * sqrt(shape1$Value) / sqrt(max(shape1$Value))
              shape1$Escenario=factor(shape1$Escenario,levels=a2)
              #Coordenadas = Coordenadas[Coordenadas$Name=="D_Tupiza",]
              shape1 = shape1[order(shape1$Escenario),]
              shape1$X=st_coordinates(st_centroid(st_geometry(shape1)))[,"X"]
              shape1$Y=st_coordinates(st_centroid(st_geometry(shape1)))[,"Y"]
              Coordenadas=shape1 %>%
                st_drop_geometry() %>%
                mutate(id=as.character(row_number()))
              str(Coordenadas)
              
              map = map %>%
                #basemap %>%
                addMinicharts(
                  as.data.frame(shape1["X"])[,"X"], as.data.frame(shape1["Y"])[,"Y"],
                  chartdata = as.data.frame(shape1["Value"])[,"Value"],
                  showLabels = TRUE,
                  #fillColor = Coordenadas$color ,
                  #colorPalette = Coordenadas$color ,
                  layerId = as.data.frame(shape1["NameDef"])[,"NameDef"],
                  time = as.data.frame(shape1["Escenario"])[,"Escenario"], #Coordenadas$Escenario,
                  #width = Coordenadas$tam,
                  #height = Coordenadas$tam
                  #popup=popupArgs(
                  #  html=my_popups
                  #)
                  #width = 60 * sqrt(Coordenadas$Value) / sqrt(max(Coordenadas$Value))
                  
                )
              
            }
            
            #if ((DatosVar$Shape_Michart=="NO")){
             
            for (i in seq(y)){
              
              shape =sf::read_sf(dsn=paste0(DatosVar$Shape_Visualizacion))
              #shape = readOGR(dsn=paste0(DatosVar$Shape_Visualizacion))
              #tabla=as.data.frame(shape)
              Nshape=paste0(DatosVar$Tema_Central," - ", var)
              Tshape=num=as.numeric(gsub("[[:punct:]]","",unlist(strsplit(DatosVar$TamanoMinMax,"-"))))
              #class(shape)[1]
              
              
              colnames(file5)[which(colnames(file5)==DatosVar$ColumnaInformacion)]=DatosVar$CampoShapeUnion
              #shape@data[,DatosVar$CampoShapeUnion]=str_replace(shape@data[,DatosVar$CampoShapeUnion], " \\s*\\([^\\)]+\\)", "")
              shape[[DatosVar$CampoShapeUnion]]=gsub(" \\s*\\([^\\)]+\\)", "",shape[[DatosVar$CampoShapeUnion]])
              
              shape = merge(shape, file5, by=DatosVar$CampoShapeUnion ,all.x=FALSE)
              #tabla=as.data.frame(shape)
              #head(shape@data)
              
              pal <- colorNumeric(palette = unlist(strsplit(DatosVar$Colores,",")),domain = NULL)
              
              #labs <- lapply(seq(nrow(shape)), function(i) {
              #  paste0( 'Variable : ',DatosVar$Nombre, '<p></p>', 
              #          'Elemento : ', shape[i, DatosVar$CampoShapeUnion]@data, '<p></p>', 
              #          'Valor ',DatosVar$Unidad , ': ', shape[i, "Value"]@data, '<p></p>', 
              #          'Tema central : ', DatosVar$Tema_Central,'</p><p>')#,
              #  #'Descripcion : ',DatosVar$Descripcion, '<p></p>')
              #})
              
              labs <- lapply(seq(nrow(shape)), function(j) {
                paste0( 'Escenario: ',a2[i], '<p></p>',
                        'Variable: ',DatosVar$Nombre, '<p></p>', 
                        'Elemento: ', shape[[DatosVar$CampoShapeUnion]][j], '<p></p>', 
                        'Valor ',DatosVar$Unidad , ': ', shape[[a2[i]]][j], '<p></p>', 
                        'Tema central: ', DatosVar$Tema_Central,'</p><p>')#,
                #'Descripcion : ',DatosVar$Descripcion, '<p></p>')
              })
              
              Textlegend=paste0(DatosVar$Nombre," ",DatosVar$Unidad,'<p></p>',
                                "Esc: ",a2[i],'<p></p>')#,
              #'Descripcion : ',DatosVar$Descripcion, '<p></p>')
              
              #names(providers)
              #"SpatialPolygonsDataFrame" "SpatialPointsDataFrame"  "SpatialLinesDataFrame" 
              
              #if (class(shape)[1]=="SpatialPolygonsDataFrame") {
              if (st_geometry_type(shape, by_geometry = FALSE)[1]=="MULTIPOLYGON"| st_geometry_type(shape, by_geometry = FALSE)[1]=="POLYGON"){
                
                shape=st_zm(shape, drop = T, what = "ZM")
                
                map <- map %>%
                  addPolygons(data=shape, stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                              color = "grey", opacity = 1, weight = 0.2,dashArray = "1",
                              #fillColor = ~pal(shape@data$Value),
                              fillColor = ~pal(shape[[a2[i]]]),
                              highlight = highlightOptions(weight = 1,
                                                           color = "black",
                                                           fillOpacity = 0.3,
                                                           bringToFront = TRUE),
                              label = lapply(labs, htmltools::HTML),
                              #label=~paste("Unidad Hidrografica : ", Name,"\n","Area km2 : ", Area_KM2,"\n","Indicador : ", Indicador,"\n","Valor : ", Valor ,"\n","Categoria : ", Categoria),
                              #group =  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(paste0(DatosVar$Shape_Visualizacion)))) %>%  
                              group =  paste0(Nshape," - Escenario:",a2[i]), options = pathOptions(pane = "principal") ) %>%  
                  #addLegend("bottomrigh", pal = pal, values = ~shape@data$Value, #"topleft"
                  addLegend("bottomrigh", pal = pal, values = as.vector(shape[[a2[i]]]),  #"topleft"
                            title = Textlegend, group = paste0(Nshape," - Escenario:",a2[i]), opacity = 1 )
                
                #map
              
                
              } else if (st_geometry_type(shape, by_geometry = FALSE)[1]=="POINT"){
                #else if (class(shape)[1]=="SpatialPointsDataFrame") {
                
                size=round(as.data.frame(Tshape[1]+(Tshape[2]-Tshape[1])/(length(unique(shape[[a2[i]]]))-1)*(0:(length(unique(shape[[a2[i]]]))-1))),1)
                #size=as.data.frame(sort(round(unique(shape[[a2[i]]])^0.4/15,2)))
                
                colnames(size)="size"
                if (DatosVar$Critico =="MAX"){
                  #size$Value=sort(unique(shape@data$Value))
                  size$Value=sort(unique(shape[[a2[i]]]))
                } 
                
                if (DatosVar$Critico =="MIN"){
                  #size$Value=sort(unique(shape@data$Value), decreasing = TRUE)
                  size$Value=sort(unique(shape[[a2[i]]]), decreasing = TRUE)
                } 
                
                colnames(size)[2]=a2[i]
                
                shape = merge(shape, size, by=a2[i],all.x=FALSE)
                #tabla=as.data.frame(shape)
                
                map <- map %>%
                  addCircles(data=as.data.frame(shape),lng = as.data.frame(st_coordinates(shape))[,"X"], lat = as.data.frame(st_coordinates(shape))[,"Y"], weight = 1,
                             #addCircles(lng = ~coords.x1, lat = ~coords.x2, weight = 1,
                             radius = ~size,
                             color = "black",opacity = 1,
                             fillColor = pal(as.vector(shape[[a2[i]]])),fillOpacity = 1,
                             highlight = highlightOptions(weight = 1,
                                                          color = "black",
                                                          fillOpacity = 0.5,
                                                          bringToFront = TRUE),
                             label = lapply(labs, htmltools::HTML),
                             #group =  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(paste0(DatosVar$Shape_Visualizacion)))) %>%
                             group =  paste0(Nshape," - Escenario:",a2[i]), options = pathOptions(pane = "principal") ) %>%  
                  addLegend("bottomrigh", pal = pal, values = as.vector(shape[[a2[i]]]),  #"topleft"
                            title = Textlegend, group = paste0(Nshape," - Escenario:",a2[i]), opacity = 1 )
                #map
              } else if (st_geometry_type(shape, by_geometry = FALSE)[1]=="LINESTRING"){
                #else if ((class(shape)[1]=="SpatialLinesDataFrame") ){
                
                #size=round(as.data.frame(Tshape[1]+(Tshape[2]-Tshape[1])/(length(unique(shape@data$Value))-1)*(0:(length(unique(shape@data$Value))-1))),1)
                size=round(as.data.frame(Tshape[1]+(Tshape[2]-Tshape[1])/(length(unique(shape[[a2[i]]]))-1)*(0:(length(unique(shape[[a2[i]]]))-1))),1)
                colnames(size)="size"
                if (DatosVar$Critico =="MAX"){
                  #size$Value=sort(unique(shape@data$Value))
                  size$Value=sort(unique(shape[[a2[i]]]))
                } 
                
                if (DatosVar$Critico =="MIN"){
                  #size$Value=sort(unique(shape@data$Value), decreasing = TRUE)
                  size$Value=sort(unique(shape[[a2[i]]]), decreasing = TRUE)
                } 
                colnames(size)[2]=a2[i]
                
                shape = merge(shape, size, by=a2[i],all.x=FALSE)
                #tabla=as.data.frame(shape)
                #sort(tabla$size)
                shape=st_zm(shape, drop = T, what = "ZM")
                map <- map %>%
                  addPolylines(data=shape, stroke = TRUE, smoothFactor = 1, fillOpacity = 1,
                               #opacity = 1, weight = shape@data$size,dashArray = "1",
                               opacity = 1, weight = shape[["size"]],dashArray = "1",
                               #color = ~pal(shape@data$Value),
                               color = pal(shape[[a2[i]]]),
                               highlight = highlightOptions(weight = 3,
                                                            color = "black",
                                                            fillOpacity = 1,
                                                            bringToFront = TRUE),
                               label = lapply(labs, htmltools::HTML),
                               #label=~paste("Unidad Hidrografica : ", Name,"\n","Area km2 : ", Area_KM2,"\n","Indicador : ", Indicador,"\n","Valor : ", Valor ,"\n","Categoria : ", Categoria),
                               #group =  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(paste0(DatosVar$Shape_Visualizacion)))) %>% 
                               group =  paste0(Nshape," - Escenario:",a2[i]), options = pathOptions(pane = "principal") ) %>%  
                  #addLegend("bottomrigh", pal = pal, values = ~shape@data$Value, #"topleft"
                  addLegend("bottomrigh", pal = pal, values = as.vector(shape[[a2[i]]]),  #"topleft"
                            title = Textlegend, group = paste0(Nshape," - Escenario:",a2[i]), opacity = 1 )
                
                
              }
              
              #map
              
            }
             
            #   }
            
            for (i in 1:length(NshapesContexto)) {
              shp=read_sf(dsn=paste0(shapesContexto[i]))
              # shp <- readOGR(shapesContexto[i])
              
              if (st_geometry_type(shp, by_geometry = FALSE)[1]=="MULTIPOLYGON"| st_geometry_type(shp, by_geometry = FALSE)[1]=="POLYGON"){
                #if (class(shp)[1]=="SpatialPolygonsDataFrame") {
                shp=st_zm(shp, drop = T, what = "ZM")
                map <- map %>%
                  addPolygons(data=shp, group =  NshapesContexto[i], weight = 0.5, 
                              color = CshapesContexto[i] , options = pathOptions(pane = "poligonos"))
                #map
              } else if (st_geometry_type(shp, by_geometry = FALSE)[1]=="POINT"){
                #} else if (class(shp)[1]=="SpatialPointsDataFrame") {
                map <- map %>%
                  addCircles(data=as.data.frame(shp),lng = as.data.frame(st_coordinates(shp))[,"X"], lat = as.data.frame(st_coordinates(shp))[,"Y"], 
                             radius = 500, weight = 0.5, group =NshapesContexto[i], 
                             color = CshapesContexto[i] , options = pathOptions(pane = "puntos"))
                #map
              } else if (st_geometry_type(shp, by_geometry = FALSE)[1]=="LINESTRING"){
                #} else if ((class(shp)[1]=="SpatialLinesDataFrame") ){
                shp=st_zm(shp, drop = T, what = "ZM")
                map <- map %>%
                  addPolylines(data=shp, group =  NshapesContexto[i], weight = 1, 
                               color = CshapesContexto[i] , options = pathOptions(pane = "lineas"))
                #map
              }
            }
            
            map = map %>% 
              addTiles(group = "OpenStreetMap") %>%
              addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
              addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorld") %>%
              addProviderTiles(providers$Esri.WorldPhysical, group = "Esri.WorldPhysical") %>%  
              addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery")              %>%
              # Layers control
              addLayersControl(
                #position = c("topright", "bottomright", "bottomleft", "topleft"),
                position = "topleft",
                baseGroups = c("OpenStreetMap", "Toner", "Esri.NatGeoWorld","Esri.WorldPhysical","Esri.WorldImagery"),
                overlayGroups = c(paste0(Nshape," - Escenario:",a2[seq(y)]), NshapesContexto),
                options = layersControlOptions(collapsed = TRUE)
              ) 
            
            
            #tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
            #basemap <- leaflet() %>%
            #  addTiles(tilesURL)

            
            # hideGroup("Rios en WEAP") %>%
            #addControl(unique( Indicadores$Indicador)[i], position = "bottomleft") #, className="map-title"
            coor=as.vector(extent(shape))
            #setView(map, mean(coor[1],coor[2]),mean(coor[3],coor[4]),zoom=8)
            fitBounds(map, coor[1], coor[3], coor[2], coor[4])
            
            #map
            
            return(map)
            
            # hide loading elem and return map
            #Sys.sleep(1)
            #hide_loading(elem = "leafletBusy")
            
            
          }
        })
        
        output[[paste0("mapa_WASH",TI1,"_",TI)]] <- renderLeaflet({
         
           if (!isTRUE(is.element(var,mapaWASH))) {
            shinyjs::hide(paste0("mapa_WASH",TI1,"_",TI))
            
          } else {
            
            y=length(a1)
            
            shapesContexto=unlist(strsplit(DatosVar$ShapesContextoWASH,","))
            NshapesContexto=unlist(strsplit(DatosVar$NombresShapesContextoWASH,","))
            CshapesContexto=unlist(strsplit(DatosVar$ColorShapesContextoWASH,","))
            
            file=fileT[,c(DatosVar$ColumnaInformacion,"Categoria","Value", "Escenario","NameDef")]
            file = file %>% pivot_wider(names_from =c("Categoria") , values_from = "Value")
            file=as.data.frame(file)
            
            ###
            
            map = leaflet()%>%
              addMapPane("principal", zIndex = 430) %>%  
              addMapPane("puntos", zIndex = 420) %>% 
              addMapPane("lineas", zIndex = 410) %>% 
              addMapPane("poligonos", zIndex = 400) 
            
              colnames(file)[which(colnames(file)==DatosVar$ColumnaInformacion)]=DatosVar$CampoShapeUnionWASH
              shape =sf::read_sf(dsn=paste0(DatosVar$Shape_WASH))
              shape=st_zm(shape, drop = T, what = "ZM")
              shape[[DatosVar$CampoShapeUnionWASH]]=gsub(" \\s*\\([^\\)]+\\)", "",shape[[DatosVar$CampoShapeUnionWASH]])
              #shape = readOGR(dsn=paste0(DatosVar$Shape_Visualizacion))
              #tabla=as.data.frame(shape)
              Nshape=paste0(DatosVar$Tema_Central," - ", var)
              shape = merge(shape, file[file$Escenario==unique(file$Escenario)[1],], by=DatosVar$CampoShapeUnionWASH ,all.x=FALSE)
              
              Textlegend=paste0(DatosVar$Nombre," ",DatosVar$Unidad)
              
              #names(providers)
              #"SpatialPolygonsDataFrame" "SpatialPointsDataFrame"  "SpatialLinesDataFrame" 
              
              #if (class(shape)[1]=="SpatialPolygonsDataFrame") {
              if (st_geometry_type(shape, by_geometry = FALSE)[1]=="MULTIPOLYGON"| st_geometry_type(shape, by_geometry = FALSE)[1]=="POLYGON"){
                
                map <- map %>%
                  addPolygons(data=shape, stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                              color = "green", opacity = 1, weight = 0.2,dashArray = "1",
                              group =  paste0(Nshape), options = pathOptions(pane = "principal") )
                
                #map
                
              } else if (st_geometry_type(shape, by_geometry = FALSE)[1]=="POINT"){
                #else if (class(shape)[1]=="SpatialPointsDataFrame") {
                shape=st_zm(shape, drop = T, what = "ZM")
                 map <- map %>%
                  addCircles(data=as.data.frame(shape),lng = as.data.frame(st_coordinates(shape))[,"X"], lat = as.data.frame(st_coordinates(shape))[,"Y"], weight = 1,
                             #addCircles(lng = ~coords.x1, lat = ~coords.x2, weight = 1,
                             color = "green",opacity = 1,
                             fillOpacity = 1,
                             group =  paste0(Nshape), options = pathOptions(pane = "principal") ) 
                #map
              }
              
                coords=data.frame(as.vector(shape[[DatosVar$CampoShapeUnionWASH]]),as.vector(st_coordinates(st_centroid(st_geometry(shape)))[,"X"]),as.vector(st_coordinates(st_centroid(st_geometry(shape)))[,"Y"]))
                colnames(coords)=c(DatosVar$ColumnaInformacion,"X","Y")
                Coordenadas=NULL
                Coordenadas=as.data.frame(unique(file[,DatosVar$ColumnaInformacion]))
                colnames(Coordenadas)=DatosVar$ColumnaInformacion
                Coordenadas=merge(Coordenadas,coords, by=DatosVar$ColumnaInformacion,all.x=FALSE)
                Coordenadas=merge(Coordenadas,file, by=DatosVar$ColumnaInformacion)
                Coordenadas$Escenario=factor(Coordenadas$Escenario,levels=a2)
                Coordenadas=merge(Coordenadas,file1, by=c("NameDef","Escenario"))
                Coordenadas$tam=60 * sqrt(Coordenadas$Value) / sqrt(max(Coordenadas$Value))
                
                #lID=paste0("Barras Escenarios: ", Nshape)
                map = map %>%
                  #basemap %>%
                  addMinicharts(
                    Coordenadas$X, Coordenadas$Y,
                    chartdata = Coordenadas[,unique(fileT$Categoria)],
                    showLabels = TRUE,
                    layerId = Coordenadas$NameDef,
                    type="pie",
                    colorPalette = unlist(strsplit(DatosVar$Colores_CategoriasApiladas,",")),
                    time = Coordenadas$Escenario,
                    #layerId = lID ,
                    width = Coordenadas$tam,
                    height = Coordenadas$tam
                  )

            
            for (i in 1:length(NshapesContexto)) {
              shp=read_sf(dsn=paste0(shapesContexto[i]))
              # shp <- readOGR(shapesContexto[i])
              
              if (st_geometry_type(shp, by_geometry = FALSE)[1]=="MULTIPOLYGON"| st_geometry_type(shp, by_geometry = FALSE)[1]=="POLYGON"){
                #if (class(shp)[1]=="SpatialPolygonsDataFrame") {
                shp=st_zm(shp, drop = T, what = "ZM")
                map <- map %>%
                  addPolygons(data=shp, group =  NshapesContexto[i], weight = 0.5, 
                              color = CshapesContexto[i] , options = pathOptions(pane = "poligonos"))
                #map
              } else if (st_geometry_type(shp, by_geometry = FALSE)[1]=="POINT"){
                #} else if (class(shp)[1]=="SpatialPointsDataFrame") {
                map <- map %>%
                  addCircles(data=as.data.frame(shp),lng = as.data.frame(st_coordinates(shp))[,"X"], lat = as.data.frame(st_coordinates(shp))[,"Y"], 
                             radius = 500, weight = 0.5, group =NshapesContexto[i], 
                             color = CshapesContexto[i] , options = pathOptions(pane = "puntos"))
                #map
              } else if (st_geometry_type(shp, by_geometry = FALSE)[1]=="LINESTRING"){
                #} else if ((class(shp)[1]=="SpatialLinesDataFrame") ){
                shp=st_zm(shp, drop = T, what = "ZM")
                map <- map %>%
                  addPolylines(data=shp, group =  NshapesContexto[i], weight = 1, 
                               color = CshapesContexto[i] , options = pathOptions(pane = "lineas"))
                #map
              }
            }
            
            map = map %>% 
              addTiles(group = "OpenStreetMap") %>%
              addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
              addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorld") %>%
              addProviderTiles(providers$Esri.WorldPhysical, group = "Esri.WorldPhysical") %>%  
              addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery")              %>%
              # Layers control
              addLayersControl(
                #position = c("topright", "bottomright", "bottomleft", "topleft"),
                position = "topleft",
                baseGroups = c("OpenStreetMap", "Toner", "Esri.NatGeoWorld","Esri.WorldPhysical","Esri.WorldImagery"),
                overlayGroups = c(Nshape, NshapesContexto),
                options = layersControlOptions(collapsed = TRUE)
              ) 
            
            
            #tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
            #basemap <- leaflet() %>%
            #  addTiles(tilesURL)
            
            
            # hideGroup("Rios en WEAP") %>%
            #addControl(unique( Indicadores$Indicador)[i], position = "bottomleft") #, className="map-title"
            coor=as.vector(extent(shape))
            #setView(map, mean(coor[1],coor[2]),mean(coor[3],coor[4]),zoom=8)
            fitBounds(map, coor[1], coor[3], coor[2], coor[4])
            
            #map
            
            return(map)
            
            # hide loading elem and return map
            #Sys.sleep(1)
            #hide_loading(elem = "leafletBusy")
            
            
          }
        })
        
        output[[paste0("graficoWaTT",TI1,"_",TI)]]= renderPlotly({
          
          if (!isTRUE(is.element(input[[paste0("out_",TI,"_",TI1)]],compuesto))) {
            shinyjs::hide(paste0("graficoWaTT",TI1,"_",TI))
            
          } 
          
        })
        
        output[[paste0("tabla",TI1,"_",TI)]] = renderDataTable({
          
          if (!isTRUE(is.element(input[[paste0("out_",TI,"_",TI1)]],tabla))) {
            shinyjs::hide(paste0("tabla",TI1,"_",TI)) 
          } else {

            file4
          }
        },rownames = FALSE, options = list(searching = TRUE,scrollX = T))
      })
    })
  })
  
  ##################################
  
  #Mapas
  ##################################
  
  observeEvent(input$ActualizarRuns1,{ 
    runsScript=unique(substring(list.files(pattern = "_Indicadores.csv$"), 1,1))
    updateSelectInput(session, "Nrun1",
                      label = "Seleccione el numero de corrida" ,
                      choices = runsScript, runsScript[length(runsScript)])
  })
  
  observeEvent(input$test1, {
    shinyjs::toggle(id= "panel1")
  })
  
  output$TableRun1 = DT::renderDataTable({
    acciones=read.csv(paste0(FolderAcciones,"Lista acciones.csv"), check.names = F, stringsAsFactors = F,header = T)
    #colnames(acciones)[1] <- gsub('^...','',colnames(acciones)[1])
    
    acciones=subset(acciones,acciones$N_Corrida==input$Nrun1)
    t_acciones=transpose(acciones)
    t_acciones$Accion=colnames(acciones)
    colnames(t_acciones) <- c("Expresion","Accion")
    t_acciones[,c(2,1)]
  },rownames = FALSE, options = list(searching = TRUE, pageLength = 10,scrollX = T))
  
  output$mapaTodos <- renderLeaflet({
    
    # simulate building
    show_loading(elem = "leafletBusy")
    
    indicadores=unique(subset(InvIndicadores,InvIndicadores$Visualizacion_Mapa=="SI" & InvIndicadores$Fuente_Informacion == "Script Indicadores","Nombre"))
    indicadores=as.vector(t(indicadores))
    #indicadores=as.vector(t(indicadores))[c(1,19,5,40)]
    
    #indicadores=NULL
    #for (i in 1:nrow(unique(subset(InvIndicadores,InvIndicadores$Visualizacion_Mapa=="SI","Tema_Central")))) {
    #  indicadores = c(indicadores, input[[paste0(unique(InvIndicadores$Tema_Central)[i])]])
    #}
    
    InvIndicadores1=subset(InvIndicadores,InvIndicadores$Visualizacion_Mapa=="SI" & InvIndicadores$Fuente_Informacion == "Script Indicadores")
    temas=unique(as.vector(InvIndicadores1$Tema_Central))
    InvIndicadores1$N=NA
    for (i in 1:length(temas)){
      InvIndicadores1$N[which(InvIndicadores1$Tema_Central==temas[i])]=1:length(which(InvIndicadores1$Tema_Central==temas[i]))
    }
    indicadoreshide=paste0(InvIndicadores1$Tema_Central[which(InvIndicadores1$N!=1)]," - ",InvIndicadores1$Nombre[which(InvIndicadores1$N!=1)])
    
    shapesContexto=NULL
    NshapesContexto=NULL
    CshapesContexto=NULL
    for (z in 1:length(indicadores)) {
      var=indicadores[z]
      DatosVar=subset(InvIndicadores,InvIndicadores$Nombre==var)
      shapesContexto=c(shapesContexto,unlist(strsplit(DatosVar$ShapesContexto,",")))
      NshapesContexto=c(NshapesContexto,unlist(strsplit(DatosVar$NombresShapesContexto,",")))
      CshapesContexto=c(CshapesContexto,unlist(strsplit(DatosVar$ColorShapesContexto,",")))
    }
    shapes=as.data.frame(unique(cbind(shapesContexto,NshapesContexto,CshapesContexto)))
    shapesContexto=shapes$shapesContexto
    NshapesContexto=shapes$NshapesContexto
    CshapesContexto=shapes$CshapesContexto
    
    map = leaflet()%>%
      addMapPane("principalpuntos", zIndex = 450) %>%
      addMapPane("principallineas", zIndex = 440) %>% 
      addMapPane("principalpoligonos", zIndex = 430) %>% 
      addMapPane("puntos", zIndex = 420) %>% 
      addMapPane("lineas", zIndex = 410) %>% 
      addMapPane("poligonos", zIndex = 400) 
    
    
    for (i in 1:length(NshapesContexto)) {
      shp=read_sf(dsn=paste0(shapesContexto[i]))
      # shp <- readOGR(shapesContexto[i])
      
      if (st_geometry_type(shp, by_geometry = FALSE)[1]=="MULTIPOLYGON" | st_geometry_type(shp, by_geometry = FALSE)[1]=="POLYGON"){
        #if (class(shp)[1]=="SpatialPolygonsDataFrame") {
        shp=st_zm(shp, drop = T, what = "ZM")
        map <- map %>%
          addPolygons(data=shp, group =  NshapesContexto[i], weight = 0.5, 
                      color = CshapesContexto[i] , options = pathOptions(pane = "poligonos"))
        #map
      } else if (st_geometry_type(shp, by_geometry = FALSE)[1]=="POINT"){
        #} else if (class(shp)[1]=="SpatialPointsDataFrame") {
        map <- map %>%
          addCircles(data=as.data.frame(shp),lng = as.data.frame(st_coordinates(shp))[,"X"], lat = as.data.frame(st_coordinates(shp))[,"Y"], 
                     radius = 500, weight = 0.5, group =NshapesContexto[i], 
                     color = CshapesContexto[i] , options = pathOptions(pane = "puntos"))
        #map
      } else if (st_geometry_type(shp, by_geometry = FALSE)[1]=="LINESTRING"){
        #} else if ((class(shp)[1]=="SpatialLinesDataFrame") ){
        shp=st_zm(shp, drop = T, what = "ZM")
        map <- map %>%
          addPolylines(data=shp, group =  NshapesContexto[i], weight = 1, 
                       color = CshapesContexto[i] , options = pathOptions(pane = "lineas"))
      }
    }
    #map
    
    #Nrun=1
    Nrun= input$Nrun1
    
    file=NULL
    for (k in 1:length(a1)){
      f1=read.csv(paste0(Nrun,"_",a1[k],"_Indicadores.csv"), check.names = F, stringsAsFactors = F,header = T)
      #colnames(f1)[1] = gsub('^...','',colnames(f1)[1])
      f1$Nrun=k
      f1$Escenario=a2[k]
      file=rbind(file,f1)
    }
    
    remove(f1)
    
    fileT = file 
    #py=unique(fileT$Escenario)[1]
    py=input$yearpop1
    fileT=subset(fileT,fileT$Escenario==py)
    #z=1
    
    for (z in 1:length(indicadores)) {
      #var="Poblacion_DemandaInsatisfecha-Media" #"ExtraccionTransmision-Media"   #"ExtraccionDiversion-Media" # "Flujo de retorno-Media" #"Poblacion_DemandaInsatisfecha-Media" #"Poblacion" #"Irrigacion_AreaRiego" #Area_" #"Caudal-Media"  #"Poblacion" 
      var=indicadores[z]
      #is.element(var,mapa)==TRUE
      DatosVar=subset(InvIndicadores,InvIndicadores$Nombre==var)
      
      file=subset(fileT,fileT$Prefix==var)
      file$NameDef=gsub(paste0("^",gsub("\\\\", "-", DatosVar$LimpiarNombre)),"",gsub("\\\\", "-",file[,DatosVar$ColumnaInformacion]))
      
      file2=subset(file,file$Prefix==var & file$Escenario==py, c("NameDef", "Value"))
      file2=unique(as.vector(file2[order(file2$Value, decreasing = TRUE),"NameDef"]))
      #file[,DatosVar$ColumnaInformacion] = factor(file[,DatosVar$ColumnaInformacion], labels=file2, levels = unique(file[,DatosVar$ColumnaInformacion]))
      file$NameDef = factor(file$NameDef, levels = file2)
      if (!is.na(DatosVar$RedondearDecimales)){
        file$Value=round(file$Value,DatosVar$RedondearDecimales)
      }
      remove(file2)
        
      #str(file)
        
      if (!is.na(DatosVar$RedondearDecimales)){
        file$Value=round(file$Value,DatosVar$RedondearDecimales)
      }
        
      file3=file
        
      file5=file[,c(DatosVar$ColumnaInformacion,"Value", "Escenario", "NameDef")]
      file5 = file5 %>% pivot_wider(names_from ="Escenario" , values_from = "Value")
      file5=as.data.frame(file5)
      #str(file)

      shape = read_sf(dsn=paste0(DatosVar$Shape_Visualizacion),stringsAsFactors = FALSE)
      # shape = readOGR(dsn=paste0(DatosVar$Shape_Visualizacion))
      #tabla=as.data.frame(shape)
      Nshape=paste0(DatosVar$Tema_Central," - ", var)
      Tshape=num=as.numeric(gsub("[[:punct:]]","",unlist(strsplit(DatosVar$TamanoMinMax,"-"))))
      #class(shape)[1]
      #tabla=as.data.frame(shape)
      
      colnames(file5)[which(colnames(file5)==DatosVar$ColumnaInformacion)]=DatosVar$CampoShapeUnion
      #sort(file[,DatosVar$CampoShapeUnion])
      #sort(shape@data[,DatosVar$CampoShapeUnion])
      #is.element(sort(shape@data[,DatosVar$CampoShapeUnion]),sort(shape@data[,DatosVar$CampoShapeUnion]))
      #shape@data[,DatosVar$CampoShapeUnion]=str_replace(shape@data[,DatosVar$CampoShapeUnion], " \\s*\\([^\\)]+\\)", "")
      shape[[DatosVar$CampoShapeUnion]]=gsub(" \\s*\\([^\\)]+\\)", "",shape[[DatosVar$CampoShapeUnion]])
      #sort(shape@data[,DatosVar$CampoShapeUnion])
      #is.element(sort(file[,DatosVar$CampoShapeUnion]),sort(shape@data[,DatosVar$CampoShapeUnion]))
      
      shape = merge(shape, file5, by=DatosVar$CampoShapeUnion ,all.x=FALSE)
      #tabla=as.data.frame(shape)
      #tabla
      #head(shape@data)
      #str(shape)
      #unique(shape$Prefix)
      #pal <- colorNumeric(palette = "",domain = NULL)
      pal <- colorNumeric(palette = unlist(strsplit(DatosVar$Colores,",")),domain = NULL)
      #pal(log(sort(shape@data$Value)))
      #pal(sort(shape@data$Value))
      
      #labs <- lapply(seq(nrow(shape)), function(i) {
      #  paste0( 'Variable : ',DatosVar$Nombre, '<p></p>', 
      #          'Elemento : ', shape[i, DatosVar$CampoShapeUnion]@data, '<p></p>', 
      #          'Valor ',DatosVar$Unidad , ': ', shape[i, "Value"]@data, '<p></p>', 
      #          'Tema central : ', DatosVar$Tema_Central,'</p><p>')#,
      #  #'Descripcion : ',DatosVar$Descripcion, '<p></p>')
      #})
      
      labs <- lapply(seq(nrow(shape)), function(j) {
        paste0( 'Escenario: ',py, '<p></p>',
                'Variable: ',DatosVar$Nombre, '<p></p>', 
                'Elemento: ', shape[["NameDef"]][j], '<p></p>', 
                'Valor ',DatosVar$Unidad , ': ', shape[[py]][j], '<p></p>', 
                'Tema central: ', DatosVar$Tema_Central,'</p><p>')#,
        #'Descripcion : ',DatosVar$Descripcion, '<p></p>')
      })
      
      Textlegend=paste0(DatosVar$Nombre," ",DatosVar$Unidad,'<p></p>',
                        "Esc: ",py,'<p></p>')#,
      
      
      #names(providers)
      #"SpatialPolygonsDataFrame" "SpatialPointsDataFrame"  "SpatialLinesDataFrame" 
      
      #if (class(shape)[1]=="SpatialPolygonsDataFrame") {
      if (st_geometry_type(shape, by_geometry = FALSE)[1]=="MULTIPOLYGON" | st_geometry_type(shape, by_geometry = FALSE)[1]=="POLYGON"){
        shape=st_zm(shape, drop = T, what = "ZM")
        map = map %>% 
          addPolygons(data=shape, stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                      color = "grey", opacity = 1, weight = 0.2,dashArray = "1",
                      #fillColor = ~pal(shape@data$Value),
                      fillColor = pal(as.vector(shape[[py]])) ,
                      highlight = highlightOptions(weight = 1,
                                                   color = "black",
                                                   fillOpacity = 0.3,
                                                   bringToFront = TRUE),
                      label = lapply(labs, htmltools::HTML),
                      #label=~paste("Unidad Hidrografica : ", Name,"\n","Area km2 : ", Area_KM2,"\n","Indicador : ", Indicador,"\n","Valor : ", Valor ,"\n","Categoria : ", Categoria),
                      #group =  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(paste0(DatosVar$Shape_Visualizacion)))) %>%  
                      group =  Nshape, options = pathOptions(pane = "principalpoligonos") ) %>%  
          #addLegend("bottomrigh", pal = pal, values = ~shape@data$Value, #"topleft"
          addLegend("bottomrigh", pal = pal, values = shape[[py]], #"topleft"
                    title = Textlegend, group = Nshape, opacity = 1 )
        
        #labFormat = labelFormat(prefix = "$"), 
        
        #map
      } else if (st_geometry_type(shape, by_geometry = FALSE)[1]=="POINT"){
        #else if (class(shape)[1]=="SpatialPointsDataFrame") {
        
        #size=round(as.data.frame(Tshape[1]+(Tshape[2]-Tshape[1])/(length(unique(shape@data$Value))-1)*(0:(length(unique(shape@data$Value))-1))),1)
        size=round(as.data.frame(Tshape[1]+(Tshape[2]-Tshape[1])/(length(unique(shape[[py]]))-1)*(0:(length(unique(shape[[py]]))-1))),1)
        colnames(size)="size"
        if (DatosVar$Critico =="MAX"){
          #size$Value=sort(unique(shape@data$Value))
          size$Value=sort(unique(shape[[py]]))
        } 
        
        if (DatosVar$Critico =="MIN"){
          #size$Value=sort(unique(shape@data$Value), decreasing = TRUE)
          size$Value=sort(unique(shape[[py]]), decreasing = TRUE)
        } 
        
        colnames(size)[2]=py
        
        shape = merge(shape, size, by=py,all.x=FALSE)
        #tabla=as.data.frame(shape)
        map = map %>%
          addCircles(data=as.data.frame(shape), lng = as.data.frame(st_coordinates(shape))[,"X"], lat = as.data.frame(st_coordinates(shape))[,"Y"], weight = 1,
                     radius = ~size,
                     color = "black",opacity = 1,
                     fillColor = pal(as.vector(shape[[py]])),fillOpacity = 1,
                     highlight = highlightOptions(weight = 1,
                                                  color = "black",
                                                  fillOpacity = 0.5,
                                                  bringToFront = TRUE),
                     label = lapply(labs, htmltools::HTML),
                     #group =  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(paste0(DatosVar$Shape_Visualizacion)))) %>%
                     group =  Nshape , options = pathOptions(pane = "principalpuntos")) %>% 
          addLegend(pal = pal, values = as.vector(shape[[py]]), 
                    title =Textlegend, group = Nshape, opacity = 1 )
        #labFormat = labelFormat(prefix = "$"),
        
        #map1
      } else if (st_geometry_type(shape, by_geometry = FALSE)[1]=="LINESTRING"){
        #else if ((class(shape)[1]=="SpatialLinesDataFrame") ){
        
        #size=round(as.data.frame(Tshape[1]+(Tshape[2]-Tshape[1])/(length(unique(shape@data$Value))-1)*(0:(length(unique(shape@data$Value))-1))),1)
        size=round(as.data.frame(Tshape[1]+(Tshape[2]-Tshape[1])/(length(unique(shape[[py]]))-1)*(0:(length(unique(shape[[py]]))-1))),1)
        colnames(size)="size"
        if (DatosVar$Critico =="MAX"){
          #size$Value=sort(unique(shape@data$Value))
          size$Value=sort(unique(shape[[py]]))
        } 
        
        if (DatosVar$Critico =="MIN"){
          #size$Value=sort(unique(shape@data$Value), decreasing = TRUE)
          size$Value=sort(unique(shape[[py]]), decreasing = TRUE)
        } 
        
        colnames(size)[2]=py
        
        shape = merge(shape, size, by=py,all.x=FALSE)
        shape=st_zm(shape, drop = T, what = "ZM")
        #tabla=as.data.frame(shape)
        #sort(tabla$size)
        map = map %>% 
          addPolylines(data=shape, stroke = TRUE, smoothFactor = 1, fillOpacity = 1,
                       opacity = 1, weight = shape[["size"]],dashArray = "1",
                       color = pal(as.vector(shape[[py]])),
                       highlight = highlightOptions(weight = 3,
                                                    color = "black",
                                                    fillOpacity = 1,
                                                    bringToFront = TRUE),
                       label = lapply(labs, htmltools::HTML),
                       #label=~paste("Unidad Hidrografica : ", Name,"\n","Area km2 : ", Area_KM2,"\n","Indicador : ", Indicador,"\n","Valor : ", Valor ,"\n","Categoria : ", Categoria),
                       #group =  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(paste0(DatosVar$Shape_Visualizacion)))) %>% 
                       group =  Nshape , options = pathOptions(pane = "principallineas") )%>% 
          addLegend(pal = pal, values = as.vector(shape[[py]]), 
                    title = Textlegend, group = Nshape, opacity = 1 )
        #labFormat = labelFormat(prefix = "$"),
        
        
      }
      
    }  
    #map
    Capas=subset(InvIndicadores, InvIndicadores$Visualizacion_Mapa=="SI",c("Tema_Central", "Nombre"))
    Capas=paste0(Capas$Tema_Central," - ", Capas$Nombre)
    
    map = map %>% 
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorld") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Esri.WorldPhysical") %>%  
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery")              %>%
      # Layers control
      addLayersControl(
        #position = c("topright", "bottomright", "bottomleft", "topleft"),
        position = "topleft",
        baseGroups = c("OpenStreetMap", "Toner", "Esri.NatGeoWorld","Esri.WorldPhysical","Esri.WorldImagery"),
        overlayGroups = c(Capas, NshapesContexto),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(indicadoreshide) %>%
      addMiniMap(toggleDisplay = TRUE, position = "bottomright")
    
    #addControl(unique( Indicadores$Indicador)[i], position = "bottomleft") #, className="map-title"
    coor=as.vector(extent(shape))
    #setView(map, mean(coor[1],coor[2]),mean(coor[3],coor[4]),zoom=8)
    fitBounds(map, coor[1], coor[3], coor[2], coor[4])
    print("done")
    
    return(map)
    # hide loading elem and return map
    Sys.sleep(1)
    hide_loading(elem = "leafletBusy")
    
    
  })
  
  
  ##################################
}

shinyApp(ui, server)

