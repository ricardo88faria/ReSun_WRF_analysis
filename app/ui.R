
# for shinyapps.io, use theme="spacelab.css" with file in www folder.
# for local/RStudio and shiny-server, use theme="http://bootswatch.com/spacelab/bootstrap.css" (this is ignored on shinyapps.io)
# shinytheme() from shinythemes package must be avoided because it conflicts with bsModal in shinyBS.

shinyUI(navbarPage(theme="http://bootswatch.com/spacelab/bootstrap.css", inverse=F,
                   title=div(img(src="img/LREC.png", height = 30, width = 50),
                             img(src="img/ReSun.jpg", height = 30, width = 70),
                             "Atlas da Radiação Solar"
                   ),
                   windowTitle="Atlas da Radiação Solar",
                   collapsible=TRUE,
                   id="nav",
                   position = "static-top",
                   tabPanel("Mapa Solar", value="vis",
                            div(class="outer",
                                #position = "static-top",
                                tags$head(includeCSS("www/styles.css")),
                                #tags$style("html, body {width:100%;height:100%}"),
                                leafletOutput("Map", width="100%", height="100%"),
                                #absolutePanel(top=20, left=60, height=20, width=600, h4("Northwest Territories Future Climate Outlook")),
                                absolutePanel(#h2("ZIP explorer"),
                                  top=10, right=10, draggable = F,
                                  sliderInput("date", "Mês", min=min(seq_time), max=max(seq_time), value=seq_time[1], step=1, sep="", #format="## Months", timeFormat = "%B",
                                              animate = animationOptions(interval = 2500, loop = F)), # , post=" Mês"  , playButton = "PLAY", pauseButton = "PAUSA"
                                  #checkboxInput("legend", "Legenda adaptada para valores anuais (desabilitar para maior gama valores)", TRUE),
                                  checkboxInput("units", "Mudar unidades para [Wh/m^2.dia]", FALSE), 
                                  checkboxInput("addMarker", "Adicionar marcador ao clicar no mapa."),
                                  #textInput("userlat", "Latitude do ponto em graus decimais:", "32.648904"),
                                  #textInput("userlong", "longitude  do ponto em graus decimais:", "-16.940765"),
                                  #actionButton("coordsinsert", "inserir coordenadas para criar marcador"),
                                  actionButton("clearMarkers", "Limpar marcadores"),
                                  sliderInput("opac", "Opacitade do Raster", min=0, max=1, value=.8, step=.1, sep=""), # para opacidade
                                  checkboxInput("EMAS", "Mostrar EMAS e pontos do Utilizador", TRUE),
                                  conditionalPanel("input.EMAS == true",
                                                   selectInput("location", "Localização das EMAS", c("", levels(locs$loc)), selected=""),
                                                   conditionalPanel("input.location !== null && input.location !== ''",
                                                                    actionButton("button_plot_and_table", "Ver Gráfico/Tabela da EMA", class="btn-block"))),
                                  #actionButton("about", "Sobre", class="btn-block"),
                                  
                                  style = "opacity: 0.9"
                                  #    img(src="MJi_clean.png", height = 70, width = 100)
                                ),
                                
                                absolutePanel(bottom = 0, right = 10, width = 700, height = "auto", draggable = F, fixed = TRUE,
                                              bsCollapse(id = "collapseExample", open = NULL,
                                                         bsCollapsePanel("Notas:", 
                                                                         "1- O intante Mês 0, corresponde à média anual do TMY corrigido.
                                                                         2- Os valores mensais são representados sem correção, uma vez que a correção foi aplicada à média anual.
                                                                         3- Os meses que compõem o Ano Meteorológico Tipico a simular com o ReSun tiveram por base 10 anos medidos na sua estimativa.
                                                                         4- EMAS (Estações Meteorológicas Automáticas).
                                                                         5- Clique num ponto no mapa para ver mais detalhes sobre esse ponto.",
                                                                         style = "info", 
                                                                         actionButton("about", "Sobre", class="btn-block"))
                                              ), 
                                              style = "opacity: 0.9"
                                )),
                            
                            bsModal("abouttext", "Sobre o Atlas Solar", "about",    
                                    wellPanel(     #HTML(markdownToHTML(fragment.only=TRUE, text=c(
                                      "Este trabalho foi realizado no âmbito do estágio professional de Ricardo Jorge Agrela Faria (LREC), através do código de modelação solar ReSun (PereiraR., 2013).
                                      
                                      Autor: Ricardo Jorge Agrela Faria", a("GitHub", href="https://github.com/ricardo88faria/ReSun_WRF_analysis")
                                    )
                                    ),
                            
                            bsModal("Plot_and_table", "Gráfico e Tabela", "button_plot_and_table", size = "large",
                                    #highchartOutput("TestPlot"),
                                    plotOutput("TestPlot"),
                                    #    plotOutput("TestPlot1"),
                                    dataTableOutput("TestTable"))
                            
                            
                                    ),
                   tabPanel("Relatório",
                            uiOutput("page1")
                   ),
                   navbarMenu("Mais",
                              tabPanel("Sobre",
                                       "about_tab e laladinha"
                              )
                   )
                            )
)
