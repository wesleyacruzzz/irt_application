shinyServer(function(input, output, session) {
  
  # Menu inicial ####
  values <- reactiveValues(authenticated0 = FALSE, authenticated1 = FALSE)
  
  dataModal0 <- function() {
    modalDialog(
      checkboxGroupButtons("cb","Are you:",
                           choices = c("Student","Teacher")),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("ok1", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.  
  # This `observe` is suspended only whith right user credential
  
  obs0 <- observe({
    showModal(dataModal0())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal. 
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  observeEvent(input$ok1,{
    dataModal <- function(failed = FALSE) {
      if (input$cb %in% "Student"){
        modalDialog(
          h3("Student area (password is 'student')"),
          passwordInput("password", "Password"),
          footer = tagList(
            # modalButton("Cancel"),
            actionButton("ok", "OK")
          )
        )
      } else {
        modalDialog(
          h3("Teacher area (password is 'teacher')"),
          passwordInput("password2", "Password"),
          footer = tagList(
            # modalButton("Cancel"),
            actionButton("ok2", "OK")
          )
        )
      }
      
    }
    
    # Show modal when button is clicked.  
    # This `observe` is suspended only whith right user credential
    
    obs1 <- observe({
      showModal(dataModal())
    })
    
    # When OK button is pressed, attempt to authenticate. If successful,
    # remove the modal. 
    
    obs2 <- observe({
      req(input$ok)
      isolate({
        Password <- "student"
      })
      Id.password <- which(my_password == Password)
      if (Password %in% my_passwordG){
        if (length(Id.password) > 0) {
          Logged <<- TRUE
          values$authenticated1 <<- TRUE
          values$authenticated0 <<- FALSE
          obs1$suspend()
          removeModal()
        } else {
          values$authenticated1 <<- FALSE
          values$authenticated0 <<- FALSE
        }
      } else {
        shinyalert("Oops!", "Wrong password!", type = "error")
      }
    })  
    
    obs3 <- observe({
      req(input$ok2)
      isolate({
        Password2 <- "teacher"
      })
      Id.password <- which(my_password == Password2)
      if (Password2 %in% my_passwordC){
        if (length(Id.password) > 0) {
          Logged <<- TRUE
          values$authenticated0 <<- TRUE
          values$authenticated1 <<- FALSE
          obs1$suspend()
          removeModal()
        } else {
          values$authenticated1 <<- FALSE
          values$authenticated0 <<- FALSE
        }
      } else {
        shinyalert("Oops!", "Wrong password!", type = "error")
      }  
    })
  })
  
  # Navibar dinâmica ####
  
  #renderCachedPlot()
  qual_num <- reactive({
    isolate(which(input$password == my_passwordG))
  })
  
  media_area <- reactive({
    aluno <- qual_num()
    res <- alunos[aluno,185:189]
    return(round(res))
  })

  observe({
    if(values$authenticated1){
      fluidPage(output$nav <- renderUI({
        resul <- media_area()
        num1 <- qual_num()
        allu <- alunos$nome[num1]
        fluidRow(
          argonColumn(width=12,
                      br(),
                      splitLayout(cellWidths = c("32%","68%"),
                                  cellArgs = list(style = "padding: 2px"),
                                  selectInput("cur",paste0("Hi, ",str_split(allu," ")[[1]][1],"! 
                                                           Which area do you identify the most?
                                                           "),
                                              choices = cursos$area,
                                              selected = cursos$area[1],
                                              multiple = F,
                                              selectize = F),
                                  splitLayout(cellWidths = c("33%","33%","33%"),
                                              cellArgs = list(style = "padding:
                                                              2px"),
                                              argonInfoCard(value = textOutput("info_a"),
                                                            icon = icon("user-graduate"),
                                                            title = paste0("For this area of knowledge your score was:"),
                                                            hover_lift = TRUE,
                                                            shadow = FALSE,
                                                            gradient = FALSE,
                                                            width = 12,
                                                            background_color = "#ffb6ab"),
                                              argonInfoCard(value = textOutput("info_b"),
                                                                                                        icon = icon("user-graduate"),
                                                                                                        title = paste0("Your general position was:"),
                                                                                                        hover_lift = TRUE,
                                                                                                        shadow = FALSE,
                                                                                                        gradient = FALSE,
                                                                                                        width = 12,
                                                                                                        background_color = "#ffb6ab"),
                                              argonInfoCard(value = textOutput("info_c"),
                                                            icon = icon("user-graduate"),
                                                            title = paste0("Your position in your class was:"),
                                                            hover_lift = TRUE,
                                                            shadow = FALSE,
                                                            gradient = FALSE,
                                                            width = 12,
                                                            background_color = "#ffb6ab")
                                              )),
                      argonTabSet(
                        id = "tabset",
                        card_wrapper = TRUE,
                        horizontal = TRUE,
                        circle = FALSE,
                        size = "sm",
                        width = 12,
                        iconList = list(
                          icon("book-open"), 
                          icon("landmark")
                        ),
                        argonTab(
                          tabName = "First day",
                          active = TRUE,
                          cartao("Answer sheet!",
                                 "If the gradient is redder means the question was harder, grey questions were not considered.",
                                 "plot")
                        ),
                        argonTab(
                          tabName = "Second day",
                          active = FALSE,
                          cartao("Answer sheet!",
                                 "If the gradient is redder means the question was harder, grey questions were not considered.",
                                 "plot_a")
                        )
                      )),
          column(12,
                 cartao("Class comparison!",
                        " ",
                        "plot2")),
          argonColumn(width=12,
                      argonTabSet(
                        id = "tabset1",
                        card_wrapper = TRUE,
                        horizontal = TRUE,
                        circle = FALSE,
                        size = "sm",
                        width = 12,
                        iconList = list(
                          icon("book-open"), 
                          icon("landmark"), 
                          icon("flask"), 
                          icon("calculator"), 
                          icon("pencil-alt")
                        ),
                        argonTab(
                          tabName = "Linguistics",
                          active = TRUE,
                          cartao1("Your score in Linguistics!",
                                  resul[1],
                                  "plot3_1")
                        ),
                        argonTab(
                          tabName = "Humanities",
                          active = FALSE,
                          cartao1("Your score in Humanities!",
                                  resul[2],
                                  "plot3_2")
                        ),
                        argonTab(
                          tabName = "Physical and Chemical Sciences",
                          active = FALSE,
                          cartao1("Your score in Physical and Chemical Sciences!",
                                  resul[3],
                                  "plot3_4")
                        ),
                        argonTab(
                          tabName = "Mathematics",
                          active = FALSE,
                          cartao1("Your score in Mathematics!",
                                  resul[4],
                                  "plot3_5")
                        ),
                        argonTab(
                          tabName = "Writing",
                          active = FALSE,
                          cartao1("Your writing score!",
                                  resul[5],
                                  "plot3_3")
                        )
                      )),
          argonColumn(width=12,
                      argonTabSet(
                        id = "tabset2",
                        card_wrapper = TRUE,
                        horizontal = TRUE,
                        circle = FALSE,
                        size = "sm",
                        width = 12,
                        iconList = list(
                          icon("book-open"), 
                          icon("landmark"), 
                          icon("flask"), 
                          icon("calculator"), 
                          icon("pencil-alt")
                        ),
                        argonTab(
                          tabName = "General",
                          active = TRUE,
                          cartao("Your general score!",
                                 " ",
                                 "plot4_3")
                        ),
                        argonTab(
                          tabName = "Linguistics",
                          active = FALSE,
                          cartao("Your Score!",
                                 " ",
                                 "plot4_1")
                        ),
                        argonTab(
                          tabName = "Humanities",
                          active = FALSE,
                          cartao("Your Score!",
                                 " ",
                                 "plot4_2")
                        ),
                        argonTab(
                          tabName = "Physical and Chemical Sciences",
                          active = FALSE,
                          cartao("Your Score!",
                                 " ",
                                 "plot4_4")
                        ),
                        argonTab(
                          tabName = "Mathematics",
                          active = FALSE,
                          cartao("Your Score!",
                                 "Texto",
                                 "plot4_5")
                        )
                      ))
          
        )
      }))
    } else {
      wellPanel("Wrong password")
    }
  })
  
  observe({
    if(values$authenticated0){
      fluidPage(output$nav <- renderUI({
        resul <- media_area()
        fluidRow(
          argonColumn(width=12,
                      br(),
                      splitLayout(cellWidths = c("20%","80%"),
                                  cellArgs = list(style = "padding: 2px"),
                                  selectInput("cur",paste0("Do you want to evaluate a specific student?"),
                                              choices = c("All of them",alunos$nome),
                                              selected = "All of them",
                                              multiple = F,
                                              selectize = F,
                                              width = "200px"),
                                  splitLayout(cellWidths = c("33%","33%","33%"),
                                              cellArgs = list(style = "padding:
                                                              2px"),
                                              argonInfoCard(value = textOutput("info1_a"),
                                                            icon = icon("user-graduate"),
                                                            title = paste0("This student's general score was:"),
                                                            hover_lift = TRUE,
                                                            shadow = FALSE,
                                                            gradient = FALSE,
                                                            width = 12,
                                                            background_color = "#ffb6ab"),
                                              argonInfoCard(value = textOutput("info1_b"),
                                                            icon = icon("user-graduate"),
                                                            title = paste0("This student general position is:"),
                                                            hover_lift = TRUE,
                                                            shadow = FALSE,
                                                            gradient = FALSE,
                                                            width = 12,
                                                            background_color = "#ffb6ab"),
                                              argonInfoCard(value = textOutput("info1_c"),
                                                            icon = icon("user-graduate"),
                                                            title = paste0("This students position in comparison with the class is:"),
                                                            hover_lift = TRUE,
                                                            shadow = FALSE,
                                                            gradient = FALSE,
                                                            width = 12,
                                                            background_color = "#ffb6ab")
                                  )),
                      argonTabSet(
                        id = "tabset",
                        card_wrapper = TRUE,
                        horizontal = TRUE,
                        circle = FALSE,
                        size = "sm",
                        width = 12,
                        iconList = list(
                          icon("book-open"), 
                          icon("landmark")
                        ),
                        argonTab(
                          tabName = "First day",
                          active = TRUE,
                          cartao("Answer sheet!",
                                 "If the gradient is redder means the question was harder, grey questions were not considered.",
                                 "plot_p")
                        ),
                        argonTab(
                          tabName = "Second day",
                          active = FALSE,
                          cartao("Answer sheet!",
                                 "If the gradient is redder means the question was harder, grey questions were not considered.",
                                 "plot_pp")
                        )
                      )),
          column(12,
                 cartao("Class comparison!",
                        "Explicação",
                        "plot2_p")),
          argonColumn(width=12,
                      argonTabSet(
                        id = "tabset1",
                        card_wrapper = TRUE,
                        horizontal = TRUE,
                        circle = FALSE,
                        size = "sm",
                        width = 12,
                        iconList = list(
                          icon("book-open"), 
                          icon("landmark"), 
                          icon("flask"), 
                          icon("calculator"), 
                          icon("pencil-alt")
                        ),
                        argonTab(
                          tabName = "Linguistics",
                          active = TRUE,
                          cartao("This students score in Linguistics!",
                                 " ",
                                 "plot3_1_p")
                        ),
                        argonTab(
                          tabName = "Humanities",
                          active = FALSE,
                          cartao("This students score in  Humanities!",
                                 " ",
                                 "plot3_2_p")
                        ),
                        argonTab(
                          tabName = "Physical and Chemical Sciences",
                          active = FALSE,
                          cartao("This students score in Physical and Chemical Sciences!",
                                 " ",
                                 "plot3_4_p")
                        ),
                        argonTab(
                          tabName = "Mathematics",
                          active = FALSE,
                          cartao("This students score in Mathematics!",
                                 " ",
                                 "plot3_5_p")
                        ),
                        argonTab(
                          tabName = "Writing",
                          active = FALSE,
                          cartao("This students score in writing!",
                                 " ",
                                 "plot3_3_p")
                        )
                      )),
          argonColumn(width=12,
                      argonTabSet(
                        id = "tabset2",
                        card_wrapper = TRUE,
                        horizontal = TRUE,
                        circle = FALSE,
                        size = "sm",
                        width = 12,
                        iconList = list(
                          icon("book-open"), 
                          icon("landmark"), 
                          icon("flask"), 
                          icon("calculator"), 
                          icon("pencil-alt")
                        ),
                        argonTab(
                          tabName = "General",
                          active = TRUE,
                          cartao("The student score!",
                                 " ",
                                 "plot4_3_p")
                        ),
                        argonTab(
                          tabName = "Linguistics",
                          active = FALSE,
                          cartao("The student score!",
                                 " ",
                                 "plot4_1_p")
                        ),
                        argonTab(
                          tabName = "Humanities",
                          active = FALSE,
                          cartao("The student score!",
                                 " ",
                                 "plot4_2_p")
                        ),
                        argonTab(
                          tabName = "Physical and Chemical Sciences",
                          active = FALSE,
                          cartao("The student score!",
                                 " ",
                                 "plot4_4_p")
                        ),
                        argonTab(
                          tabName = "Mathematics",
                          active = FALSE,
                          cartao("The student score!",
                                 " ",
                                 "plot4_5_p")
                        )
                      )),
          argonColumn(width=12,
                      argonTabSet(
                        id = "tabset3",
                        card_wrapper = TRUE,
                        horizontal = TRUE,
                        circle = FALSE,
                        size = "sm",
                        width = 12,
                        iconList = list(
                          icon("book-open"), 
                          icon("landmark"), 
                          icon("flask"), 
                          icon("calculator")
                        ),
                        argonTab(
                          tabName = "Linguistics",
                          active = TRUE,
                          cartao(" ",
                                 "Questions difficulty histogram",
                                 "plot5_1_p")
                        ),
                        argonTab(
                          tabName = "Humanities",
                          active = FALSE,
                          cartao(" ",
                                 "Questions difficulty histogram",
                                 "plot5_2_p")
                        ),
                        argonTab(
                          tabName = "Physical and Chemical Sciences",
                          active = FALSE,
                          cartao(" ",
                                 "Questions difficulty histogram",
                                 "plot5_3_p")
                        ),
                        argonTab(
                          tabName = "Mathematics",
                          active = FALSE,
                          cartao(" ",
                                 "Questions difficulty histogram",
                                 "plot5_4_p")
                        )
                      )),
          argonColumn(width=12,
                      cartao2("Students final placement",
                              " ",
                              "table_p"))
          
        )
      }))
    } else {
      wellPanel("Wrong password")
    }
  })
  
  # Navbar redirecionamento ####
  observeEvent(input$navibar,{
    if(input$navibar == "home"){
      #browseURL("https://www.google.com")
    }
  })
  # ÁREA DO ALUNO ####
  
  num <- reactiveVal(NULL)
  
  output$plot <- renderPlot({
    alu <- qual_num()
    
    p1 <- mapa_calor(aluno=alu,num1=1:45,num2=5:49,col1="white",col2="firebrick3")
    p2 <- mapa_calor(aluno=alu,num1=46:90,num2=50:94,col1="white",col2="firebrick3")
    
    gridExtra::grid.arrange(arrangeGrob(p1 + theme(legend.position="bottom"),
                                        p2 + theme(legend.position="bottom"),
                                        nrow=1,ncol=2))
  })
  
  output$plot_a <- renderPlot({
    alu <- qual_num()
    
    p3 <- mapa_calor(aluno=alu,num1=91:135,num2=95:139,col1="white",col2="firebrick3")
    p4 <- mapa_calor(aluno=alu,num1=136:180,num2=140:184,col1="white",col2="firebrick3")
    mylegend<-g_legend(p4)
    
    gridExtra::grid.arrange(arrangeGrob(p3 + theme(legend.position="bottom"),
                                        p4 + theme(legend.position="bottom"),
                                        nrow=1,ncol=2))
  })
  
  output$plot2 <- renderPlot({
    alu <- qual_num()
    dd_a <- gather(alunos[alu,185:189])
    dd_o <- gather(alunos[-as.numeric(alu),185:189])
    dd_o <- dd_o %>% group_by(key) %>% summarise(across(c("value"), ~ mean(.x, na.rm = TRUE)))
    dd_t <- gather(alunos[-as.numeric(alu),c(2,185:189)],"key","value",-turma)
    dd_t <- dd_t %>% group_by(turma,key) %>% summarise(across(c("value"), ~ mean(.x, na.rm = TRUE)))
    tt <- alunos[alu,2]
    dd_t <- dd_t %>% filter(turma == tt)
    dd1 <- left_join(dd_a,dd_t,by="key")
    dd1 <- left_join(dd1,dd_o,by="key")
    dd1 <- dd1[,-3]
    names(dd1) <- c("chave","Aluno","Turma","Geral")
    dd1 <- gather(dd1)
    area <- dd1[c(1:5),2]
    dd1 <- dd1[-c(1:5),]
    dd1$disciplinas <- rep(area,3) 
    names(dd1)[3] <- "area"
    dd1$area <- as.factor(dd1$area)
    levels(dd1$area) <- c("Ciências Humanas","Ciências da Natureza",
                          "Linguagens e Códigos","Matemática","Redação")
    dd1$area <- factor(dd1$area,levels = levels(dd1$area)[c(1,3,2,4,5)])
    dd1$value <- round(as.numeric(dd1$value))
    
    ggplot(dd1)+
      geom_col(aes(x = area,y = value, fill = key),position = "dodge2")+
      geom_text(aes(label = value,x = area, y=value), vjust = 0.2, hjust = -0.5,
                position = position_dodge2(width=0.9))+
      ylim(c(0,1000))+
      coord_flip()+
      labs(x="",y="", fill = "Nota média")+
      scale_fill_manual(values = c("#95eda5","tomato","forestgreen"))+
      theme_light()+
      theme(panel.grid = element_blank(),
            strip.text=element_text(face="bold",colour = "black"),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            legend.position = "bottom")
  })
  
  output$plot3_1 <- renderPlot({
    alu <- qual_num()
    densidade(dat=dl,aluno=alu,n=185,ll="Linguagens e Códigos",lms = c(300,1000))
  })
  output$plot3_2 <- renderPlot({
    alu <- qual_num()
    densidade(dat=dh,aluno=alu,n=186,ll="Ciências Humanas",lms = c(300,1000))
  })
  output$plot3_3 <- renderPlot({
    alu <- qual_num()
    densidade(dat=dr,aluno=alu,n=189,ll="Redação",lms = c(300,1000))
  })
  output$plot3_4 <- renderPlot({
    alu <- qual_num()
    densidade(dat=dn,aluno=alu,n=187,ll="Ciências da Natureza",lms = c(300,1000))
  })
  output$plot3_5 <- renderPlot({
    alu <- qual_num()
    densidade(dat=dm,aluno=alu,n=188,ll="Matemática",lms = c(300,1000))
  })
  
  output$plot4_1 <- renderPlot({
    alu <- qual_num()
    pontos(acertos.lc,alu,"Linguagens e Códigos",min(quant.lc),max(quant.lc))
  })
  output$plot4_2 <- renderPlot({
    alu <- qual_num()
    pontos(acertos.ch,alu,"Ciências Humanas",min(quant.ch),max(quant.ch))
  })
  output$plot4_3 <- renderPlot({
    alu <- qual_num()
    
    num <- which(cursos$area == input$cur)
    pesos <- cursos[num,2:6]
    pesos <- gather(pesos)
    
    media_pon <- NULL
    media_pon <- foreach::foreach(i=1:dim(alunos)[1], .combine = rbind) %dopar% {
      weighted.mean(x=alunos[i,185:189],w=pesos$value)
    }
    
    acertos<-alunos[,c(5:184)]
    quant<-apply(acertos,1,sum)
    acertos<-cbind(quant,nota=media_pon,acertos)
    
    pontos(acertos,alu,"Gerais",min(quant),max(quant))
  })
  output$plot4_4 <- renderPlot({
    alu <- qual_num()
    pontos(acertos.cn,alu,"Ciências da Natureza",min(quant.cn),max(quant.cn))
  })
  output$plot4_5 <- renderPlot({
    alu <- qual_num()
    pontos(acertos.mt,alu,"Matemática",min(quant.mt),max(quant.mt))
  })
  
  num_curso <- reactive({
    which(cursos$area == input$cur)
  })
  
  output$info_a <- renderText({
    num <- num_curso()
    alu <- qual_num()
    pesos <- cursos[num,2:6]
    pesos <- gather(pesos)
    
    nota <- weighted.mean(x=alunos[alu,185:189],w=pesos$value)
    
    as.character(round(nota))
  })
  output$info_b <- renderText({
    num <- num_curso()
    pesos <- cursos[num,2:6]
    pesos <- gather(pesos)
    
    alu <- qual_num()
    media_pon <- NULL
    media_pon <- foreach::foreach(i=1:dim(alunos)[1], .combine = rbind) %dopar% {
      weighted.mean(x=alunos[i,185:189],w=pesos$value)
    }
    
    media_pon <- cbind(media_pon,alunos[,2])
    media_pon <- as.data.frame(media_pon)
    media_pon$V1 <- as.numeric(media_pon$V1)
    media_pon <- media_pon %>% group_by(V1) %>% arrange(desc(V1))
    
    nota <- weighted.mean(x=alunos[alu,185:189],w=pesos$value)
    posicao <- which(round(media_pon$V1,6) == round(as.numeric(nota),6))
    
    paste0(posicao[1],".º de ",dim(alunos)[1])
  })
  output$info_c <- renderText({
    num <- num_curso()
    pesos <- cursos[num,2:6]
    pesos <- gather(pesos)
    
    alu <- qual_num()
    media_pon <- NULL
    media_pon <- foreach::foreach(i=1:dim(alunos)[1], .combine = rbind) %dopar% {
      weighted.mean(x=alunos[i,185:189],w=pesos$value)
    }
    
    media_pon <- cbind(media_pon,alunos[,2])
    media_pon <- as.data.frame(media_pon)
    media_pon$V1 <- as.numeric(media_pon$V1)
    media_pon <- media_pon %>% group_by(V1) %>% arrange(desc(V1))
    nota <- weighted.mean(x=alunos[alu,185:189],w=pesos$value)
    posicao <- which(round(media_pon$V1,6) == round(as.numeric(nota),6))
    
    media_pon_turma <- media_pon %>% filter(V2 == alunos[alu,2])
    posicao_turma <- which(round(media_pon_turma$V1,6)==round(nota,6))
    paste0(posicao_turma[1],".º de ",dim(media_pon_turma)[1])
  })
  
  # ÁREA DO PROFESSOR ####
  
  output$plot_p <- renderPlot({
    if(input$cur == "Todos"){
      p1 <- mapa_calor_professor(num=1:45,col1="white",col2="firebrick3")
      p2 <- mapa_calor_professor(num=46:90,col1="white",col2="firebrick3")
    } else {
      alu <- which(alunos$nome == input$cur)
      
      p1 <- mapa_calor(aluno=alu,num1=1:45,num2=5:49,col1="white",col2="firebrick3")
      p2 <- mapa_calor(aluno=alu,num1=46:90,num2=50:94,col1="white",col2="firebrick3")
    }
    gridExtra::grid.arrange(arrangeGrob(p1 + theme(legend.position="bottom"),
                                        p2 + theme(legend.position="bottom"),
                                        nrow=1,ncol=2))
  })
  output$plot_pp <- renderPlot({
    
    if(input$cur == "Todos"){
      p3 <- mapa_calor_professor(num=91:135,col1="white",col2="firebrick3")
      p4 <- mapa_calor_professor(num=136:180,col1="white",col2="firebrick3")
    } else {
      alu <- which(alunos$nome == input$cur)
    
      p3 <- mapa_calor(aluno=alu,num1=91:135,num2=95:139,col1="white",col2="firebrick3")
      p4 <- mapa_calor(aluno=alu,num1=136:180,num2=140:184,col1="white",col2="firebrick3")
      mylegend<-g_legend(p4)
    }
    gridExtra::grid.arrange(arrangeGrob(p3 + theme(legend.position="bottom"),
                                        p4 + theme(legend.position="bottom"),
                                        nrow=1,ncol=2))
  })
  
  output$plot2_p <- renderPlot({
    if(input$cur == "Todos"){
      dd_o <- gather(alunos[,185:189]) %>%
              group_by(key) %>%
              summarise_at(vars("value"),c(mean))
      dd_o$Tipo <- "Escola"
      
      dd_t <- gather(alunos[,c(2,185:189)],"key","value",-turma) %>% 
              group_by(turma,key) %>% summarise_at(vars("value"),c(mean))
      names(dd_t)[1] <- "Tipo"
      dd_t$Tipo <- paste0("Turma ",dd_t$Tipo)
      
      dd <- rbind(dd_t,dd_o)
      dd$key <- as.factor(dd$key)
      levels(dd$key) <- c("Ciências Humanas","Ciências da Natureza",
                            "Linguagens e Códigos","Matemática","Redação")
      dd$key <- factor(dd$key,levels = levels(dd$key)[c(1,3,2,4,5)])
      
      ggplot(dd)+
        geom_col(aes(x = key,y = value, fill = Tipo),position = "dodge2")+
        geom_text(aes(label = round(value),x = key, y=round(value)),
                  vjust = 0.5, hjust = -0.5,
                  position = position_dodge2(width=0.9))+
        ylim(c(0,1000))+
        coord_flip()+
        labs(x="",y="", fill = "Nota média")+
        #scale_fill_manual(values = c("#95eda5","tomato","forestgreen"))+
        theme_light()+
        theme(panel.grid = element_blank(),
              strip.text=element_text(face="bold",colour = "black"),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              panel.border = element_blank(),
              legend.position = "bottom")
      
    } else {
      alu <- which(alunos$nome == input$cur)
      dd_a <- gather(alunos[alu,185:189])
      dd_o <- gather(alunos[-as.numeric(alu),185:189])
      dd_o <- dd_o %>% group_by(key) %>% summarise(across(c("value"), ~ mean(.x, na.rm = TRUE)))
      dd_t <- gather(alunos[-as.numeric(alu),c(2,185:189)],"key","value",-turma)
      dd_t <- dd_t %>% group_by(turma,key) %>% summarise(across(c("value"), ~ mean(.x, na.rm = TRUE)))
      tt <- alunos[alu,2]
      dd_t <- dd_t %>% filter(turma == tt)
      dd1 <- left_join(dd_a,dd_t,by="key")
      dd1 <- left_join(dd1,dd_o,by="key")
      dd1 <- dd1[,-3]
      names(dd1) <- c("chave","Aluno","Turma","Geral")
      dd1 <- gather(dd1)
      area <- dd1[c(1:5),2]
      dd1 <- dd1[-c(1:5),]
      dd1$disciplinas <- rep(area,3) 
      names(dd1)[3] <- "area"
      dd1$area <- as.factor(dd1$area)
      levels(dd1$area) <- c("Ciências Humanas","Ciências da Natureza",
                            "Linguagens e Códigos","Matemática","Redação")
      dd1$area <- factor(dd1$area,levels = levels(dd1$area)[c(1,3,2,4,5)])
      dd1$value <- round(as.numeric(dd1$value))
      
      ggplot(dd1)+
        geom_col(aes(x = area,y = value, fill = key),position = "dodge2")+
        geom_text(aes(label = value,x = area, y=value), vjust = 0.2, hjust = -0.5,
                  position = position_dodge2(width=0.9))+
        ylim(c(0,1000))+
        coord_flip()+
        labs(x="",y="", fill = "Nota média")+
        scale_fill_manual(values = c("#95eda5","tomato","forestgreen"))+
        theme_light()+
        theme(panel.grid = element_blank(),
              strip.text=element_text(face="bold",colour = "black"),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              panel.border = element_blank(),
              legend.position = "bottom")
    }
  })
  
  output$plot3_1_p <- renderPlot({
    if(input$cur == "Todos"){
      densidade_professor(dat=dl,ll="Linguagens e Códigos",lms = c(300,1000))
    } else {
      alu <- which(alunos$nome == input$cur)
      densidade(dat=dl,aluno=alu,n=185,ll="Linguagens e Códigos",lms = c(300,1000))
    }
    
  })
  output$plot3_2_p <- renderPlot({
    if(input$cur == "Todos"){
      densidade_professor(dat=dh,ll="Ciências Humanas",lms = c(300,1000))
    } else {
      alu <- which(alunos$nome == input$cur)
      densidade(dat=dh,aluno=alu,n=186,ll="Ciências Humanas",lms = c(300,1000))
    }
    
  })
  output$plot3_3_p <- renderPlot({
    if(input$cur == "Todos"){
      densidade_professor(dat=dr,ll="Redação",lms = c(300,1000))
    } else {
      alu <- which(alunos$nome == input$cur)
      densidade(dat=dr,aluno=alu,n=189,ll="Redação",lms = c(300,1000))
    }
  })
  output$plot3_4_p <- renderPlot({
    if(input$cur == "Todos"){
      densidade_professor(dat=dn,ll="Ciências da Natureza",lms = c(300,1000))
    } else {
      alu <- which(alunos$nome == input$cur)
      densidade(dat=dn,aluno=alu,n=187,ll="Ciências da Natureza",lms = c(300,1000))
    }
  })
  output$plot3_5_p <- renderPlot({
    if(input$cur == "Todos"){
      densidade_professor(dat=dm,ll="Matemática",lms = c(300,1000))
    } else {
      alu <- which(alunos$nome == input$cur)
      densidade(dat=dm,aluno=alu,n=187,ll="Matemática",lms = c(300,1000))
    }
  })
  
  output$plot4_1_p <- renderPlot({
    if(input$cur == "Todos"){
      pontos_professor(acertos.lc,"Linguagens e Códigos",min(quant.lc),max(quant.lc))
    } else {
      alu <- which(alunos$nome == input$cur)
      pontos(acertos.lc,alu,"Linguagens e Códigos",min(quant.lc),max(quant.lc))
    }
    
  })
  output$plot4_2_p <- renderPlot({
    if(input$cur == "Todos"){
      pontos_professor(acertos.ch,"Ciências Humanas",min(quant.ch),max(quant.ch))
    } else {
      alu <- which(alunos$nome == input$cur)
      pontos(acertos.ch,alu,"Ciências Humanas",min(quant.ch),max(quant.ch))
    }
  })
  output$plot4_3_p <- renderPlot({
    if(input$cur == "Todos"){
      media <- apply(alunos[,185:189],1,mean)
      
      acertos<-alunos[,c(5:184)]
      quant<-apply(acertos,1,sum)
      acertos<-cbind(quant,nota=media,acertos)
      
      pontos_professor(acertos,"Gerais",min(quant),max(quant))
    } else {
      alu <- which(alunos$nome == input$cur)
      
      media <- apply(alunos[,185:189],1,mean)
      acertos<-alunos[,c(5:184)]
      quant<-apply(acertos,1,sum)
      acertos<-cbind(quant,nota=media,acertos)
      
      pontos(acertos,alu,"Gerais",min(quant),max(quant))
    }
  })
  output$plot4_4_p <- renderPlot({
    if(input$cur == "Todos"){
      pontos_professor(acertos.cn,"Ciências da Natureza",min(quant.cn),max(quant.cn))
    } else {
      alu <- which(alunos$nome == input$cur)
      pontos(acertos.cn,alu,"Ciências da Natureza",min(quant.cn),max(quant.cn))
    }
  })
  output$plot4_5_p <- renderPlot({
    if(input$cur == "Todos"){
      pontos_professor(acertos.mt,"Matemática",min(quant.mt),max(quant.mt))
    } else {
      alu <- which(alunos$nome == input$cur)
      pontos(acertos.mt,alu,"Matemática",min(quant.mt),max(quant.mt))
    }
  })
  
  output$plot5_1_p <- renderPlot({
    histograma("CH",20)
  })
  output$plot5_2_p <- renderPlot({
    histograma("LC",20)
  })
  output$plot5_3_p <- renderPlot({
    histograma("CN",20)
  })
  output$plot5_4_p <- renderPlot({
    histograma("MT",20)
  })
  
  output$table_p <- renderDataTable({
    alunos1 <- alunos[,c(1,2,185:189)]
    alunos1 <- cbind(alunos1[,c(1,2)],apply(as.matrix(alunos1[,3:7]),1,mean),
                     alunos1[,3:7])
    names(alunos1) <- c("Nome","Turma","media","Linguagens e códigos","Ciências humanas","Ciências da natureza","Matemática","Redação")
    alunos1 <- alunos1 %>% group_by(media) %>% arrange(desc(media))
    names(alunos1)[3] <- "Nota média";alunos1
  })
  
  output$info1_a <- renderText({
    if(input$cur == "Todos"){
      "Nenhum aluno escolhido!"
    } else {
      alu <- which(alunos$nome == input$cur)
      round(mean(as.numeric(alunos[alu,185:189])))
    }
  })
  output$info1_b <- renderText({
    if(input$cur == "Todos"){
      "Nenhum aluno escolhido!"
    } else {
      alu <- which(alunos$nome == input$cur)
      medias <- as.data.frame(apply(as.matrix(alunos[,185:189]),1,mean))
      names(medias) <- "a"
      medias <- medias %>% group_by(a) %>% arrange(desc(a))
      media <- mean(as.numeric(alunos[alu,185:189]))
      a <- as.character(which(medias==media))
      paste0(a[1],".º de ",dim(alunos)[1])
    }
  })
  output$info1_c <- renderText({
    if(input$cur == "Todos"){
      "Nenhum aluno escolhido!"
    } else {
      alu <- which(alunos$nome == input$cur)
      medias <- cbind(apply(as.matrix(alunos[,185:189]),1,mean),alunos[,2]) %>%
        as.data.frame() %>% group_by(V1) %>% arrange(desc(V1))
      media <- mean(as.numeric(alunos[alu,185:189]))
      medias <- medias %>% filter(V2 == alunos[alu,2])
      a <- as.character(which(medias==media))
      paste0(a[1],".º de ",dim(medias)[1])
    }
  })

})
                                    
