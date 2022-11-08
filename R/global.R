# Código Exemplo: 3954HpAb!
  
# Pacotes ####

library(DT)
library(DBI)
library(httr)
library(bslib)
library(rvest)
library(shiny)
library(argonR)
library(dbplyr)
library(doSNOW)
library(ggpage)
library(ggpubr)
library(rdrop2)
library(scales)
library(foreach)
library(stringi)
library(viridis)
library(argonDash)
library(emojifont)
library(gridExtra)
library(tidyverse)
library(ggalluvial)
library(shinyalert)
library(shinyFiles)
library(htmlwidgets)
library(shinyWidgets)
library(flexdashboard)
library(shinydashboard)
library(shinycssloaders)

# Autorizações externas ####

#token <- drop_auth() # Token para a autorização (Dropbox)
#saveRDS(token, "my-token.rds")

drop_auth(rdstoken = "my-token.rds")
drop_download("AppX2/dados_teste2019_atualizado.csv",overwrite = T)

load.fontawesome()

# Funções ####

`%notin%` <- Negate(`%in%`) # Negação de %in%

grafico_aluvial <- function(num,nome1,nome2,...){
  group_ <- syms(...)
  
  dd <- dados %>% select(!!!group_)
  dd <- cbind(dd,dd[,num])
  names(dd) <- c("variavel","a1","a2","a3")
  
  ggplot(dd,aes(y = variavel,axis1 = a1,axis2 = a2)) +
    geom_alluvium(aes(fill = a3), width = 1/8,curve_type = "cubic", reverse = FALSE)+
    guides(fill = FALSE) +
    geom_stratum(width = 1/8, reverse = FALSE,fill="snow",colour="black") +
    geom_text(stat = "stratum",colour="black", size = 5, fontface = "bold",aes(label = after_stat(stratum)),
              reverse = FALSE) +
    scale_x_discrete(limits = c(nome1, nome2), expand = c(.05, .05)) +
    scale_fill_brewer(type = "qual", palette = "Set1")+
    coord_flip() +
    theme_light()+
    labs(y = "")+
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank())
  
}

cartao <- function(titulo,comentario,plot){
  argonCard(
    title = titulo,
    hover_lift = TRUE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    width = 12,
    border_level = 0,
    icon = icon("check"),
    status = "red",
    background_color = "snow",
    gradient = FALSE, 
    floating = FALSE,
    comentario,
    splitLayout(
      
      cellWidths = "100%",
      cellArgs = list(style = "padding: 2px"),
      plotOutput(plot,height = "470px")))
}
cartao1 <- function(titulo,num,plot){
  argonCard(
    title = titulo,
    hover_lift = TRUE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    width = 12,
    border_level = 0,
    icon = icon("check"),
    status = "primary",
    background_color = "snow",
    gradient = FALSE, 
    floating = FALSE,
    argonInfoCard(value = num,
                  icon = icon("chart-line"),
                  title = paste0("Sua nota foi:"),
                  background_color = "tomato"),
    splitLayout(
      
      cellWidths = c("100%"),
      cellArgs = list(style = "padding: 2px"),
      plotOutput(plot,height = "470px")))
}
cartao2 <- function(titulo,comentario,tabl){
  argonCard(
    title = titulo,
    hover_lift = TRUE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    width = 12,
    border_level = 0,
    icon = icon("check"),
    status = "red",
    background_color = "snow",
    gradient = FALSE, 
    floating = FALSE,
    comentario,
    splitLayout(
      
      cellWidths = "100%",
      cellArgs = list(style = "padding: 2px"),
      dataTableOutput(tabl)))
}

num <- function(dat,n){
  num <- which(ifelse(alunos[1,n]-2 < density(dat$value)$x & density(dat$value)$x < alunos[1,n]+2,1,0)==1)
  num1 <- which(abs(density(dat$value)$x[num]-alunos[1,n])==min(abs(density(dat$value)$x[num]-alunos[1,n])))
  num <- num[num1]
  return(num)
} # Auxilia no gráfico de densidade
densidade <- function(dat,aluno,n,ll,lms){
  library(scales)
  a <- as.numeric(aluno)
  b <- as.numeric(n)
  ggplot(dat,aes(x = value))+
    geom_density(fill="white",col="black",position = "identity",size=1)+
    geom_area(
      aes(x = stage(value, after_stat = oob_censor(x, c(lms[1],alunos[a,b])))),
      stat = "density",fill="tomato",color="black",size=1)+
    #geom_segment(aes(x = alunos[a,b], y = 0, xend = alunos[a,b],
    #                 yend = density(dat$value)$y[num(dat,n)]),colour = "black",size=1)+
    geom_area(
      aes(x = stage(value, after_stat = oob_censor(x, c(alunos[a,b],lms[2])))),
      stat = "density",fill="tomato",color="black",size=1,alpha=0.04)+
    #annotate("label",x=400,y=max(density(dat$value)$y):max(density(dat$value)$y)+min(density(dat$value)$y),label=ll,fill="#daf2e0",size = 2)+
    scale_x_continuous(breaks = c(0,500,850))+
    xlim(lms)+
    theme_light()+
    labs(y = "", x = "")+
    theme(strip.background = element_rect(fill="grey97",
                                          colour = "lightgrey"),
          panel.grid = element_blank(),
          strip.text=element_text(face="bold",colour = "black"),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          legend.position = "none")
} # Gráfico de densidade
densidade_professor <- function(dat,ll,lms){
  library(scales)
  ggplot(dat,aes(x = value))+
    geom_density(fill="tomato",col="black",position = "identity",size=1)+
    scale_x_continuous(breaks = c(0,500,850))+
    xlim(lms)+
    theme_light()+
    labs(y = "", x = "")+
    theme(strip.background = element_rect(fill="grey97",
                                          colour = "lightgrey"),
          panel.grid = element_blank(),
          strip.text=element_text(face="bold",colour = "black"),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          legend.position = "none")
}

mapa_calor <- function(aluno,num1,num2,col1,col2){
  dat <- matrix(unlist(itens$b[num1]), ncol=9)
  dat <- as.data.frame(dat)
  names(dat) <- paste0("V",9:1)
  
  dat2 <- dat %>%
    as_tibble() %>%
    rownames_to_column('Var1') %>%
    gather(Var2, value, -Var1) %>%
    mutate(
      Var2 = factor(gsub("V", "", Var2), levels=1:9)
    )
  
  s1 <- fontawesome('fa-check')
  s2 <- fontawesome('fa-times')
  ss <- ifelse(as.numeric(alunos[aluno,num2]) == 1,
               paste0("           ",s1),paste0("           ",s2))
  tt <- paste0("Q",num1,":")
  
  ggplot(dat2, aes(as.factor(Var1), as.factor(Var2)))+
    geom_tile(aes(fill = value))+
    geom_text(aes(label=ss), size=4,family = 'fontawesome-webfont')+
    geom_text(aes(label=tt), size=4,hjust = 0.7)+
    #scale_fill_viridis_c(option= "plasma",na.value = "black")+
    scale_fill_gradient(low = col1, high = col2,
                        na.value = "grey80",
                        breaks=c(min(na.omit(dat2$value)),
                                 max(na.omit(dat2$value))),
                        labels=c("Muito fácil","Muito difícil"))+
    theme_void()+
    labs(x = "", y = "", fill = "")+
    theme(legend.position = "bottom",axis.text = element_blank(),
          legend.margin=margin(0,0,0,0))
} # Mapa de calor dos acertos por dificuldade
mapa_calor_professor <- function(num,col1,col2){
  dat <- matrix(unlist(itens$b[num]), ncol=9)
  dat <- as.data.frame(dat)
  names(dat) <- paste0("V",9:1)
  
  dat2 <- dat %>%
    as_tibble() %>%
    rownames_to_column('Var1') %>%
    gather(Var2, value, -Var1) %>%
    mutate(
      Var2 = factor(gsub("V", "", Var2), levels=1:9)
    )
  
  tt <- paste0("Q",num)
  
  ggplot(dat2, aes(as.factor(Var1), as.factor(Var2)))+
    geom_tile(aes(fill = value))+
    geom_text(aes(label=tt), size=4,hjust = 0.7)+
    scale_fill_gradient(low = col1, high = col2,
                        na.value = "grey80",
                        breaks=c(min(na.omit(dat2$value)),
                                 max(na.omit(dat2$value))),
                        labels=c("Muito fácil","Muito difícil"))+
    theme_void()+
    labs(x = "", y = "", fill = "")+
    theme(legend.position = "bottom",axis.text = element_blank(),
          legend.margin=margin(0,0,0,0))
} # Mapa de calor por dificuldade, só aparece na opção "todos" da área do professor
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)} # Retira a legenda

pontos <- function(dat,num,area,min,max){
  names(dat)[1:2] <- c("quant","nota")
  dd <- dat[num,c(1,2)]
  dd$quant <- factor(dd$quant,levels=min:max)
  dat %>%
    mutate(quant=factor(quant,levels=min:max))%>%
    ggplot(.,aes(x=quant,y=nota))+geom_point(alpha=0.3)+
    geom_point(aes(x=quant,y=nota), size = 4, colour = "red",data=dd,
               inherit.aes = FALSE)+
    labs(x=paste0("Acertos (",area,")"),
         y="Pontuação")+
    theme_light()+ylim(300,950)+
    scale_x_discrete(breaks=0:max)+
    theme(axis.title.x = element_text(size = rel(1.1), angle = 0,
                                      vjust = 0.0),
          axis.text.x = element_text(size = 10, angle = 45,
                                     vjust = 0.5,color="black"),
          axis.title.y = element_text(size = rel(1.1), angle = 90,
                                      vjust = 1.5),
          axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                     size=10,color="black"),
          panel.grid = element_blank(),
          panel.border = element_blank())
}
pontos_professor <- function(dat,area,min,max){
  names(dat)[1:2] <- c("quant","nota")
  dat %>%
    mutate(quant=factor(quant,levels=min:max))%>%
    ggplot(.,aes(x=quant,y=nota))+geom_point(alpha=0.3)+
    labs(x=paste0("Acertos (",area,")"),
         y="Pontuação")+
    theme_light()+ylim(300,950)+
    scale_x_discrete(breaks=0:max)+
    theme(axis.title.x = element_text(size = rel(1.1), angle = 0,
                                      vjust = 0.0),
          axis.text.x = element_text(size = 10, angle = 45,
                                     vjust = 0.5,color="black"),
          axis.title.y = element_text(size = rel(1.1), angle = 90,
                                      vjust = 1.5),
          axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                     size=10,color="black"),
          panel.grid = element_blank(),
          panel.border = element_blank())
}

histograma <- function(a,n){
  dd <- itens %>% dplyr::filter(itens$area == {{a}})
  
  ggplot(dd)+
    geom_histogram(aes(x=b),color="white",fill="tomato",bins = n)+
    labs(x="dificuldade",
         y="Pontuação")+
    theme_light()+
    xlim(300,950)+
    theme(axis.title.x = element_text(size = rel(1.1), angle = 0,
                                      vjust = 0.0),
          axis.text.x = element_text(size = 10,color="black"),
          axis.title.y = element_text(size = rel(1.1), angle = 90,
                                      vjust = 1.5),
          axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                     size=10,color="black"),
          panel.grid = element_blank(),
          panel.border = element_blank())
}

# Dados ####

# Alunos, acertos e notas
alunos <- read.csv("dados_teste2019_atualizado.csv")
alunos <- alunos[,-1]

#codigo <- NULL
#for (i in 1:dim(alunos)[1]) {
#  codigo[i] <- str_flatten(c(sample(1:10,4),sample(LETTERS,1),sample(letters,1#),sample(LETTERS,1),sample(letters,1),sample(c("!","@","#","&","+","-","?"),1))) #
#}
#alunos$COD <- codigo
#write.csv(alunos,"dados_teste2019_atualizado.csv")

# Parâmetros dos itens
itens<-read.csv("param_teste2019.csv")
itens$b <- ifelse(itens$b > 1100 | itens$b < 200,NA,itens$b)

# Cursos
cursos <- read.csv("cursos.csv")
cursos <- unique(cursos$x)
cursos[57] <- NA
cursos <- na.omit(cursos)

H1 <- cursos[c(1:2,13,15)]
H2 <- cursos[c(6:8,16:22,45,52,53,56:67,71,74,75,78,80)]
B <- cursos[c(3,9,12,23,25,44,47,48,54,55,69,72,73,83)]
T1 <- cursos[5]
T2 <- cursos[which(cursos %notin% c(H1,H2,B,T1))]
B <- data.frame(area = "Biológicas", p_l = 1.5, p_m = 1, p_h = 1.5, p_cn = 3, p_red = 1.5)
H1 <- data.frame(area = "Humanísticas I", p_l = 2, p_m = 2, p_h = 2, p_cn = 1, p_red = 1.5)
H2 <- data.frame(area = "Humanísticas II", p_l = 2.5, p_m = 1, p_h = 2.5, p_cn = 1, p_red = 1.5)
T1 <- data.frame(area = "Tecnológicas I", p_l = 1, p_m = 2, p_h = 2, p_cn = 2, p_red = 1.5)
T2 <- data.frame(area = "Tecnológicas II", p_l = 1, p_m = 3, p_h = 1, p_cn = 2, p_red = 1.5)
cursos <- rbind(B,H1,H2,T1,T2)

acertos.lc<-alunos[,c(5:49)]
quant.lc<-apply(acertos.lc[,-1],1,sum)
acertos.lc<-cbind(quant.lc,nota.lc=alunos$lc,acertos.lc)

acertos.ch<-alunos[,c(50:94)]
quant.ch<-apply(acertos.ch,1,sum)
acertos.ch<-cbind(quant.ch,nota.ch=alunos$ch,acertos.ch)

acertos.cn<-alunos[,c(95:139)]
quant.cn<-apply(acertos.cn,1,sum)
acertos.cn<-cbind(quant.cn,nota.cn=alunos$cn,acertos.cn)

acertos.mt<-alunos[,c(140:184)]
quant.mt<-apply(acertos.mt,1,sum)
acertos.mt<-cbind(quant.mt,nota.mt=alunos$mt,acertos.mt)

# Menu inicial ####
op <- TRUE
Logged <- FALSE
my_password = c("student","teacher")
my_passwordG = "student"
my_passwordC = "teacher"
"alunos$COD"

# Densidade ####
dd <- gather(alunos[,185:189])
dd$key <- as.factor(dd$key)
dd$key <- factor(dd$key,levels = levels(dd$key)[c(1,2,5,3,4)])
levels(dd$key) <- c("Ciências Humanas","Ciências da Natureza",
                    "Redação","Linguagens e Códigos","Matemática")

dh <- dd[dd$key=="Ciências Humanas",]
dn <- dd[dd$key=="Ciências da Natureza",]
dr <- dd[dd$key=="Redação",]
dl <- dd[dd$key=="Linguagens e Códigos",]
dm <- dd[dd$key=="Matemática",]

# Outros ####
#prograd <- read_html("https://ufrn.br/academico/ensino/graduacao/cursos")
#cursos <- prograd %>% 
#  html_nodes("a.curso.nome") %>%
#  html_text()

# 6542BuWz?

# Alluvial plot ####
#dados <- cbind(alunos[,1],rowSums(alunos[,5:184]),alunos[,185:190])
#names(dados)[1:2] <- c("aluno","acertos")
#
#dados1 <- rbind(itens$b,alunos[,5:184])
#
#num <- dim(dados1)[1]
#dd <- foreach(i=1:num, .combine = rbind) %dopar%{
#  dd <- dados1[c(1,i),]
#  num <- which(is.na(dd[1,]))
#  dd <- dd[,-num]
#  
#  dd <- dd[,dd[2,] == 1]
#  
#  dd[1,] <- case_when(
#    dd[1,] < 500 ~ "Fácil",
#    dd[1,] >= 500 & dd[1,] < 700 ~ "Médio",
#    dd[1,] >= 700 ~ "Difícil",
#  )
#  dd <- table(as.character(dd[1,]))
#  dd
#}
#
#rm(dados1)
#dados <- cbind(dados,dd)
#
#dados$maior <- foreach(i=1:dim(dados)[1], .combine = rbind) %dopar% {
#  num <- which(dados[i,]==max(dados[i,3:6]))
#  names(dados)[num]
#}
#
#dados$menor <- foreach(i=1:dim(dados)[1], .combine = rbind) %dopar% {
#  num <- which(dados[i,]==min(dados[i,3:6]))
#  names(dados)[num]
#}
#dados$red <- case_when(
#  dados$red < 600 ~ "Ruim",
#  dados$red >= 600 & dados$red < 700 ~ "Razoável",
#  dados$red >= 700 & dados$red < 800 ~ "Bom",
#  dados$red >= 800 ~ "Muito bom"
#)
#
#dados$acertos_cat <- case_when(
#  dados$acertos < 40 ~ "Ruim",
#  dados$acertos >= 40 & dados$acertos < 70 ~ "Razoável",
#  dados$acertos >= 70 & dados$acertos < 100 ~ "Bom",
#  dados$acertos >= 100 ~ "Muito bom"
#)
#
#dd <- gather(dados[,9:11])
#ggplot(dd)+
#  geom_histogram(aes(x=value, fill=key),col="white")+
#  facet_wrap(~key)+
#  theme_void()
#
#grafico_aluvial(2,"Menor nota","Maior nota",c("acertos","menor","maior"))
#grafico_aluvial(2,"Maior nota","Redação",c("Difícil","maior","red"))
#grafico_aluvial(2,"Maior nota","Redação",c("Médio","maior","red"))
#grafico_aluvial(2,"Maior nota","Redação",c("Fácil","maior","red"))
#
