##### Packages #####
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(plotly)
library(shinythemes)
library(tidymodels)
library(tidytext)
library(embed)

##### DATA #####
df = read.csv("https://raw.githubusercontent.com/WXYS1209/STAT436/main/DATA.csv",
              row.names = 1)

desp = read.csv("https://raw.githubusercontent.com/WXYS1209/STAT436/main/Desp.csv",
                row.names = 1)
desp[11,2] = "Minutes played divided by 90"

##### PCA #####
get_pca_rec = function(DF){
  PCA = recipe(~., data = DF) %>%
    update_role(Player, Pos, new_role = "id") %>%
    step_normalize(all_predictors()) %>%
    step_pca(all_predictors())
}
pca_rec = get_pca_rec(df[,-c(2:3,5:7)])
pca_prep = prep(pca_rec)
components = tidy(pca_prep, 2)
components$exp = rep(desp$Meaning[-1:-7], 112)
# scores = juice(pca_prep)
variances = tidy(pca_prep, 2, type = "variance")

X_prcomp = prcomp(df[,-1:-7], center = TRUE,
                  scale. = TRUE,
                  retx = TRUE)
scores_pca = data.frame(Player = df$Player, 
                        Pos = df$Pos, 
                        X_prcomp$x)

variances_summary <- variances %>%
  filter(terms %in% c("cumulative percent variance", "percent variance")) %>%
  group_by(component) %>%
  summarize(
    percent_variance = value[terms == "percent variance"],
    cumulative_percent_variance = value[terms == "cumulative percent variance"]
  )

##### UMAP #####
cc = read.csv("https://raw.githubusercontent.com/WXYS1209/STAT436/main/umap_cc.csv",
              row.names = 1)

cc$name[which(cc$name == "sil")] = "SC"
cc$name[which(cc$name == "db")] = "DB"
cc$name[which(cc$name == "ch")] = "CH"

cols <- c("#51b48c", "#cf3d6e", "#7ab743", "#7b62cb", "#c49644", "#c364b9", "#6a803a", "#688dcd", "#c95a38", "#c26b7e")

get_umap_rec = function(DF, neib = 5, mindist = 0, n_comp = 2){
  UMAP = recipe(~., data = DF) %>%
    update_role(Player, Pos, new_role = "id") %>%
    step_normalize(all_predictors()) %>%
    step_umap(all_predictors(), neighbors = neib, min_dist = mindist, num_comp = n_comp)
  return(UMAP)
}

set.seed(99)
umap_rec = get_umap_rec(df[,-c(2:3,5:7)],
                        neib = 5,
                        mindist = 0,
                        n_comp = 2)
umap_prep = prep(umap_rec)
scores_umap = juice(umap_prep)
km15 = kmeans(scores_umap[,3:4], centers = 5, iter.max = 100, algorithm = "MacQueen", nstart = 25)


##### UI #####
ui = fluidPage(
  tags$head(
    tags$style(HTML("
                    .navbar-brand {
                      font-size: 30px;
                      font-weight: bold;
                      padding-top: 15px;
                      padding-bottom: 15px;
                    }
                    .navbar {
                      font-size: 18px;
                    }
                    .sidebar-panel {
                      background-color: #f8f9fa;
                    }
                  "))
  ),
  navbarPage("2021-22 Season Top 5 European Leagues Player Classification", theme = shinytheme("lumen"),
             tabPanel("Introduction",
                      tags$div(style = "padding: 20px;",
                               tags$style(type = 'text/css', "
                            .custom-h1 {
                              font-size: 28px;
                              font-weight: bold;
                              margin-bottom: 20px;
                            }
                            .custom-h2 {
                              font-size: 24px;
                              font-weight: bold;
                              margin-top: 30px;
                              margin-bottom: 10px;
                            }
                            .custom-p {
                              font-size: 16px;
                              line-height: 1.5;
                            }"),
                               h1("Welcome to the 2021-22 Season Top 5 European Leagues Player Classification App", class = "custom-h1"),
                               column(6,
                                      h2("About this app", class = "custom-h2"),
                                     p("This app helps to understand the new soccer players' playing style induced by dimensional reduction and clustering method.
                                     The data is the basic information, shooting, passing, defense, and other data of players in the five major European soccer leagues in the 2021-2022 season, 
                                     the unit is every 90 minutes.", class = "custom-p"),
                                     p("For dimensional reduction, PCA and UMAP were used. After comparing these two methods with internal evaluation of clustering,
                                       model that take UMAP with n_neighbor = 5 and min_dist = 2 to reduce dimension and k-Means with k = 5 to cluster were selected. 
                                       The internal evaluation contains Calinski-Harabaz Index (CH), Davies-Bouldin Index (BD) and Silhouette Coefficient (SC). 
                                       Moreover, above evaluations were used to select hyperparameters for UMAP as well.", class = "custom-p"),
                                     h2("How to use", class = "custom-h2"),
                                     p("Use the navigation bar to switch between different sections of the app. Each section provides different functionalities and visualizations.", class = "custom-p")
                                     ),
                               column(6,
                                      h2("Radar Chart", class = "custom-h2"),
                                      p("Below is the radar chart of some variables.", class = "custom-p"),
                                      plotlyOutput("Radar")),
                               h2("Variable Explanations", class = "custom-h2"),
                               p("Below is the meaning of each variable in the data set.", class = "custom-p"),
                               dataTableOutput("var_exp")
                               
                      )
                      
             ),
             
             tabPanel("DR-PCA",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     helpText(h3("Instruction:")),
                                     helpText("Slide to select the number of PCs in the Scree Plot."),
                                     helpText("Select one PC to update the Variable Composition."),
                                     helpText("Select two PCs to update Scatter Plot for PCA Result."),
                                     wellPanel(
                                       fluidRow(
                                         column(12,
                                                sliderInput(inputId = "pc_num",
                                                            label = "Select the number of PCs for Scree Plot",
                                                            min = 1,
                                                            max = 112,
                                                            value = 50,
                                                            step = 1
                                                ),
                                                pickerInput(inputId = "pc_comp",
                                                            label = "Select PCs for Composition",
                                                            choices = paste0("PC", 1:112),
                                                            multiple = T,
                                                            options = list(`actions-box` = T,
                                                                           `live-search`=TRUE)
                                                ),
                                                pickerInput(inputId = "pc_res",
                                                            label = "Select two PCs for Scatter Plot",
                                                            choices = paste0("PC", 1:112),
                                                            multiple = TRUE,
                                                            options = list(`actions-box` = TRUE,
                                                                           `live-search`=TRUE)
                                                ),
                                                
                                         )
                                       )
                                     )
                        ),
                        mainPanel(
                          fluidRow(
                            column(6, h3("Screeplot of PCA"),
                                   plotlyOutput("Scree")),
                            column(6, h3("Principal Components' Composition"),
                                   plotlyOutput("Comp"))
                          ),
                          fluidRow(
                            h3("PCA Result"),
                            plotlyOutput("PCAres", height = "600px")
                          )
                        )
                      )
             ),
             tabPanel("DR-UMAP",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     helpText(h3("Instruction:")),
                                     helpText("Select different hyperparameters to update both plots."),
                                     helpText("For the above plot, color and alpha represents n_neighbor, line width and line type represents min_dist."),
                                     helpText("In more detail, alpha and line width represents which hyperparameters that you have selected."),
                                     wellPanel(
                                       fluidRow(
                                         column(6,
                                                selectInput(inputId = "umap_hyper1",
                                                            label = "n_neighbor",
                                                            choices = c(3,5,10,20,50),
                                                            multiple = F
                                                )
                                         ),
                                         column(6,
                                                selectInput(inputId = "umap_hyper2",
                                                            label = "min_dist",
                                                            choices = c(0, 0.1,0.25,0.5,0.8,0.99),
                                                            multiple = F
                                                )
                                         )
                                       )
                                     )
                        ),
                        mainPanel(
                          fluidRow(
                            h3("Selection of Hyperparameters"),
                            plotlyOutput("UMAPhyper"),
                            h3("UMAP Result"),
                            plotlyOutput("UMAPres", height = "600px")
                          )
                        )
                      )
             ),
             
             tabPanel("Clustering",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     helpText(h3("Instruction:")),
                                     helpText("The model is UMAP (n_neighbor = 5, min_dist = 0) with k-Means (k = 5)."),
                                     helpText("Select players' Leagues, Teams and Positions that you're interested in."),
                                     wellPanel(
                                       pickerInput(inputId = "league",
                                                   label = "Select League",
                                                   choices = unique(df$Comp),
                                                   multiple = TRUE,
                                                   options = list(`actions-box` = TRUE)),
                                       pickerInput(inputId = "team",
                                                   label = "Select Team",
                                                   choices = unique(df$Squad),
                                                   multiple = TRUE,
                                                   options = list(`actions-box` = TRUE,
                                                                  `live-search`=TRUE)),
                                       pickerInput(inputId = "position",
                                                   label = "Select Position",
                                                   choices = unique(df$Pos),
                                                   multiple = TRUE,
                                                   options = list(`actions-box` = TRUE))
                                     )
                        ),
                        mainPanel(
                          fluidRow(
                            h3("Cluster Result"),
                            plotlyOutput("Clures", height = "500px")
                          )
                        )
                      ),
                      column(6, h3("Player Information"),
                             dataTableOutput("player")),
                      column(6, h3("Cluster Explanation by PCs"),
                             plotlyOutput("box", height = "600px"))
             )
  )
)


##### SERVER #####
server <- function(input, output, session) {
  # Update Radar
  output$Radar = renderPlotly({

    data_radar = df[, c("Player", "Pos", "Squad", "Comp",
                                    "Gls.y", "Tkl.Int", "SCA", "G.A.1")] %>% 
      pivot_longer(!Player & !Pos & !Squad & !Comp, names_to = "Var", values_to = "value")
    
    figr = plot_ly(data_radar, 
                  type = "scatterpolar",
                  mode = "lines+markers",
                  theta = ~Var,
                  r = ~value,
                  line = list(shape = "spline"),
                  marker = list(symbol = "circle"),
                  hoverinfo = 'text',
                  text = ~paste('</br> Player: ', Player,
                                '</br> Team: ', Squad,
                                '</br> League: ', Comp,
                                '</br> Position: ', Pos),
                  color=~factor(Pos)) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE
          )
        ),
        showlegend = T
      )
    
    figr
    
  })
  # Update VarExp
  output$var_exp = renderDataTable({
    data.frame(desp)
  })
  # Scree Plot for PCA
  output$Scree = renderPlotly({
    p1 <- variances_summary %>% 
      head(input$pc_num) %>% 
      ggplot() +
      geom_col(aes(x = component, y = cumulative_percent_variance), fill = "royalblue") +
      labs(y = "Variance")
    
    p1_plotly <- ggplotly(p1, tooltip = "text") %>%
      style(
        text = paste(
          "PC", variances_summary$component, "<br>",
          "Variance Percentage: ", round(variances_summary$percent_variance, 2), "%<br>",
          "Cumulative Variance Percentage: ", round(variances_summary$cumulative_percent_variance, 2), "%"
        ),
        hoverinfo = "text",
        traces = 1
      )
    p1_plotly
  })
  
  # pc-comp for PCA
  observeEvent(input$pc_comp, {
    if (length(input$pc_comp) > 1) {
      updatePickerInput(session,
                        inputId = "pc_comp",
                        selected = input$pc_comp[1])
    }
  })
  
  # Composition for PCA
  output$Comp = renderPlotly({
    if (length(input$pc_comp) != 1){
      p = ggplot(data=NULL,mapping = aes(x=c(0,1),y=c(0,1)))+
        annotate("text",x=0.5,y=0.5,label="Please select only one PC") + 
        labs(x="", y="") +
        theme(plot.caption = element_text(size = 10))
      ggplotly(p)
    }
    else{
      components_ = components %>%
        filter(component %in% input$pc_comp,
               abs(value) >= 0.1) %>%
        mutate(terms = reorder_within(terms, abs(value), component))
      
      pc = ggplot(components_, aes(value, terms)) +
        geom_col(show.legend = FALSE, fill = "royalblue") +
        scale_y_reordered() +
        labs(y = NULL, x=NULL) +
        theme(axis.text = element_text(size = 7)) +
        ggtitle(input$pc_comp)
      
      pc_plotly <- ggplotly(pc, tooltip = "text") %>%
        style(
          text = paste(
            "Value: ", round(components_$value, 2), "<br>",
            "Meaning: ", components_$exp
          ),
          hoverinfo = "text",
          traces = 1
        )
      pc_plotly
    }
  })
  
  # pc-res for PCA
  observeEvent(input$pc_res, {
    if (length(input$pc_res) > 2) {
      updatePickerInput(session,
                        inputId = "pc_res",
                        selected = input$pc_res[1:2])
    }
  })
  
  # Result for PCA
  output$PCAres = renderPlotly({
    if (length(input$pc_res) != 2){
      p = ggplot(data=NULL,mapping = aes(x=c(0,1),y=c(0,1)))+
        annotate("text",x=0.5,y=0.5,label="Please select exactly two PCs") + 
        labs(x="", y="") +
        theme(plot.caption = element_text(size = 10))
      ggplotly(p)
    }
    else{
      PCs = input$pc_res
      a1 = as.integer(str_split_fixed(PCs[1], "C", 2)[,2])
      a2 = as.integer(str_split_fixed(PCs[2], "C", 2)[,2])
      colors = factor(scores_pca$Pos)
      
      fig = plot_ly(scores_pca, x = scores_pca[,a1+2], y = scores_pca[,a2+2], type="scatter",
                    color = colors, size = 3, mode = "markers",
                    hoverinfo = 'text',
                    text = ~paste('</br> Player: ', Player,
                                  '</br> Position: ', Pos)) %>% 
        layout(showlegend = T, 
               xaxis = list(title = PCs[1]),
               yaxis = list(title = PCs[2]))
      fig
    }
  })
  
  # Hyper for UMAP
  output$UMAPhyper = renderPlotly({
    phyperumap = cc %>%
      filter(ncom == 2) %>%
      mutate(selected_n = factor(ifelse(nei == input$umap_hyper1, 1, 0)),
             selected_m = factor(ifelse(min_d == input$umap_hyper2, 1, 0))) %>%
      ggplot() +
      geom_line(aes(x = cluster, y = value, alpha = selected_n, col = factor(nei),
                    lty = factor(min_d), lwd = selected_m)) +
      facet_wrap(~name, scales = "free_y") +
      scale_x_discrete(limits = 1:10) + 
      scale_alpha_manual(values = c(0.3,1)) +
      scale_linewidth_manual(values = c(0.3,1)) + 
      theme(legend.position = "none")
    
    ggplotly(phyperumap)
  })
  
  # Result of UMAP
  output$UMAPres = renderPlotly({
    set.seed(99)
    umap_rec = get_umap_rec(df[,-c(2:3,5:7)],
                            neib = as.double(input$umap_hyper1),
                            mindist = as.double(input$umap_hyper2),
                            n_comp = 2)
    umap_prep = prep(umap_rec)
    scores_umap = juice(umap_prep)
    
    presumap = ggplot(scores_umap, aes(UMAP1, UMAP2, label = Player)) +
      geom_point(aes(color = Pos), alpha = 0.7, size = 1.5)
    ggplotly(presumap)
  })
  
  # Update Team selection
  observeEvent(input$league, {
    req(input$league)
    x = input$league
    if (is.null(x)){
      x = unique(df$Comp)
    }
    teams = ""
    if (length(x) > 1){      for (i in 1:(length(x)-1)){
      teams = paste0(teams, x[i], sep=", ")
    }
      teams = paste0(teams, x[length(x)])
    }
    else{teams = paste0(teams, x)}
    
    updatePickerInput(session, "team",
                      label = paste("Select Team in ", teams),
                      choices = unique(df %>%
                                         filter(Comp %in% x) %>% 
                                         select(Squad))
    )
  })
  
  # Cluster
  output$Clures = renderPlotly({
    Poss = input$position
    Team = input$team
    if (is.null(Poss)){
      Poss = unique(df$Pos)
    }
    
    players = scores_umap %>%
      mutate(Squad = df$Squad,
             Comp = df$Comp) %>%
      mutate(cluster = factor(km15$cluster),
             selected = factor(ifelse(Squad %in% Team & Pos %in% Poss, 1, 0)))
    pclures = ggplot(mapping = aes(x = UMAP1, y = UMAP2,
                                   col = cluster, shape = Pos)) +
      geom_point(data = players %>% filter(selected == 1), aes(text = paste("Player: ", Player, "<br>",
                                                                            "League: ", Comp, "<br>",
                                                                            "Team: ", Squad, "<br>",
                                                                            "Cluster: ", cluster, "<br>",
                                                                            "Position: ", Pos
      ), size = 1.7, alpha = 2)) +
      geom_point(data = players %>% filter(selected == 0), size = 0.7, alpha = 0.2)
    # scale_color_manual(values = cols[1:5])
    
    pclu_plotly <- ggplotly(pclures, tooltip = "text") 
    pclu_plotly
  })
  
  
  # Player
  output$player = renderDataTable({
    Poss = input$position
    Team = input$team
    if (is.null(Poss)){
      Poss = unique(df$Pos)
    }
    players = scores_umap %>% 
      mutate(cluster = km15$cluster,
             Comp = df$Comp,
             Squad = df$Squad,
             X90s = df$X90s,
             Goals = df$Gls.y,
             Tkl_Int = df$Tkl.Int,
             SCA = df$SCA,
             Pass_Complete = df$Cmp.
             ) %>% 
      filter(Squad %in% Team,
             Pos %in% Poss) %>% 
      select(-UMAP1, -UMAP2)
    
    players
  })
  
  # Boxplot
  output$box = renderPlotly({
    
    pbox = scores_pca %>%
      mutate(cluster = factor(km15$cluster)) %>%
      pivot_longer(PC1:PC9) %>%
      select(Player, Pos, name, value, cluster) %>%
      ggplot() +
      geom_boxplot(aes(x = name, y = value, col=name), outlier.alpha = 0) +
      facet_wrap(~ cluster, nc=2) + 
      theme(legend.position = "none") +
      labs(x="", y="")
    ggplotly(pbox)
  })
}

##### RUN #####
shinyApp(ui, server)
