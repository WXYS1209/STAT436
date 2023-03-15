library(tidyverse)
library(shiny)
library(RColorBrewer)
library(shinyWidgets)
library(plotly)

############### Data Analysis ###############
df = read.csv("https://raw.githubusercontent.com/WXYS1209/STAT436/main/2021-2022%20Football%20Player%20Stats.csv",
              sep=";")
dff = read.csv("https://raw.githubusercontent.com/WXYS1209/STAT436/main/correction.txt",
               header = 2)

df$Player = dff$Player
df$Squad = dff$Squad
df$Pos = dff$Pos
df$Nation = dff$Nation
df$Comp = dff$Comp

data_new = df[,c(-3,-8,-7)]
dup = which(duplicated(data_new[,2]))
data_new[dup-1, -1:-5] = data_new[dup-1, -1:-5] + data_new[dup, -1:-5]
data_new = data_new[!duplicated(data_new[,2]),]
data_new$Pos = substr(data_new$Pos, 1, 2)
data_new$Pos[which(data_new$Pos=="")] = "MF"
data_new$Comp = str_split_fixed(data_new$Comp, " ", 2)[,2]

X_prcomp = prcomp(data_new[,-1:-5], center = TRUE, 
                  scale. = TRUE,
                  retx = TRUE)
# summary(X_prcomp)
prop = X_prcomp$rotation[,1:20]

T10 = data.frame(matrix(nr=30))
for (j in 1:20){
  t10 = prop[order(abs(prop[,j]), decreasing = T), j][1:30]
  T10[,1+2*(j-1)] = names(t10)
  T10[,2*j] = t10
  colnames(T10)[1+2*(j-1)] = "Variables"
  colnames(T10)[2*j] = paste0("PC", j)
}

x_scores = X_prcomp$x
rownames(x_scores) = data_new$Player
x_scores = data.frame(x_scores)

km14 = kmeans(x_scores[,1:20], centers = 4, iter.max = 100, algorithm = "MacQueen", nstart = 4)

EXPL = c("Player's name", "Position", "Squad??s name", "League that squad occupies", 
         "Matches played", "Matches started", "Minutes played", "Minutes played divided by 90", 
                "Goals scored or allowed", "Shots total", "Shots on target", 
                "Shots on target percentage", "Goals per shot", 
                "Goals per shot on target", "Average distance, in yards, from goal of all shots taken", 
                "Shots from free kicks", "Penalty kicks made", "Penalty kicks attempted", 
                "Passes completed", "Passes attempted", "Pass completion percentage", 
                "Total distance, in yards, that completed passes have traveled in any direction", 
                "Total distance, in yards, that completed passes have traveled towards the opponent's goal", 
                "Passes completed (Passes between 5 and 15 yards)", "Passes attempted (Passes between 5 and 15 yards)", 
                "Pass completion percentage (Passes between 5 and 15 yards)", "Passes completed (Passes between 15 and 30 yards)", 
                "Passes attempted (Passes between 15 and 30 yards)", 
                "Pass completion percentage (Passes between 15 and 30 yards)", 
                "Passes completed (Passes longer than 30 yards)", 
                "Passes attempted (Passes longer than 30 yards)", 
                "Pass completion percentage (Passes longer than 30 yards)",
                "Assists", "Passes that directly lead to a shot (assisted shots)", 
                "Completed passes that enter the 1/3 of the pitch closest to the goal", 
                "Completed passes into the 18-yard box", "Completed crosses into the 18-yard box", 
                "Completed passes that move the ball towards the opponent's goal at least 10 yards from its furthest point in the last six passes, or any completed pass into the penalty area", 
                "Passes attempted", "Live-ball passes", "Dead-ball passes", "Passes attempted from free kicks", 
                "Completed pass sent between back defenders into open space", 
                "Passes made while under pressure from opponent", 
                "Passes that travel more than 40 yards of the width of the pitch", 
                "Crosses", "Corner kicks", "Inswinging corner kicks", "Outswinging corner kicks", 
                "Straight corner kicks", "Ground passes", 
                "Passes that leave the ground, but stay below shoulder-level", 
                "Passes that are above shoulder-level at the peak height", 
                "Passes attempted using left foot", "Passes attempted using right foot", 
                "Passes attempted using head", "Throw-ins taken", 
                "Passes attempted using body parts other than the player's head or feet", 
                "Passes completed", "Offsides", "Out of bounds", "Passes intercepted", 
                "Blocked by the opponent who was standing it the path", "Shot-creating actions", 
                "Completed live-ball passes that lead to a shot attempt", 
                "Completed dead-ball passes that lead to a shot attempt", 
                "Successful dribbles that lead to a shot attempt", 
                "Shots that lead to another shot attempt", 
                "Fouls drawn that lead to a shot attempt", 
                "Defensive actions that lead to a shot attempt", "Goal-creating actions", 
                "Completed live-ball passes that lead to a goal", 
                "Completed dead-ball passes that lead to a goal", 
                "Successful dribbles that lead to a goal", 
                "Shots that lead to another goal-scoring shot", "Fouls drawn that lead to a goal", 
                "Defensive actions that lead to a goal", "Number of players tackled", 
                "Tackles in which the tackler's team won possession of the ball", 
                "Tackles in defensive 1/3", "Tackles in middle 1/3", "Tackles in attacking 1/3",
                "Number of dribblers tackled", "Number of times dribbled past plus number of tackles", 
                "Percentage of dribblers tackled", "Number of times dribbled past by an opposing player", 
                "Number of times applying pressure to opposing player who is receiving, carrying or releasing the ball",
                "Number of times the squad gained possession withing five seconds of applying pressure", 
                "Percentage of time the squad gained possession withing five seconds of applying pressure",
                "Number of times applying pressure to opposing player who is receiving, carrying or releasing the ball, in the defensive 1/3", 
                "Number of times applying pressure to opposing player who is receiving, carrying or releasing the ball, in the middle 1/3", 
                "Number of times applying pressure to opposing player who is receiving, carrying or releasing the ball, in the attacking 1/3", 
                "Number of times blocking the ball by standing in its path", "Number of times blocking a shot by standing in its path", 
                "Number of times blocking a shot that was on target, by standing in its path", 
                "Number of times blocking a pass by standing in its path", "Interceptions", 
                "Number of players tackled plus number of interceptions", "Clearances", 
                "Mistakes leading to an opponent's shot",
                "Number of times a player touched the ball. Note: Receiving a pass, then dribbling, then sending a pass counts as one touch", 
                "Touches in defensive penalty area", "Touches in defensive 1/3", "Touches in middle 1/3", "Touches in attacking 1/3", 
                "Touches in attacking penalty area", "Live-ball touches. Does not include corner kicks, free kicks, throw-ins, kick-offs, goal kicks or penalty kicks.", 
                "Dribbles completed successfully", "Dribbles attempted", "Percentage of dribbles completed successfully", "Number of players dribbled past",
                "Number of times a player dribbled the ball through an opposing player's legs", 
                "Number of times the player controlled the ball with their feet",
                "Total distance, in yards, a player moved the ball while controlling it with their feet, in any direction", 
                "Total distance, in yards, a player moved the ball while controlling it with their feet towards the opponent's goal", 
                "Carries that move the ball towards the opponent's goal at least 5 yards, or any carry into the penalty area", 
                "Carries that enter the 1/3 of the pitch closest to the goal", "Carries into the 18-yard box", 
                "Number of times a player failed when attempting to gain control of a ball",
                "Number of times a player loses control of the ball after being tackled by an opposing player", 
                "Number of times a player was the target of an attempted pass", "Number of times a player successfully received a pass", 
                "Percentage of time a player successfully received a pass", "Completed passes that move the ball towards the opponent's goal at least 10 yards from its furthest point in the last six passes, or any completed pass into the penalty area", 
                "Yellow cards", "Red cards", "Second yellow card", "Fouls committed", "Fouls drawn", "Offsides", "Crosses", 
                "Tackles in which the tackler's team won possession of the ball", "Penalty kicks won",
                "Penalty kicks conceded", "Own goals", "Number of loose balls recovered", "Aerials won", 
                "Aerials lost", "Percentage of aerials won")



############## Shiny ############## 

ui <- fluidPage(
  titlePanel("2021-22 Season Top 5 European Leagues Player Classification"),
  helpText("PCA and k-Mean method are used to obtain the classification."),
  sidebarLayout(
    sidebarPanel(
      helpText(h5("Instruction:")),
      helpText("Please select League, Team, Position and Pricipal Components",
               "that you are willing to check."),
      helpText("If nothing has been chose, every choices will be selected (except Team and PCs).\n"),
      helpText("For the PCs, choose 2 to see the plots.\n"),
      helpText("Hover over each dot to see details.\n"),
      helpText("Click the legend to filter points."),
      pickerInput(inputId = "league",
                  label = "Select League",
                  choices = unique(data_new$Comp),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      pickerInput(inputId = "team",
                  label = "Select Team",
                  choices = unique(data_new$Squad),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `live-search`=TRUE)),
      pickerInput(inputId = "position",
                  label = "Select Position",
                  choices = unique(data_new$Pos),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      selectizeInput(inputId = "principal",
                  label = "Select Principal Component",
                  choices = paste0("PC ", 1:20),
                  multiple = TRUE,
                  options = list(maxItems = 2),
                  selected = c("PC 1", "PC 2"))
      ),
    mainPanel(
      fluidRow(
        column(5, h3("Radar Chart of Players"),
               plotlyOutput("star"), height="600px"),
        column(7, h3("Classification based on PCs"),
               plotlyOutput("pca", height = "600px"))),
      fluidRow(
        column(width = 6, h3("Variable Explanations"),
               dataTableOutput("var_exp")),
        column(width = 6, h3("Principal Components Composition"),
               dataTableOutput("pc_explain"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Update Team selection
  observe({
    x = input$league
    if (is.null(x)){
      x = unique(data_new$Comp)
    }
    teams = ""
    if (length(x) > 1){
      for (i in 1:(length(x)-1)){
        teams = paste0(teams, x[i], sep=", ")
      }
      teams = paste0(teams, x[length(x)])
    }
    else{teams = paste0(teams, x)}
    
    updatePickerInput(session, "team",
                      label = paste("Select Team in ", teams),
                      choices = unique(data_new %>%
                        filter(Comp %in% x) %>% 
                        select(Squad))
                      )
  })
  
  # Update VarExp
  output$var_exp = renderDataTable({
    data.frame(Variable = colnames(data_new[-1:-5]), Explanation = EXPL[-1:-4])
  })
  
  # Update PCExplain
  output$pc_explain = renderDataTable({
    PCs = input$principal
    a1 = as.integer(str_split_fixed(PCs[1], " ", 2)[,2])
    a2 = as.integer(str_split_fixed(PCs[2], " ", 2)[,2])
    data.frame(Variables = rownames(X_prcomp$rotation), round(X_prcomp$rotation[,c(a1,a2)], 4))
    
  })

  # Update Plotly
  output$pca = renderPlotly({
    Poss = input$position
    Team = input$team
    if (is.null(Poss)){
      Poss = unique(data_new$Pos)
    }
    PCs = input$principal
    if (length(PCs) == 2){
      a1 = as.integer(str_split_fixed(PCs[1], " ", 2)[,2])
      a2 = as.integer(str_split_fixed(PCs[2], " ", 2)[,2])
      wanted = (rownames(x_scores) %in% data_new[data_new$Squad %in% Team,2]) &
        (rownames(x_scores) %in% data_new[data_new$Pos %in% Poss,2])
      
      score = x_scores[wanted,]
      colors = factor(km14$cluster[names(km14$cluster) %in% rownames(score)])
      shapes = factor(data_new[wanted, 3]) # Pos

      score = cbind(score, data_new[wanted, 2:5])
      
      fig = plot_ly(score, x = score[,a1], y = score[,a2], type="scatter",
                    color = colors, size = 3, symbol = shapes, mode = "markers",
                    hoverinfo = 'text',
                    text = ~paste('</br> Player: ', Player,
                                  '</br> Team: ', Squad,
                                  '</br> League: ', Comp,
                                  '</br> Position: ', Pos)) %>% 
        layout(showlegend = T, 
               xaxis = list(title = PCs[1]),
               yaxis = list(title = PCs[2]),
               legend = list(title=list(text='<b> Classification </b>')))
      fig
      
    }
  })
  
  # Update Radar
  output$star = renderPlotly({
    Poss = input$position
    Team = input$team
    if (is.null(Poss)){
      Poss = unique(data_new$Pos)
    }
    wanted = (rownames(x_scores) %in% data_new[data_new$Squad %in% Team,2]) &
      (rownames(x_scores) %in% data_new[data_new$Pos %in% Poss,2])
      
    score = x_scores[wanted,]
    colorss = km14$cluster[names(km14$cluster) %in% rownames(score)]

    data_radar = data_new[wanted, c("Player", "Pos", "Squad", "Comp",
                                    "Goals", "PasProg", "Tkl", "PasAss")] %>% 
      cbind(colorss) %>% 
      pivot_longer(!Player & !Pos & !Squad & !Comp & !colorss, names_to = "Var", values_to = "value")
    
    fig = plot_ly(data_radar, 
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
            color=~factor(colorss)) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE
          )
        ),
        showlegend = T,
        legend = list(title=list(text='<b> Classification </b>'))
      )
    
    fig

  })
}

shinyApp(ui, server)
