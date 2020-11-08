options( width = 10000 )

navbarPage( "ImbTreeEntropy",
            
  #### Description ####                
  tabPanel( "Description",
    
    fluidPage(
      h2("An R package for building entropy based classification trees on the imbalanced datasets"),
      
      br(),
      
      h3("Description"),
      "An R package for building binary and multiclass decision tree algorithms using generalized entropy functions, 
      such as Renyi, Tsallis, Sharma-Mittal, Sharma-Taneja and Kapur, to measure impurity of a node. 
      These are important extensions of the existing algorithms which usually employ Shannon entropy and the concept 
      of information gain. Additionally, ImbTreeEntropy is able to handle imbalanced data which is a challenging issue 
      in many practical applications. The package supports cost-sensitive learning by defining a misclassification 
      cost matrix and weight sensitive learning. It accepts all types of attributes, including continuous, 
      ordered and nominal.",
      br(),
      
      h3("Author"),
      
      a(href="http://krzysztof_gajowniczek.users.sggw.pl/", "Krzysztof Gajowniczek, PhD"),
      h3("Uploading Files"),
      
      p(strong("data:"), "Data.frame in which to interpret the parameters Target and Attributes.
      Accepted extensions: text/csv, text/comma-separated-values,text/plain, .csv, .arff"),
      h3("Fit Model"),
      
      p(strong("Target:"), "Name of the target variable. Character vector of one element."),
      p(strong("Attributes:"), "Attribute names used for target (Target) modelling. Character vector of many elements."),
      p(strong("Method:"), "Method used for learning. Character vector of one element with one of the: 
        Shannon, Renyi, Tsallis, Sharma-Mittal, Sharma-Taneja, Kapur."),
      p(strong("Depth:"), "Set the maximum depth of any node of the final tree, with the root node counted as depth 0. 
        Numeric vector of one element which is greater or equal to 1."),
      p(strong("Min obs:"), "The minimum number of observations that must exist in any terminal node (leaf). 
        Numeric vector of one element which is greater or equal to 1."),
      p(strong("Q:"), "Numeric vector specifying parameter for the following entropies: Renyi, Tsallis."),
      p(strong("Alpha:"), "Numeric vector specifying parameter for the following entropies: Sharma-Mittal, Sharma-Taneja, Kapur."),
      p(strong("Beta:"), "Numeric vector specifying parameter for the following entropies: Sharma-Mittal, Sharma-Taneja, Kapur."),
      p(strong("Overfitting method:"), "Character vector of one element with one of the: none, leafcut, prune, avoid 
        specifying which method overcoming overfitting should be used. leafcut method is used when the full tree is built, 
        it reduces the subtree when both siblings choose the same class label. avoid method is incorporated during 
        the recursive partitioning, it prohibit the split when both sibling chose the same class. 
        prune method employs pessimistic error pruning procedure, it should be specified along with the cf parameter."),
      p(strong("Cp:"), "Complexity parameter, i.e. any split that does not decrease the overall lack of fit by a factor of cp is not attempted.
        It refers to miss-classification error. If cost or weights are specified aforementioned measure takes these parameter into account. 
        Works for overfitting methods: none, leafcut, avoid. Numeric vector of one element which is greater or equal to 0."),
      p(strong("Cf:"), "Numeric vector of one element with the number in (0, 0.5] for the optional pessimistic-error-rate-based pruning step"),
      p(strong("Weights:"), "Numeric vector of cases weights. It should have as many elements as the number of observation in the data.frame passed to the data parameter."),
      p(strong("Cost:"), "Matrix of costs associated with the possible errors. The matrix should have k columns and rows, 
        where k is the number of class levels. Rows contain true classes while columns contain predicted classes. 
        Rows and columns names should take all possible categories (labels) of the target variable."),
      p(strong("Class threshold:"), "Method used for determining thresholds based on which the final class for each node is derived. 
        If cost is specified it can take one of the following: theoretical, tuned, otherwise it takes equal. Character vector of one element."),
      p(strong("Ambiguous probability:"), "Ambiguity threshold for the difference between the highest class probability and the second highest class 
        probability per node, below which the expert has to make a decision regarding the future tree structure. 
        Logical vector with one element. It works when the Ambiguous class parameter is NULL."),
      p(strong("Top splits:"), "Number of best splits, i.e. final trees structure to be presented. 
        Splits are sorted in descending order according to the information gain. Numeric vector with one element."),
      p(strong("Attribute level:"), "Decision indicating whether possible best splits are derived on 
        the attribute level (higher) or on the split point for each attribute (lower). 
        TRUE means that the expert gets the best splits, one for each variable. 
        FALSE means the best splits at all where it might happen that the expert receives Top splits splits coming from only one variable. 
        Logical vector with one element."),
      p(strong("Ambiguous class:"), "Labels of classes for which the expert will make a decision during the learning. 
        Character vector of many elements (from 1 up to number of classes). 
        Should have the same number of elements as vector passed to the Ambiguous class frequencies parameter."),
      p(strong("Ambiguous class frequencies:"), "Classes frequencies per node above which the expert will make a 
        decision. Numeric vector of many elements (from 1 up to number of classes). 
        Should have the same number of elements as vector passed to the Ambiguous class parameter."),
      h3("Accuracy"),
      
      "Accuracy measures.",
      
      h3("Rules"),
      
      "Data frame with the extracted decision rules along with the following performance measures: Support, Confidence, Lift, Conviction, AddedValue, Cosine, Jaccard, Laplace, Leverage."
    
    )
  ),
  
  #### Static Model ####
  tabPanel("Static Model",
           
   shinyjs::useShinyjs(),
   
    tabsetPanel(
      
      tabPanel( "Uploading Files", 
                
        sidebarLayout(
          
          sidebarPanel(
            
            fileInput("fileS", "Choose Table", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".arff")),
            
            hr(),
            
            checkboxInput("headerS", "Header", TRUE),
            
            radioButtons("sepS", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
            
            radioButtons("quoteS", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
            
          ),
          
          mainPanel(
            
            tableOutput("InputFileS")
            
          )
          
        )
        
      ),
      
      tabPanel( "Fit Model",
                
        sidebarLayout(
          
          sidebarPanel(
            
            actionButton( "startS", "Start Learning"),
            
            hr(),
            
            textOutput("wrongTargetS"),
            
            selectInput("Y_nameS", "Target", ""),
            
            selectInput("X_namesS", "Attributes", "", multiple = TRUE),
            
            selectInput("typeS", "Method", choices = list("Shannon", "Renyi", "Tsallis", "Sharma-Mittal", "Sharma-Taneja", "Kapur")),
            
            uiOutput("paramsS"),
            
            numericInput("depthS", "Depth", value = 5, 1, 32, 1),
            
            numericInput("min_obsS", "Min obs", value = 10, 1, Inf, 1),
            
            selectInput("overfitS", "Overfitting method", choices = list("none", "leafcut", "prune", "avoid"), selected = "leafcut"),
            
            uiOutput("overfittingS"),
            
            selectInput("class_thS", "Classification threshold", choices = list("equal", "theoretical", "tuned"), selected = "equal"),
            
            selectInput("weightscostS", "Cost-sensitive type", choices = list("none", "weights", "cost matrix"), selected = "none"),
            
            div(

              id = "AreaWeightsS",
              selectInput("WeightsS", "Weights", ""),
              textOutput("wrongWeightsS")
              
            )
            
          ),
          
          mainPanel(
            
            tabsetPanel(id = "tabsestS",
              
              tabPanel( "Tree",   
                        
                br(),
                        
                shinyjs::hidden(div(
                  
                  id = "AreaSaveS",
                  downloadButton("saveS", "Save Tree")
                  
                )),
                
                hr(),
                
                verbatimTextOutput("plotS")
                
              
              ),

              tabPanel( "Cost matrix", 
                        
                DTOutput( "CostMatS" )
                        
              )

            )
          
          )
        
        )
        
      ),
      
      
      tabPanel("Accuracy",
          
        verbatimTextOutput("AccuracyS")
        
      ),

      tabPanel("Rules",
          
        br(),
        
        shinyjs::hidden(div(
          
          id = "AreaRulesS",
          downloadButton("saveRulesS", "Save Rules")
          
        )),
        
        hr(),
        
        verbatimTextOutput("RulesS")
        
      )
      
    )
   
  ),
  
  #### Tune Model ####
  tabPanel("Tune Static Model",
           
  shinyjs::useShinyjs(), 
  
   tabsetPanel(
     
     tabPanel( "Uploading Files", 
               
       sidebarLayout(
         
         sidebarPanel(
           
           
           fileInput("fileT", "Choose Table", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".arff")),
           
           hr(),
           
           checkboxInput("headerT", "Header", TRUE),
           
           radioButtons("sepT", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
           
           radioButtons("quoteT", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
           
         ),
         
         mainPanel(
           
           tableOutput("InputFileT")
           
         )
         
       )
       
     ),
     
     tabPanel( "Fit Models",
               
       sidebarLayout(
         
         sidebarPanel(
           
           actionButton( "startT", "Start Learning"),
           
           hr(),
           
           numericInput("kfoldT", "Number of Folds", value = 10, 2, 100, 1),
           
           numericInput("seedT", "Seed for PRNG", value = 666, 1, .Machine$integer.max, 1),
           
           hr(),
           
           textOutput("wrongTargetT"),
           
           selectInput("Y_nameT", "Target", ""),
           
           selectInput("X_namesT", "Attributes", "", multiple = TRUE),
           
           selectInput("typeT", "Method", multiple = TRUE, choices = list("Shannon", "Renyi", "Tsallis", "Sharma-Mittal", "Sharma-Taneja", "Kapur")),
           
           numericInput("stepT", "Step for Q, Alpha and Beta", value = 0.1, 0.001, 10, 0.001),
           
           sliderInput("qT", "Q", -10, 10, c(1,1), 0.001),
           
           sliderInput("aT", "Alpha", -10, 10, c(1,1), 0.001),
           
           sliderInput("bT", "Beta", -10, 10, c(1,1), 0.001),
           
           sliderInput("depthT", "Depth", 1, 32, c(5,5), 1),
           
           sliderInput("min_obsT", "Min obs", 1, Inf, c(5,5), 1),
           
           selectInput("overfitT", "Overfitting method", multiple = TRUE, choices = list("none", "leafcut", "prune", "avoid"), selected = "leafcut"),
           
           numericInput("stepcpT", "Step for Cp", value = 0.1, 0.001, 1, 0.001),
           
           sliderInput("cpT", "Cp", 0, 1, c(0,0), 0.001),
           
           numericInput("stepcfT", "Step for Cf", value = 0.05, 0.001, 1, 0.001),
           
           sliderInput("cfT", "Confidence intervals", 0.001, 0.5, c(0.25,0.25), 0.001),
           
           selectInput("class_thT", "Classification threshold", choices = list("equal", "theoretical", "tuned"), selected = "equal"),
           
           selectInput("weightscostT", "Cost-sensitive type", choices = list("none", "weights", "cost matrix"), selected = "none"),
           
           div(
             
             id = "AreaWeightsT",
             selectInput("WeightsT", "Weights", ""),
             textOutput("wrongWeightsT")
             
           )
           
         ),
         
         mainPanel(
           
           tabsetPanel( id = "tabsestT", 
             
             tabPanel( "Cost matrix", 
                   
                DTOutput( "CostMatT" )
                   
             ),
             
             tabPanel( "Train Average",
                       
               hr(),
               
               shinyjs::hidden(div(
                 
                 id = "AreaTrainAggT",
                 downloadButton("saveTrainAggT", "Save Results")
                 
               )),
               
               hr(),
               
               DTOutput("TrainAggT")
               
             ),
             
             tabPanel( "Valid Average",
                       
               hr(),
               
               shinyjs::hidden(div(
                 
                 id = "AreaValidAggT",
                 downloadButton("saveValidAggT", "Save Results")
                 
               )),
               
               hr(),
               
               DTOutput("ValidAggT")
               
             ),
             
             tabPanel( "Train Detailed",
                       
               hr(),
               
               shinyjs::hidden(div(
                 
                 id = "AreaTrainT",
                 downloadButton("saveTrainT", "Save Results")
                 
               )),
               
               hr(),
               
               DTOutput("TrainT")
               
             ),
             
             tabPanel( "Valid Detailed",
                       
               hr(),
               
               shinyjs::hidden(div(
                 
                 id = "AreaValidT",
                 downloadButton("saveValidT", "Save Results")
                 
               )),
               
               hr(),
               
               DTOutput("ValidT")
               
             )
             
           )
           
         )
         
       )
       
     )
     
   )
   
  ),
  
  #### Interactive Model ####
  tabPanel( "Interactive Model",
            
    shinyjs::useShinyjs(),
    
    tabsetPanel(
      
      tabPanel( "Uploading Files",
                
        sidebarLayout(
          
          sidebarPanel(
            
            fileInput("fileI", "Choose Table", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".arff")),
            
            hr(),
            
            checkboxInput("headerI", "Header", TRUE),
            
            radioButtons("sepI", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
            
            radioButtons("quoteI", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
          ),
          
          mainPanel(
            
            tableOutput("InputFileI")
            
          )
          
        )
        
      ),

      tabPanel( "Fit Model",
                
        sidebarLayout(
          
          sidebarPanel(
            
            actionButton( "startI", "Start Learning"),
            
            hr(),
            
            shinyjs::hidden(div(
              
              id = "decisionAreaTree",
              uiOutput("Ntree"),
              actionButton("decisionI","Confirm Decision and Resume Learning"),
              
            )),
            
            hr(),
            
            textOutput("wrongTargetI"),
            
            selectInput("Y_nameI", "Target", ""),
            
            selectInput("X_namesI", "Attributes", "", multiple = TRUE),
            
            selectInput("typeI", "Method", choices = list("Shannon", "Renyi", "Tsallis", "Sharma-Mittal", "Sharma-Taneja", "Kapur")),
            
            uiOutput("paramsI"),
            
            numericInput("depthI", "Depth", value = 5, 1, 32, 1),
            
            numericInput("min_obsI", "Min obs", value = 10, 1, Inf, 1),
            
            numericInput("top_splitI", "Top splits", value = 2, 1, 10, 1),
            
            selectInput("attr_levI", "Attribute level", choices = list("TRUE", "FALSE"), selected = "TRUE"),
            
            selectInput("amb_decI", "Ambiguity type", choices = list("Probability", "Class"), selected = "Probability"),
            
            uiOutput("amb_typeI"),
            
            selectInput("overfitI", "Overfitting method", choices = list("none", "leafcut", "prune", "avoid"), selected = "leafcut"),
            
            uiOutput("overfittingI"),
            
            selectInput("class_thI", "Classification threshold", choices = list("equal", "theoretical", "tuned"), selected = "equal"),
            
            selectInput("weightscostI", "Cost-sensitive type", choices = list("none", "weights", "cost matrix"), selected = "none"),
            
            div(
              
              id = "AreaWeightsI",
              selectInput("WeightsI", "Weights", ""),
              textOutput("wrongWeightsI")
              
            )
            
          ),
          
          mainPanel(
            
            tabsetPanel( id = "tabsestI",
                         
              tabPanel( "Tree", 
                        
                br(),
                
                shinyjs::hidden(div(
                  
                  id = "AreaSaveI",
                  downloadButton("saveI", "Save Tree")
                  
                )),
                
                hr(),
                
                verbatimTextOutput("plotI")
                
              ),
              
              tabPanel( "Cost matrix", 
                        
                DTOutput( "CostMatI" )
                        
              ),
              
              tabPanel( "Ambiguity matrix", 
                        
                DTOutput( "AmbMatI" )
                        
              )
              
            )
            
          )
          
        )
        
      ),

      tabPanel("Accuracy",
          
        verbatimTextOutput("AccuracyI")
        
      ),

      tabPanel("Rules",
        
        br(),
               
        shinyjs::hidden(div(
          
          id = "AreaRulesI",
          downloadButton("saveRulesI", "Save Rules")
          
        )),
        
        hr(),
        
        verbatimTextOutput("RulesI")
        
      )
      
    )
    
  ),
  
  #### Predict ####
  tabPanel( "Predict New Data",
            
    tabsetPanel(
      
      tabPanel( "Uploading Data",
                
        sidebarLayout(
          
          sidebarPanel(
            
            fileInput("fileP", "Choose Table", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".arff")),
            
            hr(),
            
            checkboxInput("headerP", "Header", TRUE),
            
            radioButtons("sepP", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
            
            radioButtons("quoteP", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
            
          ),
          
          mainPanel(
            
            tableOutput("InputFileP")
            
          )
          
        )
        
      ),
      
      tabPanel( "Uploading Tree",
                
        sidebarLayout(
          
          sidebarPanel(
            
            fileInput("treeP", "Choose Tree", multiple = TRUE, accept = ".rds")
            
          ),
          mainPanel(
            
            verbatimTextOutput("plotP")
            
          )
          
        )
        
      ),
      
      tabPanel("Predict Data",
               
        fluidPage(
          
          br(),
          
          actionButton("startP", "Predict observations"),
          
          hr(),
          
          shinyjs::hidden(div(
            
            id = "AreaSaveP",
            downloadButton("saveP", "Save Table")
            
          )),
          
          hr(),
          
          tableOutput("Pred")
          
        )
        
      )
    
    )
    
  )
  
)
