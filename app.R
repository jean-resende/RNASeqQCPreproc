# --- 1. Carregar Bibliotecas ---
library(shiny)
library(ShortRead)

# --- 2. Interface do Usuário (UI) ---
ui <- fluidPage(
  titlePanel("QC Simples com ShortRead"),
  
  sidebarLayout(
    # Painel Lateral para Inputs
    sidebarPanel(
      h4("1. Carregar Arquivo"),
      fileInput("fastqFile",
                "Selecione o arquivo FASTQ (.fastq ou .fastq.gz)",
                multiple = FALSE, # Apenas um arquivo
                accept = c(".fastq", ".fastq.gz", ".fq", ".fq.gz")),
      
      hr(), # Linha horizontal
      
      h4("2. Executar QC"),
      # Botão para iniciar a análise
      actionButton("runQC", "Rodar Análise de Qualidade", icon = icon("play"))
    ), # Fim sidebarPanel
    
    # Painel Principal para Outputs
    mainPanel(
      h4("Status"),
      verbatimTextOutput("statusOutput"), # Para mostrar mensagens de status/erro
      
      hr(), # Linha horizontal
      
      # Área para o gráfico de qualidade por ciclo
      h4("Qualidade por Ciclo"),
      plotOutput("cycleQualityPlot"),
      
      hr(), # Linha horizontal
      
      # Área para a contagem de reads
      h4("Contagem de Reads"),
      verbatimTextOutput("readCountsOutput")
      
    ) # Fim mainPanel
  ) # Fim sidebarLayout
) # Fim fluidPage (UI)

# --- 3. Lógica do Servidor (Server) ---
server <- function(input, output, session) {
  
  # --- Valores Reativos ---
  # Armazena o resultado do ShortRead::qa
  qa_result_reactive <- reactiveVal(NULL)
  # Armazena o nome do arquivo processado
  file_name_reactive <- reactiveVal("Nenhum arquivo carregado")
  
  # --- Output: Mensagem de Status ---
  output$statusOutput <- renderPrint({
    # Simplesmente imprime o nome do arquivo e se qa_result_reactive tem dados
    fname <- file_name_reactive()
    res <- qa_result_reactive()
    if (is.null(res) && fname == "Nenhum arquivo carregado") {
      cat("Pronto para carregar arquivo.")
    } else if (is.null(res) && fname != "Nenhum arquivo carregado") {
      cat(paste("Processando ou erro em:", fname)) # Mensagem genérica se res for NULL após tentativa
    } else {
      cat(paste("Resultados para:", fname))
    }
  })
  
  # --- Ação: Executar QC quando o botão for clicado ---
  observeEvent(input$runQC, {
    # Requer que um arquivo tenha sido carregado
    req(input$fastqFile)
    
    # Limpa resultados anteriores e atualiza status
    qa_result_reactive(NULL)
    file_info <- input$fastqFile
    file_name_reactive(paste("Processando", file_info$name, "..."))
    showNotification("Iniciando análise QC...", type = "message")
    
    # Tenta executar o ShortRead::qa
    tryCatch({
      print(paste("Iniciando ShortRead::qa para:", file_info$name)) # Log no console R
      # Executa a análise de qualidade
      qa_res <- ShortRead::qa(file_info$datapath, type = "fastq")
      
      # Armazena o resultado se for bem-sucedido
      qa_result_reactive(qa_res)
      file_name_reactive(file_info$name) # Atualiza nome final
      print("ShortRead::qa concluído com sucesso.") # Log no console R
      showNotification("Análise QC concluída!", type = "message")
      
    }, error = function(e) {
      # Em caso de erro durante o qa()
      error_message <- paste("ERRO durante ShortRead::qa():", e$message)
      print(error_message) # Log do erro no console R
      # Atualiza o status para mostrar o erro (simplificado)
      file_name_reactive(paste("Erro em", file_info$name))
      # Limpa qualquer resultado parcial
      qa_result_reactive(NULL)
      # Mostra notificação de erro na UI
      showNotification("Erro durante a análise QC.", type = "error")
      # O output$statusOutput mostrará a mensagem de erro genérica
    }) # Fim do tryCatch
    
  }) # Fim do observeEvent
  
  # --- Output: Gráfico de Qualidade por Ciclo ---
  output$cycleQualityPlot <- renderPlot({
    # Acessa o resultado reativo
    res <- qa_result_reactive()
    qa_res <- ShortRead::qa(file_info$datapath, type = "fastq")
    
    
    # Requer que 'res' não seja NULL (ou seja, qa() rodou com sucesso)
    req(res)
    
    print("Gerando gráfico de Qualidade por Ciclo...") # Log no console R
    tryCatch({
      # Usa a função de plotagem exportada e estável
      plot(ShortRead:::.plotCycleQuality(qa_res))
      title(main = paste("Qualidade por Ciclo -", file_name_reactive()))
      print("Gráfico de Qualidade por Ciclo gerado.") # Log no console R
    }, error = function(e){
      # Em caso de erro na plotagem
      print(paste("ERRO ao gerar gráfico de Qualidade por Ciclo:", e$message))
      plot(1, type="n", main="Erro ao gerar gráfico")
      text(1, 1, "Erro na plotagem", col = "red")
    })
  }) # Fim do renderPlot cycleQualityPlot
  
  # --- Output: Tabela de Contagem de Reads ---
  output$readCountsOutput <- renderPrint({
    # Acessa o resultado reativo
    res <- qa_result_reactive()
    
    # Requer que 'res' não seja NULL
    req(res)
    
    print("Gerando tabela de Contagem de Reads...") # Log no console R
    tryCatch({
      # Usa a função acessora exportada e estável
      #ShortRead::readCounts(res)
      res[["readCounts"]]
    }, error = function(e){
      # Em caso de erro ao acessar readCounts
      print(paste("ERRO ao gerar contagem de reads:", e$message))
      "Erro ao obter contagem de reads."
    })
  }) # Fim do renderPrint readCountsOutput
  
} # Fim do Server

# --- 4. Rodar o Aplicativo ---
shinyApp(ui = ui, server = server)