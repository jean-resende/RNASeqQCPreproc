# --- 1. Carregar Bibliotecas Essenciais ---
library(shiny)
# Certifique-se de ter instalado via BiocManager: install.packages("BiocManager"); BiocManager::install("ShortRead")
library(ShortRead)
# ggplot2 é carregado para potencial uso futuro, mas não nas plotagens de depuração atuais
library(ggplot2)

# --- 2. Definir a Interface do Usuário (UI) ---
ui <- fluidPage(
    titlePanel("RNASeqQCPreproc - Ferramenta de QC e Pré-processamento (Versão Depuração)"), # Título do App

    sidebarLayout(
        sidebarPanel(
            h4("1. Carregar Dados"),
            # Input: Selecionar arquivos FASTQ
            fileInput("fastqFiles",
                      "Selecione UM arquivo FASTQ (.fastq ou .fastq.gz)", # Alterado para UM por simplicidade na depuração
                      multiple = FALSE, # Alterado para FALSE para simplificar a depuração inicial
                      accept = c(".fastq", ".fastq.gz", ".fq", ".fq.gz")),

            hr(),

            h4("2. Iniciar Análise"),
            # Botão para iniciar o processamento
            actionButton("runAnalysis", "Executar QC (ShortRead)", icon = icon("play")),

            hr(),

            h4("3. Download (a implementar)"),
            tags$p("Opções de download aparecerão aqui após o processamento.")

        ), # Fim do sidebarPanel

        mainPanel(
            h3("Resultados"),
            # Abas para organizar os resultados
            tabsetPanel(type = "tabs",
                tabPanel("Resumo Geral",
                         h4("Arquivo Carregado:"),
                         verbatimTextOutput("fileSummary"),
                         hr(),
                         h4("Status do Processamento:"),
                         verbatimTextOutput("processStatus")
                         ),
                tabPanel("Relatório de QC (ShortRead)",
                         h4("Estatísticas Gerais de QC"),
                         verbatimTextOutput("qcStats"), # Output para estatísticas
                         hr(),
                         h4("Qualidade por Ciclo"), # Subtítulo do Gráfico 1
                         plotOutput("qcPlotCycleQuality"), # Output para gráfico de qualidade por ciclo
                         hr(),
                         h4("Qualidade Média por Read"), # Subtítulo do Gráfico 2
                         plotOutput("qcPlotReadQuality")  # Output para gráfico de qualidade por read
                         ),
                tabPanel("Resultados Pré-processamento",
                         h4("Sumário do Pré-processamento"),
                         verbatimTextOutput("preprocessSummary") # Placeholder
                         )
            ) # Fim do tabsetPanel
        ) # Fim do mainPanel
    ) # Fim do sidebarLayout
) # Fim do fluidPage (UI)

# --- 3. Definir a Lógica do Servidor (Server) ---
server <- function(input, output, session) {

    # --- Reatividade para Arquivos Carregados ---
    output$fileSummary <- renderPrint({
        req(input$fastqFiles) # Requer que arquivo seja carregado
        df <- input$fastqFiles[, c("name", "size", "type")]
        colnames(df) <- c("Nome", "Tamanho (bytes)", "Tipo")
        print("--- renderPrint fileSummary: Exibindo resumo do arquivo ---")
        print(df)
    })

    # --- Armazenar resultados do QA e nome do arquivo ---
    qa_results <- reactiveVal(NULL)
    processed_file_name <- reactiveVal("")

    # --- Reatividade para o Botão de Análise ---
    observeEvent(input$runAnalysis, {
        req(input$fastqFiles) # Requer arquivo antes de rodar

        # Limpar resultados anteriores e status
        qa_results(NULL)
        processed_file_name("")
        # Limpa outputs explicitamente para evitar mostrar resultados antigos em caso de erro
        output$processStatus <- renderPrint({ "Iniciando análise..." })
        output$qcStats <- renderPrint({ "" })
        output$qcPlotCycleQuality <- renderPlot({ NULL })
        output$qcPlotReadQuality <- renderPlot({ NULL })
        output$preprocessSummary <- renderPrint({ "" })

        # Obter info do arquivo (agora só um)
        file_info <- input$fastqFiles
        filePath <- file_info$datapath
        fileName <- file_info$name

        processed_file_name(fileName) # Guarda o nome do arquivo

        # --- Lógica de QC com ShortRead ---
        tryCatch({
            # Notificação na UI e mensagem no console
            msg_inicio <- paste("Iniciando QC com ShortRead para:", fileName)
            showNotification(msg_inicio, type = "message", duration = 5)
            print(paste("--- observeEvent:", msg_inicio, "---")) # Console

            # Executar o QA
            qa_obj <- ShortRead::qa(filePath, type="fastq")

            print(paste("--- observeEvent: ShortRead::qa concluído para:", fileName, "---")) # Console

            # Armazenar o resultado no reactiveVal
            qa_results(qa_obj)
            print("--- observeEvent: Resultado armazenado em qa_results ---") # Console

            # Atualizar Status na UI
            output$processStatus <- renderPrint({ paste("Análise de QC (ShortRead) concluída para:", fileName) })
            showNotification("QC concluído!", type = "message", duration = 5)

        }, error = function(e) {
            # Em caso de erro durante o QA
            error_message <- paste("!!! ERRO durante ShortRead::qa para", fileName, ":\n", e$message)
            print(error_message) # Console
            output$processStatus <- renderPrint({ error_message }) # UI
            showNotification("Erro no processamento de QC.", type = "error", duration = 10)
            qa_results(NULL) # Garante que resultados inválidos não sejam usados
        }) # Fim do tryCatch para QA

        # --- Lógica de Pré-processamento (Placeholder) ---
        output$preprocessSummary <- renderPrint({ "Lógica de pré-processamento ainda não implementada."})

    }) # Fim do observeEvent runAnalysis


    # --- Renderizar Outputs de QC (ShortRead) ---

    # Output para Estatísticas de Texto
    output$qcStats <- renderPrint({
        res <- qa_results() # Acessa o resultado armazenado
        fname <- processed_file_name()

        # Mensagens de depuração para o console
        print("--- renderPrint qcStats: Verificando 'res' e 'fname' ---")
        print(paste("Classe de res:", ifelse(is.null(res), "NULL", class(res))))
        print(paste("Nome do arquivo:", ifelse(fname == "", "VAZIO", fname)))

        req(res, fname) # Requer que 'res' e 'fname' não sejam NULL/vazios
        print("--- renderPrint qcStats: 'res' e 'fname' OK. Gerando estatísticas... ---") # Console

        # Exibição das estatísticas
        cat("=== Resumo do Controle de Qualidade (ShortRead) ===\n")
        cat("Arquivo:", fname, "\n\n")
        cat("Contagem de Reads:\n")
        print(res@readCounts)
        cat("\n---------------------------------\n")
        cat("Qualidade Base Média por Ciclo (Resumo):\n")
        cycle_qual <- tryCatch(perCycle(res)$quality, error = function(e) NULL) # Pega qualidade por ciclo
        if (!is.null(cycle_qual) && "Score" %in% colnames(cycle_qual)) {
             print(summary(cycle_qual$"Score"))
        } else {
             cat("Não foi possível extrair dados de qualidade por ciclo.\n")
        }
        cat("\n---------------------------------\n")
        cat("Frequência de Bases por Ciclo (Início):\n")
        cycle_nuc <- tryCatch(perCycle(res)$baseCall, error = function(e) NULL) # Pega bases por ciclo
        if(!is.null(cycle_nuc)){
            print(head(cycle_nuc))
        } else {
            cat("Não foi possível extrair dados de frequência de bases.\n")
        }
        cat("\n")
        print("--- renderPrint qcStats: Fim da exibição das estatísticas ---") # Console
    })

    # Output para Gráfico de Qualidade por Ciclo (COM DEPURAÇÃO)
    output$qcPlotCycleQuality <- renderPlot({
        res <- qa_results() # Acessa o resultado armazenado
        fname <- processed_file_name()

        # Mensagens de depuração para o console
        print("--- renderPlot qcPlotCycleQuality: Verificando 'res' ---")
        print(paste("Classe de res:", ifelse(is.null(res), "NULL", class(res))))

        req(res) # Requer que 'res' não seja NULL

        print(paste("--- renderPlot qcPlotCycleQuality: 'res' OK. Tentando gerar gráfico para", fname, "---")) # Console
        tryCatch({
            # Tenta gerar o gráfico usando a função base plot()
            plot(ShortRead::plotCycleQuality(res))
            # Adiciona um título simples (sem ggplot2 por enquanto)
            title(main = paste("Qualidade Média por Ciclo -", fname))
            print("--- renderPlot qcPlotCycleQuality: Comando plot() executado ---") # Console
        }, error = function(e) {
            # Captura e imprime erro específico da plotagem
            error_msg_plot <- paste("!!! ERRO em renderPlot qcPlotCycleQuality:", e$message)
            print(error_msg_plot) # Console
            # Opcional: Mostrar erro na própria área do gráfico
            plot(1, type="n", xlab="", ylab="", main="Erro ao gerar gráfico de Qualidade por Ciclo")
            text(1, 1, error_msg_plot, col = "red", cex = 0.8)
        })
    })

    # Output para Gráfico de Qualidade por Read (COM DEPURAÇÃO)
    output$qcPlotReadQuality <- renderPlot({
        res <- qa_results() # Acessa o resultado armazenado
        fname <- processed_file_name()

        # Mensagens de depuração para o console
        print("--- renderPlot qcPlotReadQuality: Verificando 'res' ---")
        print(paste("Classe de res:", ifelse(is.null(res), "NULL", class(res))))

        req(res) # Requer que 'res' não seja NULL

        print(paste("--- renderPlot qcPlotReadQuality: 'res' OK. Tentando gerar gráfico para", fname, "---")) # Console
        tryCatch({
            # Tenta gerar o gráfico usando a função base plot()
            plot(ShortRead::plotReadQuality(res))
             # Adiciona um título simples (sem ggplot2 por enquanto)
            title(main = paste("Distribuição da Qualidade Média por Read -", fname))
            print("--- renderPlot qcPlotReadQuality: Comando plot() executado ---") # Console
        }, error = function(e) {
             # Captura e imprime erro específico da plotagem
            error_msg_plot <- paste("!!! ERRO em renderPlot qcPlotReadQuality:", e$message)
            print(error_msg_plot) # Console
             # Opcional: Mostrar erro na própria área do gráfico
            plot(1, type="n", xlab="", ylab="", main="Erro ao gerar gráfico de Qualidade por Read")
            text(1, 1, error_msg_plot, col = "red", cex = 0.8)
        })
    })

    # --- Lógica para Download (a implementar) ---
    # output$downloadData <- downloadHandler(...)

} # Fim do Server

# --- 4. Rodar o Aplicativo Shiny ---
shinyApp(ui = ui, server = server)