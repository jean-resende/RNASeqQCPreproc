# -- instalacao
if (!requireNamespace("BiocManager", quietly = TRUE)) 
  install.packages("BiocManager")

BiocManager::install("ShortRead")

# --- 1. Carregar a Biblioteca ---
# Certifique-se de que o pacote ShortRead está instalado!
library(ShortRead)

# --- 2. Definir o Caminho para o Arquivo FASTQ ---
# !!! IMPORTANTE: Substitua pelo caminho REAL para o SEU arquivo FASTQ !!!
fastq_file_path <- dir("examples", "*fq$", full=TRUE)
fastq_file_path <- "examples/example_1.fq"
# Exemplo Linux/Mac: "/home/usuario/dados_rna_seq/amostra1_R1.fastq.gz"
# Exemplo Windows: "C:/Usuarios/Usuario/Documentos/dados_rna_seq/amostra1_R1.fastq.gz"

# --- 3. Verificar se o Arquivo Existe ---
if (!file.exists(fastq_file_path)) {
  stop(paste("ERRO: O arquivo não foi encontrado em:", fastq_file_path,
             "\nPor favor, verifique o caminho e tente novamente."))
} else {
  print(paste("Arquivo encontrado:", fastq_file_path))
}

# --- 4. Executar a Análise de Qualidade (QA) ---
print("Iniciando ShortRead::qa()... Isso pode demorar um pouco dependendo do tamanho do arquivo.")
qa_result <- NULL # Inicializa como NULL
error_occurred <- FALSE

tryCatch({
  # A função qa() lê o arquivo e calcula as métricas de qualidade
  qa_result <- ShortRead::qa(fastq_file_path, type = "fastq")
  print("ShortRead::qa() concluído com sucesso!")

}, error = function(e) {
  # Captura e exibe erros que podem ocorrer durante o qa()
  print("!!! ERRO durante a execução de ShortRead::qa():")
  print(e)
  error_occurred <<- TRUE # Marca que ocorreu um erro
})

browseURL(report(qa_result))
head(qa_result[["readCounts"]])




# --- 5. Verificar e Inspecionar o Resultado ---
if (!error_occurred && !is.null(qa_result)) {
  print("\n--- Inspeção do Objeto QA Resultante ---")

  # Imprimir um resumo do objeto QA
  print("Resumo do objeto qa_result:")
  print(qa_result)

  # Verificar a classe do objeto
  print(paste("Classe do objeto:", class(qa_result)))

  # Mostrar a estrutura interna (primeiros níveis)
  print("Estrutura do objeto (str):")
  print(str(qa_result, max.level = 2))

  # Acessar informações específicas (exemplo: contagem de reads)
  print("Contagem de Reads (slot @readCounts):")
  # Acessando o slot diretamente ou usando a função acessora
  print(qa_result@readCounts)
  print(readCounts(qa_result$readCounts)) # Forma recomendada

  # --- 6. Gerar Saídas Similares ao Shiny (Texto no Console) ---
  print("\n--- Geração de Resumo de Texto no Console ---")
  cat("=== Resumo do Controle de Qualidade (ShortRead) ===\n")
  cat("Arquivo:", basename(fastq_file_path), "\n\n") # basename() pega só o nome do arquivo

  cat("Contagem de Reads:\n")
  print(readCounts(qa_result))
  cat("\n---------------------------------\n")

  cat("Qualidade Base Média por Ciclo (Resumo):\n")
  cycle_qual <- tryCatch(perCycle(qa_result)$quality, error = function(e) NULL)
  if (!is.null(cycle_qual) && "Score" %in% colnames(cycle_qual)) {
     print(summary(cycle_qual$"Score"))
  } else {
     cat("Não foi possível extrair dados de qualidade por ciclo.\n")
  }
  cat("\n---------------------------------\n")

  cat("Frequência de Bases por Ciclo (Início):\n")
  cycle_nuc <- tryCatch(perCycle(qa_result)$baseCall, error = function(e) NULL)
  if(!is.null(cycle_nuc)){
      print(head(cycle_nuc))
  } else {
      cat("Não foi possível extrair dados de frequência de bases.\n")
  }
  cat("\n")

  # --- 7. Gerar Gráficos ---
  # Estes gráficos devem aparecer na janela de Plots do RStudio ou dispositivo gráfico padrão
  print("\n--- Geração de Gráficos ---")
  print("Verifique a janela de Plots...")

  tryCatch({
    print("Gerando Gráfico: Qualidade por Ciclo...")
    plot(ShortRead::plotCycleQuality(qa_result))
    title(main = paste("Qualidade por Ciclo -", basename(fastq_file_path))) # Adiciona título
    print("Ok.")
  }, error = function(e){ print(paste("!!! ERRO ao gerar plotCycleQuality:", e$message)) })

  Sys.sleep(1) # Pequena pausa entre os gráficos

  tryCatch({
    print("Gerando Gráfico: Qualidade por Read...")
    plot(ShortRead::plotReadQuality(qa_result))
    title(main = paste("Qualidade Média por Read -", basename(fastq_file_path))) # Adiciona título
    print("Ok.")
  }, error = function(e){ print(paste("!!! ERRO ao gerar plotReadQuality:", e$message)) })

  Sys.sleep(1) # Pequena pausa

  tryCatch({
    print("Gerando Gráfico: Frequência de Nucleotídeos por Ciclo...")
    plot(ShortRead::plotNucleotideFrequency(qa_result))
    title(main = paste("Frequência de Bases por Ciclo -", basename(fastq_file_path))) # Adiciona título
    print("Ok.")
  }, error = function(e){ print(paste("!!! ERRO ao gerar plotNucleotideFrequency:", e$message)) })

  print("\n--- Fim do Script ---")

} else {
  # Mensagem se ocorreu erro no qa()
  print("\nNão foi possível prosseguir devido a um erro na etapa ShortRead::qa(). Verifique as mensagens de erro acima.")
}
