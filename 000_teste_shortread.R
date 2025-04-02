# -- instalacao
if (!requireNamespace("BiocManager", quietly = TRUE)) 
  install.packages("BiocManager")

BiocManager::install("ShortRead")

# --- Carregar a Biblioteca ---
# Certifique-se de que o pacote ShortRead está instalado!
library(ShortRead)

# --- Definir o Caminho para o Arquivo FASTQ ---
fastq_file_path <- dir("examples", "*fq$", full=TRUE)
#fastq_file_path <- "examples/example_1.fq"
# Exemplo Linux/Mac: "/home/usuario/dados_rna_seq/amostra1_R1.fastq.gz"
# Exemplo Windows: "C:/Usuarios/Usuario/Documentos/dados_rna_seq/amostra1_R1.fastq.gz"

# --- Executar a Análise de Qualidade (QA) ---
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

# contagens de leitura (tabela)
ShortRead:::.ppnCount(qa_result[["readCounts"]])

# contagens de leitura (grafico)
ShortRead:::.plotReadCount(qa_result)

# frequencia de bases
ShortRead:::.plotNucleotideCount(qa_result) 

# qualidade das leituras
df <- qa_result[["readQualityScore"]]
ShortRead:::.plotReadQuality(df[df$type=="read",])

df <- qa_result[["sequenceDistribution"]]
ShortRead:::.plotReadOccurrences(df[df$type=="read",], cex=.5)

ShortRead:::.freqSequences(qa_result, "read") # leituras duplicadas

# chamada de base por ciclo
perCycle <- qa_result[["perCycle"]]
ShortRead:::.plotCycleBaseCall(perCycle$baseCall)

# Pontuacao de qualidade por ciclo
perCycle <- qa_result[["perCycle"]]
ShortRead:::.plotCycleQuality(perCycle$quality)

# Contaminacao por adaptador
ShortRead:::.ppnCount(qa_result[["adapterContamination"]])