# --------------------------------------
# 1. CARREGAMENTO DE BIBLIOTECAS
# --------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(plotly)
library(scales)
library(viridis)

# --------------------------------------
# 2. LEITURA E PRÉ-PROCESSAMENTO
# --------------------------------------
data_filtro <- dmy("02/06/2025")
url <- "https://raw.githubusercontent.com/ciceroalvespro/aeroportos/b639ebfaef18d8010c8ab9b1f8babc0d758c00e9/malha.CSV"

df <- read_csv(url) %>%
  mutate(
    `Date LT` = dmy(`Date LT`),
    `Time LT` = str_pad(`Time LT`, width = 4, pad = "0"),
    `Time LT` = str_replace(`Time LT`, "(\\d{2})(\\d{2})", "\\1:\\2"),
    DataHora = dmy_hm(paste(format(`Date LT`, "%d/%m/%Y"), `Time LT`))
  ) %>%
  filter(`Date LT` == data_filtro) %>%
  mutate(
    Origem = if_else(ArrDep == "A", ORIGDEST, "VCP"),
    Destino = if_else(ArrDep == "A", "VCP", ORIGDEST)
  ) %>%
  rename(
    Companhia = `Airl.Desig`,
    Voo = FltNo,
    Movimento = ArrDep,
    TipoServico = `Serv.type`,
    Aeronave = `ICAO ActType`
  ) %>%
  select(Companhia, Voo, Movimento, TipoServico, Aeronave, DataHora, Origem, Destino)

# --------------------------------------
# 3. CRIAÇÃO DO DF_MALHA - CONECTIVIDADE
# --------------------------------------

# Separar chegadas e partidas
chegadas <- df %>%
  filter(Movimento == "A") %>%
  arrange(Companhia, Aeronave, DataHora) %>%
  select(Companhia, Voo_A = Voo, TipoServico_A = TipoServico,
         Aeronave, DataHora_A = DataHora, Origem)

partidas <- df %>%
  filter(Movimento == "D") %>%
  arrange(Companhia, Aeronave, DataHora) %>%
  select(Companhia, Voo_D = Voo, TipoServico_D = TipoServico,
         Aeronave, DataHora_D = DataHora, Destino)

# Função para calcular tempo mínimo de turnaround
calcular_tempo_turnaround <- function(aeronave, tipo_servico_a, tipo_servico_d, destino) {
  
  # Verificar se ambos os tipos de serviço são comerciais/charter
  servicos_comerciais <- c("C", "G", "J")
  eh_servico_comercial <- tipo_servico_a %in% servicos_comerciais && tipo_servico_d %in% servicos_comerciais
  
  # Se não for serviço comercial, usar tempo padrão de 45 minutos
  if (!eh_servico_comercial) {
    return(45)
  }
  
  # Aeronaves de grande porte - sempre 90 minutos
  aeronaves_grande_porte <- c("A332", "A339", "B772", "B763")
  if (aeronave %in% aeronaves_grande_porte) {
    return(90)
  }
  
  # Aeronaves regionais pequenas - sempre 45 minutos
  aeronaves_regionais <- c("AT72", "C208")
  if (aeronave %in% aeronaves_regionais) {
    return(45)
  }
  
  # Aeronaves médias - depende do destino
  aeronaves_medias <- c("A20N", "E295", "A21N")
  if (aeronave %in% aeronaves_medias) {
    # Verificar se destino é internacional
    if (destino %in% INTERNACIONAIS) {
      return(90)  # Destino internacional
    } else {
      return(60)  # Destino doméstico
    }
  }
  
  # Caso não se encaixe em nenhuma regra, usar padrão de 45 minutos
  return(45)
}

# Função para encontrar a próxima partida após chegada
vincular_voos <- function(chegadas, partidas) {
  resultado <- list()
  partidas_usadas <- c()  # Controle de partidas já utilizadas
  
  for(i in 1:nrow(chegadas)) {
    chegada_atual <- chegadas[i, ]
    
    # Buscar partidas da mesma companhia e aeronave
    partidas_candidatas <- partidas %>%
      filter(Companhia == chegada_atual$Companhia,
             Aeronave == chegada_atual$Aeronave) %>%
      # Excluir partidas já utilizadas
      filter(!paste(Companhia, Voo_D, Aeronave, DataHora_D) %in% partidas_usadas) %>%
      arrange(DataHora_D)
    
    # Encontrar primeira partida que atende ao tempo mínimo
    partida_escolhida <- NULL
    
    for(j in 1:nrow(partidas_candidatas)) {
      partida_candidata <- partidas_candidatas[j, ]
      
      # Calcular tempo mínimo específico para esta combinação
      tempo_minimo <- calcular_tempo_turnaround(
        aeronave = chegada_atual$Aeronave,
        tipo_servico_a = chegada_atual$TipoServico_A,
        tipo_servico_d = partida_candidata$TipoServico_D,
        destino = partida_candidata$Destino
      )
      
      tempo_minimo_conexao <- chegada_atual$DataHora_A + minutes(tempo_minimo)
      
      # Verificar se atende ao tempo mínimo
      if (partida_candidata$DataHora_D >= tempo_minimo_conexao) {
        partida_escolhida <- partida_candidata
        break  # Pegar a primeira válida
      }
    }
    
    if(!is.null(partida_escolhida)) {
      # Criar registro vinculado
      vinculo <- chegada_atual %>%
        left_join(partida_escolhida, by = c("Companhia", "Aeronave"))
      
      resultado[[length(resultado) + 1]] <- vinculo
      
      # Marcar partida como usada
      partidas_usadas <- c(partidas_usadas, 
                           paste(partida_escolhida$Companhia, 
                                 partida_escolhida$Voo_D,
                                 partida_escolhida$Aeronave, 
                                 partida_escolhida$DataHora_D))
    } else {
      # Chegada sem partida correspondente
      vinculo <- chegada_atual %>%
        mutate(Voo_D = NA, TipoServico_D = NA,
               DataHora_D = as.POSIXct(NA), Destino = NA)
      
      resultado[[length(resultado) + 1]] <- vinculo
    }
  }
  
  return(list(
    vinculos = bind_rows(resultado),
    partidas_usadas = partidas_usadas
  ))
}

# Criar df_malha com vínculo corrigido
resultado_vinculacao <- vincular_voos(chegadas, partidas)
df_malha <- resultado_vinculacao$vinculos

# Adicionar partidas não vinculadas (que não tiveram chegada correspondente)
partidas_nao_vinculadas <- partidas %>%
  filter(!paste(Companhia, Voo_D, Aeronave, DataHora_D) %in% resultado_vinculacao$partidas_usadas) %>%
  transmute(
    Companhia, 
    Voo_A = NA, 
    TipoServico_A = NA,
    Aeronave, 
    DataHora_A = as.POSIXct(NA), 
    Origem = NA,
    Voo_D, 
    TipoServico_D, 
    DataHora_D, 
    Destino
  )

# Combinar todos os dados
df_malha <- bind_rows(df_malha, partidas_nao_vinculadas) %>%
  arrange(Companhia, Aeronave, DataHora_A, DataHora_D)

# Verificar e remover duplicatas completas (se existirem)
df_malha <- df_malha %>%
  distinct()

# Verificar resultados
cat("DataFrame original (df):", nrow(df), "registros\n")
cat("DataFrame malha (df_malha):", nrow(df_malha), "registros\n")
cat("Vínculos completos:", sum(!is.na(df_malha$Voo_A) & !is.na(df_malha$Voo_D)), "\n")
cat("Apenas chegadas:", sum(!is.na(df_malha$Voo_A) & is.na(df_malha$Voo_D)), "\n")
cat("Apenas partidas:", sum(is.na(df_malha$Voo_A) & !is.na(df_malha$Voo_D)), "\n")

# Verificação adicional para detectar duplicações
duplicatas <- df_malha %>%
  group_by(Companhia, Aeronave, DataHora_A, DataHora_D) %>%
  filter(n() > 1) %>%
  ungroup()

if(nrow(duplicatas) > 0) {
  cat("⚠️  ATENÇÃO: Encontradas", nrow(duplicatas), "possíveis duplicações:\n")
  print(duplicatas)
} else {
  cat("✅ Nenhuma duplicação detectada no df_malha\n")
}

INTERNACIONAIS <- c("ASU", "BRC", "FLL", "LIS", "MAD", "MCO", "MDZ", "MVD", "OPO", "ORY")
CAGO_DOM_EQP <- c("B738","73P","738","AT72","C208","A21N", "A321","B734","B735","B737","B732","32X","75F")

positions <- list(
  ADS = c("ADS01"),
  M = c("M01","M02","M03","M04","M05","M06","M07","M08"),
  N = c("N101","N102","N103","N104","N105","N106","N107","N108","N109","N110","N111"),
  B = c("B02", "B04", "B06", "B08", "B10", "B12", "B14"),
  B_Int = c("B07","B09","B09A","B11", "B13","B13A"),
  A = c("A02A","A02","A04A","A06A", "A06","A08"),
  C = c("C02","C04","C05","C06","C07","C08","C09","C10","C11","C12","C13","C14","C15"),
  R = c("R01","R02","R03","R04","R05","R06","R07","R08","R09","R10","R11","R12","R13","R14","R15","R16","R17","R18","R19")
)

# --------------------------------------
# 4. DEFINIÇÃO DE ADJACÊNCIAS
# --------------------------------------
# Mapa de adjacências - quando uma posição principal é ocupada, bloqueia as adjacentes
adjacencias <- list(
  "A02" = c("A02A", "A04A"),  # A02 bloqueia A02A e A04A
  "A06" = c("A06A", "A08"),   # A06 bloqueia A06A e A08
  "B09" = c("B07", "B09A"),   # B09 bloqueia B07 e B09A
  "B13" = c("B11", "B13A")    # B13 bloqueia B11 e B13A
)

# Criar mapa reverso - quais posições principais bloqueiam cada adjacente
posicoes_bloqueadas_por <- list()
for(principal in names(adjacencias)) {
  for(adjacente in adjacencias[[principal]]) {
    posicoes_bloqueadas_por[[adjacente]] <- principal
  }
}

# --------------------------------------
# 5. FUNÇÕES DE ALOCAÇÃO COM ADJACÊNCIAS
# --------------------------------------
# Função para verificar se posição está disponível (considerando adjacências)
verificar_disponibilidade_com_adjacencias <- function(posicao, inicio_desejado, fim_desejado, ocupacoes) {
  # Buffer de segurança (15 minutos antes e depois)
  buffer_inicio <- inicio_desejado - minutes(15)
  buffer_fim <- fim_desejado + minutes(15)
  
  # 1. Verificar ocupação direta da posição
  ocupacoes_posicao <- ocupacoes[ocupacoes$posicao == posicao, ]
  
  if(nrow(ocupacoes_posicao) > 0) {
    for(i in 1:nrow(ocupacoes_posicao)) {
      inicio_existente <- ocupacoes_posicao$inicio[i]
      fim_existente <- ocupacoes_posicao$fim[i]
      
      # Verificar sobreposição com buffer
      if(!(buffer_fim <= inicio_existente || buffer_inicio >= fim_existente)) {
        return(FALSE)
      }
    }
  }
  
  # 2. Se a posição desejada é uma posição principal, verificar se suas adjacentes estão livres
  if(posicao %in% names(adjacencias)) {
    posicoes_adjacentes <- adjacencias[[posicao]]
    
    for(pos_adj in posicoes_adjacentes) {
      ocupacoes_adj <- ocupacoes[ocupacoes$posicao == pos_adj, ]
      
      if(nrow(ocupacoes_adj) > 0) {
        for(j in 1:nrow(ocupacoes_adj)) {
          inicio_adj <- ocupacoes_adj$inicio[j]
          fim_adj <- ocupacoes_adj$fim[j]
          
          # Verificar sobreposição com buffer
          if(!(buffer_fim <= inicio_adj || buffer_inicio >= fim_adj)) {
            return(FALSE)
          }
        }
      }
    }
  }
  
  # 3. Se a posição desejada é adjacente, verificar se a principal está livre
  if(posicao %in% names(posicoes_bloqueadas_por)) {
    posicao_principal <- posicoes_bloqueadas_por[[posicao]]
    ocupacoes_principal <- ocupacoes[ocupacoes$posicao == posicao_principal, ]
    
    if(nrow(ocupacoes_principal) > 0) {
      for(k in 1:nrow(ocupacoes_principal)) {
        inicio_principal <- ocupacoes_principal$inicio[k]
        fim_principal <- ocupacoes_principal$fim[k]
        
        # Verificar sobreposição com buffer
        if(!(buffer_fim <= inicio_principal || buffer_inicio >= fim_principal)) {
          return(FALSE)
        }
      }
    }
  }
  
  return(TRUE)
}

# Função para alocar aeronave considerando adjacências
alocar_aeronave_com_adjacencias <- function(voo, posicoes_disponiveis, ocupacoes) {
  inicio_ocupacao <- if(!is.na(voo$DataHora_A)) voo$DataHora_A else voo$DataHora_D - minutes(45)
  fim_ocupacao <- if(!is.na(voo$DataHora_D)) voo$DataHora_D else fim_dia
  
  for(posicao in posicoes_disponiveis) {
    if(verificar_disponibilidade_com_adjacencias(posicao, inicio_ocupacao, fim_ocupacao, ocupacoes)) {
      return(list(
        posicao = posicao,
        inicio = inicio_ocupacao,
        fim = fim_ocupacao,
        alocada = TRUE
      ))
    }
  }
  
  # Se não conseguiu alocar, vai para adensamento
  return(list(
    posicao = "ADS01",
    inicio = inicio_ocupacao,
    fim = fim_ocupacao,
    alocada = FALSE
  ))
}

# --------------------------------------
# 6. EXECUÇÃO DAS REGRAS DE ALOCAÇÃO
# --------------------------------------
# Preparar dados base
data_filtro_char <- format(data_filtro, "%Y-%m-%d")
inicio_dia <- as.POSIXct(paste(data_filtro_char, "00:00:00"))
fim_dia <- as.POSIXct(paste(data_filtro_char, "23:59:59"))

# Criar lista ordenada de todas as posições
todas_posicoes <- c(
  sort(positions$A),      
  sort(positions$ADS),    
  sort(positions$B),      
  sort(positions$B_Int),  
  sort(positions$C),      
  sort(positions$M),      
  sort(positions$N),      
  sort(positions$R)       
)

# Criar categorias para coloração
categorias_posicoes <- data.frame(
  posicao = todas_posicoes,
  categoria = c(
    rep("A", length(positions$A)),
    rep("ADS", length(positions$ADS)),
    rep("B", length(positions$B)),
    rep("B_Int", length(positions$B_Int)),
    rep("C", length(positions$C)),
    rep("M", length(positions$M)),
    rep("N", length(positions$N)),
    rep("R", length(positions$R))
  )
)

# Definir cores
cores_categorias <- c(
  "A" = "#DDA0DD", "ADS" = "#FF6B6B", "B" = "#96CEB4", "B_Int" = "#FFEAA7",
  "C" = "#98D8C8", "M" = "#4ECDC4", "N" = "#45B7D1", "R" = "#F7DC6F"
)

ocupacoes <- data.frame()
voos_adensamento <- 0

# REGRA 1: Voos de carga internacional (F) não domésticos nas posições M
voos_carga_int <- df_malha %>%
  filter((TipoServico_A == "F" | TipoServico_D == "F") &
           !Aeronave %in% CAGO_DOM_EQP) %>%
  arrange(DataHora_A, DataHora_D)

# Alocar voos de carga internacional
posicoes_M <- sort(positions$M)
for(i in 1:nrow(voos_carga_int)) {
  voo <- voos_carga_int[i, ]
  resultado <- alocar_aeronave_com_adjacencias(voo, posicoes_M, ocupacoes)
  
  if(!resultado$alocada) {
    voos_adensamento <- voos_adensamento + 1
  }
  
  # Adicionar à ocupação
  ocupacoes <- rbind(ocupacoes, data.frame(
    posicao = resultado$posicao,
    inicio = resultado$inicio,
    fim = resultado$fim,
    companhia = voo$Companhia,
    voo_a = ifelse(is.na(voo$Voo_A), "", voo$Voo_A),
    voo_d = ifelse(is.na(voo$Voo_D), "", voo$Voo_D),
    aeronave = voo$Aeronave,
    tipo = "Carga Internacional",
    adensamento = !resultado$alocada
  ))
}

# REGRA 2: Azul Conecta (2F) com aeronaves C208
voos_azul_conecta <- df_malha %>%
  filter(Companhia == "2F" & Aeronave == "C208") %>%
  arrange(DataHora_A, DataHora_D)

posicoes_azul_conecta <- c("R14", "R15")
for(i in 1:nrow(voos_azul_conecta)) {
  voo <- voos_azul_conecta[i, ]
  resultado <- alocar_aeronave_com_adjacencias(voo, posicoes_azul_conecta, ocupacoes)
  
  if(!resultado$alocada) {
    voos_adensamento <- voos_adensamento + 1
  }
  
  ocupacoes <- rbind(ocupacoes, data.frame(
    posicao = resultado$posicao,
    inicio = resultado$inicio,
    fim = resultado$fim,
    companhia = voo$Companhia,
    voo_a = ifelse(is.na(voo$Voo_A), "", voo$Voo_A),
    voo_d = ifelse(is.na(voo$Voo_D), "", voo$Voo_D),
    aeronave = voo$Aeronave,
    tipo = "Azul Conecta",
    adensamento = !resultado$alocada
  ))
}

# REGRA 3: Aeronaves AT72 nas posições N101-N111
voos_at72 <- df_malha %>%
  filter(Aeronave == "AT72") %>%
  arrange(DataHora_A, DataHora_D)

posicoes_at72 <- c("N101", "N102", "N103", "N104", "N105", "N106", "N107", "N108", "N109", "N110", "N111")
for(i in 1:nrow(voos_at72)) {
  voo <- voos_at72[i, ]
  resultado <- alocar_aeronave_com_adjacencias(voo, posicoes_at72, ocupacoes)
  
  if(!resultado$alocada) {
    voos_adensamento <- voos_adensamento + 1
  }
  
  ocupacoes <- rbind(ocupacoes, data.frame(
    posicao = resultado$posicao,
    inicio = resultado$inicio,
    fim = resultado$fim,
    companhia = voo$Companhia,
    voo_a = ifelse(is.na(voo$Voo_A), "", voo$Voo_A),
    voo_d = ifelse(is.na(voo$Voo_D), "", voo$Voo_D),
    aeronave = voo$Aeronave,
    tipo = "AT72",
    adensamento = !resultado$alocada
  ))
}

# REGRA 4: Voos de carga doméstica nas posições R
voos_carga_dom <- df_malha %>%
  filter(
    (TipoServico_A %in% c("A", "F", "H") | TipoServico_D %in% c("A", "F", "H")) &
      Aeronave %in% CAGO_DOM_EQP &
      !paste(Companhia, Aeronave, DataHora_A, DataHora_D) %in% 
      paste(
        c(voos_carga_int$Companhia, voos_azul_conecta$Companhia, voos_at72$Companhia),
        c(voos_carga_int$Aeronave, voos_azul_conecta$Aeronave, voos_at72$Aeronave),
        c(voos_carga_int$DataHora_A, voos_azul_conecta$DataHora_A, voos_at72$DataHora_A),
        c(voos_carga_int$DataHora_D, voos_azul_conecta$DataHora_D, voos_at72$DataHora_D)
      )
  ) %>%
  arrange(DataHora_A, DataHora_D)

posicoes_carga_dom <- positions$R[!positions$R %in% c("R14", "R15")]

for(i in 1:nrow(voos_carga_dom)) {
  voo <- voos_carga_dom[i, ]
  resultado <- alocar_aeronave_com_adjacencias(voo, posicoes_carga_dom, ocupacoes)
  
  if(!resultado$alocada) {
    voos_adensamento <- voos_adensamento + 1
  }
  
  ocupacoes <- rbind(ocupacoes, data.frame(
    posicao = resultado$posicao,
    inicio = resultado$inicio,
    fim = resultado$fim,
    companhia = voo$Companhia,
    voo_a = ifelse(is.na(voo$Voo_A), "", voo$Voo_A),
    voo_d = ifelse(is.na(voo$Voo_D), "", voo$Voo_D),
    aeronave = voo$Aeronave,
    tipo = "Carga Doméstica",
    adensamento = !resultado$alocada
  ))
}

# Função auxiliar para verificar se voo é internacional
eh_internacional <- function(origem, destino) {
  return((origem %in% INTERNACIONAIS) | (destino %in% INTERNACIONAIS))
}

# Criar lista de voos já alocados para exclusão
voos_ja_alocados <- paste(
  c(voos_carga_int$Companhia, voos_azul_conecta$Companhia, voos_at72$Companhia, voos_carga_dom$Companhia),
  c(voos_carga_int$Aeronave, voos_azul_conecta$Aeronave, voos_at72$Aeronave, voos_carga_dom$Aeronave),
  c(voos_carga_int$DataHora_A, voos_azul_conecta$DataHora_A, voos_at72$DataHora_A, voos_carga_dom$DataHora_A),
  c(voos_carga_int$DataHora_D, voos_azul_conecta$DataHora_D, voos_at72$DataHora_D, voos_carga_dom$DataHora_D)
)

# REGRA 5: Voos de passageiros domésticos nas posições C e B
voos_pax_domesticos <- df_malha %>%
  filter(
    (TipoServico_A %in% c("C", "G", "J") | TipoServico_D %in% c("C", "G", "J")) &
      !eh_internacional(Origem, Destino) &
      !paste(Companhia, Aeronave, DataHora_A, DataHora_D) %in% voos_ja_alocados
  ) %>%
  arrange(DataHora_A, DataHora_D)

posicoes_pax_domesticos <- c(positions$C, positions$B)

for(i in 1:nrow(voos_pax_domesticos)) {
  voo <- voos_pax_domesticos[i, ]
  resultado <- alocar_aeronave_com_adjacencias(voo, posicoes_pax_domesticos, ocupacoes)
  
  if(!resultado$alocada) {
    voos_adensamento <- voos_adensamento + 1
  }
  
  ocupacoes <- rbind(ocupacoes, data.frame(
    posicao = resultado$posicao,
    inicio = resultado$inicio,
    fim = resultado$fim,
    companhia = voo$Companhia,
    voo_a = ifelse(is.na(voo$Voo_A), "", voo$Voo_A),
    voo_d = ifelse(is.na(voo$Voo_D), "", voo$Voo_D),
    aeronave = voo$Aeronave,
    tipo = "Passageiros Domésticos",
    adensamento = !resultado$alocada
  ))
}

# Atualizar lista de voos já alocados
voos_ja_alocados <- c(voos_ja_alocados, paste(
  voos_pax_domesticos$Companhia,
  voos_pax_domesticos$Aeronave,
  voos_pax_domesticos$DataHora_A,
  voos_pax_domesticos$DataHora_D
))

# REGRA 6: Voos internacionais Widebody nas posições específicas
voos_int_widebody <- df_malha %>%
  filter(
    (TipoServico_A %in% c("C", "G", "J") | TipoServico_D %in% c("C", "G", "J")) &
      eh_internacional(Origem, Destino) &
      Aeronave %in% c("A332", "A339", "B772","B763") &
      !paste(Companhia, Aeronave, DataHora_A, DataHora_D) %in% voos_ja_alocados
  ) %>%
  arrange(DataHora_A, DataHora_D)

# Posições específicas para widebody internacional (posições principais que bloqueiam adjacentes)
posicoes_int_widebody <- c("A02", "A06", "B09", "B13")

cat("\n=== REGRA 6: INTERNACIONAIS WIDEBODY (COM ADJACÊNCIAS) ===\n")
cat("Voos identificados:", nrow(voos_int_widebody), "\n")
cat("Posições disponíveis:", length(posicoes_int_widebody), "\n")

for(i in 1:nrow(voos_int_widebody)) {
  voo <- voos_int_widebody[i, ]
  resultado <- alocar_aeronave_com_adjacencias(voo, posicoes_int_widebody, ocupacoes)
  
  if(!resultado$alocada) {
    voos_adensamento <- voos_adensamento + 1
  }
  
  ocupacoes <- rbind(ocupacoes, data.frame(
    posicao = resultado$posicao,
    inicio = resultado$inicio,
    fim = resultado$fim,
    companhia = voo$Companhia,
    voo_a = ifelse(is.na(voo$Voo_A), "", voo$Voo_A),
    voo_d = ifelse(is.na(voo$Voo_D), "", voo$Voo_D),
    aeronave = voo$Aeronave,
    tipo = "Internacional Widebody",
    adensamento = !resultado$alocada
  ))
}

# Atualizar lista de voos já alocados
voos_ja_alocados <- c(voos_ja_alocados, paste(
  voos_int_widebody$Companhia,
  voos_int_widebody$Aeronave,
  voos_int_widebody$DataHora_A,
  voos_int_widebody$DataHora_D
))

# REGRA 7: Voos internacionais Narrowbody nas posições específicas (adjacentes)
voos_int_narrowbody <- df_malha %>%
  filter(
    (TipoServico_A %in% c("C", "G", "J") | TipoServico_D %in% c("C", "G", "J")) &
      eh_internacional(Origem, Destino) &
      !Aeronave %in% c("A332", "A339", "B772","B763") &
      !paste(Companhia, Aeronave, DataHora_A, DataHora_D) %in% voos_ja_alocados
  ) %>%
  arrange(DataHora_A, DataHora_D)

# Posições específicas para narrowbody internacional (posições adjacentes)
posicoes_int_narrowbody <- c("A02A", "A04A", "A06A", "A08", "B07", "B09A", "B11", "B13A")

cat("\n=== REGRA 7: INTERNACIONAIS NARROWBODY (POSIÇÕES ADJACENTES) ===\n")
cat("Voos identificados:", nrow(voos_int_narrowbody), "\n")
cat("Posições disponíveis:", length(posicoes_int_narrowbody), "\n")

for(i in 1:nrow(voos_int_narrowbody)) {
  voo <- voos_int_narrowbody[i, ]
  resultado <- alocar_aeronave_com_adjacencias(voo, posicoes_int_narrowbody, ocupacoes)
  
  if(!resultado$alocada) {
    voos_adensamento <- voos_adensamento + 1
  }
  
  ocupacoes <- rbind(ocupacoes, data.frame(
    posicao = resultado$posicao,
    inicio = resultado$inicio,
    fim = resultado$fim,
    companhia = voo$Companhia,
    voo_a = ifelse(is.na(voo$Voo_A), "", voo$Voo_A),
    voo_d = ifelse(is.na(voo$Voo_D), "", voo$Voo_D),
    aeronave = voo$Aeronave,
    tipo = "Internacional Narrowbody",
    adensamento = !resultado$alocada
  ))
}


# Criar gantt_data incluindo origem e destino
gantt_data <- ocupacoes %>%
  # Buscar origem e destino do df_malha original
  left_join(
    df_malha %>% 
      select(Companhia, Aeronave, DataHora_A, DataHora_D, Origem, Destino),
    by = c("companhia" = "Companhia", "aeronave" = "Aeronave")
  ) %>%
  # Em caso de múltiplas correspondências, pegar a primeira
  group_by(companhia, aeronave, inicio, fim) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    voos = case_when(
      voo_a != "" & voo_d != "" ~ paste(voo_a, "/", voo_d),
      voo_a != "" ~ voo_a,
      voo_d != "" ~ voo_d,
      TRUE ~ "N/A"
    ),
    # Criar campo de rota para o tooltip
    rota = case_when(
      !is.na(Origem) & !is.na(Destino) ~ paste(Origem, "→", Destino),
      !is.na(Origem) ~ paste(Origem, "→ VCP"),
      !is.na(Destino) ~ paste("VCP →", Destino),
      TRUE ~ "N/A"
    )
  ) %>%
  select(
    posicao,
    inicio,
    fim,
    companhia,
    voos,
    aeronave,
    tipo,
    adensamento,
    rota,
    Origem,
    Destino
  )

# Adicionar linhas vazias para posições não ocupadas
posicoes_ocupadas <- unique(gantt_data$posicao)
posicoes_vazias <- todas_posicoes[!todas_posicoes %in% posicoes_ocupadas]

if(length(posicoes_vazias) > 0) {
  dados_vazios <- data.frame(
    posicao = posicoes_vazias,
    inicio = inicio_dia,
    fim = fim_dia,
    companhia = "",
    voos = "",
    aeronave = "",
    tipo = "Disponível",
    adensamento = FALSE,
    rota = "",
    Origem = NA,
    Destino = NA
  )
  
  gantt_data <- bind_rows(gantt_data, dados_vazios)
}

# Criar o gráfico com tooltip melhorado
fig_gantt <- plot_ly() %>%
  add_segments(
    data = gantt_data,
    x = ~inicio, xend = ~fim,
    y = ~factor(posicao, levels = rev(todas_posicoes)),
    yend = ~factor(posicao, levels = rev(todas_posicoes)),
    color = ~tipo,
    colors = c("Disponível" = "lightgray", 
               "Carga Internacional" = "#4ECDC4",
               "Azul Conecta" = "#1E90FF", 
               "AT72" = "#32CD32", 
               "Carga Doméstica" = "#FF8C00",
               "Passageiros Domésticos" = "#9370DB",
               "Internacional Widebody" = "#DC143C",
               "Internacional Narrowbody" = "#20B2AA",
               "Adensamento" = "#FF0000"),
    line = list(width = 15),
    hovertemplate = paste(
      "<b>Posição:</b> %{y}<br>",
      "<b>Companhia:</b>", gantt_data$companhia, "<br>",
      "<b>Voos:</b>", gantt_data$voos, "<br>",
      "<b>Rota:</b>", gantt_data$rota, "<br>",
      "<b>Aeronave:</b>", gantt_data$aeronave, "<br>",
      "<b>Início:</b>", format(gantt_data$inicio, "%H:%M"), "<br>",
      "<b>Fim:</b>", format(gantt_data$fim, "%H:%M"), "<br>",
      "<b>Tipo:</b>", gantt_data$tipo, "<br>",
      "<extra></extra>"
    )
  ) %>%
  layout(
    title = list(
      text = paste("<b>Alocação de Aeronaves - VCP</b><br>",
                   format(data_filtro, "%d/%m/%Y"),
                   if(voos_adensamento > 0) paste0("<br><span style='color:red'>", voos_adensamento, " voos em adensamento</span>") else ""),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "<b>Horário</b>", type = "date", tickformat = "%H:%M",
      dtick = 3600000, range = c(inicio_dia, fim_dia),
      showgrid = TRUE, gridcolor = "lightgray"
    ),
    yaxis = list(
      title = "<b>Posições de Pátio</b>",
      showgrid = TRUE, gridcolor = "lightgray",
      categoryorder = "array", categoryarray = rev(todas_posicoes)
    ),
    showlegend = TRUE,
    height = 800,
    margin = list(l = 80, r = 50, t = 120, b = 100),
    plot_bgcolor = "white", paper_bgcolor = "white"
  )

fig_gantt

# --------------------------------------
# 7. CRIAÇÃO DA TABELA DE ALOCAÇÃO FINAL
# --------------------------------------

# Preparar dados de ocupação com chaves mais robustas para junção
ocupacoes_para_juncao <- ocupacoes %>%
  select(companhia, aeronave, inicio, fim, posicao, tipo, adensamento, voo_a, voo_d) %>%
  # Criar chaves compostas para melhor correspondência
  mutate(
    # Chave baseada nos voos (mais precisa)
    chave_voos = paste(companhia, aeronave, 
                       ifelse(voo_a == "", "NA", voo_a), 
                       ifelse(voo_d == "", "NA", voo_d), 
                       sep = "_"),
    
    # Chave baseada em horários (fallback)
    chave_horarios = paste(companhia, aeronave, 
                           as.character(inicio), 
                           as.character(fim), 
                           sep = "_")
  )

# Preparar df_malha com as mesmas chaves
df_malha_com_chaves <- df_malha %>%
  mutate(
    # Chave baseada nos voos
    chave_voos = paste(Companhia, Aeronave, 
                       ifelse(is.na(Voo_A), "NA", Voo_A), 
                       ifelse(is.na(Voo_D), "NA", Voo_D), 
                       sep = "_"),
    
    # Chave baseada em horários (usando início de ocupação calculado)
    inicio_ocupacao = case_when(
      !is.na(DataHora_A) ~ DataHora_A,
      !is.na(DataHora_D) ~ DataHora_D - minutes(45),
      TRUE ~ as.POSIXct(NA)
    ),
    fim_ocupacao = case_when(
      !is.na(DataHora_D) ~ DataHora_D,
      TRUE ~ fim_dia
    ),
    chave_horarios = paste(Companhia, Aeronave, 
                           as.character(inicio_ocupacao), 
                           as.character(fim_ocupacao), 
                           sep = "_")
  )

# Primeira tentativa: junção por chave de voos (mais precisa)
juncao_voos <- df_malha_com_chaves %>%
  left_join(
    ocupacoes_para_juncao %>% 
      select(chave_voos, posicao, tipo, adensamento) %>%
      rename(posicao_voos = posicao, tipo_voos = tipo, adensamento_voos = adensamento),
    by = "chave_voos"
  )

# Segunda tentativa: junção por horários para casos não resolvidos
juncao_completa <- juncao_voos %>%
  left_join(
    ocupacoes_para_juncao %>% 
      select(chave_horarios, posicao, tipo, adensamento) %>%
      rename(posicao_horarios = posicao, tipo_horarios = tipo, adensamento_horarios = adensamento),
    by = "chave_horarios"
  ) %>%
  # Consolidar informações priorizando junção por voos
  mutate(
    posicao_final = coalesce(posicao_voos, posicao_horarios),
    tipo_final = coalesce(tipo_voos, tipo_horarios),
    adensamento_final = coalesce(adensamento_voos, adensamento_horarios, FALSE)
  )

# Criar tabela de alocação final
df_alocacao <- juncao_completa %>%
  # Organizar e renomear colunas
  select(
    Companhia,
    Aeronave,
    Voo_Chegada = Voo_A,
    Voo_Partida = Voo_D,
    Horario_Chegada = DataHora_A,
    Horario_Partida = DataHora_D,
    Origem,
    Destino,
    Tipo_Servico_Chegada = TipoServico_A,
    Tipo_Servico_Partida = TipoServico_D,
    Posicao_Alocada = posicao_final,
    Categoria_Alocacao = tipo_final,
    Em_Adensamento = adensamento_final
  ) %>%
  # Criar campos adicionais úteis
  mutate(
    # Rota completa
    Rota = case_when(
      !is.na(Origem) & !is.na(Destino) ~ paste(Origem, "→", Destino),
      !is.na(Origem) ~ paste(Origem, "→ VCP"),
      !is.na(Destino) ~ paste("VCP →", Destino),
      TRUE ~ "N/A"
    ),
    
    # Descrição do voo
    Descricao_Voos = case_when(
      !is.na(Voo_Chegada) & !is.na(Voo_Partida) ~ paste(Voo_Chegada, "/", Voo_Partida),
      !is.na(Voo_Chegada) ~ paste("Chegada:", Voo_Chegada),
      !is.na(Voo_Partida) ~ paste("Partida:", Voo_Partida),
      TRUE ~ "N/A"
    ),
    
    # Tempo de permanência (quando há chegada e partida)
    Tempo_Permanencia = case_when(
      !is.na(Horario_Chegada) & !is.na(Horario_Partida) ~ 
        as.numeric(difftime(Horario_Partida, Horario_Chegada, units = "hours")),
      TRUE ~ as.numeric(NA)
    ),
    
    # Status da operação
    Status_Operacao = case_when(
      Em_Adensamento == TRUE ~ "ADENSAMENTO",
      !is.na(Voo_Chegada) & !is.na(Voo_Partida) ~ "TURNAROUND",
      !is.na(Voo_Chegada) & is.na(Voo_Partida) ~ "CHEGADA APENAS",
      is.na(Voo_Chegada) & !is.na(Voo_Partida) ~ "PARTIDA APENAS",
      TRUE ~ "INDEFINIDO"
    ),
    
    # Classificação internacional/doméstico
    Tipo_Rota = case_when(
      Origem %in% INTERNACIONAIS | Destino %in% INTERNACIONAIS ~ "INTERNACIONAL",
      !is.na(Origem) | !is.na(Destino) ~ "DOMÉSTICO",
      TRUE ~ "N/A"
    )
  ) %>%
  # Ordenar por horário de chegada, depois partida
  arrange(Horario_Chegada, Horario_Partida, Companhia, Aeronave)

# Substituir NA por valores mais legíveis
df_alocacao <- df_alocacao %>%
  mutate(
    across(where(is.character), ~ifelse(is.na(.) | . == "NA", "-", .)),
    Posicao_Alocada = ifelse(is.na(Posicao_Alocada), "NÃO ALOCADA", Posicao_Alocada),
    Categoria_Alocacao = ifelse(is.na(Categoria_Alocacao) | Categoria_Alocacao == "NA", 
                                "Não Classificado", Categoria_Alocacao),
    Em_Adensamento = ifelse(is.na(Em_Adensamento), FALSE, Em_Adensamento),
    Tempo_Permanencia = round(Tempo_Permanencia, 2)
  )
