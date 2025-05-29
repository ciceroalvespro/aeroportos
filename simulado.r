# 01. Bibliotecas ####################
library(readr) 
library(dplyr)
library(lubridate)
library(tidyr)
library(plotly)
library(stringr)
library(purrr)

# 02. Lendo e processando os dados ##############
dados <- "/Users/Cicero/Documents/RStudio/ABV/malha.CSV"
df <- tryCatch({
  read_csv(dados, show_col_types = FALSE) %>%
    rename(
      Empresa = `Airl.Desig`,
      Voo = FltNo,
      Movimento = ArrDep,
      Data = `Date LT`,  
      Horario_LT = `Time LT`,  
      Origem_Destino = ORIGDEST,
      Tipo_Servico = `Serv.type`,  
      Equipamento = `ICAO ActType`
    )
}, error = function(e) {
  stop("Erro ao ler o arquivo: ", e$message)
})

# Processar colunas de data/hora
df <- df %>%
  mutate(
    Data = dmy(Data),
    Horario_LT = str_pad(as.character(Horario_LT), width = 4, side = "left", pad = "0"),
    Hora_str = paste0(substr(Horario_LT, 1, 2), ":", substr(Horario_LT, 3, 4)),
    Hora = parse_time(Hora_str),
    Data_Hora = if_else(
      !is.na(Hora),
      make_datetime(year = year(Data), month = month(Data), day = day(Data), hour = hour(Hora), min = minute(Hora)),
      NA_POSIXct_
    )
  ) %>%
  # Processamento dos movimentos
  mutate(
    Movimento = recode(Movimento, "A" = "Pouso", "D" = "Decolagem"),
    Origem = if_else(Movimento == "Pouso", Origem_Destino, "VCP"),
    Destino = if_else(Movimento == "Decolagem", Origem_Destino, "VCP")
  ) %>%
  # Regra para equipamentos específicos (ex: cargueiros)
  mutate(
    Equipamento = case_when(
      Empresa == "AD" & Tipo_Servico %in% c("F", "H") & is.na(Equipamento) ~ "32X",
      TRUE ~ Equipamento
    )
  ) %>%
  select(Empresa, Voo, Movimento, Origem, Destino, Tipo_Servico, Equipamento, Data, Data_Hora)

# Filtrar por período
data_inicio <- as.Date("2025-08-15")
data_fim <- as.Date("2025-08-16")
df <- df %>% filter(Data >= data_inicio & Data <= data_fim)

# Separar pousos e decolagens
df_pouso <- df %>% filter(Movimento == "Pouso") %>% arrange(Data_Hora)
df_decolagem <- df %>% filter(Movimento == "Decolagem") %>% arrange(Data_Hora)

# Sessão 2 - Premissas de Tempo de Turn Around ##########################

# CATEGORIAS DE EQUIPAMENTOS
EQUIPAMENTOS <- list(
  NARROWBODY_MODERNO = c("A20N", "A21N", "A320", "A321", "E295", "B38M", "B738", "73P", "738", "B734", "B735", "B737", "B732"),
  WIDEBODY = c("A332", "A339", "B772"),
  TURBO_PROPS = c("AT72", "C208"),
  EMBRAER_JETS = c("E190", "E195"),
  CARGA = c("32X", "B73F", "B763F")
)

# TEMPOS BASE POR EQUIPAMENTO (em minutos)
TEMPOS_BASE <- list(
  "AT72" = 30,
  "C208" = 30,
  "E190" = 35,
  "E195" = 40,
  "A20N" = 40,
  "A320" = 40,
  "A21N" = 50,
  "A321" = 50,
  "B738" = 45,
  "73P" = 45,
  "738" = 45,
  "B734" = 45,
  "B735" = 45,
  "B737" = 45,
  "B732" = 45,
  "B38M" = 45,
  "E295" = 45,
  "32X" = 60,
  "A332" = 90,
  "A339" = 90,
  "B772" = 90
)

# MODIFICADORES ESPECIAIS
MODIFICADORES <- list(
  INTERNACIONAL = list(
    "A20N" = 60,
    "E190" = 60,
    "E295" = 60,
    "A332" = 105,
    "A339" = 105
  ),
  CARGA = 60
)

INTERNACIONAIS_PAX <- c("ASU", "BRC", "FLL", "LIS", "MAD", "MCO", "MDZ", "MVD", "OPO", "ORY")
CARGA_DOM_EQP <- c("B738", "73P", "738", "AT72", "C208", "A21N", "A321", "B734", "B735", "B737", "B732", "32X")

# Função auxiliar: verificar se destino é internacional
eh_internacional <- function(destino) {
  if (is.na(destino)) return(FALSE)
  destino %in% INTERNACIONAIS_PAX
}

# FUNÇÃO PRINCIPAL ATUALIZADA COM CÓDIGOS DE SERVIÇO CLAROS
calcular_tempo_minimo <- function(equipamento, tipo_servico, destino = NA) {
  if (is.na(equipamento) || is.na(tipo_servico)) return(45)
  tipo_servico <- toupper(tipo_servico)
  
  if (tipo_servico %in% c("A", "F", "H")) return(60) # Carga
  
  internacional <- eh_internacional(destino)
  
  if (internacional && tipo_servico %in% c("C", "G", "J")) {
    if (equipamento %in% names(MODIFICADORES$INTERNACIONAL)) {
      return(MODIFICADORES$INTERNACIONAL[[equipamento]])
    }
    if (equipamento %in% EQUIPAMENTOS$WIDEBODY) return(105)
  }
  
  if (equipamento %in% CARGA_DOM_EQP) return(60)
  if (tipo_servico %in% c("C", "G", "J") && equipamento %in% names(TEMPOS_BASE)) {
    return(TEMPOS_BASE[[equipamento]])
  }
  
  return(45)
}

# Sessão 3 - Criar a tabela df_malha com a simulação da malha casada ################
criar_malha_casada <- function(df_pouso, df_decolagem) {
  if (nrow(df_pouso) == 0) {
    warning("df_pouso está vazio!")
    return(list())
  }
  if (nrow(df_decolagem) == 0) {
    warning("df_decolagem está vazio!")
    return(lapply(seq_len(nrow(df_pouso)), function(i) list(
      pouso = df_pouso[i, ],
      decolagem = NA,
      tempo_solo = NA,
      tempo_minimo_atendido = NA
    )))
  }
  
  pousos_disponiveis <- df_pouso %>% mutate(usado = FALSE, id_pouso = row_number())
  decolagens_disponiveis <- df_decolagem %>% mutate(usado = FALSE, id_decolagem = row_number())
  
  casamentos <- list()
  
  for (i in seq_len(nrow(pousos_disponiveis))) {
    if (pousos_disponiveis$usado[i]) next
    pouso_atual <- pousos_disponiveis[i, ]
    
    candidatas <- decolagens_disponiveis %>%
      filter(!usado, Empresa == pouso_atual$Empresa, Equipamento == pouso_atual$Equipamento, Data_Hora >= pouso_atual$Data_Hora) %>%
      arrange(Data_Hora)
    
    if (nrow(candidatas) == 0) {
      casamentos[[length(casamentos) + 1]] <- list(
        pouso = pouso_atual,
        decolagem = NA,
        tempo_solo = NA,
        tempo_minimo_atendido = NA
      )
      pousos_disponiveis$usado[i] <- TRUE
      next
    }
    
    tempo_minimo <- calcular_tempo_minimo(
      equipamento = pouso_atual$Equipamento,
      tipo_servico = pouso_atual$Tipo_Servico,
      destino = candidatas$Destino[1]
    )
    
    decolagem_escolhida <- NULL
    idx_escolhida <- NA
    
    for (j in seq_len(nrow(candidatas))) {
      candidata <- candidatas[j, ]
      tempo_solo <- as.numeric(difftime(candidata$Data_Hora, pouso_atual$Data_Hora, units = "mins"))
      if (tempo_solo >= tempo_minimo) {
        decolagem_escolhida <- candidata
        idx_escolhida <- candidata$id_decolagem
        break
      }
    }
    
    if (!is.null(decolagem_escolhida)) {
      tempo_solo_final <- as.numeric(difftime(decolagem_escolhida$Data_Hora, pouso_atual$Data_Hora, units = "mins"))
      casamentos[[length(casamentos) + 1]] <- list(
        pouso = pouso_atual,
        decolagem = decolagem_escolhida,
        tempo_solo = tempo_solo_final,
        tempo_minimo_atendido = TRUE
      )
      pousos_disponiveis$usado[i] <- TRUE
      decolagens_disponiveis$usado[decolagens_disponiveis$id_decolagem == idx_escolhida] <- TRUE
    } else {
      casamentos[[length(casamentos) + 1]] <- list(
        pouso = pouso_atual,
        decolagem = NA,
        tempo_solo = NA,
        tempo_minimo_atendido = FALSE
      )
      pousos_disponiveis$usado[i] <- TRUE
    }
  }
  
  decolagens_nao_casadas <- decolagens_disponiveis %>% filter(!usado)
  for (i in seq_len(nrow(decolagens_nao_casadas))) {
    casamentos[[length(casamentos) + 1]] <- list(
      pouso = NA,
      decolagem = decolagens_nao_casadas[i, ],
      tempo_solo = NA,
      tempo_minimo_atendido = NA
    )
  }
  
  return(casamentos)
}

# Executar algoritmo de casamento
print("Iniciando casamento de voos...")
casamentos <- criar_malha_casada(df_pouso, df_decolagem)
print(paste("Total de casamentos criados:", length(casamentos)))

# Converter para dataframe
lista_malha <- map(casamentos, function(casamento) {
  pouso_data <- if (is.null(casamento$pouso) || is.na(casamento$pouso)) {
    list(
      Empresa_Pouso = NA_character_,
      Voo_Pouso = NA_character_,
      Origem_Pouso = NA_character_,
      Tipo_Servico_Pouso = NA_character_,
      Equipamento_Pouso = NA_character_,
      Data_Pouso = as.Date(NA),
      Data_Hora_Pouso = as.POSIXct(NA)
    )
  } else {
    list(
      Empresa_Pouso = as.character(casamento$pouso$Empresa),
      Voo_Pouso = as.character(casamento$pouso$Voo),
      Origem_Pouso = as.character(casamento$pouso$Origem),
      Tipo_Servico_Pouso = as.character(casamento$pouso$Tipo_Servico),
      Equipamento_Pouso = as.character(casamento$pouso$Equipamento),
      Data_Pouso = as.Date(casamento$pouso$Data),
      Data_Hora_Pouso = as.POSIXct(casamento$pouso$Data_Hora)
    )
  }
  
  decolagem_data <- if (is.null(casamento$decolagem) || is.na(casamento$decolagem)) {
    list(
      Empresa_Decolagem = NA_character_,
      Voo_Decolagem = NA_character_,
      Destino_Decolagem = NA_character_,
      Tipo_Servico_Decolagem = NA_character_,
      Equipamento_Decolagem = NA_character_,
      Data_Decolagem = as.Date(NA),
      Data_Hora_Decolagem = as.POSIXct(NA)
    )
  } else {
    list(
      Empresa_Decolagem = as.character(casamento$decolagem$Empresa),
      Voo_Decolagem = as.character(casamento$decolagem$Voo),
      Destino_Decolagem = as.character(casamento$decolagem$Destino),
      Tipo_Servico_Decolagem = as.character(casamento$decolagem$Tipo_Servico),
      Equipamento_Decolagem = as.character(casamento$decolagem$Equipamento),
      Data_Decolagem = as.Date(casamento$decolagem$Data),
      Data_Hora_Decolagem = as.POSIXct(casamento$decolagem$Data_Hora)
    )
  }
  
  c(pouso_data, decolagem_data, list(
    Tempo_Solo_Minutos = as.numeric(casamento$tempo_solo),
    Tempo_Minimo_Atendido = as.logical(casamento$tempo_minimo_atendido),
    Tipo_Registro = case_when(
      !is.na(casamento$tempo_solo) ~ "Casamento_Completo",
      is.null(casamento$pouso) ~ "Decolagem_Isolada",
      is.null(casamento$decolagem) ~ "Pouso_Isolado",
      TRUE ~ "Indeterminado"
    )
  ))
})

df_malha <- bind_rows(lista_malha)

# Sessão 4 - Alocação no Gantt ######################
# Definição dos pátios
Patio_0R <- c("ADS01")
Patio_1R <- c("T01","T02","T03","T04","T05")
Patio_2R <- c("R01","R02","R03","R04","R05","R06","R07","R08","R09","R10","R10A","R11","R12","R13")
Patio_2I <- c("R14","R15","R16","R17","R18","R19")
Patio_3R <- c("M01","M02","M03","M04","M05","M06","M07","M08")
Patio_4R <- c("N101", "N102", "N103", "N104", "N105", "N106", "N107","N108")
Patio_4P <- c("C02", "C04", "C06", "C08", "C10", "C12", "C14")
Patio_5P <- c("C05", "C07", "C09", "C11", "C13", "C15", "B02","B04","B06","B08","B10","B12","B14")
Patio_6P <- c("B07","B09","B09A","B11","B13","B13A","A02A","A02","A04","A06A","A06","A08")

# Adjacências
adjacencias <- list(
  "A02" = c("A02A", "A04A"),
  "A06" = c("A06A", "A08"),
  "B09" = c("B07", "B09A"),
  "B13" = c("B11", "B13A")
)

# Função para mapear pátios
criar_mapeamento_patios <- function() {
  patios <- list(
    "Patio_0R" = Patio_0R,
    "Patio_1R" = Patio_1R,
    "Patio_2R" = Patio_2R,
    "Patio_2I" = Patio_2I,
    "Patio_3R" = Patio_3R,
    "Patio_4R" = Patio_4R,
    "Patio_4P" = Patio_4P,
    "Patio_5P" = Patio_5P,
    "Patio_6P" = Patio_6P
  )
  map_dfr(names(patios), function(nome_patio) {
    tibble(posicao = patios[[nome_patio]], categoria_patio = nome_patio)
  })
}

# Função para verificar posição livre
posicao_livre <- function(posicao, inicio, fim, ocupacoes, buffer_minutos = 15) {
  ocupacoes_posicao <- ocupacoes %>% filter(Posicao == posicao)
  if (nrow(ocupacoes_posicao) == 0) return(TRUE)
  inicio_com_buffer <- inicio - minutes(buffer_minutos)
  fim_com_buffer <- fim + minutes(buffer_minutos)
  for (i in seq_len(nrow(ocupacoes_posicao))) {
    ocupacao_inicio <- ocupacoes_posicao$Inicio[i]
    ocupacao_fim <- ocupacoes_posicao$Fim[i]
    if (!(fim_com_buffer <= ocupacao_inicio || inicio_com_buffer >= ocupacao_fim)) return(FALSE)
  }
  return(TRUE)
}

# Verificar adjacências
verificar_adjacencias <- function(posicao, inicio, fim, ocupacoes) {
  if (posicao %in% names(adjacencias)) {
    for (pos_bloqueada in adjacencias[[posicao]]) {
      if (!posicao_livre(pos_bloqueada, inicio, fim, ocupacoes, buffer_minutos = 20)) return(FALSE)
    }
  }
  for (pos_principal in names(adjacencias)) {
    if (posicao %in% adjacencias[[pos_principal]] && !posicao_livre(pos_principal, inicio, fim, ocupacoes, buffer_minutos = 20)) return(FALSE)
  }
  return(TRUE)
}

# Tentar alocação
tentar_alocacao <- function(voo, posicoes_candidatas, ocupacoes) {
  inicio <- voo$Data_Hora_Pouso
  fim <- voo$Data_Hora_Decolagem
  if (is.na(inicio) || is.na(fim)) return(NULL)
  for (posicao in posicoes_candidatas) {
    if (posicao_livre(posicao, inicio, fim, ocupacoes) && verificar_adjacencias(posicao, inicio, fim, ocupacoes)) {
      return(posicao)
    }
  }
  return(NULL)
}

# Função principal de alocação
alocar_voos <- function(df_malha, data_fim) {
  voos_para_alocar <- df_malha %>%
    filter(Tipo_Registro == "Casamento_Completo", as.Date(Data_Hora_Decolagem) == data_fim) %>%
    mutate(Prioridade = case_when(
      as.Date(Data_Hora_Pouso) < data_fim ~ 1,
      as.Date(Data_Hora_Pouso) == data_fim ~ 2,
      TRUE ~ 3
    )) %>%
    arrange(Prioridade, Data_Hora_Pouso)
  
  print(paste("Total de voos a serem alocados na data", data_fim, ":", nrow(voos_para_alocar)))
  
  ocupacoes <- tibble(Posicao = character(), Inicio = as.POSIXct(character()), Fim = as.POSIXct(character()), Voo_ID = character())
  resultado_alocacao <- tibble(
    Voo_Pouso = character(),
    Empresa = character(),
    Equipamento = character(),
    Posicao = character(),
    Data_Hora_Pouso = as.POSIXct(character()),
    Data_Hora_Decolagem = as.POSIXct(character()),
    Origem = character(),
    Destino = character(),
    Tipo_Servico = character(),
    Prioridade = numeric()
  )
  
  for (i in seq_len(nrow(voos_para_alocar))) {
    voo <- voos_para_alocar[i, ]
    origem <- voo$Origem_Pouso
    destino <- voo$Destino_Decolagem
    equipamento <- voo$Equipamento_Pouso
    tipo_servico <- voo$Tipo_Servico_Pouso
    eh_internacional <- !is.na(origem) && origem %in% INTERNACIONAIS_PAX || !is.na(destino) && destino %in% INTERNACIONAIS_PAX
    posicao_alocada <- NULL
    
    if (!is.na(equipamento) && equipamento == "AT72" && tipo_servico %in% c("C", "G", "J")) {
      posicao_alocada <- tentar_alocacao(voo, c(Patio_4R, Patio_2I), ocupacoes)
    }
    if (is.null(posicao_alocada) && !is.na(equipamento) && equipamento == "C208" && tipo_servico %in% c("C", "G", "J")) {
      posicao_alocada <- tentar_alocacao(voo, c("R14", "R15", Patio_1R), ocupacoes)
    }
    if (is.null(posicao_alocada) && !is.na(equipamento) && !eh_internacional && tipo_servico %in% c("C", "G", "J")) {
      posicao_alocada <- tentar_alocacao(voo, c(Patio_4P, Patio_5P, Patio_4R), ocupacoes)
    }
    if (is.null(posicao_alocada) && !is.na(equipamento) && equipamento %in% names(MODIFICADORES$INTERNACIONAL) && eh_internacional && tipo_servico %in% c("C", "G", "J")) {
      posicao_alocada <- tentar_alocacao(voo, c("A02", "A06", "B09", "B13"), ocupacoes)
    }
    if (is.null(posicao_alocada) && !is.na(equipamento) && equipamento %in% names(MODIFICADORES$INTERNACIONAL) && eh_internacional && tipo_servico %in% c("C", "G", "J")) {
      posicao_alocada <- tentar_alocacao(voo, c("A02A", "A04A", "A06A", "A08", "B07", "B09A", "B11", "B13A"), ocupacoes)
    }
    if (is.null(posicao_alocada) && !is.na(equipamento) && !(equipamento %in% CARGA_DOM_EQP) && tipo_servico %in% c("A", "F", "H")) {
      posicao_alocada <- tentar_alocacao(voo, Patio_3R, ocupacoes)
    }
    if (is.null(posicao_alocada) && !is.na(equipamento) && equipamento %in% CARGA_DOM_EQP && tipo_servico %in% c("A", "F", "H")) {
      posicao_alocada <- tentar_alocacao(voo, Patio_2R, ocupacoes)
    }
    if (is.null(posicao_alocada)) posicao_alocada <- "ADS01"
    
    if (posicao_alocada != "ADS01") {
      ocupacoes <- add_row(ocupacoes, Posicao = posicao_alocada, Inicio = voo$Data_Hora_Pouso, Fim = voo$Data_Hora_Decolagem, Voo_ID = paste(voo$Empresa_Pouso, voo$Voo_Pouso, sep = "_"))
    }
    
    resultado_alocacao <- add_row(resultado_alocacao,
                                  Voo_Pouso = voo$Voo_Pouso,
                                  Empresa = voo$Empresa_Pouso,
                                  Equipamento = voo$Equipamento_Pouso,
                                  Posicao = posicao_alocada,
                                  Data_Hora_Pouso = voo$Data_Hora_Pouso,
                                  Data_Hora_Decolagem = voo$Data_Hora_Decolagem,
                                  Origem = voo$Origem_Pouso,
                                  Destino = voo$Destino_Decolagem,
                                  Tipo_Servico = voo$Tipo_Servico_Pouso,
                                  Prioridade = voo$Prioridade)
  }
  return(resultado_alocacao)
}

# Executar alocação
print("Iniciando alocação de voos...")
alocacoes <- alocar_voos(df_malha, data_fim)
print(paste("Total de voos alocados:", nrow(alocacoes)))

# Mapeamento de pátios
mapeamento_patios <- criar_mapeamento_patios()
alocacoes_completas <- left_join(alocacoes, mapeamento_patios, by = c("Posicao" = "posicao"))

# Cores por equipamento
cores_equipamento <- c(
  "A20N" = "#FF6B6B", "A21N" = "#FF8E8E", "A320" = "#FFA0A0", "A321" = "#FFB2B2",
  "A332" = "#4ECDC4", "A339" = "#6DD5CD",
  "B738" = "#45B7D1", "B734" = "#5BC0DE", "B735" = "#6CC8E6", "B737" = "#7DD0EE", "B732" = "#8ED8F6",
  "73P" = "#96E0FF", "738" = "#A8E8FF", "B38M" = "#BAF0FF",
  "B772" = "#9B59B6", "B773" = "#A569C6",
  "E190" = "#F39C12", "E195" = "#F7A82E", "E295" = "#F9B54A",
  "AT72" = "#2ECC71", "C208" = "#58D68D",
  "32X" = "#E74C3C", "B73F" = "#EC7063", "B763F" = "#F1948A"
)

# Gráfico Gantt com anotações
criar_gantt <- function(alocacoes_completas, patios_filtro = NULL) {
  if (!is.null(patios_filtro)) {
    dados_filtrados <- alocacoes_completas %>% filter(categoria_patio %in% patios_filtro)
  } else {
    dados_filtrados <- alocacoes_completas
  }
  if (nrow(dados_filtrados) == 0) return(plot_ly() %>% layout(title = "Nenhum dado encontrado para os pátios selecionados"))
  
  dados_gantt <- dados_filtrados %>%
    mutate(
      Texto = paste(
        "Voo:", Voo_Pouso, "<br>",
        "Empresa:", Empresa, "<br>",
        "Equipamento:", Equipamento, "<br>",
        "Origem:", Origem, "<br>",
        "Destino:", Destino, "<br>",
        "Pouso:", format(Data_Hora_Pouso, "%d/%m %H:%M"), "<br>",
        "Decolagem:", format(Data_Hora_Decolagem, "%d/%m %H:%M"), "<br>",
        "Prioridade:", case_when(
          Prioridade == 1 ~ "Pousou dia anterior",
          Prioridade == 2 ~ "Pousou mesmo dia",
          TRUE ~ "Outros"
        )),
      Cor = ifelse(Equipamento %in% names(cores_equipamento), cores_equipamento[Equipamento], "#95A5A6")
    ) %>%
    arrange(Posicao, Data_Hora_Pouso)
  
  p <- plot_ly(data = dados_gantt, x = ~Data_Hora_Pouso, xend = ~Data_Hora_Decolagem, y = ~Posicao, color = ~categoria_patio, colors = "Set3", type = "bar", width = 20, hoverinfo = "text", text = ~Texto)
  
  # Adicionar anotações
  for (i in seq_len(nrow(dados_gantt))) {
    meio_x <- dados_gantt$Data_Hora_Pouso[i] + difftime(dados_gantt$Data_Hora_Decolagem[i], dados_gantt$Data_Hora_Pouso[i])/2
    p <- p %>%
      add_annotations(x = meio_x, y = dados_gantt$Posicao[i], text = dados_gantt$Equipamento[i], font = list(size = 10, color = "black"), showarrow = FALSE) %>%
      add_annotations(x = dados_gantt$Data_Hora_Pouso[i], y = dados_gantt$Posicao[i], text = dados_gantt$Origem[i], xanchor = "left", xshift = 5, yshift = -10, font = list(size = 8, color = "black"), showarrow = FALSE) %>%
      add_annotations(x = dados_gantt$Data_Hora_Decolagem[i], y = dados_gantt$Posicao[i], text = dados_gantt$Destino[i], xanchor = "right", xshift = -5, yshift = -10, font = list(size = 8, color = "black"), showarrow = FALSE)
  }
  
  p %>% layout(
    title = list(text = "Gráfico de Gantt - Alocação de Aeronaves por Posição de Pátio", font = list(size = 16)),
    xaxis = list(title = "Horário", type = "date", tickformat = "%H:%M", dtick = 3600000),
    yaxis = list(title = "Posições de Pátio", categoryorder = "array", categoryarray = rev(unique(dados_gantt$Posicao))),
    hovermode = "closest",
    legend = list(title = list(text = "Categoria de Pátio"), orientation = "v", x = 1.02, y = 1),
    margin = list(r = 150)
  )
}

# Executar gráfico
gantt_completo <- criar_gantt(alocacoes_completas)
gantt_completo
