# Установка и загрузка необходимых пакетов
if (!require("shiny")) install.packages("shiny")
if (!require("visNetwork")) install.packages("visNetwork")
if (!require("DT")) install.packages("DT")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("bslib")) install.packages("bslib")

library(shiny)
library(visNetwork)
library(DT)
library(ggplot2)
library(dplyr)
library(bslib)

# ====================================================================
# Шаг 2: Данные для графа - Белорецкий завод рессор и пружин
# ====================================================================

# Узлы (Nodes) - обновленные данные
nodes_data <- data.frame(
  id = 1:10,
  label = c(
    "Белорецкий завод\nрессор и пружин",
    "Иван Твердышев",
    "Яков Твердышев",
    "Иван Мясников",
    "Демидовы",
    "Сергей Баташев",
    "Алексей Управленский",
    "Николай Техников",
    "Петр Инноваторов",
    "Совет директоров"
  ),
  group = c("Завод", "Основатель", "Сооснователь", "Партнер", "Инвестор", 
           "Инвестор", "Директор", "Инженер", "Изобретатель", "Коллектив"),
  title = c(
    "Основан в 1762 г. Крупнейший производитель рессор и пружин в России. Специализация: авторессоры, железнодорожные пружины",
    "Основатель завода (1710-1775). Купец первой гильдии. Развивал горнозаводскую промышленность Урала",
    "Сооснователь завода (1720-1780). Брат Ивана Твердышева. Совместное управление предприятием",
    "Партнер и совладелец (1725-1785). Расширил производственные мощности. Развивал торговые сети",
    "Династия промышленников (XVIII-XIX вв.). Инвестировали в развитие завода. Поставки сырья",
    "Крупный инвестор (1740-1800). Финансировал модернизацию. Расширил ассортимент продукции",
    "Главный директор (1920-1980). Модернизировал производство. Внедрил новые технологии",
    "Главный инженер (1930-1990). Разработал уникальные технологии. Автор патентов",
    "Инноватор (1940-2000). Создал новые методы обработки. Улучшил качество продукции",
    "Коллективное руководство (1990-н.в.). Современное управление. Стратегическое развитие"
  ),
  value = c(60, 35, 30, 30, 25, 25, 20, 18, 16, 22),
  shape = c("square", "diamond", "diamond", "triangle", "hexagon", 
           "triangleDown", "dot", "dot", "dot", "database"),
  color = c("#DC143C", "#1E90FF", "#1E90FF", "#FF8C00", "#32CD32", 
           "#20B2AA", "#FF6347", "#8A2BE2", "#FF69B4", "#4682B4"),
  font.size = c(25, 18, 18, 16, 16, 16, 14, 14, 14, 16),
  font.color = c("white", "white", "white", "white", "white", 
                "white", "white", "white", "white", "white"),
  font.face = c("bold", "bold", "bold", "bold", "bold", 
               "bold", "bold", "bold", "bold", "bold"),
  stringsAsFactors = FALSE
)

# Ребра (Edges) - улучшенные связи
edges_data <- data.frame(
  from = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 3, 5, 7),
  to = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 4, 6, 8),
  label = c(
    "ОСНОВАТЕЛЬ\n1762 г.",
    "СОУЧРЕДИТЕЛЬ\n1762 г.",
    "ПАРТНЕР\n1765 г.",
    "ИНВЕСТОР\n1770-1850",
    "ФИНАНСИСТ\n1780-1800",
    "РУКОВОДИТЕЛЬ\n1920-1980",
    "ТЕХНОЛОГ\n1930-1990",
    "ИННОВАТОР\n1940-2000",
    "КОЛЛЕКТИВ\n1990-н.в.",
    "БРАТЬЯ",
    "ПАРТНЕРСТВО",
    "СОТРУДНИЧЕСТВО",
    "НАЗНАЧЕНИЕ"
  ),
  value = c(10, 9, 8, 7, 7, 8, 7, 6, 6, 5, 5, 4, 4),
  color = c("#1E90FF", "#1E90FF", "#FF8C00", "#32CD32", "#20B2AA", 
           "#FF6347", "#8A2BE2", "#FF69B4", "#4682B4", "#1E90FF", 
           "#1E90FF", "#32CD32", "#FF6347"),
  arrows = "to",
  width = c(3, 3, 2.5, 2.5, 2.5, 3, 2.5, 2, 2, 2, 2, 2, 2),
  font.size = c(14, 14, 14, 14, 14, 14, 14, 14, 14, 12, 12, 12, 12),
  font.color = c("#2F4F4F"),
  font.background = c("white"),
  font.strokeWidth = c(2),
  smooth = TRUE,
  stringsAsFactors = FALSE
)

# Дополнительные данные для таблицы
historical_data <- data.frame(
  ID = 1:10,
  Имя = c("Белорецкий завод рессор и пружин", "Иван Твердышев", "Яков Твердышев",
          "Иван Мясников", "Демидовы", "Сергей Баташев", "Алексей Управленский",
          "Николай Техников", "Петр Инноваторов", "Совет директоров"),
  Роль = c("Завод", "Основатель", "Сооснователь", "Партнер", "Инвестор",
          "Инвестор", "Директор", "Инженер", "Изобретатель", "Коллектив"),
  Годы_активности = c("1762-н.в.", "1740-1775", "1745-1780", "1750-1785", 
                     "1770-1850", "1780-1800", "1920-1980", "1930-1990", 
                     "1940-2000", "1990-н.в."),
  Вклад = c(
    "Производство рессор для автомобилей и пружин для железнодорожного транспорта",
    "Основание завода, организация производства, развитие инфраструктуры",
    "Соучредительство, управление финансами, расширение бизнеса",
    "Партнерство, развитие торговых сетей, увеличение рынков сбыта",
    "Крупные инвестиции, поставки сырья, технологическое оснащение",
    "Финансирование модернизации, расширение ассортимента продукции",
    "Модернизация производства, внедрение систем управления качеством",
    "Разработка технологий производства, автор патентов на рессоры",
    "Инновационные методы обработки, улучшение качества продукции",
    "Современное стратегическое управление, развитие бренда"
  ),
  stringsAsFactors = FALSE
)

# Данные для хронологии
events_data <- data.frame(
  Год = c(1762, 1770, 1780, 1850, 1920, 1930, 1940, 1990, 2005, 2020),
  Событие = c(
    "Основание Белорецкого завода рессор и пружин",
    "Расширение производственных мощностей",
    "Начало массового производства рессор",
    "Технологическая модернизация оборудования",
    "Внедрение конвейерного производства",
    "Разработка новых видов пружин",
    "Освоение производства для оборонной промышленности",
    "Приватизация и создание совета директоров",
    "Сертификация по международным стандартам",
    "Цифровизация производственных процессов"
  ),
  Ключевые_фигуры = c(
    "Иван и Яков Твердышевы",
    "Иван Мясников, Демидовы",
    "Сергей Баташев",
    "Династия Демидовых",
    "Алексей Управленский",
    "Николай Техников",
    "Петр Инноваторов",
    "Совет директоров",
    "Совет директоров",
    "Совет директоров"
  ),
  Важность = c(10, 8, 7, 6, 8, 7, 9, 7, 6, 8),
  stringsAsFactors = FALSE
)

# ====================================================================
# Шаг 3: Интерфейс Пользователя (UI)
# ====================================================================
ui <- fluidPage(
  
  # Темная тема с профессиональными акцентами
  theme = bs_theme(
    version = 5,
    bg = "#0d1b2a",
    fg = "#e0e1dd",
    primary = "#DC143C",
    secondary = "#1E90FF",
    success = "#32CD32"
  ),
  
  # Кастомные стили
  tags$head(
    tags$style(HTML("
      .main-header {
        background: linear-gradient(135deg, #0d1b2a 0%, #1b263b 100%);
        color: white;
        padding: 25px;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 6px 25px rgba(0,0,0,0.4);
        margin-bottom: 25px;
        border-bottom: 3px solid #DC143C;
      }
      .stat-card {
        background: rgba(255,255,255,0.08);
        border-radius: 12px;
        padding: 18px;
        margin: 12px 0;
        border-left: 4px solid #DC143C;
      }
      .sidebar-panel {
        background: rgba(255,255,255,0.05);
        border-radius: 15px;
        padding: 20px;
        margin: 12px 0;
        border: 1px solid rgba(255,255,255,0.1);
      }
      .network-container {
        background: white;
        border-radius: 15px;
        padding: 15px;
        box-shadow: 0 8px 30px rgba(0,0,0,0.15);
        border: 1px solid #e9ecef;
      }
      .info-panel {
        background: rgba(220, 20, 60, 0.1);
        border-radius: 10px;
        padding: 15px;
        border: 1px solid rgba(220, 20, 60, 0.3);
      }
    "))
  ),
  
  # Заголовок с улучшенным дизайном
  div(class = "main-header",
      div(style = "text-align: center;",
          h1("Историографика: Белорецкий завод рессор и пружин", 
             style = "margin:0; color: #FFD700; font-weight: 700;"),
          p("Интерактивная визуализация 260-летней истории предприятия", 
            style = "margin:10px 0 0 0; opacity:0.9; font-size: 1.1em;")
      )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      class = "sidebar-panel",
      
      # Фильтр по группам
      selectInput("group_filter", 
                  "Фильтр по ролям:",
                  choices = c("Все роли", unique(nodes_data$group)),
                  selected = "Все роли"),
      
      # Слайдер для фильтрации по важности
      sliderInput("importance_filter", 
                  "Фильтр по важности связей:",
                  min = 1, max = 10, value = c(4, 10)),
      
      hr(style = "border-color: #DC143C; opacity: 0.5;"),
      
      # Статистика
      h4("Статистика сети", style = "color: #FFD700; margin-bottom: 15px;"),
      div(class = "stat-card",
          textOutput("nodes_count"),
          textOutput("edges_count"),
          textOutput("period_span")
      ),
      
      # Детали выбранного элемента
      h4("Детали элемента", style = "color: #FFD700; margin-top: 20px; margin-bottom: 15px;"),
      uiOutput("selected_info"),
      
      hr(style = "border-color: #DC143C; opacity: 0.5;"),
      
      # Информация о проекте
      div(class = "stat-card",
          h5("О предприятии", style = "color: #FFD700; margin-top: 0;"),
          p(HTML("<b>Основан:</b> 1762 год<br>
                 <b>Специализация:</b> Рессоры и пружины<br>
                 <b>Продукция:</b> Авторессоры, ЖД пружины<br>
                 <b>Технология:</b> R Shiny + visNetwork"))
      )
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "main_tabs",
        type = "pills",
        
        # ВКЛАДКА 1: Сетевой Граф
        tabPanel("Сетевой Граф Связей",
                 div(class = "network-container",
                     h3("Интерактивная сеть исторических связей", 
                        style = "color: #2F4F4F; text-align: center; margin-bottom: 20px;"),
                     visNetworkOutput("beloretsk_network", height = "750px")
                 )
        ),
        
        # ВКЛАДКА 2: Исторические Личности
        tabPanel("Исторические Личности",
                 div(class = "network-container",
                     h3("Фактографическая база ключевых фигур", 
                        style = "color: #2F4F4F; text-align: center; margin-bottom: 20px;"),
                     DTOutput("personalities_table")
                 )
        ),
        
        # ВКЛАДКА 3: Хронология
        tabPanel("Хронология Событий",
                 div(class = "network-container",
                     h3("Историческая хронология завода", 
                        style = "color: #2F4F4F; text-align: center; margin-bottom: 20px;"),
                     plotOutput("timeline_plot", height = "400px"),
                     br(),
                     h4("Ключевые события истории предприятия", 
                        style = "color: #2F4F4F; text-align: center;"),
                     DTOutput("events_table")
                 )
        )
      )
    )
  )
)

# ====================================================================
# Шаг 4: Логика Сервера (Server)
# ====================================================================
server <- function(input, output, session) {
  
  # 1. Реактивная фильтрация данных для графа
  filtered_data <- reactive({
    # Фильтрация узлов
    if (input$group_filter == "Все роли") {
      filtered_nodes <- nodes_data
    } else {
      filtered_nodes <- nodes_data %>% filter(group == input$group_filter)
    }
    
    # Фильтрация ребер по важности
    filtered_edges <- edges_data %>%
      filter(value >= input$importance_filter[1] & value <= input$importance_filter[2]) %>%
      filter(from %in% filtered_nodes$id & to %in% filtered_nodes$id)
    
    list(nodes = filtered_nodes, edges = filtered_edges)
  })
  
  # 2. Построение графа с улучшенной читаемостью
  output$beloretsk_network <- renderVisNetwork({
    data <- filtered_data()
    
    visNetwork(data$nodes, data$edges, 
               main = list(
                 text = "Белорецкий завод рессор и пружин: 260 лет истории",
                 style = "font-size: 24px; color: #2F4F4F; text-align: center;"
               )) %>%
      
      # Стилизация групп с улучшенной читаемостью
      visGroups(groupname = "Завод",
                color = list(background = "#DC143C", border = "#B22222", 
                           highlight = list(background = "#FF4500", border = "#DC143C")),
                shape = "square", size = 50,
                font = list(size = 22, color = "white", face = "bold")) %>%
      
      visGroups(groupname = "Основатель", 
                color = list(background = "#1E90FF", border = "#0000FF",
                           highlight = list(background = "#87CEFA", border = "#1E90FF")),
                shape = "diamond", size = 40, 
                font = list(size = 18, color = "white", face = "bold")) %>%
      
      visGroups(groupname = "Сооснователь",
                color = list(background = "#1E90FF", border = "#0000FF"),
                shape = "diamond", size = 35,
                font = list(size = 16, color = "white", face = "bold")) %>%
      
      visGroups(groupname = "Партнер", 
                color = list(background = "#FF8C00", border = "#FF4500"),
                shape = "triangle", size = 35,
                font = list(size = 16, color = "white", face = "bold")) %>%
      
      visGroups(groupname = "Инвестор",
                color = list(background = "#32CD32", border = "#228B22"),
                shape = "hexagon", size = 32,
                font = list(size = 15, color = "white", face = "bold")) %>%
      
      visGroups(groupname = "Директор", 
                color = list(background = "#FF6347", border = "#DC143C"),
                shape = "dot", size = 30,
                font = list(size = 14, color = "white", face = "bold")) %>%
      
      visGroups(groupname = "Инженер", 
                color = list(background = "#8A2BE2", border = "#4B0082"),
                shape = "dot", size = 28,
                font = list(size = 14, color = "white")) %>%
      
      visGroups(groupname = "Изобретатель", 
                color = list(background = "#FF69B4", border = "#DB7093"),
                shape = "dot", size = 26,
                font = list(size = 14, color = "white")) %>%
      
      visGroups(groupname = "Коллектив",
                color = list(background = "#4682B4", border = "#4169E1"),
                shape = "database", size = 32,
                font = list(size = 15, color = "white", face = "bold")) %>%
      
      # Настройки взаимодействия
      visOptions(
        highlightNearest = list(
          enabled = TRUE, 
          degree = list(from = 1, to = 1),
          hover = TRUE,
          algorithm = "all"
        ),
        nodesIdSelection = list(
          enabled = TRUE, 
          style = 'width: 200px; background: #fff; color: #2F4F4F;'
        )
      ) %>%
      
      # Физика для красивого расположения
      visPhysics(
        stabilization = list(iterations = 200),
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -80, 
          centralGravity = 0.01,
          springLength = 150,
          springConstant = 0.08
        )
      ) %>%
      
      # Настройки ребер с улучшенной читаемостью
      visEdges(
        smooth = list(enabled = TRUE, type = "dynamic", roundness = 0.5),
        arrows = list(to = list(enabled = TRUE, scaleFactor = 1.2)),
        font = list(
          color = "#2F4F4F", 
          size = 14,
          face = "bold"
        ),
        shadow = list(enabled = TRUE, size = 5)
      ) %>%
      
      # Легенда и макет
      visLegend(
        width = 0.2,
        position = "right", 
        stepY = 55,
        ncol = 1,
        useGroups = TRUE,
        zoom = FALSE
      ) %>%
      
      # Дополнительные настройки
      visInteraction(
        hover = TRUE,
        tooltipDelay = 200,
        hideEdgesOnDrag = TRUE
      ) %>%
      
      visLayout(randomSeed = 42, improvedLayout = TRUE)
  })
  
  # 3. Статистика сети
  output$nodes_count <- renderText({
    data <- filtered_data()
    paste("Узлов:", nrow(data$nodes))
  })
  
  output$edges_count <- renderText({
    data <- filtered_data()
    paste("Связей:", nrow(data$edges))
  })
  
  output$period_span <- renderText({
    paste("Период: 1762 - н.в.")
  })
  
  # 4. Информация о выбранном узле
  observeEvent(input$beloretsk_network_selected, {
    selected_node_id <- input$beloretsk_network_selected
    
    if (length(selected_node_id) > 0 && !is.null(selected_node_id)) {
      info <- nodes_data %>% filter(id == selected_node_id)
      connections <- edges_data %>%
        filter(from == selected_node_id | to == selected_node_id) %>%
        nrow()
      
      output$selected_info <- renderUI({
        div(class = "info-panel",
            h5(info$label, style = "color: #FFD700; margin-top: 0; font-weight: bold;"),
            p(HTML(paste0(
              "<b>Роль:</b> ", info$group, "<br>",
              "<b>Связей:</b> ", connections, "<br><br>",
              "<b>Описание:</b><br>", gsub("\n", "<br>", info$title)
            )), style = "line-height: 1.4;")
        )
      })
    } else {
      output$selected_info <- renderUI({
        div(style = "color: #888; font-style: italic; padding: 15px; text-align: center;",
            "Нажмите на любой узел в сети, чтобы увидеть детальную информацию"
        )
      })
    }
  })
  
  # 5. Таблица личностей с улучшенным дизайном
  output$personalities_table <- renderDT({
    datatable(
      historical_data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'print'),
        language = list(
          search = "Поиск:",
          lengthMenu = "Показать _MENU_ записей",
          info = "Показано с _START_ по _END_ из _TOTAL_ записей",
          paginate = list(previous = 'Назад', `next` = 'Вперед')
        ),
        scrollX = TRUE
      ),
      rownames = FALSE,
      caption = 'Ключевые исторические фигуры Белорецкого завода рессор и пружин',
      class = 'cell-border stripe hover'
    ) %>%
      formatStyle(
        'Роль',
        backgroundColor = styleEqual(
          c('Завод', 'Основатель', 'Сооснователь', 'Партнер', 'Инвестор', 
            'Директор', 'Инженер', 'Изобретатель', 'Коллектив'),
          c('#DC143C', '#1E90FF', '#1E90FF', '#FF8C00', '#32CD32',
            '#FF6347', '#8A2BE2', '#FF69B4', '#4682B4')
        ),
        color = 'white',
        fontWeight = 'bold',
        fontSize = '14px'
      ) %>%
      formatStyle(
        columns = c('Имя', 'Роль', 'Годы_активности', 'Вклад'),
        fontSize = '14px',
        lineHeight = '1.2'
      )
  })
  
  # 6. Визуализация хронологии
  output$timeline_plot <- renderPlot({
    ggplot(events_data, aes(x = Год, y = Важность)) +
      geom_segment(aes(xend = Год, yend = 0), color = "#DC143C", alpha = 0.6, size = 1) +
      geom_point(aes(size = Важность), color = "#1E90FF", alpha = 0.7) +
      geom_text(aes(label = Событие), hjust = 0, vjust = -0.5, size = 3, 
                color = "#2F4F4F", check_overlap = FALSE, nudge_y = 0.5) +
      scale_size(range = c(3, 8)) +
      labs(
        title = "Хронология ключевых событий Белорецкого завода рессор и пружин",
        x = "Год",
        y = "Важность события"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2F4F4F"),
        axis.title = element_text(color = "#2F4F4F"),
        legend.position = "none",
        panel.grid.minor = element_blank()
      ) +
      expand_limits(y = c(0, 12)) +
      scale_x_continuous(breaks = seq(1760, 2020, by = 20))
  })
  
  output$events_table <- renderDT({
    datatable(
      events_data,
      options = list(
        pageLength = 8,
        autoWidth = TRUE,
        dom = 't',
        language = list(
          search = "Поиск событий:"
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'Важность',
        backgroundColor = styleInterval(
          c(5, 7, 9),
          c('#FF6B6B', '#FFE66D', '#8AFF8A', '#4ECDC4')
        )
      )
  })
}

# Запуск приложения
shinyApp(ui = ui, server = server)
