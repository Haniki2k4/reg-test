#----------------------------------------------------------------#
# PHẦN 1: THIẾT LẬP VÀ TẢI DỮ LIỆU
#----------------------------------------------------------------#

# 1.1. Tải các thư viện cần thiết
# Đảm bảo bạn đã cài đặt tất cả các gói này
# install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "haven", "janitor", "tidyr", "scales", "knitr", "kableExtra", "zoo", "forecast", "purrr", "plotly"))

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(haven)
library(janitor)
library(tidyr)
library(scales)
library(zoo)
library(forecast)
library(purrr)
library(plotly)

# 1.2. Hàm đọc dữ liệu (giữ nguyên từ file Rmd của bạn)
read_hh <- function(path, vars, mics) {
  df <- read_sav(path)
  filter_condition <- switch(
    as.character(mics),
    "6" = quote(HH12 == 1 & HH46 == 01), "5" = quote(HH9 == 01),
    "4" = quote(HH9 == 01), "3" = quote(hh9 == 1), "2" = quote(HI10 == 1), NULL
  )
  if (!is.null(filter_condition)) { df <- df %>% filter(!!filter_condition) }
  df <- df %>% dplyr::select(any_of(vars))
  return(df)
}

read_ch <- function(path, vars, mics) {
  df <- read_sav(path) 
  filter_condition <- switch(
    as.character(mics), "6" = quote(UF10 == 1), "5" = quote(UF9 == 1), "4" = quote(UF9 == 1), NULL
  )
  if (!is.null(filter_condition)) { df <- df %>% filter(!!filter_condition) }
  df <- df %>% dplyr::select(any_of(vars))
  return(df)
}

#----------------------------------------------------------------#
# PHẦN 1.3: TẢI VÀ XỬ LÝ TOÀN BỘ DỮ LIỆU (ĐÃ SỬA ĐƯỜNG DẪN)
#----------------------------------------------------------------#

# --- MICS 6 (2021) ---
vars_hh_ms6 <- c("HH1","HH2","HH6","WS1","WS11","helevel","HC2","windex5","windex5r","windex5u","HH5Y")
hh6 <- read_hh("data/Vietnam_MICS6_Datasets/hh.sav",
               vars_hh_ms6, mics = 6)
ch6 <- read_ch(
  "data/Vietnam_MICS6_Datasets/ch.sav",
  vars = c("HH1", "HH2", "CA1", "UF10"),
  mics = 6
)

# --- MICS 5 (2014) ---
vars_hh_ms5 <- c("HH1","HH2","HH6","WS1","WS8","helevel","windex5","HH5Y")
hh5 <- read_hh("data/Vietnam_MICS5_Datasets/hh.sav",
               vars_hh_ms5, mics = 5)
ch5 <- read_ch("data/Vietnam_MICS5_Datasets/ch.sav",
                vars = c("HH1", "HH2", "CA1", "UF9"),
                mics = 5)

# --- MICS 4 (2011) ---
vars_hh_ms4 <- c("HH1","HH2","HH6","WS1","WS8","helevel","windex5","HH5Y")
hh4 <- read_hh("data/Vietnam_MICS4_Datasets/hh.sav",
               vars_hh_ms4, mics = 4)
ch4 <- read_ch(
  "data/Vietnam_MICS4_Datasets/ch.sav",
  vars = c("HH1", "HH2", "CA1", "UF9"),
  mics = 4
)

# --- MICS 3 (2006) ---
vars_hh_ms3 <- c("diaban","hh2","hh6","ws1","ws7","helevel","wlthind5","hh5y")
hh3 <- read_hh("data/Vietnam_MICS3_Datasets/hh.sav",
               vars_hh_ms3, mics = 3)
ch3 <- read_ch(
  "data/Vietnam_MICS3_Datasets/ch.sav",
  vars = c("diaban", "hh2", "ca1"),
  mics = 3
)

# --- MICS 2 (2000) ---
vars_hh_ms2 <- c("HI1","HI2","HI6","WS1","WS3","ED16A","WLTHIND5")
hh2 <- read_hh("data/Vietnam_MICS2_Datasets/hhVI.sav",
               vars_hh_ms2, mics = 2)
ch2 <- read_ch(
  "data/Vietnam_MICS2_Datasets/chVI.sav",
  vars = c("HI1", "HI2", "CI1"),
  mics = 2
)

# Xử lý dữ liệu MICS 2021 (giữ nguyên)
ch6_clean <- ch6 %>% mutate(diarrhea = if_else(CA1 == 1, 1, 0))
diarrhea_hh <- ch6_clean %>% 
  group_by(HH1, HH2) %>% 
  summarise(
    any_diarrhea = if (all(is.na(diarrhea))) NA_real_ else max(diarrhea, na.rm = TRUE),
    .groups = "drop"
  )

hh6_clean <- hh6 %>% 
  left_join(diarrhea_hh, by = c("HH1", "HH2")) %>%
  mutate(
    safe_water = case_when(
      WS1 %in% c(11,12,13,14,21,31,41,51,72,91,92) ~ 1,
      WS1 %in% c(32,42,61,71,81,96,99) ~ 0,
      TRUE ~ NA_real_
    ),
    improved_sanitation = case_when(
      WS11 %in% c(11,12,21,22,31) ~ 1,
      WS11 %in% c(13,14,18,23,41,51,95,96,99) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Hồi quy Logistic (giữ nguyên)
model1 <- glm(improved_sanitation ~ windex5 + helevel + HH6 + HC2, data = hh6_clean, family = binomial)
model2 <- glm(improved_sanitation ~ windex5r + helevel + HC2, data = hh6_clean, family = binomial)
model3 <- glm(improved_sanitation ~ windex5u + helevel + HC2, data = hh6_clean, family = binomial)

# Xử lý dữ liệu chuỗi thời gian (giữ nguyên logic, chỉ cần đảm bảo các object hh2, hh3, hh4, hh5, hh6 đã được tải ở trên)
clean_mics2 <- hh2 %>% dplyr::select(area=HI6, water_source=WS1, toilet_type=WS3, wealth_quintile=WLTHIND5) %>% mutate(year=2000, improved_water=ifelse(water_source %in% c(1,2,3,4,5,6,8), 1, 0), improved_sanitation=ifelse(toilet_type %in% c(1,2,3,4), 1, 0))
clean_mics3 <- hh3 %>% dplyr::select(area=hh6, water_source=ws1, toilet_type=ws7, wealth_quintile=wlthind5, education=helevel, year=hh5y) %>% mutate(improved_water=ifelse(water_source %in% c(11,12,13,21,31,41,51), 1, 0), improved_sanitation=ifelse(toilet_type %in% c(11,12,13,21,22,31), 1, 0))
clean_mics4 <- hh4 %>% dplyr::select(area=HH6, water_source=WS1, toilet_type=WS8, wealth_quintile=windex5, education=helevel, year=HH5Y) %>% mutate(improved_water=ifelse(water_source %in% c(11,12,13,14,21,31,41,51,91), 1, 0), improved_sanitation=ifelse(toilet_type %in% c(11,12,21,22,31), 1, 0))
clean_mics5 <- hh5 %>% dplyr::select(area=HH6, water_source=WS1, toilet_type=WS8, wealth_quintile=windex5, education=helevel, year=HH5Y) %>% mutate(improved_water=ifelse(water_source %in% c(11,12,13,14,21,31,41,51), 1, 0), improved_sanitation=ifelse(toilet_type %in% c(11,12,21,22,31), 1, 0))
clean_mics6 <- hh6 %>% dplyr::select(area=HH6, water_source=WS1, toilet_type=WS11, wealth_quintile=windex5, education=helevel, year=HH5Y) %>% mutate(improved_water=ifelse(water_source %in% c(11,12,13,14,21,31,41,51,91,92), 1, 0), improved_sanitation=ifelse(toilet_type %in% c(11,12,21,22,31), 1, 0))

all_mics_data <- bind_rows(clean_mics6, clean_mics5, clean_mics4, clean_mics3, clean_mics2)
summary_stats <- all_mics_data %>%
  filter(!is.na(area)) %>%
  group_by(year, area) %>%
  summarise(
    pct_improved_water = mean(improved_water, na.rm = TRUE),
    pct_improved_sanitation = mean(improved_sanitation, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    year = as.numeric(year),
    pct_improved_water = as.numeric(pct_improved_water),
    area = as.numeric(area)
  )

#----------------------------------------------------------------#
# PHẦN 2: GIAO DIỆN NGƯỜI DÙNG (UI)
#----------------------------------------------------------------#

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Môi trường & Sức khỏe"),
  
  # -- Sidebar cho việc điều hướng --
  dashboardSidebar(
    sidebarMenu(
      menuItem("Giới thiệu", tabName = "intro", icon = icon("info-circle")),
      menuItem("Tổng quan MICS 2021", tabName = "overview", icon = icon("dashboard")),
      menuItem("Phân tích Suy luận", tabName = "inference", icon = icon("chart-line")),
      menuItem("Phân tích Chuỗi thời gian", tabName = "timeseries", icon = icon("clock"))
    )
  ),
  
  # -- Nội dung chính của ứng dụng --
  dashboardBody(
    tags$head(tags$style(HTML("
      #regression_summary, #arima_summary {
        font-size: 15px; 
      }
    "))),
    
    tabItems(
      # -- Tab 1: Giới thiệu --
      tabItem(tabName = "intro",
              fluidRow(
                box(width = 12, title = "Giới thiệu Dự án", status = "primary", solidHeader = TRUE,
                    h3("Chủ đề nghiên cứu:"),
                    p("Mối liên quan giữa điều kiện vệ sinh, nguồn nước, và sức khỏe của các thành viên trong hộ gia đình tại Việt Nam."),
                    h3("Câu hỏi nghiên cứu:"),
                    tags$ol(
                      tags$li("Mô tả tỷ lệ các hộ gia đình được tiếp cận với nguồn nước sạch và nhà tiêu hợp vệ sinh, so sánh giữa thành thị và nông thôn."),
                      tags$li("Kiểm định giả thuyết về mối liên quan giữa nguồn nước và tỷ lệ trẻ em bị tiêu chảy."),
                      tags$li("Xây dựng mô hình hồi quy logistic để xác định các yếu tố dự đoán khả năng sở hữu nhà tiêu hợp vệ sinh."),
                      tags$li("Phân tích xu hướng trong 20 năm qua và dự báo 10 năm tới.")
                    ),
                    h3("Nguồn dữ liệu:"),
                    p("Khảo sát Đánh giá các Mục tiêu về Trẻ em và Phụ nữ (MICS) của UNICEF tại Việt Nam, từ năm 2000 đến 2021.")
                )
              )
      ),
      
      # -- Tab 2: Thống kê mô tả MICS 2021 --
      tabItem(tabName = "overview",
              h2("Tổng quan Tình hình Nước sạch và Vệ sinh năm 2021"),
              fluidRow(
                tabBox(
                  width = 12,
                  id = "tabset1",
                  tabPanel("Tỷ lệ toàn quốc", 
                           fluidRow(
                             box(width = 6, title = "Tỷ lệ hộ sử dụng Nước sạch", status = "info", solidHeader = TRUE, plotlyOutput("pie_water")),
                             box(width = 6, title = "Tỷ lệ hộ có Nhà tiêu Hợp vệ sinh", status = "warning", solidHeader = TRUE, plotlyOutput("pie_sanitation"))
                           )
                  ),
                  tabPanel("So sánh theo khu vực",
                           fluidRow(
                             box(width = 6, title = "Nước sạch theo Khu vực", status = "info", solidHeader = TRUE, plotlyOutput("bar_water_area")),
                             box(width = 6, title = "Nhà tiêu HVS theo Khu vực", status = "warning", solidHeader = TRUE, plotlyOutput("bar_sanitation_area"))
                           )
                  )
                )
              )
      ),
      
      # -- Tab 3: Phân tích suy luận MICS 2021 --
      tabItem(tabName = "inference",
              h2("Phân tích Mối liên quan và các Yếu tố ảnh hưởng (2021)"),
              fluidRow(
                box(width = 12, title = "Kiểm định Chi-squared: Nước sạch và Tiêu chảy", status = "success", solidHeader = TRUE,
                    collapsible = TRUE,
                    fluidRow(
                      column(6, h4("Bảng chéo"), tableOutput("contingency_table")),
                      column(6, h4("Kết quả kiểm định"), verbatimTextOutput("chisq_result"))
                    )
                ),
                box(width = 12, title = "Mô hình Hồi quy Logistic: Dự đoán Nhà tiêu Hợp vệ sinh", status = "danger", solidHeader = TRUE,
                    collapsible = TRUE,
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("model_choice", "Chọn mô hình để xem:",
                                     choices = c("Mô hình tổng thể" = "model1",
                                                 "Mô hình cho Nông thôn" = "model2",
                                                 "Mô hình cho Thành thị" = "model3")),
                        helpText("Mô hình giúp xác định các yếu tố (mức sống, học vấn, khu vực, dân tộc) ảnh hưởng đến khả năng một hộ gia đình có nhà tiêu hợp vệ sinh.")
                      ),
                      mainPanel(
                        h4("Kết quả mô hình hồi quy"),
                        verbatimTextOutput("regression_summary")
                      )
                    )
                )
              )
      ),
      
      # -- Tab 4: Phân tích chuỗi thời gian --
      tabItem(tabName = "timeseries",
              h2("Xu hướng (2000-2021) và Dự báo (2022-2031)"),
              fluidRow(
                box(width = 12, title = "Xu hướng Tiếp cận Nước sạch & Nhà tiêu HVS", status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("Nước sạch", plotlyOutput("trend_plot_water")),
                      tabPanel("Nhà tiêu HVS", plotlyOutput("trend_plot_sanitation"))
                    )
                ),
                box(width = 12, title = "Dự báo bằng mô hình ARIMA", status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             selectInput("forecast_metric", "Chọn chỉ số dự báo:",
                                         choices = c("Tỷ lệ Nước sạch" = "pct_improved_water",
                                                     "Tỷ lệ Nhà tiêu HVS" = "pct_improved_sanitation")),
                             selectInput("forecast_area", "Chọn khu vực:",
                                         choices = c("Nông thôn" = 2, "Thành thị" = 1))
                      ),
                      column(8,
                             plotOutput("forecast_plot")
                      )
                    ),
                    fluidRow(
                      column(12,
                             h4("Thông tin mô hình ARIMA"),
                             verbatimTextOutput("arima_summary")
                      )
                    )
                )
              )
      )
    )
  )
)


#----------------------------------------------------------------#
# PHẦN 3: MÁY CHỦ (SERVER)
#----------------------------------------------------------------#

server <- function(input, output) {
  
  # --- Tab 2: Tổng quan MICS 2021 ---
  
  # Biểu đồ tròn Nước sạch
  output$pie_water <- renderPlotly({
    safe_tab <- hh6_clean %>% 
      filter(!is.na(safe_water)) %>% 
      count(safe_water) %>%
      mutate(label = ifelse(safe_water == 1, "Có", "Không"))
    
    plot_ly(safe_tab, labels = ~label, values = ~n, type = 'pie',
            marker = list(colors = c('#e74c3c', '#2ecc71'))) %>%
      layout(title = 'Tỷ lệ hộ sử dụng nguồn nước sạch')
  })
  
  # Biểu đồ tròn Nhà tiêu HVS
  output$pie_sanitation <- renderPlotly({
    san_tab <- hh6_clean %>% 
      filter(!is.na(improved_sanitation)) %>% 
      count(improved_sanitation) %>%
      mutate(label = ifelse(improved_sanitation == 1, "Có", "Không"))
    
    plot_ly(san_tab, labels = ~label, values = ~n, type = 'pie',
            marker = list(colors = c('#e67e22', '#3498db'))) %>%
      layout(title = 'Tỷ lệ hộ có nhà tiêu hợp vệ sinh')
  })
  
  # Biểu đồ cột Nước sạch theo khu vực
  output$bar_water_area <- renderPlotly({
    water_by_area <- hh6_clean %>% 
      filter(!is.na(safe_water)) %>% 
      group_by(HH6, safe_water) %>% 
      summarise(n = n(), .groups = "drop") %>% 
      group_by(HH6) %>% 
      mutate(pct = n / sum(n)) %>%
      mutate(KhuVuc = ifelse(HH6 == 1, "Thành thị", "Nông thôn"),
             NuocSach = ifelse(safe_water == 1, "Có", "Không"))
    
    p <- ggplot(water_by_area, aes(x = KhuVuc, y = pct, fill = NuocSach)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("Có" = "#2ecc71", "Không" = "#e74c3c")) +
      labs(x = "Khu vực", y = "Tỷ lệ", fill = "Nước sạch") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Biểu đồ cột Nhà tiêu HVS theo khu vực
  output$bar_sanitation_area <- renderPlotly({
    san_by_area <- hh6_clean %>% 
      filter(!is.na(improved_sanitation)) %>% 
      group_by(HH6, improved_sanitation) %>% 
      summarise(n = n(), .groups = "drop") %>% 
      group_by(HH6) %>% 
      mutate(pct = n / sum(n)) %>%
      mutate(KhuVuc = ifelse(HH6 == 1, "Thành thị", "Nông thôn"),
             NhaTieuHVS = ifelse(improved_sanitation == 1, "Có", "Không"))
    
    p <- ggplot(san_by_area, aes(x = KhuVuc, y = pct, fill = NhaTieuHVS)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("Có" = "#3498db", "Không" = "#e67e22")) +
      labs(x = "Khu vực", y = "Tỷ lệ", fill = "Nhà tiêu HVS") +
      theme_minimal()
    ggplotly(p)
  })
  
  # --- Tab 3: Phân tích Suy luận ---
  
  chisq_res <- reactive({
    chisq.test(hh6_clean$safe_water, hh6_clean$any_diarrhea)
  })
  
  output$contingency_table <- renderTable({
    req(chisq_res())
    # Thêm tên cho hàng và cột để dễ đọc hơn
    tbl <- as.data.frame.matrix(chisq_res()$observed)
    colnames(tbl) <- c("Không tiêu chảy", "Có tiêu chảy")
    rownames(tbl) <- c("Nước không sạch", "Nước sạch")
    tbl
  }, rownames = TRUE)
  
  output$chisq_result <- renderPrint({
    req(chisq_res())
    chisq_res()
  })
  
  output$regression_summary <- renderPrint({
    model <- switch(input$model_choice,
                    "model1" = model1,
                    "model2" = model2,
                    "model3" = model3)
    summary(model)
  })
  
  # --- Tab 4: Chuỗi thời gian ---
  
  # Biểu đồ xu hướng nước sạch
  output$trend_plot_water <- renderPlotly({
    p <- ggplot(summary_stats, aes(x = year, y = pct_improved_water, color = factor(area), group = factor(area))) +
      geom_line(linewidth = 1) + geom_point(size = 2.5) +
      scale_y_continuous(labels = scales::percent, limits = c(0.7, 1)) +
      scale_color_manual(name = "Khu vực", values = c("1" = "#00bec5", "2" = "#f9776d"), labels = c("1" = "Thành thị", "2" = "Nông thôn")) +
      labs(title = "Xu hướng Tiếp cận Nguồn nước sạch (2000–2021)", x = "Năm", y = "Tỷ lệ Hộ gia đình") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })
  
  # Biểu đồ xu hướng nhà tiêu HVS
  output$trend_plot_sanitation <- renderPlotly({
    p <- ggplot(summary_stats, aes(x = year, y = pct_improved_sanitation, color = factor(area), group = factor(area))) +
      geom_line(linewidth = 1) + geom_point(size = 2.5) +
      scale_y_continuous(labels = scales::percent, limits = c(0.2, 1)) +
      scale_color_manual(name = "Khu vực", values = c("1" = "#00bec5", "2" = "#f9776d"), labels = c("1" = "Thành thị", "2" = "Nông thôn")) +
      labs(title = "Xu hướng Sử dụng Nhà tiêu Hợp vệ sinh (2000-2021)", x = "Năm", y = "Tỷ lệ Hộ gia đình") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })
  
  # Reactive expression cho mô hình ARIMA
  arima_forecast_reactive <- eventReactive(c(input$forecast_metric, input$forecast_area), {
    
    value_col <- input$forecast_metric
    area_filter <- as.numeric(input$forecast_area)
    
    ts_data_sparse <- summary_stats %>% filter(area == area_filter)
    full_years <- data.frame(year = min(summary_stats$year):max(summary_stats$year))
    
    ts_data_full <- full_years %>% left_join(ts_data_sparse, by = "year")
    ts_data_full$interpolated <- zoo::na.approx(ts_data_full[[value_col]], na.rm = FALSE)
    
    ts_object <- ts(ts_data_full$interpolated, start = min(summary_stats$year), frequency = 1)
    
    arima_model <- forecast::auto.arima(ts_object)
    arima_forecast <- forecast::forecast(arima_model, h = 10)
    
    return(list(forecast = arima_forecast, model = arima_model, sparse_data = ts_data_sparse, value_col = value_col))
  })
  
  # Biểu đồ dự báo
  output$forecast_plot <- renderPlot({
    res <- arima_forecast_reactive()
    
    area_label <- ifelse(as.numeric(input$forecast_area) == 1, "Thành thị", "Nông thôn")
    title_text <- ifelse(input$forecast_metric == "pct_improved_water", "Nguồn nước sạch", "Nhà tiêu HVS")
    
    autoplot(res$forecast) +
      geom_point(data = res$sparse_data, aes_string(x = "year", y = res$value_col), color = "red", size = 3) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = paste("Dự báo:", title_text, "-", area_label),
        subtitle = "Dữ liệu nội suy từ các điểm khảo sát (màu đỏ)",
        x = "Năm", y = "Tỷ lệ Hộ gia đình (%)"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # Tóm tắt mô hình ARIMA
  output$arima_summary <- renderPrint({
    summary(arima_forecast_reactive()$model)
  })
  
}


# Chạy ứng dụng
shinyApp(ui, server)