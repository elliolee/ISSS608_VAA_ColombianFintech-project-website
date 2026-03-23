# ============================================================
# COFINFAD: Colombian Fintech Customer Analytics Dashboard
# Left nav for 3 modules — Module 1 implemented
# ============================================================

library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(DT)
library(scales)
library(heatmaply)
library(viridis)
library(bslib)

# ── Data ─────────────────────────────────────────────────────
customer_data <- read_excel("customer_data.xlsx") %>%
  mutate(
    gender              = as.factor(gender),
    customer_segment    = as.factor(customer_segment),
    acquisition_channel = as.factor(acquisition_channel),
    income_bracket      = factor(income_bracket,
                                 levels = c("Very Low","Low","Medium",
                                            "High","Very High")),
    city                = as.factor(city),
    age_group = cut(age,
                    breaks = c(18,25,35,45,55,70),
                    labels = c("18-25","26-35","36-45","46-55","55+"),
                    include.lowest = TRUE),
    active_products = savings_account + credit_card +
      personal_loan + investment_account +
      insurance_product
  )

seg_col <- c("inactive"  = "#95A5A6",
             "occasional"= "#3498DB",
             "power"     = "#E67E22",
             "regular"   = "#2ECC71")

# ══════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(tags$style(HTML("

    body, html { height:100%; margin:0; font-family:'Segoe UI',sans-serif; }
    .outer-wrap { display:flex; flex-direction:column; min-height:100vh; }

    /* ── top title bar ── */
    .top-bar {
      background:#1a252f; color:#fff;
      padding:13px 22px; font-size:1.1rem; font-weight:700;
      letter-spacing:.3px; flex-shrink:0;
      display:flex; align-items:center;
    }
    .top-bar span { opacity:.6; font-size:.85rem;
                    margin-left:12px; font-weight:400; }

    /* ── body row ── */
    .body-row { display:flex; flex:1; }

    /* ── module nav (leftmost) ── */
    .mod-nav {
      width:185px; min-width:185px; background:#2C3E50;
      display:flex; flex-direction:column;
      padding-top:14px; flex-shrink:0;
    }
    .mod-nav .nav-label {
      color:#7f8c8d; font-size:.7rem; font-weight:700;
      text-transform:uppercase; letter-spacing:.9px;
      padding:0 16px 8px;
    }
    .mod-btn {
      display:flex; align-items:center;
      width:100%; background:none; border:none;
      color:#bdc3c7; padding:11px 16px;
      font-size:.86rem; cursor:pointer;
      border-left:3px solid transparent;
      transition:background .15s, color .15s;
      text-align:left;
    }
    .mod-btn:hover   { background:#34495e; color:#ecf0f1; }
    .mod-btn.active  { background:#34495e; color:#fff;
                       border-left-color:#3498DB; font-weight:600; }
    .mod-num {
      display:inline-flex; align-items:center; justify-content:center;
      width:21px; height:21px; border-radius:50%;
      font-size:.7rem; font-weight:700; color:#fff;
      margin-right:9px; flex-shrink:0;
      background:#3498DB;
    }
    .mod-btn.dim .mod-num { background:#4a6278; }
    .mod-btn.dim { color:#7f96a8; }

    /* ── filter sidebar ── */
    .filter-panel {
      width:195px; min-width:195px; background:#f5f7f9;
      border-right:1px solid #dde;
      overflow-y:auto; padding:14px 12px; flex-shrink:0;
    }
    .filter-panel h6 {
      color:#2C3E50; font-weight:700; font-size:.78rem;
      text-transform:uppercase; letter-spacing:.5px;
      border-bottom:2px solid #3498DB;
      padding-bottom:5px; margin-bottom:10px; margin-top:4px;
    }
    .filter-panel .form-group { margin-bottom:7px; }
    .filter-panel label { font-size:.77rem; color:#555; margin-bottom:1px; }
    .filter-panel select { font-size:.79rem; }
    .filter-panel .form-check { padding-left:1.4rem; }
    .filter-panel .form-check-label { font-size:.77rem; }
    .filter-hr { border-color:#ccd; margin:9px 0; }

    /* ── main content ── */
    .main-panel {
      flex:1; overflow-y:auto;
      padding:16px 20px; background:#fff;
    }

    /* ── KPI row ── */
    .kpi-row {
      display:flex; gap:10px; margin-bottom:18px; flex-wrap:wrap;
    }
    .kpi-card {
      flex:1; min-width:105px; background:#fff;
      border-radius:10px; padding:12px 12px;
      text-align:center;
      box-shadow:0 2px 8px rgba(0,0,0,.07);
      border-top:3px solid #3498DB;
    }
    .kpi-val { font-size:1.5rem; font-weight:700; color:#2C3E50; }
    .kpi-lbl { font-size:.7rem; color:#7F8C8D; margin-top:3px;
               line-height:1.2; }

    /* ── coming soon placeholder ── */
    .coming-soon {
      display:flex; align-items:center; justify-content:center;
      height:65vh; flex-direction:column; color:#aaa; gap:10px;
    }
    .cs-icon { font-size:2.8rem; }
    .cs-title { font-size:1.05rem; font-weight:600; color:#888; }
    .cs-sub   { font-size:.85rem; color:#bbb; }

    /* sub-tab text size */
    .nav-tabs .nav-link { font-size:.84rem; padding:.4rem .9rem; }

  "))),
  
  div(class = "outer-wrap",
      
      # ── Top bar ─────────────────────────────────────────────
      div(class = "top-bar",
          "COFINFAD",
          tags$span("Colombian Fintech Customer Analytics Dashboard")
      ),
      
      div(class = "body-row",
          
          # ── Module navigation (leftmost column) ───────────────
          div(class = "mod-nav",
              div(class = "nav-label", "Modules"),
              
              tags$button(
                id = "btn_m1", class = "mod-btn active",
                onclick = "Shiny.setInputValue('active_module','mod1',{priority:'event'})",
                tags$span(class="mod-num","1"), "Customer Overview"
              ),
              tags$button(
                id = "btn_m2", class = "mod-btn dim",
                onclick = "Shiny.setInputValue('active_module','mod2',{priority:'event'})",
                tags$span(class="mod-num","2"), "Engagement"
              ),
              tags$button(
                id = "btn_m3", class = "mod-btn dim",
                onclick = "Shiny.setInputValue('active_module','mod3',{priority:'event'})",
                tags$span(class="mod-num","3"), "Retention & Value"
              )
          ),
          
          # ── Filter sidebar (rendered per module) ──────────────
          uiOutput("filter_sidebar"),
          
          # ── Main panel ────────────────────────────────────────
          div(class = "main-panel",
              uiOutput("main_content")
          )
      )
  ),
  
  # JS: sync active button highlight
  tags$script(HTML("
    Shiny.addCustomMessageHandler('highlight_btn', function(mod) {
      ['btn_m1','btn_m2','btn_m3'].forEach(function(id){
        var el = document.getElementById(id);
        el.classList.remove('active');
        el.classList.add('dim');
      });
      var a = document.getElementById('btn_' + mod);
      a.classList.add('active');
      a.classList.remove('dim');
    });
  "))
)

# ══════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  active_mod <- reactiveVal("mod1")
  
  observeEvent(input$active_module, {
    active_mod(input$active_module)
    session$sendCustomMessage("highlight_btn", input$active_module)
  })
  
  # ── Module 1 filtered data ──────────────────────────────
  m1 <- reactive({
    df <- customer_data
    if (!is.null(input$m1_seg)     && input$m1_seg     != "All")
      df <- df %>% filter(customer_segment    == input$m1_seg)
    if (!is.null(input$m1_gender)  && input$m1_gender  != "All")
      df <- df %>% filter(gender              == input$m1_gender)
    if (!is.null(input$m1_income)  && input$m1_income  != "All")
      df <- df %>% filter(income_bracket      == input$m1_income)
    if (!is.null(input$m1_channel) && input$m1_channel != "All")
      df <- df %>% filter(acquisition_channel == input$m1_channel)
    if (!is.null(input$m1_city)    && input$m1_city    != "All")
      df <- df %>% filter(city                == input$m1_city)
    if (isTRUE(input$m1_sav)) df <- df %>% filter(savings_account    == 1)
    if (isTRUE(input$m1_crd)) df <- df %>% filter(credit_card        == 1)
    if (isTRUE(input$m1_ln))  df <- df %>% filter(personal_loan      == 1)
    if (isTRUE(input$m1_inv)) df <- df %>% filter(investment_account == 1)
    if (isTRUE(input$m1_ins)) df <- df %>% filter(insurance_product  == 1)
    df
  })
  
  # ── Filter sidebar UI ────────────────────────────────────
  output$filter_sidebar <- renderUI({
    if (active_mod() == "mod1") {
      div(class = "filter-panel",
          tags$h6("Filters"),
          selectInput("m1_seg",     "Customer Segment",
                      c("All", levels(customer_data$customer_segment)),
                      width="100%"),
          selectInput("m1_gender",  "Gender",
                      c("All", levels(customer_data$gender)),
                      width="100%"),
          selectInput("m1_income",  "Income Bracket",
                      c("All", levels(customer_data$income_bracket)),
                      width="100%"),
          selectInput("m1_channel", "Acquisition Channel",
                      c("All", levels(customer_data$acquisition_channel)),
                      width="100%"),
          selectInput("m1_city",    "City",
                      c("All", levels(customer_data$city)),
                      width="100%"),
          tags$hr(class="filter-hr"),
          tags$h6("Product Owned"),
          checkboxInput("m1_sav", "Savings Account",    FALSE),
          checkboxInput("m1_crd", "Credit Card",        FALSE),
          checkboxInput("m1_ln",  "Personal Loan",      FALSE),
          checkboxInput("m1_inv", "Investment Account", FALSE),
          checkboxInput("m1_ins", "Insurance Product",  FALSE)
      )
    }
    # Modules 2 & 3: no filter panel
  })
  
  # ── Main content UI ──────────────────────────────────────
  output$main_content <- renderUI({
    switch(active_mod(),
           
           # ── MODULE 1 ──────────────────────────────────────────
           "mod1" = tagList(
             div(class = "kpi-row",
                 div(class="kpi-card",
                     div(class="kpi-val", textOutput("m1_kpi_total")),
                     div(class="kpi-lbl", "Total Customers")),
                 div(class="kpi-card",
                     div(class="kpi-val", textOutput("m1_kpi_prod")),
                     div(class="kpi-lbl", "Avg Active Products")),
                 div(class="kpi-card",
                     div(class="kpi-val", textOutput("m1_kpi_seg")),
                     div(class="kpi-lbl", "Dominant Segment")),
                 div(class="kpi-card",
                     div(class="kpi-val", textOutput("m1_kpi_age")),
                     div(class="kpi-lbl", "Avg Age")),
                 div(class="kpi-card",
                     div(class="kpi-val", textOutput("m1_kpi_ch")),
                     div(class="kpi-lbl", "Top Channel")),
                 div(class="kpi-card",
                     div(class="kpi-val", textOutput("m1_kpi_freq")),
                     div(class="kpi-lbl", "Avg Tx Frequency"))
             ),
             tabsetPanel(id = "m1_tabs",
                         tabPanel("Segment by Age",
                                  br(),
                                  fluidRow(
                                    column(6, plotlyOutput("m1_age_seg", height="370px")),
                                    column(6, plotlyOutput("m1_txfreq",  height="370px"))
                                  )
                         ),
                         tabPanel("Behaviour Analysis",
                                  br(),
                                  fluidRow(
                                    column(8, plotlyOutput("m1_scatter",      height="390px")),
                                    column(4, plotlyOutput("m1_channel_plot", height="390px"))
                                  ),
                                  br(),
                                  fluidRow(
                                    column(6, plotlyOutput("m1_gender_seg", height="310px")),
                                    column(6, plotlyOutput("m1_income_seg", height="310px"))
                                  )
                         ),
                         tabPanel("Product Adoption",
                                  br(),
                                  fluidRow(
                                    column(8, plotlyOutput("m1_product",     height="390px")),
                                    column(4, plotlyOutput("m1_active_prod", height="390px"))
                                  )
                         ),
                         tabPanel("City Heatmap",
                                  br(),
                                  plotlyOutput("m1_heatmap", height="570px")
                         ),
                         tabPanel("Explorer Table",
                                  br(),
                                  DTOutput("m1_table")
                         )
             )
           ),
           
           # ── MODULE 2 placeholder ──────────────────────────────
           "mod2" = div(class = "coming-soon",
                        div(class="cs-icon","📊"),
                        div(class="cs-title","Module 2 — Customer Engagement & Experience"),
                        div(class="cs-sub", "Coming soon")
           ),
           
           # ── MODULE 3 placeholder ──────────────────────────────
           "mod3" = div(class = "coming-soon",
                        div(class="cs-icon","🔍"),
                        div(class="cs-title","Module 3 — Retention Risk & Customer Value"),
                        div(class="cs-sub", "Coming soon")
           )
    )
  })
  
  # ── KPIs ─────────────────────────────────────────────────
  output$m1_kpi_total <- renderText(scales::comma(nrow(m1())))
  output$m1_kpi_prod  <- renderText(
    round(mean(m1()$active_products, na.rm=TRUE), 1))
  output$m1_kpi_seg   <- renderText({
    m1() %>% count(customer_segment) %>% slice_max(n, n=1) %>%
      pull(customer_segment) %>% as.character()
  })
  output$m1_kpi_age   <- renderText(round(mean(m1()$age, na.rm=TRUE), 1))
  output$m1_kpi_ch    <- renderText({
    m1() %>% count(acquisition_channel) %>% slice_max(n, n=1) %>%
      pull(acquisition_channel) %>% as.character()
  })
  output$m1_kpi_freq  <- renderText(
    round(mean(m1()$transaction_frequency, na.rm=TRUE), 1))
  
  # ── Segment by Age stacked bar ───────────────────────────
  output$m1_age_seg <- renderPlotly({
    df <- m1() %>% filter(!is.na(age_group)) %>%
      count(age_group, customer_segment) %>%
      group_by(age_group) %>% mutate(pct=n/sum(n))
    p <- ggplot(df, aes(x=age_group, y=pct, fill=customer_segment,
                        text=paste0(customer_segment,": ",
                                    scales::percent(pct,accuracy=0.1)))) +
      geom_bar(stat="identity", position="stack",
               colour="#2C3E50", linewidth=0.2) +
      scale_y_continuous(labels=percent_format()) +
      scale_fill_manual(values=seg_col) +
      labs(title="Segment Distribution by Age Group",
           x="Age Group", y="Proportion", fill="Segment") +
      theme_minimal(base_size=11) + theme(legend.position="bottom")
    ggplotly(p, tooltip="text") %>%
      layout(legend=list(orientation="h", y=-0.25))
  })
  
  # ── Tx Frequency boxplot ─────────────────────────────────
  output$m1_txfreq <- renderPlotly({
    df <- m1() %>% filter(!is.na(age_group))
    p <- ggplot(df, aes(x=age_group, y=transaction_frequency,
                        fill=customer_segment)) +
      geom_boxplot(alpha=0.7, position=position_dodge(0.8)) +
      scale_y_log10() +
      scale_fill_manual(values=seg_col) +
      labs(title="Transaction Frequency by Age & Segment",
           x="Age Group", y="Tx Frequency (log)", fill="Segment") +
      theme_minimal(base_size=11) + theme(legend.position="bottom")
    ggplotly(p) %>% layout(legend=list(orientation="h", y=-0.25))
  })
  
  # ── Scatter: Tx Freq vs Value ────────────────────────────
  output$m1_scatter <- renderPlotly({
    df <- m1() %>% slice_sample(n=min(5000, nrow(m1())))
    p <- ggplot(df, aes(x=transaction_frequency,
                        y=average_transaction_value/1000,
                        color=customer_segment,
                        text=paste0("Segment: ",customer_segment,
                                    "<br>Freq: ",
                                    round(transaction_frequency,1),
                                    "<br>Value (K): ",
                                    round(average_transaction_value/1000,1)))) +
      geom_point(alpha=0.4, size=1.2) +
      geom_smooth(method="lm", se=FALSE,
                  color="#2C3E50", linewidth=0.8) +
      scale_x_log10() +
      scale_color_manual(values=seg_col) +
      labs(title="Transaction Frequency vs Value",
           x="Tx Frequency (log)",
           y="Avg Tx Value (K COP)", color="Segment") +
      theme_minimal(base_size=11)
    ggplotly(p, tooltip="text") %>%
      layout(legend=list(orientation="h", y=-0.15))
  })
  
  # ── Acquisition channel ──────────────────────────────────
  output$m1_channel_plot <- renderPlotly({
    df <- m1() %>% count(acquisition_channel) %>%
      mutate(acquisition_channel=fct_reorder(acquisition_channel,n))
    p <- ggplot(df, aes(x=acquisition_channel, y=n,
                        fill=acquisition_channel,
                        text=paste0(acquisition_channel,": ",
                                    scales::comma(n)))) +
      geom_bar(stat="identity", show.legend=FALSE) +
      coord_flip() + scale_fill_brewer(palette="Set2") +
      labs(title="Acquisition Channel", x=NULL, y="Count") +
      theme_minimal(base_size=11)
    ggplotly(p, tooltip="text")
  })
  
  # ── Gender × Segment ─────────────────────────────────────
  output$m1_gender_seg <- renderPlotly({
    df <- m1() %>% count(gender, customer_segment) %>%
      group_by(gender) %>% mutate(pct=n/sum(n))
    p <- ggplot(df, aes(x=gender, y=pct, fill=customer_segment,
                        text=paste0(customer_segment,": ",
                                    scales::percent(pct,accuracy=0.1)))) +
      geom_bar(stat="identity", position="stack",
               colour="#2C3E50", linewidth=0.2) +
      scale_y_continuous(labels=percent_format()) +
      scale_fill_manual(values=seg_col) +
      labs(title="Segment by Gender",
           x=NULL, y="Proportion", fill="Segment") +
      theme_minimal(base_size=11) + theme(legend.position="bottom")
    ggplotly(p, tooltip="text") %>%
      layout(legend=list(orientation="h", y=-0.25))
  })
  
  # ── Income × Segment ─────────────────────────────────────
  output$m1_income_seg <- renderPlotly({
    df <- m1() %>% count(income_bracket, customer_segment) %>%
      group_by(income_bracket) %>% mutate(pct=n/sum(n))
    p <- ggplot(df, aes(x=income_bracket, y=pct, fill=customer_segment,
                        text=paste0(customer_segment,": ",
                                    scales::percent(pct,accuracy=0.1)))) +
      geom_bar(stat="identity", position="stack",
               colour="#2C3E50", linewidth=0.2) +
      scale_y_continuous(labels=percent_format()) +
      scale_fill_manual(values=seg_col) +
      labs(title="Segment by Income Bracket",
           x=NULL, y="Proportion", fill="Segment") +
      theme_minimal(base_size=11) +
      theme(axis.text.x=element_text(angle=30, hjust=1),
            legend.position="bottom")
    ggplotly(p, tooltip="text") %>%
      layout(legend=list(orientation="h", y=-0.3))
  })
  
  # ── Product adoption ─────────────────────────────────────
  output$m1_product <- renderPlotly({
    df <- m1() %>% group_by(customer_segment) %>%
      summarise(savings=mean(savings_account),
                credit=mean(credit_card),
                investment=mean(investment_account),
                loan=mean(personal_loan),
                insurance=mean(insurance_product)) %>%
      pivot_longer(-customer_segment,
                   names_to="product", values_to="adoption_rate")
    p <- ggplot(df, aes(x=product, y=adoption_rate,
                        fill=customer_segment,
                        text=paste0(customer_segment," — ",product,": ",
                                    scales::percent(adoption_rate,
                                                    accuracy=0.1)))) +
      geom_bar(stat="identity", position="dodge",
               colour="#2C3E50", linewidth=0.2, alpha=0.9) +
      scale_y_continuous(labels=percent_format()) +
      scale_fill_manual(values=seg_col) +
      labs(title="Product Adoption Rate by Segment",
           x="Product", y="Adoption Rate", fill="Segment") +
      theme_minimal(base_size=11) + theme(legend.position="bottom")
    ggplotly(p, tooltip="text") %>%
      layout(legend=list(orientation="h", y=-0.15))
  })
  
  # ── Avg active products ──────────────────────────────────
  output$m1_active_prod <- renderPlotly({
    df <- m1() %>% group_by(customer_segment) %>%
      summarise(avg=mean(active_products, na.rm=TRUE)) %>%
      mutate(customer_segment=fct_reorder(customer_segment, avg))
    p <- ggplot(df, aes(x=customer_segment, y=avg,
                        fill=customer_segment,
                        text=paste0(customer_segment,": ",round(avg,2)))) +
      geom_bar(stat="identity", show.legend=FALSE) +
      scale_fill_manual(values=seg_col) + coord_flip() +
      labs(title="Avg Active Products by Segment",
           x=NULL, y="Avg Products") +
      theme_minimal(base_size=11)
    ggplotly(p, tooltip="text")
  })
  
  # ── City heatmap (always full data) ──────────────────────
  output$m1_heatmap <- renderPlotly({
    df <- customer_data %>% group_by(city) %>%
      summarise(
        Tx_Frequency      = mean(transaction_frequency,     na.rm=TRUE),
        Daily_Tx          = mean(avg_daily_transactions,    na.rm=TRUE),
        Tx_Value          = mean(average_transaction_value, na.rm=TRUE),
        App_Logins        = mean(app_logins_frequency,      na.rm=TRUE),
        Feature_Diversity = mean(feature_usage_diversity,   na.rm=TRUE),
        Credit_Util       = mean(credit_utilization_ratio,  na.rm=TRUE),
        Satisfaction      = mean(satisfaction_score,        na.rm=TRUE),
        NPS               = mean(nps_score,                 na.rm=TRUE),
        Tenure            = mean(customer_tenure,           na.rm=TRUE),
        Churn_Prob        = mean(churn_probability,         na.rm=TRUE)
      )
    mat <- df %>% column_to_rownames("city") %>% as.matrix()
    heatmaply(scale(mat), colors=viridis(100),
              Rowv=TRUE, Colv=TRUE, k_row=4,
              xlab="Behaviour Indicators", ylab="Cities",
              showticklabels=c(TRUE,TRUE), dendrogram="row",
              key.title="Z-score", margins=c(60,100,40,20))
  })
  
  # ── Explorer table ────────────────────────────────────────
  output$m1_table <- renderDT({
    m1() %>%
      select(customer_id, age, age_group, gender, city,
             income_bracket, occupation, education_level,
             marital_status, acquisition_channel, customer_segment,
             active_products, savings_account, credit_card,
             personal_loan, investment_account, insurance_product) %>%
      datatable(
        filter="top", extensions="Buttons",
        options=list(pageLength=10, scrollX=TRUE,
                     dom="Bfrtip", buttons=c("copy","csv","excel"))
      )
  })
}

# ── Run ───────────────────────────────────────────────────────
shinyApp(ui, server)
