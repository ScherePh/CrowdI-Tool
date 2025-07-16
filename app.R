#--------------------------- Load Packages ----------------------------------#
library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(showtext)
library(scales)
library(purrr)

#--------------------------- Font & Theme Setup ------------------------------#
theme_set(
  theme_minimal() +
    theme(
      text = element_text(family = "sourcesanspro"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_blank(),
      axis.text = element_text(size = 12),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5)
    )
)

#--------------------------- UI ---------------------------------------------#
ui <- fluidPage(
  titlePanel("\U0001F9ED Analyse-Tool CrowdI Umfragen"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Excel-Datei hochladen (.xlsx)", accept = ".xlsx"),
      uiOutput("frage_ui"),
      uiOutput("facet_ui"),
      textInput("titel", "Individueller Titel (optional):", ""),
      textAreaInput("order_values", "Reihenfolge der Ausprägungen (eine pro Zeile):", "", rows = 5),
      textAreaInput("order_facets", "Reihenfolge der Facets (eine pro Zeile):", "", rows = 5),
      sliderInput("plot_width", "Breite (px):", min = 400, max = 1600, value = 900),
      sliderInput("plot_height", "Höhe (px):", min = 200, max = 1000, value = 500),
      numericInput("facet_nrow", "Facets pro Zeile:", value = 2, min = 1, max = 6),
      selectInput("axis_labels", "Achsentitel anzeigen:",
                  choices = c("Beide", "Nur X", "Nur Y", "Keine"), selected = "Beide"),
      selectInput("category_labels", "Ausprägungen anzeigen auf:",
                  choices = c("Beide", "Nur X", "Nur Y", "Keine"), selected = "Beide"),
      downloadButton("download_plot", "\U0001F4E5 Grafik herunterladen")
    ),
    mainPanel(
      conditionalPanel("!input.file", h4("\u2b06 Bitte lade eine Excel-Datei hoch")),
      plotOutput("bar_plot", height = "auto", width = "100%"),
      tags$hr(),
      fluidRow(
        column(6,
               selectInput("chart_type", "Diagrammtyp:",
                           choices = list(
                             "Horizontal Balken" = "horizontal",
                             "Vertikal Balken" = "vertikal",
                             "Gestapelte Balken" = "stacked"
                           )),
               textInput("bar_color", "Balkenfarben (kommagetrennte Hex-Codes):", "#2b2b2b, #66c2a5, #8da0cb"),
               selectInput("wertdarstellung", "Werte anzeigen als:", choices = c("Prozent", "Absolut")),
               selectInput("wert_position", "Position der Wertebeschriftung:",
                           choices = c("In Balkenmitte", "Außerhalb rechts"))
        ),
        column(6,
               selectInput("legendenauswahl", "Legende Position:",
                           choices = c("Keine", "Rechts", "Links", "Unten", "Oben")),
               textInput("legend_title", "Legendentitel (optional):", ""),
               selectInput("sortierung", "Reihenfolge der Balken:",
                           choices = c("Absteigend", "Aufsteigend")),
               selectInput("grid_lines", "Gitternetzlinien:",
                           choices = c("Keine", "Horizontal", "Vertikal", "Beides"))
        )
      )
    )
  )
)

#--------------------------- Server ------------------------------------------#
server <- function(input, output, session) {
  
  clean_dataset <- function(data) {
    data %>%
      select(createDateTime, author.id, starts_with("answerData.")) %>%
      select(-ends_with(".type")) %>%
      rename_with(~ gsub("\\.feature", "\\.comment", .), starts_with("answerData.")) %>%
      rename_with(~ gsub("(^answerData\\.|\\.0$|\\.answer$|\\.data$|\\.comment$|\\.feature*\\.0\\.data$|\\.0\\.type$|\\.0\\.data$)", "", .), starts_with("answerData.")) %>%
      rename_with(~ gsub("\\.0\\.data$", "", .))
  }
  
  data_long <- reactive({
    req(input$file)
    raw <- read_excel(input$file$datapath)
    data <- clean_dataset(raw)
    
    all_cols <- colnames(data)
    base_vars <- all_cols[!str_detect(all_cols, "\\.[0-9]+$")]
    mc_base <- base_vars[sapply(base_vars, function(x) any(paste0(x, ".", 1:20) %in% all_cols))]
    sc_vars <- setdiff(base_vars, mc_base)
    mc_cols <- all_cols[all_cols %in% unlist(lapply(mc_base, function(x) c(x, paste0(x, ".", 1:20))))]
    
    sc_long <- if (length(sc_vars) > 0) {
      data %>% select(author.id, all_of(sc_vars)) %>%
        mutate(across(-author.id, as.character)) %>%
        pivot_longer(-author.id, names_to = "frage", values_to = "antwort")
    } else tibble(author.id = character(), frage = character(), antwort = character())
    
    mc_long <- if (length(mc_cols) > 1) {
      data %>% select(author.id, all_of(mc_cols)) %>%
        pivot_longer(-author.id, names_to = "frage_raw", values_to = "antwort") %>%
        mutate(frage = str_remove(frage_raw, "\\.[0-9]+$")) %>%
        filter(!is.na(antwort), antwort != "", antwort != "NA") %>%
        select(author.id, frage, antwort)
    } else tibble(author.id = character(), frage = character(), antwort = character())
    
    bind_rows(sc_long, mc_long) %>% filter(!is.na(antwort), antwort != "", antwort != "NA")
  })
  
  output$frage_ui <- renderUI({
    req(data_long())
    selectInput("frage", "Frage auswählen:", choices = unique(data_long()$frage))
  })
  
  output$facet_ui <- renderUI({
    req(data_long())
    if (input$chart_type %in% c("horizontal", "vertikal")) {
      selectInput("facet_var", "Gruppieren nach (optional):", choices = c("Keine", unique(data_long()$frage)))
    }
  })
  
  plot_reactive <- reactive({
    req(data_long(), input$frage)
    df <- data_long() %>% filter(frage == input$frage)
    if (nrow(df) == 0) return(NULL)
    
    auspr_ord <- str_split(input$order_values, "\n")[[1]] %>% str_trim() %>% discard(~.x == "")
    facet_ord <- str_split(input$order_facets, "\n")[[1]] %>% str_trim() %>% discard(~.x == "")
    colors <- str_split(input$bar_color, ",")[[1]] %>% str_trim()
    
    all_lvls <- if (length(auspr_ord) > 0) auspr_ord else unique(df$antwort)
    df$antwort <- factor(df$antwort, levels = all_lvls)
    
    if (input$chart_type %in% c("horizontal", "vertikal") && input$facet_var != "Keine") {
      df_facet <- data_long() %>% filter(frage == input$facet_var)
      df <- left_join(df, df_facet, by = "author.id", suffix = c("", ".facet")) %>%
        filter(!is.na(antwort.facet))
      if (length(facet_ord) > 0) df$antwort.facet <- factor(df$antwort.facet, levels = facet_ord)
      else df$antwort.facet <- factor(df$antwort.facet)
    } else {
      df$antwort.facet <- "Alle"
    }
    
    df_plot <- df %>%
      count(antwort.facet, antwort) %>%
      complete(antwort.facet, antwort = factor(all_lvls, levels = all_lvls), fill = list(n = 0)) %>%
      group_by(antwort.facet) %>%
      mutate(Proportion = n / sum(n)) %>%
      ungroup()
    
    df_plot$yval <- if (input$wertdarstellung == "Prozent") df_plot$Proportion * 100 else df_plot$n
    df_plot$label <- if (input$wertdarstellung == "Prozent") sprintf("%.0f%%", df_plot$yval) else df_plot$n
    
    legend_position <- switch(input$legendenauswahl,
                              "Keine" = "none", "Rechts" = "right",
                              "Links" = "left", "Unten" = "bottom", "Oben" = "top")
    
    # Dynamische Achsentitelsteuerung je nach Ausrichtung
    axis_text_x <- if (input$chart_type == "horizontal") {
      if (input$category_labels %in% c("Beide", "Nur Y")) element_text(size = 12) else element_blank()
    } else {
      if (input$category_labels %in% c("Beide", "Nur X")) element_text(size = 12) else element_blank()
    }
    
    axis_text_y <- if (input$chart_type == "horizontal") {
      if (input$category_labels %in% c("Beide", "Nur X")) element_text(size = 12) else element_blank()
    } else {
      if (input$category_labels %in% c("Beide", "Nur Y")) element_text(size = 12) else element_blank()
    }
    
    base_theme <- theme_minimal(base_family = "sourcesanspro") +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = if (input$axis_labels %in% c("Beide", "Nur X")) element_text(size = 12) else element_blank(),
        axis.title.y = if (input$axis_labels %in% c("Beide", "Nur Y")) element_text(size = 12) else element_blank(),
        axis.text.x = axis_text_x,
        axis.text.y = axis_text_y,
        legend.position = legend_position,
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        strip.text = element_text(size = 12, face = "bold"),
        plot.caption = element_text(size = 10, hjust = 1, face = "italic")
      )
    
    if (input$grid_lines == "Horizontal") {
      base_theme <- base_theme + theme(panel.grid.major.y = element_line(color = "grey80"))
    } else if (input$grid_lines == "Vertikal") {
      base_theme <- base_theme + theme(panel.grid.major.x = element_line(color = "grey80"))
    } else if (input$grid_lines == "Beides") {
      base_theme <- base_theme + theme(panel.grid.major = element_line(color = "grey80"))
    }
    
    caption_text <- paste0("n = ", nrow(df))
    
    p <- ggplot(df_plot, aes(x = antwort, y = yval, fill = antwort)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = label),
                position = if (input$wert_position == "Außerhalb rechts") position_stack(vjust = 1.05) else position_stack(vjust = 0.5),
                hjust = if (input$wert_position == "Außerhalb rechts") 0 else 0.5,
                size = 4,
                color = ifelse(input$wert_position == "Außerhalb rechts", "black", "white")) +
      scale_fill_manual(values = rep(colors, length.out = length(all_lvls))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = ifelse(input$titel == "", input$frage, input$titel),
           fill = input$legend_title,
           caption = caption_text,
           x = if (input$axis_labels %in% c("Beide", "Nur X")) "Antwort" else NULL,
           y = if (input$axis_labels %in% c("Beide", "Nur Y")) ifelse(input$wertdarstellung == "Prozent", "Prozent", "Anzahl") else NULL) +
      base_theme
    
    if (input$chart_type == "horizontal") {
      p + coord_flip() + facet_wrap(~antwort.facet, nrow = input$facet_nrow, scales = "free")
    } else if (input$chart_type == "vertikal") {
      p + facet_wrap(~antwort.facet, nrow = input$facet_nrow, scales = "free")
    } else {
      df_stack <- df %>%
        count(antwort) %>%
        complete(antwort = factor(all_lvls, levels = all_lvls), fill = list(n = 0)) %>%
        mutate(Proportion = n / sum(n),
               yval = if (input$wertdarstellung == "Prozent") Proportion * 100 else n,
               label = if (input$wertdarstellung == "Prozent") sprintf("%.0f%%", Proportion * 100) else n)
      
      ggplot(df_stack, aes(x = "Antworten", y = yval, fill = antwort)) +
        geom_bar(stat = "identity", width = 0.8) +
        geom_text(aes(label = label),
                  position = if (input$wert_position == "Außerhalb rechts") position_stack(vjust = 1.05) else position_stack(vjust = 0.5),
                  hjust = if (input$wert_position == "Außerhalb rechts") 0 else 0.5,
                  size = 4,
                  color = ifelse(input$wert_position == "Außerhalb rechts", "black", "white")) +
        coord_flip() +
        scale_fill_manual(values = rep(colors, length.out = length(all_lvls))) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        labs(title = ifelse(input$titel == "", input$frage, input$titel),
             fill = input$legend_title,
             caption = caption_text,
             x = NULL, y = NULL) +
        base_theme
    }
  })
  
  output$bar_plot <- renderPlot({
    req(input$file)
    req(plot_reactive())
  }, height = function() input$plot_height, width = function() input$plot_width)
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("plot_", input$frage, ".png"),
    content = function(file) {
      ggsave(file, plot = plot_reactive(), width = input$plot_width / 96, height = input$plot_height / 96, dpi = 96)
    }
  )
}

#--------------------------- Run App -----------------------------------------#
shinyApp(ui = ui, server = server)

  
