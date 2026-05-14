library(shiny)
library(bslib)
library(DT)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

# ── Global ──────────────────────────────────────────────────────────────────

masters <- load_masters()

inst_choices <- sort(unique(masters$institution_short))
names(inst_choices) <- institution_labels[inst_choices]

glu_choices <- sort(unique(masters$GLU[!is.na(masters$GLU)]))

lang_choices <- sort(unique(masters$language[!is.na(masters$language)]))
names(lang_choices) <- ifelse(
  lang_choices %in% names(language_labels),
  language_labels[lang_choices],
  lang_choices
)

year_range <- range(masters$year, na.rm = TRUE)
n_authors_max <- max(masters$n_authors, na.rm = TRUE)

# ── UI ──────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = "Masters Thesis Browser",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # ── Browse tab ──
  nav_panel(
    "Browse",
    layout_sidebar(
      sidebar = sidebar(
        width = 290,
        textInput("search", "Search title + abstract",
          placeholder = "e.g. lesing motivasjon"
        ),
        selectizeInput("inst", "Institution",
          choices = inst_choices, multiple = TRUE,
          options = list(placeholder = "All institutions")
        ),
        sliderInput("year", "Year",
          min = year_range[1], max = year_range[2],
          value = year_range, step = 1, sep = ""
        ),
        selectizeInput("glu", "GLU program",
          choices = glu_choices, multiple = TRUE,
          options = list(placeholder = "All programs")
        ),
        selectizeInput("lang", "Language",
          choices = lang_choices, multiple = TRUE,
          options = list(placeholder = "All languages")
        ),
        sliderInput("n_authors", "Number of authors",
          min = 1, max = n_authors_max,
          value = c(1, n_authors_max), step = 1
        ),
        checkboxInput("has_abstract", "Only with abstract", value = FALSE),
        checkboxInput("has_subject", "Only with subject keywords", value = FALSE),
        actionButton("clear", "Clear filters", class = "btn-sm btn-outline-secondary")
      ),

      layout_columns(
        col_widths = c(7, 5),
        fill = TRUE,
        DTOutput("table"),
        tags$div(
          class = "viewer",
          radioButtons("view", NULL,
            choices = c("Abstract" = "abstract",
                        "Title" = "title",
                        "Metadata" = "meta"),
            inline = TRUE
          ),
          uiOutput("viewer_content")
        )
      )
    )
  ),

  # ── Stats tab ──
  nav_panel(
    "Stats",
    tags$div(
      style = "padding: 1rem 1.5rem;",
      tags$div(
        class = "stats-section",
        tags$h4("Theses by institution × year"),
        DTOutput("stats_inst_year")
      ),
      tags$div(
        class = "stats-section",
        tags$h4("Coverage by institution"),
        tags$p(class = "text-muted",
          "Share of theses with each field populated."),
        DTOutput("stats_coverage")
      ),
      tags$div(
        class = "stats-section",
        tags$h4("GLU program × institution"),
        DTOutput("stats_glu")
      ),
      tags$div(
        class = "stats-section",
        tags$h4("Language distribution"),
        DTOutput("stats_lang")
      )
    )
  )
)

# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  observeEvent(input$clear, {
    updateTextInput(session, "search", value = "")
    updateSelectizeInput(session, "inst", selected = character(0))
    updateSliderInput(session, "year", value = year_range)
    updateSelectizeInput(session, "glu", selected = character(0))
    updateSelectizeInput(session, "lang", selected = character(0))
    updateSliderInput(session, "n_authors", value = c(1, n_authors_max))
    updateCheckboxInput(session, "has_abstract", value = FALSE)
    updateCheckboxInput(session, "has_subject", value = FALSE)
  })

  filtered <- reactive({
    df <- masters

    if (length(input$inst) > 0)
      df <- df[df$institution_short %in% input$inst, ]

    df <- df[!is.na(df$year) &
             df$year >= input$year[1] & df$year <= input$year[2], ]

    if (length(input$glu) > 0)
      df <- df[!is.na(df$GLU) & df$GLU %in% input$glu, ]

    if (length(input$lang) > 0)
      df <- df[!is.na(df$language) & df$language %in% input$lang, ]

    df <- df[!is.na(df$n_authors) &
             df$n_authors >= input$n_authors[1] &
             df$n_authors <= input$n_authors[2], ]

    if (input$has_abstract)
      df <- df[!is.na(df$abstract) & nchar(df$abstract) > 0, ]

    if (input$has_subject)
      df <- df[!is.na(df$subject) & nchar(df$subject) > 0, ]

    df <- df[matches_query(df, input$search), ]
    df
  })

  output$table <- renderDT({
    df <- filtered()
    display <- data.frame(
      Institution = institution_labels[df$institution_short],
      Year        = df$year,
      GLU         = ifelse(is.na(df$GLU), "", df$GLU),
      Authors     = ifelse(is.na(df$authors), "", df$authors),
      Title       = ifelse(is.na(df$title), "", df$title),
      Lang        = ifelse(is.na(df$language), "", df$language),
      URL         = ifelse(
        is.na(df$url), "",
        paste0('<a href="', htmltools::htmlEscape(df$url),
               '" target="_blank">Link</a>')
      ),
      stringsAsFactors = FALSE
    )
    datatable(
      display,
      selection = "single",
      escape = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "lftip",
        columnDefs = list(
          list(width = "55px", targets = c(1, 5)),
          list(width = "50px", targets = 6)
        )
      ),
      rownames = FALSE
    )
  }, server = TRUE)

  selected_thesis <- reactive({
    idx <- input$table_rows_selected
    if (is.null(idx) || length(idx) == 0) return(NULL)
    filtered()[idx, ]
  })

  output$viewer_content <- renderUI({
    sel <- selected_thesis()
    if (is.null(sel)) {
      return(tags$p(class = "text-muted",
        "Select a row to view thesis details."))
    }

    q <- input$search

    if (input$view == "abstract") {
      blocks <- list()
      if (!is.na(sel$abstract) && nzchar(sel$abstract)) {
        blocks <- c(blocks, list(
          tags$div(class = "alt-label", "Abstract"),
          tags$div(class = "abstract-display",
                   HTML(highlight_tokens(sel$abstract, q)))
        ))
      }
      if (!is.na(sel$abstract_alt) && nzchar(sel$abstract_alt)) {
        blocks <- c(blocks, list(
          tags$div(class = "alt-label", "Abstract (alt language)"),
          tags$div(class = "abstract-display",
                   HTML(highlight_tokens(sel$abstract_alt, q)))
        ))
      }
      if (length(blocks) == 0) {
        return(tags$p(class = "text-muted",
          "No abstract available for this thesis."))
      }
      return(do.call(tagList, blocks))
    }

    if (input$view == "title") {
      blocks <- list(
        tags$div(class = "alt-label", "Title"),
        tags$div(class = "title-display",
                 HTML(highlight_tokens(sel$title, q)))
      )
      if (!is.na(sel$title_alt) && nzchar(sel$title_alt)) {
        blocks <- c(blocks, list(
          tags$div(class = "alt-label", "Title (alt language)"),
          tags$div(class = "title-display",
                   HTML(highlight_tokens(sel$title_alt, q)))
        ))
      }
      return(do.call(tagList, blocks))
    }

    # Metadata view
    rows <- list(
      list("Institution", institution_labels[sel$institution_short]),
      list("Year", as.character(sel$year)),
      list("GLU", ifelse(is.na(sel$GLU), "—", sel$GLU)),
      list("Authors", ifelse(is.na(sel$authors), "—", sel$authors)),
      list("# authors", as.character(sel$n_authors)),
      list("Language", ifelse(is.na(sel$language), "—", sel$language)),
      list("Subject", ifelse(is.na(sel$subject), "—", sel$subject)),
      list("Full text", ifelse(is.na(sel$full_text_available), "—",
                                sel$full_text_available)),
      list("Collection", ifelse(is.na(sel$collection), "—", sel$collection)),
      list("Handle", if (is.na(sel$url)) "—" else
        sprintf('<a href="%s" target="_blank">%s</a>',
                htmltools::htmlEscape(sel$url),
                htmltools::htmlEscape(sel$url))),
      list("ID", sel$id)
    )
    tag_rows <- lapply(rows, function(r) {
      tags$tr(
        tags$th(r[[1]]),
        tags$td(HTML(r[[2]]))
      )
    })
    tags$table(class = "meta-table", do.call(tags$tbody, tag_rows))
  })

  # ── Stats ──
  output$stats_inst_year <- renderDT({
    tab <- masters |>
      filter(!is.na(year)) |>
      count(institution_short, year) |>
      pivot_wider(names_from = year, values_from = n, values_fill = 0) |>
      arrange(institution_short) |>
      mutate(institution_short = institution_labels[institution_short]) |>
      rename(Institution = institution_short)
    year_cols <- setdiff(names(tab), "Institution")
    year_cols <- year_cols[order(as.integer(year_cols))]
    tab <- tab[, c("Institution", year_cols), drop = FALSE]
    tab$Total <- rowSums(tab[, year_cols, drop = FALSE])
    datatable(tab, rownames = FALSE,
      options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                     ordering = FALSE))
  })

  output$stats_coverage <- renderDT({
    tab <- masters |>
      group_by(institution_short) |>
      summarise(
        N            = n(),
        `Abstract %` = round(100 * mean(!is.na(abstract) & nchar(abstract) > 0)),
        `Subject %`  = round(100 * mean(!is.na(subject)  & nchar(subject)  > 0)),
        `Language %` = round(100 * mean(!is.na(language))),
        `Title alt %`    = round(100 * mean(!is.na(title_alt))),
        `Abstract alt %` = round(100 * mean(!is.na(abstract_alt))),
        .groups = "drop"
      ) |>
      mutate(Institution = institution_labels[institution_short], .before = 1) |>
      select(-institution_short)
    datatable(tab, rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = TRUE))
  })

  output$stats_glu <- renderDT({
    tab <- masters |>
      mutate(GLU = ifelse(is.na(GLU), "(missing)", GLU)) |>
      count(institution_short, GLU) |>
      pivot_wider(names_from = GLU, values_from = n, values_fill = 0) |>
      mutate(Institution = institution_labels[institution_short], .before = 1) |>
      select(-institution_short)
    datatable(tab, rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = TRUE))
  })

  output$stats_lang <- renderDT({
    tab <- masters |>
      mutate(language = ifelse(is.na(language), "(missing)", language)) |>
      count(institution_short, language) |>
      pivot_wider(names_from = language, values_from = n, values_fill = 0) |>
      mutate(Institution = institution_labels[institution_short], .before = 1) |>
      select(-institution_short)
    datatable(tab, rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = TRUE))
  })
}

shinyApp(ui, server)
