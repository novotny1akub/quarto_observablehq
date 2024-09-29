if(rstudioapi::isAvailable()){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

library(rvest)

ohq_to_quarto <- function(ohq_ref, output_dir, quarto_filename = NULL, echo = FALSE) {
  
  ohq_ref <- ohq_ref[1]
  if (grepl("^@", ohq_ref)) ohq_ref <- sprintf("https://observablehq.com/%s", ohq_ref)
  
  output_dir <- output_dir[1]
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  quarto_filename <- quarto_filename[1]
  
  pg <- rvest::read_html(ohq_ref)
  
  pg |> 
    html_nodes("script#__NEXT_DATA__") |> 
    html_text() |> 
    jsonlite::fromJSON() -> x
  
  meta <- x$props$pageProps$initialNotebook
  nodes <- x$props$pageProps$initialNotebook$nodes
  
  if (is.null(quarto_filename)) quarto_filename <- sprintf("%.qmd", meta$slug)
  
  c(
    "---", 
    sprintf("title: '%s'", meta$title), 
    "format: html", 
    if (echo) "echo: true" else "echo: false",
    "---",
    "",
    purrr::map2(nodes$value, nodes$mode, ~{
      c(
        
        "```{ojs}",
        dplyr::case_when(
          .y == "md" ~ sprintf("md`%s`", .x),
          .y == "html" ~ sprintf("html`%s`", .x),
          TRUE ~ .x
        ),
        "```",
        ""
      )
      
    })
  ) |> 
    purrr::flatten_chr() |> 
    cat(
      file = file.path(output_dir, quarto_filename), 
      sep = "\n"
    )
  
  if (length(meta$files)) {
    if (nrow(meta$files) > 0) {
      purrr::walk2(
        meta$files$download_url,
        meta$files$name, ~{
          download.file(
            url = .x,
            destfile = file.path(output_dir, .y),
            quiet = TRUE
          )
        }
      )
    }
  }
  
  print(paste0("quarto preview ", file.path(output_dir, quarto_filename)))
  
}

ohq_to_quarto(
  ohq_ref = "@juba/bar-chart-race", 
  output_dir = file.path(getwd(), "35_ohq"),
  quarto_filename = "quarto.qmd",
  echo = T
)

