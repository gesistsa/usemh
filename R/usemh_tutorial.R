#' @export
use_mh_tutorial_template <- function(title = "tutorial", dir = NULL, file = "index.qmd") {
    active_dir <- getwd()
    if (is.null(dir)) {
        dir <- file.path(active_dir, gsub(" ", "_", title))
    }
    if (dir.exists(dir)) {
        rlang::abort("a directory with the same name already exists.")
    }
    dir.create(dir)
    setwd(dir)
    yaml_proj <- readLines(system.file("templates", "quarto.yaml", package = "usemh"))
    writeLines(
        whisker::whisker.render(
            yaml_proj,
            data = list("Package" = title, "file" = file)
        ), "_quarto.yml"
    )
    file.copy(system.file("templates", "tutorial.qmd", package = "usemh"), file)
    file.copy(system.file("templates", "apa.csl", package = "usemh"), "apa.csl")
    file.copy(system.file("templates", "references.bib", package = "usemh"), "references.bib")
    write("/.quarto/", ".gitignore", append = TRUE)
    cli::cli_alert_info("basic tutorial files created in {dir}. After finishing writing the tutorial run {.fn use_mh_tutorial_utils}")
    on.exit(setwd(active_dir))
}

#' @export
use_mh_tutorial_utils <- function(title = NULL, file = NULL, overwrite = TRUE) {
    if (!quarto::is_using_quarto()) {
        rlang::abort("no tutorial (qmd file) found in current directory.")
    }
    if (is.null(file)) {
        qmd_files <- list.files(pattern = "*qmd")
        if (length(qmd_files) > 1) {
            rlang::abort("more than one qmd file found, please specify file parameter")
        }
        file <- qmd_files
    }
    if (is.null(title)) {
        txt <- suppressWarnings(readLines(file))
        title <- gsub("title: ", "", txt[grepl("^title:", txt)])
        if (length(title) == 0) {
            rlang::abort("no title field found in file.")
        }
    }
    if (!file.exists("_quarto.yml") || isTRUE(overwrite)) {
        usethis::use_template(
            "quarto.yaml", "_quarto.yml",
            data = list("Package" = title, "file" = file), package = "usemh"
        )
        usethis::use_git_ignore("/.quarto/")
    }
    notebook <- paste0(tools::file_path_sans_ext(file), ".ipynb")
    if (!file.exists(notebook) || isTRUE(overwrite)) {
        system(paste0("quarto convert ", file, " --output ", notebook)) # TODO: Error handling
    }
    quarto:::quarto_use(args = c("binder", "--no-prompt"))
    if (!file.exists("install.R") || isTRUE(overwrite)) {
        .create_install_R()
    }
}

.create_install_R <- function(path = ".") {
    deps <- unique(renv::dependencies(path)$Package)
    reqs <- remotes::system_requirements("ubuntu", "22.04", package = deps)
    writeLines(reqs, "apt.txt")
    writeLines(paste0('install.packages("', deps, '")'), "install.R")
}
