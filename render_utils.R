library(roxygen2)
library(Rd2md)

script_path <- "utils.R"
out_md <- "utils_reference.md"

# Load utils.R into an isolated environment so roxygen2 can read default arguments
env <- new.env()
sys.source(script_path, envir = env)

# Parse roxygen blocks into Rd topics
blocks <- parse_file(script_path, env = env)
topics <- roclet_process(rd_roclet(), blocks, env = env, base_path = ".")

# Convert each topic directly to a Markdown string
md_sections <- lapply(topics, function(topic) {
  rd_tmp <- tempfile(fileext = ".Rd")
  writeLines(format(topic), rd_tmp)

  rd_obj <- read_rdfile(rd_tmp)
  paste(as_markdown(rd_obj, section_level = 2), collapse = "\n")
})

header <- c(
  "# `utils.R` Function Reference",
  paste0("*Auto-generated on ", Sys.Date(), "*\n"),
  "---\n"
)

writeLines(c(header, paste(md_sections, collapse = "\n\n---\n\n")), out_md)

# Call the Quarto CLI directly
system2("quarto", c("render", "utils_reference.md", "--to", "pdf"))
file.remove("utils_reference.md")
