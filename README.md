# OSPO Survey 2026

Data cleaning and analysis for the Fall 2026 CURIOSS OSPO survey.

## Tech Stack

- **R** with `renv` for dependency management
- **Quarto** for reproducible notebooks

## Project Structure

```
ospo-survey-2026/
├── import_data.qmd       # Notebook: Data cleaning and exploration
├── data/                 # Data (not checked into version control)
│   └── raw_data.xlsx     # Raw survey responses
├── notebook_pdfs/        # PDF copies of my notebooks
├── renv/                 # R environment configuration
└── renv.lock             # R package versions
```

## To use

1. Ensure R is installed.
2. Open the project and run `renv::restore()` to install dependencies.
3. Run the Quarto notebooks to process and analyze the survey data.
4. If desired, render quarto notebooks to PDFs with `quarto render`.
