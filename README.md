# query

Quarto website for querying and reporting on research projects.

## Structure

- `index.qmd` - Home page
- `OIS/` - OIS project
  - `progetti_propri.qmd` - Progetti Propri
  - `bandi.qmd` - Bandi
  - `data_in/` - Input data
  - `data_out/` - Output data
- `SIME/` - SIME project (coming soon)
  - `data_in/` - Input data
  - `data_out/` - Output data
- `R/` - Shared R functions
- `assets/` - Styles and bibliography
- `docs/` - Rendered website output

## Usage

Render the website:

```bash
quarto render
```

Preview locally:

```bash
quarto preview
```