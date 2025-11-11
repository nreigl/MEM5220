# Quarto Migration Guide

This document explains the migration from R Markdown/bookdown to Quarto and provides guidance for contributors.

## What is Quarto?

[Quarto](https://quarto.org/) is an open-source scientific and technical publishing system built on Pandoc. It's the next generation of R Markdown, developed by Posit (formerly RStudio).

## Why Quarto?

### Advantages Over Bookdown

1. **Modern HTML Output**
   - Responsive design that works on all devices
   - Built-in dark mode
   - Better search functionality
   - Code folding and copying
   - Interactive features

2. **Better Performance**
   - `freeze` feature: Only re-run changed code
   - Faster builds for large books
   - Smarter caching

3. **Improved Cross-References**
   - Native support for figures, tables, equations
   - Consistent syntax across all reference types
   - No more `\@ref()` confusion

4. **Enhanced Features**
   - Callout blocks (notes, warnings, tips)
   - Tabsets for organizing content
   - Code annotation
   - Observable JS for interactivity

5. **Multi-Language Support**
   - R, Python, Julia, Observable JS
   - Future-proof for expanding to Python examples

6. **Better Developer Experience**
   - Cleaner syntax
   - Better error messages
   - Excellent documentation

## Project Structure

### Quarto Files (.qmd)

All chapter files have been converted to `.qmd` format:

- `index.qmd` - Introduction (fully converted)
- `LM.qmd` - Linear Models (using child chunk from LM.Rmd)
- `Panel.qmd` - Panel Data (using child chunk from Panel.Rmd)
- `GLM.qmd` - Generalized Linear Models (using child chunk from GLM.Rmd)
- `IV.qmd` - Instrumental Variables (using child chunk from IV.Rmd)
- `TimeSeries.qmd` - Time Series (using child chunk from TimeSeries.Rmd)
- `Bayes.qmd` - Bayesian Methods (using child chunk from Bayes.Rmd)
- `special_topics.qmd` - Special Topics (using child chunk from special_topics.Rmd)
- `references.qmd` - Bibliography

### Configuration Files

- `_quarto.yml` - Main Quarto configuration (replaces `_bookdown.yml`)
- `custom.scss` - Custom styling for light mode
- `custom-dark.scss` - Custom styling for dark mode

### Original Files (Preserved)

All original `.Rmd` files are preserved for backward compatibility:

- Used as child documents in `.qmd` files
- Can still be rendered with bookdown
- Will be gradually converted to native Quarto

## Building the Book

### Prerequisites

Install Quarto CLI:

- **Windows/Mac**: Download from [quarto.org](https://quarto.org/docs/get-started/)
- **Linux**:
  ```bash
  # Download and install the latest version
  wget https://quarto.org/download/latest/quarto-linux-amd64.deb
  sudo dpkg -i quarto-linux-amd64.deb
  ```

### Build Commands

```bash
# Preview the book (with live reload)
quarto preview

# Render to HTML
quarto render

# Render to PDF
quarto render --to pdf

# Render specific format
quarto render --to html

# Clean build artifacts
quarto clean
```

### RStudio Integration

RStudio (version ≥ 2022.07) has built-in Quarto support:

1. Open the project in RStudio
2. Use the "Render" button
3. Or use the Build pane

## Syntax Differences

### YAML Headers

**R Markdown (.Rmd)**
```yaml
---
title: "Chapter Title"
output: html_document
---
```

**Quarto (.qmd)**
```yaml
---
title: "Chapter Title"
format: html
---
```

*Note: Most YAML is now in `_quarto.yml`*

### Chunk Options

**R Markdown**
```r
```{r fig.width=8, echo=FALSE, warning=FALSE}
plot(x, y)
```
```

**Quarto**
```r
```{r}
#| label: fig-myplot
#| fig-width: 8
#| echo: false
#| warning: false
plot(x, y)
```
```

### Cross-References

**R Markdown**
```markdown
See Figure \@ref(fig:myplot).
See Table \@ref(tab:mytable).
See Equation \@ref(eq:myeq).
```

**Quarto**
```markdown
See @fig-myplot.
See @tbl-mytable.
See @eq-myeq.
```

### Callouts

**Quarto Only** (no bookdown equivalent)

```markdown
::: {.callout-note}
## Optional Title
This is a note.
:::

::: {.callout-warning}
This is a warning.
:::

::: {.callout-important}
This is important!
:::

::: {.callout-tip}
This is a tip.
:::
```

### Tabsets

**Quarto**
```markdown
::: {.panel-tabset}

## Tab 1
Content for tab 1

## Tab 2
Content for tab 2

:::
```

## Migration Strategy

### Phase 1: Initial Setup ✅ COMPLETE

- [x] Create `_quarto.yml` configuration
- [x] Create custom themes
- [x] Convert `index.Rmd` to `index.qmd`
- [x] Create placeholder `.qmd` files using child chunks
- [x] Update GitHub Actions
- [x] Update `.gitignore`

### Phase 2: Gradual Chapter Conversion (IN PROGRESS)

Each chapter should be converted individually:

1. **LM.qmd** - Linear Models (HIGH PRIORITY - largest chapter)
2. **Panel.qmd** - Panel Data
3. **GLM.qmd** - Generalized Linear Models
4. **IV.qmd** - Instrumental Variables
5. **TimeSeries.qmd** - Time Series
6. **Bayes.qmd** - Bayesian Methods
7. **special_topics.qmd** - Special Topics

### Phase 3: Enhancement

After conversion, add Quarto-specific features:

- [ ] Add callout blocks for important notes
- [ ] Use tabsets for alternative approaches
- [ ] Add code folding where appropriate
- [ ] Improve cross-references
- [ ] Add margin notes
- [ ] Consider Observable JS for interactive visualizations

## Conversion Checklist (Per Chapter)

When converting a chapter from `.Rmd` to `.qmd`:

- [ ] Copy content from `.Rmd` to `.qmd`
- [ ] Update chunk options to Quarto format (`#|`)
- [ ] Update cross-references (`\@ref()` → `@fig-`, etc.)
- [ ] Add proper labels to figures, tables, equations
- [ ] Convert special blocks to callouts
- [ ] Test that all code runs
- [ ] Build the book and check output
- [ ] Update references if needed

## Backward Compatibility

The project maintains both systems:

- **Bookdown**: Original `.Rmd` files + `_bookdown.yml`
- **Quarto**: New `.qmd` files + `_quarto.yml`

You can build either version:

```bash
# Bookdown (R Markdown)
Rscript -e 'bookdown::render_book("index.Rmd")'

# Quarto
quarto render
```

## CI/CD

### GitHub Actions

Two workflows are maintained:

1. **bookdown.yml** - Builds the bookdown version
2. **quarto-publish.yml** - Builds and deploys the Quarto version

The Quarto workflow:
- Runs on push/PR
- Installs all R dependencies
- Renders the book
- Deploys to GitHub Pages (on main branch)

### Local Testing

Before pushing, test locally:

```bash
# Quick preview
quarto preview

# Full render
quarto render

# Check for errors
quarto check
```

## Troubleshooting

### Common Issues

**1. Package not found**
```bash
# Install missing R packages
install.packages("package-name")
```

**2. Chunk option errors**
Check that options use `#|` prefix and proper YAML syntax:
```r
#| label: my-chunk
#| echo: false
```

**3. Cross-reference not working**
- Labels must start with prefix: `fig-`, `tbl-`, `eq-`
- References use `@` not `\@ref()`

**4. Build fails**
```bash
# Clean and rebuild
quarto clean
quarto render
```

## Resources

- [Quarto Documentation](https://quarto.org/docs/guide/)
- [Quarto Books](https://quarto.org/docs/books/)
- [Quarto vs R Markdown](https://quarto.org/docs/faq/rmarkdown.html)
- [Quarto Gallery](https://quarto.org/docs/gallery/)

## Getting Help

- Check [Quarto Discussions](https://github.com/quarto-dev/quarto-cli/discussions)
- Review [Quarto Issues](https://github.com/quarto-dev/quarto-cli/issues)
- Consult [R Markdown Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/) (many concepts apply)

## Contributing

When contributing to this project:

1. **New content**: Write in `.qmd` format using Quarto syntax
2. **Fixes to existing**: Update the `.qmd` file (or `.Rmd` if not yet converted)
3. **Always test**: Render locally before submitting PR
4. **Follow conventions**: Use callouts, proper cross-references, etc.

See [CONTRIBUTING.md](CONTRIBUTING.md) for full guidelines.
