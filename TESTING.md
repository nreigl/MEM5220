# Testing Guide for MEM5220

This document provides comprehensive testing procedures for the Quarto book project.

## Quick Testing Commands

```bash
# Validate project structure
python3 scripts/validate_quarto.py

# Check Quarto installation and project
quarto check

# Preview book (with live reload)
quarto preview

# Full render
quarto render

# Clean build artifacts
quarto clean
```

## Pre-Commit Testing Checklist

Before committing changes, verify:

- [ ] All `.qmd` files have valid syntax (no unclosed blocks)
- [ ] Code chunks use `#|` prefix for options
- [ ] Cross-references use modern syntax (`@sec-`, `@fig-`, `@tbl-`, `@eq-`)
- [ ] Child chunk references point to existing files
- [ ] YAML in `_quarto.yml` is valid
- [ ] R code executes without errors
- [ ] Preview renders correctly in browser
- [ ] Dark mode works properly
- [ ] All links and cross-references resolve

## Automated Testing

### Local Validation

Run the validation script before committing:

```bash
python3 scripts/validate_quarto.py
```

This checks:
- YAML syntax
- Code block pairing
- Div block pairing
- Cross-reference style
- Child chunk references
- Package name typos

### GitHub Actions

The `.github/workflows/quarto-publish.yml` workflow automatically:
- Validates project structure
- Installs R dependencies
- Renders the book
- Runs on every push/PR
- Deploys to GitHub Pages (main branch only)

## Testing Levels

### Level 1: Syntax Validation (Fast, ~5 seconds)

**What it tests**: File syntax without executing code

```bash
# Validate all .qmd files
python3 scripts/validate_quarto.py

# Check YAML syntax
python3 -c "import yaml; yaml.safe_load(open('_quarto.yml'))"

# Verify Quarto can parse project
quarto check
```

**When to run**: After every edit, before committing

### Level 2: Preview Testing (Medium, ~30-60 seconds)

**What it tests**: Renders book with code execution (using cache)

```bash
# Start preview server
quarto preview
```

**What to check**:
- [ ] Book renders without errors
- [ ] Code chunks execute successfully
- [ ] Figures display correctly
- [ ] Tables format properly
- [ ] Cross-references work
- [ ] Search functionality works
- [ ] Navigation is correct
- [ ] Dark/light mode toggle works

**When to run**: After significant changes, before pushing

### Level 3: Full Render (Slow, ~5-15 minutes)

**What it tests**: Complete fresh build without cache

```bash
# Clean everything and render from scratch
quarto clean
quarto render --execute-debug
```

**What to check**:
- [ ] All chapters render
- [ ] All R code executes
- [ ] No missing dependencies
- [ ] Bibliography renders
- [ ] PDF output (if needed)
- [ ] All cross-references resolve
- [ ] No broken links

**When to run**: Before major releases, periodically

## Common Issues and Solutions

### Issue 1: Unclosed Code Blocks

**Symptom**: Rendering fails with "unexpected end of document"

**Check**:
```bash
grep -c '```' file.qmd
# Should be even number
```

**Fix**: Add missing closing ` ``` `

### Issue 2: Unclosed Div Blocks

**Symptom**: Content appears in wrong callout/tabset

**Check**:
```bash
grep -c ':::' file.qmd
# Should be even number (outside code blocks)
```

**Fix**: Add missing `:::`

### Issue 3: Old-Style Cross-References

**Symptom**: References show as `??` or fail to render

**Bad**:
```markdown
See Figure \@ref(fig:myplot)
```

**Good**:
```markdown
See @fig-myplot
```

### Issue 4: R Package Not Found

**Symptom**: `Error: package 'xyz' not found`

**Fix**:
```r
# Install missing package
install.packages("xyz")

# Or add to .github/workflows/quarto-publish.yml
```

### Issue 5: Child Chunk File Not Found

**Symptom**: `Error: child file not found`

**Check**:
```bash
grep -n "child:" file.qmd
# Verify referenced file exists
```

**Fix**: Correct filename or create missing file

### Issue 6: Freeze Not Working

**Symptom**: Code re-executes every time despite no changes

**Check**:
```yaml
# In _quarto.yml
execute:
  freeze: auto  # Should be 'auto' not 'false'
```

**Reset freeze**:
```bash
rm -rf _freeze
quarto render
```

### Issue 7: Bibliography Not Rendering

**Symptom**: Citations show as `[@author2023]` instead of formatted references

**Check**:
```yaml
# In _quarto.yml
bibliography: MEM5220.bib  # File must exist
```

**Verify**:
```bash
ls -lh MEM5220.bib
```

## R Code Testing

### Test Individual Chunks

Open R/RStudio and test code chunks:

```r
# Load setup
source("before-chapter.R")

# Test chunk code
library(plm)
data("crime2", package = "wooldridge")
# ... rest of code
```

### Test All Code in a Chapter

```r
# In R console
knitr::purl("LM.qmd", output = "test_LM.R")
source("test_LM.R")
```

### Check Package Dependencies

```r
# List all packages used in a file
renv::dependencies("LM.qmd")

# Check if installed
sapply(c("plm", "lmtest", "sandwich"), requireNamespace)
```

## Continuous Integration Testing

### Local CI Simulation

Test the same way GitHub Actions will:

```bash
# Install all packages
Rscript -e 'install.packages(c("knitr", "rmarkdown", "tidyverse", ...))'

# Render
quarto render

# Check output
ls -lh _book/
```

### GitHub Actions Workflow

The CI workflow (`.github/workflows/quarto-publish.yml`) runs:

1. **Setup**: Install Quarto, R, and dependencies
2. **Validate**: Check project structure
3. **Render**: Build the book
4. **Test**: Verify output exists
5. **Deploy**: Publish to GitHub Pages (main only)

**View logs**: Go to repository → Actions → Latest workflow run

### Debugging CI Failures

1. **Check workflow logs** on GitHub Actions tab
2. **Look for error messages** in the render step
3. **Test locally** with same R version (4.3.2)
4. **Verify package versions** match DESCRIPTION

## Performance Testing

### Render Time Benchmarking

```bash
# Time full render
time quarto render

# Check cache usage
du -sh _freeze/

# Identify slow chunks
quarto render --profile timer
```

### Optimization Tips

- Use `freeze: auto` to cache chunk results
- Set `eval: false` for example-only code
- Use `cache: true` for expensive computations
- Consider `dependson` for chunk dependencies

## Accessibility Testing

### Check for

- [ ] Alternative text on all figures
- [ ] Proper heading hierarchy (H1 → H2 → H3)
- [ ] Descriptive link text (not "click here")
- [ ] Color contrast in custom themes
- [ ] Keyboard navigation works

### Tools

```bash
# Check HTML accessibility (requires npm)
npx pa11y-ci _book/index.html

# Validate HTML
npx html-validate _book/*.html
```

## Browser Testing

Test rendered output in:

- [ ] Chrome/Edge (latest)
- [ ] Firefox (latest)
- [ ] Safari (latest)
- [ ] Mobile browsers

**Check**:
- Responsive design
- Dark mode toggle
- Code copy buttons
- Search functionality
- Table of contents
- Cross-reference links

## Release Testing Checklist

Before creating a new release:

- [ ] All automated tests pass
- [ ] Full clean render completes
- [ ] Preview looks correct
- [ ] All cross-references work
- [ ] Bibliography renders
- [ ] PDF output works (if applicable)
- [ ] GitHub Actions deploys successfully
- [ ] Deployed site works on GitHub Pages
- [ ] No console errors in browser
- [ ] Dark mode works
- [ ] Mobile view works
- [ ] Update version number
- [ ] Update CHANGELOG

## Testing After Migration

After converting a chapter to native Quarto:

- [ ] Compare rendered output to original
- [ ] Verify all figures appear
- [ ] Check all tables format correctly
- [ ] Test all code chunks execute
- [ ] Verify cross-references resolve
- [ ] Check equations render properly
- [ ] Test tabsets expand/collapse
- [ ] Verify callouts display correctly
- [ ] Check syntax highlighting works
- [ ] Preview in dark mode

## Automated Testing Scripts

### scripts/validate_quarto.py

Comprehensive validation of `.qmd` files:
- YAML syntax
- Code/div block pairing
- Cross-reference style
- Child chunk references
- Common typos

**Usage**:
```bash
python3 scripts/validate_quarto.py
```

### scripts/check_links.py (TODO)

Check for broken links:
- Internal cross-references
- External URLs
- Image paths

### scripts/test_r_code.R (TODO)

Test all R code execution:
- Extract code from `.qmd`
- Execute in clean session
- Report errors

## Reporting Issues

When reporting rendering issues, include:

1. **Error message** (full text)
2. **File and line number** where error occurs
3. **Quarto version**: `quarto --version`
4. **R version**: `R --version`
5. **Operating system**
6. **Relevant code chunk** that fails
7. **Steps to reproduce**

## Resources

- [Quarto Documentation](https://quarto.org/docs/guide/)
- [Quarto Troubleshooting](https://quarto.org/docs/troubleshooting/)
- [Quarto GitHub Issues](https://github.com/quarto-dev/quarto-cli/issues)
- [R Markdown Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/)

## Version History

- **v1.0** (2024): Initial testing guide created during Quarto migration
