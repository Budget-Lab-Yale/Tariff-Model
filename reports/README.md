# Report Generation

This directory contains templates and resources for generating State of Tariffs reports.

## Files

| File | Purpose |
|------|---------|
| `report_instructions.md` | Instructions for Claude API on how to write the report |
| `reference_report.md` | Example report in Markdown for style reference |
| `tariff_report_template.docx` | Original Word template (for reference) |
| `ybl_style.docx` | Pandoc reference doc for Yale Budget Lab styling |

## Setup

### 1. Create the Style Reference Document

The `ybl_style.docx` file is used by Pandoc to apply Yale Budget Lab formatting (fonts, colors, heading styles) to the generated report.

To create it:

1. Open Word and create a new document
2. Apply Yale Budget Lab styling:
   - Set fonts (e.g., Yale New for headings)
   - Define heading styles (Heading 1, Heading 2, etc.)
   - Set paragraph spacing
   - Define table styles
   - Set page margins
3. Save as `ybl_style.docx` in this directory

Alternatively, save a copy of a recent State of Tariffs report and delete all content (keeping only the styles).

### 2. Set API Key

Set your Anthropic API key:

```r
Sys.setenv(ANTHROPIC_API_KEY = 'your-api-key-here')
```

Or add to your `.Renviron` file:
```
ANTHROPIC_API_KEY=your-api-key-here
```

### 3. Install Pandoc

Pandoc converts Markdown to Word. Install from: https://pandoc.org/installing.html

Verify installation:
```bash
pandoc --version
```

## Usage

```r
source('src/run_model.R')

# After running scenarios...
generate_report(
  baseline_scenario = '11-17',
  comparison_scenario = '11-17-ex-ieepa',
  report_date = 'November 17, 2025',
  policy_changes = 'The administration expanded agricultural exemptions...',
  previous_report_date = 'October 30, 2025'
)
```

Or use the quick helper:
```r
quick_report('11-17', 'November 17, 2025')
```

## Output

Generated files are saved to `output/{scenario}/report/`:
- `state_of_tariffs.md` - Markdown source
- `state_of_tariffs.docx` - Word document
- `prompt.txt` - Full prompt sent to Claude (for debugging)

## Costs

Claude API pricing (approximate):
- Claude Sonnet: ~$3/M input tokens, ~$15/M output tokens
- A typical report generation costs a few cents

## Customization

### Changing the Model

Pass a different model to `generate_report()`:

```r
generate_report(..., model = 'claude-opus-4-20250514')  # Higher quality
generate_report(..., model = 'claude-sonnet-4-20250514')  # Default, good balance
```

### Editing Instructions

Modify `report_instructions.md` to change:
- Report structure
- Style guidelines
- How specific metrics are presented

### Updating the Reference

Update `reference_report.md` when report format changes significantly.
