------------------------------------------------------------------------

# Cleaning Master’s Theses in Teacher Education

We collect metadata dumps of all teacher education master’s theses from local Brage repositories at each institution. These `.csv` files are stored in `data-raw/`.

The cleaning script `data-raw/masters.R` runs all institution-specific processing functions from `R/process_masters.R`, merges the results, and writes the final combined dataset to:

`data/masters.RDS`

The output structure is validated with `validate_masters()`.

------------------------------------------------------------------------

## How the processing works

Each institution has its own processing function, for example:

-   `process_hiof()`
-   `process_hvl()`
-   `process_inn()`
-   `process_oslomet_old()` / `process_oslomet_new()`
-   `process_uia()`
-   `process_uit()`
-   `process_usn()`

Since Brage metadata formats differ between institutions, each function:

-   standardizes column names and variable formats\
-   extracts GLU categories from collection codes or filenames\
-   cleans authors, titles, and abstracts\
-   ensures that the final set of variables is consistent across all institutions

The goal is a single, unified tibble with identical columns despite different raw formats.

------------------------------------------------------------------------

## Data quality notes (source issues)

### UiT — missing year *and* authors for the same 207 theses

The UiT exports (`10037/8169` and `10037/8170`) are incomplete.\
Exactly **207 theses** have both `year` and `authors` missing in the CSV, even though the information *is visible on the UiT webpage*.

Checks:

sum(is.na(year)) = 207\
sum(is.na(authors)) = 207\
sum(is.na(year) & is.na(authors)) = 207\
sum(is.na(year) & !is.na(authors)) = 0\
sum(!is.na(year) & is.na(authors)) = 0

Breakdown by file:

| file | complete | missing_both |
|------|----------|--------------|
| 8169 | 131      | 11           |
| 8170 | 275      | 196          |

This is a **metadata export problem in UiT’s Brage system**, not a problem with our pipeline.

------------------------------------------------------------------------

### INN — 77 theses without GLU cathegory

INN provided three files:

-   two proper GLU collections\
-   one additional collection containing theses **not registered as GLU**

The third file has no `collection` code, and GLU cannot be inferred from title or subject.

Result:

`GLU = NA` for 77 theses\

------------------------------------------------------------------------

### Oslomet — one missing year

One Oslomet thesis is missing a year in the metadata.

It remains: `year = NA`

------------------------------------------------------------------------

## Summary of data quality findings

-   **All missing values come from the source data.**
-   **UiT:** 207 theses have missing authors *and* year in the original CSV export.
-   **INN:** 77 theses are not part of a GLU collection → `GLU = NA`.
-   **Oslomet:** 1 missing year.
-   The final dataset `masters.RDS` is complete. Missing values are kept as `NA` where metadata was absent in the source files.

------------------------------------------------------------------------
