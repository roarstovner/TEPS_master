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
-   `process_hivolda()`
-   `process_hvl()`
-   `process_inn()`
-   `process_nord()`
-   `process_oslomet_old()` / `process_oslomet_new()`
-   `process_uia()`
-   `process_uit()`
-   `process_usn()`

Nord ships its dump as `.xlsx` (read with `openxlsx2`); all other institutions are CSV.

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

### Hivolda — corrupt CSV export

The Hivolda Brage export `2025-03_hivolda_11250-3012619 (4).csv` arrived
in a triply broken state:

1.  Norwegian and typographic characters were stored as multi-byte
    mojibake (e.g. `å` is the byte pair `0xC7 0xBE`; smart quotes are
    3-byte placeholders ending in literal `??`).
2.  Every physical line had `;;;;;;` appended as junk.
3.  Multi-line abstract fields had spurious `"` wrappers added at every
    line break, which broke standard CSV parsing.

`process_hivolda()` undoes all three layers in memory before parsing
(see `R/process_masters.R`). After cleaning, all 43 thesis rows parse
to the expected 34-column schema. One additional source-level error
(a stray `"` inside a citation in the Norhagen 2024 thesis) is patched
explicitly.

A few residual data-quality limitations:

-   The single-byte mojibake `0xC7 ?` is ambiguous between `Ø`, `Å`,
    and `Ö`. We resolve it heuristically (`Å` when followed by a
    space, `Ø` otherwise), which is correct for the common Norwegian
    cases but produces `Ø` instead of `Ö` for the Turkish surname
    *Özerk* (cited twice in one abstract).
-   `dc.description.localcode` is populated for only 5 of 43 theses, so
    `GLU = NA` for the remaining 38. This mirrors the missing-GLU
    pattern at INN.

------------------------------------------------------------------------

### Nord — malformed export rows + sparse fields

The Nord `.xlsx` export contains 212 rows, but 3 have shifted columns
(values bled across columns at export time) and are dropped during
processing. The remaining 209 rows are kept.

Within those 209:

-   1 thesis has no `dc.description[en_US]`, so `GLU = NA`.
-   1 thesis has no author.
-   3 theses have no title.
-   All abstracts are `NA` — Nord's Brage export does not include the
    abstract column.

GLU (1-7 vs 5-10) is inferred from `dc.description[en_US]` rather than
from `collection`, because all Nord MGLU theses share the same
collection code (`11250/2721983`).

------------------------------------------------------------------------

### Oslomet — one missing year

One Oslomet thesis is missing a year in the metadata.

It remains: `year = NA`

------------------------------------------------------------------------

## Summary of data quality findings

-   **All missing values come from the source data.**
-   **UiT:** 207 theses have missing authors *and* year in the original CSV export.
-   **INN:** 77 theses are not part of a GLU collection → `GLU = NA`.
-   **Nord:** 3 malformed export rows dropped; abstracts not provided in source; small numbers of missing GLU/title/author within the kept 209 rows.
-   **Hivolda:** 43 theses recovered from a corrupt export; 38 have `GLU = NA` because `dc.description.localcode` is empty in the source.
-   **Oslomet:** 1 missing year.
-   The final dataset `masters.RDS` is complete. Missing values are kept as `NA` where metadata was absent in the source files.

------------------------------------------------------------------------
