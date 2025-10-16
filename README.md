

# Current: Clean Master’s degrees

We’ve asked librarians to send us data dumps of all teacher education
Master’s theses from their local library Brage system. These are in
`data-raw/` as `.csv`. We’re writing a script `data-raw/masters.r`,
which cleans the data and puts it into one big tibble which is written
to `data/masters.RDS`. The format is defined by `validate_masters()`.
For each institution, we write a function `process_institution()`, for
example `process_oslomet()`.

Goal: make `process` functions for each institution to complete
`masters.RDS`.
