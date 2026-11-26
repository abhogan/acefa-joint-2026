# Public inputs required for the study analysis

Copy the study's actual vaccine allocation file to:

- `inputs/vaccine_allocation_table.csv`

Required columns: `scenario`, `age_group_cea`, `doses`, `method`, `deliver`.
`method` is `IM` or `LAIV`; `deliver` is `school` or `other`.

Copy the life-table workbook to:

- `inputs/Australian Life Table.xlsx`

The source script reads an Excel worksheet containing `Age` and `Combined` columns.
Check licence conditions before redistributing the Australian life table.

The actual allocation and life-table input files were **not included in the supplied R-code upload**.
They are therefore not present in this draft. Synthetic equivalents are in `data_example/`.

Do not store the confidential model outputs here. Those belong in `data_private/`.
