# Before making this repository public

1. Obtain approval from the UNSW/UoM model data owners for the code/data availability statement.
2. Never commit `data_private/`, `.RData`, `.rds`, `.dta`, or `outputs/` by force (`git add -f`).
3. Confirm the provenance and redistribution rights of external life-table inputs.
4. Run `python tests/check_public_release.py` before every public release.
5. Verify that results from the refactored scripts agree with the original model on approved inputs.
6. Check `git status`, `git ls-files` and Git history for previously tracked restricted files.
7. Add a licence, citation, contributor list and final manuscript DOI only with coauthor approval.

A `.gitignore` is not a security boundary. Files accidentally committed in the past can remain recoverable from Git history even after deletion.
