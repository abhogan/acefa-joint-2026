"""Conservative static release check: run before any public repository push."""
from pathlib import Path
import re
import sys
root = Path(__file__).resolve().parents[1]
issues = []
for path in root.rglob('*'):
    if not path.is_file():
        continue
    rel = path.relative_to(root)
    relstr = str(rel).replace('\\', '/')
    if relstr.startswith('data_private/') and relstr != 'data_private/.gitkeep':
        issues.append(f'Restricted directory contains: {relstr}')
        continue
    if relstr.startswith('outputs/') and relstr != 'outputs/.gitkeep':
        issues.append(f'Unreviewed result present: {relstr}')
        continue
    if path.suffix.lower() in {'.dta', '.rdata', '.rda', '.rds'}:
        issues.append(f'Sensitive-file type present: {relstr}')
        continue
    if path.suffix.lower() not in {'.r', '.md', '.py', '.txt', '.csv', '.gitignore'}:
        continue
    text = path.read_text(encoding='utf-8', errors='replace')
    if relstr.startswith('data_example/'):
        continue  # Known synthetic data only.
    # Do not check the checker itself: patterns below would match their own source.
    if relstr == 'tests/check_public_release.py':
        continue
    if re.search(r'C:[/\\]Users[/\\]|OneDrive - The University|(?:\\\\|/home/[^/]+/)', text, re.I):
        issues.append(f'Absolute/private path reference in {relstr}')

if issues:
    print('RELEASE CHECK FAILED:\n' + '\n'.join('- ' + x for x in issues))
    sys.exit(1)
print('Static scan passed. Also inspect Git history and approve all files manually.')
