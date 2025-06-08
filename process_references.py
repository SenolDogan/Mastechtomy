import regex as re
from collections import OrderedDict

# Read the full text
with open('full_text.txt', 'r', encoding='utf-8') as f:
    text = f.read()

# Split text and reference section
ref_split = re.split(r'\nReferences \(extra cited In the text\):|\nReferences:', text, flags=re.IGNORECASE)
main_text = ref_split[0]
ref_section = ref_split[1] if len(ref_split) > 1 else ''

# Extract all author-year references from the main text
# Matches (Author et al., 2020), (Author1 and Author2, 2020), (Author, 2020), (Author et al., 2020; Author2 et al., 2021)
ref_pattern = re.compile(r'\((?:[A-Z][a-zA-Z\-]+(?:,? (?:[A-Z][a-zA-Z\-]+|and [A-Z][a-zA-Z\-]+|et al\.)?)*,? \d{4}(?:[a-z])?)(?:; [A-Z][a-zA-Z\-]+(?:,? (?:[A-Z][a-zA-Z\-]+|and [A-Z][a-zA-Z\-]+|et al\.)?)*,? \d{4}(?:[a-z])?)*\)', re.UNICODE)

# Find all matches
matches = ref_pattern.findall(main_text)

# Flatten and clean up references (handle multiple refs in one parenthesis)
cited_refs = set()
for match in matches:
    for part in match[1:-1].split(';'):
        cited_refs.add(part.strip())

# Parse the reference section into a dict: key = normalized author-year, value = full reference (multi-line aware)
def normalize_key(key):
    # Remove extra spaces, unify 'et al.' and 'and', lower case, etc.
    key = re.sub(r'\s+', ' ', key)
    key = key.replace('et al.', 'et al.')
    key = key.replace(' and ', ' and ')
    key = key.replace(',', ',')
    return key.strip().lower()

ref_lines = [line.rstrip() for line in ref_section.split('\n')]
ref_dict = OrderedDict()
current_key = None
current_ref = []
for line in ref_lines:
    if not line.strip():
        continue
    # Try to extract author-year key
    m = re.match(r'([A-Z][^\(\d]+\d{4}[a-z]?)', line)
    if m:
        # Save previous reference
        if current_key and current_ref:
            ref_dict[normalize_key(current_key)] = ' '.join(current_ref)
        key = m.group(1)
        key_match = re.match(r'([A-Z][a-zA-Z\-]+(?:,? (?:[A-Z][a-zA-Z\-]+|and [A-Z][a-zA-Z\-]+|et al\.)?)*),? (\d{4}[a-z]?)', key)
        if key_match:
            author_part = key_match.group(1)
            year_part = key_match.group(2)
            current_key = f"{author_part}, {year_part}"
        else:
            current_key = key
        current_ref = [line.strip()]
    else:
        # Continuation of previous reference
        if current_key:
            current_ref.append(line.strip())
# Save last reference
if current_key and current_ref:
    ref_dict[normalize_key(current_key)] = ' '.join(current_ref)

# Try to match cited refs to normalized keys
used_refs = []
not_found = []
for k in sorted(cited_refs):
    norm_k = normalize_key(k)
    found = False
    for ref_key in ref_dict:
        # Allow for partial match if exact not found
        if norm_k == ref_key or norm_k in ref_key or ref_key in norm_k:
            used_refs.append(ref_dict[ref_key])
            found = True
            break
    if not found:
        not_found.append(k)

# Compose the new text
new_text = main_text.strip() + '\n\nReferences:\n' + '\n'.join(used_refs)
if not_found:
    new_text += '\n\n# The following references were cited in the text but not found in the reference list:\n'
    for nf in not_found:
        new_text += f'- {nf}\n'

# Save to new file
with open('full_text_with_refs.txt', 'w', encoding='utf-8') as f:
    f.write(new_text) 