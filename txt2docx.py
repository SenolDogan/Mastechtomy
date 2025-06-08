from docx import Document

# Read the processed text
with open('full_text_with_refs.txt', 'r', encoding='utf-8') as f:
    lines = f.read().split('\n')

# Create a new Word document
doc = Document()
for line in lines:
    doc.add_paragraph(line)

# Save the new document
output_path = 'Review_2025_with_refs.docx'
doc.save(output_path)
print(f'Saved: {output_path}') 