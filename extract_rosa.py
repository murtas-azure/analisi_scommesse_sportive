import re
import csv
import sys
import os

if len(sys.argv) < 2:
    print("Usage: python extract_rosa.py <anno>")
    sys.exit(1)

anno = sys.argv[1]
input_file = f"raw/rose_{anno}.txt"
output_file = f"dati/rose_2024.csv"

with open(input_file, "r", encoding="utf-8") as f:
    testo = f.read()

rows = []

for linea in testo.strip().split("\n"):
    colonne = re.split(r'\t+', linea)
    if len(colonne) < 5:
        colonne = re.split(r'\s{2,}', linea)
    squadra = colonne[1]
    # Extract the last number in the row as valore
    numeri = re.findall(r'([\d,.]+)', linea)
    if numeri:
        numero = numeri[-1].replace('.', '').replace(',', '.')
        valore = float(numero)
        rows.append([squadra, valore, anno])

header = ["squadra", "valore_rosa", "anno"]
write_header = not os.path.exists(output_file)

with open(output_file, "a", newline='', encoding="utf-8") as csvfile:
    writer = csv.writer(csvfile)
    if write_header:
        writer.writerow(header)
    writer.writerows(rows)
