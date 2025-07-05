import os
import pandas as pd
import re

# Directory containing the CSV files (change if needed)
directory = os.path.dirname("/home/personale/Documenti/POLIMI/Statistica/analisi_scommesse_sportive/dati/")

# Pattern to match files like quote_2024_2025.csv
pattern = re.compile(r"quote_(\d{4})_\d{4}\.csv")

merged_df = pd.DataFrame()

for filename in os.listdir(directory):
    print(filename)
    match = pattern.match(filename)
    if match:
        year = int(match.group(1))
        filepath = os.path.join(directory, filename)
        df = pd.read_csv(filepath)
        df['anno'] = year
        merged_df = pd.concat([merged_df, df], ignore_index=True)

# Save merged file
merged_df.to_csv('quote.csv', index=False)