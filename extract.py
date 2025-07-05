import csv
import re

def is_date(line):
    return bool(re.match(r"\d{1,2} [A-Za-z]{3,} \d{4}", line))

def is_time(line):
    return bool(re.match(r"\d{1,2}:\d{2}", line))

def is_score(line):
    return "–" in line or "-" in line

def is_odd(line):
    try:
        float(line.replace(",", "."))
        return True
    except ValueError:
        return False
anno = "2015_2016"
with open(anno+".txt", encoding="utf-8") as f:
    lines = [l.strip() for l in f if l.strip()]

rows = []
i = 0
while i < len(lines):
    if is_date(lines[i]):
        date = lines[i]
        i += 1
        # Process all matches for this date
        while i < len(lines) and not is_date(lines[i]):
            # Skip lines until we find time
            while i < len(lines) and not is_time(lines[i]) and not is_date(lines[i]):
                i += 1
            if i >= len(lines) or is_date(lines[i]):
                break
            time = lines[i] if is_time(lines[i]) else ""
            i += 1

            # Home team (skip empty lines)
            while i < len(lines) and not lines[i]:
                i += 1
            home = lines[i] if i < len(lines) else ""
            i += 1
            # Skip duplicate home team line if present
            if i < len(lines) and lines[i] == home:
                i += 1

            # Home goals
            home_goals = ""
            away_goals = ""
            if i < len(lines) and (lines[i].isdigit() or lines[i] == "0"):
                home_goals = lines[i]
                i += 1
            # Dash
            if i < len(lines) and (lines[i] == "–" or lines[i] == "-"):
                i += 1
            # Away goals
            if i < len(lines) and (lines[i].isdigit() or lines[i] == "0"):
                away_goals = lines[i]
                i += 1

            # Away team
            away = lines[i] if i < len(lines) else ""
            i += 1
            # Skip duplicate away team line if present
            if i < len(lines) and lines[i] == away:
                i += 1

            # Odds: next 3 lines
            odds = []
            for _ in range(3):
                if i < len(lines) and is_odd(lines[i]):
                    odds.append(lines[i].replace(",", "."))
                    i += 1
                else:
                    odds.append("")

            # Save row only if teams are present
            if home and away:
                outcome = '1' if home_goals > away_goals else '2' if away_goals > home_goals else 'X'
                rows.append([home, away, outcome, *odds])
    else:
        i += 1

with open("quote_" + anno + ".csv", "w", newline="", encoding="utf-8") as f:
    writer = csv.writer(f)
    writer.writerow(["Home", "Away", "Outcome", "Odd_1", "Odd_X", "Odd_2"])
    writer.writerows(rows)

print("Estrazione completata. Dati salvati in quote.csv")