import pandas as pd
from pathlib import Path

# Step 1: Set folder containing CSV files
csv_folder = Path(
    r"C:\Users\walke\Documents\umich\courses\SI544\final_proj\tornado_analysis\csv_files"
)

# Step 2: Find all CSV files in the folder
csv_files = list(csv_folder.glob("*.csv"))

if not csv_files:
    print("No CSV files found in the folder! Check your path.")
    exit()

print(f"Found {len(csv_files)} CSV files:")
for f in csv_files:
    print(f" - {f.name}")

# Step 3: Read each CSV with robust parsing
dfs = []
for file in csv_files:
    try:
        df = pd.read_csv(file, dtype=str, on_bad_lines="warn", encoding="ISO-8859-1")
        dfs.append(df)
        print(f"Loaded {file.name} with {len(df)} rows.")
    except Exception as e:
        print(f"Failed to read {file.name}: {e}")

# Step 4: Concatenate all DataFrames
if dfs:
    mega_df = pd.concat(dfs, ignore_index=True)
    print(
        f"Combined mega DataFrame has {len(mega_df)} rows and {len(mega_df.columns)} columns."
    )

    # Step 5: Save the mega CSV
    mega_csv_path = Path(
        r"C:\Users\walke\Documents\umich\courses\SI544\final_proj\tornado_analysis\merged\mega_tornado_data.csv"
    )
    mega_df.to_csv(mega_csv_path, index=False)
    print(f"Mega CSV saved to {mega_csv_path}")
else:
    print("No dataframes to concatenate. Check the CSV files for issues.")

print(mega_df.columns.tolist())

# List the columns you want to keep (exact names from the CSV)
columns_to_keep = [
    "EVENT_ID",
    "BEGIN_DATE",
    "END_DATE",
    "CZ_NAME_STR",
    "EVENT_TYPE",
    "TOR_LENGTH",
    "TOR_WIDTH",
    "TOR_F_SCALE",
    "DEATHS_DIRECT",
    "INJURIES_DIRECT",
    "DAMAGE_PROPERTY_NUM",
    "DAMAGE_CROPS_NUM",
    "BEGIN_LAT",
    "BEGIN_LON",
    "END_LAT",
    "END_LON"
]


# Keep only these columns
mega_df = mega_df[columns_to_keep]

# Save the filtered CSV
mega_df.to_csv(
    r"C:\Users\walke\Documents\umich\courses\SI544\final_proj\tornado_analysis\merged\mega_tornado_filtered.csv",
    index=False,
)
