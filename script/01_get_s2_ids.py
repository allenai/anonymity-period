import pandas as pd
import requests
from tqdm import tqdm


df = pd.read_csv("data/baby_iclr.csv")

N = 10
df["block"] = df.index // N

s2_ids = []
n_groups = df["block"].max()
for block, group in tqdm(df.groupby("block"), total=n_groups):
    titles = group["title"]
    query_json = [{"title": entry} for entry in titles]

    try:
        response = requests.post(
            "http://pipeline-api.prod.s2.allenai.org/citation/match",
            json=query_json,
        )
        result = response.json()
    except Exception:
        print("Query failed.")
        result = [-1] * len(query_json)

    s2_ids.extend(result)

df["s2_id"] = s2_ids
del df["block"]

df.to_csv("data/baby_iclr_with_s2_ids.csv", index=False)
