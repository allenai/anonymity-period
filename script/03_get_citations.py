"""
Get info on all the citing papers.
"""

import pandas as pd
import requests
import os
from tqdm import tqdm
import json

# Requires an S2 API key.
API_KEY = os.environ["S2_API_KEY"]


def get_all_citations(s2_id):
    """
    Get all citations for a given paper.
    """
    url = f"https://api.semanticscholar.org/graph/v1/paper/CorpusId:{s2_id}/citations"
    params = {"fields": "paperId,corpusId,title,year,publicationDate", "offset": 0}
    headers = {"x-api-key": API_KEY}

    cited_papers = []
    while True:
        response = requests.get(url, params=params, headers=headers)
        result = response.json()
        cited_papers.extend(result["data"])
        # If there's more results, get the next set.
        if "next" in result:
            params["offset"] = result["next"]
        # Otherwise, return.
        else:
            return cited_papers


# Get citations for all papers.
df = pd.read_csv("data/baby_iclr_with_s2_ids.csv")

res = []

for _, row in tqdm(df.iterrows(), total=len(df)):
    to_append = {"title": row["title"], "s2_id": row["s2_id"]}
    if row["s2_id"] == -1:
        to_append["status"] = "No ID"
        to_append["citations"] = None
    else:
        try:
            citations = get_all_citations(row["s2_id"])
        except Exception:
            to_append["status"] = "Failed query"
            to_append["citations"] = None
        else:
            to_append["status"] = "Success"
            to_append["citations"] = citations

    res.append(to_append)


# Dump to file.
fname = "data/baby_iclr_citations.jsonl"
with open(fname, "w") as f:
    for entry in res:
        print(json.dumps(entry), file=f)
