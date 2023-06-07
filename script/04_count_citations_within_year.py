"""
Count the number of citations within a year of publication, for each data in ICLR.
"""


from pathlib import Path
import pandas as pd
import json
import numpy as np
from datetime import timedelta
from tqdm import tqdm


def get_citations_within_window(row, citation_dict, window=365):
    pub_date = row["publication_date"]
    cites = pd.DataFrame(
        [x["citingPaper"] for x in citation_dict[row["s2_id"]]["citations"]]
    )
    if len(cites) == 0:
        return 0

    cite_dates = pd.to_datetime(cites["publicationDate"].dropna())
    diff = cite_dates - pub_date
    delta = timedelta(days=window)

    return np.sum(diff <= delta)


data_dir = Path("/net/nfs.cirrascale/allennlp/davidw/proj/end-of-anonymity/data")

# Submitted papers
df = pd.read_csv(data_dir / "baby_iclr_ids_and_dates.csv")

# Citations for submitted papers.
citations = [json.loads(line) for line in open(data_dir / "baby_iclr_citations.jsonl")]

# Create citation dict keyed by paper.
citation_dict = {}
for entry in citations:
    if entry["status"] != "Success":
        continue
    else:
        citation_dict[entry["s2_id"]] = entry

# Get rid of papers where we couldn't get a publication date.
df = df.dropna(subset=["s2_id", "publication_date"])
df["publication_date"] = pd.to_datetime(df["publication_date"])


# Get citations within a year of publication.
res = []
for _, row in tqdm(df.iterrows(), total=len(df)):
    s2_id = row["s2_id"]
    if s2_id not in citation_dict:
        continue

    row["cites_within_year"] = get_citations_within_window(
        row, citation_dict, window=365
    )
    res.append(row)

res = pd.DataFrame(res)
res.to_csv("results/citations_within_year.csv", index=False)
