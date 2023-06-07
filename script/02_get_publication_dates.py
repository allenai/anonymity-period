"""
Get publication dates of all baby_iclr papers.
"""

import pandas as pd
import requests
from tqdm import tqdm
import os

API_KEY = os.environ["S2_API_KEY"]


def cleanup_response(s2_ids, response_json):
    "Very occasionally, a response is None; need to clean these up."
    if len(s2_ids) != len(response_json):
        raise ValueError("Lengths don't match.")

    res = []

    for s2_id, entry in zip(s2_ids, response_json):
        if entry is None:
            print(f"None entry for {s2_id}.")
            entry = {
                "paperId": None,
                "corpusId": s2_id,
                "year": None,
                "publicationDate": None,
            }
        res.append(entry)

    return res


def main():
    df = pd.read_csv("data/baby_iclr_with_s2_ids.csv")

    N = 100
    df["block"] = df.index // N

    headers = {"x-api-key": API_KEY}
    n_groups = df["block"].max()

    res = []

    for _, group in tqdm(df.groupby("block"), total=n_groups):
        s2_ids = group["s2_id"]
        ix = (s2_ids != -1).values
        s2_ids_query = s2_ids[ix].values
        params = {"fields": "corpusId,year,publicationDate"}
        json_query = {"ids": [f"CorpusId:{entry}" for entry in s2_ids_query]}

        response = requests.post(
            "https://api.semanticscholar.org/graph/v1/paper/batch",
            params=params,
            json=json_query,
            headers=headers,
        )
        response_json = cleanup_response(s2_ids_query, response.json())
        df_add = pd.DataFrame(response_json)

        new_group = group.copy()
        year_col = pd.Series([None] * len(group))
        year_col[ix] = df_add["year"].values
        date_col = pd.Series([None] * len(group))
        date_col[ix] = df_add["publicationDate"].values
        new_group["publication_year"] = year_col.values
        new_group["publication_date"] = date_col.values

        res.append(new_group)

    res = pd.concat(res)
    del res["block"]

    if not (res["submission_id"] == df["submission_id"]).all():
        raise Exception("Something went wrong.")

    res.to_csv("data/baby_iclr_ids_and_dates.csv", index=False)


if __name__ == "__main__":
    main()
