"""
Count the number of citations within a given time window of publication.

Time windows include: 1/2 year, 1 year, 2 years, 3 years, 5 years.
"""


from pathlib import Path
import pandas as pd
import json
import numpy as np
from datetime import timedelta


def safe_nan(x):
    if isinstance(x, str):
        return False
    else:
        return np.isnan(x)


def get_citations_within_window(pub_date, citation, window):
    cites = pd.DataFrame([x["citingPaper"] for x in citation["citations"]])
    if len(cites) == 0:
        return 0

    cite_dates = pd.to_datetime(cites["publicationDate"].dropna())
    diff = cite_dates - pub_date

    return np.sum(diff <= window)


def get_citations(df, citations, window, include_missed=False):
    today = pd.to_datetime("2023-06-14")

    # Create citation dict keyed by paper.
    res = []
    for (_, input_row), citation in zip(df.iterrows(), citations):
        row = input_row.copy()

        # Skip if there's no title.
        if safe_nan(row["title"]) and safe_nan(citation["title"]):
            continue

        if row["title"] != citation["title"]:
            raise ValueError("Ordering is wrong.")

        # Either skip rows with missing s2 ID's, or set the number of cites to 0.
        if row["s2_id"] == -1:
            if not include_missed:
                continue
            else:
                row["cites_within_window"] = 0
                res.append(row)
                continue

        # Skip rows with a missing publication date (excluding those with missing ID's).
        elif safe_nan(row["publication_date"]):
            continue

        # Otherwise, we have the publication date and we have the ID.
        else:
            # Skip the rows where the query failed.
            if citation["status"] == "Failed query":
                continue
            if citation["status"] != "Success":
                raise Exception("Unexpected status.")

            publication_date = pd.to_datetime(row["publication_date"])
            # If the paper was published inside the time window, skip it.
            if today - publication_date < window:
                continue

            row["cites_within_window"] = get_citations_within_window(
                publication_date, citation, window
            )
            res.append(row)

    res = pd.DataFrame(res)
    return res


def main():
    data_dir = Path("/net/nfs.cirrascale/allennlp/davidw/proj/end-of-anonymity/data")
    # Submitted papers
    df = pd.read_csv(data_dir / "baby_iclr_ids_and_dates.csv")

    # Citations for submitted papers.
    citations = [
        json.loads(line) for line in open(data_dir / "baby_iclr_citations.jsonl")
    ]

    # Loop over settings.
    windows = [round(365 * years) for years in (0.5, 1, 2, 3, 5)]
    for window in windows:
        window_str = window
        window = timedelta(days=window)
        for include_missed in [False, True]:
            print((window, include_missed))
            res = get_citations(df, citations, window, include_missed)
            fname = f"results/citations_within_{window_str}_include_no_s2orc_{include_missed}.tsv".lower()
            res.to_csv(fname, sep="\t", index=False)


if __name__ == "__main__":
    main()
