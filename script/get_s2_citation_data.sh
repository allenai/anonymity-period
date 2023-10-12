# Usage: Run this script from the root of the repo with:
# bash script/get_s2_citation_data.sh

echo "Getting S2 ID's of all papers in baby ICLR."
python script/01_get_s2_ids.py

echo "Getting publication dates for all papers."
python script/02_get_publication_dates.py

echo "Getting citation information for all papers."
python script/03_get_citations.py

echo "Counting citations for all papers,"
python script/04_count_citations.py
