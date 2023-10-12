# Estimating the Causal Effect of Early ArXiving on Paper Acceptance

<img align="right" src="img/graph.png" width=400px>

This repository contains code and data for the paper [Estimating the Causal Effect of Early ArXiving on Paper Acceptance](https://arxiv.org/abs/2306.13891) by Yanai Elazar*, Jiayao Zhang*, David Wadden*, Bo Zhang, Noah A. Smith.

## Setup

### `Python` Environment

We use `python` for the majority of our anlaysis, which can be set up as
follows.
```bash
# Set up conda env.
conda create --name anon python=3.10
conda activate anon
conda install conda-build
conda install r-essentials r-base

# Clone this repo and install.
git clone https://github.com/allenai/anonymity-period.git
cd anonymity-period
pip install -r requirements.txt
conda develop .
```

### `R` Environment
We use `R` for statistical matching, which is used in [`03_s2_data_analysis.ipynb`](./notebooks/03_s2_data_analysis.ipynb).
This is optional, since the matched data is provided as part of the processed
data documented in the following sections. To set up `R` kernel in `Jupyter`,
one may refer to the [`IRKernel` Package](https://github.com/IRkernel/IRkernel).


## Data Acquisition and Preprocessing

### ICLR Data
ICLR data can be obtained from [ICLR Database](https://cogcomp.github.io/iclr_database/).

### Semantic Scholar Citation Data
We use Semantic Scholar to obtain citation data. To get the citation data, run `bash script/get_s2_citation_data.sh`. This will output:
- A file `data/baby_iclr_citations.jsonl`, which is a version of the `baby_iclr` dataset, with citation information on each paper.
- Files `results/citations_within_{time_window}_include_no_s2orc_{include_missed_papers}.tsv`: These files count the number of citations for each paper in `baby_iclr`, at `time_window` years after each paper was released. If `include_missed_papers` is `true`, it sets the citation count for papers not found in Semantic Scholar to 0; otherwise they're left out entirely.
	- **NOTE** The `time_window` indicates the time elapsed after each individual paper was published, *not* the calendar year.


### (Optional) Processed Data
We provide a few processed dataframes for making analyses easier, these are provided within `./data`. For instructions on generating these
dataframe, see next section.

- [`baby_iclr`](./data/baby_iclr.csv): this contains minimal columns we used from the ICLR database. This is used for the scripts under `./script`
to get the correpsonding S2 data for the ICLR submissions.
- [`c_n`](./data/s2_citation/): these are S2 citation data, where the numeral `n` denotes an `n`-day window for counting citations.
- [`specter_embedding`](./data/submission_cluster_20.csv): this contains 20 topic clusters obtained from running spectral clustering on the embedding from the Specter model.
- [`design_mat`](./data/design_mat.csv): this is the aggregated dataframe with unused columns removed, as the  "design matrix" in regression analysis.
- [`fb_mathced_design_mat_ordered`](./data/fb_matched_design_mat_ordered.csv): this is the matched treated/control group with fine-balance on topic clusters.
- [Bootstrap samples](./data/bootstrap/): These are used for estimating ATET and confidence intervals.

## Notebooks

This study is conducted in the notebook in the order below. The notebooks assumes the root directory as the
current working directory. It may be convenient to symlink the notebooks via `ln -s ./notebook/*.ipynb ./`
and run them from the root directory for ease of module and data imports.


1. Prepare ICLR Data ([`01_iclr_data.ipynb`](./notebooks/01_iclr_data.ipynb)): load data from the ICLR database, generate dataframes `baby_iclr` and `design_mat`
for subsquent analysis.
2. Statistical Matching ([`02_matching.ipynb`](./notebooks/02_matching.ipynb)): uses `design_mat` and performs statistical matching with fine-balance on topic clusters. Generates `fb_mathced_design_mat_ordered` for subsequent analysis.
3. Obtain S2 citation data ([`03_s2_data_analysis.ipynb`](./notebooks/03_s2_data_analysis.ipynb)): uses `baby_iclr` and scripts under `./script/` to obtain S2 citation data. Generates `c_n` citation data for an `n`-day window.
4. Primary and NOC Analysis ([`04_effect_analysis.ipynb`](./notebooks/04_effect_analysis.ipynb)): uses `design_mat` and `c_n` to perform primary and NOC anlaysis.
5. Ploting results ([`05_plots.ipynb`](./notebooks/05_plots.ipynb)): plots the results in the primary and NOC analysis.

*Note*: for reproducing all tables and figures, one can use processed data and only
   notebooks 04 and 05 are needed.

# Reference
```bib
@inproceedings{EZW+23,
	Author = {Elazar*, Yanai and Zhang*, Jiayao and Wadden*, David and Zhang, Bo and Smith, Noah A.~},
	booktitle = {Technical Report},
	Title = {Estimating the Causal Effect of Early ArXiving on Paper Acceptance},
	Year = {2023},
	url = {https://arxiv.org/abs/2306.13891},
}
```
