# Estimating the Causal Effect of Early ArXiving on Paper Acceptance

<img align="right" src="img/graph.png" width=400px>

This repository contains code and data for the paper [Estimating the Causal Effect of Early ArXiving on Paper Acceptance](https://arxiv.org/abs/2306.13891) by Yanai Elazar, Jiayao Zhang, David Wadden, Bo Zhang, Noah A. Smith.


## Tasks to do before making public

- [ ] @jiayao explain how we get from the ICLR database to the `baby_iclr.csv` file. We should probably have a script to download the necessary data.
- [ ] @jiayao update `requirements.txt` with versions of packages needed to run analysis
- [ ] @dave explain how we get citation data, starting from `baby_iclr.csv`. As above, have scripts that do all the processing and put the data somewhere.
- [ ] @all Write scripts that put all data in a `data` folder, and set all paths in notebooks to point there. Right now, the notebooks won't be runnable because the paths won't point to the right place.


## Setup

```bash
# Set up conda env.
conda create --name anon python=3.10
conda activate anon

# Clone this repo and install.
git clone https://github.com/allenai/anonymity-period.git
cd anonymity-period
pip install -r requirements.txt
```


## Data Acquisition and preprocessing

### ICLR Data
ICLR data can be obtained from [ICLR Database](https://cogcomp.github.io/iclr_database/).

### Processed Data
Bootstrap samples for estimating ATET and confidence intervals can be found from
this [anonymous Dropbox link](https://www.dropbox.com/s/rdamix57aq9pzz5/processed_data.zip?dl=://www.dropbox.com/s/rdamix57aq9pzz5/processed_data.zip?dl=1).

## Notebooks

### S2 Citation Data
- `analysis.ipynb`

### ICLR Data Collection and Processing
- `iclr_data.ipynb`

### Tripartite Matching
- `matching.ipynb`

### Primary and NOC Analysis
- `effect_analysis.ipynb`

### Plots
- `plots.ipynb`
