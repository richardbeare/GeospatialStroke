# Geospatial analysis with Python

## Setup

To reproduce the analytical environment, the easiest method is with conda + conda-forge.

First, install the latest version of [miniconda](https://docs.conda.io/en/latest/miniconda.html).

Next, create/activate the virtual environment by opening a terminal (Mac/Linux) or command prompt (Windows), changing directories to this `python` folder, then running these commands:

```
conda config --prepend channels conda-forge
conda create -n GEO --strict-channel-priority --yes python=3 --file requirements.txt
conda activate GEO
python -m ipykernel install --user --name GEO --display-name "Python (GEO)"
```

To run the notebooks, change directories in your terminal/command prompt to the location of the notebook files, then run the command: `jupyter lab`.
