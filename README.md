# GSI-ABM

**Agent-Based Model for Green Stormwater Infrastructure Adoption**

This repository contains the code and data used to reproduce the agent-based model (ABM) and analyses presented in the **Nature Cities** notebook.

## Requirements
* Python ≥ 3.7  
* Jupyter Notebook / JupyterLab  
* Recommended packages:  
  `pip install mesa pandas geopandas shapely fiona matplotlib numpy networkx`

## Getting started
1. **Clone the repo**  
       git clone https://github.com/Koazizi/GSI-ABM.git
       cd GSI-ABM
2. **(Optional) create & activate a conda env**  
       conda create -n gsi-abm python=3.8
       conda activate gsi-abm
3. **Install dependencies**  
       pip install -r requirements.txt
4. **Launch the notebook**  
       jupyter notebook Nature_Cities.ipynb  
   (Follow the cells to load spatial data, initialize the ABM, run the simulation, and generate figures.)

## Data
All spatial input data live in **`shapefils/`**. Ensure the filenames match exactly what `Nature_Cities.ipynb` expects.

## Citation
If you use or adapt this code, please cite:  
Azizi, K., et al. (2025). *Coupled Human–Infrastructure Systems: Behavioral and Spatial Dynamics in Green Stormwater Infrastructure Adoption*. **Nature Cities**.

## Author
Dr. Koorosh Azizi — Postdoctoral Fellow, University of Texas at Austin (<https://github.com/Koazizi>)
