Single Cell Network Synthesis Toolkit
============

## Synthesis Engine

## Scripts
Two scripts are provided in the toolkit, for converting single cell gene expression data into a format that can be handled by the synthesis engine, and for subsequent analysis of synthesised Boolean networks.

### ConstructSTG.fsx
F# script which discretises a CSV file containing single cell gene expression data to binary expression values, and then constructs a state transition graph.

The script produces CSV files for input to the synthesis engine and a SIF file for visualisation in Cytoscape (or a similar tool).

### genysis_perturbations.R
R script for Linux which automates the process of running GenYsis (http://lsisrv5.epfl.ch/lsi/~garg/genysis_v2.html) on a model and performing all single-gene perturbations. The perturbed models are then compared to the wild-type model in terms of alterations to the stable states that the model is able to reach.

Both a failure to reach states normally reachable for the wild-type model, as well as stabilisation at novel "unnatural" states can be important, with the former mimicking for example the failure of a cell to develop down a given lineage, while the latter could be used to gain mechanistic understanding of pathological cellular states (such as in cancer cells). A summary of these results are collated into a CSV file.

To run the script on combined_embryo_model.net, place genysis_perturbations.R in the genysis/ directory and combined_embryo_model.net in genysis/networks. Then use the command:
```
Rscript genysis_perturbations.R combined_embryo_model
```

## License
The SCNS Toolkit is released under the Apache 2.0 license; see LICENSE for more details.

However, note that use of Z3 (which is used as the satisfiability solver in the synthesis engine) is bound by the Microsoft Research License Agreement (http://z3.codeplex.com/license) and is for Non-Commercial Use Only.
