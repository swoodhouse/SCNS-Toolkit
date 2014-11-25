Single Cell Network Synthesis Toolkit
============

### Synthesis Engine

### Scripts

Two scripts are provided in the toolkit, for converting single cell gene expression data into a format that can be handled by the synthesis engine, and for subsequent analysis of synthesised Boolean networks.

### constructSTG.fsx

#### genysis_perturbations.R
R script which automates the process of running GenYsis (http://lsisrv5.epfl.ch/lsi/~garg/genysis_v2.html) on a model and performing all single-gene perturbations. The perturbed models are compared to the wild-type model in terms of alterations to the stable states that the model is able to reach.

Both a failure to reach states normally reachable for the wild-type model, as well as stabilisation at novel "unnatural" states can be important, with the former mimicking for example the failure of a cell to develop down a given lineage, while the latter could be used to gain mechanistic understanding of pathological cellular states (such as in cancer cells). A summary of these results are collated into a CSV file.

### License

The SCNS Toolkit is released under the Apache 2.0 license; see LICENSE for more details.

However, note that use of Z3 (which is used as the satisfiability solver) is bound by the Microsoft Research License Agreement (http://z3.codeplex.com/license) and is for Non-Commercial Use Only.
