# Single Cell Network Synthesis Toolkit

[The SCNS Toolkit](http://scns.stemcells.cam.ac.uk/) is a set of tools for the synthesis of Boolean gene regulatory networks from single cell gene expression experiments.  It was originally designed for single cell qPCR data but can also be used with RNA-seq. The toolkit produces binary gene expression values from measured data, which can be viewed as the state space of an asynchronous Boolean network. A synthesis algorithm is then used to identify the underlying Boolean logic between genes, from which networks can be built.  Network stable state analysis and in silico perturbations can be carried out to generate hypotheses about gene regulation and function.

## Synthesis Engine

The synthesis engine is written in F# and uses the Z3 theorem prover. It compiles and runs on Windows and Linux with F# 3.1 and .NET 4.5 or Mono 3.10.

To build on Windows with Visual Studio 2013, open SynthesisEngine.sln and then select `Build -> Build Solution`. You may be prompted to give permissions to FSharp.Data type providers.

After compiling, the following will run the synthesis engine on the provided example:
```
SynthesisEngine.exe cmpStates.csv cmpEdges.csv cmpParameters.csv cmp_initial_states.txt cmp_target_states.txt cmp_all_states.txt <output_directory>
```

## Scripts
Two scripts are provided in the toolkit, for converting single cell gene expression data into a format that can be handled by the synthesis engine, and for subsequent analysis of synthesised Boolean networks.

### constructSTG.fsx
F# script which discretises a CSV file containing single cell gene expression data to binary expression values, and then constructs a state transition graph. The input CSV file must have genes as column names and unique cell identifiers as row names.

The script produces CSV files for input to the synthesis engine and a SIF file for visualisation in Cytoscape (or a similar tool).

To run the script on input.csv with a discretisation threshold of 25 (all expression values greater than or equal to 25 will be considered unexpressed, all other values will be considered expressed) and output files of outputStates.csv, outputEdges.csv and output.sif:

```
fsi.exe --exec constructSTG.fsx -- input.csv 25 outputStates.csv outputEdges.csv output.sif
```

On Windows, `fsi.exe` can be found in `C:\Program Files (x86)\Microsoft SDKs\F#\<version>\Framework\<version>\` after installing F#.

### genysis_perturbations.R
R script for Linux which automates the process of running GenYsis (http://lsisrv5.epfl.ch/lsi/~garg/genysis_v2.html) on a model and performing all single-gene perturbations. The perturbed models are then compared to the wild-type model in terms of alterations to the stable states that the model is able to reach.

Both a failure to reach states normally reachable for the wild-type model, as well as stabilisation at novel "unnatural" states can be important, with the former mimicking for example the failure of a cell to develop down a given lineage, while the latter could be used to gain mechanistic understanding of pathological cellular states (such as in cancer cells). A summary of these results are collated into a CSV file.

To run the script on `combined_embryo_model.net`, place `genysis_perturbations.R` in the `genysis/` directory and `combined_embryo_model.net` in `genysis/networks/`. Then use the command:
```
Rscript genysis_perturbations.R combined_embryo_model
```

R can be downloaded from [http://www.r-project.org/](http://www.r-project.org/).

## License
The SCNS Toolkit is released under the Apache 2.0 license; see LICENSE for more details.

However, note that use of Z3 (which is used as the satisfiability solver in the synthesis engine) is bound by the Microsoft Research License Agreement (http://z3.codeplex.com/license) and is for Non-Commercial Use Only.
