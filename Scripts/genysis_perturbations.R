# Place in genysis/ directory, run as 'Rscript genysis_perturbations.R <network-file-name>'
# Takes the network file name (minus extension) as a command-line argument. This file must be in genysis/networks/
networkFile = commandArgs(TRUE)

# check file exists
if (!file.exists(paste0("networks/", networkFile, ".net"))) {
  stop(paste0("networks/", networkFile, ".net ", "does not exist!"))
}

# run genysis on unperturbed network to get WT stable states, put the result in results/
system(paste0("./genYsis -p3 -f networks/", networkFile, ".net -o results/", networkFile))

# generate the experiments files for all genes, both KO and OE. put in experiments/
genes = as.vector(read.table(paste0("results/", networkFile, "_1"), skip=1)[, 1])

for (g in genes) {
  sink(paste0("experiments/", networkFile, "_", g, "KO.net"))
  cat("1\n")
  cat("1 0 0\n")
  cat(g)
  cat("\n")
  sink()

  sink(paste0("experiments/", networkFile, "_", g, "OE.net"))
  cat("1\n")
  cat("0 1 0\n")
  cat(g)
  cat("\n")
  sink()
}

# run all perturbations, put results in results/networkGeneOE_i.txt
for (g in genes) {
  for (perturbation in c("KO", "OE")) {
    system(paste0("./genYsis -p3 -f networks/", networkFile, ".net -e experiments/", networkFile, "_", g, perturbation, ".net"))
    output.files = paste0("results/", list.files("results/", paste0("^", networkFile, "_SS_1_\\d+.txt")))
    for (i in 1:length(output.files)) {
      file.rename(output.files[i], paste0("results/", networkFile, g, perturbation, "_", i, ".txt"))
    }
  }
}

# build the output csv
wt.files = paste0("results/", list.files("results/", paste0("^", networkFile, "_\\d+")))
load.state = function (filename) {
  state = as.matrix(read.table(filename, skip=1, header=F, row.names=1))
  if (ncol(state) > 1) {
    stop("Cannot handle loop attractors")
  }
  state
}

wt.states = do.call(cbind, lapply(wt.files, load.state))

cols.equal = function(col1, col2) {
  sum(col1 != col2) == 0
}

wt.state.retained = function(perturbation.states) {
  function (wt.state) {
    for (i in 1:ncol(perturbation.states)) {
      if (cols.equal(wt.state, perturbation.states[,i])) {
        return(TRUE)
      }
    }

    FALSE
  }
}

M <- rbind(c("Gene", "Perturbation", paste0("WT-S", 1:ncol(wt.states)), "Number of non-WT states introduced"))
for (g in genes) {
  for (perturbation in c("KO", "OE")) {
    perturbation.files = paste0("results/", list.files("results/", paste0("^", networkFile, g, perturbation, "_\\d+.txt")))
    perturbation.states = do.call(cbind, lapply(perturbation.files, load.state))
    retained.wt.states = apply(wt.states, 2, wt.state.retained(perturbation.states))
    num.non.wt = ncol(perturbation.states) - sum(retained.wt.states)
    M <- rbind(M, c(g, perturbation, retained.wt.states, num.non.wt))
  }
}
write.table(M, paste(networkFile, "perturbations.csv"), row.names=F, col.names=F, sep=",")
