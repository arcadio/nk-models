# Create tmp directory for storing results and run all models.

mkdir -p tmp

Rscript hypothesis.r
Rscript betareg.r
Rscript betabay.r
