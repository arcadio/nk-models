# Create tmp directory for storing results and run all models.

mkdir -p tmp

Rscript example.r
Rscript betareg.r
Rscript betabay.r
