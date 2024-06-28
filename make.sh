# Download data, create tmp directory for storing results and run all
# statistical inference models.

SRC='https://github.com/arcadio/nk-models/releases/latest/download/dat.tar.gz'

curl -L $SRC | tar -xzf -
mkdir -p tmp

Rscript hypothesis.r
Rscript betareg.r
Rscript betabay.r
