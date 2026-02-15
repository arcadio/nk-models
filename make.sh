# Download data, create tmp directory for storing results and run all
# statistical inference models.

SRC='https://github.com/arcadio/nk-models/releases/latest/download/dat.tar.gz'

curl -L $SRC | tar -xzf -
rm -Rf tmp && mkdir tmp

Rscript src/hypothesis.r > tmp/hypothesis.log
Rscript src/betareg.r > tmp/betareg.log
Rscript src/betabay.r > tmp/betabay.log
