# Run build, generate design document, and prepare assets for release.

rm -Rf rel && mkdir rel
bash make.sh
latexmk -pdf -silent -outdir=doc doc/design.tex > doc/latexmk.log

tar -czf rel/dat.tar.gz dat
tar -czf rel/tmp.tar.gz tmp
cp doc/design.pdf rel
