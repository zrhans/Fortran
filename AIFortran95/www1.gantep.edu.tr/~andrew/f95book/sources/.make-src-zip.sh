#!/bin/sh

rm -r zipped/
mkdir zipped/

zip -r f90.zip f90/
zip -r f95.zip f95/
tar czf f90.tgz f90/
tar czf f95.tgz f95/

mv f90.zip zipped/
mv f95.zip zipped/
mv f90.tgz zipped/
mv f95.tgz zipped/

echo -n "Archives last updated " >> zipped/README.TXT
date >> zipped/README.TXT
echo "" >> zipped/README.TXT
echo "Fortran program sources compressed to archives:" >> zipped/README.TXT
echo "" >> zipped/README.TXT
echo "  f90.tgz   Program sources with .f90 extension (.tgz format)" >> zipped/README.TXT
echo "  f90.zip   Program sources with .f90 extension (.zip format)" >> zipped/README.TXT
echo "" >> zipped/README.TXT
echo "  f95.tgz   Program sources with .f95 extension (.tgz format)" >> zipped/README.TXT
echo "  f95.zip   Program sources with .f95 extension (.zip format)" >> zipped/README.TXT
echo "" >> zipped/README.TXT

