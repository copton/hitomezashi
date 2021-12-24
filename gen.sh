#! /bin/sh

rm -fr out
mkdir out
for i in `seq 0 100`; do
  echo $i
  stack run -- -x 100 -y 100 -s $i > out/out.svg
  qlmanage -t -s 10000 -o out out/out.svg
  mv out/out.svg.png out/$i.png
  rm out/out.svg
done
