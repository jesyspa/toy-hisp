./m > .graphs.sh
rm -rf pics
mkdir pics
cd pics
. ../.graphs.sh
for a in *.dot; do
    dot -Tpng $a > ${a%.dot}.png
done
