rm -rf pics
mkdir pics
cd pics
. ../.graph.sh
for a in *.dot; do
    dot -Tpng $a > ${a%.dot}.png
done
