cd output
for name in *.dot; do
    dot -Tpng $name > ${name%.dot}.png
done
