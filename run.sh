for i in `seq 1 10`; do
  echo -en "\nP$i : "
  ./mysplinterpreter code/pr$i.spl < code/inputs/pr$i.txt | tr '\n' ', ' | sed 's/.$//'
done
