echo -en "P1 : "
./mysplinterpreter code/pr1.spl < code/inputs/pr1.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP2 : "
./mysplinterpreter code/pr2.spl < code/inputs/pr2.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP3 : "
./mysplinterpreter code/pr3.spl < code/inputs/pr3.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP4 : "
./mysplinterpreter code/pr4.spl < code/inputs/pr4.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP5 : "
./mysplinterpreter code/pr5.spl < code/inputs/pr5.txt | tr '\n' ', ' | sed 's/.$//'
