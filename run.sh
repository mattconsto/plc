echo -en "P1 : "
./mysplinterpreter code/pr1.spl < code/pr1.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP2 : "
./mysplinterpreter code/pr2.spl < code/pr2.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP3 : "
./mysplinterpreter code/pr3.spl < code/pr3.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP4 : "
./mysplinterpreter code/pr4.spl < code/pr4.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP5 : "
./mysplinterpreter code/pr5.spl < code/pr5.txt | tr '\n' ', ' | sed 's/.$//'
