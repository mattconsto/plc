echo -en "P1 : "
./mysplinterpreter pr1.spl < pr1.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP2 : "
./mysplinterpreter pr2.spl < pr2.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP3 : "
./mysplinterpreter pr3.spl < pr3.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP4 : "
./mysplinterpreter pr4.spl < pr4.txt | tr '\n' ', ' | sed 's/.$//'

echo -en "\nP5 : "
./mysplinterpreter pr5.spl < pr5.txt | tr '\n' ', ' | sed 's/.$//'
