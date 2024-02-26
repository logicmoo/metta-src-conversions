animal(goat).
animal(elephant).
animal(X) :-
	write('please type one more animal name: '),
	read(X).
