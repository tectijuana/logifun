# Define the Erlang compiler and interpreter commands
ERLC = erlc
ERL = erl

# Define the default task that will run both compile and run tasks
all: compile run

# Compile the Erlang source file into a .beam file
compile:
	$(ERLC) hola_mundo.erl

# Run the compiled Erlang program
run:
	$(ERL) -noshell -s hola_mundo saludar -s init stop

# Clean up: remove all .beam files
clean:
	rm -f *.beam

