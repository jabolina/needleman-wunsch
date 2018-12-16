# Needleman-Wunsch
The Needleman-Wunsch assignment for bioinformatics class.

### Usage
Entering the Erlang runtime system, ```erl```, compile the ```alignment```:

    c(alignment).

You have two option, enter the nucleotides in the command line, or by file.

    alignment:compare_input().

Entering the sequences when asked.

    alignment:compare_file("path/to/file.txt").

If everything turns to be ok, the output will be a tuple, with the aligned sequences and the score:

    {"-ga-ac","caagac",0.0}

