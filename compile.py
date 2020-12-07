from lexer import Lexer
from parser import Parser
import sys


if __name__ == "__main__":
	if len(sys.argv) == 1:
		print("Usage: python3 compile.py your_program.sui output_fname")
	
	elif len(sys.argv) == 3:
		fname = sys.argv[1]
		parser = Parser.from_file(fname)
		parser.parse()

		with open(sys.argv[2], "w") as f:
			f.write(parser.root.gen_code())
	else:
		print("Wrong number of arguments")



