from lexer import Lexer
from parser import Parser
from lexer import TokenType as T


lexer = Lexer("testprog.sui")
tokens = [tok for tok in lexer.tokenize()]
parser = Parser(tokens)

[print(imp) for imp in parser.imports()]

