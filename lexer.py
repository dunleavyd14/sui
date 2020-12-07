from enum import Enum, auto
import string
from colors import color

class TokenType(Enum):
	#literals
	INT_LITERAL = auto()
	BINARY_LITERAL = auto()
	HEX_LITERAL = auto()
	UINT_LITERAL = auto()
	FLOAT_LITERAL = auto()
	DOUBLE_LITERAL = auto()
	BYTE_LITERAL = auto()
	STRING_LITERAL = auto()
	BOOLEAN_LITERAL = auto()
	
	#keywords
	FUNC = auto()
	TYPE = auto()
	ENUM = auto()
	IMPORT = auto()
	WHILE = auto()
	FOR = auto()
	BREAK = auto()
	IN = auto()
	IF = auto()
	ELSE = auto()
	AND = auto()
	OR = auto()
	NOT = auto()
	NULL = auto()
	TRUE = auto()
	FALSE = auto()
	CIMPORT = auto()
	FROM = auto()
	RETURN = auto()
	
	#symbols
	L_SINGLE_QUOTE = auto()
	R_SINGLE_QUOTE = auto()
	L_DOUBLE_QUOTE = auto()
	R_DOUBLE_QUOTE = auto()
	LEFT_PAREN = auto()
	RIGHT_PAREN = auto()
	LEFT_BRACE = auto()
	RIGHT_BRACE = auto()
	LEFT_BRACKET = auto()
	RIGHT_BRACKET = auto()
	L_ANGLE_BRACKET = auto()
	R_ANGLE_BRACKET = auto()
	COMMA = auto()
	COLON = auto()
	SEMICOLON = auto()
	EQUALS = auto()
	
	#operators
	PLUS = auto()
	MINUS = auto()
	STAR = auto()
	SLASH = auto()
	MOD = auto()
	BAR = auto()
	AMPERSAND = auto()
	CARET = auto()
	TILDE = auto()
	PERIOD = auto()
	BANG = auto()
	NOT_EQUALS = auto()
	DOUBLE_EQUALS = auto()
	LESS_THAN_EQUAL = auto()
	GREATER_THAN_EQUAL = auto()
	
	#other
	IDENTIFIER = auto()
	LINE_COMMENT = auto()

	#special
	FSTRING_OPEN = auto()
	FSTRING_SUBSTR = auto()
	FSTRING_BEGIN_EXPR = auto()
	FSTRING_END_EXPR = auto()
	FSTRING_CLOSE = auto()
	EOF = auto()



class Token:
	__slots__ = ("type", "text", "line_no", "char_no")
	def __init__(self, token_type, text, line_no, char_no):
		self.type = token_type
		self.text = text
		self.line_no = line_no
		self.char_no = char_no

	def __repr__(self):
		return f"Token<{self.type}, {self.text} at {self.line_no}:{self.char_no}>"

	@staticmethod
	def fromEOF():
		return Token(TokenType.EOF, None, -1, -1)


class Lexer:
	def __init__(self, s):
		self.text = s

		self.char_idx = 0
		self.line_no = 1
		self.char_no = 0 #meaning the number of characters since new line
	
	@staticmethod
	def from_file(fname):
		with open(fname) as f:
			text = f.read()

		l = Lexer(text)
		l.fname = fname
		return l

	def make_token(self, token_type, text):
		return Token(token_type, text, self.line_no, self.char_no)

	def error(self, msg):
		fname = color(f"{self.fname}", "green")
		print(f"[{fname} {self.line_no}:{self.char_no}] {msg}")
		exit()

	def peek(self, offset=0):
		if self.char_idx + 1 + offset >= len(self.text):
			return "\0"
		else:
			return self.text[self.char_idx + offset]

	def cur(self):
		pass
	
	def eat(self):
		if self.char_idx >= len(self.text):
			return "\0"
		else:
			char = self.text[self.char_idx]
			self.char_idx += 1
			self.char_no += 1
			return char

	def switch(self, char):
		if char == "\0":
			return Token.fromEOF()

		elif char == "f" and self.peek() in "\"'":
			char += self.eat()
			return [tok for tok in self.fstring(char)]

		elif char.isalpha() or char == "_":
			return self.kwd_or_identifier(char)
		
		elif char.isdigit():
			return self.numeric_literal(char)

		elif char in "\"'":
			return self.string(char)

		elif char == "\n":
			self.line_no += 1
			self.char_no = 1
			return

		elif char == "(":
			return self.make_token(TokenType.LEFT_PAREN, char)

		elif char == ")":
			return self.make_token(TokenType.RIGHT_PAREN, char)

		elif char == "<":
			if self.peek() == "=":
				char += "="
				return self.make_token(TokenType.LESS_THAN_EQUAL, char)
			return self.make_token(TokenType.L_ANGLE_BRACKET, char)

		elif char == ">":
			if self.peek() == "=":
				char += "="
				return self.make_token(TokenType.GREATER_THAN_EQUAL, char)
			return self.make_token(TokenType.R_ANGLE_BRACKET, char)

		elif char == "{":
			return self.make_token(TokenType.LEFT_BRACE, char)

		elif char == "}":
			return self.make_token(TokenType.RIGHT_BRACE, char)

		elif char == "[":
			return self.make_token(TokenType.LEFT_BRACKET, char)

		elif char == "]":
			return self.make_token(TokenType.RIGHT_BRACKET, char)

		elif char == ",":
			return self.make_token(TokenType.COMMA, char)

		elif char == ":":
			return self.make_token(TokenType.COLON, char)

		elif char == ";":
			return self.make_token(TokenType.SEMICOLON, char)

		elif char == "=":
			if self.peek() == "=":
				self.eat()
				char += "="
				return self.make_token(TokenType.DOUBLE_EQUALS, char)
			return self.make_token(TokenType.EQUALS, char)

		elif char == "+":
			return self.make_token(TokenType.PLUS, char)

		elif char == "-":
			return self.make_token(TokenType.MINUS, char)

		elif char == "*":
			return self.make_token(TokenType.STAR, char)

		elif char == "/":
			if self.peek() == "/":
				char += "/"
				return self.line_comment(char)
			return self.make_token(TokenType.SLASH, char)

		elif char == "%":
			return self.make_token(TokenType.MOD, char)

		elif char == "|":
			return self.make_token(TokenType.BAR, char)

		elif char == "&":
			return self.make_token(TokenType.AMPERSAND, char)

		elif char == "^":
			return self.make_token(TokenType.CARET, char)

		elif char == "~":
			return self.make_token(TokenType.TILDE, char)

		elif char == ".":
			return self.make_token(TokenType.PERIOD, char)

		elif char == "!":
			if self.peek() == "=":
				char += "="
				return self.make_token(TokenType.NOT_EQUALS, char)
			return self.make_token(TokenType.BANG, char)


		elif char in string.whitespace:
			return
		else:
			self.error(f"Invalid character {char}")

	
	def kwd_or_identifier(self, char):
		valid_chars = string.ascii_letters + string.digits + "_"
		chars = char
		while self.peek() in valid_chars:
			chars += self.eat()

		if chars == "func":
			tok_type = TokenType.FUNC
		elif chars == "type":
			tok_type = TokenType.TYPE
		elif chars == "enum":
			tok_type = TokenType.ENUM
		elif chars == "import":
			tok_type = TokenType.IMPORT
		elif chars == "while":
			tok_type = TokenType.WHILE
		elif chars == "for":
			tok_type = TokenType.FOR
		elif chars == "break":
			tok_type = TokenType.BREAK
		elif chars == "in":
			tok_type = TokenType.IN
		elif chars == "if":
			tok_type = TokenType.IF
		elif chars == "else":
			tok_type = TokenType.ELSE
		elif chars == "and":
			tok_type = TokenType.AND
		elif chars == "or":
			tok_type = TokenType.OR
		elif chars == "not":
			tok_type = TokenType.NOT
		elif chars == "null":
			tok_type = TokenType.NULL
		elif chars == "true" or chars == "false":
			tok_type = TokenType.BOOLEAN_LITERAL
		elif chars == "import":
			tok_type = TokenType.IMPORT
		elif chars == "cimport":
			tok_type = TokenType.CIMPORT
		elif chars == "from":
			tok_type = TokenType.FROM
		elif chars == "return":
			tok_type = TokenType.RETURN
		else:
			#must be identifier if not any of above
			tok_type = TokenType.IDENTIFIER

		return self.make_token(tok_type, chars)

	def numeric_literal(self, char):
		seen_dot = False
		chars = char
		tok_type = TokenType.INT_LITERAL
		
		second_char = self.peek()

		if second_char == "b":
			chars += self.eat()
			return self.binary_literal(chars)
		elif second_char == "x":
			chars += self.eat()
			return self.hex_literal(chars)
		else:
			while self.peek() in string.digits + "_.uf":
				if self.peek() == "." and self.peek(offset=1) == ".":
					chars = "".join([c for c in chars if c != "_"])
					return self.make_token(tok_type, chars)
				if self.peek() == ".":
					if seen_dot:
						self.error(f"Too many decimal points in numeric literal")
					seen_dot = True
					chars += self.eat()
					tok_type = TokenType.DOUBLE_LITERAL
				elif self.peek() == "_":
					chars += self.eat()
				elif self.peek().isdigit():
					chars += self.eat()
				elif self.peek() == "f":
					tok_type = TokenType.FLOAT_LITERAL
					chars += self.eat()
					chars = "".join([c for c in chars if c != "_"])
					return self.make_token(tok_type, chars)
				elif self.peek() == "u":
					tok_type = TokenType.UINT_LITERAL
					if seen_dot:
						self.error(f"Uint literal cannot contain decimal point")
					chars += self.eat()
					chars = "".join([c for c in chars if c != "_"])
					return self.make_token(tok_type, chars)
				else:
					self.error(f"Unexpected character in numeric literal")

			chars = "".join([c for c in chars if c != "_"])
			return self.make_token(tok_type, chars)


	def binary_literal(self, chars):
		while self.peek() in "01_":
			chars += self.eat()

		chars = "".join([c for c in chars if c != "_"])
		return self.make_token(TokenType.BINARY_LITERAL, chars)

	def hex_literal(self, chars):
		while self.peek() in string.hexdigits + "_":
			chars += self.eat()
		
		chars = "".join([c for c in chars if c != "_"])
		return self.make_token(TokenType.HEX_LITERAL, chars)


	def string(self, char):
		initial_char = char
		chars = char
		
		while self.peek() != initial_char:
			chars += self.eat()

		chars += self.eat()

		return self.make_token(TokenType.STRING_LITERAL, chars)
	
	def line_comment(self, char):
		chars = char
		while self.peek() not in "\n\0":
			chars += self.eat()

		return self.make_token(TokenType.LINE_COMMENT, chars)

	def fstring(self, char):
		"""I'm sure this is incomplete somehow. Fairly sure mismatched braces are
		not covered, I'm sure some things with strings are messy also"""

		initial_char = char[-1]
		chars = ""
		yield self.make_token(TokenType.FSTRING_OPEN, char)
		while self.peek() != initial_char:
			if self.peek() == "{":
				if chars:
					yield self.make_token(TokenType.FSTRING_SUBSTR, chars)
					chars = ""

				yield self.make_token(TokenType.FSTRING_BEGIN_EXPR, self.eat())
				while self.peek() != "}":
					tok = self.switch(self.eat())
					if tok.type == TokenType.EOF:
						self.error("Unexpected EOF in fstring")
					else:
						yield tok
				yield self.make_token(TokenType.FSTRING_END_EXPR, self.eat())
			else:
				chars += self.eat()
		yield self.make_token(TokenType.FSTRING_CLOSE, self.eat())


	def tokenize(self):
		while True:
			tok = self.switch(self.eat())
			if tok is None:
				continue
			elif isinstance(tok, list): #needed for fstrings
				for sub in tok:
					yield sub
			elif tok.type == TokenType.EOF:
				yield tok
				break
			else:
				yield tok

class TokenList:
	def __init__(self, tok_list):
		self.tok_list = tok_list
		self.tok_idx = 0
	
	def peek(self, offset=0):
		if len(tok_list) <= tok_idx + 1 + offset:
			return Token.fromEOF()
		
	
	def eat(self):
		self.tok_idx += 1
		if self.tok_idx - 1 >= len(tok_list):
			return Token.fromEOF()
		return self.tok_list[tok_idx - 1]
		
	
	

if __name__ == "__main__":
	lexer = Lexer("testprog.sui", Token)
	for tok in lexer.tokenize():
		print(tok)
