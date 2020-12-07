import ast
from lexer import TokenType as T
from lexer import Token, Lexer
from colors import color

class UnreachableException(Exception):
	pass

class ParseException(Exception):
	pass

class Parser:
	def __init__(self, tokens, fname=None):
		self.tokens = tokens
		self._tok_idx = 0
		self.root = ast.Root()
		self.fname = fname
	
	@staticmethod
	def from_file(fname):
		lex = Lexer.from_file(fname)
		toks = [tok for tok in lex.tokenize() if tok.type not in [T.LINE_COMMENT]]
		return Parser(toks, fname=fname)

	@staticmethod
	def from_str(s):
		lex = Lexer(s)
		toks = [tok for tok in lex.tokenize() if tok.type not in [T.LINE_COMMENT]]
		print(toks)
		return Parser(toks)

	def error(self, msg):
		fname = color(f"{self.fname}", "green")
		tok = self.eat()
		print(f"[{fname} {tok.line_no}:{tok.char_no}] {msg}")
		exit()

	def peek(self, lookahead=0):
		if len(self.tokens) <= self._tok_idx + lookahead:
			return Token.fromEOF()
		return self.tokens[self._tok_idx + lookahead]

	def eat(self):
		if self._tok_idx >= len(self.tokens):
			return Token.fromEOF()
		else:
			tok = self.tokens[self._tok_idx]
			self._tok_idx += 1
			return tok
	
	def expect(self, exp_type):
		tok = self.eat()
		if tok.type != exp_type:
			self.error(f"Expected {exp_type} but instead got {tok}")
		else:
			return tok
	
	def parse(self):
		while self.peek().type in (T.IMPORT, T.CIMPORT):
			self.imports()

		while self.peek().type != T.EOF:
			tok = self.peek()

			if tok.type == T.FUNC:
				self.func_defn()
			elif tok.type == T.TYPE:
				self.struct_defn()
	
	def imports(self):
		imps = []
		while self.peek().type in [T.IMPORT, T.CIMPORT]:
			imp_type = self.eat()
			identifier = self.expect(T.IDENTIFIER)
			self.expect(T.FROM)
			fname = self.expect(T.STRING_LITERAL)
			self.expect(T.SEMICOLON)

		
		return imps

	def type(self):
		generic = False
		indirection = 0
		while self.peek().type == T.STAR:
			indirection += 1
			self.eat()

		type_name = self.expect(T.IDENTIFIER).text

		if self.peek().type == T.L_ANGLE_BRACKET:
			generic = True
			self.eat()
			gen_types = [self.type()]
			while self.peek().type != T.R_ANGLE_BRACKET:
				self.expect(T.COMMA)
				gen_types.append(self.type())

			self.expect(T.R_ANGLE_BRACKET)

			print(gen_types)

		arr_lens = []
		while self.peek().type == T.LEFT_BRACKET:
			self.eat()
			self.expect(T.RIGHT_BRACKET)
			arr_lens.append(-1)

		if generic:
			gen_type_defn = self.root.generic_types[type_name]
			concrete_type_defn = gen_type_defn.concrete_type_def(gen_types)
			self.root.concrete_types[concrete_type_defn.name] = concrete_type_defn

			return ast.TypeInfo(concrete_type_defn.name, indirection, tuple(arr_lens))

		return ast.TypeInfo(type_name, indirection, tuple(arr_lens))

	def struct_defn(self):
		self.expect(T.TYPE)
		type_name = self.expect(T.IDENTIFIER).text
		generic_args = []
		if self.peek().type == T.L_ANGLE_BRACKET:
			self.eat()
			generic_args.append(self.expect(T.IDENTIFIER))
			while self.peek().type != T.R_ANGLE_BRACKET:
				self.expect(T.COMMA)
				generic_args.append(self.expect(T.IDENTIFIER))

			self.expect(T.R_ANGLE_BRACKET)

		self.expect(T.LEFT_BRACE)


		members = {}
		while self.peek().type != T.RIGHT_BRACE:
			field_name = self.expect(T.IDENTIFIER).text
			self.expect(T.COLON)
			type_info = self.type()
			self.expect(T.SEMICOLON)
			members[field_name] = type_info

		self.expect(T.RIGHT_BRACE)
		
		if len(generic_args) == 0:
			defn = ast.TypeDefn(type_name, members)
			print(defn.gen_code())
			self.root.concrete_types[type_name] = defn

			return defn
		else:
			defn = ast.GenericTypeDefn(type_name, generic_args, members)
			self.root.generic_types[type_name] = defn
	
	def func_defn(self):
		self.expect(T.FUNC)
		if self.peek(1).type == T.L_ANGLE_BRACKET:
			defn = self._generic_func_defn()
			self.root.generic_funcs[defn.name] = defn
		elif self.peek(1).type == T.LEFT_PAREN:
			defn = self._concrete_func_defn()
			self.root.concrete_funcs[defn.name, tuple(defn.args.values())] = \
					defn
			defn.check_types(self.root)
			print(defn.gen_code())
		else:
			self.error("Unexpected token {self.peek(1)} while \
					parsing function definition")

	
	def _generic_func_defn(self):
		func_name = self.expect(T.IDENTIFIER).text
		generic_args = []

		self.expect(T.L_ANGLE_BRACKET)

		generic_args.append(self.expect(T.IDENTIFIER))
		
		while self.peek().type != T.R_ANGLE_BRACKET:
			self.expect(T.COMMA)
			generic_args.append(self.expect(T.IDENTIFIER))

		self.expect(T.R_ANGLE_BRACKET)
		
		#get argument defns
		self.expect(T.LEFT_PAREN)
		if self.peek().type == T.RIGHT_PAREN: #no args
			args = {}
			self.expect(T.RIGHT_PAREN)

		elif self.peek().type == T.IDENTIFIER:
			args = {}
			arg_name = self.expect(T.IDENTIFIER).text
			self.expect(T.COLON)
			arg_type = self.type()

			args[arg_name] = arg_type

			while self.peek().type != T.RIGHT_PAREN:
				self.expect(T.COMMA)
				arg_name = self.expect(T.IDENTIFIER).text
				self.expect(T.COLON)
				arg_type = self.type()

				args[arg_name] = arg_type
			
			self.expect(T.RIGHT_PAREN)

		else:
			self.error(f"Unexpected token {self.peek()} \
					while parsing generic function definition")


		return_type = self.type()
		print(args, return_type)

		self.root.push_scope(args.keys())
		statements = self.block()
		self.root.pop_scope()

		return ast.GenericFuncDefn(func_name, generic_args, \
				args, return_type, statements)
	
	def _concrete_func_defn(self):
		func_name = self.expect(T.IDENTIFIER).text
		self.expect(T.LEFT_PAREN)

		if self.peek().type == T.RIGHT_PAREN:
			self.expect(T.RIGHT_PAREN)
			args = {}
		elif self.peek().type == T.IDENTIFIER:
			args = {}
			arg_name = self.expect(T.IDENTIFIER).text
			self.expect(T.COLON)
			arg_type = self.type()

			args[arg_name] = arg_type

			while self.peek().type != T.RIGHT_PAREN:
				self.expect(T.COMMA)
				arg_name = self.expect(T.IDENTIFIER).text
				self.expect(T.COLON)
				arg_type = self.type()

				args[arg_name] = arg_type

			self.expect(T.RIGHT_PAREN)
			
		else:
			self.error(f"Unexpected token {self.peek()} \
					while parsing concrete function definition")
		print("before statements", args)
		copy = args.copy()
		return_type = self.type()

		self.root.push_scope(args)
		self.root.push_scope()
		statements = self.block()
		self.root.pop_scope()
		self.root.pop_scope()

		print("AFTER", args, copy)
		return ast.FuncDefn(func_name, args, return_type, statements)


	def for_loop(self):
		self.expect(T.FOR)
		name = self.expect(T.IDENTIFIER)
		self.expect(T.IN)
		start = self.expr()
		self.expect(T.PERIOD)
		self.expect(T.PERIOD)
		end = self.expr()
		block = self.block()

		return ast.ForLoop(name, start, end, block)

	def while_loop(self):
		self.expect(T.WHILE)
		cond = self.expr()
		block = self.block()

		return ast.WhileLoop(cond, block)

	
	def block(self):
		self.expect(T.LEFT_BRACE)
		self.root.push_scope()
		stmts = []
		while self.peek().type != T.RIGHT_BRACE:
			stmts.append(self.statement())

		self.expect(T.RIGHT_BRACE)
		self.root.pop_scope()

		return ast.Block(stmts)
		
	def declare_and_assign(self):
		var_name = self.expect(T.IDENTIFIER).text
		if self.root.var_exists(var_name):
			self.error(f"Variable '{var_name}' already delcared")

		self.expect(T.COLON)
		if self.peek().type == T.EQUALS:
			self.expect(T.EQUALS)
			expr = self.expr()
			self.expect(T.SEMICOLON)

			if expr is ast.NULL_PTR:
				self.error(f"Cannot infer type of null pointer")
			else:
				self.root.add_var(var_name)
				return ast.DeclAssignment(var_name, expr)
		else:
			declared_type = self.type()
			expr = self.expr()
			self.root.add_var(var_name)
			return ast.DeclAssignment(var_name, expr, decl_type=declared_type)
	
	def assign(self):
		#get lvalue
		pass

	def return_statement(self):
		self.expect(T.RETURN)
		if self.peek().type == T.SEMICOLON:
			expr = None
		expr = self.expr()
		self.expect(T.SEMICOLON)

		return ast.ReturnStmt(expr)

	def expr(self):
		return self.logical_or()
	

	def logical_or(self):
		lexpr = self.logical_and()

		while self.peek().type == T.OR:
			op = self.eat()
			rexpr = self.logical_and()
			lexpr = ast.LogicalBinExpr(lexpr, op, rexpr)

		return lexpr

	def logical_and(self):
		lexpr = self.bitwise_or()

		while self.peek().type == T.AND:
			op = self.eat()
			rexpr = self.bitwise_or()
			lexpr = ast.LogicalBinExpr(lexpr, op, rexpr)

		return lexpr

	def bitwise_or(self):
		lexpr = self.bitwise_and()

		while self.peek().type == T.BAR:
			op = self.eat()
			rexpr = self.bitwise_and()
			lexpr = ast.BitwiseBinExpr(lexpr, op, rexpr)

		return lexpr

	def bitwise_and(self):
		lexpr = self.equality()

		while self.peek().type == T.AMPERSAND:
			op = self.eat()
			rexpr = self.equality()
			lexpr = ast.BitwiseBinExpr(lexpr, op, rexpr)

		return lexpr

	def equality(self):
		lexpr = self.comparison()

		while self.peek().type == [T.NOT_EQUALS, T.DOUBLE_EQUALS]:
			op, _  = self.eat(), self.eat()

			rexpr = self.comparison()
			lexpr = ast.NumericalComparisonExpr(lexpr, op, rexpr)

		return lexpr

	def comparison(self):
		lexpr = self.bitshift()

		while self.peek().type in [T.LESS_THAN_EQUAL, T.GREATER_THAN_EQUAL, \
				T.L_ANGLE_BRACKET, T.R_ANGLE_BRACKET]:
			
			op = self.eat()
			rexpr = self.bitshift()
			lexpr = ast.LogicalBinExpr(lexpr, op, rexpr)
		
		return lexpr

	def bitshift(self):
		lexpr = self.term()

		while self.peek().type == self.peek(1).type == T.L_ANGLE_BRACKET or \
				self.peek().type == self.peek(1).type == T.R_ANGLE_BRACKET:

			op = self.eat().text + self.eat().text

			rexpr = self.term()
			lexpr = ast.BitwiseBinExpr(lexpr, op, rexpr)

		return lexpr

	def term(self):
		lexpr = self.factor()

		while self.peek().type in [T.MINUS, T.PLUS]:
			op = self.eat()
			rexpr = self.factor()
			lexpr = ast.MathBinExpr(lexpr, op, rexpr)
		return lexpr

	def factor(self):
		lexpr = self.unary()

		while self.peek().type in [T.STAR, T.SLASH, T.MOD]:
			op = self.eat()
			rexpr = self.unary()
			lexpr = ast.MathBinExpr(lexpr, op, rexpr)

		return lexpr

	def unary(self):
		if self.peek().type == T.BANG:
			op = self.eat()
			rexpr = self.unary()
			return LogicalUnaryExpr(op, rexpr)
	
		elif self.peek().type == T.MINUS:
			op = self.eat()
			rexpr = self.unary()
			return ast.MathUnaryExpr(op, rexpr)

		elif self.peek().type == T.TILDE:
			op = self.eat()
			rexpr = self.unary()
			return ast.BitwiseUnaryExpr(op, rexpr)

		elif self.peek().type in [T.STAR, T.AMPERSAND]:
			op = self.eat()
			rexpr = self.unary()
			return ast.PointerUnaryExpr(op, rexpr)

		return self.array_access()

	def member_access(self):
		expr = self.array_access()

		if self.peek().type == T.PERIOD:
			self.eat()
			identifier = self.expect(T.IDENTIFIER)




	def array_access(self):
		expr = self.primary()

		while self.peek().type == T.LEFT_BRACKET:
			self.eat()
			expr = ast.ArrayAccessExpr(expr, self.expr())
			self.expect(T.RIGHT_BRACKET)

		return expr

	def primary(self):
		if self.peek().type == T.INT_LITERAL:
			return ast.LiteralExpr(self.eat(), "int")
		elif self.peek().type == T.BINARY_LITERAL:
			return ast.LiteralExpr(self.eat(), "uint")
		elif self.peek().type == T.HEX_LITERAL:
			return ast.LiteralExpr(self.eat(), "uint")
		elif self.peek().type == T.UINT_LITERAL:
			return ast.LiteralExpr(self.eat(), "uint")
		elif self.peek().type == T.FLOAT_LITERAL:
			return ast.LiteralExpr(self.eat(), "float")
		elif self.peek().type == T.DOUBLE_LITERAL:
			return ast.LiteralExpr(self.eat(), "double")
		elif self.peek().type == T.STRING_LITERAL:
			return ast.LiteralExpr(self.eat(), "string")
		elif self.peek().type == T.BOOLEAN_LITERAL:
			return ast.LiteralExpr(self.eat(), "bool")
		elif self.peek().type == T.IDENTIFIER:

			tok = self.eat()
			if self.root.var_exists(tok.text): #it's a variable
				return ast.VariableExpr(tok)
			else:
				funcs = self.root.func_name_exists(tok.text)

				if not funcs: #meaning no match to any known identifier
					self.error(f"Undeclared identifier '{tok.text}'")

				generic = False
				gen_types = []
				if self.peek().type == T.L_ANGLE_BRACKET: #func is generic
					self.eat()
					gen_types = [self.type()]
					while self.peek().type != T.R_ANGLE_BRACKET:
						self.expect(T.COMMA)
						gen_types.append(self.type())

					self.expect(T.R_ANGLE_BRACKET)

				self.expect(T.LEFT_PAREN)
				args = [self.expr()]
				while self.peek().type != T.RIGHT_PAREN:
					self.expect(T.COMMA)
					gen_types.append(self.expr())

				self.expect(T.RIGHT_PAREN)
				
				if generic:
					defn = self.root.generic_funcs[tok.text]
					return ast.GenericFunctionCallExpr(defn, 


		elif self.peek().type == T.LEFT_PAREN:
			self.eat()
			expr = self.expr()
			self.expect(T.RIGHT_PAREN)
			return expr
		
		elif self.peek().type == T.LEFT_BRACE: #compound literal
			self.eat()
			type_info = self.type()
			self.expect(T.COLON)
			members = [self.expr()]

			while self.peek().type != T.RIGHT_BRACE:
				self.expect(T.COMMA)
				members.append(self.expr())

			self.expect(T.RIGHT_BRACE)

			#check to see that types are valid
			self.root.lookup_type_by_name(type_info.name)

			return ast.CompoundLiteralExpr(type_info, members)

		elif self.peek().type == T.LEFT_BRACKET:
			self.eat()
			type_info = self.type()
			self.expect(T.COLON)

			members = [self.expr()] #for now this basically asserts that array
									#literals are nonempty

			while self.peek().type != T.RIGHT_BRACKET:
				self.expect(T.COMMA)
				members.append(self.expr())

			self.expect(T.RIGHT_BRACKET)

			return ast.ArrayLiteralExpr(type_info, members)

		else:
			self.error(f"Unexpected token {self.peek()} while parsing expr")
			
	


	def statement(self):
		tok_type = self.peek().type

		if tok_type == T.FOR:
			return self.for_loop()
		elif tok_type == T.WHILE:
			return self.while_loop()
		elif tok_type == T.IDENTIFIER and self.peek(1).type == T.COLON:
			return self.declare_and_assign()
		elif tok_type == T.LEFT_BRACE:
			return self.block()
		elif tok_type == T.RETURN:
			return self.return_statement()
		else:
			expr = self.expr()
			self.expect(T.SEMICOLON)
			return ast.ExprAsStatement(expr)

		# might be possible to just do else: expr_as_statement() here



if __name__ == "__main__":
	print(Parser.from_str("(4 + -3) * 5").expr())
	parser = Parser.from_file("testprog.sui")
	parser.parse()
	print(parser.root.concrete_types)

















