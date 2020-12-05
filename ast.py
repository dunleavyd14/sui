from collections import ChainMap

NULL_PTR = "__NULL_PTR__"


class TypeInfo:
	def __init__(self, name, indirection=0, arr=()):
		self.name = name
		self.indirection = indirection
		self.arr = arr
	
	def __eq__(self, other):
		return self.name == other.name and self.arr == other.arr \
				and self.indirection == other.indirection
	
	def __hash__(self):
		return hash((self.name, self.indirection, self.arr))
	
	
	def __repr__(self):
		stars = "*"*self.indirection
		if len(self.arr):
			arr_str = "".join([f"[{dim}]" for dim in self.arr])
		else:
			arr_str = ""

		return f"TypeInfo({stars}{self.name}{arr_str})"

class TypeDefn:
	def __init__(self, name, members):
		self.name = name
		self.members = members

class GenericTypeDefn:
	def __init__(self, name, gen_args, members):
		self.name = name
		self.gen_args = gen_args
		self.members = members
		self.child_types = {}


class Root:
	def __init__(self):
		self.generic_funcs = {}
		self.generic_impls = {}
		self.generic_types = {}
		self.concrete_funcs = {}
		self.concrete_types = {}
		self.vars = ChainMap()
	
	def push_scope(self, scope=None):
		if not scope:
			self.vars.maps.append({})
		else:
			self.vars.maps.append(scope)
	
	def pop_scope(self):
		self.vars.maps.pop()
	
	def register_generic_func(self, generic):
		pass

	def register_concrete_func(self, func):
		pass

	def register_generic_type(self, func):
		pass
	
	def register_type(self, type_defn):
		pass
	
	def func_name_exists(self, name):
		if name in self.generic_funcs:
			return self.generic_funcs[name]
		elif name in [n for n, args in self.concrete_funcs]:
			return [defn for (n, args), defn in self.concrete_funcs.items() \
						if n == name]
	

class GenericFuncDefn:
	def __init__(self, name, gen_args, args, return_type, statements):
		self.name = name
		self.gen_args = gen_args
		self.args = args
		self.return_type = return_type
		self.statements = statements


class FuncDefn:
	def __init__(self, name, args, return_type, statements):
		self.name = name
		self.args = args
		self.statements = statements

#statements

class DeclAssignment:
	def __init__(self, var, expr):
		self.var = var
		self.expr = expr

class WhileLoop:
	def __init__(self, cond, statement):
		self.cond = cond
		self.statement = statement
	
	def check_types(self):
		pass
	


class ForLoop:
	def __init__(self, name, start, end, statement):
		self.name = name
		self.start = start
		self.end = end
		self.statement = statement
	
	def check_types(self):
		pass
	
	def make_type_substituions(self, root):
		pass

class IfStmt:
	def __init__(self, cond, statement):
		self.cond = cond
		self.statement = statement

class IfElseStmt:
	def __init__(self, cond, if_stmt, else_stmt):
		self.cond = cond
		self.if_stmt = if_stmt
		self.else_stmt = else_stmt

class ReturnStmt:
	def __init__(self, expr):
		self.expr = expr

class Block:
	def __init__(self, statements):
		self.statements = statements
	
class ExprAsStatement:
	def __init__(self, expr):
		self.expr = expr
	


#exprs

class LogicalBinExpr:
	def __init__(self, lexpr, op, rexpr):
		self.lexpr = lexpr
		self.op = op
		self.rexpr = rexpr

	def __repr__(self):
		return f"({self.lexpr} {self.op.text} {self.rexpr})"

class MathBinExpr:
	def __init__(self, lexpr, op, rexpr):
		self.lexpr = lexpr
		self.op = op
		self.rexpr = rexpr
	
	def __repr__(self):
		return f"({self.lexpr} {self.op.text} {self.rexpr})"

class NumericalComparisonExpr:
	def __init__(self, lexpr, op, rexpr):
		self.lexpr = lexpr
		self.op = op
		self.rexpr = rexpr

	def __repr__(self):
		return f"({self.lexpr} {self.op.text} {self.rexpr})"

class BitwiseBinExpr:
	def __init__(self, lexpr, op, rexpr):
		self.lexpr = lexpr
		self.op = op
		self.rexpr = rexpr

	def __repr__(self):
		return f"({self.lexpr} {self.op} {self.rexpr})"

class LogicalUnaryExpr:
	def __init__(self, op, rexpr):
		self.op = op
		self.rexpr = rexpr
	
	def __repr__(self):
		return f"({self.op.text}{self.rexpr})"

class MathUnaryExpr:
	def __init__(self, op, rexpr):
		self.op = op
		self.rexpr = rexpr
	
	def __repr__(self):
		return f"({self.op.text}{self.rexpr})"

class PointerUnaryExpr:
	def __init__(self, op, rexpr):
		self.op = op
		self.rexpr = rexpr

	def __repr__(self):
		return f"({self.op.text}{self.rexpr})"


class BitwiseUnaryExpr:
	def __init__(self, op, rexpr):
		self.op = op
		self.rexpr = rexpr

	def __repr__(self):
		return f"({self.op.text}{self.rexpr})"

class LiteralExpr:
	def __init__(self, tok, type_name):
		self.tok = tok
		self.type_info = TypeInfo(type_name)
	
	def __repr__(self):
		return f"{self.tok.text}"

class VariableExpr:
	def __init__(self, tok):
		self.tok = tok

	def __repr__(self):
		return f"Variable({self.tok.text} : {self.type_info}"

class CompoundLiteralExpr:
	def __init__(self, type_info, members):
		self.type_info = type_info
		self.members = members
	
	def __repr__(self):
		return f"CompoundLiteral({self.type_info.name}: {self.members})"






