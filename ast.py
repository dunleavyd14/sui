from collections import ChainMap
from copy import deepcopy
NULL_PTR = "__NULL_PTR__"

class TypeMap:
	def __init__(self, d):
		self.dict = d
	
	def __getitem__(self, key):
		if key in self.dict:
			return self.dict[key]
		else:
			return key

class TypeInfo:
	def __init__(self, name, indirection=0, arr=(), gen_args=()):
		self.name = name
		self.indirection = indirection
		self.arr = arr
		self.gen_args = gen_args
	
	def addr_of(self):
		return TypeInfo(self.name, indirection=self.indirection+1, arr=self.arr, \
				gen_args=self.gen_args)

	def deref(self):
		return TypeInfo(self.name, indirection=self.indirection-1, arr=self.arr, \
				gen_args=self.gen_args)

	
	def is_numeric(self):
		return self in [TypeInfo("uint"), TypeInfo("int"), TypeInfo("float"), \
				TypeInfo("double"), TypeInfo("byte")]
	
	def is_boolean(self):
		return self in [TypeInfo("uint"), TypeInfo("int"), TypeInfo("boolean")]

	def __eq__(self, other):
		return self.name == other.name and self.arr == other.arr \
				and self.indirection == other.indirection
	
	def __hash__(self):
		return hash((self.name, self.indirection, self.arr))
	
	def gen_code(self):
		stars = "*"*self.indirection
		if len(self.arr):
			arr_str = "".join([f"[]" for dim in self.arr])
		else:
			arr_str = ""

		return f"{self.name}{stars}{arr_str}"
	
	
	def __repr__(self):
		stars = "*"*self.indirection
		if len(self.arr):
			arr_str = "".join([f"[]" for dim in self.arr])
		else:
			arr_str = ""

		return f"TypeInfo({stars}{self.name}{arr_str})"

class TypeDefn:
	def __init__(self, name, members):
		self.name = name
		self.members = members
	
	def __repr__(self):
		return f"TypeDefn({self.name}{self.members})"
	
	def gen_code(self):
		mems = [f"{v.gen_code()} {k}" for k, v in self.members.items()]
		return f"typedef struct {{{'; '.join(mems)}}} {self.name};"

class GenericTypeDefn:
	def __init__(self, name, gen_args, members):
		self.name = name
		self.gen_args = gen_args
		self.members = members
		self.child_types = {}
	
	def concrete_type_def(self, gen_args):
		gen_args = tuple(gen_args)
		if gen_args in self.child_types:
			return self.child_types[gen_args]
		if len(gen_args) != len(self.gen_args):
			raise TypeError("Invalid number of arguments for generic type {self.name}")
		type_map = TypeMap({TypeInfo(a.text) : b for a, b in zip(self.gen_args, gen_args)})
		print("mems", self.members, type_map.dict)

		conc_mems = {n : type_map[t] for n, t in self.members.items()}
		
		
		defn = TypeDefn(f"{self.name}_{len(self.child_types)}", conc_mems)
		print(defn)
		self.child_types[gen_args] = defn
		return defn


class Root:
	def __init__(self):
		self.generic_funcs = {}
		self.generic_impls = {}
		self.generic_types = {}
		self.concrete_funcs = {}
		self.concrete_types = {}
		self.vars = []
	
	def var_exists(self, var_name):
		return var_name in set.union(*self.vars)
	
	def push_scope(self, args=None):
		self.vars.append(set(args)) if args else self.vars.append(set())
	
	def pop_scope(self):
		self.vars.pop()
	
	def add_var(self, var_name):
		self.vars[-1].add(var_name)
	
	def lookup_type_by_name(self, name):
		impls = [gen.child_types for gen in self.generic_types.values()]

		all_types = {}
		for i in impls:
			all_types.update(i)
		
		print("ALL", all_types)

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
		self.impls = {}
	
	def make_concrete_func(self, gen_args):
		type_map = TypeMap({a : b for a, b in zip(self.gen_args, gen_args)})
		new_args = {k : type_map[v] for k, v in self.args.items()}
		return_type = TypeMap[self.return_type]

		new_statements = deepcopy(statements)

		new_statements.substitute_types(type_map)

		defn = FuncDefn(self.name, new_args, return_type, new_statements)
		self.impls[gen_args] = defn
		return defn


class FuncDefn:
	_id = 0
	def __init__(self, name, args, return_type, statements):
		self.name = name
		self.args = args
		self.return_type = return_type
		self.statements = statements
		self.id = FuncDefn._id
		FuncDefn._id += 1
		self.c_name = f"{self.name}_SUI{self.id}"
	
	def check_types(self, root):
		scopes = ChainMap(self.args.copy())
		#these next two lines are awful but it gets the job done
		scopes.func = self
		scopes.root = root
		self.statements.check_types(scopes)
		
	
	def gen_code(self):
		if self.name == "main":
			self.id = ""
		args = [f"{v.gen_code()} {k}" for k, v in self.args.items()]
		print("IN GEN_CODE", args)
		top = f"{self.return_type.gen_code()} {self.name}{self.id} ({','.join(args)})"
		return top + self.statements.gen_code()

#statements

class DeclAssignment:
	def __init__(self, var, expr, decl_type=None):
		self.var = var
		self.expr = expr
		self.decl_type = decl_type
		self.type_info = None
	
	def substitute(self, type_map):
		self.decl_type = type_map[decl_type]
		self.expr.substitute(type_map)
	
	def check_types(self, scopes):
		self.expr.check_types(scopes)
		if self.decl_type:
			if self.decl_type != self.expr.type_info:
				raise TypeError(f"Declared type doesn't match expr type")

		else:
			self.type_info = self.expr.type_info

		scopes[self.var] = self.type_info
	
	def __repr__(self):
		return f"{self.var}:= {self.expr}"
	
	def gen_code(self):
		return f"{self.type_info.gen_code()} {self.var} = {self.expr.gen_code()};" + "\n"

class WhileLoop:
	def __init__(self, cond, statement):
		self.cond = cond
		self.statement = statement
	
	def substitute(self, type_map):
		self.cond.substitute(type_map)
		self.statement.substitute(type_map)
	
	def check_types(self, scopes):
		self.cond.check_types(scopes)
		
		if self.cond.type_info not in (TypeInfo("int"), TypeInfo("boolean")):
			raise TypeError(f"Invalid condition type for while loop")

		self.statement.check_types(scopes)



class ForLoop:
	def __init__(self, name, start, end, statement):
		self.name = name
		self.start = start
		self.end = end
		self.statement = statement

	def substitute(self, type_map):
		self.start.substitute(type_map)
		self.end.substitute(type_map)
		self.statement.substitute(type_map)
	
	def check_types(self, scopes):
		self.start.check_types()
		self.end.check_types()

		if self.start.type_info != self.end.type_info:
			raise TypeError(f"For loop bounds must be of same type")

		if self.start.type_info.name not in \
				["int", "double", "float", "uint", "byte"]:
			raise TypeError(f"For loop bounds must be numeric")

		if self.start.type_info.indirection or len(self.start.type_info.arr):
			raise TypeError(f"For loop bounds must be a value type")

		scopes = scopes.new_child(m={self.name : self.start.type_info})
		self.statement.check_types(scopes)
	

class IfStmt:
	def __init__(self, cond, statement, else_stmt=None):
		self.cond = cond
		self.statement = statement
		self.else_stmt = else_stmt
	
	def substitute(self, type_map):
		self.cond.substitute(type_map)
		self.statement.substitute(type_map)
		if self.else_stmt:
			self.else_stmt.substitute(type_map)
	
	def check_types(self, scopes):
		self.cond.check_types(scopes)
		
		if self.cond.type_info not in (TypeInfo("int"), TypeInfo("boolean")):
			raise TypeError(f"Invalid condition type for while loop")

		self.statement.check_types(scopes)
		if self.else_stmt:
			self.else_stmt.check_types(scopes)


class ReturnStmt:
	def __init__(self, expr):
		self.expr = expr
	
	def substitute(self, type_map):
		if self.expr:
			self.expr.substitute(type_map)
	
	def	check_types(self, scopes):
		if not self.expr:
			if scopes.func.return_type != TypeInfo("void"):
				raise TypeError(f"Can't return expr if return type is void")
			return

		self.expr.check_types(scopes)
		if self.expr.type_info != scopes.func.return_type:
			raise TypeError(f"Incorrect return type in {func}")
	
	def gen_code(self):
		if self.expr:
			return f"return {self.expr.gen_code()};"
		else:
			return "return;"


class Block:
	def __init__(self, statements):
		self.statements = statements
	
	def substitute(self, type_map):
		for s in self.statements:
			s.substitute(type_map)
	
	def check_types(self, scopes):
		for s in self.statements:
			s.check_types(scopes)
	
	def gen_code(self):
		statements = [s.gen_code() for s in self.statements]
		return f"{{ {''.join(statements)} }}"
	
class ExprAsStatement:
	def __init__(self, expr):
		self.expr = expr
	
	def substitute(self, type_map):
		self.expr.substitute(type_map)
	
	def check_types(self, scopes):
		self.expr.check_types(scopes)
	
	def gen_code(self, tabs=0):
		return f"{''    ''*tabs}{self.expr.gen_code()};"

#exprs

class LogicalBinExpr:
	def __init__(self, lexpr, op, rexpr):
		self.lexpr = lexpr
		self.op = op
		self.rexpr = rexpr
	
	def substitute(self, type_map):
		self.lexpr.substitute(type_map)
		self.rexpr.substitute(type_map)
	
	def check_types(self, scopes):
		self.lexpr.check_types(type_map)
		self.rexpr.check_types(type_map)

		
		if not self.lexpr.type_info.is_boolean():
			raise TypeError(f"Invalid type for operator {self.op}: {self.lexpr}")
		if not self.rexpr.type_info.is_boolean():
			raise TypeError(f"Invalid type for operator {self.op}: {self.rexpr}")
		
		self.type_info = TypeInfo("boolean")

	def __repr__(self):
		return f"({self.lexpr} {self.op.text} {self.rexpr})"

	def gen_code(self, tabs=0):
		return f"{''    ''*tabs}{self.lexpr.gen_code()}{self.op}{self.rexpr.gen_code()}"

class MathBinExpr:
	def __init__(self, lexpr, op, rexpr):
		self.lexpr = lexpr
		self.op = op
		self.rexpr = rexpr
	
	def substitute(self, type_map):
		self.lexpr.substitute(type_map)
		self.rexpr.substitute(type_map)
	
	def check_types(self, scopes):
		self.lexpr.check_types(scopes)
		self.rexpr.check_types(scopes)

		if not self.lexpr.type_info.is_numeric():
			raise TypeError(f"Invalid type for operator {self.op}: {self.lexpr}")
		if not self.rexpr.type_info.is_numeric():
			raise TypeError(f"Invalid type for operator {self.op}: {self.rexpr}")

		types = [TypeInfo("double"), TypeInfo("float"), TypeInfo("int"), \
					TypeInfo("uint"), TypeInfo("byte")]

		for t in types:
			if t in [self.lexpr.type_info, self.rexpr.type_info]:
				self.type_info = t
				return
		else:
			raise Exception()

	def __repr__(self):
		return f"({self.lexpr} {self.op.text} {self.rexpr})"

	def gen_code(self, tabs=0):
		return f"{''    ''*tabs}{self.lexpr.gen_code()}{self.op}{self.rexpr.gen_code()}"

class NumericalComparisonExpr:
	def __init__(self, lexpr, op, rexpr):
		self.lexpr = lexpr
		self.op = op
		self.rexpr = rexpr

	def substitute(self, type_map):
		self.lexpr.substitute(type_map)
		self.rexpr.substitute(type_map)

	def check_types(self, scopes):
		self.lexpr.check_types(scopes)
		self.rexpr.check_types(scopes)

		if not self.lexpr.type_info.is_numeric():
			raise TypeError(f"Invalid type for operator {self.op}: {self.lexpr}")
		if not self.rexpr.type_info.is_numeric():
			raise TypeError(f"Invalid type for operator {self.op}: {self.rexpr}")

		types = [TypeInfo("double"), TypeInfo("float"), TypeInfo("int"), \
					TypeInfo("uint"), TypeInfo("byte")]

		for t in types:
			if t in [self.lexpr.type_info, self.rexpr.type_info]:
				self.type_info = t
				return
		else:
			raise Exception()

	def __repr__(self):
		return f"({self.lexpr} {self.op.text} {self.rexpr})"

	def gen_code(self, tabs=0):
		return f"{''    ''*tabs}{self.lexpr.gen_code()}{self.op}{self.rexpr.gen_code()}"

class BitwiseBinExpr:
	def __init__(self, lexpr, op, rexpr):
		self.lexpr = lexpr
		self.op = op
		self.rexpr = rexpr
	
	def substitute(self, type_map):
		self.lexpr.substitute(type_map)
		self.rexpr.substitute(type_map)
	
	def check_types(self, scopes):
		self.lexpr.check_types(scopes)
		self.rexpr.check_types(scopes)

		if self.lexpr.type_info != TypeInfo("uint"):
			raise TypeError(f"Invalid type for operator {self.op}: {self.lexpr}")

		if self.rexpr.type_info != TypeInfo("uint"):
			raise TypeError(f"Invalid type for operator {self.op}: {self.rexpr}")
		
		self.type_info = TypeInfo("uint")

	def __repr__(self):
		return f"({self.lexpr} {self.op} {self.rexpr})"
	
	def gen_code(self, tabs=0):
		return f"{'    '*tabs}{self.lexpr.gen_code()}{self.op}{self.rexpr.gen_code()}"


class LogicalUnaryExpr:
	def __init__(self, op, rexpr):
		self.op = op
		self.rexpr = rexpr
	
	def substitute(self, type_map):
		self.rexpr.substitute(type_map)
	
	def check_types(self, scopes):
		self.rexpr.check_types(scopes)
		
		if self.rexpr.type_info != TypeInfo("boolean"):
			raise TypeError(f"Invalid type for operator {self.op}: {self.rexpr}")
		
		self.type_info = TypeInfo("boolean")
	
	def __repr__(self):
		return f"({self.op.text}{self.rexpr})"


	def gen_code(self, tabs=0):
		return f"{'    '*tabs}{self.op}{self.rexpr.gen_code()}"

class MathUnaryExpr:
	def __init__(self, op, rexpr):
		self.op = op
		self.rexpr = rexpr

	def substitute(self, type_map):
		self.rexpr.substitute(type_map)
	
	def check_types(self, scopes):
		self.rexpr = rexpr

		if not self.rexpr.type_info.is_numeric():
			raise TypeError(f"Invalid type {self.rexpr.type_info} \
					for operator {self.op}: {self.rexpr}")
		
		self.type_info = self.rexpr.type_info

	
	def __repr__(self):
		return f"({self.op.text}{self.rexpr})"

	def gen_code(self, tabs=0):
		return f"{''    ''*tabs}{self.op}{self.rexpr.gen_code()}"

class PointerUnaryExpr:
	def __init__(self, op, rexpr):
		self.op = op
		self.rexpr = rexpr

	def substitute(self, type_map):
		self.rexpr.substitute(type_map)
	
	def check_types(self, scopes):
		self.rexpr.check_types(scopes)
		
		if self.op.type == T.STAR:
			if self.rexpr.type_info.indirection < 1:
				raise TypeError(f"{self.rexpr} cannot be dereferenced")
			self.type_info = self.rexpr.type_info.deref()
		elif self.op.type == T.AMPERSAND:
			self.type_info = self.rexpr.type_info.addr_of()

		else:
			raise Exception()

	def __repr__(self):
		return f"({self.op.text}{self.rexpr})"

	def gen_code(self, tabs=0):
		return f"{'    '*tabs}{self.op}{self.rexpr.gen_code()}"


class BitwiseUnaryExpr:
	def __init__(self, op, rexpr):
		self.op = op
		self.rexpr = rexpr
	
	def substitute(self, type_map):
		self.rexpr.substitute(type_map)
	
	def check_types(self, scopes):
		self.rexpr.check_types(scopes)
		
		if self.rexpr.type_info != TypeInfo("uint"):
			raise TypeError(f"Invalid type for operator {self.op}: {self.rexpr}")

	def __repr__(self):
		return f"({self.op.text}{self.rexpr})"

	def gen_code(self, tabs=0):
		return f"{'    '*tabs}{self.op}{self.rexpr.gen_code()}"

class LiteralExpr:
	def __init__(self, tok, type_name):
		self.tok = tok
		self.type_info = TypeInfo(type_name)
	
	
	def substitute(self, type_map):
		pass

	def check_types(self, scopes):
		pass
	
	def __repr__(self):
		return f"{self.tok.text}"
	
	def gen_code(self):
		print("LITERAL GEN", self.tok.text)
		return self.tok.text

class VariableExpr:
	def __init__(self, tok):
		self.tok = tok
	
	def substiute(self, type_map):
		pass

	def check_types(self, scopes):
		self.type_info = scopes[self.tok.text]

	def __repr__(self):
		return f"Variable({self.tok.text})"

	def gen_code(self, tabs=0):
		return f"{'    '*tabs}{self.tok.text}"

class ArrayAccessExpr:
	def __init__(self, expr, idx):
		self.expr = expr
		self.idx = idx
	
	def substitute(self, type_map):
		self.expr.substitute(type_map)
		
	def check_types(self, scopes):
		self.expr.check_types(scopes)
		if self.expr.type_info.arr == ():
			raise TypeError(f"Cannot index type {self.expr.type_info}")

		self.type_info = TypeInfo(self.expr.type_info.name, \
							self.expr.type_info.indirection, \
							arr=self.expr.type_info.arr[:-1])

	
	def gen_code(self, tabs=0):
		print(self.idx.gen_code)
		return f"{self.expr.gen_code()}[{self.idx.gen_code()}]"
		

	def __repr__(self):
		return f"ArrayAccess({self.expr}{self.idx})"

class CompoundLiteralExpr:
	def __init__(self, type_info, members):
		self.type_info = type_info
		self.members = members
	
	def substitute(self, type_map):
		self.type_info = type_map[self.type_info]
	
	def check_types(self, scopes):
		for m in self.members:
			m.check_types(scopes)
		
		if self.type_info.indirection or self.type_info.arr != ():
			raise TypeError(f"Can't make literal of type {self.type_info}")
		
		if self.type_info.name in scopes.root.generic_types:
			print(self.type_info)
			return
		else:
			defn_members = scopes.root.concrete_types[self.type_info.name].members
			print(defn_members)

		if len(self.members) != len(defn_members):
			raise TypeError("Invalid number of members for type {self.type_info}")
	


	def __repr__(self):
		return f"CompoundLiteral({self.type_info.name}: {self.members})"
	
	def gen_code(self):
		mems = [m.gen_code() for m in self.members]
		return f"{{ {','.join(mems)} }}"


class ArrayLiteralExpr:
	def __init__(self, type_info, members):
		self.dec_type_info = type_info
		self.members = members

	def substitute(self, type_map):
		self.type_info = type_map[self.type_info]
	
	def check_types(self, scopes):
		for m in self.members:
			m.check_types(scopes)
			if m.type_info != self.dec_type_info:
				raise TypeError(f"Wrong type for array literal in expr {m}")

		name = self.dec_type_info.name
		arr = *self.dec_type_info.arr, -1

		self.type_info = TypeInfo(name, arr=arr)


		
	
	def _repr__(self):
		return f"ArrayLiteral({self.type_info.name}: {self.members})"

	def gen_code(self):
		mems = [m.gen_code() for m in self.members]
		return f"{{ {','.join(mems)} }}"

class FunctionCallExpr:
	def __init__(self, func_defn, args):
		self.func_defn = func_defn
		self.args = args
	
	def substitute(self, type_map):
		for a in self.args:
			a.substitute(type_map)
	
	def check_types(self, scopes):
		for a in self.args:
			a.check_types(scopes)
		


class GenericFunctionCallExpr:
	def __init__(self, func_defn, gen_args, args):
		self.func_defn = func_defn
		self.gen_args = gen_args
		self.args = args
	
	def substitute(self, type_map):
		self.gen_args = [type_map[ga] for ga in self.gen_args]
		for a in self.args:
			a.substitute(type_map)

	def check_types(self, scopes):
		for a in self.args:
			a.check_types(scopes)









