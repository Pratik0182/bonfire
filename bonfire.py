################### Libs ###################
from string_with_arrows import * 

################### Constants
DIGITS = set([str(i) for i in range(10)]) #123456789

################### Error Handling ###################
class Error:
	def __init__(self, error_start, error_end, error_type, details):
		self.error_start = error_start
		self.error_end = error_end
		self.error_type = error_type
		self.details = details
	
	def res(self):
		result = f'File {self.error_start}, line {self.error_start.ln + 1}\n'
		result += string_with_arrows(self.error_start.ftxt, self.error_start, self.error_end) + '\n'
		result += f'{self.error_type} {self.details}\n'
		return result

class IllegalCharError(Error):
	def __init__(self, error_start, error_end, details):
		super().__init__(error_start, error_end, 'Going more Hollow: ', details)
		'''
		might change this later

		'''
class InvalidSyntaxError(Error):
	def __init__(self, error_start, error_end, details):
		super().__init__(error_start, error_end, 'Bonfire not found: ', details)
		'''
		sochunga

		'''
class RunTimeError(Error):
	def __init__(self, error_start, error_end, details):
		super().__init__(error_start, error_end, 'Bonfire Exhausted: ', details)

################### Position ###################
class Position:
	def __init__(self, idx, ln, col, fn, ftxt):
		self.idx = idx
		self.ln = ln 
		self.col = col
		self.fn = fn
		self.ftxt = ftxt
		'''
	 	  0    1    2    3
		0  acha aisa hain kya
		1  mujhe nhi pta tha
		2  ohh acha aisa 
		'''
	def advance(self, cchar = None):
		self.idx += 1
		self.col += 1
		if cchar == '\n': #idx inc, col reset
			self.ln += 1
			self.col = 0
		return self
	
	def copy(self):
		return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)


################### Tokens ###################
T_INT = 'INT'
T_FLOAT = 'FLOAT'
T_ADD = 'ADD'
T_SUB = 'SUB'
T_MUL = 'MUL'
T_DIV = 'DIV'
T_LPAR = 'LPAR'
T_RPAR = 'RPAR'
T_EOF = 'EOF'

class Token:
	def __init__(self, type_, value = None, pos_start = None, pos_end = None):
		self.type = type_
		self.value = value
		if pos_start:
			self.pos_start = pos_start.copy()
			self.pos_end = pos_end.copy() if pos_end else pos_start.copy()
			self.pos_end.advance()

	def __repr__(self):
		if self.value:
			return f'{self.type} : {self.value}'
		return f'{self.type}'

################### LEXER ###################
class Lexer:
	def __init__(self, fn, text):
		self.fn = fn
		self.text = text
		self.pos = Position(-1, 0, -1, fn, text)
		self.cchar = None
		self.advance()

	def advance(self):
		self.pos.advance(self.cchar)
		self.cchar = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

	def dev_token(self):
		tokens = []
		while self.cchar != None:
			if self.cchar in set([' ', '\t']):
				self.advance()
			elif self.cchar in DIGITS:
				tokens.append(self.dev_number())
			elif self.cchar == '+':
				tokens.append(Token(T_ADD, pos_start = self.pos))
				self.advance()
			elif self.cchar == '-':
				tokens.append(Token(T_SUB, pos_start = self.pos))
				self.advance()
			elif self.cchar == '*':
				tokens.append(Token(T_MUL, pos_start = self.pos))
				self.advance()
			elif self.cchar == '/':
				tokens.append(Token(T_DIV, pos_start = self.pos))
				self.advance()
			elif self.cchar == ')':
				tokens.append(Token(T_RPAR, pos_start = self.pos))
				self.advance()
			elif self.cchar == '(':
				tokens.append(Token(T_LPAR, pos_start = self.pos))	
				self.advance()
			else:
				pos_start = self.pos.copy()
				curr = self.cchar
				self.advance()
				return [], IllegalCharError(pos_start, self.pos, curr)
			'''
			self.advance()
			'''
		tokens.append(Token(T_EOF, pos_start = self.pos))
		return tokens, None

	def dev_number(self):
		N = []
		dot = 0
		pos_start = self.pos.copy()
		while self.cchar != None and (self.cchar in DIGITS or self.cchar == '.'):
			if self.cchar == '.':
				if dot == 1: break
				dot += 1
				N.append('.') 
			else:
				N.append(self.cchar)
			self.advance()
		return Token(T_FLOAT, float(''.join(N)), pos_start, self.pos) if dot else Token(T_INT, int(''.join(N)), pos_start, self.pos)
		# 2.004 or 2;;;;  0 and 1 dots

################### Nodes ###################
class NumberNode:
	def __init__(self, tok):
		self.tok = tok
		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end
	
	def __repr__(self):
		return f'{self.tok}'

class BinOpNode:
	def __init__(self, l_node, op_tok, r_node):
		self.l_node = l_node
		self.op_tok = op_tok
		self.r_node = r_node
		self.pos_start = self.l_node.pos_start
		self.pos_end = self.r_node.pos_end

	def __repr__(self):
		return f'({self.l_node}, {self.op_tok}, {self.r_node})'
class UnaryOpNode:
	def __init__(self, op_tok, node):
		self.op_tok = op_tok
		self.node = node
		self.pos_start = self.op_tok.pos_start
		self.pos_end = self.node.pos_end
	def __repr__(self):
		return f'({self.op_tok}, {self.node})'

################### Parse Resutl ###################
class ParseResult:
	def __init__(self):
		self.error = None
		self.node = None
	
	def register(self, res):
		if isinstance(res, ParseResult):
			if res.error:
				self.error = res.error
			return res.node
		return res
	
	def success(self, node):
		self.node = node
		return self
	
	def failure(self, error):
		self.error = error
		return self
################### Parser ###################
class Parser:
	def __init__(self, tokens):
		self.tokens = tokens
		self.tok_idx = -1
		self.advance()
	
	def advance(self):
		self.tok_idx += 1
		if self.tok_idx < len(self.tokens):
			self.curr_tok = self.tokens[self.tok_idx]
		return self.curr_tok
	

######################################
	def parse(self):
		res = self.expr()
		if not res.error and self.curr_tok.type != T_EOF:
			return res.failure(InvalidSyntaxError(self.curr_tok.pos_start, self.curr_tok.pos_end, "Expected '+', '-', '*' or '/'"))
		return res

	def factor(self):
		res = ParseResult()
		tok = self.curr_tok
		if tok.type in (T_ADD, T_SUB):
			res.register(self.advance())
			factor = res.register(self.factor())
			if res.error:
				return res
			return res.success(UnaryOpNode(tok, factor))
		elif tok.type == T_LPAR:
			res.register(self.advance())
			expr = res.register(self.expr())
			if res.error:
				return res
			if self.curr_tok.type == T_RPAR:
				res.register(self.advance())
				return res.success(expr)
			else:
				return res.failure(InvalidSyntaxError(self.curr_tok.pos_start, self.curr_tok.pos_end, "Expected ')'"))
		elif tok.type in (T_INT, T_FLOAT):
			res.register(self.advance())
			return res.success(NumberNode(tok))
		return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_start, "Expected int or float"))

	def term(self):
		return self.bin_op(self.factor, (T_MUL, T_DIV))

	def expr(self):
		return self.bin_op(self.term, (T_ADD, T_SUB))

	def bin_op(self, func, ops):
		res = ParseResult()
		left = res.register(func())
		if res.error:
			return res
		while self.curr_tok.type in ops:
			op_tok = self.curr_tok
			res.register(self.advance())
			right = res.register(func())
			if res.error:
				return res
			left = BinOpNode(left, op_tok, right)
		return res.success(left) 
################### RUNTIME RESULT ###################
class RunTimeResult:
	def __init__(self):
		self.value = None
		self.error = None

	def register(self, res):
		if isinstance(res, RunTimeResult):
			if res.error:
				self.error = res.error
			return res.value
		return res

	def success(self, value):
		self.value = value
		return self

	def failure(self, error):
		self.error = error
		return self

################### VALUES ###################
class Value:
	def __init__(self, value):
		self.value = value
		self.set_pos()
	
	def set_pos(self, pos_start = None, pos_end = None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self
	
	def added_to(self, other):
		if isinstance(other, Value):
			return Number(self.value + other.value), None
	
	def subted_by(self, other):
		if isinstance(other, Value):
			return Number(self.value - other.value), None

	def mul_by(self, other):
		if isinstance(other, Value):
			return Number(self.value * other.value), None
	def div_by(self, other):
		if isinstance(other, Value):
			if other.value == 0:
				return None, RunTimeError(other.pos_start, other.pos_end, "Division by zero")
			return Number(self.value / other.value), None
	
	def __repr__(self):
		return str(self.value)
def Number(value):
	return Value(value)
################### INTERPRETER ###################
class Interpreter:
	def visit(self, node):
		method_name = f'visit_{type(node).__name__}'
		method = getattr(self, method_name, self.no_visit_method)
		return method(node)
	
	def no_visit_method(self, node):
		raise Exception(f'No visit_{type(node).__name__} method defined ')
	
	def visit_NumberNode(self, node):
		
		return RunTimeResult().success(Number(node.tok.value).set_pos(node.tok.pos_start, node.tok.pos_end))

	def visit_BinOpNode(self, node):
		res = RunTimeResult()
		left = res.register(self.visit(node.l_node))
		right = res.register(self.visit(node.r_node))
		if res.error:
			return res
		if node.op_tok.type == T_ADD:
			result, error = left.added_to(right)
		elif node.op_tok.type == T_SUB:
			result, error = left.subted_by(right)
		elif node.op_tok.type == T_MUL:
			result, error = left.mul_by(right)
		elif node.op_tok.type == T_DIV:
			result, error = left.div_by(right)
		if error:
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end))

	def visit_UnaryOpNode(self, node):
		res = RunTimeResult()
		number = res.register(self.visit(node.node))
		if res.error:
			return res
		error = None
		if node.op_tok.type == T_SUB:
			number, error = number.mul_by(Number(-1))
		if error:
			return res.failure(error)
		else:
			return res.success(number.set_pos(node.pos_start, node.pos_end))
################### RUN ###################
def run(fn, text):
	#generate tokens
	lexer = Lexer(fn, text)
	tokens, error = lexer.dev_token()
	if error:
		return None, error
	#generate ast
	parser = Parser(tokens)
	ast = parser.parse()
	if ast.error:
		return None, ast.error
	#run interpreter
	interpreter = Interpreter()
	result = interpreter.visit(ast.node)
	return result.value, result.error

