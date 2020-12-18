from sys import stdin

def tokenize(s):
    return s.replace('(', '( ').replace(')', ' )').split()

def parse_expr(toks, i):
    elts = []
    while i < len(toks):
        if toks[i] == '(':
            xs, i = parse_expr(toks, i+1)
            elts.append(xs)
        elif toks[i] == ')':
            return elts, i+1
        elif toks[i] in '0123456789':
            elts.append(int(toks[i]))
            i += 1
        elif toks[i] in ('+', '*'):
            elts.append(toks[i])
            i += 1
    return elts

def apply_op(op, x, y):
    if op == '+':
        return x + y
    elif op == '*':
        return x * y
    else:
        assert False

def eval_expr_a(expr):
    if isinstance(expr, int):
        return expr
    acc = eval_expr_a(expr[0])
    i = 1
    while i < len(expr):
        op = expr[i]
        operand = expr[i+1]
        operand = eval_expr_a(operand)
        acc = apply_op(op, acc, operand)
        i += 2
    return acc

def eval_expr_b(expr):
    if isinstance(expr, int):
        return expr
    acc = eval_expr_b(expr[0])
    mul_acc = []
    i = 1
    while i < len(expr):
        op = expr[i]
        operand = expr[i+1]
        operand = eval_expr_b(operand)
        if op == '*':
            if len(mul_acc) > 0:
                acc *= mul_acc.pop()
            mul_acc.append(operand)
        elif op == '+':
            if len(mul_acc) > 0:
                mul_acc[0] += operand
            else:
                acc += operand
        i += 2
    if len(mul_acc) > 0:
        acc *= mul_acc.pop()
    return acc

exprs = [tokenize(line.strip()) for line in stdin]
print(sum(eval_expr_a(parse_expr(expr, 0)) for expr in exprs))
print(sum(eval_expr_b(parse_expr(expr, 0)) for expr in exprs))
