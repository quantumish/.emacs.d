from sympy.parsing.latex import parse_latex
from sympy import *

def clean_line(line):
    line = line.replace("\\\\", "")
    cleaned = line.strip()
    if cleaned[:2] == "&=":
        cleaned = cleaned[2:]
    return cleaned

while True:
    inp = input()
    split = inp.index(" ")
    cmd, arg = inp[:split], clean_line(inp[split:])
    if cmd == "diff":
        expr = parse_latex(arg)
        print(latex(expr.diff()))
    elif cmd == "int":
        expr = parse_latex(arg)
        print(latex(expr.integrate()))
    elif cmd == "simp":
        expr = parse_latex(arg)
        print(latex(expr.simplify()))
    elif cmd == "doit":
        expr = parse_latex(arg)
        print(latex(expr.doit()))        
    elif cmd == "check":
        orig = parse_latex(clean_line(input()))
        eq = True
        for i in range(int(arg)-1):
            expr = parse_latex(clean_line(input()))
            eq = orig.equals(expr)
            if not eq:
                print(i+1)
                break
        else:            
            print("t")

# example = """
# \\int \\frac{4\\sec\\theta \\tan \\theta }{16\\sec^2\\theta \\sqrt{16\\sec^2\\theta-16}} d\\theta &= \\int \\frac{4\\sec\\theta \\tan \\theta }{16\\sec^2\\theta \\sqrt{16(\\sec^2\\theta-1)}}d\\theta\\\\
# &= \\int \\frac{4\\sec\\theta \\tan \\theta }{16\\sec^2\\theta \\sqrt{16\\tan^2\\theta}}d\\theta\\\\
# &= \\int \\frac{4\\sec\\theta \\tan \\theta }{16\\sec^2\\theta \\cdot 4 \\tan \\theta }d\\theta\\\\
# &= \\int \\frac{\\sec\\theta }{16\\sec^2\\theta }d\\theta\\\\
# &= \\int \\frac{1}{16\\sec \\theta }d\\theta\\\\
# &= \\frac{1}{16} \\int \\frac{1}{\\sec \\theta }d\\theta\\\\
# &= \\frac{1}{16} \\int \\cos \\theta d\\theta\\\\
# &= \\frac{\\sin \\theta }{16} + C
# """

# ex2 = """
# &= 4k_1+2 + 6k_2 + 3 \\\\
# &= 4k_1+2 + 6k_2 + 2 + 1 \\\\
# &= 2(2k_1 + 3k_2 + 2) + 1
# """

# lines = ex2.split("\\\\")
# lhs = parse_latex(clean_line(lines[0]))
# # rhs = parse_latex(lines[0]).rhs
# # print(lhs, "\n", rhs)
# # assert(lhs.equals(rhs))

# for line in lines[1:]:
#     cleaned = clean_line(line)
#     expr = parse_latex(cleaned)
#     # print(expr)
#     assert(lhs.equals(expr))


    
