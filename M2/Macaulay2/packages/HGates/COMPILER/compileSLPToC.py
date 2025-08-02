from lex import *

def main():
    source = """1 => (HMatrixGate, {x, y, x, 2}, 4),
            2 => (DetHMatrixGate, (1)),
            3 => (HMatrixGate, {1, 2}, 2),
            4 => (SumHMatrixGate, (3))
            """
