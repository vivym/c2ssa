'''
use this .py file in command line like: python draw_call_graph.py t6.ssa.c
please install graphviz
'''

import re
from graphviz import Digraph
import argparse


def draw_call_graph(source_file):
    regex = r"\/\*\* Call Graph[\s\S]*?\*\*\/"
    print(">>>>>>>> source file is ", source_file)
    with open(source_file, 'r') as f:
        tem = f.readlines()
    source_code = ''
    for i in tem:
        source_code += i
    m = re.match(regex, source_code)
    call_code = m.group(0)
    call_code = call_code.split('\n')
    call_code = call_code[1:-1]
    call_code_dict = {}
    for call in call_code:
        call_code_dict[call] = 0
    for call in call_code:
        call_code_dict[call] = call_code_dict[call] + 1

    g = Digraph('G', filename=source_file + '.call_graph.gv')
    g.attr(rankdir='LR', size='8,5')
    g.attr('node', shape='circle')
    for call in call_code_dict:
        edge = call.split('\tcalled by\t')
        g.edge(edge[1], edge[0], label=str(call_code_dict[call]))
    g.view()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("src", type=str, help="ssa source file.")
    arg = parser.parse_args()
    draw_call_graph(arg.src)
