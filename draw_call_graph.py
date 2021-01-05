import re
from graphviz import Digraph
import sys


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
    source_file = sys.argv[1]
    draw_call_graph(source_file)
