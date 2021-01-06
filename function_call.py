import networkx as nx
import re
import argparse

FUNCTION_VAR = {} #function新的内容列表
functions = {} #储存原始的function的内容
OUTPUT = []

#构造新的function输出列表
def function_call(seq, func_name):
    func_names = func_name + seq
    funcline = functions[func_name]["func_content"]

    # 如果这个function没有其他实体内容，比如rand
    if len(funcline) == 0:
        return

    if func_names not in FUNCTION_VAR:
        FUNCTION_VAR[func_names] = []
    for id in range(len(funcline)):
        line = funcline[id]
        words = re.split('/', line)
        new_seq = ""
        func_name1 = ""
        for lens in range(len(words)):
            if re.match(r"\**CALL\**", words[lens]) and re.match(r".*printf.*", line) == None and re.match(r".*puts.*", line) == None and lens > 0:
                # 判断CALL
                func = words[lens - 1].replace(" ", "").split('=')
                variable = func[0]
                func = re.split('\+|\-|\*|\/|\^', func[1])
                for i in range(len(func)):
                    function = re.split(' |\(|\)|,', func[i])
                    if len(function) > 0 and len(functions[function[0]]["func_content"]) > 0:
                        func_name1 = function[0]
                        new_seq = seq + '_' + variable.split('_', )[-1]
                        function_call(new_seq, func_name1)
                # print(variable, func_name, func_var)
        if id == 0:
            new_line = line.replace(func_name, func_names)
            FUNCTION_VAR[func_names].append(new_line)
        elif func_name1 == "":
            FUNCTION_VAR[func_names].append(line)
        else:
            new_line = line.replace(func_name1, func_name1 + new_seq)
            FUNCTION_VAR[func_names].append(new_line)
    return
    funcpd = 0
    word_list = [] #要被替换的参数
    new_word_list  = [] #新的参数
    for id in range(len(funcline)):
        line = funcline[id]
        if id == 0: #function的第一行
            words = line.split('(', 1)[1].split(')', 1)[0]
            word = words.split(',')
            for i in range(len(word)): #函数传入的参数
                var = word[i] + seq + " = " + func_var[i] + ";"
                word_list.append(word[i].split(' ', )[-1])
                new_word_list.append(word[i].split(' ', )[-1] + seq)
            function_description = line.split('(', 1)[0] + seq +"() {"
            FUNCTION_VAR[func_names].append(function_description)
        elif line.replace(" ", "") == "/***********LOCAL_VAR_BEGIN***********/":
            funcpd = 1
        elif line.replace(" ", "") == "/************LOCAL_VAR_END************/":
            funcpd = 0
        elif funcpd == 1: #这是LOCAL_VAR内的内容
            words = line.split(';', 1)[0]
            word_list.append(words.split(' ', )[-1])
            new_word_list.append(words.split(' ', )[-1] + seq)
            var = words + seq + ";"
        else: #这是非LOCAL_VAR内的内容
            new_line = line
            words = re.split('/', line)
            head_word = line.replace(" ", "").split('(', 1)
            for lens in range(len(words)):
                if words[lens] == "******CALL******" and head_word[0] != "printf" and head_word[0] != "puts" and lens > 0:
                    #判断CALL
                    func = words[lens - 1].replace(" ", "").split('=')
                    variable = func[0]
                    function = re.split(' |\(|\)|,', func[1])
                    func_name = function[0]
                    func_var = []
                    for i in range((len(function) - 1)):
                        if function[i + 1] != "":
                            func_var.append(function[i + 1])
                    new_seq = seq + "_" + variable.split('_', )[-1]
                    function_call(new_seq, func_name)
                    #print(variable, func_name, func_var)
                    new_line = new_line.replace(func_name, func_name + new_seq)

            for j in range(len(word_list)):
                new_line = new_line.replace(word_list[j],new_word_list[j])
            FUNCTION_VAR[func_names].append(new_line)


def Graph(lines):
    #有向图其实没用到。。。但是用来其中部分的变量参数
    DG = nx.DiGraph()
    #构建有向图节点
    funcpd = 0
    for line in lines:
        regex = r"/\* Function Declarations \*/"
        m = re.match(regex, line)
        #print(m)
        if re.match(r"/\* Function Declarations \*/", line):
            funcpd = 1
        elif re.match(r"/\* Global Variable Definitions and Initialization \*/", line):
            break
        elif funcpd == 1:
            func_name = line.split(' ', 1)
            if len(func_name) >= 2:
                func_name = func_name[1].split('(', 1)[0]
            else:
                func_name = ""
            if func_name != "":
                DG.add_node(func_name)
                if func_name not in functions:
                    functions[func_name] = {}
                if "func_content" not in functions[func_name]:
                    functions[func_name]["func_content"] = []
    #print(DG.nodes.data())

    #构建有向边
    funcpd = 0 #判断是否在一个func中
    head_func_name = ""
    for id in range(len(lines)):
        line = lines[id]
        if re.match(r"/\**FUNC_BEGIN\**/", line):
            funcpd = 1
            line2 = lines[id+1]
            head_func_name = line2.split(' ', 1)[1]
            head_func_name = head_func_name.split('(', 1)[0]
        elif re.match(r"/\**FUNC_END\**/", line):
            funcpd = 0
        elif funcpd == 1:
            functions[head_func_name]["func_content"].append(line)
            words = re.split('/', line)
            for lens in range(len(words)):
                if re.match(r"\**CALL\**", words[lens]) and lens > 0 and re.match(r".*printf.*", line) == None and re.match(r".*puts.*", line) == None:
                    func = words[lens - 1].replace(" ","").split('=')
                    func = re.split('\+|\-|\*|\/|\^', func[1])
                    for i in range(len(func)):
                        function = re.split(' |\(|\)|,', func[i])
                        if len(function) > 0 and len(functions[function[0]]["func_content"]) > 0:
                            func_name = function[0]
                            if DG.has_edge(head_func_name, func_name):
                                num = DG[head_func_name][func_name]['num'] + 1
                                DG.add_edge(head_func_name, func_name, num=num)
                            else:
                                DG.add_edge(head_func_name, func_name, num=1)
        else:
            OUTPUT.append(line)

    #print(list(DG.edges))
    #print(DG["main"]["sum"]['num'])
    #print(functions["main"]["func_content"])

#处理main函数内的内容
def work3(lines, src):
    mainfunc_line = functions["main"]["func_content"]
    if "main" not in FUNCTION_VAR:
        FUNCTION_VAR["main"] = []
    for id in range(len(mainfunc_line)):
        line = mainfunc_line[id]
        words = re.split('/', line)
        seq = "_"
        new_seq = ""
        func_name = ""
        for lens in range(len(words)):
            if re.match(r"\**CALL\**", words[lens]) and re.match(r".*printf.*", line) == None and re.match(r".*puts.*", line) == None and lens > 0:
                # 判断CALL
                #print(line)
                func = words[lens - 1].replace(" ", "").split('=')
                variable = func[0]
                func = re.split('\+|\-|\*|\/|\^', func[1])
                for i in range(len(func)):
                    function = re.split(' |\(|\)|,', func[i])
                    if len(function) > 0:
                        func_name = function[0]
                        new_seq = seq + variable.split('_', )[-1]
                        function_call(new_seq, func_name)
                # print(variable, func_name, func_var)
        if func_name == "":
            FUNCTION_VAR["main"].append(line)
        else:
            new_line = line.replace(func_name, func_name + new_seq)
            FUNCTION_VAR["main"].append(new_line)

    keys = list(FUNCTION_VAR.keys())
    lens = len(keys)
    for i in range(lens):
        begin = "/************FUNC_BEGIN*****************/\n"
        OUTPUT.append(begin)
        key = keys[lens-i-1]
        for var in FUNCTION_VAR[key]:
            OUTPUT.append(var)
        end = "/************FUNC_END*****************/\n\n"
        OUTPUT.append(end)

    with open(src, "w", encoding="utf-8") as f:
        f.writelines(OUTPUT)

def main(args):
    src = args.src
    with open(src, "r", encoding="utf-8") as f:
        text = f.readlines()

    Graph(text)
    work3(text, src)


#这部分是main函数,处理main函数内的内容
if __name__=='__main__':
    """
    file = open("t6.ssa.c")
    lines = []
    for line in file:
        #lines.append(line.replace("\t", " ").replace("\n", " "))
        lines.append(line)
    print(lines)
    file.close()
    """

    #parser = argparse.ArgumentParser()
    #parser.add_argument("src", type=str, help="ssa source file.")

    #Graph(parser.parse_args())

    parser = argparse.ArgumentParser()
    parser.add_argument("src", type=str, help="ssa source file.")
    main(parser.parse_args())


    for out in OUTPUT:
        print(out)
#print(LOCAL_VAR)
#print(FUNCTION_VAR)
