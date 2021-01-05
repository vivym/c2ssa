import os
import argparse
import re

text = []
globalv = []

def insertGlobalv():
    global text
    global globalv
    n = len(text)
    for i in range(n):
        if re.search(r"/\* Global Declarations \*/", text[i]):
            line = i+1
            for j in range(len(globalv)):
                text.insert(line+j, globalv[j])
            break

def removeLocalv():
    global text
    global globalv
    n = len(text)
    i = 0
    while i < n:
        if re.search(r"/\*{11}FUNC_BEGIN\*{16}/", text[i]):
            i += 1
            searchFunc = re.search(r"^\s*(.*)\s+(.*)\((.*)\)\s*\{", text[i])
            while not searchFunc:
                i += 1
                searchFunc = re.search(r"^\s*(.*)\s+(.*)\((.*)\)\s*\{", text[i])
            func = searchFunc.group(2)
            print(func)
            i += 1
            while not re.search(r"/\*{12}FUNC_END\*{17}/", text[i]):
                print(text[i])
                if re.search(r"/\*{11}LOCAL_VAR_BEGIN\*{11}/", text[i]):
                    text.pop(i)
                    n -= 1
                    while not re.search(r"/\*{12}LOCAL_VAR_END\*{12}/", text[i]):
                        if re.search(r"^\s*(.*)\s+(.*);", text[i]):
                            pos = re.search(r"c2ssa", text[i]).span()[1]
                            s1 = text[i][0:pos]
                            s2 = text[i][pos:len(text[i])]
                            globalv.append((s1+ '_' + func + s2).lstrip())
                            text.pop(i)
                            n -= 1
                        else:
                            i += 1
                    text.pop(i)
                    n -= 1
                    while re.search(r"^\s*$", text[i]):
                        text.pop(i)
                        n -= 1
                else:
                    i += 1 # 没搜索到 LOCAL_VAR_BEGIN ，接着检查下一行
        i += 1 # 找下一个 FUNC_BEGIN


def scanText(src):
    global text
    with open(src, "r", encoding="utf-8") as f:
        text = f.readlines()

    removeLocalv()
    insertGlobalv()

    if os.path.exists(src+'.bak'):
        os.remove(src+'.bak')
    os.rename(src, src+'.bak')

    with open(src, "w", encoding="utf-8") as f:
        f.writelines(text)


def main(args):
    src = args.src

    scanText(src)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("src", type=str, help="ssa source file.")

    main(parser.parse_args())