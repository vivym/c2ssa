'''
use this .py file in command line like: python local2global.py t6.ssa.c
'''

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
    # 处理ssa文件
    while i < n:
        # 处理 FUNC 块
        if re.search(r"/\*{11}FUNC_BEGIN\*{16}/", text[i]):
            localv = []
            newv = []
            exist_localv = False
            # 提取函数名
            i += 1
            search_func = re.search(r"^\s*(.*)\s+(.*)\((.*)\)\s*\{", text[i])
            while not search_func:
                i += 1
                search_func = re.search(r"^\s*(.*)\s+(.*)\((.*)\)\s*\{", text[i])
            func = search_func.group(2)
            i += 1
            while not re.search(r"/\*{12}FUNC_END\*{17}/", text[i]):
                if exist_localv and localv:
                    for v in range(len(localv)):
                        text[i] = re.sub(localv[v], newv[v], text[i])

                # 处理 LOCAL_VAR 块
                if re.search(r"/\*{11}LOCAL_VAR_BEGIN\*{11}/", text[i]) and not exist_localv:
                    text.pop(i)
                    n -= 1
                    while not re.search(r"/\*{12}LOCAL_VAR_END\*{12}/", text[i]):
                        search_localv = re.search(r"^\s*(.*)\s+(.*);", text[i])
                        if search_localv:
                            s = search_localv.group(2).strip()
                            localv.append(s)
                            pos = re.search(r"c2ssa", s).span()[1]
                            s1 = s[0:pos]
                            s2 = s[pos:len(s)]
                            newv.append(s1 + '_'+func + s2)
                            pos = re.search(r"c2ssa", text[i]).span()[1]
                            s1 = text[i][0:pos]
                            s2 = text[i][pos:len(text[i])]
                            globalv.append((s1+ '_'+func + s2).lstrip())
                            text.pop(i)
                            n -= 1
                        else:
                            i += 1
                    text.pop(i)
                    n -= 1
                    while re.search(r"^\s*$", text[i]):
                        text.pop(i)
                        n -= 1
                    exist_localv = True
                else:
                    i += 1 # 没搜索到 LOCAL_VAR_BEGIN ，接着检查下一行
        i += 1 # 一个 FUNC 块已经处理完毕，接着找下一个 FUNC_BEGIN


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