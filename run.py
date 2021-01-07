from typing import List
import re
import os
import sys
import subprocess
import argparse
import tempfile
import networkx as nx


def get_include_paths(source_filename: str) -> List[str]:
    ret = subprocess.run(
        ["gcc", "-v", "-o", "/dev/null", source_filename],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT
    )
    ret = ret.stdout.decode("utf-8")

    m = re.search("#include <...> search starts here:([\s\S]*)End of search list.", ret)
    if m is None:
        print("could not find clang or gcc.")
        sys.exit(-1)
    paths = []
    for l in m.group(1).splitlines(False):
        idx = l.find("(")
        if idx >= 0:
            l = l[:idx]
        l = l.strip()
        if len(l) > 0:
            paths.append(l)
    
    return paths


class Pass(object):
    def __init__(self, name):
        self.name = name

    def run(self, code: str) -> str:
        raise NotImplementedError

    def check_dependencies(self) -> bool:
        return True


class PassManager(object):
    def __init__(self):
        self.passes = []

    def add(self, p: Pass):
        self.passes.append(p)
        return self

    def check_dependencies(self):
        for p in self.passes:
            if not p.check_dependencies():
                return False

        return True

    def run(self, code: str) -> str:
        for p in self.passes:
            try:
                code = p.run(code)
            except Exception as e:
                print("failed on:", p.name)
                raise e
        return code

class VerifyPass(Pass):
    def __init__(self, src):
        super().__init__("verify")

        self.src = src

    def run(self, code: str) -> str:
        lines = code.split("\n")
        new_lines = []

        if not any("<stdio.h>" in l for l in lines):
            new_lines.append("#include <stdio.h>")
            new_lines.append("")
        
        for l in lines:
            new_lines.append(l)
            indent = re.match(r"^(\s*)", l).group(1)
            m1 = re.match(r"^(\s*)(\S+)\s*((=)|(\+=)|(\-=)|(\*=)|(\/=)|(\%=)|(\~=)|(\^=)|(\&=)|(\+\+)|(\-\-))", l)
            m2 = re.match(r"^(\s*)((\+\+)|(\-\-))\s*(\S+)", l)
            if m1 is not None:
                var = m1.group(2).strip().strip(";")
            elif m2 is not None:
                var = m2.group(5).strip().strip(";")
            else:
                continue
            new_lines.append(f"{indent}printf(\"{var}: %d\\n\", {var});")

        code = "\n".join(new_lines)

        open(os.path.splitext(self.src)[0] + ".v.c", "w").write(code)

        return code


class RemoveLifetimeIntrinsicPass(Pass):
    def __init__(self):
        super().__init__("remove-lifetime-intrinsic")
    
    def run(self, code: str) -> str:
        lines = code.split("\n")
        new_lines = []
        deleted_pos = []
        for i in range(len(lines)):
            l = lines[i]
            if "/* lifetime_start */;" == l.strip():
                deleted_l = new_lines.pop()
                var = re.match(r"^\s*(\S+)\s*", deleted_l).group(1)
                for p in range(len(new_lines) - 1, -1, -1):
                    if re.match(r"^\s*\S+ " + var + r";", new_lines[p]):
                        deleted_pos.append(p)
                        break
            elif "/* lifetime_end */;" == l.strip():
                pass
            elif re.match(r"^llvm\.lifetime.[\s\S]+called by", l) is not None:
                pass
            else:
                new_lines.append(l)

        return "\n".join([new_lines[i] for i in range(len(new_lines)) if i not in deleted_pos])


class RemoveMarksPass(Pass):
    def __init__(self):
        super().__init__("remove-marks")

    def run(self, code: str) -> str:
        lines = code.split("\n")
        new_lines = []
        in_call_graph = False
        for l in lines:
            if l.startswith("/** Call Graph"):
                in_call_graph = True
            
            if in_call_graph:
                if l.startswith("**/"):
                    in_call_graph = False
            else:
                if l.startswith("/***********"):
                    continue
                l = l.replace("/******CALL******/", "")
                new_lines.append(l)
        
        return "\n".join(new_lines)


class ClangTidyPass(Pass):
    def __init__(self):
        super().__init__("clang-tidy")

    def run(self, code: str) -> str:
        with tempfile.NamedTemporaryFile(delete=True) as tf:
            tf.write(code.encode("utf-8"))

            with open(os.devnull, 'w') as devnull:
                subprocess.call(f"clang-tidy {tf.name} --fix", shell=True, stdout=devnull, stderr=devnull)

            tf.seek(0)
            code = tf.read().decode("utf-8")
        return code

    def check_dependencies(self):
        with open(os.devnull, 'w') as devnull:
            ret = subprocess.call("clang-tidy -h", shell=True, stdout=devnull, stderr=devnull)
            if ret != 0:
                print("Command not found: clang-tidy. Please install it first.")
                return False

        return True


class C2SSAPass(Pass):
    def __init__(self):
        super().__init__("c2ssa")

    def run(self, code: str) -> str:
        root_dir = os.path.dirname(sys.argv[0])
        build_dir = os.path.join(root_dir, "build")
        if not os.path.exists(build_dir):
            print("run cmake first.")
            sys.exit(-1)

        subprocess.call(f"cd \"{build_dir}\" && make", shell=True, stdout=open(os.devnull, 'w'))

        with tempfile.TemporaryDirectory() as tmpdir:
            src = os.path.join(tmpdir, "t.c")
            open(src, "w").write(code)

            subprocess.call(
                f"\"{os.path.join(build_dir, 'c2ssa')}\" {src}",
                shell=True
            )

            code = open(os.path.join(tmpdir, "t.ssa.c")).read()
        return code


class DrawCallGraphPass(Pass):
    """
        Author: yifan123
    """

    def __init__(self, src):
        super().__init__("draw-call-graph")

        self.src = src

    def run(self, code: str) -> str:
        from graphviz import Digraph

        regex = r"\/\*\* Call Graph[\s\S]*?\*\*\/"
        m = re.match(regex, code)
        call_code = m.group(0)
        call_code = call_code.split('\n')
        call_code = call_code[1:-1]
        call_code_dict = {}
        for call in call_code:
            call_code_dict[call] = 0
        for call in call_code:
            call_code_dict[call] = call_code_dict[call] + 1

        output_file = os.path.splitext(self.src)[0] + ".call_graph.gv"
        g = Digraph('G')
        g.attr(rankdir='LR', size='8,5')
        g.attr('node', shape='circle')
        for call in call_code_dict:
            edge = call.split('\tcalled by\t')
            g.edge(edge[1], edge[0], label=str(call_code_dict[call]))
        g.render(output_file)

        return code


class ExpandFunctionCallPass(Pass):
    """
        Author: zxc351200
    """

    def __init__(self):
        super().__init__("expand-function-call")

        self.FUNCTION_VAR = {}
        self.FUNCTION_DEC = {}
        self.functions = {}
        self.OUTPUT = []
        self.call_num = {}

    def function_call(self, seq, func_name, layer):
        FUNCTION_VAR = self.FUNCTION_VAR
        FUNCTION_DEC = self.FUNCTION_DEC
        functions = self.functions
        OUTPUT = self.OUTPUT
        call_num = self.call_num

        func_names = func_name + seq
        funcline = functions[func_name]["func_content"]
        keys = list(functions.keys())

        if len(funcline) == 0:
            return

        if func_names not in FUNCTION_VAR:
            FUNCTION_VAR[func_names] = []
        for id in range(len(funcline)):
            line = funcline[id]
            words = re.split('/', line)
            func_name1 = ""
            new_line = line
            for lens in range(len(words)):
                if re.match(r"\**CALL\**", words[lens]) and lens > 0:
                    for key in keys:
                        if len(functions[key]["func_content"]) > 0 and re.match(".*"+key+"\(.*", line) and key != "main":
                            func_name1 = key
                            new_seq = "_" + str(call_num[func_name1])
                            call_num[func_name1] += 1
                            self.function_call(new_seq, func_name1, layer + 1)
                            new_line = new_line.replace(func_name1 + "(", func_name1 + new_seq + "(")
            if id == 0:
                new_line = line.replace(func_name + "(", func_names + "(")
                FUNCTION_VAR[func_names].append(new_line)
            elif func_name1 == "":
                FUNCTION_VAR[func_names].append(line)
            else:
                FUNCTION_VAR[func_names].append(new_line)

    def build_graph(self, lines):
        FUNCTION_VAR = self.FUNCTION_VAR
        FUNCTION_DEC = self.FUNCTION_DEC
        functions = self.functions
        OUTPUT = self.OUTPUT
        call_num = self.call_num

        DG = nx.DiGraph()
        funcpd = 0
        if "func_begin" not in functions:
            functions["func_begin"] = {}
            functions["func_begin"]["func_content"] = []
        if "func_end" not in functions:
            functions["func_end"] = {}
            functions["func_end"]["func_content"] = []
        for line in lines:
            if re.match(r"/\* Function Declarations \*/", line):
                funcpd = 1
            elif re.match(r"/\*.*", line) and funcpd == 1:
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

        keys = list(functions.keys())

        funcpd = 0
        head_func_name = ""
        for id in range(len(lines)):
            line = lines[id]
            if re.match(r"/\**FUNC_BEGIN\**/", line):
                if "line" not in functions["func_begin"]:
                    functions["func_begin"]["line"] = line
                funcpd = 1
                line2 = lines[id+1]
                head_func_name = line2.split(' ', 1)[1]
                head_func_name = head_func_name.split('(', 1)[0]
            elif re.match(r"/\**FUNC_END\**/", line):
                if "line" not in functions["func_end"]:
                    functions["func_end"]["line"] = line
                funcpd = 2
            elif funcpd == 1:
                functions[head_func_name]["func_content"].append(line)
            elif funcpd == 0 or line != "\n":
                OUTPUT.append(line)

    def work3(self, lines):
        FUNCTION_VAR = self.FUNCTION_VAR
        FUNCTION_DEC = self.FUNCTION_DEC
        functions = self.functions
        OUTPUT = self.OUTPUT
        call_num = self.call_num

        mainfunc_line = functions["main"]["func_content"]
        keys = list(functions.keys())

        for key in functions.keys():
            call_num[key] = 1
        if "main" not in FUNCTION_VAR:
            FUNCTION_VAR["main"] = []
        for id in range(len(mainfunc_line)):
            line = mainfunc_line[id]
            words = re.split('/', line)
            seq = "_"
            new_seq = ""
            func_name = ""
            new_line = line

            for lens in range(len(words)):
                if re.match(r"\**CALL\**", words[lens]) and lens > 0:
                    for key in keys:
                        if len(functions[key]["func_content"]) > 0 and re.match(".*"+key+"\(.*", line) and key != "main":
                            func_name = key
                            new_seq = seq + str(call_num[func_name])
                            call_num[func_name] += 1
                            self.function_call(new_seq, func_name, layer=1)
                            new_line = new_line.replace(func_name + "(", func_name + new_seq + "(")
            if func_name == "":
                FUNCTION_VAR["main"].append(line)
            else:
                FUNCTION_VAR["main"].append(new_line)

        keys = list(FUNCTION_VAR.keys())
        lens = len(keys)
        for i in range(lens):
            begin = functions["func_begin"]["line"] #"/************FUNC_BEGIN*****************/\n"
            OUTPUT.append(begin)
            key = keys[lens-i-1]
            for var in FUNCTION_VAR[key]:
                OUTPUT.append(var)
            end = functions["func_end"]["line"] + "\n" #"/************FUNC_END*****************/\n"
            OUTPUT.append(end)

        return "\n".join(OUTPUT)

    def run(self, code: str) -> str:
        lines = code.split("\n")
        self.build_graph(lines)
        return self.work3(lines)


class StrConstStructEliminationPass(Pass):
    """
        Author: cheng052
    """

    def __init__(self):
        super().__init__("str-const-struct-elimination")

    def run(self, code: str) -> str:
        struct_define_regex = r'struct\s+(\S+)\s*{\s*char\s+array\s*\[\s*\d+\s*\]\s*;\s*}\s*;\s'
        str_define_regex = [r'static\s+const\s+struct\s+', None, r'\s+(\S+)\s*=\s*{\s*"([\s\S]*?)"\s*}\s*;\s']
        str_use_regex = [r'\(\(&', None, r'.array\[UINT64_C\(0\)\]\)\)']

        txt = code
        # Get self-defined string variable type with format 'struct [name] { char [[number]]; }; '
        struct_def_patrn = re.compile(struct_define_regex)
        struct_def_list = [i for i in struct_def_patrn.finditer(txt)]

        # Get const string dictionary <const_str_name, string>
        const_str_dictionary = dict()
        const_str_def_list = list()
        for struct_def in struct_def_list:
            # static const struct [struct_name] [const_var_name] = { "[string]" } ;
            regex = str_define_regex[0] + struct_def.group(1) + str_define_regex[2]
            pattern = re.compile(regex)
            for i in pattern.finditer(txt):
                const_str_dictionary[i.group(1)] = i.group(2)
                const_str_def_list.append(i)
        
        # Replace str var with const string
        for k, v in const_str_dictionary.items():
            regex = str_use_regex[0] + k + str_use_regex[2]
            v = '"' + v.replace('\\', '\\\\') + '"' 
            txt = re.sub(regex, v, txt)
        
        # Delete struct definition and string definition
        for struct_def in struct_def_list:
            txt = txt.replace(struct_def.group(), '')
        for str_def in const_str_def_list:
            txt = txt.replace(str_def.group(), '')

        return txt


class PromoteLocalVariablesPass(Pass):
    """
        Author: tbw19970424
    """
    
    def __init__(self):
        super().__init__("promote-local-variable")

        self.text = []
        self.globalv = []

    def remove_local_variables(self):
        text = self.text
        globalv = self.globalv

        n = len(text)
        i = 0
        while i < n:
            if re.search(r"/\*{11}FUNC_BEGIN\*{16}/", text[i]):
                localv = []
                newv = []
                exist_localv = False
                i += 1
                search_func = re.search(r"^\s*(\S+)\s+(\S+)\((.*)\)\s*\{", text[i])
                while not search_func:
                    i += 1
                    search_func = re.search(r"^\s*(\S+)\s+(\S+)\((.*)\)\s*\{", text[i])
                func = search_func.group(2)
                i += 1
                while not re.search(r"/\*{12}FUNC_END\*{17}/", text[i]):
                    if exist_localv and localv:
                        for v in range(len(localv)):
                            text[i] = re.sub(localv[v], newv[v], text[i])

                    if re.search(r"/\*{11}LOCAL_VAR_BEGIN\*{11}/", text[i]) and not exist_localv:
                        text.pop(i)
                        n -= 1
                        while not re.search(r"/\*{12}LOCAL_VAR_END\*{12}/", text[i]):
                            search_localv = re.search(r"^\s*(\S+)\s+(\S+);", text[i])
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
                        i += 1
            i += 1

    def insert_globalv(self):
        text = self.text
        globalv = self.globalv

        n = len(text)
        for i in range(n):
            if re.search(r"/\* Global Declarations \*/", text[i]):
                line = i+1
                for j in range(len(globalv)):
                    text.insert(line+j, globalv[j])
                break

    def run(self, code: str) -> str:
        self.text = code.split("\n")
        self.remove_local_variables()
        self.insert_globalv()

        return "\n".join(self.text)


def main(args):
    src = args.src.strip()
    dst = args.output.strip()
    if not src.endswith(".c"):
        raise RuntimeError("invalid src filename: " + src)
    if dst == "":
        dst = os.path.splitext(src)[0] + ".ssa.c"
    if not dst.endswith(".c"):
        raise RuntimeError("invalid output filename: " + dst)

    include_paths = get_include_paths(src)
    os.putenv("C_INCLUDE_PATH", ":".join(include_paths))

    pm = PassManager()
    if args.verify:
        pm.add(VerifyPass(src))
        pm.add(ClangTidyPass())

    # core pass
    pm.add(C2SSAPass())

    # postprocess
    pm.add(RemoveLifetimeIntrinsicPass())

    if args.draw_call_graph:
        pm.add(DrawCallGraphPass(src))

    pm.add(ExpandFunctionCallPass())
    pm.add(StrConstStructEliminationPass())
    pm.add(PromoteLocalVariablesPass())
    pm.add(RemoveMarksPass())
    pm.add(ClangTidyPass())

    if not pm.check_dependencies():
        return

    code = pm.run(open(src).read())
    open(dst, "w").write(code)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("src", type=str, help="c soruce file (*.c).")
    parser.add_argument("--output", "-o", type=str, default="", help="output file (default: *.ssa.c).")
    parser.add_argument("--verify", action="store_true", help="verify.")
    parser.add_argument("--draw-call-graph", action="store_true", help="draw call graph.")

    main(parser.parse_args())
