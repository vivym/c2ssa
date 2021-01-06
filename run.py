import re
import os
import sys
import subprocess
import argparse


def get_include_paths(source_filename):
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


def replace_ext(src, src_ext, dst_ext):
    assert src.endswith(src_ext)
    return src[:-len(src_ext)] + dst_ext


def get_indent(s):
    return re.match(r"^(\s*)", s).group(1)


def verify_pass(src):
    dst = replace_ext(src, ".c", ".v.c")
    lines = open(src).readlines()
    new_lines = []
    for l in lines:
        new_lines.append(l)
        indent = get_indent(l)
        m1 = re.match(r"^(\s*)(\S+)\s*((=)|(\+=)|(\-=)|(\*=)|(\/=)|(\%=)|(\~=)|(\^=)|(\&=)|(\+\+)|(\-\-))", l)
        m2 = re.match(r"^(\s*)((\+\+)|(\-\-))\s*(\S+)", l)
        if m1 is not None:
            var = m1.group(2).strip().strip(";")
        elif m2 is not None:
            var = m2.group(5).strip().strip(";")
        else:
            continue
        new_lines.append(f"{indent}printf(\"{var}: %d\\n\", {var});\n")

    open(dst, "w").writelines(new_lines)
    devnull = open(os.devnull, 'w')
    subprocess.call(f"clang-tidy {dst} --fix", shell=True, stdout=devnull, stderr=devnull)
    return dst


def preprocess(src, verify):
    if verify:
        src = verify_pass(src)

    return src


def remove_lifetime_intrinsic(code):
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


def postprocess(dst):
    code = open(dst).read()

    code = remove_lifetime_intrinsic(code)

    open(dst, "w").write(code)


def main(args):
    src = args.src.strip()
    if not src.endswith(".c"):
        raise RuntimeError("invalid src filename: " + src)

    include_paths = get_include_paths(src)
    os.putenv("C_INCLUDE_PATH", ":".join(include_paths))

    src = preprocess(src, args.verify)

    if not args.preprocess_only:
        root_dir = os.path.dirname(sys.argv[0])
        build_dir = os.path.join(root_dir, "build")
        if not os.path.exists(build_dir):
            print("run cmake first.")
            sys.exit(-1)

        subprocess.call(f"cd \"{build_dir}\" && make", shell=True, stdout=open(os.devnull, 'w'))

        subprocess.call(
            f"\"{os.path.join(build_dir, 'c2ssa')}\" {src}",
            shell=True
        )

        dst = replace_ext(src, ".c", ".ssa.c")
        postprocess(dst)

    print("done.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("src", type=str, help="c soruce file (*.c).")
    parser.add_argument("--verify", action="store_true", help="verify.")
    parser.add_argument("--preprocess-only", action="store_true", help="preprocess-only")

    main(parser.parse_args())
