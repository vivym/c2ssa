import re
import os
import sys
import subprocess


def get_include_paths(source_filename):
    ret = subprocess.run(
        ["clang", "-v", "-o", "/dev/null", source_filename],
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


def main():
    source_filename = sys.argv[1]

    include_paths = get_include_paths(source_filename)

    root_dir = os.path.dirname(sys.argv[0])
    build_dir = os.path.join(root_dir, "build")
    if not os.path.exists(build_dir):
        print("run cmake first.")
        sys.exit(-1)

    subprocess.call(f"cd \"{build_dir}\" && make", shell=True)

    os.putenv("C_INCLUDE_PATH", ":".join(include_paths))
    subprocess.call(
        f"\"{os.path.join(build_dir, 'c2ssa')}\" {source_filename}",
        shell=True
    )

    print("done.")


if __name__ == "__main__":
    main()
