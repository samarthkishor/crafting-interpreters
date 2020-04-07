import os
import re

expected_regex = re.compile(r".*//\s*expect:\s*(.*)")

for dir_name, sub_dirs, files in os.walk("."):
    if dir_name == "./_other":
        continue
    print(dir_name + ":")
    for filename in files:
        if filename == ".DS_Store" or "lox" not in filename:
            continue
        filename = filename.replace(".lox", "")
        expectations = []
        with open(f"{dir_name}/{filename}.lox", "r") as f:
            for line in f:
                match = expected_regex.match(line)
                if match:
                    m = match.group(1)
                    if m != "" and m != "\n":
                        expectations.append(m)
        if expectations != []:
            with open(f"{dir_name}/{filename}.expected", "w") as f:
                f.write("\n".join(expectations))
            print(f'"{filename}";')  # I just copy paste the output into `test.ml`
