from cjfx import list_files, write_to, read_from

fn = ".gitignore"

all_files = list_files("TxtInOut", "csv")
all_files += list_files("TxtInOut", "txt")

for file_ in all_files:
    write_to(fn, file_.replace("\\", "/") + "\n", mode = "append")
