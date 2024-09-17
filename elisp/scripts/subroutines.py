# Copied from tools/extract/subroutines.py in https://github.com/federicotdn/pimacs
# Licensed under GPL3.
# Unmodified except for these comments.

import json
from pathlib import Path

from pyparsing import (
    Literal,
    Regex,
    QuotedString,
    SkipTo,
    Opt,
    StringStart,
    StringEnd,
    delimited_list,
)

DEFUN_EXPR_IDENTIFIER = Regex(r"[0-9a-zA-Z_-]+")
DEFUN_EXPR_NUMBER = Regex(r"[0-9]+")
DEFUN_EXPR = (
    StringStart()
    + Literal("DEFUN")
    + "("
    + QuotedString('"')("lname")
    + ","
    + DEFUN_EXPR_IDENTIFIER("fnname")
    + ","
    + DEFUN_EXPR_IDENTIFIER("sname")
    + ","
    + (DEFUN_EXPR_NUMBER | DEFUN_EXPR_IDENTIFIER)("minargs")
    + ","
    + (DEFUN_EXPR_NUMBER | "MANY" | "UNEVALLED")("maxargs")
    + ","
    + (DEFUN_EXPR_NUMBER | QuotedString('"', esc_char="\\", multiline=True) | "NULL")(
        "intspec"
    )
    + ","
    + "doc:"
    + "/*"
    + SkipTo("*/")("doc")
    + "*/"
    + Opt("attributes:" + (Literal("noreturn") | "const")("attributes"))
    + ")"
    + (
        (
            Literal("(")
            + (
                delimited_list(
                    Opt("register") + "Lisp_Object" + DEFUN_EXPR_IDENTIFIER("args*"),
                    min=1,
                )
                | "void"
                | Literal("ptrdiff_t nargs, Lisp_Object *args")
                | Literal("ptrdiff_t n, Lisp_Object *args")
            )
            + ")"
        )
        | StringEnd()
    )
)
EMACS_CONSTANTS = {
    "coding_arg_max": 13,
    "charset_arg_max": 17,
}


def process_defun(defun: dict) -> dict:
    val = defun["maxargs"]
    try:
        maxargs = int(val)
        if maxargs < 0 or maxargs > 8:
            raise ValueError(val)
    except ValueError:
        if val == "MANY":
            maxargs = -1
        elif val == "UNEVALLED":
            maxargs = -2
        else:
            raise ValueError(val)

    defun["maxargs"] = maxargs

    val = defun["minargs"]
    try:
        minargs = int(val)
    except ValueError:
        minargs = EMACS_CONSTANTS[val]

    defun["minargs"] = minargs
    defun["doc"] = defun["doc"].strip()
    defun["args"] = list(defun["args"]) if defun["args"] is not None else None

    return defun


def extract_defuns(path: Path) -> list[dict]:
    with open(path) as f:
        contents = f.read()

    lines = contents.splitlines()
    current_defun = []
    defuns = []

    for line in lines:
        if line.startswith("DEFUN"):
            if current_defun:
                raise Exception(
                    "Previous DEFUN not closed: " + "\n".join(current_defun)
                )

            current_defun.append(line)
        elif line.startswith("{"):
            if not current_defun:
                continue

            defun = "\n".join(current_defun)
            current_defun.clear()

            try:
                parsed = DEFUN_EXPR.parse_string(defun, parse_all=True)
            except Exception:
                print(f"----- context: ----- (file: {path})")
                print(defun)
                print("--------------------")
                raise

            result = {
                "lname": parsed["lname"],
                "fnname": parsed["fnname"],
                "sname": parsed["sname"],
                "minargs": parsed["minargs"],
                "maxargs": parsed["maxargs"],
                "intspec": parsed["intspec"],
                "args": parsed.get("args"),
                "attributes": parsed.get("attributes"),
                "doc": parsed["doc"],
                "path": path.name,
            }

            defuns.append(process_defun(result))
        elif current_defun:
            current_defun.append(line)

    if current_defun:
        raise Exception("Previous DEFUN not closed: " + "\n".join(current_defun))

    return defuns


def resolve_repeated(defuns: list[dict]) -> list[dict]:
    processed = {}

    for defun in defuns:
        lname = defun["lname"]
        prev = processed.get(lname)
        if prev:
            prev["maxargs"] = max(prev["maxargs"], defun["maxargs"])

            for attr in ["minargs", "attributes", "intspec"]:
                if prev[attr] != defun[attr]:
                    raise Exception(f"{attr} incompatibility for {lname}")

            prev["doc"] += "\n\n" + defun["doc"]
        else:
            processed[lname] = defun

    return sorted(processed.values(), key=lambda d: d["lname"])


def ignore_file(filename: str) -> bool:
    return any(
        [
            filename == "msdos.c",
            filename == "dosfns.c",
            "android" in filename,
            "haiku" in filename,
        ]
    )


def extract_subroutines(
    pimacs_base: Path, emacs_base: Path, emacs_commit: str, emacs_branch: str
) -> None:
    all_defuns = []

    for p in sorted(emacs_base.joinpath("src").rglob("*")):
        if not p.suffix == ".c" or ignore_file(p.name):
            continue

        defuns = extract_defuns(p)
        all_defuns.extend(defuns)

    all_defuns = resolve_repeated(all_defuns)

    target = pimacs_base / "test" / "data" / "emacs_subroutines.json"
    with open(target, "w") as f:
        data = {
            "subroutines": all_defuns,
            "stats": {
                "subroutines_count": len(all_defuns),
                "constants_count": len(EMACS_CONSTANTS),
            }
        }
        json.dump(data, f, indent=4, sort_keys=True)

    print("wrote", target)
