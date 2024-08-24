#!/bin/python

import argparse
import dataclasses
import json
import re

from pathlib import Path


ARG_MANY = -0xF
ARG_UNEVALLED = -0xFF


@dataclasses.dataclass
class LispSubroutine:
    name: str
    fname: str
    sname: str
    min_args: int
    max_args: int
    interactive: str
    doc: str

    def lower(self):
        return f'{self.fname[1].lower()}{self.fname[2:]}'

    def args(self):
        args = max(0, self.max_args, self.min_args)
        s = ', '.join(f'Object {chr(ord("a") + i)}'for i in range(args))
        if self.max_args == ARG_MANY or self.max_args == ARG_UNEVALLED:
            if s != '':
                s += ', '
            s += 'Object[] args'
        return s

    def format_java(self):
        return f'''
    @ELispBuiltIn(name = "{self.name}", minArgs = {max(0, self.min_args)}, \
maxArgs = {max(0, self.min_args) if self.max_args < 0 else self.max_args}\
{", varArgs = true" if self.max_args < 0 else ""}\
{", rawArg = true" if self.max_args == ARG_UNEVALLED else ""}\
{f", doc = {json.dumps(self.doc.strip())}" if self.doc != "" else ""})
    @GenerateNodeFactory
    public abstract static class {self.fname} extends ELispBuiltInBaseNode {{
        @Specialization
        public static Object {self.lower()}({self.args()}) {{
            throw new UnsupportedOperationException();
        }}
    }}'''


def arg_count(n: str):
    if n == 'MANY':
        return ARG_MANY
    if n == 'UNEVALLED':
        return ARG_UNEVALLED
    return int(n)


def better_fname(name: str):
    assert name[0] == 'F'
    prefix = 'F'
    camel = ''.join(segment.capitalize() for segment in name[1:].split('_'))
    return f'{prefix}{camel}'


parser = argparse.ArgumentParser(
    prog='extract-defun',
    description='Extracts function definitions out of Emacs source code.',
)
parser.add_argument('filename')
parser.add_argument('-j', '--java', required=True)
parser.add_argument('-c', '--context', required=True)
args = parser.parse_args()


DEFUN_REGEX = re.compile(
    r'DEFUN\s*\(\s*'
    r'"(\S+?)",\s*'
    r'(\S+?),\s*(\S+?),\s*'
    r'(\S+?),\s*(\S+?),\s*'
    r'(\S+?|".*?"),\s*?doc:\s*?/\*(.+?)\*/',
    re.MULTILINE | re.DOTALL,
)
DEFUN_DETECT = re.compile(r'DEFUN\s*\(\s*"(\S+?)"')
DEFSYM_REGEX = re.compile(
    r'DEFSYM\s*\(\s*'
    r'(\S+?),\s*'
    r'"([^"]*?)"\s*'
    r'\);?',
    re.MULTILINE | re.DOTALL,
)
DEFSYM_DETECT = re.compile(r'DEFSYM\s*\((\w+),')


with open(args.filename, 'r') as f:
    contents = f.read()
    count = len(DEFUN_DETECT.findall(contents))
    matches = DEFUN_REGEX.findall(contents)
    assert (
        set(DEFUN_DETECT.findall(contents))
        == set(m[0] for m in matches)
    ), set(DEFUN_DETECT.findall(contents)) - set(m[0] for m in matches)
    assert len(matches) == count, (matches, count)
    subroutines = dict(
        (
            better_fname(fname),
            (
                i,
                LispSubroutine(
                    name,
                    better_fname(fname),
                    sname,
                    arg_count(min_args),
                    arg_count(max_args),
                    interactive,
                    doc,
                ),
            ),
        )
        for i, (name, fname, sname, min_args, max_args, interactive, doc)
        in enumerate(matches)
    )

    count = len(DEFSYM_DETECT.findall(contents))
    matches = DEFSYM_REGEX.findall(contents)
    assert (
        set(DEFSYM_DETECT.findall(contents))
        == set(m[0] for m in matches)
    )
    assert len(matches) == count
    symbols: dict[str, str] = dict(
        (c_symbol[1:].upper(), symbol_str)
        for c_symbol, symbol_str in matches
    )


JAVA_NODE_DETECT = re.compile(
    r'public abstract static class (\w+) extends ELispBuiltInBaseNode',
    re.MULTILINE | re.DOTALL,
)


with open(args.java, 'r') as f:
    contents = f.read()
    inserted = set(JAVA_NODE_DETECT.findall(contents))
    extra_functions = inserted - subroutines.keys()
    print("Functions not in the C file: ", extra_functions)
    new_functions = [
        subroutines[k] for k in (subroutines.keys() - inserted)
    ]
    new_functions.sort(key=lambda v: v[0])
    original = contents[0:contents.rindex('}')]
    for _, subroutine in new_functions:
        original += subroutine.format_java()
        original += '\n'
    original += '}\n'

with open(args.java, 'w') as f:
    f.write(original)


JAVA_SYMBOL_DETECT = re.compile(
    r'public final ELispSymbol (\S+?) = new ELispSymbol',
    re.MULTILINE | re.DOTALL,
)


with open(args.context, 'r') as f:
    c_file = Path(args.filename).name
    stem = Path(args.filename).stem
    section_start = (
        f'    /* @generated region="{c_file}" by="extract-emacs-src.py" */'
    )
    section_end = (
        f'    /* @end region="{c_file}" */'
    )
    contents = f.read()
    try:
        start = contents.index(section_start)
        end = contents.index(section_end)
        assert start < end
        processed = True
        already_defined = JAVA_SYMBOL_DETECT.findall(
            contents[0:start] + contents[end:],
        )
    except ValueError:
        start = None
        end = None
        processed = False
        already_defined = JAVA_SYMBOL_DETECT.findall(contents)
    duplicates = set(already_defined)
    java_symbols = '\n'.join(
        f'    public final ELispSymbol {varname} = '
        f'new ELispSymbol("{symbol}");'
        for varname, symbol in sorted(symbols.items())
        if varname not in duplicates
    )
    gather_symbols = f'''
    private ELispSymbol[] {stem}Symbols() {{
        return new ELispSymbol[] {{
{'\n'.join(f'            {k},' for k in sorted(symbols.keys()))}
        }};
    }}
'''
    if processed:
        contents = (
            contents[0:start]
            + section_start
            + '\n'
            + java_symbols
            + gather_symbols
            + section_end
            + contents[(end + len(section_end)):]
        )
    else:
        last = contents.rindex('}')
        contents = (
            contents[0:last]
            + section_start
            + '\n'
            + java_symbols
            + gather_symbols
            + section_end
            + '\n'
            + contents[last:]
        )

with open(args.context, 'w') as f:
    f.write(contents)
