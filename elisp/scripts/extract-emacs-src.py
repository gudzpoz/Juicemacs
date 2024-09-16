#!/bin/python

import argparse
import dataclasses
import json
import re
import typing

from pathlib import Path


parser = argparse.ArgumentParser(
    prog='extract-defun',
    description='Extracts function definitions out of Emacs source code.',
)
parser.add_argument('filename')
parser.add_argument('-j', '--java', required=True)
parser.add_argument('-c', '--context', required=True)
parser.add_argument('-b', '--buffer', required=True)
parser.add_argument('-g', '--globals', required=True)
args = parser.parse_args()


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

    @classmethod
    def lower(cls, fname: str):
        return f'{fname[1].lower()}{fname[2:]}'

    @classmethod
    def better_fname(cls, name: str):
        assert name[0] == 'F'
        prefix = 'F'
        camel = ''.join(
            segment.capitalize() for segment in name[1:].split('_')
        )
        return f'{prefix}{camel}'

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
        public static Object {self.lower(self.fname)}({self.args()}) {{
            throw new UnsupportedOperationException();
        }}
    }}'''


@dataclasses.dataclass
class Variable:
    name: str
    c_name: str
    lisp_type: str
    init_value: typing.Any

    def jname(self):
        stem = self.c_name[1:] if self.c_name[0] == 'V' else self.c_name
        camel = ''.join(segment.capitalize() for segment in stem.split('_'))
        var_name = camel[0].lower() + camel[1:]
        return var_name

    def symbol_jname(self):
        return '_'.join(
            seg.upper() for seg in self.name.split('-')
        )

    def jtype(self):
        t = self.lisp_type
        if t == 'INT':
            return 'long', '0'
        elif t == 'BOOL':
            return 'boolean', 'false'
        elif t.startswith('LISP'):
            return 'Object', 'NIL /* uninitialized */'
        else:
            return 'Object', 'null /* TODO */'

    @classmethod
    def value(cls, v):
        if v is None:
            return None
        if type(v) is bool:
            return 'true' if v else 'false'
        if type(v) is int:
            return f'{v}'
        if type(v) is str:
            return f'new ELispString({json.dumps(v)})'
        if type(v) is float:
            return f'{v}'
        if type(v) is tuple:
            if len(v) == 1:
                return f'new ELispCons({cls.value(v[0])})'
            if len(v) == 2:
                return (
                    f'ELispCons.listOf({cls.value(v[0])}, {cls.value(v[1])})'
                )
            raise Exception(v)
        if type(v) is dict:
            if 'symbol' in v:
                return v['symbol'][1:].upper()
            if 'CALLN' in v:
                if v['CALLN'] == 'FMakeHashTable.makeHashTable':
                    return f'''{v["CALLN"]}(new Object[]{{{
                        ", ".join(cls.value(arg) for arg in v["args"])
                    }}})'''
                return f'{v["CALLN"]}({
                    ", ".join(cls.value(arg) for arg in v["args"])})'
            if 'raw' in v:
                return v['raw']
        raise Exception(v)

    def format(self):
        t, default_v = self.jtype()
        v = self.value(self.init_value)
        v = default_v if v is None else v
        if t == 'boolean':
            if v == '1':
                v = 'true'
            elif v == '0':
                v = 'false'
        return (
            f'ELispSymbol.Value.Forwarded {self.jname()} = '
            f'new ELispSymbol.Value.Forwarded(({t}) {v})'
        )

    def init(self):
        return f'{self.symbol_jname()}.forwardTo({self.jname()})'


def arg_count(n: str):
    if n == 'MANY':
        return ARG_MANY
    if n == 'UNEVALLED':
        return ARG_UNEVALLED
    if n.startswith('charset_arg_'):
        # TODO: We will probably handle this at runtime
        return 0
    return int(n)


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
DEFVAR_REGEX = re.compile(
    r'DEFVAR_(\w+?)\s*\('
    r'\s*"([^"]+?)"\s*,'
    r'\s*(\w+?)\s*,',
    re.MULTILINE | re.DOTALL,
)
DEFVAR_BUF_REGEX = re.compile(
    r'DEFVAR_PER_BUFFER\s*\('
    r'\s*"([^"]+?)"\s*,'
    r'\s*&BVAR\s*\(\s*current_buffer\s*,\s*(\w+)\s*\)\s*,'
    r'\s*(\w+)\s*,'
)
DEFVAR_DETECT = re.compile(r'DEFVAR_\w+\s*\("(\S+?)"')


with open(args.filename, 'r') as f:
    contents = f.read()

    # Subroutines
    detected = DEFUN_DETECT.findall(contents)
    count = len(detected)
    matches = DEFUN_REGEX.findall(contents)
    not_found = set(detected) - set(m[0] for m in matches)
    if not_found != {'testme'}:
        assert set(detected) == set(m[0] for m in matches), not_found
        assert len(matches) == count, (matches, count)
    subroutines = dict(
        (
            LispSubroutine.better_fname(fname),
            (
                i,
                LispSubroutine(
                    name,
                    LispSubroutine.better_fname(fname),
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

    # Symbols
    detected = DEFSYM_DETECT.findall(contents)
    count = len(detected)
    matches = DEFSYM_REGEX.findall(contents)
    assert set(detected) == set(m[0] for m in matches)
    assert len(matches) == count
    symbols: dict[str, str] = dict(
        (symbol_str.replace('-', '_').upper(), symbol_str)
        if '--' in symbol_str else (c_symbol[1:].upper(), symbol_str)
        for c_symbol, symbol_str in matches
    )

    # Variables
    ifdef_inits = [
        'Vload_suffixes',
        'Vmodule_file_suffix',
        'Vdynamic_library_suffixes',
        'Vdynamic_library_alist',
    ]

    class LazyDict(dict):
        def __missing__(self, key: str):
            if key[0] == 'Q':
                return {'symbol': key}
            if key[0] == 'F':
                return lambda *args: {'CALLN': (
                    f'{LispSubroutine.better_fname(key)}.'
                    f'''{LispSubroutine.lower(
                        LispSubroutine.better_fname(key)
                    )}'''
                ), 'args': args}
            if key == 'CALLN':
                return lambda func, *args: {
                    'CALLN': func()['CALLN'],
                    'args': args,
                }
            raise Exception(f'unsupported: {key}')

    def ID(i):  # identity
        return i
    ENV = LazyDict({
        'make_float': ID,
        'make_fixnum': ID,
        'build_pure_c_string': ID,
        'build_string': ID,
        'build_unibyte_string': ID,
        'empty_unibyte_string': '',
        'intern_c_string': ID,
        'true': True,
        'false': False,
        'pure_list': lambda *items: tuple(items),
        'MOST_POSITIVE_FIXNUM': {'raw': 'Long.MAX_VALUE'},
        'MOST_NEGATIVE_FIXNUM': {'raw': 'Long.MIN_VALUE'},
        'list1': lambda a: (a,),
        'decode_env_path': lambda *args: ('',),
        'PATH_DUMPLOADSEARCH': '',
        'SYSTEM_TYPE': 'jvm',
        'EMACS_CONFIGURATION': '',
        'EMACS_CONFIG_OPTIONS': '',
        'EMACS_CONFIG_FEATURES': '',
        'emacs_copyright': 'TODO: Copy over GPL',
        'emacs_version': '30.0',
        'emacs_bugreport': '',
    })
    detected = DEFVAR_DETECT.findall(contents)
    matches = DEFVAR_REGEX.findall(contents)
    buffer_local = DEFVAR_BUF_REGEX.findall(contents)
    matched_names = set(m[1] for m in matches).union(
        set(m[0] for m in buffer_local)
    )
    assert (
        set(detected) == matched_names
    ), set(detected) - matched_names
    assert len(matched_names) == len(detected)
    init_section = contents[contents.find('\nsyms_of_') + 9:]
    if Path(args.filename).stem != 'search':
        assert '\nsyms_of_' not in init_section
    variables: list[Variable] = []
    for lisp_type, name, c_name in matches:
        init = re.compile(
            f'\\s+{c_name}\\s*=\\s*([^;]+);'
        ).findall(init_section)
        init_value = None
        if c_name not in ifdef_inits:
            assert len(init) <= 1, (name, init)
            if c_name == 'Vbytecomp_version_regexp':
                # TODO: This needs manual updates.
                init_value = \
                    "^;;;.\\(?:in Emacs version\\|bytecomp version FSF\\)"
                assert json.dumps(init_value) in contents
            elif c_name == 'Vsource_directory':
                init_value = {'raw': 'new ELispString("")'}
            elif c_name == 'Vpath_separator':
                init_value = '/'
            elif len(init) > 0:
                try:
                    init_value = eval(
                        init[0].replace('\n', '').split('=')[-1],
                        ENV,
                    )
                except Exception as e:
                    raise Exception(c_name, e)
        variables.append(Variable(name, c_name, lisp_type, init_value))
    for v in variables:
        if v.symbol_jname() not in symbols:
            symbols[v.symbol_jname()] = v.name


######################
# Export Subroutines #
######################

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


##################
# Export Symbols #
##################

def replace_or_insert_region(contents: str, marker: str, update: str):
    section_start = (
        f'    /* @generated region="{marker}" by="extract-emacs-src.py" */\n'
    )
    section_end = (
        f'    /* @end region="{marker}" */\n'
    )
    if section_start in contents:
        start = contents.index(section_start)
        end = contents.index(section_end)
        assert start < end
        return (
            f'{contents[:start]}{section_start}'
            f'{update}{contents[end:]}'
        )
    else:
        last = contents.rfind('}')
        return (
            f'{contents[:last]}{section_start}{update}'
            f'{section_end}{contents[last:]}'
        )


JAVA_SYMBOL_DETECT = re.compile(
    r'public final static ELispSymbol (\S+?) = new ELispSymbol',
    re.MULTILINE | re.DOTALL,
)


c_file = Path(args.filename).name
stem = Path(args.filename).stem
with open(args.context, 'r') as f:
    contents = f.read()
    already_defined = JAVA_SYMBOL_DETECT.findall(
        replace_or_insert_region(contents, c_file, '')
    )
    duplicates = set(already_defined)
    lisp_names = set()
    for symbol in symbols.values():
        assert symbol not in lisp_names, symbol
        lisp_names.add(symbol)
    java_symbols = '\n'.join(
        f'    public final static ELispSymbol {varname} = '
        f'new ELispSymbol("{symbol}");'
        for varname, symbol in sorted(symbols.items())
        if varname not in duplicates
    )
    gather_symbols = f'''
    private ELispSymbol[] {stem}Symbols() {{
        return new ELispSymbol[]{{
{'\n'.join(f'                {k},' for k in sorted(symbols.keys()))}
        }};
    }}
'''
    contents = replace_or_insert_region(
        contents, c_file, java_symbols + gather_symbols,
    )

with open(args.context, 'w') as f:
    f.write(contents)

####################
# Export Variables #
####################

with open(args.globals, 'r') as f:
    contents = f.read()
    contents = replace_or_insert_region(
        contents,
        c_file,
        ''.join(
            f'    public static {v.format()};\n'
            for v in variables
        ) + f'''
    private static void {stem}Vars() {{
{'\n'.join(f'        {v.init()};' for v in variables)}
    }}
''',
    )

with open(args.globals, 'w') as f:
    f.write(contents)
