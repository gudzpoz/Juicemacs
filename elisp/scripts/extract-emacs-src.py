#!/bin/python

import argparse
import dataclasses
import json
import re
import typing


from pathlib import Path
from pyparsing import (
    alphanums,
    DelimitedList,
    LineStart,
    Literal,
    QuotedString,
    Regex,
    SkipTo,
    Word,
)
from xml.sax.saxutils import escape as escape_xml

from subroutines import extract_defuns

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


ARG_MANY = -1
ARG_UNEVALLED = -2
JAVA_KEYWORDS = {
    'abstract',
    'do',
    'if',
    'package',
    'synchronized',
    'boolean',
    'double',
    'implements',
    'private',
    'this',
    'break',
    'else',
    'import',
    'protected',
    'throw',
    'byte',
    'extends',
    'instanceof',
    'public',
    'throws',
    'case',
    'false',
    'int',
    'return',
    'transient',
    'catch',
    'final',
    'interface',
    'short',
    'true',
    'char',
    'finally',
    'long',
    'static',
    'try',
    'class',
    'float',
    'native',
    'strictfp',
    'void',
    'const',
    'for',
    'new',
    'super',
    'volatile',
    'continue',
    'goto',
    'null',
    'switch',
    'while',
    'default',
    'assert',
}
SPECIAL_FORM_VARARGS = {
    'and': True,
    'catch': True,
    'cond': True,
    'condition-case': True,
    'defconst': 3,
    'defvar': 3,
    'function': 1,
    'if': True,
    'interactive': 1,
    'lambda': True,
    'let': True,
    'let*': True,
    'or': True,
    'prog1': True,
    'prog2': True,
    'progn': True,
    'quote': 1,
    'save-current-buffer': True,
    'save-excursion': True,
    'save-restriction': True,
    'setq': True,
    'setq-default': True,
    'unwind-protect': True,
    'while': True,
}
USAGE_NAMES = {
    'handler-bind-1': 'handler-bind',
}
WANTS_NODE_ACCESS = [
    'eval',
    'load',
    'makeInterpretedClosure',
    'require',
]


@dataclasses.dataclass
class LispSubroutine:
    name: str
    fname: str
    sname: str
    arg_names: list[str]
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

    def arg_name(self, s: str, sep: str):
        if s == '...':
            return 'args'
        name = ''.join(
            (seg if i == 0 else seg.capitalize())
            for i, seg in enumerate(s.strip('[].').split(sep))
        )
        if name in JAVA_KEYWORDS:
            name = f'{name}_'
        assert name != '', (s, self)
        return name

    def args_from_usage(self):
        '''Extract argument names from the usage string in function doc.'''
        r = (
            LineStart()
            + Literal('usage:')
            + '('
            + Literal(USAGE_NAMES.get(self.name, self.name))
            + DelimitedList(Regex(r'[&.[\]0-9a-zA-Z_-]+')('args*'), delim='')
            + ')'
        )
        parsed = r.search_string(self.doc or '')
        if len(parsed) == 0:
            return False
        args = parsed[0]['args']  # type: ignore
        self.arg_names = [
            self.arg_name(arg.lower(), '-') for arg in args
            if arg != '&optional' and arg != '&rest'
        ]
        return True

    def check_args(self):
        args = max(0, self.max_args, self.min_args)
        many = self.max_args == ARG_MANY
        if self.max_args == ARG_UNEVALLED:
            assert self.name in SPECIAL_FORM_VARARGS, self
            varargs = SPECIAL_FORM_VARARGS[self.name]
            if isinstance(varargs, bool) and varargs:
                assert '...' in self.doc or '&rest' in self.doc, self
                many = True
            else:
                assert varargs >= args
                args = varargs
        self.arg_names = [
            self.arg_name(arg.lower(), '_') for arg in self.arg_names
        ]
        c_args = self.arg_names.copy()
        if not self.args_from_usage() or len(self.arg_names) < args:
            if len(c_args) >= args:
                self.arg_names = c_args
            else:
                assert False, (self, self.arg_names)
        if len(self.arg_names) != args:
            assert (
                (self.max_args == ARG_MANY or self.max_args == ARG_UNEVALLED)
                and len(self.arg_names) >= args + 1
            ), (self.arg_names, self)
        return args, many

    def args(self):
        args, many = self.check_args()
        s = ', '.join(f'Object {name}'for name in self.arg_names[:args])
        if many:
            if s != '':
                s += ', '
            s += f'Object[] {
                self.arg_names[-1]
                if len(self.arg_names) == args + 1 else 'args'
            }'
        return s

    def proper_name(self):
        name = self.lower(self.fname)
        if name in JAVA_KEYWORDS:
            return f'{name}_'
        return name

    def check_existing_args(self, body: str):
        s = self.args().replace('Object[]', '').replace('Object', '').replace(',', '')
        args = [n for n in s.split(' ') if n != '']
        r = (
            LineStart()
            + (
                Literal('@Specialization\n') |
                (Literal('@Specialization(') + SkipTo('\n'))
            )
            + SkipTo('\n')('line')
        )
        params = (
            SkipTo('(') + '('
            + (
                DelimitedList(
                    Word(alphanums + '[]') + Regex(r'[a-zA-Z0-9_]+')('args*'),
                    delim=',',
                ) | ')'
            )
        )
        impls = r.search_string(body)
        assert len(impls) >= 1, body
        for impl in impls:
            impl = impl['line'].strip()
            assert (
                impl.startswith('public static')
                or any(f in impl for f in WANTS_NODE_ACCESS)
            ), impl
            if 'VirtualFrame frame' in impl:
                args.insert(0, 'frame')
            p = params.search_string(impl)
            assert len(p) == 1, (impl, p)
            if 'args' not in p[0]:
                assert '()' in impl, (impl, args)
            else:
                assert list(p[0]['args']) == args, (impl, args)  # type: ignore

    def format_existing(self, name: str, fname: str, attrs: str,
                        extends: str, body: str):
        assert name == self.name
        assert fname == self.fname
        assert attrs == self.attrs(), (self.name, attrs, self.attrs())
        assert (
            extends == 'extends ELispBuiltInBaseNode '
            or extends == 'extends ELispBuiltInBaseNode '
            'implements ELispBuiltInBaseNode.InlineFactory '
        )
        assert '@Specialization' in body
        self.check_existing_args(body)
        return f'''
    {self.javadoc()}
    @ELispBuiltIn(name = "{name}"{attrs})
    @GenerateNodeFactory
    public abstract static class {fname} {extends}{{
        {body}
    }}'''

    def javadoc(self):
        self.doc = self.doc or ''
        return f'''/**
     * <pre>
{'\n'.join(
    '     *' if l.strip() == '' else f'     * {l}'
    for l in escape_xml(self.doc).splitlines()
    )}
     * </pre>
     */'''

    def attrs(self):
        upper = max(0, self.min_args) if self.max_args < 0 else self.max_args
        if self.max_args == ARG_MANY:
            varargs = ', varArgs = true'
        elif self.max_args == ARG_UNEVALLED:
            if isinstance(
                SPECIAL_FORM_VARARGS[self.name], bool
            ) and SPECIAL_FORM_VARARGS[self.name]:
                varargs = ', varArgs = true'
            else:
                upper = SPECIAL_FORM_VARARGS[self.name]
                varargs = ', varArgs = false'
        else:
            varargs = ''
        return f''', minArgs = {max(0, self.min_args)}, \
maxArgs = {upper}\
{varargs}\
{", rawArg = true" if self.max_args == ARG_UNEVALLED else ""}'''

    def format_java(self):
        return f'''
    {self.javadoc()}
    @ELispBuiltIn(name = "{self.name}"{self.attrs()})
    @GenerateNodeFactory
    public abstract static class {self.fname} extends ELispBuiltInBaseNode {{
        @Specialization
        public static Void {self.proper_name()}({self.args()}) {{
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
            return 'Object', 'false /* uninitialized */'
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
        if t == 'long':
            v = f'{int(v):_}'
        if t == 'boolean':
            if v == '1':
                v = 'true'
            elif v == '0':
                v = 'false'
            value = v
        else:
            value = f'({t}) {v}'
        if value == '(Object) NIL':
            value = 'false'
        return (
            f'ELispSymbol.Value.Forwarded {self.jname()} = '
            f'new ELispSymbol.Value.Forwarded({value})'
        )

    def init(self):
        return f'{self.symbol_jname()}.forwardTo({self.jname()})'


def arg_count(n: int):
    # if n.startswith('charset_arg_'):
    #     # TODO: We will probably handle this at runtime
    if n == 17 or n == 13:
        return 0
    assert -2 <= n and n <= 8, n
    if n == -1:  # 'MANY'
        return ARG_MANY
    if n == -2:  # 'UNEVALLED'
        return ARG_UNEVALLED
    return n


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
    extracted = extract_defuns(Path(args.filename))
    not_found = set(detected) - set(m['lname'] for m in extracted)
    if not_found != {'testme'}:
        assert len(not_found) == 0, not_found
    subroutines = dict(
        (
            LispSubroutine.better_fname(f['fnname']),
            (
                i,
                LispSubroutine(
                    f['lname'],
                    LispSubroutine.better_fname(f['fnname']),
                    f['sname'],
                    [] if f['args'] is None else f['args'],
                    arg_count(f['minargs']),
                    arg_count(f['maxargs']),
                    f['intspec'],
                    f['doc'],
                ),
            ),
        )
        for i, f
        in enumerate(extracted)
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
        'Vface_ignored_fonts',
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
        'CURRENT_TIME_LIST': True,
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
    if Path(args.filename).stem not in ['search', 'timefns']:
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
            elif c_name == 'Vface_new_frame_defaults':
                init_value = {'raw': 'new ELispHashtable()'}
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
JAVA_NODE_MATCH = (
    LineStart()
    + Literal('@ELispBuiltIn(name =')
    + QuotedString('"')('name')
    + SkipTo(')')('attrs') + ')'
    + Literal('@GenerateNodeFactory')
    + Literal('public abstract static class')
    + Word('F', alphanums)('fname')
    + SkipTo('{')('extends') + '{'
    + SkipTo('\n    }\n')('body')
)
assert args.java.endswith('.java')


with open(args.java, 'r') as f:
    contents = f.read()

    existing = dict(
        (m['fname'], m)
        for m in JAVA_NODE_MATCH.search_string(contents)
    )
    assert (
        len(set(JAVA_NODE_DETECT.findall(contents))) == len(existing)
    ), existing
    extra_functions = existing.keys() - subroutines.keys()
    assert len(extra_functions) == 0, extra_functions

    start = contents.find('\n    @ELispBuiltIn(name =')
    if start == -1:
        original = contents[0:contents.rindex('}')]
    else:
        original = contents[0:start]
        comment_end = original.rfind('*/')
        if original[comment_end:].strip() == '*/':
            trailing_comment_start = original.rfind('\n    /**\n')
            assert trailing_comment_start != -1
            original = original[0:trailing_comment_start]
    for _, subroutine in sorted(subroutines.values(), key=lambda v: v[0]):
        if subroutine.fname in existing:
            info = existing[subroutine.fname]
            original += subroutine.format_existing(
                name=info['name'],
                attrs=info['attrs'],
                fname=info['fname'],
                extends=info['extends'],
                body=info['body'],
            )
        else:
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
