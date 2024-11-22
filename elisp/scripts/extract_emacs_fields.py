#!/bin/python

import argparse
import re
import typing

from pathlib import Path

from extract_emacs_init import DEFVAR_BUF_REGEX
from extract_emacs_utils import (
    exec_c_as_python,
    preprocess_c,
    replace_or_insert_region_general,
)


parser = argparse.ArgumentParser(
    prog='extract_emacs_fields',
    description='Extract various Emacs fields from various sources',
)
parser.add_argument('emacs_src_dir')
parser.add_argument('output_file')
parser.add_argument('-b', '--buffer', required=True)
args = parser.parse_args()

emacs_src_dir = Path(args.emacs_src_dir)
output_file = Path(args.output_file)
output_buffer_file = Path(args.buffer)


def extract_enum_section(enum_name: str, src: str):
    enum_start = re.compile(f'^enum {re.escape(enum_name)}$\\s+\\{{', re.MULTILINE)
    match = next(enum_start.finditer(src))
    assert match is not None, enum_start.pattern
    start = match.end() - 1
    end = src.index('}', start) + 1
    src = preprocess_c(f'{src[start:end]}\n').strip()
    src = re.sub('^#.*$', '', src, flags=re.MULTILINE)
    assert src[-1] == '}'
    return src


EMACS_ENUMS = [
    ('charset.h', 'define_charset_arg_index'),
    ('charset.h', 'charset_attr_index'),
    ('coding.c', 'coding_category'),
    ('coding.h', 'coding_attr_index'),
    ('coding.h', 'define_coding_system_arg_index'),
    # These define_coding_*_arg_index depend on define_coding_system_arg_index
    ('coding.h', 'define_coding_iso2022_arg_index'),
    ('coding.h', 'define_coding_utf8_arg_index'),
    ('coding.h', 'define_coding_utf16_arg_index'),
    ('coding.h', 'define_coding_ccl_arg_index'),
    ('coding.h', 'define_coding_undecided_arg_index'),
    ('lisp.h', 'Lisp_Closure'),
    ('syntax.h', 'syntaxcode'),
]
EMACS_DEFINES = [
    ('coding.c', r'CODING_ISO_FLAG_\w+'),
]


def extract_enums(original: str):
    global_vars = {}
    for file, enum_name in EMACS_ENUMS:
        with open(emacs_src_dir / file, 'r') as f:
            src = f.read()
            section = extract_enum_section(enum_name, src).strip()
            assert section.startswith('{'), (enum_name, section)
            assert section.endswith('}'), (enum_name, section)
            enum_items = [i.strip() for i in section[1:-1].split(',')]
            enum_names = [i.split('=')[0].strip() for i in enum_items]
            enum_evals = [
                (f'{item} = inc_i()' if '=' not in item else
                 f'{item.split('=')[0]} = inc_i({item.split("=")[1]})').strip()
                for item in enum_items
            ]
            def missing(k: str):
                raise Exception(f'Could not find enum item {k} in {enum_name}')
            def assign(k: typing.Any, v: typing.Any):
                global_vars[k] = v
                return v
            exec_c_as_python(f'''
i = 0
def inc_i(x=None):
    global i
    if x is not None:
        i = x
    ret = i
    i += 1
    return ret
{'\n'.join(enum_evals)}
''', global_vars, missing, assign, None)
            enum_values = [
                (name, global_vars[name])
                for name in enum_names
            ]
            original = replace_or_insert_region_general(
                original,
                f'enum {enum_name}',
                ''.join(
                    f'    public final static int {name.upper()} = {i};\n'
                    for name, i in enum_values
                ),
            )
    return original


def extract_defines(original: str):
    for file, regexp in EMACS_DEFINES:
        with open(emacs_src_dir / file, 'r') as f:
            src = f.read()
            defines = re.compile(
                f'^#define\\s+({regexp})\\s+(\\w+)',
                re.MULTILINE,
            ).findall(src)
            original = replace_or_insert_region_general(
                original,
                f'define {regexp}',
                ''.join(
                    f'    public final static int {name.upper()} = {i};\n'
                    for name, i in defines
                ),
            )
    return original


######################
### Enum Constants ###
######################


with open(output_file, 'r') as f:
    original = f.read()

output = extract_enums(original)
output = extract_defines(original)

with open(output_file, 'w') as f:
    f.write(output)


def extract_struct_section(struct_name: str, src: str, preprocessors: str):
    struct_start = f'\nstruct {struct_name}\n{{'
    start = src.index(struct_start) + len(struct_start) - 1
    end = src.index('};', start) + 2
    src = preprocess_c(f'{src[start:end]}\n', preprocessors).strip()
    src = re.sub('^#.*$', '', src, flags=re.MULTILINE)
    assert src.endswith('};'), (struct_name, src)
    return src


def extract_function(f_sig: str, src: str, preprocessors: str):
    f_start = f'\n{f_sig}\n{{\n'
    start = src.index(f_start) + len(f_start) - 2
    end = src.index('\n}\n', start) + 2
    src = preprocess_c(f'{src[start:end]}\n', preprocessors).strip()
    src = re.sub('^#.*$', '', src, flags=re.MULTILINE)
    assert src.endswith('}'), (f_sig, src)
    return src


def extract_buffer_fields():
    with open(emacs_src_dir / 'buffer.h', 'r') as f:
        buffer_h = f.read()
    buffer_fields = extract_struct_section('buffer', buffer_h, '''
#define HAVE_TREE_SITTER 1
''')
    fields: list[str] = re.findall(r'^  Lisp_Object ([A-Za-z_]+)_;$', buffer_fields, re.MULTILINE)
    init = f'Collections.nCopies({len(fields)}, false).toArray()'
    container = f'''    private final Object[] bufferLocalFields;
    private final static Object[] BUFFER_DEFAULTS = {init};
    private final static byte[] BUFFER_LOCAL_FLAGS = new byte[{len(fields)}];
    private final static byte[] BUFFER_PERMANENT_LOCAL_FLAGS = new byte[{len(fields)}];
    private final static ELispSymbol[] BUFFER_LOCAL_SYMBOLS = new ELispSymbol[{len(fields)}];
'''
    indices = []
    accessors = []
    for i, name in enumerate(fields):
        indices.append(f'    public final static int BVAR_{name.upper()} = {i};')
        jname = ''.join(seg.capitalize() for seg in name.split('_'))
        accessors.append(f'''
    public Object get{jname}() {{ return bufferLocalFields[BVAR_{name.upper()}]; }}
    public void set{jname}(Object value) {{ bufferLocalFields[BVAR_{name.upper()}] = value; }}''')
    return f'{container}{'\n'.join(indices)}{''.join(accessors)}\n', fields


def extract_init_buffer_once(fields: list[str]):
    with open(emacs_src_dir / 'buffer.c', 'r') as f:
        buffer_c = f.read()
    init_buffer = extract_function('init_buffer_once (void)', buffer_c, '''
#define XSETFASTINT(a, b) (a) = (b)
#define BVAR(a, b) (a)[#b]
#define PDUMPER_REMEMBER_SCALAR(a) pass
#define pdumper_remember_lv_ptr_raw(a, b) pass
#define memset(a, b, c) pass

#define reset_buffer_local_variables(buffer, b) py_reset_buffer_local_variables(#buffer, b)
''').strip()
    assert init_buffer.startswith('{') and init_buffer.endswith('}'), init_buffer
    init_buffer = re.sub(r'^\s+', '', init_buffer[1:-1], flags=re.MULTILINE)
    init_buffer = init_buffer.replace('=\n', '=')
    init_buffer = init_buffer.replace('int idx;', '')
    init_buffer = init_buffer.replace(
        'if (idx >= MAX_PER_BUFFER_VARS)',
        'if (idx >= MAX_PER_BUFFER_VARS):'
    )
    init_buffer = init_buffer.replace('emacs_abort ();', '  emacs_abort ();')
    init_buffer = init_buffer.replace(
        '{ static_assert (sizeof (EMACS_INT) == word_size); }', '',
    )
    init_buffer = init_buffer.replace('&buffer_local_flags', 'buffer_local_flags')
    init_buffer = init_buffer.replace('&buffer_defaults', 'buffer_defaults')
    init_buffer = init_buffer.replace('&buffer_local_symbols', 'buffer_local_symbols')
    init_buffer = init_buffer.replace('++idx', 'inc_idx(idx)')
    init_buffer = init_buffer.replace('idx++', 'idx_inc(idx)')
    init_buffer = '''
def inc_idx(idx_):
    global idx
    assert idx_ == idx
    idx += 1
    return idx
def idx_inc(idx_):
    global idx
    assert idx_ == idx
    idx += 1
    return idx_
def eassert(cond):
    assert cond
''' + init_buffer
    ID = lambda x: x
    class Buffer(dict):
        def __init__(self):
            self.text = None
            self.own_text = None
            self.indirections = None
            self.window_count = None
            for name in fields:
                self[name] = None
    post_inits = []
    def reset_buffer(buffer: dict[str, typing.Any]):
        for name in fields:
            buffer[name] = None
    def reset_buffer_locals(buffer: str, b: int):
        if buffer == 'buffer_defaults':
            assert b == 1
            post_inits.append(
                f'DEFAULT_VALUES.resetLocalVariables({"true" if int(b) != 0 else "false"})'
            )
        else:
            assert buffer == 'buffer_local_symbols'
            # I don't think it is necessary to reset buffer_local_symbols
    symbols = {}
    buffers = {
        'buffer_local_flags': Buffer(),
        'buffer_local_symbols': Buffer(),
        'buffer_permanent_local_flags': Buffer(),
        'buffer_defaults': Buffer(),
    }
    c_globals = {
        'MAX_PER_BUFFER_VARS': 50,
        'make_fixnum': ID,
        'build_pure_c_string': lambda s: f'new ELispString("{s}")',
        'Fput': lambda sym, prop, val: post_inits.append(f'FPut.put({sym}, {prop}, {val})'),
        'Fget_buffer_create': lambda buff, flag: f'FGetBufferCreate.getBufferCreate({buff}, {flag})',
        'Fset_buffer': lambda buff: post_inits.append(f'FSetBuffer.setBuffer({buff})'),
        'NULL': None,
        'Qnil': 'false',
        'Qt': 'true',
        'DEFSYM': symbols.__setitem__,
        'BUFFER_PVEC_INIT': ID,
        'set_buffer_intervals': lambda *_: None,
        'reset_buffer': reset_buffer,
        'py_reset_buffer_local_variables': reset_buffer_locals,
        'NILP': lambda x: x is None,
    }
    c_globals.update(buffers)
    def missing(name: str):
        if name.startswith('bset_'):
            var = name[5:]
            assert var in fields, var
            return lambda o, v: o.__setitem__(var, v)
        if name.startswith('Q'):
            return name[1:].upper()
        raise KeyError(name)
    def assign(name, value):
        c_globals[name] = value
        return value
    exec_c_as_python(
        init_buffer,
        c_globals,
        missing,
        assign,
        None,
    )
    def init_array(array: str, buffer: dict[typing.Any, typing.Any]):
        inits = []
        for name, value in buffer.items():
            if value is not None:
                if array == 'BUFFER_DEFAULTS' and isinstance(value, int):
                    value = f'{value}L'
                if isinstance(name, str):
                    name = f'BVAR_{name.upper()}'
                inits.append(f'        {array}[{name}] = {value};\n')
        return ''.join(inits)
    post_init_body = ''.join(f'        {line};\n' for line in post_inits)
    buffers['buffer_local_symbols']['name'] = None
    for name, buffer in buffers.items():
        post_init_body = init_array(name.upper(), buffer) + post_init_body
    return post_init_body


def extract_defvar_per_buffer(fields: list[str]):
    with open(emacs_src_dir / 'buffer.c', 'r') as f:
        buffer_c = f.read()
    buffer_locals = DEFVAR_BUF_REGEX.findall(buffer_c)
    inits = []
    for name, c_name, predicate in buffer_locals:
        assert predicate.startswith('Q')
        predicate = predicate[1:].upper()
        index = fields.index(c_name)
        if len(name) == len(c_name):
            jname = c_name.upper()
        else:
            jname = name.upper().replace('-', '_')
        container = f'new ELispSymbol.Value.ForwardedPerBuffer({index}, {predicate})'
        inits.append(f'''        {jname}.initForwardTo({container}); // {name}
        BUFFER_LOCAL_SYMBOLS[{index}] = {jname};
''')
    return ''.join(inits)


#########################
### Buffer-Local Vars ###
#########################


with open(output_buffer_file, 'r') as f:
    original = f.read()

field_def, fields = extract_buffer_fields()
original = replace_or_insert_region_general(
    original,
    'struct buffer',
    field_def,
    insertion=False,
)
buffer_locals = extract_defvar_per_buffer(fields)
original = replace_or_insert_region_general(
    original,
    'DEFVAR_PER_BUFFER',
    buffer_locals,
    indents=8,
    insertion=False,
)
init = extract_init_buffer_once(fields)
original = replace_or_insert_region_general(
    original,
    'init_buffer_once',
    init,
    indents=8,
    insertion=False,
)

with open(output_buffer_file, 'w') as f:
    f.write(original)
