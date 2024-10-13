# %%
#!/bin/python

import argparse
import re
import subprocess

from pathlib import Path

# %%
parser = argparse.ArgumentParser()
parser.add_argument('--input', '-i', type=str, required=True)
parser.add_argument('--output', '-o', type=str, required=True)
args = parser.parse_args()

# %%
data_c: str = args.input
assert Path(data_c).name == 'data.c', data_c
with open(data_c, 'r') as f:
    data = f.read()
    start = data.index('syms_of_data')
    syms_of_data = data[start:]
    assert 'syms_of_data' not in syms_of_data[1:]
    assert syms_of_data.endswith('\n}\n')
    assert '\n}\n' not in syms_of_data[:-1]

start = syms_of_data.index('error_tail = pure_cons (Qerror, Qnil);')
assert 'PUT_ERROR' not in syms_of_data[:start]

# %%
# Pass syms_of_data to GCC preprocessor
p = subprocess.Popen(
    ['gcc', '-E', '-'],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE
)
stdout, stderr = p.communicate(f'''
#define DEFSYM(a, b) pass
#define DEFVAR(a, b) pass
#define DEFVAR_LISP(a, b, c) pass
#define DEFVAR_BOOL(a, b, c) pass
#define defsubr(a) pass
{syms_of_data}
'''.encode('utf-8')
)
assert stderr is None, stderr
syms_body = stdout.decode('utf-8')
syms_body = syms_body.replace('syms_of_data (void)\n{', '')
syms_body = syms_body.replace('}\n', '')
assert 'Lisp_Object error_tail, arith_tail, recursion_tail;' in syms_body
syms_body = syms_body.replace('Lisp_Object error_tail, arith_tail, recursion_tail;', '')
syms_body = re.sub(r'^\s+', '', syms_body, flags=re.MULTILINE)
syms_body = re.sub(r'^;$', '', syms_body, flags=re.MULTILINE)

# %%
class LazyDict(dict):
    def __missing__(self, key: str):
        if key == 'Qnil':
            return 'false'
        if key == 'Qt':
            return 'true'
        if key[0] == 'Q':
            return f'{key[1:].upper()}'
        if key == 'false':
            return 'false'
        raise Exception(f'unsupported: {key}')

java_body = ''
def f_put(sym, prop, v):
    global java_body
    java_body += f'        {sym}.putProperty({prop}, {v});\n'

c_globals = LazyDict({
    'pure_cons': lambda a, b: f'new ELispCons({a}, {b})',
    'Fcons': lambda a, b: f'new ELispCons({a}, {b})',
    'build_pure_c_string': lambda a: f'new ELispString("{a}")',
    'make_fixnum': lambda a: f'{a}L',
    'Fput': f_put,
    'MOST_POSITIVE_FIXNUM': 0,
    'MOST_NEGATIVE_FIXNUM': 0,
    'make_symbol_constant': lambda _: None,
    'intern_c_string': lambda _: None,
})
exec(syms_body, c_globals)

# %%
def replace_region(contents: str, marker: str, update: str):
    section_start = (
        f'        /* @generated region="{marker}" by="extract-emacs-errors.py" */\n'
    )
    section_end = (
        f'        /* @end region="{marker}" */\n'
    )
    assert section_start in contents
    start = contents.index(section_start)
    end = contents.index(section_end)
    assert start < end
    return (
        f'{contents[:start]}{section_start}'
        f'{update}{contents[end:]}'
    )

with open(args.output, 'r') as f:
    contents = f.read()
    contents = replace_region(
        contents,
        'data.c:error_initialization',
        java_body
    )

with open(args.output, 'w') as f:
    f.write(contents)
