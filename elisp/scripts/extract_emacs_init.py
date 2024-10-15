import dataclasses
import json
import re
import typing

from extract_emacs_utils import (
    extract_syms_section,
    remove_statements,
    exec_c_as_python,
)


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
            return 'Object', 'false /* TODO */'

    def format(self):
        t, default_v = self.jtype()
        v = self.init_value
        v = default_v if v is None else v
        if t == 'long':
            v = f'{int(v):_}'
        if t == 'boolean':
            if str(v) == '1':
                v = 'true'
            elif str(v) == '0':
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


def exec_init_func(stem: str, contents: str):
    contents = re.sub(r'#if 0\n.*?\n#endif', '', contents, flags=re.DOTALL)
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

    post_inits: list[str] = []
    def ID(i):  # identity
        return i
    def STR(s):
        return f'new ELispString({json.dumps(s)})'
    def LONG(i):
        return f'(long) ({i})'
    def Fadd_variable_watcher(var, val):
        ignored = [
            'GC_CONS_PERCENTAGE',
            'GC_CONS_THRESHOLD',
        ]
        assert var in ignored
    intern = lambda sym: f'ELispContext.intern({json.dumps(sym)})'
    c_globals = {
        # Constants
        'EMACS_CONFIGURATION': '',
        'EMACS_CONFIG_OPTIONS': '',
        'EMACS_CONFIG_FEATURES': '',
        'emacs_copyright': 'TODO: Copy over GPL',
        'emacs_version': '30.0',
        'emacs_bugreport': '',
        'empty_unibyte_string': 'new ELispString("")',
        'true': 'true',
        'false': 'false',
        'MOST_POSITIVE_FIXNUM': 'Long.MAX_VALUE',
        'MOST_NEGATIVE_FIXNUM': 'Long.MIN_VALUE',

        'make_float': ID,
        'make_fixnum': LONG,
        'make_int': LONG,
        'build_pure_c_string': STR,
        'build_string': STR,
        'build_unibyte_string': STR,
        'intern': intern,
        'intern_c_string': intern,
        'make_symbol_constant': lambda sym: post_inits.append(f'{sym}.setConstant(true);'),

        'Fcons': lambda a, b: f'new ELispCons({a}, {b})',
        'pure_cons': lambda a, b: f'new ELispCons({a}, {b})',
        'pure_list': lambda *items: f'ELispCons.listOf({', '.join(items)})',
        'list1': lambda a: f'new ELispCons({a})',
        'list2': lambda a, b: f'ELispCons.listOf({a}, {b})',
        'list3': lambda a, b, c: f'ELispCons.listOf({a}, {b}, {c})',
        'list4': lambda a, b, c, d: f'ELispCons.listOf({a}, {b}, {c}, {d})',
        'ARRAYELTS': len,
        'make_nil_vector': lambda length: f'new ELispVector(Collections.nCopies({length}, false))',
        'make_vector': lambda length, fill: f'new ELispVector(Collections.nCopies({length}, {fill}))',

        'Fmake_symbol': lambda s: f'FMakeSymbol.makeSymbol({s})',
        'Fmake_hash_table': lambda *args: f'FMakeHashTable.makeHashTable(new Object[]{{{", ".join(args)}}})',
        'Fmake_marker': lambda: 'FMakeMarker.makeMarker()',
        'Fpurecopy': lambda x: f'FPurecopy.purecopy({x})',

        'Fadd_variable_watcher': Fadd_variable_watcher,
        'Fmake_var_non_special': lambda sym: post_inits.append(f'{sym}.setSpecial(false);'),
        'Fmake_variable_buffer_local': lambda sym: post_inits.append(f'{sym}.setBufferLocal(true);'),
        'Fmake_sparse_keymap': lambda s: f'FMakeSparseKeymap.makeSparseKeymap({s})',
        'Fset': lambda sym, value: post_inits.append(f'{sym}.setValue({value});'),
        'Fput': lambda sym, prop, v: post_inits.append(f'{sym}.putProperty({prop}, {v});'),
        'Fprovide': lambda sym, version: post_inits.append(f'FProvide.provide({sym}, {version});'),
        'Funintern': lambda sym, _: post_inits.append(f'ELispContext.unintern({sym});'),
    }

    def missing(key: str):
        if key[0] == 'Q':
            return key[1:].upper()
        raise Exception(f'unsupported: {key}')
    init_section = extract_syms_section(f'syms_of_{stem}', contents)
    file_specifics = {
        'alloc': {
            'globals': {'watcher': None},
            'invalid_statements': [
                'symbols_consed +=',
                'static union Aligned_Lisp_Subr Swatch_gc_cons_threshold =',
                'static union Aligned_Lisp_Subr Swatch_gc_cons_percentage =',
            ]
        },
        'charset': {
            'globals': {
                'charset_table_init': [],
                'define_charset_internal': (
                    lambda *args: post_inits.append(f'defineCharsetInternal({", ".join(
                        [args[0]] + [json.dumps(arg) for arg in args[1:5]]
                        + [str(ord(args[5]) if isinstance(args[5], str) else args[5])]
                        + [json.dumps(arg) for arg in args[6:]]
                    )});')
                ),
                'MAX_UNICODE_CHAR': 0x10FFFF,
                'MAX_5_BYTE_CHAR': 0x3FFF7F,
            },
        },
        'comp': {
            'invalid_statements': [
                'comp.exported_funcs_h = Qnil',
                'comp.imported_funcs_h = Qnil',
                'comp.emitter_dispatcher = Qnil',
            ],
        },
        'editfns': {
            'extra_replaces': {
                r'\{\s+Lisp_Object obuf;[^}]+?\}': '',
            },
        },
        'emacs': {
            'globals': {'SYSTEM_TYPE': 'jvm'},
            'extra_replaces': {
                r'\{\s+char c = SEPCHAR;\s+Vpath_separator = [^\n]+?\n[^\n]*?\}':
                    'Vpath_separator = build_pure_c_string(":");',
            },
        },
        'keyboard': {
            'globals': {
                'lispy_wheel_names': ["wheel-up", "wheel-down", "wheel-left", "wheel-right"],
                'lossage_limit': 300,
                'ord': ord,
                'init_while_no_input_ignore_events': lambda: None,
            },
            'extra_replaces': {
                r', 033\)': ', 0o33)',
                r'\{\s*int i;\s+for[^}]+?\}\s+\}': '',
                r'\{\s*int i;[^}]+?\}': '',
                re.escape(r'XSYMBOL (Qtop_level)->u.s.declared_special = false;'):
                    'Fmake_var_non_special(Qtop_level);',
                r"' '": str(ord(' ')),
            },
        },
        'lread': {
            'globals': {'PATH_DUMPLOADSEARCH': ''},
            'extra_replaces': {
                re.escape('XBARE_SYMBOL (intern ("values"))->u.s.declared_special = false;'):
                    'Fmake_var_non_special(intern("values"));',
                r'Vsource_directory\s+=[^;]+;': 'Vsource_directory = build_pure_c_string("");',
            },
        },
        'process': {
            'invalid_statements': [
                'const struct socket_options',
                'for (sopt = socket_options; sopt->name; sopt++)',
            ],
            'extra_replaces': {
                r'(Fprovide[^\n]+?subfeatures\);)\n\s*\}': r'\1',
                r'{\s+Lisp_Object subfeatures = Qnil;': 'subfeatures = Qnil;',
            },
        },
        'search': {
            'extra_replaces': {
                r'for \(int i = 0; i < REGEXP_CACHE_SIZE; \+\+i\)[^}]+?\}': '',
            }
        },
        'timefns': {
            'globals': {
                'TIMESPEC_HZ': 1000000000,
                'CURRENT_TIME_LIST': 'true',
                'flt_radix_power_size': 1075,
            },
        },
        'xfaces': {
            'extra_replaces': {
                re.escape('make_hash_table (&hashtest_eq, 33, Weak_None, false)'):
                    '"new ELispHashtable()"',
            },
        }
    }
    init_section = remove_statements(
        init_section,
        file_specifics.get(stem, {}).get('invalid_statements', []),
        file_specifics.get(stem, {}).get('extra_replaces', {})
    )
    extra_globals = file_specifics.get(stem, {}).get('globals', {})
    c_globals.update(extra_globals)
    exec_c_as_python(
        init_section,
        c_globals,
        missing,
        '''
#define DEFSYM(a, b) pass
#define DEFVAR_INT(a, b, c) pass
#define DEFVAR_BOOL(a, b, c) pass
#define DEFVAR_LISP(a, b, c) pass
#define DEFVAR_KBOARD(a, b, c) pass
#define DEFVAR_PER_BUFFER(a, b, c, d) pass
#define XSETSUBR(a, b) pass
#define defsubr(a) pass
#define staticpro(a) pass
#define PDUMPER_REMEMBER_SCALAR(a) pass
#define pdumper_do_now_and_after_load(a) pass
#define XSETFASTINT(a, b) (a) = (b)
#define XSETINT(a,b) (a) = make_fixnum (b)
#define CALLN(f, ...) (f)(__VA_ARGS__)

// comp.c
#define HAVE_NATIVE_COMP 1

// lread.c
#define MSDOS 1
#define IEEE_FLOATING_POINT 1

// keyboard.c
#define Ctl(c) (ord(c)&0o37)

// process.c
#define subprocesses 1
        ''',
    )
    variables: list[Variable] = []
    for lisp_type, name, c_name in matches:
        if c_name in c_globals:
            variables.append(Variable(name, c_name, lisp_type, c_globals[c_name]))
        else:
            variables.append(Variable(name, c_name, lisp_type, None))
    return variables, post_inits