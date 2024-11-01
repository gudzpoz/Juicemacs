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
        if value == '(Object) NIL' or value == '(Object) false':
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

        'CALLN': lambda f, *args: f(len(args), args),

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
        'ASET': lambda vec, idx, val: post_inits.append(f'{vec}.set({idx}, {val});'),

        'Fmake_symbol': lambda s: f'FMakeSymbol.makeSymbol({s})',
        'Fmake_hash_table': lambda _, args: f'FMakeHashTable.makeHashTable(new Object[]{{{", ".join(args)}}})',
        'Fmake_marker': lambda: 'FMakeMarker.makeMarker()',
        'Fpurecopy': lambda x: f'FPurecopy.purecopy({x})',

        'Fmake_char_table': lambda *args: f'FMakeCharTable.makeCharTable({", ".join(args)})',
        'char_table_set_range': lambda table, start, end, val: post_inits.append(f'{table}.setRange({start}, {end}, {val});'),
        'Fset_char_table_range': lambda table, ran, val: post_inits.append(f'FSetCharTableRange.setCharTableRange({table}, {ran}, {val});'),
        'Fset_char_table_extra_slot': lambda table, idx, val: post_inits.append(f'{table}.setExtra((int) {idx}, {val});'),

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
        'character': {
            'globals': {
                'MAX_CHAR': 0x3FFFFF,
                'MAX_5_BYTE_CHAR': 0x3FFF7F,
                'CHAR_TABLE_SET': lambda table, char, val: post_inits.append(
                    f'{table}.setChar({ord(char)}, {val});',
                ),
            },
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
        'coding': {
            'extra_replaces': {
                r'Vcoding_category_table = make_nil_vector \(coding_category_max\);':
                    'py_def_var("ELispVector", "codingCategoryTable", make_nil_vector (coding_category_max))',
                r'\{\s+int i;\s+Vcoding_category_list = Qnil;[^}]+?\}':
                    'Vcoding_category_list = py_init_coding_category_list(Vcoding_category_table);',
                r'Lisp_Object args\[coding_arg_undecided_max\];[^}]+?\}[^}]+'
                r'Fset \(AREF \(Vcoding_category_table, i\), Qno_conversion\);':
                    'py_setup_coding_system("""\1""")',
            },
            'globals': {
                'Vcoding_category_table': 'codingCategoryTable',
                'coding_category_iso_7': 'CODING_CATEGORY_ISO_7',
                'coding_category_iso_7_tight': 'CODING_CATEGORY_ISO_7_TIGHT',
                'coding_category_iso_8_1': 'CODING_CATEGORY_ISO_8_1',
                'coding_category_iso_8_2': 'CODING_CATEGORY_ISO_8_2',
                'coding_category_iso_7_else': 'CODING_CATEGORY_ISO_7_ELSE',
                'coding_category_iso_8_else': 'CODING_CATEGORY_ISO_8_ELSE',
                'coding_category_utf_8_auto': 'CODING_CATEGORY_UTF_8_AUTO',
                'coding_category_utf_8_nosig': 'CODING_CATEGORY_UTF_8_NOSIG',
                'coding_category_utf_8_sig': 'CODING_CATEGORY_UTF_8_SIG',
                'coding_category_utf_16_auto': 'CODING_CATEGORY_UTF_16_AUTO',
                'coding_category_utf_16_be': 'CODING_CATEGORY_UTF_16_BE',
                'coding_category_utf_16_le': 'CODING_CATEGORY_UTF_16_LE',
                'coding_category_utf_16_be_nosig': 'CODING_CATEGORY_UTF_16_BE_NOSIG',
                'coding_category_utf_16_le_nosig': 'CODING_CATEGORY_UTF_16_LE_NOSIG',
                'coding_category_charset': 'CODING_CATEGORY_CHARSET',
                'coding_category_sjis': 'CODING_CATEGORY_SJIS',
                'coding_category_big5': 'CODING_CATEGORY_BIG5',
                'coding_category_ccl': 'CODING_CATEGORY_CCL',
                'coding_category_emacs_mule': 'CODING_CATEGORY_EMACS_MULE',
                'coding_category_raw_text': 'CODING_CATEGORY_RAW_TEXT',
                'coding_category_undecided': 'CODING_CATEGORY_UNDECIDED',
                'coding_category_max': 'CODING_CATEGORY_MAX',
                'py_def_var': lambda t, name, v: post_inits.append(f'{t} {name} = {v};'),
                'py_init_coding_category_list': lambda table: f'false /* TODO */',
                'py_setup_coding_system': lambda s: post_inits.append('// TODO: setup coding system'),
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
        'frame': {
            'globals': {
                'py_init_x_frame_parameter': lambda: post_inits.extend(
                    f'FPut.put({prop.replace('-', '_').upper()}, X_FRAME_PARAMETER, (long) {i});'
                    for i, prop in enumerate([
                        # static const struct frame_parm_table frame_parms[] =
                        "auto-raise",
                        "auto-lower",
                        "background-color",
                        "border-color",
                        "border-width",
                        "cursor-color",
                        "cursor-type",
                        "font",
                        "foreground-color",
                        "icon-name",
                        "icon-type",
                        "child-frame-border-width",
                        "internal-border-width",
                        "right-divider-width",
                        "bottom-divider-width",
                        "menu-bar-lines",
                        "mouse-color",
                        "name",
                        "scroll-bar-width",
                        "scroll-bar-height",
                        "title",
                        "unsplittable",
                        "vertical-scroll-bars",
                        "horizontal-scroll-bars",
                        "visibility",
                        "tab-bar-lines",
                        "tool-bar-lines",
                        "scroll-bar-foreground",
                        "scroll-bar-background",
                        "screen-gamma",
                        "line-spacing",
                        "left-fringe",
                        "right-fringe",
                        "wait-for-wm",
                        "fullscreen",
                        "font-backend",
                        "alpha",
                        "sticky",
                        "tool-bar-position",
                        "inhibit-double-buffering",
                        "undecorated",
                        "parent-frame",
                        "skip-taskbar",
                        "no-focus-on-map",
                        "no-accept-focus",
                        "z-group",
                        "override-redirect",
                        "no-special-glyphs",
                        "alpha-background",
                        "use-frame-synchronization",
                    ])
                ),
            },
            'extra_replaces': {
                r'\{\s*int i;[^{}]+\{[^{}]+\}\s+\}': 'py_init_x_frame_parameter()',
            },
        },
        'keyboard': {
            'globals': {
                'lispy_wheel_names': ["wheel-up", "wheel-down", "wheel-left", "wheel-right"],
                'lossage_limit': 300,
                'ord': ord,
                'init_while_no_input_ignore_events': lambda: 'false',
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
        'xdisp': {
            'globals': {
                'echo_buffer': [None] * 2,
                'echo_area_buffer': [None] * 2,
                'NULL': [],
                'DEFAULT_TAB_BAR_BUTTON_MARGIN': 1,
                'DEFAULT_TAB_BAR_BUTTON_RELIEF': 1,
                'DEFAULT_TOOL_BAR_BUTTON_MARGIN': 4,
                'DEFAULT_TOOL_BAR_BUTTON_RELIEF': 1,
                'DEFAULT_TOOL_BAR_LABEL_SIZE': 14,
                'DEFAULT_HOURGLASS_DELAY': 1,
            },
            'extra_replaces': {
                r'Lisp_Object icon_title_name_format': 'icon_title_name_format',
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
    variable_map = {
        v.c_name: v
        for v in [
            Variable(name, c_name, lisp_type, None)
            for lisp_type, name, c_name in matches
        ]
    }
    is_symnum = re.compile(r'^[0-9\.A-Za-z_]+$')
    def assign(key, value):
        nonlocal c_globals, variable_map
        c_globals[key] = value
        if key not in variable_map:
            return value
        if (
            isinstance(value, int)
            or isinstance(value, float)
            or isinstance(value, bool)
            or is_symnum.match(value)
        ):
            return value
        v = variable_map[key]
        if v.lisp_type == 'INT' or v.lisp_type == 'BOOL':
            return value
        jname = v.jname()
        post_inits.append(f'var {jname}JInit = {value};')
        post_inits.append(f'{jname}.setValue({jname}JInit);')
        c_globals[key] = 'NIL'
        return f'{jname}JInit'
    exec_c_as_python(
        init_section,
        c_globals,
        missing,
        assign,
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
