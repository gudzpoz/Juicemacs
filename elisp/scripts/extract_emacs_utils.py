import re
import subprocess
import typing


def replace_or_insert_region_general(
        contents: str,
        marker: str,
        update: str,
        generator: str,
        indents: int = 4,
        insertion: bool = True
):
    section_start = (
        f'{' ' * indents}/* @generated region="{marker}" by="{generator}" */\n'
    )
    section_end = (
        f'{' ' * indents}/* @end region="{marker}" */\n'
    )
    if not insertion or section_start in contents:
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


def replace_region_general(
        contents: str,
        marker: str,
        update: str,
        generator: str,
        indents: int = 4
):
    return replace_or_insert_region_general(
        contents, marker, update, generator, indents, insertion=False
    )


def extract_syms_section(
        fname: str,
        content: str,
):
    assert fname.startswith('syms_of_')
    start = content.index(f'{fname} (void)\n{{')
    syms_body = content[start:]

    def prune_func(extra_f: str, syms_body: str):
        pdumper_start = syms_body.index(f'{extra_f} (void)\n{{')
        pdumper_body = syms_body[pdumper_start:]
        assert pdumper_body.endswith('\n}\n')
        assert '\n}\n' not in pdumper_body[:-1], pdumper_body
        end = syms_body[:pdumper_start].rfind('}')
        syms_body = syms_body[:end + 2]
        return syms_body

    if fname == 'syms_of_keyboard':
        syms_body = prune_func('mark_kboards', syms_body)
        syms_body = prune_func('keys_of_keyboard', syms_body)
    if f'{fname}_for_pdumper (void)' in syms_body:
        syms_body = prune_func(f'{fname}_for_pdumper', syms_body)

    assert f'{fname} (void)' not in syms_body[1:]
    if not syms_body.endswith('\n}\n'):
        syms_body = syms_body[:syms_body.rindex('\n}\n') + 3]
    assert syms_body.endswith('\n}\n')
    assert '\n}\n' not in syms_body[:-1]
    syms_body = syms_body.replace(f'{fname} (void)\n{{', '')
    syms_body = syms_body.replace('\n}\n', '')
    syms_body = re.sub(r'^\s+', '', syms_body, flags=re.MULTILINE)
    syms_body = re.sub(r'^;$', '', syms_body, flags=re.MULTILINE)
    return syms_body


def remove_statements(contents: str, statements: list[str], extra_replaces: dict[str, str] = {}):
    for k, v in extra_replaces.items():
        r = re.compile(k, re.MULTILINE)
        contents, n = r.subn(v, contents)
        assert n > 0, (k, v, contents)

    for statement in statements:
        assert statement in contents
        start = contents.index(f'\n{statement}')
        end = contents[start:].index(';\n') + start + 1
        contents = contents[:start + 1] + contents[end:]

    return contents


VAR_DECLARATION = re.compile(
    r'^(Lisp_Object) (\w+)(, \w+)*;$',
    re.MULTILINE
)


def exec_c_as_python(
        src: str,
        c_globals: typing.Dict[str, typing.Any],
        missing: typing.Callable[[str], typing.Any],
        prepocessors: typing.Optional[str] = None,
):
    if prepocessors is not None:
        p = subprocess.Popen(
            ['gcc', '-E', '-'],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE
        )
        stdout, stderr = p.communicate(f'{prepocessors}\n{src}'.encode('utf-8'))
        assert stderr is None, stderr
        src = stdout.decode('utf-8')
        src = re.sub(r'^\s+', '', src, flags=re.MULTILINE)
        src = re.sub(r'\s+$', '', src, flags=re.MULTILINE)
        src = re.sub(r'^;$', '', src, flags=re.MULTILINE)
        src = src.replace('\n=', '=')
        src = src.replace('=\n', '=')
        src = re.sub('^pass;?$', '', src, flags=re.MULTILINE)
        src = re.sub('^#.*$', '', src, flags=re.MULTILINE)
        src = re.sub(r'([^;])\n', r'\1', src, flags=re.MULTILINE)
    src = VAR_DECLARATION.sub('', src)

    class LazyDict(dict):
        def __missing__(self, key: str):
            return missing(key)
        def __setitem__(self, k: typing.Any, v: typing.Any):
            nonlocal c_globals
            c_globals[k] = v
            return super(LazyDict, self).__setitem__(k, v)

    try:
        exec(src, LazyDict(c_globals))
    except SyntaxError as e:
        print(src)
        raise e
