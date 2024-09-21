import argparse
import re
from pathlib import Path


parser = argparse.ArgumentParser()
parser.add_argument('--js', type=str, required=True)
parser.add_argument('--java', type=str, required=True)
args = parser.parse_args()


def output(path: str, content: str):
    with open(Path('/tmp').joinpath(Path(path).name).with_suffix('.txt'), 'w') as f:
        f.write(content)

js_substitutions = {
    'charCodeAt': 'charAt',
    'CharCode\\.CarriageReturn': r"'\\r'",
    'CharCode\\.LineFeed': r"'\\n'",
    '===': '==',
    '!==': '!=',
    '!\\.': '.',
    r'leftest\(([^()]+)\)': r'\1.leftest()',
    r'righttest\(([^()]+)\)': r'\1.rightest()',
    r'for \(let ': r'for (',
    r'13': r"'\\r'",
    r'10': r"'\\n'",
    r'NodeColor\.Black': r'TreeNode.BLACK',
    r'NodeColor\.Red': r'TreeNode.RED',
    r'\.push': r'.add',
    r'AverageBufferSize': r'AVERAGE_BUFFER_SIZE',
    r'lastVisitedLine\.value': 'lastVisitedLineValue',
    r'lastVisitedLine\.lineNumber': 'lastVisitedLine',
}
js_removals = [
    r'^let ',
    r'^const ',
    r'\(\)',
    r'this\._',
    r'this\.'
]

INDENT = re.compile(r'^\s+', re.MULTILINE)

with open(args.js, 'r') as f:
    js = f.read()
    js = re.sub(INDENT, '', js)
    for k, v in js_substitutions.items():
        js = re.sub(re.compile(k, re.MULTILINE), v, js)
    for r in js_removals:
        js = re.sub(re.compile(r, re.MULTILINE), '', js)
    output(args.js, js)

java_removals = [
    r'^final ',
    r'^int ',
    r'^char ',
    r'^boolean ',
    r'^String ',
    r'^StringBuffer ',
    r'^StringBuilder ',
    r'^BufferCursor ',
    r'^NodePosition ',
    r'^Piece ',
    r'^Index ',
    r'^OrderedIntArrayList ',
    r'^TreeNode ',
    r'\.toString',
    r'\(\)',
    r'this\.'
]
java_replaces = {
    r'\.set\(([^(),]+), ([^()]+)\)': r'[\1] = \2',
    r'\.get\(([^(),]+)\)': r'[\1]',
    r'\.getFirst': r'[0]',
    r'\.size([^\w_])': r'.length\1',
    r'for \(int ': r'for (',
    r' != null': r'',
    r'\.append\(([^;]+)\);': r' += \1;',
    r'\"': r"'",
    r'new StringBuilder': r"''",
    r'List<(\w+)> (\w+) = new ArrayList<>;': r'\2: \1[] = [];',
    r'createLineStartsFast\((\w+), true\)': r'createLineStartsFast(\1)',
    r'^List<\w+> ': '',
}

with open(args.java, 'r') as f:
    java = f.read()
    java = re.sub(INDENT, '', java)
    for r in java_removals:
        java = re.sub(re.compile(r, re.MULTILINE), '', java)
    for k, v in java_replaces.items():
        java = re.sub(re.compile(k, re.MULTILINE), v, java)
    output(args.java, java)
