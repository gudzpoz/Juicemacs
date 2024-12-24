package party.iroiro.juicemacs.juice;

import com.oracle.truffle.api.source.Source;
import org.eclipse.jdt.annotation.Nullable;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.jline.builtins.SyntaxHighlighter;
import org.jline.keymap.KeyMap;
import org.jline.reader.*;
import org.jline.reader.impl.DefaultParser;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStringBuilder;
import org.jline.utils.AttributedStyle;
import org.jline.widget.AutopairWidgets;
import party.iroiro.juicemacs.elisp.parser.ELispParser;
import party.iroiro.juicemacs.elisp.runtime.ELispGlobals;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.mule.MuleString;
import picocli.CommandLine;
import picocli.CommandLine.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.regex.Pattern;

@Command(
        name = "elisp",
        description = "Emacs Lisp REPL"
)
public class ELispRepl implements Callable<Integer> {
    @Option(names = {"-L", "--directory"}, description = "Prepend DIR to load-path")
    File @Nullable[] userLoadPaths;

    @Option(names = {"--emacs-data"}, description = "Emacs data directory")
    @Nullable
    File emacsDataDir;

    private final static String PROMPT_STRING = new AttributedStringBuilder()
            .style(AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN))
            .append(">>> ")
            .toAnsi();

    private final static String SECONDARY_PROMPT_STRING = new AttributedStringBuilder()
            .style(AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN))
            .append("  > ")
            .toAnsi();

    @Override
    public Integer call() throws Exception {

        if (userLoadPaths == null || userLoadPaths.length == 0) {
            userLoadPaths = new File[]{Path.of("..", "elisp", "emacs", "lisp").toFile()};
        }
        String loadPaths = String.join(
                File.pathSeparator,
                Arrays.stream(userLoadPaths).map(File::getAbsolutePath).toArray(String[]::new)
        );
        String emacsData = emacsDataDir == null
                ? Path.of("..", "elisp", "emacs", "etc").toAbsolutePath().toString()
                : emacsDataDir.getAbsolutePath();
        try (Context context = Context.newBuilder("elisp")
                .environment("EMACSLOADPATH", loadPaths)
                .environment("EMACSDATA", emacsData)
                .build()) {
            LineReader lineReader = getLineReader(context);
            try {
                context.eval("elisp", "(load \"loadup\")");
            } catch (PolyglotException e) {
                printStackTrace(e, lineReader);
            }

            while (true) {
                String line;
                try {
                    line = lineReader.readLine(PROMPT_STRING);
                } catch (EndOfFileException ignored) {
                    break;
                }
                try {
                    Value value = context.eval("elisp", line);
                    AttributedString output = lineReader.getHighlighter().highlight(lineReader, value.toString());
                    lineReader.printAbove(output.toAnsi());
                } catch (PolyglotException e) {
                    printStackTrace(e, lineReader);
                }
            }
        }
        return 0;
    }

    private void printStackTrace(PolyglotException e, LineReader lineReader) {
        lineReader.printAbove(e.getMessage());
        boolean guestFrame = false;
        for (PolyglotException.StackFrame stackFrame : e.getPolyglotStackTrace()) {
            lineReader.printAbove(stackFrame.toString());
            if (stackFrame.isGuestFrame()) {
                guestFrame = true;
            } else if (guestFrame) {
                break;
            }
        }
    }

    private LineReader getLineReader(Context context) throws IOException {
        Terminal terminal = TerminalBuilder.builder().build();
        Thread currentThread = Thread.currentThread();
        terminal.handle(Terminal.Signal.INT, _ -> currentThread.interrupt());

        LineReader reader = LineReaderBuilder.builder()
                .appName("ELisp REPL")
                .terminal(terminal)
                .completer(new LispCompleter(context))
                .highlighter(new LispHighlighter())
                .parser(new LispParser())
                .option(LineReader.Option.INSERT_BRACKET, true)
                .variable(LineReader.SECONDARY_PROMPT_PATTERN, SECONDARY_PROMPT_STRING)
                .variable(LineReader.INDENTATION, 2)
                .variable(LineReader.WORDCHARS, "")
                .build();

        String[] disabled = {"`", "'"};
        AutopairWidgets autopairWidgets = new AutopairWidgets(reader, true);
        KeyMap<Binding> keyMap = autopairWidgets.getKeyMap();
        HashMap<String, Binding> disabledBindings = new HashMap<>();
        for (String disabledKey : disabled) {
            disabledBindings.put(disabledKey, keyMap.getBound(disabledKey));
        }
        autopairWidgets.enable();
        for (String disabledKey : disabled) {
            keyMap.bind(disabledBindings.get(disabledKey), disabledKey);
        }
        return reader;
    }

    public static void main(String[] args) {
        int exitCode = new CommandLine(new ELispRepl()).execute(args);
        System.exit(exitCode);
    }

    private static class LispHighlighter implements Highlighter {
        private final SyntaxHighlighter highlighter = SyntaxHighlighter.build("classpath:/elisp.nanorc");

        @Override
        public AttributedString highlight(LineReader reader, String buffer) {
            return highlighter.highlight(buffer);
        }

        @Override
        public void setErrorPattern(Pattern errorPattern) {
        }

        @Override
        public void setErrorIndex(int errorIndex) {
        }
    }

    private static class LispParser extends DefaultParser {
        private static final ELispParser.InternContext intern = new ELispParser.InternContext() {
            @Override
            public ELispSymbol intern(String name) {
                return ELispGlobals.NIL;
            }

            @Override
            public ELispSymbol intern(MuleString name) {
                return intern("");
            }

            @Override
            public MuleString applyShorthands(MuleString symbol) {
                return symbol;
            }
        };

        private LispParser() {
            lineCommentDelims(new String[]{";"})
                    .eofOnUnclosedBracket(DefaultParser.Bracket.ROUND, DefaultParser.Bracket.SQUARE)
                    .eofOnUnclosedQuote(false);
        }

        @Override
        public ParsedLine parse(String line, int cursor, ParseContext context) throws SyntaxError {
            try {
                ELispParser.read(intern, Source.newBuilder("elisp", line, "<jline>").build());
            } catch (IOException | ELispSignals.ELispSignalException e) {
                if (context == ParseContext.ACCEPT_LINE) {
                    throw new EOFError(0, 0, e.getMessage());
                }
            }
            return super.parse(line, cursor, context);
        }

        @Override
        public boolean isDelimiterChar(CharSequence buffer, int pos) {
            char c = buffer.charAt(pos);
            return Character.isWhitespace(c) || "()[]'`,".indexOf(c) != -1;
        }
    }

    private record LispCompleter(Value topLevelBindings) implements Completer {
        public LispCompleter(Context context) {
            this(context.getBindings("elisp"));
        }

        @Override
        public void complete(LineReader reader, ParsedLine line, List<Candidate> candidates) {
            candidates.clear();
            for (String symbol : topLevelBindings.getMemberKeys()) {
                candidates.add(new Candidate(symbol));
            }
        }
    }
}
