/// This `module-info.java` file is not guaranteed to be up to date.
///
/// IntelliJ IDEA seems to have trouble with the `org.graalvm.truffle` module,
/// always erring with something like:
///
/// ```
/// Package 'com.oracle.truffle.api' is declared in module 'org.graalvm.truffle',
/// but module 'Juicemacs.elisp.main' does not read it
/// ```
///
/// ... despite the fact that the `requires` statement is present and the tests runs fine.
///
/// So basically when I am working in IDEA, I just delete this file (without commiting it)
/// and go on.
module party.iroiro.juicemacs.elisp {
    // Utilities
    requires org.apache.commons.compress;
    requires org.apache.commons.lang3;
    requires org.apache.commons.text;
    requires org.apache.fory.core;
    requires org.eclipse.collections.impl;
    requires org.graalvm.collections;
    requires com.github.albfernandez.juniversalchardet;
    requires org.jspecify;
    requires strftime4j;

    // GraalVM/Truffle
    requires org.graalvm.truffle;
    requires org.graalvm.polyglot;

    // Java
    requires java.management;

    // Juicemacs
    requires party.iroiro.juicemacs.piecetree;
    requires party.iroiro.juicemacs.mule;

    provides com.oracle.truffle.api.provider.TruffleLanguageProvider
            with party.iroiro.juicemacs.elisp.ELispLanguageProvider;

    exports party.iroiro.juicemacs.elisp.parser;
    exports party.iroiro.juicemacs.elisp.runtime;
    exports party.iroiro.juicemacs.elisp.runtime.array;
    exports party.iroiro.juicemacs.elisp.runtime.objects;
    exports party.iroiro.juicemacs.elisp.runtime.string;
}
