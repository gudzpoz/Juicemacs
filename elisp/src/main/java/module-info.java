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
    requires java.logging;
    requires jdk.unsupported;
    requires org.apache.commons.text;
    requires org.apache.fury.core;
    requires org.eclipse.jdt.annotation;
    requires org.eclipse.collections.impl;
    requires org.graalvm.collections;
    requires org.graalvm.truffle;
    requires org.graalvm.polyglot;
    requires party.iroiro.juicemacs.mule;
    requires party.iroiro.juicemacs.piecetree;
    requires com.github.albfernandez.juniversalchardet;

    provides com.oracle.truffle.api.provider.TruffleLanguageProvider
            with party.iroiro.juicemacs.elisp.ELispLanguageProvider;
}
