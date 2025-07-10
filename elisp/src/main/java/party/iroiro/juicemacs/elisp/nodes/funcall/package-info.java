/// This package contains nodes for efficient function calls.
///
/// ## Too Long; Didn't Read
///
/// Use an inlined [party.iroiro.juicemacs.elisp.nodes.funcall.FuncallDispatchNode].
///
/// ## ELisp Function Calls
///
/// We want to unify the following function calls in ELisp as much as possible:
///
/// - `(func args)`, where `func` could be:
///   - a symbol, direct or indirect
///   - a cons started with `lambda` (or maybe `macro`?)
///   - a callable object (`#[...]` or native subroutines)
/// - `(funcall func args)` or `(apply func args list)`
/// - `(mapc func list)` or `(mapcar func list)` where we internally call the functions
/// - `CALL/CALL1/2/.../N` bytecode
///
/// There are at least three indirections we need to handle:
///
/// 1. From symbols to their symbol-function values
/// 2. From raw conses `(lambda () ...)` to actual functions
/// 3. From functions to [com.oracle.truffle.api.CallTarget] containers
///
/// And there are two more things to consider:
///
/// 1. Calling convention:
///    - Foreign call: unimplemented
///    - ELisp function call: we pass the closure object itself as the first argument,
///      which contains the environment of closures. This is to enable sharing RootNodes
///      between closure
/// 2. Error messages: when a symbol function call fails, we very much want the error
///    message to mention that symbol instead of the function object.
///
/// ## Classes
///
/// - [party.iroiro.juicemacs.elisp.nodes.funcall.ReadFunctionObjectNodes] handles two
///   indirections: from symbol to functions, and from raw conses to functions.
/// - [party.iroiro.juicemacs.elisp.nodes.funcall.FunctionObjectCallNode] handles
///   calling convention and the last indirection: from functions to call targets.
///   - It uses [party.iroiro.juicemacs.elisp.nodes.funcall.FunctionDispatchNode]
///     to make direct calls as much as possible.
///   - [party.iroiro.juicemacs.elisp.nodes.funcall.ReadFunctionArgNode] is also for
///     calling convention handling.
///
/// Finally, there's this monolith class that uses all above for a funcall interface:
/// [party.iroiro.juicemacs.elisp.nodes.funcall.FuncallDispatchNode].
@NonNullByDefault
package party.iroiro.juicemacs.elisp.nodes.funcall;

import org.eclipse.jdt.annotation.NonNullByDefault;
