package party.iroiro.juicemacs.elisp.forms;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Information about an ELisp builtin subroutine
 *
 * <p>
 * We use this in combination with {@link com.oracle.truffle.api.dsl.GenerateNodeFactory}
 * to reduce the boilerplate code.
 * </p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface ELispBuiltIn {

    /**
     * @return the name of the builtin routine
     */
    String name();

    /**
     * @return minimal number of arguments
     */
    int minArgs();

    /**
     * @return minimal number of arguments
     */
    int maxArgs();

    /**
     * @return whether the function takes {@code &rest varargs} parameters
     */
    boolean varArgs() default false;

    /**
     * @return documentation
     */
    String doc() default "";

}
